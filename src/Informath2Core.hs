{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Informath2Core where

import Informath

data SEnv = SEnv {varlist :: [String]}
initSEnv = SEnv {varlist = []}

newVar :: SEnv -> (GIdent, SEnv)
newVar senv = (xi, senv{varlist = x : varlist senv}) where
  x = head [x | x <- ["_h" ++ show i | i <- [0..]], notElem x (varlist senv)]
  xi = GStrIdent (GString x)
  
semantics :: Tree a -> Tree a
semantics = addCoercions . addParenth . sem initSEnv . removeFonts

addCoercions :: Tree a -> Tree a
addCoercions t = case t of
  GAxiomJmt label hypos prop -> GAxiomJmt label (addCoercions hypos) (proofProp prop)
  {-
  AxiomPropJmt : Label -> ListHypo -> Prop -> Jmt ;
  DefPropJmt : Label -> ListHypo -> Prop -> Prop -> Jmt ;
  SuchThatKind : Ident -> Kind -> Prop -> Kind ;
  ThmJmt : Label -> ListHypo -> Prop -> Proof -> Jmt ;
  AxiomExpJmt : Label -> ListHypo -> Exp -> Kind -> Jmt ;
  AxiomKindJmt : Label -> ListHypo -> Kind -> Jmt ;
  DefExpJmt : Label -> ListHypo -> Exp -> Kind -> Exp -> Jmt ;
  DefKindJmt : Label -> ListHypo -> Kind -> Kind -> Jmt ;
  ElemKind : Kind -> Kind ;
  IdentsArgKind : Kind -> ListIdent -> ArgKind ;
  KindArgKind : Kind -> ArgKind ;
  TypedExp : Exp -> Kind -> Exp ;
  -}
  GPropHypo prop -> GPropHypo (proofProp prop)
  GVarsHypo idents kind -> GVarsHypo idents (GElemKind kind)
  _ -> composOp addCoercions t
 where
   proofProp prop = case prop of
     GProofProp _ -> prop
     _ -> GProofProp prop

removeFonts :: Tree a -> Tree a
removeFonts t = case t of
  GTextbfTerm term -> removeFonts term
  _ -> composOp removeFonts t

addParenth :: Tree a -> Tree a
addParenth t = case t of
  GSimpleAndProp (GListProp props) -> GAndProp (GListProp (map addParenth props))
  GSimpleOrProp (GListProp props) -> GOrProp (GListProp (map addParenth props))
  GSimpleIfProp a b -> GIfProp (addParenth a) (addParenth b)
  GSimpleIffProp a b -> GIffProp (addParenth a) (addParenth b)
  _ -> composOp addParenth t
  
sem :: SEnv -> Tree a -> Tree a
sem env t = case t of

  GLetFormulaHypo formula -> case (sem env formula) of
    GFElem (GListTerm terms) (GSetTerm set) ->
      GVarsHypo (GListIdent [x | GTIdent x <- terms]) (GSetKind set) ---- TODO: check that all terms are idents

    _ -> GPropHypo (sem env (GFormulaProp (sem env formula)))
    
  GLetDeclarationHypo decl -> case (sem env decl) of
    GDElem (GListTerm terms) (GSetTerm set) ->
      GVarsHypo (GListIdent [x | GTIdent x <- terms]) (GSetKind set) ---- TODO: check that all terms are idents

  GSimpleIfProp cond@(GFormulaProp (GFElem (GListTerm terms) (GSetTerm set))) prop ->
    case getJustVarsFromTerms env terms of
      Just xs -> sem env (GAllProp (GListArgKind [GIdentsArgKind (GSetKind set) (GListIdent xs)]) prop)
      _ ->  GSimpleIfProp (sem env cond) (sem env prop)
      
  GSimpleIfProp cond@(GKindProp exp kind) prop -> case getJustVars env exp of
    Just xs -> sem env (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent xs)]) prop)
    _ -> GSimpleIfProp (sem env cond) (sem env prop)
    
  GSimpleIfProp cond prop -> case getAndProps cond of
    Just props -> sem env (foldr (\a b -> GSimpleIfProp a b) prop props)
    _ -> GSimpleIfProp (sem env cond) (sem env prop)

  GPostQuantProp prop exp -> case exp of
    GEveryIdentKindExp ident kind ->
      sem env (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [ident])]) prop)
    GIndefIdentKindExp ident kind ->
      sem env (GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent [ident])]) prop)
    GSomeIdentsKindExp idents kind ->
      sem env (GExistProp (GListArgKind [GIdentsArgKind kind idents]) prop)
    GAllIdentsKindExp idents kind ->
      sem env (GAllProp (GListArgKind [GIdentsArgKind kind idents]) prop)
    GNoIdentsKindExp idents kind ->
      sem env (GAllProp (GListArgKind [GIdentsArgKind kind idents]) (GNotProp prop))

  GAllProp argkinds prop -> case argkinds of
    GListArgKind [GIdentsArgKind (GAdjKind adj kind) vars@(GListIdent xs)] ->
      GAllProp
        (GListArgKind [GIdentsArgKind kind vars])
        (GSimpleIfProp
	  (sem env (mkAndProp [GAdjProp adj (GTermExp (GTIdent x)) | x <- xs]))
	  (sem env prop))
    akinds -> GAllProp akinds (sem env prop)
    
  GKindProp exp (GAdjKind adj kind_) ->
    sem env (GAdjProp adj exp) --- ignoring kind, if not in hypothesis position

  GAdjKind adj kind ->
    let (var, nenv) = newVar env
    in GSuchThatKind var (sem nenv kind) (sem nenv (GAdjProp adj (GTermExp (GTIdent var))))

  GAdjProp adj (GAllIdentsKindExp (GListIdent [x]) kind) ->
    sem env (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
              (GAdjProp adj (GTermExp (GTIdent x))))
  GAdjProp adj (GEveryIdentKindExp x kind) ->
    sem env (GAdjProp adj (GAllIdentsKindExp  (GListIdent [x]) kind))
  GAdjProp adj (GSomeIdentsKindExp (GListIdent [x]) kind) ->
    sem env (GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
              (GAdjProp adj (GTermExp (GTIdent x))))
  GAdjProp adj (GIndefIdentKindExp x kind) ->
    sem env (GAdjProp adj (GSomeIdentsKindExp  (GListIdent [x]) kind))
  GAdjProp adj (GNoIdentsKindExp (GListIdent [x]) kind) ->
    sem env (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
              (GNotAdjProp adj (GTermExp (GTIdent x))))
	      
  GAdjProp adj (GEveryKindExp kind) ->
    let (x, env') = newVar env
    in sem env'
      (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
        (GAdjProp adj (GTermExp (GTIdent x))))
  GAdjProp adj (GAllKindExp kind) ->
    sem env (GAdjProp adj (GEveryKindExp kind))
  GAdjProp adj (GSomeKindExp kind) ->
    let (x, env') = newVar env
    in sem env'
      (GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
        (GAdjProp adj (GTermExp (GTIdent x))))
  GAdjProp adj (GIndefKindExp kind) ->
    sem env (GAdjProp adj (GSomeKindExp kind))
  GAdjProp adj (GNoKindExp kind) ->
    let (x, env') = newVar env
    in sem env'
      (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
        (GNotAdjProp adj (GTermExp (GTIdent x))))
	
  GAdjProp (GAndAdj (GListAdj adjs)) x ->
    let sx = sem env x
    in GAndProp (GListProp [GAdjProp adj sx | adj <- adjs])
  GAdjProp (GOrAdj (GListAdj adjs)) x ->
    let sx = sem env x
    in GOrProp (GListProp [GAdjProp adj sx | adj <- adjs])
  GAdjProp a (GAndExp (GListExp exps)) ->
    let sa = sem env a
    in GAndProp (GListProp [GAdjProp sa exp | exp <- exps])
  GAdjProp a (GOrExp (GListExp exps)) ->
    let sa = sem env a
    in GOrProp (GListProp [GAdjProp sa exp | exp <- exps])
  GNotAdjProp adj exp -> GNotProp (sem env (GAdjProp adj exp))

  GFormulaProp (GFModulo term1 term2 term3) ->
    GAdjProp (GPred3Adj (LexPred3 "modulo_Pred3") (sem env (GTermExp term2)) (sem env (GTermExp term3)))
      (sem env (GTermExp term1))
  GFormulaProp (GFEquation (GEBinary (GComparEqsign compar) term1 term2)) ->
    GAdjProp (GComparAdj compar (sem env (GTermExp term2))) (sem env (GTermExp term1))
  GFormulaProp (GFEquation equation@(GEChain _ _ _)) -> case chainedEquations equation of
    triples -> GAndProp (GListProp
      [sem env (GFormulaProp (GFEquation (GEBinary eqsign x y))) | (eqsign, x, y) <- triples])
    
  GTermExp (GConstTerm const) -> GConstExp const
  GTermExp (GAppOperTerm oper x y) ->
    GOperListExp oper (GAddExps (sem env (GTermExp x)) (GOneExps (sem env (GTermExp y))))
  GTermExp (GAppOperOneTerm (LexOper "minus_Oper") x) ->  ---- should not be needed
    GOperListExp (LexOper "neg_Oper") (GOneExps (sem env (GTermExp x)))
  GTermExp (GAppOperOneTerm oper x) ->
    GOperListExp oper (GOneExps (sem env (GTermExp x)))
  GTermExp (GTTimes x y) -> sem env (GTermExp (GAppOperTerm (LexOper "times_Oper") x y))
  GTermExp (GTFrac x y) -> sem env (GTermExp (GAppOperTerm (LexOper "div_Oper") x y))
  GTermExp (GTNeg x) ->  sem env (GTermExp (GAppOperOneTerm (LexOper "neg_Oper") x))
----  GTermExp (GTEnumSet xs) -> sem env (GEnumSetExp ())
  GTParenth term -> sem env term
      
  _ -> composOp (sem env) t

chainedEquations :: GEquation -> [(GEqsign, GTerm, GTerm)]
chainedEquations equation = case equation of
  GEChain eqsign term equ ->
    let triples@((_, x, _):_) = chainedEquations equ
    in (eqsign, term, x) : triples
  GEBinary eqsign term1 term2 ->
    [(eqsign, term1, term2)]

{-
ifs2hypos :: [GHypo] -> GProp -> ([GHypo], GProp)
ifs2hypos hs prop = case prop of
  GSimpleIfProp p q -> 
-}


hypoVars :: GHypo -> [GIdent]
hypoVars hypo = case hypo of
  GVarsHypo (GListIdent idents) _ -> idents
  _ -> []

-- identify exp lists that are just variable lists, possibly bindings
getJustVars :: SEnv -> GExp -> Maybe [GIdent]
getJustVars env exp = case exp of
  GTermExp (GTIdent x) -> Just [x]
  GAndExp (GListExp exps) -> do
    xss <- mapM (getJustVars env) exps
    return (concat xss)
  _ -> Nothing

getJustVarsFromTerms :: SEnv -> [GTerm] -> Maybe [GIdent]
getJustVarsFromTerms env terms = case terms of
  GTIdent x : ts ->  do
    xs <- getJustVarsFromTerms env ts
    return (x : xs)
  [] -> return []
  _ -> Nothing

mkAndProp :: [GProp] -> GProp
mkAndProp props = case props of
  [prop] -> prop
  _ -> GSimpleAndProp (GListProp props)


getAndProps :: GProp -> Maybe [GProp]
getAndProps prop = case prop of
  GSimpleAndProp (GListProp props) -> Just props
  _ -> Nothing


