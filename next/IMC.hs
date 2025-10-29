{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module IMC where
--module Informath2Core where

import NextInformath

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
  GAndProp (GListProp props) -> foldl1 GCoreAndProp (map addParenth props)
  GOrProp (GListProp props) -> foldl1 GCoreOrProp (map addParenth props)
  GIfProp a b -> GCoreIfProp (addParenth a) (addParenth b)
  GIffProp a b -> GCoreIffProp (addParenth a) (addParenth b)
----  GAllProp argkinds prop -> 
  _ -> composOp addParenth t
  
sem :: SEnv -> Tree a -> Tree a
sem env t = case t of
{- ----
  GLetFormulaHypo formula -> case (sem env formula) of
    GElemFormula (GListTerm terms) (GSetTerm set) ->
      GVarsHypo (GListIdent [x | GIdentTerm x <- terms]) (GSetKind set) ---- TODO: check that all terms are idents

    _ -> GPropHypo (sem env (GFormulaProp (sem env formula)))
    
  GLetDeclarationHypo decl -> case (sem env decl) of
    GElemDeclaration (GListTerm terms) (GSetTerm set) ->
      GVarsHypo (GListIdent [x | GIdentTerm x <- terms]) (GSetKind set) ---- TODO: check that all terms are idents
  GIfProp cond@(GFormulaProp (GElemFormula (GListTerm terms) (GSetTerm set))) prop ->
    case getJustVarsFromTerms env terms of
      Just xs -> sem env (GAllProp (GListArgKind [GIdentsArgKind (GSetKind set) (GListIdent xs)]) prop)
      _ ->  GIfProp (sem env cond) (sem env prop)
-}      
  GIfProp cond@(GKindProp exp kind) prop -> case getJustVars env exp of
    Just xs -> sem env (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent xs)]) prop)
    _ -> GIfProp (sem env cond) (sem env prop)
    
  GIfProp cond prop -> case getAndProps cond of
    Just props -> sem env (foldr (\a b -> GIfProp a b) prop props)
    _ -> GIfProp (sem env cond) (sem env prop)

  GOnlyIfProp cond prop -> sem env (GIfProp cond prop)
  GFormulaImpliesProp cond prop ->
    sem env (GIfProp (GFormulaProp cond) (GFormulaProp prop))

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
        (GIfProp
	  (sem env (mkAndProp [GAdjProp adj (GTermExp (GIdentTerm x)) | x <- xs]))
	  (sem env prop))
    akinds -> GAllProp (sem env akinds) (sem env prop)
    
  GKindProp exp (GAdjKind adj kind_) ->
    sem env (GAdjProp adj exp) --- ignoring kind, if not in hypothesis position

  GAdjKind adj kind ->
    let (var, nenv) = newVar env
    in GSuchThatKind var (sem nenv kind) (sem nenv (GAdjProp adj (GTermExp (GIdentTerm var))))

  GAdjProp adj (GAllIdentsKindExp (GListIdent [x]) kind) ->
    sem env (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
              (GAdjProp adj (GTermExp (GIdentTerm x))))
  GAdjProp adj (GEveryIdentKindExp x kind) ->
    sem env (GAdjProp adj (GAllIdentsKindExp  (GListIdent [x]) kind))
  GAdjProp adj (GSomeIdentsKindExp (GListIdent [x]) kind) ->
    sem env (GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
              (GAdjProp adj (GTermExp (GIdentTerm x))))
  GAdjProp adj (GIndefIdentKindExp x kind) ->
    sem env (GAdjProp adj (GSomeIdentsKindExp  (GListIdent [x]) kind))
  GAdjProp adj (GNoIdentsKindExp (GListIdent [x]) kind) ->
    sem env (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
              (GNotAdjProp adj (GTermExp (GIdentTerm x))))
	      
  GAdjProp adj (GEveryKindExp kind) ->
    let (x, env') = newVar env
    in sem env'
      (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
        (GAdjProp adj (GTermExp (GIdentTerm x))))
  GAdjProp adj (GAllKindExp kind) ->
    sem env (GAdjProp adj (GEveryKindExp kind))
  GAdjProp adj (GSomeKindExp kind) ->
    let (x, env') = newVar env
    in sem env'
      (GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
        (GAdjProp adj (GTermExp (GIdentTerm x))))
  GAdjProp adj (GIndefKindExp kind) ->
    sem env (GAdjProp adj (GSomeKindExp kind))
  GAdjProp adj (GNoKindExp kind) ->
    let (x, env') = newVar env
    in sem env'
      (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])])
        (GNotAdjProp adj (GTermExp (GIdentTerm x))))
	
  GAdjProp adj exp -> case sem env adj of
    GAndAdj (GListAdj adjs)  ->
      let sx = sem env exp
      in GAndProp (GListProp [GAdjProp adj sx | adj <- adjs])
    GOrAdj (GListAdj adjs)  ->
      let sx = sem env exp
      in GOrProp (GListProp [GAdjProp adj sx | adj <- adjs])
    sa -> case sem env exp of
      (GAndExp (GListExp exps)) ->
        GAndProp (GListProp [GAdjProp sa exp | exp <- exps])
      (GOrExp (GListExp exps)) ->
        GOrProp (GListProp [GAdjProp sa exp | exp <- exps])
      sexp -> GAdjProp sa sexp

  GBothAndProp a b -> GAndProp (GListProp [sem env a, sem env b])
  GBothAndAdj a b -> GAndAdj (GListAdj [sem env a, sem env b])
  GBothAndExp a b -> GAndExp (GListExp [sem env a, sem env b])

  GEitherOrProp a b -> GOrProp (GListProp [sem env a, sem env b])
  GEitherOrAdj a b -> GOrAdj (GListAdj [sem env a, sem env b])
  GEitherOrExp a b -> GOrExp (GListExp [sem env a, sem env b])


  GNotAdjProp adj exp -> GNotProp (sem env (GAdjProp adj exp))

  GKindArgKind kind -> 
    let (var, nenv) = newVar env
    in GIdentsArgKind (sem nenv kind) (GListIdent [var])
{- ----    
  GFormulaProp (GFModulo term1 term2 term3) ->
    GAdjProp (GPred3Adj (LexPred3 "modulo_Pred3") (sem env (GTermExp term2)) (sem env (GTermExp term3)))
      (sem env (GTermExp term1))
  GFormulaProp (GFEquation (GEBinary (GComparEqsign compar) term1 term2)) ->
    GAdjProp (GComparAdj compar (sem env (GTermExp term2))) (sem env (GTermExp term1))
  GFormulaProp (GFEquation (GEBinary (GComparnounEqsign compar) term1 term2)) ->
    GComparnounProp compar (sem env (GTermExp term1)) (sem env (GTermExp term2))
  GFormulaProp (GFEquation equation@(GEChain _ _ _)) -> case chainedEquations equation of
    triples -> GAndProp (GListProp
      [sem env (GFormulaProp (GFEquation (GEBinary eqsign x y))) | (eqsign, x, y) <- triples])
  GFormulaProp (GElemFormula (GListTerm xs) y) -> case xs of
  
    [x] -> GComparnounProp (LexComparnoun "element_Comparnoun")
              (sem env (GTermExp x)) (sem env (GTermExp y))
    _ -> GAndProp (GListProp [sem env (GFormulaProp (GElemFormula (GListTerm [x]) y)) | x <- xs])
    
  GTermExp (GConstTerm const) -> GConstExp const
  GTermExp (GAppOperTerm oper x y) ->
    GOperListExp oper (GAddExps (sem env (GTermExp x)) (GOneExps (sem env (GTermExp y))))
  GTermExp (GAppOperOneTerm (LexOper "minus_Oper") x) ->  ---- should not be needed
    GOperListExp (LexOper "neg_Oper") (GOneExps (sem env (GTermExp x)))
  GTermExp (GAppOperOneTerm oper x) ->
    GOperListExp oper (GOneExps (sem env (GTermExp x)))
  GTermExp (GTSum3dots m m1 n) ->
    let
      [sm, sm1, sn] = map (sem env) [m, m1, n]
      (var, nenv) = newVar env
    in sem nenv (GTermExp (iqTest var m m1 n)) 
  GTermExp (GTSigma i m n f) ->
    GSigmaExp i (sem env (GTermExp m)) (sem env (GTermExp n)) (sem env (GTermExp f))
  GTermExp (GTTimes x y) -> sem env (GTermExp (GAppOperTerm (LexOper "times_Oper") x y))
  GTermExp (GTFrac x y) -> sem env (GTermExp (GAppOperTerm (LexOper "div_Oper") x y))
  GTermExp (GTNeg x) ->  sem env (GTermExp (GAppOperOneTerm (LexOper "neg_Oper") x))
  GTermExp (GTEnumSet (GListTerm xs)) -> sem env (GEnumSetExp (gExps (map GTermExp xs)))
-}
  GParenthTerm term -> sem env term


-- Naproche extensions
  GSupposePropHypo prop -> sem env (GPropHypo prop)
  GIffIffProp a b -> sem env (GIffProp a b)
  GWeHaveProp prop -> sem env prop
  GNoCommaAllProp argkinds prop -> sem env (GAllProp argkinds prop)
  GBareIdentsArgKind idents -> sem env (GIdentsArgKind (GNounKind (LexNoun "number_Noun")) idents) ---- TODO: get from env
{- ----
  GDeclarationArgKind declaration -> case sem env declaration of
    GElemDeclaration (GListTerm terms) (GSetTerm set) -> ---- TODO: check that all are idents
      GIdentsArgKind (GSetKind set) (GListIdent [x | GIdentTerm x <- terms])
    GElemDeclaration (GListTerm terms) term -> 
      GIdentsArgKind (GTermKind term) (GListIdent [x | GIdentTerm x <- terms])
    _ -> t ---- error "cannot use declaration as argkind yet"
-}
  GNoCommaExistProp argkinds prop ->
    sem env (GExistProp argkinds prop)
  GNoArticleExistProp argkind prop ->
    sem env (GExistProp (GListArgKind [argkind]) prop)
  _ -> composOp (sem env) t

{- ----
-- trying to guess the summation term from given examples
iqTest :: GIdent -> GTerm -> GTerm -> GTerm -> GTerm
iqTest i mterm m1term nterm = case findTerm mterm m1term nterm of
  Just term -> term
  _ -> foldl1 (GAppOperTerm (LexOper "plus_Oper")) [mterm, m1term, unknownTerm, nterm]
 where
   findTerm mterm m1term nterm = case refactorTerms mterm nterm of
     Just (m, n, f) -> return (GTSigma i m n (f (GIdentTerm i))) ---- verify with m1term
     _ -> Nothing
   refactorTerms :: GTerm -> GTerm -> Maybe (GTerm, GTerm, GTerm -> GTerm)
   refactorTerms mterm nterm = case (mterm, nterm) of
     (GAppOperTerm f1 x1 y1, GAppOperTerm f2 x2 y2) | f1 == f2 -> case () of  --- special case: one binop
       _ | x1 == x2 && y1 /= y2 -> return (y1, y2, \y -> GAppOperTerm f1 x1 y)
       _ | x1 /= x2 && y1 == y2 -> return (x1, x2, \x -> GAppOperTerm f1 x y1)
     _ -> Nothing
-}

chainedEquations :: GEquation -> [(GCompar, GTerm, GTerm)]
chainedEquations equation = case equation of
  GChainEquation eqsign term equ ->
    let triples@((_, x, _):_) = chainedEquations equ
    in (eqsign, term, x) : triples
  GBinaryEquation eqsign term1 term2 ->
    [(eqsign, term1, term2)]

{-
ifs2hypos :: [GHypo] -> GProp -> ([GHypo], GProp)
ifs2hypos hs prop = case prop of
  GIfProp p q -> 
-}


hypoVars :: GHypo -> [GIdent]
hypoVars hypo = case hypo of
  GVarsHypo (GListIdent idents) _ -> idents
  _ -> []

-- identify exp lists that are just variable lists, possibly bindings
getJustVars :: SEnv -> GExp -> Maybe [GIdent]
getJustVars env exp = case exp of
  GTermExp (GIdentTerm x) -> Just [x]
  GAndExp (GListExp exps) -> do
    xss <- mapM (getJustVars env) exps
    return (concat xss)
  _ -> Nothing

getJustVarsFromTerms :: SEnv -> [GTerm] -> Maybe [GIdent]
getJustVarsFromTerms env terms = case terms of
  GIdentTerm x : ts ->  do
    xs <- getJustVarsFromTerms env ts
    return (x : xs)
  [] -> return []
  _ -> Nothing

mkAndProp :: [GProp] -> GProp
mkAndProp props = case props of
  [prop] -> prop
  _ -> GAndProp (GListProp props)


getAndProps :: GProp -> Maybe [GProp]
getAndProps prop = case prop of
  GAndProp (GListProp props) -> Just props
  _ -> Nothing

gExps :: [GExp] -> GExps
gExps exps = foldr GAddExps (GOneExps (last exps)) (init exps)

--- used in iqTest when guessing summation terms 
unknownTerm :: GTerm
unknownTerm = GIdentTerm (GStrIdent (GString "UNKNOWN"))

