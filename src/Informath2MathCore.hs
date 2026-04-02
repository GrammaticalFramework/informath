{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Informath2MathCore where

import Informath
import PGF (showExpr)

data SEnv = SEnv {varlist :: [String]}
initSEnv = SEnv {varlist = []}

-- used when no Kind is given e.g. as quantifier domain
unspecifiedKind :: GKind
unspecifiedKind = GNounKind (LexNoun "object_Noun")

newVar :: SEnv -> (GIdent, SEnv)
newVar senv = (xi, senv{varlist = x : varlist senv}) where
  x = head [x | x <- ["_h" ++ show i | i <- [0..]], notElem x (varlist senv)]
  xi = GStrIdent (GString x)
  
semantics :: Tree a -> Tree a
semantics = addCoercions . addParenth . sem initSEnv . removeFonts

addCoercions :: Tree a -> Tree a
addCoercions t = case t of
  GAxiomJmt label hypos prop -> GAxiomJmt label (addCoercions hypos) (proofProp prop)  
  GThmJmt label hypos prop proof -> GThmJmt label (addCoercions hypos) (proofProp prop) proof
  GAxiomExpJmt label hypos exp kind -> GAxiomExpJmt label (addCoercions hypos) exp (elemKind kind)

---- TODO: check where exactly coercions are needed
  
  --AxiomPropJmt : Label -> ListHypo -> Prop -> Jmt ;
  --DefPropJmt : Label -> ListHypo -> Prop -> Prop -> Jmt ;
  --SuchThatKind : Ident -> Kind -> Prop -> Kind ;
  --AxiomKindJmt : Label -> ListHypo -> Kind -> Jmt ;
  --DefExpJmt : Label -> ListHypo -> Exp -> Kind -> Exp -> Jmt ;
  --DefKindJmt : Label -> ListHypo -> Kind -> Kind -> Jmt ;

  GPropHypo prop -> GPropHypo (proofProp prop)
  GVarsHypo idents kind -> GVarsHypo idents (elemKind kind)
  _ -> composOp addCoercions t
 where
   proofProp prop = case prop of
     GProofProp _ -> prop
     _ -> GProofProp prop
   elemKind kind = case kind of
     GElemKind kind -> kind
     _ -> GElemKind kind

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
  GAllProp (GListArgKind argkinds) prop ->
    foldr (\ (var, exp) y -> GCoreAllProp exp var y)
        (addParenth prop)
        (concatMap semArgkind argkinds)
  GExistProp (GListArgKind argkinds) prop ->
    foldr (\ (var, exp) y -> GCoreExistProp exp var y)
        (addParenth prop)
        (concatMap semArgkind argkinds)
  _ -> composOp addParenth t

semArgkind :: GArgKind -> [(GIdent, GKind)]
semArgkind argkind = case argkind of
  GIdentArgKind kind ident -> [(ident, kind)]
  GIdentsArgKind kind (GListIdent idents) -> [(ident, kind) | ident <- idents]
  GIndexedDeclarationArgKind (GInt i) ->
     [((GStrIdent (GString ("UNRESOLVED_" ++ show i))),
       GExpKind (GTermExp (GIdentTerm (GStrIdent (GString "UNRESOLVED_KIND")))))]
  GKindArgKind kind -> [(GStrIdent (GString "KIND_"), kind)]
  GBareIdentsArgKind (GListIdent idents) -> [(ident, unspecifiedKind) | ident <- idents]
  --- these should have been resolved in sem
  _ -> error ("semArgKind failure")


semHypo :: GHypo -> [GHypo]
semHypo hypo = case hypo of
  GAdjKindHypo xs@(GListIdent xx) adj kind ->
    GVarsHypo xs kind : [GPropHypo (GAdjProp adj (GTermExp (GIdentTerm x))) | x <- xx]
  _ -> [hypo]


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

  GListHypo hypos -> GListHypo (concatMap (semHypo . sem env) hypos)

  GIfProp cond prop -> case getAndProps cond of
    Just props -> sem env (foldr (\a b -> GIfProp a b) prop props)
    _ -> GIfProp (sem env cond) (sem env prop)

  GOnlyIfProp cond prop -> sem env (GIfProp cond prop)
  GFormulaImpliesProp cond prop ->
    sem env (GIfProp (GFormulaProp cond) (GFormulaProp prop))

  GExistNoProp argkinds prop -> GCoreNotProp (sem env (GExistProp argkinds prop))

  GPostQuantProp prop exp -> case sem env exp of
    GEveryIdentKindExp ident kind ->
      sem env (GCoreAllProp kind ident prop)
    GIndefIdentKindExp ident kind ->
      sem env (GCoreExistProp kind ident prop)
    GSomeIdentsKindExp idents kind ->
      sem env (GExistProp (GListArgKind [GIdentsArgKind kind idents]) prop)
    GAllIdentsKindExp idents kind ->
      sem env (GAllProp (GListArgKind [GIdentsArgKind kind idents]) prop)
    GNoIdentsKindExp idents kind ->
      sem env (GAllProp (GListArgKind [GIdentsArgKind kind idents]) (GCoreNotProp prop))
    _ -> t ----TODO some cases: error ("sem not yet: " ++ showExpr [] (gf t))

  GAllProp argkinds prop -> GAllProp (sem env argkinds) (sem env prop)
    
  ---- TODO: generalize agremment and in situ resolution to all predication functions
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
    GAdj2Adj adj2 exp2 ->
      GAdj2Prop adj2 (sem env exp) (sem env exp2)
    GAdj3Adj adj3 exp2 exp3 ->
      GAdj3Prop adj3 (sem env exp) (sem env exp2) (sem env exp3)
    sa -> case sem env exp of
      (GAndExp (GListExp exps)) ->
        GAndProp (GListProp [GAdjProp sa exp | exp <- exps])
      (GOrExp (GListExp exps)) ->
        GOrProp (GListProp [GAdjProp sa exp | exp <- exps])
      sexp -> GAdjProp sa sexp

  GAdjCCollProp adj (GListExp [x, y]) -> GAdjCProp adj (sem env x) (sem env y)
  ---- TODO: AdjCColl properly implemented in Dedukti
  
  GAdjECollProp adj (GListExp xs) ->
    GAndProp (GListProp [GAdjEProp adj x y | (x, y) <- subsequentPairs (map (sem env) xs)])

  GBothAndProp a b -> GAndProp (GListProp [sem env a, sem env b])
  GBothAndAdj a b -> GAndAdj (GListAdj [sem env a, sem env b])
  GBothAndExp a b -> GAndExp (GListExp [sem env a, sem env b])

  GEitherOrProp a b -> GOrProp (GListProp [sem env a, sem env b])
  GEitherOrAdj a b -> GOrAdj (GListAdj [sem env a, sem env b])
  GEitherOrExp a b -> GOrExp (GListExp [sem env a, sem env b])

  GNotAdjProp adj exp -> GCoreNotProp (sem env (GAdjProp adj exp))
  GNotAdj2Prop adj x y -> GCoreNotProp (sem env (GAdj2Prop adj x y))
  GNotAdjCProp adj exps -> GCoreNotProp (sem env (GAdjCCollProp adj exps))
  GNotAdjEProp adj exps -> GCoreNotProp (sem env (GAdjECollProp adj exps))
  GNotNoun1Prop adj exp -> GCoreNotProp (sem env (GNoun1Prop adj exp))
  GNotNoun2Prop adj x y -> GCoreNotProp (sem env (GNoun2Prop adj x y))
  GNotVerbProp adj exp -> GCoreNotProp (sem env (GVerbProp adj exp))
  GNotVerb2Prop adj x y -> GCoreNotProp (sem env (GVerb2Prop adj x y))

  GPluralKindExp kind -> GKindExp kind

  GKindArgKind kind -> 
    let (var, nenv) = newVar env
    in GIdentsArgKind (sem nenv kind) (GListIdent [var])

  GDisplayFormulaProp formula -> sem env (GFormulaProp formula)
  
  GFormulaProp (GEquationFormula equation@(GChainEquation _ _ _)) -> case chainedEquations equation of
    triples -> GAndProp (GListProp
      [sem env (GFormulaProp (GEquationFormula (GBinaryEquation eqsign x y))) | (eqsign, x, y) <- triples])
  GFormulaProp (GElemFormula (GListTerm xs) y) -> case xs of
    [x] -> GNoun2Prop (LexNoun2 "element_Noun2")
              (sem env (GTermExp x)) (sem env (GTermExp y))
    _ -> GAndProp (GListProp [sem env (GFormulaProp (GElemFormula (GListTerm [x]) y)) | x <- xs])

{-
  GTermExp (GTSum3dots m m1 n) ->
    let
      [sm, sm1, sn] = map (sem env) [m, m1, n]
      (var, nenv) = newVar env
    in sem nenv (GTermExp (iqTest var m m1 n)) 
-}
  Gtimes_Term x y -> GOper2Term (LexOper2 "times_Oper2") (sem env x) (sem env y)
  GParenthTerm term -> sem env term


-- Naproche extensions
  GSupposePropHypo prop -> sem env (GPropHypo prop)
  GIffIffProp a b -> sem env (GIffProp a b)
  GWeHaveProp prop -> sem env prop
  GNoCommaAllProp argkinds prop -> sem env (GAllProp argkinds prop)
  GBareIdentsArgKind idents -> sem env (GIdentsArgKind unspecifiedKind idents) ---- TODO: get from env
  GDeclarationArgKind declaration -> case sem env declaration of
   ---- TODO: check that all are idents
    GElemDeclaration (GListTerm terms) term ->
      GIdentsArgKind (GExpKind (GTermExp term)) (GListIdent [x | GIdentTerm x <- terms])
    GElemDeclaration (GListTerm terms) term -> 
      GIdentsArgKind (GExpKind (GTermExp term)) (GListIdent [x | GIdentTerm x <- terms])
    _ -> t ---- error "cannot use declaration as argkind yet"
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

--- also in DMC
gExps :: [GExp] -> GExps
gExps exps = case exps of
  [exp] -> GOneExps exp
  _ -> GManyExps (GListExp exps)


--- used in iqTest when guessing summation terms 
unknownTerm :: GTerm
unknownTerm = GIdentTerm (GStrIdent (GString "UNKNOWN"))

-- used for decomposing AdjE over a list
subsequentPairs xs = [(xs !! k, xs !! (k+1)) | k <- [0..(length xs - 2)] ]