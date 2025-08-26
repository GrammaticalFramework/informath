{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core2Informath where

import Informath

import Data.List (nub, sortOn)
import Data.Char (isDigit)

type Opts = [String]

nlg :: Opts -> Tree a -> [Tree a]
nlg opts tree = case opts of
   _ | elem "-variations" opts ->
         nub $ concatMap variations [t, ut, ft, aft, iaft, viaft]
   _ -> [viaft]
 where
   t = unparenth tree
   ut = uncoerce t
   ft = formalize ut
   aft = aggregate (flatten ft)
   iaft = insitu aft
   viaft = varless iaft

unparenth :: Tree a -> Tree a
unparenth t = case t of
  GAndProp (GListProp props) -> GSimpleAndProp (GListProp (map unparenth props))
  GOrProp (GListProp props) -> GSimpleOrProp (GListProp (map unparenth props))
  GIfProp a b -> GSimpleIfProp (unparenth a) (unparenth b)
  GIffProp a b -> GSimpleIffProp (unparenth a) (unparenth b)
  _ -> composOp unparenth t

uncoerce :: Tree a -> Tree a
uncoerce t = case t of
  GProofProp prop -> uncoerce prop
  GElemKind kind -> uncoerce kind
  GCoercionExp coercion_ exp -> uncoerce exp
  _ -> composOp uncoerce t

formalize :: Tree a -> Tree a
formalize t = case t of
  GVarsHypo (GListIdent [f]) (GFam2Kind (LexFam "function_Fam") (GSetKind a) (GSetKind b)) ->
    GLetDeclarationHypo (GDFunction f (GSetTerm a) (GSetTerm b))
  GAdjProp (GPred3Adj p@(LexPred3 "congruent_Pred3") y z) x -> case (getTerm x, getTerm y, getTerm z) of
    (Just tx, Just ty, Just tz) ->
      GFormulaProp (GFModulo tx ty tz)
    _ -> GAdjProp (GPred3Adj p (formalize y) (formalize z)) (formalize x)
  GAdjProp (GComparAdj compar y) x -> case (getTerm x, getTerm y) of
    (Just tx, Just ty) ->
      GFormulaProp (GFEquation (GEBinary (GComparEqsign compar) tx ty))
    _ -> GAdjProp (GComparAdj compar (formalize y)) (formalize x)
  GAdjProp adj x -> maybe t (GAdjProp adj . GTermExp) (getTerm x)
  GComparnounProp compar x y -> case (getTerm x, getTerm y) of
    (Just tx, Just ty) ->
      GFormulaProp (GFEquation (GEBinary (GComparnounEqsign compar) tx ty))
  GOperListExp oper xy@(GAddExps x (GOneExps y)) -> case getTerm t of
    Just tr -> GTermExp tr
    _ -> GOperListExp oper (formalize xy)
  GConstExp const -> GTermExp (GConstTerm const)
  GFunListExp f exps -> maybe t GTermExp (getTerm t) 
  GOperListExp f exps -> maybe t GTermExp (getTerm t) 
  GAppExp f exps -> maybe t GTermExp (getTerm t)
  _ -> composOp formalize t

getTerm :: Tree a -> Maybe GTerm
getTerm = maybe Nothing (return . optTerm) . gT where
  gT :: Tree a -> Maybe GTerm
  gT t = case t of
    GConstExp const -> return (GConstTerm const)
    GOperListExp oper (GOneExps x) -> do
      tx <- gT x
      return (GAppOperOneTerm oper tx)
    GOperListExp oper (GAddExps x (GOneExps y)) -> do
      tx <- gT x
      ty <- gT y
      return (GAppOperTerm oper tx ty)
    GAppExp t@(GTermExp (GTIdent f)) exps -> case mapM gT (exps2list exps) of
      Just xs -> return (GTApp (GFIdent f) (GListTerm xs))
      _ -> Nothing
    GEnumSetExp exps -> case mapM gT (exps2list exps) of
      Just xs -> return (GTEnumSet (GListTerm xs))
      _ -> Nothing
    GTermExp term -> return term
    GSigmaExp i m n f -> do
      tm <- getTerm m
      tn <- getTerm n
      tf <- getTerm f
      return $ GTSigma i tm tn tf
    _ -> Nothing
  optTerm :: Tree a -> Tree a
  optTerm t = case t of
    GAppOperTerm (LexOper "times_Oper") x y -> GTTimes (optTerm x) (optTerm y)
    GAppOperOneTerm (LexOper "neg_Oper") x -> GTNeg (optTerm x) ---- really needed?
    _ -> composOp optTerm t

aggregate :: Tree a -> Tree a
aggregate t = case t of
  GNotProp prop -> case aggregate prop of
    GAdjProp adj x -> GNotAdjProp adj x
    aprop -> GNotProp aprop
  GSimpleAndProp (GListProp props) ->
    case groupProps "and" props of
      [p] -> p
      pp -> GSimpleAndProp (GListProp pp)
  GSimpleOrProp (GListProp props) ->
    case groupProps "or" props of
      [p] -> p
      pp -> GSimpleOrProp (GListProp pp)
  GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent xs)]) prop -> case getExists kind prop of
    (ys, body) -> GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent (xs ++ ys))]) (aggregate body)
  GListHypo hypos -> GListHypo (aggregateHypos hypos)
  _ -> composOp aggregate t
 where
   aggregateHypos hypos = case hypos of
     GVarsHypo (GListIdent [x]) kind :
       GPropHypo (GAdjProp adj exp@(GTermExp (GTIdent y))) : hs | x == y ->
         GPropHypo (GKindProp exp (GAdjKind adj kind)) : aggregateHypos hs
     GPropHypo a : GPropHypo b : hs ->
       GPropHypo (aggregate (GSimpleAndProp (GListProp [a, b]))) : aggregateHypos hs
     h : hs -> aggregate h : aggregateHypos hs
     _ -> hypos

getAdjs :: [GProp] -> GExp -> Maybe ([GAdj], [GProp])
getAdjs props x = case props of
  GAdjProp adj y : pp | x == y -> do
    (adjs, ps) <- getAdjs pp x
    return (adj : adjs, ps)
  _ -> return ([], props)

getAdjArgs :: [GProp] -> GAdj -> Maybe ([GExp], [GProp])
getAdjArgs props a = case props of
  GAdjProp b y : pp | a == b -> do
    (exps, ps) <- getAdjArgs pp a
    return (y : exps, ps)
  _ -> return ([], props)

getExists :: GKind -> GProp -> ([GIdent], GProp)
getExists kind prop = case prop of
  GExistProp (GListArgKind [GIdentsArgKind k (GListIdent xs)]) body | k == kind ->
    case getExists kind body of
      (ys, bd) -> (xs ++ ys, bd)
  _ -> ([], prop)

getEquations :: [GProp] -> GTerm -> Maybe (GEquation, [GProp])
getEquations props b = case props of
  p@(GFormulaProp (GFEquation eq@(GEBinary lt c d))) : pp | c == b -> do
    case getEquations pp d of
      Nothing -> return (eq, pp)
      Just (eqs, ps) -> return (GEChain lt c eqs, ps)
  _ -> Nothing

-- group flattened conjuncts to aggregated sublists; conj :: String is "and" or "or"
groupProps :: String -> [GProp] -> [GProp]
groupProps conj = groups where
  groups props = case props of
    p@(GAdjProp a x) : pp ->
      case getAdjs pp x of
        Just (adjs@(_:_), ps) -> (GAdjProp (adjConj conj (GListAdj (a:adjs))) x) : groups ps
	_ -> case getAdjArgs pp a of
          Just (exps@(_:_), ps) -> (GAdjProp a (expConj conj (GListExp (x:exps)))) : groups ps
	  _ -> p : groups pp
    p@(GFormulaProp (GFEquation (GEBinary lt a b))) : pp ->
      case getEquations pp b of
        Just (eqs, ps) -> (GFormulaProp (GFEquation (GEChain lt a eqs))) : groups ps
	_ -> p : groups pp
    p : pp -> p : groups pp
    _ -> []
  adjConj conj = case conj of
    "and" -> GAndAdj
    "or" -> GOrAdj
  expConj conj = case conj of
    "and" -> GAndExp
    "or" -> GOrExp

flatten :: Tree a -> Tree a
flatten t = case t of
  GSimpleAndProp (GListProp props) -> case getAndProps props of
    Just ps -> GSimpleAndProp (GListProp ps)
    _ -> GSimpleAndProp (GListProp (map flatten props))
  GSimpleOrProp (GListProp props) -> case getOrProps props of
    Just ps -> GSimpleOrProp (GListProp ps)
    _ -> GSimpleOrProp (GListProp (map flatten props))
  _ -> composOp flatten t

getAndProps :: [GProp] -> Maybe [GProp]
getAndProps props = case props of
  GSimpleAndProp (GListProp ps):qs -> do
    pss <- getAndProps ps
    qss <- getAndProps qs
    return (pss ++ qss)
  prop : qs -> do
    qss <- getAndProps qs
    return (prop : qss)
  _ -> return []

getOrProps :: [GProp] -> Maybe [GProp]
getOrProps props = case props of
  GSimpleOrProp (GListProp ps):qs -> do
    pss <- getOrProps ps
    qss <- getOrProps qs
    return (pss ++ qss)
  prop : qs -> do
    qss <- getOrProps qs
    return (prop : qss)
  _ -> return []


variations :: Tree a -> [Tree a]
variations tree = case tree of
  GAxiomJmt label (GListHypo hypos) prop -> 
    let splits = [splitAt i hypos | i <- [0..length hypos]]
    in tree : [GAxiomJmt label (GListHypo hypos11) hypoprop |
          (hypos1, hypos2) <- splits,
	  hypos11 <- sequence (map variations hypos1),
	  prop2 <- variations prop,
	  hypoprop <- concatMap variations (hypoProp hypos2 prop2)
	  ]
  GVarsHypo (GListIdent xs) (GSetKind set) ->
    [tree, GLetDeclarationHypo (GDElem (GListTerm [GTIdent x | x <- xs]) (GSetTerm set))]
  GVarsHypo fs@(GListIdent [f]) (GFam2Kind fam@(LexFam "function_Fam") (GSetKind a) (GSetKind b)) ->
    [tree, GVarsHypo fs (GFam2Kind fam (GTermKind (GSetTerm a)) (GTermKind (GSetTerm b)))]
  GAllProp (GListArgKind [argkind]) prop ->
    tree : [GPostQuantProp prop exp | exp <- allExpVariations argkind]
  GExistProp (GListArgKind [argkind]) prop ->
    tree : [GPostQuantProp prop exp | exp <- existExpVariations argkind]
  GNotProp (GExistProp argkinds prop) ->
    tree : [GExistNoProp argkinds prop]
  GSimpleAndProp (GListProp [GFormulaProp (GFEquation (GEBinary lt a b)), GFormulaProp (GFEquation (GEBinary eq b' c))]) | b == b' ->
    tree : [GFormulaProp (GFEquation (GEChain lt a (GEBinary eq b c)))] ---- TODO: generalize to longer chains
  GSimpleIfProp a@(GFormulaProp fa) b@(GFormulaProp fb) ->
    tree : [GOnlyIfProp a b, GFormulaImpliesProp fa fb]
  GSimpleIfProp a b ->
    tree : [GOnlyIfProp a b ]
  _ -> composOpM variations tree

allExpVariations :: GArgKind -> [GExp]
allExpVariations argkind = case argkind of
  GIdentsArgKind kind (GListIdent [x]) -> [GEveryIdentKindExp x kind , GAllIdentsKindExp (GListIdent [x]) kind]
  GIdentsArgKind kind xs -> [GAllIdentsKindExp xs kind]
  _ -> []

existExpVariations :: GArgKind -> [GExp]
existExpVariations argkind = case argkind of
  GIdentsArgKind kind (GListIdent [x]) -> [GIndefIdentKindExp x kind, GSomeIdentsKindExp (GListIdent [x]) kind]
  GIdentsArgKind kind xs -> [GSomeIdentsKindExp xs kind]
  _ -> []

hypoProp :: [GHypo] -> GProp -> [GProp]
hypoProp hypos prop = case hypos of
  GPropHypo p : hs -> [GSimpleIfProp p q | q <- hypoProp hs prop]
  GVarsHypo xs k : hs -> [GAllProp (GListArgKind [GIdentsArgKind k xs]) q | q <- hypoProp hs prop]
  _:_ -> [] ---- TODO: prop for let hypos
--  h:hs -> PostHyposProp hypos prop
  [] -> [prop]


---- a very simple special case of in situ so far
insitu :: Tree a -> Tree a
insitu t = case t of
  GAllProp (GListArgKind [argkind]) (GAdjProp adj exp) -> case subst argkind exp of
    Just (x, kind) -> GAdjProp adj (GAllIdentsKindExp (GListIdent [x]) kind)
    _ -> t
  GAllProp (GListArgKind [argkind]) (GNotAdjProp adj exp) -> case subst argkind exp of
    Just (x, kind) -> GAdjProp adj (GNoIdentsKindExp (GListIdent [x]) kind)
    _ -> t
  GExistProp (GListArgKind [argkind]) (GAdjProp adj exp) -> case subst argkind exp of
    Just (x, kind) -> GAdjProp adj (GSomeIdentsKindExp (GListIdent [x]) kind)
    _ -> t
  _ -> composOp insitu t

subst :: GArgKind -> GExp -> Maybe (GIdent, GKind)
subst argkind exp = case (argkind, exp) of
  (GIdentsArgKind kind (GListIdent [x]), GTermExp (GTIdent y)) | x == y -> Just (x, kind)
  _ -> Nothing

varless :: Tree a -> Tree a
varless t = case t of
  GEveryIdentKindExp _ kind -> GEveryKindExp kind
  GAllIdentsKindExp (GListIdent [_]) kind -> GAllKindExp kind
  GNoIdentsKindExp (GListIdent [_]) kind -> GNoKindExp kind
  GSomeIdentsKindExp (GListIdent [_]) kind -> GSomeKindExp kind
  GIndefIdentKindExp _ kind -> GSomeKindExp kind
  _ -> composOp varless t

exps2list :: GExps -> [GExp]
exps2list exps = case exps of
  GOneExps e -> [e]
  GAddExps e es -> e : exps2list es

