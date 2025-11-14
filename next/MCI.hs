{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module MCI where
--module Core2Informath where

import NextInformath
import Environment4
import Utils

import Data.List (nub, sortOn)
import Data.Char (isDigit)
import qualified Data.Map as M

type Opts = [String]

nlg :: Env -> Tree a -> [Tree a]
nlg env tree = case () of
  _ | elem "-mathcore" (flags env) -> [tree]
  _  -> concat [[ft, t, tree], sts, afts, iafts, viafts, cviafts, ncviafts, vncviafts]
  ---- TODO more option combinations
 where
   t = unparenth tree
   ut = uncoerce t
   ft = flatten ut
   sts = synonyms env ut
   afts = map aggregate sts
   iafts = map insitu afts
   viafts = map varless iafts
   cviafts = concatMap collectivize viafts
   ncviafts = map negated cviafts  -- better do this at this late stage
   vncviafts = concatMap variations ncviafts

unparenth :: Tree a -> Tree a
unparenth t = case t of
  GCoreAndProp a b -> GAndProp (GListProp (map unparenth [a, b]))
  GCoreOrProp a b -> GOrProp (GListProp (map unparenth [a, b]))
  GCoreIfProp a b -> GIfProp (unparenth a) (unparenth b)
  GCoreIffProp a b -> GIffProp (unparenth a) (unparenth b)
  _ -> composOp unparenth t

uncoerce :: Tree a -> Tree a
uncoerce t = case t of
  GProofProp prop -> uncoerce prop
  GElemKind kind -> uncoerce kind
  GCoercionExp coercion_ exp -> uncoerce exp
  _ -> composOp uncoerce t

synonyms :: forall a. Env -> Tree a -> [Tree a]
synonyms env t = symbs t ++ verbs t where

  symbs :: forall a. Tree a -> [Tree a]
  symbs t = case t of
    GAdjCProp (LexAdjC c) exps ->
      case exps2list exps of  --- currently 2 args from Dedukti ---- but not always
         x:y:_ -> [sympred alt [sx, sy] | alt <- ssyns c, sx <- terms x, sy <- terms y]
    GAdjEProp (LexAdjE c) exps ->
      case exps2list exps of  --- always 2 args from Dedukti
         x:y:_ -> [sympred alt [sx, sy] | alt <- ssyns c, sx <- terms x, sy <- terms y]
    GAdj2Prop (LexAdj2 c) x y ->
      [sympred alt [sx, sy] | alt <- ssyns c, sx <- terms x, sy <- terms y]
    GFun2Exp _ _ _ -> map GTermExp (terms t)
    GFunCExp _ _  -> map GTermExp (terms t)
    GFunExp _ _ -> map GTermExp (terms t)
    _ -> composOpM symbs t

  terms :: GExp -> [GTerm]
  terms exp = case exp of
    GFunCExp (LexFunC c) exps ->
      case exps2list exps of  --- always 2 args from Dedukti
         x:y:_ -> [app alt [sx, sy] | alt <- ssyns c, sx <- terms x, sy <- terms y]
    GFun2Exp (LexFun2 c) x y ->
      [app alt [sx, sy] | alt <- ssyns c, sx <- terms x, sy <- terms y]
    GFunExp (LexFun c) x ->
      [app alt [sx] | alt <- ssyns c, sx <- terms x]
    GNameExp (LexName c) ->
      [app alt [] | alt <- ssyns c]
    GTermExp t -> [t]
    _ -> []
      

  ssyns c = maybe [] fst (M.lookup c (synonymConstantTableNLG env))
  
  verbs :: forall a. Tree a -> [Tree a]
  verbs t = case t of
----    GAdj2Prop (LexAdj2 c) x y ->
----      [pred alt [sx, sy] | alt <- vsyns c, sx <- verbs x, sy <- verbs y]
    GFun2Exp _ _ _ -> t : map GTermExp (terms t) ---- also verbal synonyms
    GFunCExp _ _  -> t : map GTermExp (terms t)
    GFunExp _ _ -> t : map GTermExp (terms t)
    _ -> composOpM verbs t

  vsyns c = maybe [] snd (M.lookup c (synonymConstantTableNLG env))
 
  pred (fun, cat) xs = case cat of
    "Adj2" -> GAdj2Prop (LexAdj2 fun) (xs !! 0) (xs !! 1)
    "AdjC" -> GAdjCProp (LexAdjC fun) (gExps xs)
    "AdjE" -> GAdjEProp (LexAdjE fun) (gExps xs)
    _ -> error $ "NOT YET: " ++ show cat

  sympred (fun, cat) xs = case cat of
    "Compar" -> GFormulaProp (GEquationFormula
                  (GBinaryEquation (LexCompar fun) (xs !! 0) (xs !! 1)))
    _ -> error $ "NOT YET: " ++ show cat
 
  app (fun, cat) xs = case cat of
    "Const" -> GConstTerm (LexConst fun)
    "Oper" -> GOperTerm (LexOper fun) (xs !! 0)
    "Oper2" -> GOper2Term (LexOper2 fun) (xs !! 0) (xs !! 1)
    _ -> error $ "NOT YET: " ++ show cat
     
  ---- also in DMC, IMC
  gExps :: [GExp] -> GExps
  gExps exps = case exps of
    [exp] -> GOneExps exp
    _ -> GManyExps (GListExp exps)

  

aggregate :: Tree a -> Tree a
aggregate t = case t of
  GNotProp prop -> case aggregate prop of
    GAdjProp adj x -> GNotAdjProp adj x
    aprop -> GNotProp aprop
  GAndProp (GListProp props) ->
    case groupProps "and" props of
      [p] -> p
      pp -> GAndProp (GListProp pp)
  GOrProp (GListProp props) ->
    case groupProps "or" props of
      [p] -> p
      pp -> GOrProp (GListProp pp)
  GCoreAllProp x kind prop -> case getAlls kind prop of
    (ys, body) -> GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent (x : ys))]) (aggregate body)
  GCoreExistProp x kind prop -> case getExists kind prop of
    (ys, body) -> GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent (x : ys))]) (aggregate body)
  GListHypo hypos -> GListHypo (aggregateHypos hypos)
  _ -> composOp aggregate t

 where
   aggregateHypos hypos = case hypos of
     GVarsHypo (GListIdent [x]) kind :
       GPropHypo (GAdjProp adj exp@(GTermExp (GIdentTerm y))) : hs | x == y ->
         GPropHypo (GKindProp exp (GAdjKind adj kind)) : aggregateHypos hs
     GPropHypo a : GPropHypo b : hs ->
       GPropHypo (aggregate (GAndProp (GListProp [a, b]))) : aggregateHypos hs
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
  GCoreExistProp x k body | k == kind ->
    case getExists kind body of
      (ys, bd) -> (x : ys, bd)
  _ -> ([], prop)
  
getAlls :: GKind -> GProp -> ([GIdent], GProp)
getAlls kind prop = case prop of
  GCoreAllProp x k body | k == kind ->
    case getAlls kind body of
      (ys, bd) -> (x : ys, bd)
  _ -> ([], prop)

getEquations :: [GProp] -> GTerm -> Maybe (GEquation, [GProp])
getEquations props b = case props of
  p@(GFormulaProp (GEquationFormula eq@(GBinaryEquation lt c d))) : pp | c == b -> do
    case getEquations pp d of
      Nothing -> return (eq, pp)
      Just (eqs, ps) -> return (GChainEquation lt c eqs, ps)
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
    p@(GFormulaProp (GEquationFormula (GBinaryEquation lt a b))) : pp ->
      case getEquations pp b of
        Just (eqs, ps) -> (GFormulaProp (GEquationFormula (GChainEquation lt a eqs))) : groups ps
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
  GAndProp (GListProp props) -> case getAndProps props of
    Just ps -> GAndProp (GListProp ps)
    _ -> GAndProp (GListProp (map flatten props))
  GOrProp (GListProp props) -> case getOrProps props of
    Just ps -> GOrProp (GListProp ps)
    _ -> GOrProp (GListProp (map flatten props))
  _ -> composOp flatten t

getAndProps :: [GProp] -> Maybe [GProp]
getAndProps props = case props of
  GAndProp (GListProp ps):qs -> do
    pss <- getAndProps ps
    qss <- getAndProps qs
    return (pss ++ qss)
  prop : qs -> do
    qss <- getAndProps qs
    return (prop : qss)
  _ -> return []

getOrProps :: [GProp] -> Maybe [GProp]
getOrProps props = case props of
  GOrProp (GListProp ps):qs -> do
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
  GVarsHypo (GListIdent xs) (GTermKind term) ->
    [tree, GLetDeclarationHypo (GElemDeclaration (GListTerm [GIdentTerm x | x <- xs]) term)]
----  GVarsHypo fs@(GListIdent [f]) (GFam2Kind fam@(LexFam "function_Fam") (GSetKind a) (GSetKind b)) ->
----    [tree, GVarsHypo fs (GFam2Kind fam (GTermKind (GSetTerm a)) (GTermKind (GSetTerm b)))]
  GAllProp (GListArgKind [argkind]) prop ->
    tree : [GPostQuantProp prop exp | exp <- allExpVariations argkind]
  GExistProp (GListArgKind [argkind]) prop ->
    tree : [GPostQuantProp prop exp | exp <- existExpVariations argkind]
  GNotProp (GExistProp argkinds prop) ->
    tree : [GExistNoProp argkinds prop]
----  GAndProp (GListProp [GFormulaProp (GFEquation (GEBinary lt a b)), GFormulaProp (GFEquation (GEBinary eq b' c))]) | b == b' ->
----    tree : [GFormulaProp (GFEquation (GEChain lt a (GEBinary eq b c)))] ---- TODO: generalize to longer chains
  GIfProp a@(GFormulaProp fa) b@(GFormulaProp fb) ->
    tree : [GOnlyIfProp a b, GFormulaImpliesProp fa fb]
  GIfProp a b ->
    tree : [GOnlyIfProp a b ]
  GAndProp (GListProp [a, b]) ->
    tree : [GBothAndProp va vb | va <- variations a, vb <- variations b]
  GAndAdj (GListAdj [a, b]) ->
    tree : [GBothAndAdj va vb | va <- variations a, vb <- variations b]
  GAndExp (GListExp [a, b]) ->
    tree : [GBothAndExp va vb | va <- variations a, vb <- variations b]
  GOrProp (GListProp [a, b]) ->
    tree : [GEitherOrProp va vb | va <- variations a, vb <- variations b]
  GOrAdj (GListAdj [a, b]) ->
    tree : [GEitherOrAdj va vb | va <- variations a, vb <- variations b]
  GOrExp (GListExp [a, b]) ->
    tree : [GEitherOrExp va vb | va <- variations a, vb <- variations b]

  GSigmaExp i (GTermExp m) (GTermExp n) (GTermExp f) ->
    tree : [GTermExp term | term <- variations (Gsigma_Term i m n f)]
  GSeriesExp i (GTermExp m) (GTermExp f) ->
    tree : [GTermExp term | term <- variations (Gseries_Term i m f)]
  GIntegralExp i (GTermExp m) (GTermExp n) (GTermExp f) ->
    tree : [GTermExp term | term <- variations (Gintegral_Term i m n f)]
  
  Gsigma_Term i m n f ->
    let m1s = case m of
                GNumberTerm (GInt m) -> [GNumberTerm (GInt (m + 1))]
	        _ -> [GOper2Term (LexOper2 "plus_Oper2") m (GNumberTerm (GInt 1))]  --- not to be included with GInt m 
    in tree : [Gsum3dots_Term (substTerm i m f) (substTerm i m1 f) (substTerm i n f) | m1 <- m1s]
  GFormulaProp formula ->
    ifNeeded tree [GDisplayFormulaProp f | f <- variations formula, hasDisplaySize f]

  _ -> composOpM variations tree


hasDisplaySize :: Tree a -> Bool
hasDisplaySize = not . null . includesThese where
  includesThese :: Tree a -> [Tree a]
  includesThese t = case t of
    Gsigma_Term _ _ _ _ -> [t]
    Gseries_Term _ _ _ -> [t]
    Gintegral_Term _ _ _ _ -> [t]
    Gsum3dots_Term _ _ _ -> [t]
    _ -> composOpM includesThese t


ifNeeded :: a -> [a] -> [a]
ifNeeded given alts = case alts of
  [] -> [given]
  _ -> alts

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
  GPropHypo p : hs -> [GIfProp p q | q <- hypoProp hs prop]
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
  (GIdentsArgKind kind (GListIdent [x]), GTermExp (GIdentTerm y)) | x == y -> Just (x, kind)
  _ -> Nothing

substTerm :: GIdent -> GTerm -> Tree a -> Tree a
substTerm x val body = case body of
  GIdentTerm y | y == x -> val
  _ -> composOp (substTerm x val) body

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
  GManyExps (GListExp es) -> es

list2mexps :: [GExp] -> Maybe GExps
list2mexps exps = case exps of
  [e] -> return $ GOneExps e
  _ : _ -> return $ GManyExps (GListExp exps)
  [] -> Nothing

flattenExps :: [GExps] -> Maybe GExps -- Nothing for empty list
flattenExps = list2mexps .  concatMap exps2list

collectivize :: Tree a -> [Tree a]
collectivize t = case t of

  -- put together instances of an equivalence relation that have common elements
  GAndProp (GListProp props) -> maybe [t] return $ do
    (adjc, expss) <- commonRel props
    nexps <- list2mexps $ nub $ expss 
    return $ GAdjEProp adjc nexps

  -- put together arguments of collective functions
  GFunCExp func exps -> do
    let args = collectArgs func (exps2list exps)
    let Just margs = list2mexps args
    return $ GFunCExp func margs

  _ -> composOpM collectivize t
  
 where
   commonRel :: [GProp] -> Maybe (GAdjE, [GExp])
   commonRel props = case props of
     GAdjEProp adjc exps : [] ->
       return (adjc, exps2list exps)
     GAdjEProp adjc exps : pp -> do
       (adjc2, expss) <- commonRel pp
       let lexp = exps2list exps
       if adjc2 == adjc && any (flip elem expss) lexp
         then return (adjc, lexp ++ expss)
         else Nothing
     _ -> Nothing

   collectArgs :: GFunC -> [GExp] -> [GExp]
   collectArgs func exps = case exps of
     GFunCExp f xs : ee | f == func ->
       collectArgs func (exps2list xs ++ ee)
     exp : ee -> exp : collectArgs func ee ---- TODO collectivize exp ?
     [] -> []


---- TODO: move more of negation from earlier stages here
negated :: Tree a -> Tree a
negated t = case t of
  GNotProp (GAdjEProp adj exps) -> GNotAdjEProp adj exps
  _ -> composOp negated t

