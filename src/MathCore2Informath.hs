{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module MathCore2Informath where

import Informath
import Environment
import BuildConstantTable (nlgTable)
import Semantics (appNLGDefs)

import Data.List (nub)

type Opts = [String]

nlg :: Env -> GJmt -> [GJmt] --- Tree a -> [Tree a]
nlg env tree = case () of
  _ | elem "-mathcore" (flags env) -> [t]
  _  -> sample (concat [[ft], afts, iafts, viafts, cviafts, ncviafts, vncviafts, uservariants])
  ---- TODO more option combinations
 where
   t = unparenth tree
   ut = uncoerce t
   ft = flatten ut
   afts = [aggregate ft]
   iafts = map insitu afts
   viafts = map varless iafts
   cviafts = concatMap collectivize viafts
   ncviafts = map negated cviafts  -- better do this at this late stage
---   vncviafts = if isFlag "-more-variants" env then concatMap variations ncviafts else []
   vncviafts = concatMap variations ncviafts
   uservariants = concatMap (appNLGDefs (nlgTable (symbolTable env))) vncviafts

   sample ts = [t | (t, i) <- zip ts [0 ..], mod i fact == 0]
   fact = samplingFactor env

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
  GCoercionExp _ exp -> uncoerce exp
  _ -> composOp uncoerce t


---- also in DMC, IMC
gExps :: [GExp] -> GExps
gExps exps = case exps of
  [exp] -> GOneExps exp
  _ -> GManyExps (GListExp exps)


aggregate :: Tree a -> Tree a
aggregate t = case t of
  GCoreNotProp prop -> case aggregate prop of
    GAdjProp adj x -> GNotAdjProp adj x
    aprop -> GCoreNotProp aprop
  GAndProp (GListProp props) ->
    case groupProps "and" props of
      [p] -> p
      pp -> GAndProp (GListProp pp)
  GOrProp (GListProp props) ->
    case groupProps "or" props of
      [p] -> p
      pp -> GOrProp (GListProp pp)
  GCoreAllProp kind x prop -> case getAlls kind prop of
    (ys, body) -> GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent (x : ys))]) (aggregate body)
  GCoreExistProp kind x prop -> case getExists kind prop of
    (ys, body) -> GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent (x : ys))]) (aggregate body)
  GListHypo hypos -> GListHypo (aggregateHypos hypos)
  _ -> composOp aggregate t

 where
   aggregateHypos hypos = case hypos of
     GVarsHypo xs@(GListIdent [x]) kind :
       GPropHypo (GAdjProp adj (GTermExp (GIdentTerm y))) : hs | x == y ->
         GAdjKindHypo xs adj kind : aggregateHypos hs
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
  GCoreExistProp k x body | k == kind ->
    case getExists kind body of
      (ys, bd) -> (x : ys, bd)
  _ -> ([], prop)
  
getAlls :: GKind -> GProp -> ([GIdent], GProp)
getAlls kind prop = case prop of
  GCoreAllProp k x body | k == kind ->
    case getAlls kind body of
      (ys, bd) -> (x : ys, bd)
  _ -> ([], prop)

getEquations :: [GProp] -> GTerm -> Maybe (GEquation, [GProp])
getEquations props b = case props of
  (GFormulaProp (GEquationFormula eq@(GBinaryEquation lt c d))) : pp | c == b -> do
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
        Just (eqs, ps) | conj == "and" -> (GFormulaProp (GEquationFormula (GChainEquation lt a eqs))) : groups ps
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
  GVarsHypo (GListIdent xs) (GExpKind (GTermExp term)) ->
    [tree, GLetDeclarationHypo (GElemDeclaration (GListTerm [GIdentTerm x | x <- xs]) term)]
  GAllProp (GListArgKind [argkind]) prop ->
    tree : [GPostQuantProp prop exp | exp <- allQuantVariations argkind]
  GExistProp (GListArgKind [argkind]) prop ->
    tree : [GPostQuantProp prop exp | exp <- existQuantVariations argkind]
  GCoreNotProp (GExistProp argkinds prop) ->
    tree : [GExistNoProp argkinds prop]
  GIfProp a@(GFormulaProp fa) b@(GFormulaProp fb) ->
    tree : [GOnlyIfProp a b, GFormulaImpliesProp fa fb]

  GApp4MacroTerm (GStringMacro (GString "\\Summa")) m n (GIdentTerm i) f ->
    let m1s = case m of
         GNumberTerm (GInt m) -> [GNumberTerm (GInt (m + 1))]
         _ -> [GOper2Term (LexOper2 "plus_Oper2") m (GNumberTerm (GInt 1))]
             --- not to be included with GInt m
    in tree : [Gsum3dots_Term (substTerm i m f) (substTerm i m1 f) (substTerm i n f) | m1 <- m1s]

  GFormulaProp formula ->
    tree : [GDisplayFormulaProp f | f <- variations formula, hasDisplaySize f]
    -- ifNeeded tree [GDisplayFormulaProp f | f <- variations formula, hasDisplaySize f]

  GOper2Term (LexOper2 "times_Oper2") x y ->
    tree : [Gtimes_Term vx vy | vx <- variations x, vy <- variations y]

--- moved to extrasemantics.dkgf 16/7/2026
---  GIfProp a b ->
---    tree : [GOnlyIfProp a b ]
---  GKindExp kind -> tree : [GPluralKindExp k | k <- kind : variations kind]
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

  _ -> composOpM variations tree


hasDisplaySize :: Tree a -> Bool
hasDisplaySize = not . null . includesThese where
  includesThese :: Tree a -> [Tree a]
  includesThese t = case t of
    GApp3MacroTerm _ _ _ _   -> [t]
    GApp4MacroTerm _ _ _ _ _  -> [t]
    Gsum3dots_Term _ _ _ -> [t]
    _ -> composOpM includesThese t


ifNeeded :: a -> [a] -> [a]
ifNeeded given alts = case alts of
  [] -> [given]
  _ -> alts

allQuantVariations :: GArgKind -> [GQuant]
allQuantVariations argkind = case argkind of
  GIdentsArgKind kind (GListIdent [x]) -> [GEveryIdentKindQuant x kind]
    --- , GAllIdentsKindQuant (GListIdent [x]) kind]
    --- can give "all numbers are even or odd"
  GIdentsArgKind kind xs -> [GAllIdentsKindQuant xs kind]
  _ -> []

existQuantVariations :: GArgKind -> [GQuant]
existQuantVariations argkind = case argkind of
  GIdentsArgKind kind (GListIdent [x]) -> [GSomeIdentsKindQuant (GListIdent [x]) kind]
  --- , GIndefIdentKindQuant x kind]
  --- gives potential ambiguities with "a"
  GIdentsArgKind kind xs -> [GSomeIdentsKindQuant xs kind]
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
    Just (x, kind) -> GAdjProp adj (GQuantExp (GEveryIdentKindQuant x kind))
    _ -> t
  GAllProp (GListArgKind [argkind]) (GCoreNotProp (GAdjProp adj exp)) -> case subst argkind exp of
    Just (x, kind) -> GAdjProp adj (GQuantExp (GNoIdentsKindQuant (GListIdent [x]) kind))
    _ -> t
  GCoreNotProp (GExistProp (GListArgKind [argkind]) (GAdjProp adj exp)) -> case subst argkind exp of
    Just (x, kind) -> GAdjProp adj (GQuantExp (GNoIdentsKindQuant (GListIdent [x]) kind))
    _ -> t
  GExistProp (GListArgKind [argkind]) (GAdjProp adj exp) -> case subst argkind exp of
    Just (x, kind) -> GAdjProp adj (GQuantExp (GSomeIdentsKindQuant (GListIdent [x]) kind))
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
  GEveryIdentKindQuant _ kind -> GEveryKindQuant kind
  GAllIdentsKindQuant (GListIdent [_]) kind -> GAllKindQuant kind
  GNoIdentsKindQuant (GListIdent [_]) kind -> GNoKindQuant kind
  GSomeIdentsKindQuant (GListIdent [_]) kind -> GSomeKindQuant kind
  GIndefIdentKindQuant _ kind -> GIndefKindQuant kind
  GPropVarHypo _ prop -> GPropHypo prop
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

collectivize :: Tree a -> [Tree a]
collectivize t = case t of

  -- put together instances of an equivalence relation that have common elements
  GAndProp (GListProp props) -> maybe [t] return $ do
    (adjc, expss) <- commonRel props
    let nexps = GListExp (nub expss) 
    return $ GAdjECollProp adjc nexps

  -- put together arguments of collective functions
  GFunCExp func x y -> do
    let args = collectArgs func [x, y]
    let margs = GListExp args
    return $ GFunCCollExp func margs

  _ -> composOpM collectivize t
  
 where
   commonRel :: [GProp] -> Maybe (GAdjE, [GExp])
   commonRel props = case props of
     GAdjEProp adjc x y : [] ->
       return (adjc, [x, y])
     GAdjEProp adjc x y : pp -> do
       (adjc2, expss) <- commonRel pp
       let lexp = [x, y]
       if adjc2 == adjc && any (flip elem expss) lexp
         then return (adjc, lexp ++ expss)
         else Nothing
     _ -> Nothing

   collectArgs :: GFunC -> [GExp] -> [GExp]
   collectArgs func exps = case exps of
     GFunCExp f x y : ee | f == func ->
       collectArgs func ([x, y] ++ ee)
     exp : ee -> exp : collectArgs func ee ---- TODO collectivize exp ?
     [] -> []


---- TODO: move more of negation from earlier stages here
negated :: Tree a -> Tree a
negated t = case t of
  GCoreNotProp (GAdjProp adj x) -> GNotAdjProp adj x
  GCoreNotProp (GAdj2Prop adj x y) -> GNotAdj2Prop adj x y
  GCoreNotProp (GAdjCProp adj x y) -> GNotAdjCProp adj (GListExp [x, y])
  GCoreNotProp (GAdjEProp adj x y) -> GNotAdjEProp adj (GListExp [x, y])
  GCoreNotProp (GNoun1Prop adj x) -> GNotNoun1Prop adj x
  GCoreNotProp (GNoun2Prop adj x y) -> GNotNoun2Prop adj x y
  GCoreNotProp (GVerbProp adj x) -> GNotVerbProp adj x
  GCoreNotProp (GVerb2Prop adj x y) -> GNotVerb2Prop adj x y
  GCoreNotProp (GAdvProp adv x) -> GNotAdvProp adv x
  GCoreNotProp (GAdv2Prop adv x y) -> GNotAdv2Prop adv x y
  GCoreNotProp (GAdvCProp adv x y) -> GNotAdvCProp adv (GListExp [x, y])
  _ -> composOp negated t

