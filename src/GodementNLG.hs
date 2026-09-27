{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- Godement-style variants in natural language generation, with the flag -godement.
-- The inverse of GodementSemantics: from the trees that MathCore2Informath produces,
-- build trees with the constructions of GodementSyntax, as Godement would say them:
--
--   Let g be a mathematical object. Assume that g is a group.
--     ==> Let g be a group.
--   g is commutative, if g is a group and P
--     ==> a group g is said to be commutative if P
--   N is a normal subgroup of g, if N is a subgroup of g and P
--     ==> a subgroup N of g is said to be normal if P
--   E is a mathematical object defined as F
--     ==> E is defined to be F
--   for all mathematical objects s, if s is an element of G, then P
--     ==> for all elements s of G, P
--   x is a group and x is commutative
--     ==> x is a commutative group
--   P(x) if and only if Q
--     ==> for x to be P it is necessary and sufficient that Q
--
-- All combinations of these are returned; the caller chooses among them, e.g. the
-- one closest to Godement's own sentence.

module GodementNLG (godementVariants) where

import Informath
import Data.List (nub)

godementVariants :: GJmt -> [GJmt]
godementVariants jmt = nub (foldl step [jmt] transformations)
  where
    step ts f = ts ++ [t' | t <- ts, Just t' <- [f t], notElem t' ts]
    transformations = [typedHypos, typedDefinition, calledDefinition, definedExp, typedQuantifiers, adjKinds, necSuff]

------------------------------------------------------------
-- typed variables: "let g be a mathematical object. assume that g is a group"

typedHypos :: GJmt -> Maybe GJmt
typedHypos = onHypos typeHypos

typeHypos :: [GHypo] -> Maybe [GHypo]
typeHypos hypos =
  let hs = concatMap splitPropHypo hypos
      typings = [(x, k) | GPropHypo p <- hs, Just (x, k) <- [typing p]]
      objs = concat [xs | GVarsHypo (GListIdent xs) k <- hs, isObjKind k]
      typed = [(x, k) | (x, k) <- typings, elem x objs]
      firstTyped = nubFirst [(x, k) | (x, k) <- typed]
  in if null firstTyped then Nothing
     else Just (group (concatMap (retype firstTyped) hs))
 where
   retype :: [(GIdent, GKind)] -> GHypo -> [GHypo]
   retype typed h = case h of
     GVarsHypo (GListIdent xs) k | isObjKind k ->
       case [x | x <- xs, notElem x (map fst typed)] of
         [] -> []
         ys -> [GVarsHypo (GListIdent ys) k]
     GPropHypo p | Just (x, k) <- typing p, lookup x typed == Just k -> [GVarHypo x k]
     _ -> [h]
   nubFirst :: [(GIdent, GKind)] -> [(GIdent, GKind)]
   nubFirst xks = [(x, k) | ((x, k), i) <- zip xks [0 :: Int ..], notElem x (map fst (take i xks))]
   -- consecutive variables of the same kind: "let g and h be groups"
   group :: [GHypo] -> [GHypo]
   group hs = case hs of
     GVarHypo x k : rest ->
       let (ys, rest') = sameKind k rest
       in (if null ys then GVarHypo x k else GVarsHypo (GListIdent (x : ys)) k) : group rest'
     h : rest -> h : group rest
     [] -> []
   sameKind :: GKind -> [GHypo] -> ([GIdent], [GHypo])
   sameKind k hs = case hs of
     GVarHypo y k' : rest | k' == k -> let (ys, r) = sameKind k rest in (y : ys, r)
     _ -> ([], hs)

splitPropHypo :: GHypo -> [GHypo]
splitPropHypo h = case h of
  GPropHypo (GAndProp (GListProp ps)) -> map GPropHypo ps
  _ -> [h]

-- a proposition that says what kind of thing a variable is
typing :: GProp -> Maybe (GIdent, GKind)
typing p = case p of
  GNoun1Prop (GNounNoun1 n) (GTermExp (GIdentTerm x)) -> Just (x, GNounKind n)
  GNoun2Prop n2 (GTermExp (GIdentTerm x)) y | Just k <- noun2Kind n2 y -> Just (x, k)
  GNoun3Prop (GNounPrepsNoun3 n p1 p2) (GTermExp (GIdentTerm x)) y z ->
    Just (x, GDep2Kind (GNounPrepDep2 n p1 p2) y z)
  _ -> Nothing

noun2Kind :: GNoun2 -> GExp -> Maybe GKind
noun2Kind n2 y = case n2 of
  GNounPrepNoun2 n p -> Just (GDepKind (GNounPrepDep n p) y)
  GAdjNoun2Noun2 a n2' -> fmap (GAdjKind a) (noun2Kind n2' y)
  _ -> Nothing

isObjKind :: GKind -> Bool
isObjKind k = case k of
  GNounKind (GAdjNounNoun (LexAdj "mathematical_Adj") (LexNoun "object_Noun")) -> True
  _ -> False

------------------------------------------------------------
-- definitions of predicates, with the type of the subject taken from the definiens

typedDefinition :: GJmt -> Maybe GJmt
typedDefinition jmt = case jmt of
  -- "a group g is said to be commutative if P"
  GDefPropJmt label (GListHypo hs) (GAdjProp adj x@(GTermExp (GIdentTerm v))) df
    | Just (k, rest) <- typingConjunct v df ->
        Just (GDefAdjJmt label (GListHypo (dropVar v hs)) (indef v k) Gsaid_DefVerb adj rest)
  -- "a subgroup N of g is said to be normal if P"
  GDefPropJmt label (GListHypo hs) (GNoun2Prop (GAdjNoun2Noun2 adj n2) (GTermExp (GIdentTerm v)) y) df
    | Just k <- noun2Kind n2 y, Just rest <- removeConjunct (GNoun2Prop n2 (var v) y) df ->
        Just (GDefAdjJmt label (GListHypo (dropVar v hs)) (indef v k) Gsaid_DefVerb adj rest)
  -- "x is said to be a K if P"
  GDefPropJmt label hs p df | Just (x, k) <- predicateKind p ->
        Just (GDefNounJmt label hs x Gsaid_DefVerb k df)
  GDefPropJmt label hs (GAdjProp adj x) df ->
        Just (GDefAdjJmt label hs x Gsaid_DefVerb adj df)
  _ -> Nothing
 where
   indef :: GIdent -> GKind -> GExp
   indef v k = GQuantExp (GIndefIdentKindQuant v k)

-- "x is a K" as a predication with a noun
predicateKind :: GProp -> Maybe (GExp, GKind)
predicateKind p = case p of
  GNoun1Prop (GNounNoun1 n) x -> Just (x, GNounKind n)
  GNoun2Prop n2 x y | Just k <- noun2Kind n2 y -> Just (x, k)
  GNoun3Prop (GNounPrepsNoun3 n p1 p2) x y z -> Just (x, GDep2Kind (GNounPrepDep2 n p1 p2) y z)
  _ -> Nothing

-- the conjunct of a definiens that types variable v, and the rest
typingConjunct :: GIdent -> GProp -> Maybe (GKind, GProp)
typingConjunct v df = case conjuncts df of
  ps -> case [(k, p) | p <- ps, Just (x, k) <- [typing p], x == v] of
    (k, p) : _ | length ps > 1 -> Just (k, mkAnd [q | q <- ps, q /= p])
    _ -> Nothing

removeConjunct :: GProp -> GProp -> Maybe GProp
removeConjunct p df = case conjuncts df of
  ps | elem p ps && length ps > 1 -> Just (mkAnd [q | q <- ps, q /= p])
  _ -> Nothing

conjuncts :: GProp -> [GProp]
conjuncts p = case p of
  GAndProp (GListProp ps) -> concatMap conjuncts ps
  GCoreAndProp a b -> conjuncts a ++ conjuncts b
  _ -> [p]

mkAnd :: [GProp] -> GProp
mkAnd ps = case ps of
  [p] -> p
  _ -> GAndProp (GListProp ps)

dropVar :: GIdent -> [GHypo] -> [GHypo]
dropVar v hs = concatMap drop1 hs
 where
   drop1 :: GHypo -> [GHypo]
   drop1 h = case h of
     GVarsHypo (GListIdent xs) k -> case filter (/= v) xs of
       [] -> []
       ys -> [GVarsHypo (GListIdent ys) k]
     GVarHypo x _ | x == v -> []
     _ -> [h]

var :: GIdent -> GExp
var v = GTermExp (GIdentTerm v)

-- "x is called A if P", as a variant of "x is said to be A if P"
calledDefinition :: GJmt -> Maybe GJmt
calledDefinition jmt = case jmt of
  GDefAdjJmt l hs x Gsaid_DefVerb a p -> Just (GDefAdjJmt l hs x Gcalled_DefVerb a p)
  GDefNounJmt l hs x Gsaid_DefVerb k p -> Just (GDefNounJmt l hs x Gcalled_DefVerb k p)
  _ -> Nothing

------------------------------------------------------------
-- definitions of expressions: "E is defined to be F"

definedExp :: GJmt -> Maybe GJmt
definedExp jmt = case jmt of
  GDefExpJmt label hs e _ df -> Just (GNamingJmt label hs e Gdefined_DefVerb df)
  _ -> Nothing

------------------------------------------------------------
-- quantifiers over mathematical objects restricted by a typing:
-- "for all elements s of G, P", "there exists an element x of G such that P"

typedQuantifiers :: GJmt -> Maybe GJmt
typedQuantifiers jmt = let jmt' = tq jmt in if jmt' == jmt then Nothing else Just jmt'

tq :: Tree a -> Tree a
tq t = case t of
  GCoreAllProp k x (GCoreIfProp p body) | Just k' <- retyped k x p -> allProp k' x body
  GCoreAllProp k x (GIfProp p body) | Just k' <- retyped k x p -> allProp k' x body
  GCoreExistProp k x body
    | isObjKind k, p : rest@(_:_) <- conjuncts body, Just (y, k') <- typing p, y == x ->
        GExistProp (GListArgKind [GIdentsArgKind k' (GListIdent [x])]) (tq (mkAnd rest))
  _ -> composOp tq t
 where
   retyped :: GKind -> GIdent -> GProp -> Maybe GKind
   retyped k x p = case typing p of
     Just (y, k') | isObjKind k && y == x -> Just k'
     _ -> Nothing
   allProp :: GKind -> GIdent -> GProp -> GProp
   allProp k' x body = GAllProp (GListArgKind [GIdentsArgKind k' (GListIdent [x])]) (tq body)

------------------------------------------------------------
-- "x is a group and x is commutative" ==> "x is a commutative group"

adjKinds :: GJmt -> Maybe GJmt
adjKinds jmt = let jmt' = adjKind jmt in if jmt' == jmt then Nothing else Just jmt'

adjKind :: Tree a -> Tree a
adjKind t = case t of
  GAndProp (GListProp ps) -> case merge ps of
    [p] -> p
    ps' -> GAndProp (GListProp ps')
  GCoreAndProp a b -> case merge [a, b] of
    [p] -> p
    [p, q] -> GCoreAndProp p q
    ps' -> GAndProp (GListProp ps')
  _ -> composOp adjKind t
 where
   merge :: [GProp] -> [GProp]
   merge ps = case ps of
     p : q : rest
       | Just (x, k) <- predicateKind p, GAdjProp a y <- q, x == y ->
           GKindProp x (GAdjKind a k) : merge rest
       | GAdjProp a y <- p, Just (x, k) <- predicateKind q, x == y ->
           GKindProp x (GAdjKind a k) : merge rest
     p : rest -> adjKind p : merge rest
     [] -> []

------------------------------------------------------------
-- "for x to be A it is necessary and sufficient that Q"

necSuff :: GJmt -> Maybe GJmt
necSuff jmt = let jmt' = ns jmt in if jmt' == jmt then Nothing else Just jmt'
 where
   ns :: Tree a -> Tree a
   ns t = case t of
     GIffProp (GAdjProp a x@(GTermExp (GIdentTerm _))) q ->
       GNecSuffProp Gnecsuff_Condition x (GAdjPred a) (ns q)
     _ -> composOp ns t

------------------------------------------------------------

onHypos :: ([GHypo] -> Maybe [GHypo]) -> GJmt -> Maybe GJmt
onHypos f jmt = case jmt of
  GAxiomJmt l (GListHypo hs) p -> fmap (\hs' -> GAxiomJmt l (GListHypo hs') p) (f hs)
  GThmJmt l (GListHypo hs) p pr -> fmap (\hs' -> GThmJmt l (GListHypo hs') p pr) (f hs)
  GDefPropJmt l (GListHypo hs) p q -> fmap (\hs' -> GDefPropJmt l (GListHypo hs') p q) (f hs)
  GDefExpJmt l (GListHypo hs) e k d -> fmap (\hs' -> GDefExpJmt l (GListHypo hs') e k d) (f hs)
  GDefKindJmt l (GListHypo hs) k d -> fmap (\hs' -> GDefKindJmt l (GListHypo hs') k d) (f hs)
  GDefAdjJmt l (GListHypo hs) x v a p -> fmap (\hs' -> GDefAdjJmt l (GListHypo hs') x v a p) (f hs)
  GDefNounJmt l (GListHypo hs) x v k p -> fmap (\hs' -> GDefNounJmt l (GListHypo hs') x v k p) (f hs)
  GNamingJmt l (GListHypo hs) x v y -> fmap (\hs' -> GNamingJmt l (GListHypo hs') x v y) (f hs)
  _ -> Nothing
