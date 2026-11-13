{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}

-- | An empirical check of the specification of doc/nlg-and-semantics.md: if
-- NLG produces a tree t' from a MathCore tree t, the semantics of t'
-- includes t. Each NLG variant t' is classified as
--
--   * exact: t is among the semantics of t';
--   * equivalent: it is so after both sides are normalized (coercions
--     removed, conjunctions and disjunctions flattened, antecedents
--     uncurried, quantified conclusions as hypotheses, hypotheses split and
--     sorted, bound variables renamed, ∀ distributed over ∧, ¬∃ as ∀¬);
--   * lost: not even then.
--
-- The variants are made by the passes of MathCore2Informath.nlg, rebuilt
-- here so that each variant is attributed to the first stage that makes it.

module NLGCheck (Stage, nlgStages, normalize, Verdict(..), classify, firstDifference, Report, report) where

import Informath
import qualified MathCore2Informath as MCI
import qualified Informath2MathCore as IMC
import PGF (Expr, showExpr, unApp, showCId)

import Data.List (sortOn, nub)
import qualified Data.Map as M

type Stage = String

-- | The stages of NLG, each with its trees (as with -more-variants); the
-- user's #NLG rules and the implicit kinds are given by the caller; with
-- safe (-roundtrip-safe), the stages as MathCore2Informath.nlg makes them then.
nlgStages :: Bool -> (GJmt -> [GJmt]) -> (GJmt -> GJmt) -> GJmt -> [(Stage, [GJmt])]
nlgStages safe userNLG bare tree =
  [ ("flatten (passes 1-5)", [ft])
  , ("aggregate", afts)
  , ("insitu", iafts)
  , ("varless", viafts)
  , ("collectivize", cviafts)
  , ("negated, implied, scoped", ncviafts)
  , ("letDeclared (default output)", map MCI.letDeclared ncviafts)
  , ("bareQuantifiers", map (bare . MCI.letDeclared) ncviafts)
  , ("variations", take 200 vncviafts)
  , ("#NLG rules", if safe then [] else take 200 (concatMap userNLG vncviafts))
  ]
 where
   t = MCI.unparenth tree
   ut = MCI.funKinds (MCI.uncoerce t)
   ft = MCI.flatten (MCI.extendStructure ut)
   afts = [(if safe then MCI.aggregateSafe else MCI.aggregate) ft]
   iafts = map MCI.insitu afts
   viafts = map MCI.varless iafts
   cviafts = concatMap (if safe then MCI.collectivizeSafe else MCI.collectivize) viafts
   ncviafts = map (MCI.scoped . MCI.implied . MCI.negated) cviafts
   vncviafts = concatMap (\u -> [v | v <- MCI.variations u, not safe || MCI.restorableVariation u v]) ncviafts

-- | SameHypotheses: equivalent with the hypotheses as in t, in number and
-- order (normalizeKeepingHypos), so that the constant has the same arguments
data Verdict = Exact | SameHypotheses | Equivalent | Lost
  deriving (Eq, Ord, Show)

-- | The verdict for t and the semantics of a variant
classify :: GJmt -> [GJmt] -> Verdict
classify t sems
  | elem t sems = Exact
  | elem (normalizeKeepingHypos t) (map normalizeKeepingHypos sems) = SameHypotheses
  | elem (normalize t) (map normalize sems) = Equivalent
  | otherwise = Lost

------------------------------------------------------------
-- normalization

normalize :: GJmt -> GJmt
normalize = alpha . hypos . canonicalHypoVars . distributeAll . pullConclusion . props . forms . IMC.addParenth . MCI.uncoerce

-- | The normalization without what changes the hypotheses: they are not split
-- into one proposition each, not sorted, and the quantifiers of the
-- conclusion stay there; only the variables of one VarsHypo are separated
-- (as they are separate arguments) and labels dropped.
normalizeKeepingHypos :: GJmt -> GJmt
normalizeKeepingHypos = alpha . varHypos . distributeAll . props . forms . IMC.addParenth . MCI.uncoerce
 where
  varHypos :: Tree a -> Tree a
  varHypos t = case t of
    GListHypo hs -> GListHypo (concatMap one (map varHypos hs))
    _ -> composOp varHypos t
  one :: GHypo -> [GHypo]
  one h = case h of
    GVarsHypo (GListIdent xs) k -> [GVarsHypo (GListIdent [x]) k | x <- xs]
    GPropVarHypo _ p -> [GPropHypo p]
    _ -> [h]

-- | Forms that the semantics rewrites to equivalent ones: an adjective kind as
-- a such-that kind, a plural kind as the kind, a nested collective function
-- folded to the left
forms :: Tree a -> Tree a
forms t = case t of
  GAdjKind a k -> GSuchThatKind (forms k) z (GAdjProp a (GTermExp (GIdentTerm z)))
  GPluralKindExp k -> GKindExp (forms k)
  GFunCExp f _ _ -> foldl1 (GFunCExp f) (map forms (args f t))
  _ -> composOp forms t
 where
  z = GStrIdent (GString "_g0")
  args :: GFunC -> GExp -> [GExp]
  args f e = case e of
    GFunCExp g x y | g == f -> args f x ++ args f y
    _ -> [e]

-- | Conjunctions and disjunctions as flat lists, binary or not; an
-- implication with its antecedents as one conjunction (uncurried); ∀ over ∧
-- distributed, ¬∃ as ∀¬
props :: Tree a -> Tree a
props t = case t of
  GCoreAndProp a b -> andP (conjuncts (props a) ++ conjuncts (props b))
  GAndProp (GListProp ps) -> andP (concatMap (conjuncts . props) ps)
  GCoreOrProp a b -> orP (disjuncts (props a) ++ disjuncts (props b))
  GOrProp (GListProp ps) -> orP (concatMap (disjuncts . props) ps)
  GCoreIfProp a b -> ifP (props a) (props b)
  GIfProp a b -> ifP (props a) (props b)
  GCoreIffProp a b -> GIffProp (props a) (props b)
  GCoreNotProp p -> case props p of
    GCoreExistProp k x q -> GCoreAllProp k x (props (GCoreNotProp q))
    q -> GCoreNotProp q
  -- an argument kind with a generated name is an unnamed one
  GIdentsArgKind k (GListIdent [GStrIdent (GString ('_':'h':_))]) -> GKindArgKind (props k)
  _ -> composOp props t
 where
   conjuncts p = case p of
     GAndProp (GListProp ps) -> ps
     _ -> [p]
   disjuncts p = case p of
     GOrProp (GListProp ps) -> ps
     _ -> [p]
   andP ps = case ps of
     [p] -> p
     _ -> GAndProp (GListProp ps)
   orP ps = case ps of
     [p] -> p
     _ -> GOrProp (GListProp ps)
   -- if A, then if B, then C: if A and B, then C
   ifP a b = case b of
     GIfProp a' c -> GIfProp (andP (conjuncts a ++ conjuncts a')) c
     _ -> GIfProp a b

-- | ∀ distributed over ∧ (after the conclusion's quantifiers are hypotheses)
distributeAll :: Tree a -> Tree a
distributeAll t = case t of
  GCoreAllProp k x p -> case distributeAll p of
    GAndProp (GListProp ps) -> GAndProp (GListProp [GCoreAllProp k x q | q <- ps])
    q -> GCoreAllProp k x q
  _ -> composOp distributeAll t

-- | The universal quantifiers and implications of a theorem's conclusion as
-- its hypotheses
pullConclusion :: GJmt -> GJmt
pullConclusion j = case j of
  GAxiomJmt l (GListHypo hs) p -> let (hs', q) = pull p in GAxiomJmt l (GListHypo (hs ++ hs')) q
  GThmJmt l (GListHypo hs) p pr -> let (hs', q) = pull p in GThmJmt l (GListHypo (hs ++ hs')) q pr
  _ -> j
 where
   pull :: GProp -> ([GHypo], GProp)
   pull p = case p of
     GCoreAllProp k x q -> let (hs, r) = pull q in (GVarsHypo (GListIdent [x]) k : hs, r)
     GIfProp a q -> let (hs, r) = pull q in (map GPropHypo (conjuncts a) ++ hs, r)
     _ -> ([], p)
   conjuncts p = case p of
     GAndProp (GListProp ps) -> ps
     _ -> [p]

-- | The variables declared by hypotheses renamed _v0, _v1, ... in the order of
-- their first occurrence in the conclusion, then of their names, so
-- that a variable that NLG has dropped and the semantics renamed (_h0)
-- matches the original
canonicalHypoVars :: GJmt -> GJmt
canonicalHypoVars j = case j of
  GAxiomJmt _ (GListHypo hs) p -> renaming hs p j
  GThmJmt _ (GListHypo hs) p _ -> renaming hs p j
  _ -> j
 where
   renaming :: [GHypo] -> GProp -> GJmt -> GJmt
   renaming hs p u =
     let declared = concat [xs | GVarsHypo (GListIdent xs) _ <- hs]
         -- the ones not in the conclusion by name, as NLG may reorder them
         order = nub ([x | x <- MCI.identsIn p, elem x declared] ++ sortOn (showExpr [] . gf) declared)
         new = zip order [GStrIdent (GString ("_v" ++ show i)) | i <- [(0 :: Int) ..]]
     in rename new u
   rename :: [(GIdent, GIdent)] -> Tree b -> Tree b
   rename m u = case u of
     GStrIdent _ | Just v <- lookup u m -> v
     _ -> composOp (rename m) u

-- | Hypotheses one variable or one proposition at a time, unlabelled, sorted
hypos :: Tree a -> Tree a
hypos t = case t of
  GListHypo hs -> GListHypo (sortOn (showExpr [] . gf) (concatMap split (map hypos hs)))
  _ -> composOp hypos t
 where
   split :: GHypo -> [GHypo]
   split h = case h of
     GVarsHypo (GListIdent xs) k -> [GVarsHypo (GListIdent [x]) k | x <- xs]
     GPropVarHypo _ p -> split (GPropHypo p)
     GPropHypo (GAndProp (GListProp ps)) -> concatMap (split . GPropHypo) ps
     _ -> [h]

-- | The variables bound by quantifiers renamed _b0, _b1, ... in the order of
-- the binders
alpha :: Tree a -> Tree a
alpha t = fst (go 0 t)
 where
   go :: Int -> Tree b -> (Tree b, Int)
   go n u = case u of
     GCoreAllProp k x p ->
       let v = bound n
           (p', n') = go (n + 1) (rename x v p)
       in (GCoreAllProp k v p', n')
     GCoreExistProp k x p ->
       let v = bound n
           (p', n') = go (n + 1) (rename x v p)
       in (GCoreExistProp k v p', n')
     _ -> (composOp (\w -> fst (go n w)) u, n)
   bound i = GStrIdent (GString ("_b" ++ show i))
   rename :: GIdent -> GIdent -> Tree b -> Tree b
   rename x v u = case u of
     GStrIdent _ | u == x -> v
     _ -> composOp (rename x v) u

------------------------------------------------------------
-- where two trees differ

-- | The smallest subtrees in which two trees differ: the first child that
-- differs, as long as the heads and the numbers of children agree
firstDifference :: Expr -> Expr -> Maybe (Expr, Expr)
firstDifference a b
  | a == b = Nothing
  | otherwise = case (unApp a, unApp b) of
      (Just (f, as), Just (g, bs)) | f == g && length as == length bs ->
        case [d | (x, y) <- zip as bs, Just d <- [firstDifference x y]] of
          d : _ -> Just d
          [] -> Just (a, b)
      _ -> Just (a, b)

------------------------------------------------------------
-- the report

-- | For each judgement and each of its MathCore trees: the stage of each
-- variant, the variant in words, and the semantics of the variant
type Report = [(String, GJmt, [(Stage, GJmt, String, [GJmt])])]

report :: Report -> String
report results = unlines $
  [ "# The semantics of NLG: an empirical check"
  , ""
  , show (length results) ++ " MathCore trees, " ++ show (length checked) ++ " variants checked."
  , ""
  , "Same hypotheses: equivalent, with the hypotheses of t in number and order."
  , ""
  , "| stage | variants | exact | same hypotheses | equivalent | lost |"
  , "|---|---:|---:|---:|---:|---:|"
  ] ++
  [ "| " ++ s ++ " | " ++ show (n s Nothing) ++ " | " ++ show (n s (Just Exact)) ++ " | "
      ++ show (n s (Just SameHypotheses)) ++ " | "
      ++ show (n s (Just Equivalent)) ++ " | " ++ show (n s (Just Lost)) ++ " |"
  | s <- stages ] ++
  [ "| **all** | " ++ show (length checked) ++ " | " ++ show (total Exact) ++ " | "
      ++ show (total SameHypotheses) ++ " | "
      ++ show (total Equivalent) ++ " | " ++ show (total Lost) ++ " |"
  , ""
  , "## Where the lost ones differ"
  , ""
  , "The smallest subtrees in which the normalized t and the normalized semantics of t' differ,"
  , "by their head functions (t's first), with the number of variants and an example."
  , ""
  , "| t | semantics of t' | variants | stages | example |"
  , "|---|---|---:|---|---|"
  ] ++
  [ "| " ++ code a ++ " | " ++ code b ++ " | " ++ show (length xs) ++ " | "
      ++ unwords (nub [s | (s, _, _) <- xs]) ++ " | " ++ example (head xs) ++ " |"
  | ((a, b), xs) <- sortOn (negate . length . snd) (M.toList causes) ] ++
  [ ""
  , "## Examples of lost variants"
  , ""
  ] ++
  concat (take 40 [ [ "### " ++ name ++ " (" ++ s ++ ")", ""
                    , "* t': " ++ lin
                    , "* t: `" ++ showExpr [] (gf (normalize t)) ++ "`"
                    , "* semantics of t' (" ++ show (length sems) ++ "), the first: `"
                        ++ concat [showExpr [] (gf (normalize x)) | x <- take 1 sems] ++ "`"
                    , "" ]
                  | (name, t, s, lin, sems) <- lost ]) ++
  [ ""
  , "## Examples of the default output with other hypotheses"
  , ""
  , "Equivalent, but the hypotheses of the semantics differ from those of t in number or order."
  , ""
  ] ++
  concat (take 20 [ [ "### " ++ name, ""
                    , "* t': " ++ lin
                    , "* t: `" ++ showExpr [] (gf (normalizeKeepingHypos t)) ++ "`"
                    , "* semantics of t': `"
                        ++ concat [showExpr [] (gf (normalizeKeepingHypos x)) | x <- take 1 sems] ++ "`"
                    , "" ]
                  | (name, t, s, _, lin, sems, Equivalent) <- checked, take 11 s == "letDeclared" ])
 where
   checked = [(name, t, s, t', lin, sems, classify t sems) | (name, t, vs) <- results, (s, t', lin, sems) <- vs]
   stages = nub [s | (_, _, s, _, _, _, _) <- checked]
   n s mv = length [() | (_, _, s', _, _, _, v) <- checked, s' == s, maybe True (== v) mv]
   total v = length [() | (_, _, _, _, _, _, v') <- checked, v' == v]
   lost = [(name, t, s, lin, sems) | (name, t, s, _, lin, sems, Lost) <- checked]
   causes = M.fromListWith (flip (++))
     [ (key d, [(s, name, d)])
     | (name, t, s, _, sems) <- lost
     , let d = case sems of
                 x : _ -> firstDifference (gf (normalize t)) (gf (normalize x))
                 [] -> Just (gf (normalize t), gf (normalize t))
     , Just _ <- [d] ]
   key d = case d of
     Just (a, b) -> (headOf a, headOf b)
     Nothing -> ("", "")
   headOf e = maybe (showExpr [] e) (showCId . fst) (unApp e)
   code s = "`" ++ s ++ "`"
   example (_, name, d) = case d of
     Just (a, b) -> name ++ ": `" ++ cut (showExpr [] a) ++ "` / `" ++ cut (showExpr [] b) ++ "`"
     Nothing -> name
   cut s = if length s > 120 then take 117 s ++ "..." else s
