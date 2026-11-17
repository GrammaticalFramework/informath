{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}

-- | Preparing Informath-Dedukti structures for translation to records
-- (Agda, Rocq) and structures (Lean).
--
-- Structures are represented as in test/algebra.dk: a kind K : Structure,
-- the type Str K of the structures of that kind, constructors with values in Str K,
--
-- > Magma : (S : Set) -> ({|*|} : Op S) -> Str MagmaS.
--
-- and selectors defined by rules, as made by scripts/structure_selectors.py:
--
-- > def S_Magma : (m : Str MagmaS) -> Set.
-- > [S, {|*|}] S_Magma (Magma S {|*|}) --> S.
--
-- The first constructor of each kind becomes the constructor of a record
-- whose fields are its arguments. A field is named by its selector
-- (a definition with one rule returning that argument), which therefore
-- becomes a projection and is dropped from the output. Other selectors with
-- one rule on the constructor become definitions by the projections, so that
-- the selectors of an extension, such as S_Semigroup' M --> S_Magma M,
-- are defined for records whose parent is a field.
-- Other constructors of the same kind stay as postulates, with their
-- selectors translated as rules.
--
-- The translations erase Str, so that Str K becomes the record K. The
-- declarations of Structure and Str are dropped, and kinds without a
-- constructor become postulated types.

module DeduktiStructures (
  StructureRecord(..),
  prepareStructures,
  recordLevel
  ) where

import Dedukti.AbsDedukti
import DeduktiOperations (splitApp, splitType, identsInTree, hypo2topvars, hypo2type, isWildIdent)
import DeduktiTheoryAPI (isStructureKind)
import DeduktiRules (RuleEq(..), ruleEquation, substExp)

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isAlphaNum)
import Data.List (nub)
import Data.Maybe (listToMaybe)

-- | A record to replace the declaration of a constructor.
data StructureRecord = StructureRecord {
  srKind :: QIdent,             -- ^ the kind K, the name of the record
  srConstructor :: QIdent,      -- ^ the constructor
  srFields :: [(QIdent, Exp)]   -- ^ the fields, with types in terms of the earlier fields
  }

-- | Universe level of a record: 1 if some field is a type or contains
-- a record of level 1, otherwise 0. The argument gives the levels of
-- the earlier records.
recordLevel :: M.Map QIdent Int -> StructureRecord -> Int
recordLevel levels r = maximum (0 : map (level . snd) (srFields r))
  where
    level t = let (hypos, val) = splitType t
              in maximum (valueLevel val : [level h | Just h <- map hypo2type hypos])
    valueLevel v = case splitApp v of
      (EIdent (QIdent u), []) | elem u ["Set", "Type", "Prop"] -> 1
      (EIdent k, []) -> M.findWithDefault 0 k levels
      (f, [EIdent k]) | isStructureKind f -> M.findWithDefault 0 k levels
      _ -> 0

-- | The judgements to translate, with the records keyed by their constructors.
-- The declaration of a constructor with a record is kept, as the place where the
-- record is to be printed.
prepareStructures :: [Jmt] -> ([Jmt], M.Map QIdent StructureRecord)
prepareStructures jmts
  | null kinds = (jmts, M.empty)
  | otherwise = (concatMap rewrite indexed, records)
  where
    indexed = zip [0 :: Int ..] jmts

    kinds = [k | JStatic k (EIdent (QIdent "Structure")) <- jmts]
    kindPositions = M.fromList [(k, i) | (i, JStatic k (EIdent (QIdent "Structure"))) <- indexed]

    -- constructors: their kinds, positions and argument hypotheses
    constructors = [(c, k, i, hypos) |
       (i, JStatic c typ) <- indexed,
       let (hypos, val) = splitType typ,
       not (null hypos),
       (f, [EIdent k]) <- [splitApp val],
       isStructureKind f,
       elem k kinds]

    -- the first constructor of each kind, if the kind is not used before it
    primaries = M.fromList [(k, (c, i, hypos)) |
       k <- kinds,
       (c, _, i, hypos) <- take 1 [x | x@(_, k', _, _) <- constructors, k' == k],
       let d = kindPositions M.! k,
       and [notElem k (identsInTree j) | (n, j) <- indexed, d < n, n < i]]

    -- rules with their heads and positions
    rules = [(i, r, ruleEquation r) | (i, JRules rs) <- indexed, r <- rs]
    rulesOf = M.fromListWith (++) [(reHead eq, [(i, r, eq)]) | (i, r, eq) <- rules]
    declarations = M.fromList [(s, (i, typ)) | (i, JDef s (MTExp typ) MENone) <- indexed]

    -- a selector: a definition with one rule s (c x1 ... xn) --> e
    selectors :: [(QIdent, QIdent, Int, Rule, [QIdent], Exp)]  -- s, c, position of rule, rule, xs, e
    selectors = [(s, c, i, r, xs, reRhs eq) |
       (s, [(i, r, eq)]) <- M.toList rulesOf,
       Just (d, _) <- [M.lookup s declarations],
       d < i,
       (EIdent s', [arg]) <- [splitApp (reLhs eq)],
       s' == s,
       (EIdent c, args) <- [splitApp arg],
       Just (_, _, hypos) <- [M.lookup c constructorOf],
       length args == length hypos,
       let xs = [x | EIdent x <- args],
       length xs == length args,
       length (nub xs) == length xs,
       and [notElem s (identsInTree j) | (n, j) <- indexed, d < n, n < i]]

    primaryOf = M.fromList [(c, (k, i, hypos)) | (k, (c, i, hypos)) <- M.toList primaries]
    constructorOf = M.fromList [(c, (k, i, hypos)) | (c, k, i, hypos) <- constructors, M.member k primaries]

    -- projections: the first selector returning each argument
    projections :: M.Map (QIdent, Int) QIdent
    projections = M.fromListWith (\_ old -> old)
      [((c, n), s) | (s, c, _, _, xs, EIdent x) <- selectors, M.member c primaryOf, (n, y) <- zip [1..] xs, y == x]
    projectionSelectors = S.fromList (M.elems projections)

    fieldNames c hypos = [maybe (generated c n h) id (M.lookup (c, n) projections) |
                          (n, h) <- zip [1 :: Int ..] hypos]
    generated :: QIdent -> Int -> Hypo -> QIdent
    generated (QIdent c) n h = case hypo2topvars h of
      [QIdent x] | not (isWildIdent (QIdent x)) && all (\ch -> isAlphaNum ch || ch == '\'') x
        -> QIdent (x ++ "_" ++ c)
      _ -> QIdent ("arg" ++ show n ++ "_" ++ c)

    records = M.fromList [(c, StructureRecord k c (fields c hypos)) | (k, (c, _, hypos)) <- M.toList primaries]
    fields :: QIdent -> [Hypo] -> [(QIdent, Exp)]
    fields c hypos =
      let names = fieldNames c hypos
          vars = map hypo2topvars hypos
          sub n = M.fromList [(v, EIdent f) | (vs, f) <- take n (zip vars names), v <- vs]
      in [(f, substExp (sub n) t) | (n, (f, h)) <- zip [0..] (zip names hypos), Just t <- [hypo2type h]]

    -- other selectors become definitions by the projections
    derived = M.fromList [(s, (i, r, definitionBy s (byProjections c xs e))) |
       (s, c, i, r, xs, e) <- selectors, M.member c primaryOf, S.notMember s projectionSelectors]

    -- e with the variables xs of the constructor c replaced by the projections of m
    byProjections :: QIdent -> [QIdent] -> Exp -> Exp -> Exp
    byProjections c xs e m =
      let StructureRecord _ _ fs = records M.! c
      in substExp (M.fromList [(x, EApp (EIdent f) m) | (x, (f, _)) <- zip xs fs]) e

    -- def s : T := m => y1 => ... => body m y1 ...
    definitionBy :: QIdent -> (Exp -> Exp) -> Jmt
    definitionBy s mk =
      let (_, typ) = declarations M.! s
          (hypos, _) = splitType typ
          m = case hypos of
            h : _ | [v] <- hypo2topvars h -> v
            _ -> QIdent "m"
          extra = [QIdent ("y" ++ show n) | n <- [1 .. length hypos - 1]]
          body = foldl EApp (mk (EIdent m)) (map EIdent extra)
      in JDef s (MTExp typ) (MEExp (foldr (\ x b -> EAbs (BVar x) b) body (m : extra)))

    -- Another constructor c' of a kind with a record is defined by the record
    -- constructor, if each field f_c has a selector f_c' of c' (as named by
    -- scripts/structure_selectors.py), and c' (ys) --> e. Then f_c' is the
    -- projection f_c, and the selector of a parent y, if each field p_cp of
    -- the parent has a field f_c' --> p_cp y, is cp (f_c m) ...
    secondaries :: M.Map QIdent (Jmt, M.Map QIdent (Int, Rule, Jmt))
    secondaries = M.fromList [(c', (def, sels)) |
       (c', k, i, hypos) <- constructors,
       Just (c, _, _) <- [M.lookup k primaries],
       c' /= c,
       Just (def, sels) <- [secondary c' i hypos (records M.! c)]]

    secondary :: QIdent -> Int -> [Hypo] -> StructureRecord -> Maybe (Jmt, M.Map QIdent (Int, Rule, Jmt))
    secondary c'@(QIdent c's) i hypos (StructureRecord _ c fs) = do
      let ys = [case hypo2topvars h of
                  [v] | not (isWildIdent v) -> v
                  _ -> QIdent ("a" ++ show n) | (n, h) <- zip [1 :: Int ..] hypos]
          own = [(s, (i', r, substExp (M.fromList (zip xs (map EIdent ys))) e)) |
                 (s, c'', i', r, xs, e) <- selectors, c'' == c']
          QIdent cs = c
          base (QIdent f) = let suffix = "_" ++ cs in
            if drop (length f - length suffix) f == suffix then Just (take (length f - length suffix) f) else Nothing
      values <- mapM (\ (f, _) -> do
                         b <- base f
                         (_, (_, _, e)) <- listToMaybe [x | x@(QIdent s, _) <- own, s == b ++ "_" ++ c's]
                         return (f, e)) fs
      let earlier = S.fromList (concat [identsDeclared j | (n, j) <- indexed, n < i])
      if and [S.member x earlier || elem x ys | (_, e) <- values, x <- identsInTree e]
        then return () else Nothing
      sels <- mapM (\ (s, (i', r, e)) -> do
                     mk <- selectorBody ys hypos values e
                     return (s, (i', r, definitionBy s mk))) own
      let (_, typ) = constructorType M.! c'
          def = JDef c' (MTExp typ) (MEExp (foldr (\ y b -> EAbs (BVar y) b)
                  (foldl EApp (EIdent c) (map snd values)) ys))
      return (def, M.fromList sels)

    -- the selector of c' returning e, in terms of the fields of the record
    selectorBody :: [QIdent] -> [Hypo] -> [(QIdent, Exp)] -> Exp -> Maybe (Exp -> Exp)
    selectorBody ys hypos values e = case [f | (f, v) <- values, alias v == alias e] of
      f : _ -> Just (\ m -> EApp (EIdent f) m)
      [] -> case e of
        EIdent y | Just h <- lookup y (zip ys hypos), Just t <- hypo2type h,
                   (fk, [EIdent kp]) <- splitApp t, isStructureKind fk,
                   Just (cp, _, _) <- M.lookup kp primaries -> do
          let StructureRecord _ _ pfs = records M.! cp
          parts <- mapM (\ (p, _) -> listToMaybe [f | (f, v) <- values, alias v == EApp (EIdent p) e]) pfs
          return (\ m -> foldl EApp (EIdent cp) [EApp (EIdent f) m | f <- parts])
        _ -> Nothing

    -- the selectors of the other constructors as the fields of the record, by
    -- their names: S_Semigroup' as S_Semigroup
    alias :: Exp -> Exp
    alias = substExp (M.fromList [(QIdent (b ++ "_" ++ c's), EIdent f) |
      (c', k, _, _) <- constructors,
      Just (c, _, _) <- [M.lookup k primaries], c' /= c,
      let QIdent c's = c',
      let QIdent cs = c,
      let StructureRecord _ _ fs = records M.! c,
      (f@(QIdent fs'), _) <- fs,
      let suffix = "_" ++ cs,
      drop (length fs' - length suffix) fs' == suffix,
      let b = take (length fs' - length suffix) fs'])

    constructorType = M.fromList [(c, (i, t)) | (i, JStatic c t) <- indexed]

    identsDeclared :: Jmt -> [QIdent]
    identsDeclared j = case j of
      JStatic c _ -> [c]
      JDef c _ _ -> [c]
      JThm c _ _ -> [c]
      JInj c _ _ -> [c]
      _ -> []

    secondarySelectors = M.unions [sels | (_, sels) <- M.elems secondaries]

    rewrite :: (Int, Jmt) -> [Jmt]
    rewrite (i, j) = case j of
      JStatic (QIdent "Structure") _ -> []
      JStatic f _ | f == QIdent "Str" -> []
      JStatic k (EIdent (QIdent "Structure"))
        | M.member k primaries -> []
        | otherwise -> [JStatic k (EIdent (QIdent "Type"))]
      JStatic c' _ | Just (def, _) <- M.lookup c' secondaries -> [def]
      JDef s _ MENone | S.member s projectionSelectors || M.member s defined -> []
      JRules rs ->
        let keep r = not (isSelectorRule r)
            defs = [def | r <- rs, (_, (i', r', def)) <- M.toList defined, i' == i, r' == r]
        in defs ++ [JRules kept | let kept = filter keep rs, not (null kept)]
      _ -> [j]

    defined = M.union derived secondarySelectors

    isSelectorRule :: Rule -> Bool
    isSelectorRule r = or [r == r' | (s, _, _, r', _, _) <- selectors,
                                     S.member s projectionSelectors || M.member s defined]
