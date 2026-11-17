{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}

-- | Preparing Dedukti rewrite rules for translation to Agda, Lean and Rocq.
--
-- None of the targets has Dedukti's rewrite rules in general. This module
-- does the target-independent part of the translation:
--
-- * easy cases become ordinary definitions: a constant with exactly one rule,
--   whose left-hand side applies it to distinct variables, is defined by
--   that rule ('prepareRules');
-- * the remaining rules are turned into equations 'RuleEq' with explicit
--   variables, and classified by 'ruleKind': each target then translates
--   them in its own way (Agda: REWRITE, Rocq: Rewrite Rules, Lean: axioms).
--
-- Elem and Proof are erased by all translations; therefore a rule for
-- Elem (c x y) is treated as a rule for c, and similarly for Proof.

module DeduktiRules (
  RuleContext(..),
  emptyRuleContext,
  prepareRules,
  RuleEq(..),
  ruleEquation,
  ruleEquationIn,
  RuleKind(..),
  ruleKind,
  ruleHeadName,
  metaVariables,
  substExp
  ) where

import Dedukti.AbsDedukti
import DeduktiOperations (splitApp, splitType, isWildIdent, identsInTree, pattbindIdents)
import DeduktiTheoryAPI (dkElem, dkProof)

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)

-- | What a translator needs to know about the rules of the whole module.
data RuleContext = RuleContext {
  rcTypes   :: M.Map QIdent Exp,  -- ^ types of the constants declared in the module
  rcSymbols :: S.Set QIdent,      -- ^ constants in the left-hand sides of the remaining rules
  rcHasRewrites :: Bool,          -- ^ whether some rule is translated as a rewrite rule
  rcNumbers :: M.Map Rule Int     -- ^ rules numbered 1, 2, ... for each head, to name them
  }

emptyRuleContext :: RuleContext
emptyRuleContext = RuleContext M.empty S.empty False M.empty

-- | A rule as an equation: lhs = rhs for all vars.
-- The variables have types if the rule context gives them.
-- Wildcards _ in the pattern are replaced by fresh variables.
data RuleEq = RuleEq {
  reHead :: QIdent,
  reVars :: [(QIdent, Maybe Exp)],
  reLhs  :: Exp,
  reRhs  :: Exp,
  reWilds :: [QIdent]  -- ^ the variables that replace wildcards
  }

data RuleKind =
    RKRewrite   -- ^ to be translated as a rewrite rule or an equation
  | RKProof     -- ^ between proofs: holds by proof irrelevance, can be omitted
  | RKUniverse  -- ^ with a universe as value: cannot be translated after erasure of Elem
  deriving (Eq, Show)

ruleEquation :: Rule -> RuleEq
ruleEquation (RRule pattbinds patt rhs) = RuleEq {
    reHead = headOf lhs,
    reVars = [(x, M.lookup x bindTypes) | x <- pattbindIdents pattbinds] ++
             [(w, Nothing) | w <- wilds],
    reLhs = lhs,
    reRhs = rhs,
    reWilds = wilds
    }
  where
    (lhs, wilds) = renameWilds used (unwrapErased (patt2exp patt))
    used = pattbindIdents pattbinds ++ identsInTree patt ++ identsInTree rhs
    bindTypes = M.fromList [(x, t) | PBTyped x t <- pattbinds]

-- | A rule as an equation, with the wildcards that typing determines
-- replaced by what they must be. For instance, in
-- first A _ (pair _ _ a _), where pair : (A : Set) -> (B : Set) -> Elem A -> Elem B -> Elem (Prod A B),
-- the first two wildcards of pair must be A and the wildcard of first.
-- This is done by first-order unification of the declared types of
-- the constants in the module.
ruleEquationIn :: RuleContext -> Rule -> RuleEq
ruleEquationIn ctx rule = eq {
    reVars = [v | v@(x, _) <- reVars eq, M.notMember x solution],
    reWilds = [x | x <- reWilds eq, M.notMember x solution],
    reLhs = substExp solution (reLhs eq)
    }
  where
    eq = ruleEquation rule
    wilds = S.fromList (reWilds eq)
    solution = resolve (snd (inferType M.empty (reLhs eq)))

    resolve sub = let sub' = M.map (substExp sub) sub in if sub' == sub then sub else resolve sub'

    -- the type of an application whose head has a known type, and a substitution for wildcards
    inferType :: M.Map QIdent Exp -> Exp -> (Maybe Exp, M.Map QIdent Exp)
    inferType sub e = case splitApp e of
      (EIdent c, args) | Just typ <- M.lookup c (rcTypes ctx) -> checkArgs sub typ args
      _ -> (Nothing, sub)

    checkArgs sub typ args = case (typ, args) of
      (_, []) -> (Just typ, sub)
      (EFun h body, a : as) ->
        let expected = maybe metaType id (hypoType h)
            (inferred, sub1) = inferType sub a
            sub2 = maybe sub1 (\ t -> unify sub1 (substExp sub1 expected) (substExp sub1 t)) inferred
            body' = case hypoVar h of
              [x] -> substExp (M.singleton x a) body
              _ -> body
        in checkArgs sub2 body' as
      _ -> (Nothing, sub)

    unify :: M.Map QIdent Exp -> Exp -> Exp -> M.Map QIdent Exp
    unify sub t u = case (substExp sub t, substExp sub u) of
      (EIdent x, EIdent y) | x == y -> sub
      (EIdent x, EIdent y) | S.member x wilds && S.member y wilds -> M.insert y (EIdent x) sub
      (EIdent x, v) | S.member x wilds && notElem x (identsInTree v) -> M.insert x v sub
      (v, EIdent y) | S.member y wilds && notElem y (identsInTree v) -> M.insert y v sub
      (EApp f a, EApp g b) -> unify (unify sub f g) a b
      _ -> sub  -- no solution: leave as it is

    metaType = EIdent (QIdent "Type")

-- | The head of a rule, after unwrapping Elem and Proof.
ruleHeadName :: Rule -> QIdent
ruleHeadName = reHead . ruleEquation

ruleKind :: RuleContext -> RuleEq -> RuleKind
ruleKind ctx eq
  | isUniverse (reRhs eq) = RKUniverse
  | isProofValued = RKProof
  | otherwise = RKRewrite
 where
  isUniverse e = case e of
    EIdent (QIdent s) -> elem s ["Set", "Prop", "Type"]
    _ -> False
  isProofValued = case M.lookup (reHead eq) (rcTypes ctx) of
    Just typ -> case splitApp (snd (splitType typ)) of
      (EIdent (QIdent p), [_]) -> p == dkProof
      _ -> False
    _ -> False

-- | The rules of a module numbered 1, 2, ... for each head.
ruleNumbers :: [Jmt] -> M.Map Rule Int
ruleNumbers jmts = M.fromList (number M.empty [r | JRules rs <- jmts, r <- rs])
  where
    number seen rules = case rules of
      r : rs ->
        let c = ruleHeadName r
            k = M.findWithDefault 0 c seen + 1
        in (r, k) : number (M.insert c k seen) rs
      [] -> []

-- | The variables of a rule replaced by metavariables ?x (in Rocq).
metaVariables :: RuleEq -> RuleEq
metaVariables eq = eq {reLhs = subst (reLhs eq), reRhs = subst (reRhs eq)}
  where
    vars = S.fromList (map fst (reVars eq))
    subst = metas vars
    metas :: S.Set QIdent -> Exp -> Exp
    metas vs e = case e of
      EIdent x@(QIdent s) | S.member x vs -> EIdent (QIdent ('?' : s))
      EIdent _ -> e
      EApp f a -> EApp (metas vs f) (metas vs a)
      EAbs b body -> EAbs (metasBind vs b) (metas (S.delete (bindVar b) vs) body)
      EFun h body -> EFun (metasHypo vs h) (metas (foldr S.delete vs (hypoVar h)) body)
      _ -> e
    metasBind :: S.Set QIdent -> Bind -> Bind
    metasBind vs b = case b of
      BTyped x t -> BTyped x (metas vs t)
      _ -> b
    metasHypo :: S.Set QIdent -> Hypo -> Hypo
    metasHypo vs h = case h of
      HExp t -> HExp (metas vs t)
      HVarExp x t -> HVarExp x (metas vs t)
      HParVarExp x t -> HParVarExp x (metas vs t)
      _ -> h

-- | Turn the easy cases of rules into definitions, and collect the context
-- for translating the rest. The easy case: a constant c, declared in the
-- module without a definition, has exactly one rule, of the form
-- c x1 ... xn --> e, where the xi are distinct variables, n is the arity of c,
-- e does not contain c, and no judgement between the declaration and the
-- rule mentions c. Then the declaration is dropped and the rule becomes
-- def c : T := x1 => ... => xn => e.
prepareRules :: [Jmt] -> ([Jmt], RuleContext)
prepareRules jmts = (newJmts, ctx)
  where
    indexed = zip [0 :: Int ..] jmts
    types = M.fromList (concatMap declType jmts)

    declarations = M.fromList [(c, i) | (i, j) <- indexed, Just c <- [bodiless j]]
    rulesOf = M.fromListWith (++)
      [(ruleHeadName r, [(i, r)]) | (i, JRules rs) <- indexed, r <- rs]

    defined :: M.Map QIdent (Int, Rule)  -- constant -> its defining rule
    defined = M.fromList [(c, (i, r)) | (c, [(i, r)]) <- M.toList rulesOf, isEasy c i r]

    isEasy c i r = case (M.lookup c declarations, M.lookup c types) of
      (Just d, Just typ) ->
        let eq = ruleEquation r
            (_, args) = splitApp (reLhs eq)
            vars = [x | EIdent x <- args]
        in d < i
           && length vars == length args
           && length (nub vars) == length vars
           && all (`elem` map fst (reVars eq)) vars
           && length args == length (fst (splitType typ))
           && notElem c (identsInTree (reRhs eq))
           && notElem c [QIdent dkElem, QIdent dkProof]
           && and [notElem c (identsInTree j) | (k, j) <- indexed, d < k, k < i]
      _ -> False

    newJmts = concatMap rewrite indexed
    rewrite (i, j) = case j of
      _ | Just c <- bodiless j, M.member c defined -> []
      JRules rs ->
        let (defs, rest) = foldr split ([], []) rs
            split r (ds, rr) = case M.lookup (ruleHeadName r) defined of
              Just (k, r') | k == i && r' == r -> (r : ds, rr)
              _ -> (ds, r : rr)
        in map definition defs ++ [JRules rest | not (null rest)]
      _ -> [j]

    definition r =
      let eq = ruleEquation r
          c = reHead eq
          (_, args) = splitApp (reLhs eq)
          typ = types M.! c
      in JDef c (MTExp typ) (MEExp (foldr (\ (EIdent x) e -> EAbs (BVar x) e) (reRhs eq) args))

    remaining = [r | JRules rs <- newJmts, r <- rs]
    ctx0 = emptyRuleContext {rcTypes = types, rcNumbers = ruleNumbers newJmts}
    rewrites = [eq | r <- remaining, let eq = ruleEquation r, ruleKind ctx0 eq == RKRewrite]
    ctx = ctx0 {
      rcSymbols = S.fromList (concat [constantsIn eq | eq <- rewrites]),
      rcHasRewrites = not (null rewrites)
      }

    constantsIn eq = [x | x <- identsInTree (reLhs eq), notElem x (map fst (reVars eq))]

    bodiless :: Jmt -> Maybe QIdent
    bodiless j = case j of
      JStatic c _ -> Just c
      JDef c (MTExp _) MENone -> Just c
      JInj c (MTExp _) MENone -> Just c
      _ -> Nothing

    declType :: Jmt -> [(QIdent, Exp)]
    declType j = case j of
      JStatic c t -> [(c, t)]
      JDef c (MTExp t) _ -> [(c, t)]
      JInj c (MTExp t) _ -> [(c, t)]
      JThm c (MTExp t) _ -> [(c, t)]
      _ -> []

-- * Auxiliaries

patt2exp :: Patt -> Exp
patt2exp p = case p of
  PVar x -> EIdent x
  PApp f a -> EApp (patt2exp f) (patt2exp a)
  PBracket q -> patt2exp q
  PBind b q -> EAbs b (patt2exp q)

-- | Elem t and Proof t become t, as the translations erase Elem and Proof.
unwrapErased :: Exp -> Exp
unwrapErased e = case splitApp e of
  (EIdent (QIdent c), [t]) | elem c [dkElem, dkProof] -> t
  _ -> e

headOf :: Exp -> QIdent
headOf e = case fst (splitApp e) of
  EIdent c -> c
  _ -> QIdent "UNKNOWN_HEAD"

-- | Replace each wildcard by a fresh variable w1, w2, ...
renameWilds :: [QIdent] -> Exp -> (Exp, [QIdent])
renameWilds used e = (e', reverse ws)
  where
    fresh = [QIdent ("w" ++ show k) | k <- [1 :: Int ..], notElem (QIdent ("w" ++ show k)) used]
    (e', (_, ws)) = go e (fresh, [])
    go :: Exp -> ([QIdent], [QIdent]) -> (Exp, ([QIdent], [QIdent]))
    go x st@(fs, acc) = case x of
      EIdent q | isWildIdent q -> (EIdent (head fs), (tail fs, head fs : acc))
      EApp f a ->
        let (f', st1) = go f st
            (a', st2) = go a st1
        in (EApp f' a', st2)
      EAbs b body ->
        let (body', st1) = go body st
        in (EAbs b body', st1)
      _ -> (x, st)

bindVar :: Bind -> QIdent
bindVar b = case b of
  BVar x -> x
  BTyped x _ -> x
  BLet x _ _ -> x

hypoType :: Hypo -> Maybe Exp
hypoType h = case h of
  HExp t -> Just t
  HVarExp _ t -> Just t
  HParVarExp _ t -> Just t
  _ -> Nothing

-- | Substitution of expressions for variables, respecting shadowing.
substExp :: M.Map QIdent Exp -> Exp -> Exp
substExp sub e
  | M.null sub = e
  | otherwise = case e of
      EIdent x -> M.findWithDefault e x sub
      EApp f a -> EApp (substExp sub f) (substExp sub a)
      EAbs b body -> EAbs b (substExp (M.delete (bindVar b) sub) body)
      EFun h body -> EFun (substHypo h) (substExp (foldr M.delete sub (hypoVar h)) body)
      _ -> e
  where
    substHypo :: Hypo -> Hypo
    substHypo h = case h of
      HExp t -> HExp (substExp sub t)
      HVarExp x t -> HVarExp x (substExp sub t)
      HParVarExp x t -> HParVarExp x (substExp sub t)
      _ -> h

hypoVar :: Hypo -> [QIdent]
hypoVar h = case h of
  HVarExp x _ -> [x]
  HParVarExp x _ -> [x]
  _ -> []
