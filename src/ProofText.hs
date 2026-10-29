{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}

module ProofText where

import Dedukti.AbsDedukti hiding (Tree)
import Dedukti.PrintDedukti hiding (prt)

import DeduktiOperations
import DeduktiTheoryAPI (identProof, identElem, identImpl, identNot, identPi, identFalse)
import BuildConstantTable
import Environment
import AnnotateDedukti (annotateDkIdents, subst)

import Informath
import Dedukti2MathCore (exp2prop, hypos2hypos, ident2label, fgTree)
import CommonConcepts (lookupConstant)
import Utils (latexPreamble, latexEndDoc)


import Data.List (intersperse, isPrefixOf, nub, mapAccumL)
import qualified Data.Map as M

-- experiment with Jan von Plato 2017. "From Gentzen to Jaskowski and Back:
-- Algorithmic Translation of Derivations Between the Two Main Systems of Natural Deduction."
-- code partly borrowed from https://github.com/aarneranta/PESCA
--
-- main datatypes:
--
--   Term  (Martin-Löf style, from Dedukti)
--   Tree  (Gentzen style)  -- not used here
--   Lines (Jaskowski/Prawitz style)
--
-- main conversions:
--
--   Term -> Lines
--   Lines -> Term  -- TODO
--

-- | The proof texts of the proofs of a module, each with one table of lines
-- for each linearization given (one for each language).
-- With a printer of formulas (-proof-trees), each proof is also shown as a
-- natural deduction tree.
proofDemo :: Env -> Module -> Module -> [GUnit -> String] -> Maybe (Exp -> String) -> String
proofDemo env base (MJmts proofs) informalizes trees =
  prLatexFile (macroCommands (macroTable (symbolTable env))) $
    unlines $ intersperse "\n\n" [
      oneProof env dkmap informalizes trees t e | (t, e) <- concatMap proved proofs]
 where
   -- the constants of the base and of the module itself: a lemma stated in
   -- the module and used in a later proof is a rule with premisses, not a
   -- hypothesis, and its type says which of its arguments are premisses
   dkmap = M.union (identTypes base) (identTypes (MJmts proofs))

   -- a theorem is a proof, and so is a definition whose type is a Proof:
   -- Dedukti has no separate form for the theorems of a development
   proved :: Jmt -> [(Exp, Exp)]
   proved jmt = case jmt of
     JThm _ (MTExp t) (MEExp e) -> [(t, e)]
     JDef _ (MTExp t) (MEExp e) | provesAProposition t -> [(t, e)]
     _ -> []

   provesAProposition t = case snd (splitType t) of
     e -> case splitApp e of
       (EIdent f, _:_) -> f == identProof
       _ -> False

oneProof :: Env -> M.Map QIdent Exp -> [GUnit -> String] -> Maybe (Exp -> String) -> Exp -> Exp -> String
oneProof env dkmap lins trees typ exp = unlines $ intersperse "\n\n" (
    [ "\\subsection*{" ++ verbatimInline (printTree typ) ++ "}" ] ++
    [ prls (\ln -> prlu lin (indentation ln) ln) unitlines | lin <- lins ] ++
    [ proofTree latex rawlines number | Just latex <- [trees] ] ++
    (if isFlag "-proof-terms" env
       then [ "The proof term"
            , verbatim (printTree exp)
            , "The same proof in Dedukti"
            , prls prle (compressVarsLines linesterm)
            ]
       else []) ++
    ["\\clearpage"])
  where
    term = showOnly (proofProfile (showTable (symbolTable env)) dkmap)
                    (typeAnnotate dkmap [] typ exp)
    (rawlines, linesterm, renumbering) = term2linesMap term
    -- the variables of implicit types are not introduced, and neither are
    -- the hypotheses of the statement that the proof does not use
    (explicitlines, renumbering1) = dropImplicitLines dropped (map statedGoal linesterm)
    dropped l = isImplicitType env (formula (step l)) ||
                (elem (rule (step l)) statementVars && notElem (line l) cited)
    cited = concat [premisses l ++ closes l | l <- linesterm]
    (unitlines0, renumbering2) = compressVarsHyposMap (map (line2unitline env dkmap) explicitlines)
    -- with -proof-merge-hypos, the variables and hypotheses of a scope in one line
    (unitlines, renumbering3) = if isFlag "-proof-merge-hypos" env
      then compressScopesMap unitlines0
      else (unitlines0, [])
    -- a goal that is the conclusion of the statement is not repeated
    statedGoal l = if goal l == Just (formula (step (last linesterm))) then l{goal = Nothing} else l
    -- with -proof-indent, a line is indented by the number of scopes open at
    -- it that the proof itself introduces (not the variables and hypotheses
    -- of the statement), as in Fitch's notation: a scope is what one lambda
    -- binds, and the lines introducing its variables have the same context
    -- (a scope is open at a line if its context is a prefix of the line's:
    -- the same name can be bound in different scopes)
    indentation ln = if isFlag "-proof-indent" env
      then length (nub [c | (h, c) <- introduced, notElem h statementVars, c `isPrefixOf` context ln])
      else 0
    introduced = [(rule (step l), context l) | l <- unitlines, null (premisses l), elem (rule (step l)) (context l)]
    statementVars = map bind2ident (fst (splitAbs term))
    -- the number in the proof text of a line before compression
    number l = let k0 = maybe l id (lookup l renumbering)
                   k = maybe k0 id (lookup k0 renumbering1)
                   k2 = maybe k id (lookup k renumbering2)
               in maybe k2 id (lookup k2 renumbering3)
    


line2unitline :: Env ->  M.Map QIdent Exp -> Line Exp -> Line GUnit
line2unitline env dkmap line = line {
  step = (step line){formula = unit}
  }
 where
   -- a line is an assumption only if it cites a hypothesis without
   -- premisses; a hypothesis applied to premisses (a derived rule taken as
   -- a premiss) yields a conclusion, as a constant does
   unit = case (mark line, rule (step line)) of
     (CasesMark n, _) -> GNumberCasesGoal (caseNumber n)
     (CaseMark k, _) -> GNumberCaseGoal (caseNumber k) (exp2prop fla)
     (_, h) | constant h || not (null (premisses line)) -> case ruleWords h of
       Just (Left label) -> GLabelPropConclusion label (exp2prop fla)  -- "by modus ponens, B"
       Just (Right hence) -> GHencePropConclusion hence (exp2prop fla)  -- "in particular, B"
       Nothing -> GConclusionUnit (exp2prop fla)
     (_, h) -> case goal line of
       -- with -proof-goals: "to prove that B , assume that A"
       Just g | isFlag "-proof-goals" env ->
         GGoalHyposUnit (GListHypo (hypos2hypos [HVarExp h fla])) (exp2prop (head (annotateDkIdents env g)))
       _ -> GHyposUnit (GListHypo (hypos2hypos [HVarExp h fla]))
   fla = head (annotateDkIdents env (formula (step line)))

   caseNumber k = [Gone_CaseNumber, Gtwo_CaseNumber, Gthree_CaseNumber, Gfour_CaseNumber,
                   Gfive_CaseNumber, Gsix_CaseNumber, Gseven_CaseNumber, Geight_CaseNumber,
                   Gnine_CaseNumber] !! (k - 1)

   -- a rule in words, if the symbol table gives it as a Label (its name: "by
   -- the definition of union") or as a Hence (an adverb: "in both cases")
   ruleWords h = case annotateDkIdents env (EIdent h) of
     EIdent q@(QIdent s) : _ -> case lookupConstant s of
       Just ("Hence", c) -> Just (Right (fgTree c))
       _ -> case ident2label q of
         GIdentLabel _ -> Nothing
         label -> Just (Left label)
     _ -> Nothing

   constant h = maybe False (const True) (M.lookup h dkmap)

-- merge consecutive lines introducing variables of the same kind into one,
-- "let x, y be A", and renumber the lines and their premisses accordingly;
-- the first argument recognizes such a line, the second builds the merged one
compressVars :: Eq k => (Line a -> Maybe ([v], k)) -> ([v] -> k -> Line a -> Line a)
                     -> [Line a] -> [Line a]
compressVars varsLine merge = fst . compressVarsMap varsLine merge

-- the same, with the renumbering: old line number -> new line number
compressVarsMap :: Eq k => (Line a -> Maybe ([v], k)) -> ([v] -> k -> Line a -> Line a)
                        -> [Line a] -> ([Line a], [(Int, Int)])
compressVarsMap varsLine merge = go 0 []
 where
  go gaps relines ls = case ls of
    ln : rest | Just (xs, kind) <- varsLine ln ->
      let (same, rest') = span (sameKind kind) rest
          xss = xs ++ concat [ys | Just (ys, _) <- map varsLine same]
          nln = line ln - gaps
          relines' = [(line l, nln) | l <- ln : same] ++ relines
          (lns, rels) = go (gaps + length same) relines' rest'
      in ((merge xss kind ln){line = nln} : lns, rels)
    ln : rest ->
      let nln = line ln - gaps
          (lns, rels) = go gaps ((line ln, nln) : relines) rest
      in (ln{line = nln,
             premisses = nub [maybe p id (lookup p relines) | p <- premisses ln],
             closes = nub [maybe p id (lookup p relines) | p <- closes ln]} : lns, rels)
    [] -> ([], relines)

  sameKind kind ln = case varsLine ln of
    Just (_, k) -> k == kind
    _ -> False

compressVarsHypos :: [Line GUnit] -> [Line GUnit]
compressVarsHypos = fst . compressVarsHyposMap

compressVarsHyposMap :: [Line GUnit] -> ([Line GUnit], [(Int, Int)])
compressVarsHyposMap = compressVarsMap varsHypo merge
 where
  varsHypo :: Line GUnit -> Maybe ([GIdent], GKind)
  varsHypo ln = case formula (step ln) of
    GHyposUnit (GListHypo [GVarsHypo (GListIdent xs) kind]) -> Just (xs, kind)
    _ -> Nothing
  merge xs kind ln =
    ln{step = (step ln){formula = GHyposUnit (GListHypo [GVarsHypo (GListIdent xs) kind])}}

-- | The lines without those that introduce variables of implicit types (or
-- that the predicate otherwise selects among the introducing lines), and
-- the renumbering of the others; they are no longer cited as premisses, and
-- the goal of a dropped line goes to the next line of its scope.
dropImplicitLines :: (Line Exp -> Bool) -> [Line Exp] -> ([Line Exp], [(Int, Int)])
dropImplicitLines implicit = go 0 []
 where
  go gaps rel ls = case ls of
    l : rest | introduces l && implicit l ->
      let rest' = case rest of
            l2 : rest2 | goal l /= Nothing && introduces l2 && context l2 == context l ->
              l2{goal = goal l} : rest2
            _ -> rest
      in go (gaps + 1) rel rest'
    l : rest ->
      let n = line l - gaps
          (lns, rels) = go gaps ((line l, n) : rel) rest
      in (l{line = n, premisses = [p' | p <- premisses l, Just p' <- [lookup p rel]],
            closes = [p' | p <- closes l, Just p' <- [lookup p rel]]} : lns, rels)
    [] -> ([], rel)
  introduces l = null (premisses l) && elem (rule (step l)) (context l) && mark l == NoMark

-- | The consecutive lines that introduce the variables and hypotheses of one
-- scope (they have the same context) as one line, "let x be an A such that
-- B", aggregated as the hypotheses of a statement are; with the goal of its
-- first line. The lines of a case analysis are not merged.
compressScopesMap :: [Line GUnit] -> ([Line GUnit], [(Int, Int)])
compressScopesMap = compressVarsMap scopeLine merge
 where
  scopeLine :: Line GUnit -> Maybe ([([GHypo], Maybe GProp)], [QIdent])
  scopeLine ln
    | mark ln == NoMark && null (premisses ln) && elem (rule (step ln)) (context ln) =
        case formula (step ln) of
          GHyposUnit (GListHypo hs) -> Just ([(hs, Nothing)], context ln)
          GGoalHyposUnit (GListHypo hs) g -> Just ([(hs, Just g)], context ln)
          _ -> Nothing
    | otherwise = Nothing
  merge xs _ ln =
    let hs = conjoined (concatMap fst xs)
        unit = case xs of
          (_, Just g) : _ -> GGoalHyposUnit (GListHypo hs) g
          _ -> GHyposUnit (GListHypo hs)
    in ln{step = (step ln){formula = unit}}

  -- consecutive hypotheses of propositions as one: "assume that A and B"
  conjoined hs = case hs of
    h : rest@(h2 : _) | Just p <- prop h, Just _ <- prop h2 ->
      let (ps, rest') = span (maybe False (const True) . prop) rest
      in GPropHypo (GAndProp (GListProp (p : [q | Just q <- map prop ps]))) : conjoined rest'
    h : rest -> h : conjoined rest
    [] -> []
  prop :: GHypo -> Maybe GProp
  prop h = case h of
    GPropHypo p -> Just p
    GPropVarHypo _ p -> Just p
    _ -> Nothing

-- in Dedukti, the merged line shows the variables as one label "x, y : A"
compressVarsLines :: [Line Exp] -> [Line Exp]
compressVarsLines = compressVars varsLine merge
 where
  varsLine ln
    | null (premisses ln) && elem (rule (step ln)) (context ln)
        && headedBy identElem (formula (step ln))
      = Just ([rule (step ln)], formula (step ln))
    | otherwise = Nothing
  merge xs _ ln =
    ln{step = (step ln){rule = QIdent (concat (intersperse ", " [x | QIdent x <- xs]))}}


-------------------------------
-- data types and constructors
-------------------------------

-- proof steps (lines on trees)

data Step a = Step {
  hypovar :: QIdent,      -- relevant only for hypotheses
  formula :: a,           -- formula assumed or concluded
  rule    :: QIdent,      -- the rule that is used
  discharged :: [QIdent]  -- hypolabels of discharged formulas
  }
  deriving (Show, Eq)

mkStep :: QIdent -> a -> QIdent -> [QIdent] -> Step a
mkStep li fo ru di = Step li fo ru di

-- proof lines in Jaskowski-style notation

data Line a = Line {
  line      :: Int,      -- line number
  context   :: [QIdent], -- labels of open hypotheses
  premisses :: [Int],    -- line numbers of premisses
  step :: Step a,        -- the main content of the line
  mark :: Mark,          -- its part in a case analysis
  goal :: Maybe Exp,     -- for the first variable or hypothesis that a premiss
                         -- binds: the conclusion of the rule, which it serves
  closes :: [Int]        -- the lines of the hypotheses that the rule discharges
  }
  deriving (Show, Eq)

-- | The lines of a case analysis: "we have n cases", and the hypothesis that
-- opens case k
data Mark = NoMark | CasesMark Int | CaseMark Int
  deriving (Show, Eq)

mkLine :: Int -> [QIdent] -> a -> QIdent -> [Int] -> [QIdent] -> Line a
mkLine li co fo ru prs di = Line li co prs (mkStep noIdent fo ru di) NoMark Nothing []

mkHypoLine :: Int -> a -> QIdent -> QIdent -> Line a
mkHypoLine li fo ru hy = Line li [hy] [] (mkStep hy fo ru []) NoMark Nothing []

-- | The line of "we have n cases"
mkCasesLine :: Int -> [QIdent] -> Int -> Line Exp
mkCasesLine li co n = Line li co [] (mkStep noIdent (EIdent casesIdent) casesIdent []) (CasesMark n) Nothing []

casesIdent = QIdent "#CASES" ----

noIdent = QIdent "#NOIDENT" ---- 

-- an object-language type Elem A (as opposed to a Proof of a proposition)
isElemType :: Exp -> Bool
isElemType e = case splitApp e of
  (EIdent _, _) -> True
  _ -> False

-- | A type that a symbol table declares implicit (#IMPLICIT Elem universeset):
-- its variables are not introduced in statements and proof texts, unless
-- with -show-implicit-hypotheses
isImplicitType :: Env -> Exp -> Bool
isImplicitType env t =
  not (isFlag "-show-implicit-hypotheses" env) &&
  elem (normalizeType (printTree t)) (implicitTypes (symbolTable env))

-- | The positions of the hypotheses of implicit types in a judgement that does
-- not prove anything (a definition): they are not dropped, as the definiendum
-- applies the constant to them, but only left unsaid (Dedukti2MathCore.hideHypoVars)
implicitPositions :: Env -> Jmt -> [Int]
implicitPositions env jmt = case jmtType jmt of
  Just t | not (headedBy identProof (snd (splitType t))) ->
    [i | (i, h) <- zip [0..] (fst (splitType t)), Just a <- [hypo2type h], isImplicitType env a]
  _ -> []

-- | The variables at those positions, in the judgement as annotated
hypoVarsAt :: [Int] -> Jmt -> [QIdent]
hypoVarsAt [] _ = []
hypoVarsAt is jmt = case jmtType jmt of
  Just t -> [v | (i, h) <- zip [0..] (fst (splitType t)), elem i is, v <- hypo2vars h]
  _ -> []

jmtType :: Jmt -> Maybe Exp
jmtType jmt = case jmt of
  JStatic _ t -> Just t
  JDef _ (MTExp t) _ -> Just t
  JThm _ (MTExp t) _ -> Just t
  JInj _ (MTExp t) _ -> Just t
  _ -> Nothing

-- | A statement (of a proof) without its leading hypotheses of implicit types
dropImplicitHypos :: Env -> Jmt -> Jmt
dropImplicitHypos env jmt = case jmt of
  JStatic c t | proves t -> JStatic c (dropFun t)
  JThm c (MTExp t) MENone | proves t -> JThm c (MTExp (dropFun t)) MENone
  JDef c (MTExp t) MENone | proves t -> JDef c (MTExp (dropFun t)) MENone
  _ -> jmt
 where
   proves t = headedBy identProof (snd (splitType t))
   dropFun t = case t of
     EFun (HVarExp _ a) r | isImplicitType env a -> dropFun r
     EFun (HParVarExp _ a) r | isImplicitType env a -> dropFun r
     EFun h r -> EFun h (dropFun r)
     _ -> t

-- the variables a lambda binds over objects, which a proof text introduces
objectBinders :: [Bind] -> [(QIdent, Exp)]
objectBinders binds = [(v, t) | BTyped v t <- binds, headedBy identElem t]

-- the variables it binds over objects and over proofs, in their order: the
-- "let x be an A" and the "assume that A" of the proof text, which are
-- stated where the lambda binds them, not where they are first used
introducedBinders :: [Bind] -> [(QIdent, Exp)]
introducedBinders binds =
  [(v, t) | BTyped v t <- binds, headedBy identElem t || headedBy identProof t]


-----------------------------------------
-- type annotation
-----------------------------------------

-- | The type of the bound variable x and of the body of an abstraction of type
-- typ: a function type, or a proof of an implication, a negation or a
-- universal statement, which are functions in proofs translated from Lean
-- (Proof (if A B) is Proof A -> Proof B, Proof (not A) is Proof A -> Proof
-- false, and Proof (forall A P) is (x : Elem A) -> Proof (P x)).
abstractionTypes :: [QIdent] -> QIdent -> Exp -> Maybe (Exp, Exp)
abstractionTypes bound x typ = case typ of
  EFun h val -> do
    ty <- hypo2type h
    return (ty, subst (zip (hypo2vars h) [EIdent x]) bound val)
  EApp (EIdent p) prop | p == identProof -> case splitApp prop of
    (EIdent c, [a, b]) | c == identImpl -> return (proof a, proof b)
    (EIdent c, [a]) | c == identNot -> return (proof a, proof (EIdent identFalse))
    (EIdent c, [k, EAbs b pbody]) | c == identPi ->
      return (EApp (EIdent identElem) k, proof (subst [(bind2ident b, EIdent x)] bound pbody))
    (EIdent c, [k, pred]) | c == identPi ->
      return (EApp (EIdent identElem) k, proof (EApp pred (EIdent x)))
    _ -> Nothing
  _ -> Nothing
 where
   proof = EApp (EIdent identProof)

typeAnnotate :: M.Map QIdent Exp -> [(QIdent, Exp)] -> Exp -> Exp -> Exp
typeAnnotate mo cont typ exp = case exp of
  EApp _ _ -> case splitApp exp of
    (EIdent fun, args) ->
      let
        (hs, body) = splitType (look fun)
        vars = [case hypo2topvars h of {v:_ -> v; [] -> QIdent "_"} | h <- hs] -- one var per hypo, aligned 1:1 with args
        apptyp = subst (zip vars args) (map fst cont) body
        newargs = [typeAnnotate mo cont (subst (zip vars args) (map fst cont) ty) arg | (h, arg) <- zip hs args, Just ty <- [hypo2type h]]
      in ETyped (foldl EApp (EIdent fun) newargs) apptyp
  EIdent fun -> ETyped exp (look fun)
  EAbs bind body -> 
    let
      bindvar = bind2var bind
      (argtyp, bodytyp) = case abstractionTypes (map fst cont) bindvar typ of
        Just ts -> ts
        _ -> error ("incorrect type of abstraction " ++ printTree exp)
      vartyp = case bind of
        BTyped _ ty -> ty
        BVar _ -> argtyp
    in EAbs (BTyped bindvar vartyp)
            (typeAnnotate mo ((bindvar, vartyp): cont) bodytyp body)

 where
   look fun = case lookup fun cont of
     Just ty -> ty
     _ -> case M.lookup fun mo of
        Just ty -> ty
        _ -> typ

{- -- now in AnnotateDedukti
subst :: [(QIdent, Exp)] -> [QIdent] -> Exp -> Exp
subst gamma bs e = case e of
  EIdent x {- | notElem x bs -} -> case lookup x gamma of
    Just v -> v
    _ -> e
  EApp f a -> case subst gamma bs f of
    EAbs bind body -> subst [(bind2ident bind, subst gamma bs a)] bs body -- beta-reduce redex from substituted higher-order arg
    f' -> EApp f' (subst gamma bs a)
  EAbs (BTyped x ty) a -> EAbs (BTyped x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EAbs b a -> EAbs b (subst gamma (bind2ident b : bs) a)
  EFun (HVarExp x ty) a -> EFun (HVarExp x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EFun (HParVarExp x ty) a -> EFun (HParVarExp x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EFun h a -> EFun h (subst gamma (hypo2vars h ++ bs) a)
  _ -> e
-}

--------------------------------------
-- which subtrees of a step are shown
--------------------------------------

-- A Dedukti rule is applied to the propositions and objects it speaks about
-- as well as to its premisses, and only the premisses belong in a proof
-- text.  Which arguments those are is a profile of the constant, the same
-- notion that #DROP expresses as "all but the first k": the symbol table can
-- give one with #SHOW, and failing that it is read off the constant's type,
-- where the premisses are the arguments that are themselves proofs.
proofProfile :: M.Map QIdent Profile -> M.Map QIdent Exp -> QIdent -> Profile
proofProfile given types f = case M.lookup f given of
  Just prof -> prof
  _ -> case M.lookup f types of
    Just ty -> PermProfile [i | (i, h) <- zip [1 ..] (fst (splitType ty)), isProofHypo h]
    _ -> NoProfile
 where
   -- a premiss is an argument that yields a proof: not only Proof A, but
   -- also the functions that a rule discharging hypotheses takes, such as
   -- the step case of induction, (n : Elem Nat) -> Proof (C n) -> Proof ...
   isProofHypo h = maybe False (headedBy identProof . snd . splitType) (hypo2type h)

headedBy :: QIdent -> Exp -> Bool
headedBy f e = case splitApp e of
  (EIdent g, _:_) -> g == f
  _ -> False

-- keep, of every application in the proof term, the arguments its profile shows
showOnly :: (QIdent -> Profile) -> Exp -> Exp
showOnly prof e = case e of
  ETyped x ty -> ETyped (showOnly prof x) ty
  EApp _ _ -> case splitApp e of
    (EIdent f, args@(_:_)) ->
      foldl EApp (EIdent f) (map (showOnly prof) (select (prof f) args))
    (fun, args) -> foldl EApp (showOnly prof fun) (map (showOnly prof) args)
  EAbs bind body -> EAbs bind (showOnly prof body)
  _ -> e
 where
   select p args = case p of
     NoProfile -> args
     DropProfile k -> drop k args
     PermProfile is -> [args !! (i - 1) | i <- is, i <= length args]
     HoasProfile is -> [args !! (i - 1) | i <- is, i <= length args]

-----------------------
-- linear proofs
----------------------

term2lines :: Exp -> [Line Exp]
term2lines = (\(_, ls, _) -> ls) . term2linesMap

-- | The lines before compression (one for each step of the proof term, so
-- that they make a tree by their premisses), the compressed lines, and the
-- renumbering from the former to the latter, in which a dropped line points
-- to the line that it repeats.
term2linesMap :: Exp -> ([Line Exp], [Line Exp], [(Int, Int)])
term2linesMap proof = (raw, lns, relines)
  where
 raw = ps 1 [] proof
 (lns, relines) = compress 0 [] [] [] raw

 -- generate lines starting with this line number and context
 ps :: Int -> [QIdent] -> Exp -> [Line Exp]
 ps ln cont proof = case proof of -- next line number, its context 

   ETyped e typ -> case splitApp e of
    (EIdent fun, args) ->
      let
         -- a case analysis: two or more premisses that are hypothetical
         -- proofs of the conclusion itself, numbered in their order
         caseArgs = [a | a <- args, isCase typ a]
         cases = if length caseArgs >= 2 && length caseArgs <= 9
                   then snd (mapAccumL (\k a -> if isCase typ a then (k + 1, Just (k, length caseArgs))
                                                               else (k, Nothing)) 1 args)
                   else map (const Nothing) args
         (argss, prems, hypoLines, lnConcl) = psArgs ln cont typ (zip args cases)
      in concat argss ++
         [(mkLine lnConcl cont typ fun (nub prems) (concatMap absIdents args)){closes = nub hypoLines}]
   -- an abstraction introduces variables: the ones it binds over objects are
   -- the "let x be an A" of the proof text, and the ones over proofs its
   -- "assume that A", in the order the statement has them
   EAbs _ _ -> case splitAbs proof of
     (binds, body) ->
       let cont' = cont ++ map bind2ident binds
           introLines = binderLines ln cont' Nothing binds
       in introLines ++ ps (ln + length introLines) cont' body

   _ -> error ("term2lines " ++ printTree proof)

 -- process arguments left to right, threading line numbers; each argument yields its lines
 -- (eigenvariable lines for its Elem-typed binders, then its own derivation) together with the
 -- line numbers to cite as premisses (those eigenvariables and the argument's conclusion)
 -- the arguments of a rule whose conclusion is concl
 -- with the lines of the hypotheses that the premisses bind, which the rule
 -- discharges (not their eigenvariables, which are premisses)
 psArgs :: Int -> [QIdent] -> Exp -> [(Exp, Maybe (Int, Int))] -> ([[Line Exp]], [Int], [Int], Int)
 psArgs n _ _ [] = ([], [], [], n)
 psArgs n cont concl ((a, c):as) =
   let -- the first case is preceded by "we have n cases"
       casesLines = [mkCasesLine n cont k | Just (1, k) <- [c]]
       (ls, prem, hyps) = psArg (n + length casesLines) cont c concl a
       ls' = casesLines ++ ls
       (lss, prems, hypss, nf) = psArgs (nextline n ls') cont concl as
   in (ls' : lss, prem ++ prems, hyps ++ hypss, nf)

 -- an argument; the lines of the variables it binds over objects are
 -- premisses of the rule (eigenvariables), the lines of its hypotheses are not
 psArg :: Int -> [QIdent] -> Maybe (Int, Int) -> Exp -> Exp -> ([Line Exp], [Int], [Int])
 psArg n cont c concl arg = case arg of
   EAbs _ _ ->
     let (binds, body) = splitAbs arg
         cont' = cont ++ map bind2ident binds
         -- the goal is the conclusion of the rule, stated where it differs
         -- from what the premiss proves (as in an introduction rule, not in
         -- an elimination rule, which passes its conclusion on); a case has none
         proves = case body of
           ETyped _ t -> Just t
           _ -> Nothing
         -- the first line has the conclusion of the rule as its goal; in a
         -- scope that begins with variables, the first hypothesis has what
         -- the scope proves ("let x be ... ; to prove that x in B, assume ...")
         bls = binderLines n cont' (fmap fst c) binds
         firstHypo = take 1 [i | (i, l) <- zip [0 :: Int ..] bls, headedBy identProof (formula (step l))]
         introduces = c == Nothing && proves /= Just concl
         introLines = [if introduces && i == 0 then l{goal = Just concl}
                       else if introduces && i `elem` firstHypo then l{goal = proves} else l
                      | (i, l) <- zip [0 :: Int ..] bls]
         bodyLines = ps (n + length introLines) cont' body
     in (introLines ++ bodyLines,
         [line l | l <- introLines, headedBy identElem (formula (step l))] ++
         [lastline bodyLines | not (null bodyLines)],
         [line l | l <- introLines, headedBy identProof (formula (step l))])
   _ ->
     let ls = ps n cont arg
     in (ls, [lastline ls | not (null ls)], [])

 -- the lines that introduce the variables of a lambda; the first hypothesis
 -- of case k is marked as opening that case
 binderLines :: Int -> [QIdent] -> Maybe Int -> [Bind] -> [Line Exp]
 binderLines n cont' c binds =
   let vs = introducedBinders binds
       firstHypo = take 1 [v | (v, t) <- vs, headedBy identProof t]
   in [(mkLine (n + i) cont' t v [] []){mark = caseMark v}
      | (i, (v, t)) <- zip [0 ..] vs, let caseMark v = maybe NoMark CaseMark (if v `elem` firstHypo then c else Nothing)]

 -- an argument that is a hypothetical proof of the conclusion typ
 isCase :: Exp -> Exp -> Bool
 isCase typ a = case a of
   EAbs _ _ -> case splitAbs a of
     (binds, ETyped _ bodytyp) ->
       any (headedBy identProof . snd) (introducedBinders binds) && bodytyp == typ
     _ -> False
   _ -> False

 lastline = line . last
 nextline ln p = if null p then ln else lastline p + 1

 -- compress lines by dropping repeated hypotheses and repeated conclusions,
 -- and renumbering lines accordingly.  A conclusion is repeated if the same
 -- formula was concluded before in a context that is still open (its
 -- hypotheses and variables are among the present ones), so that the
 -- earlier line can be cited instead.
 -- A hypothesis or conclusion can be cited later only within its scope: where
 -- its context is a prefix of the present one (contexts grow by appending the
 -- variables bound), not in another case that binds a variable of the same name.
 compress :: Int -> [(Int, Int)] -> [((QIdent, Exp), Int, [QIdent])] -> [(Exp, Int, [QIdent])]
          -> [Line Exp] -> ([Line Exp], [(Int, Int)])
 compress gaps relines rehypos reconcls ls = case ls of
   ln : rest | isCasesLine ln ->  -- "we have n cases": kept, and not a conclusion to cite
     let nln = line ln - gaps
     in cons ln{line = nln} (compress gaps ((line ln, nln) : relines) rehypos reconcls rest)
   ln : rest | isHypoLine ln ->
     let key = (rule (step ln), formula (step ln)) -- a hypothesis is identified by its label and formula
     in case [k | (key', k, ctx) <- rehypos, key' == key, ctx `isPrefixOf` context ln] of
          k : _ ->  -- old hypothesis: drop this line, re-point its number to the first occurrence
            compress (gaps + 1) ((line ln, k) : relines) rehypos reconcls rest
          _ ->       -- new hypothesis: keep it, renumbering to close earlier gaps
            let nln = line ln - gaps
            in cons ln{line = nln}
               (compress gaps ((line ln, nln) : relines) ((key, nln, context ln) : rehypos) reconcls rest)
   ln : rest | k : _ <- [k | (f, k, ctx) <- reconcls, f == formula (step ln),
                              ctx `isPrefixOf` context ln] ->
     -- old conclusion: drop this line, re-point its number to the earlier one
     compress (gaps + 1) ((line ln, k) : relines) rehypos reconcls rest
   ln : rest ->
     let nln = line ln - gaps
         ds  = discharged (step ln) -- hypotheses closed here may be reused (as new) below
         rehypos' = [r | r@((h, _), _, _) <- rehypos, notElem h ds]
         -- and conclusions that depend on them can no longer be cited
         reconcls' = [r | r@(_, _, ctx) <- (formula (step ln), nln, context ln) : reconcls,
                          not (any (`elem` ds) ctx)]
     in cons (renumberLine nln relines ln)
        (compress gaps ((line ln, nln) : relines) rehypos' reconcls' rest)
   _ -> (ls, relines)

 cons l (ls, rels) = (l : ls, rels)

 -- a hypothesis line just cites an open assumption: no premisses, rule is a context variable
 isHypoLine ln = null (premisses ln) && elem (rule (step ln)) (context ln)
 isCasesLine ln = case mark ln of
   CasesMark _ -> True
   _ -> False

 -- change the line number and re-point all references to premiss line numbers
 renumberLine num relines ln = ln {
    premisses = [maybe p id (lookup p relines) | p <- premisses ln],
    closes = nub [maybe p id (lookup p relines) | p <- closes ln],
    line = num
    }

----------------------------
-- natural deduction trees
----------------------------

-- | The proof as a natural deduction tree (bussproofs), one node for each
-- line before compression: its formula, on the left the number of its line
-- in the proof text (the same number can occur several times, as the text
-- states a repeated conclusion or hypothesis once), on the right the rule
-- and the lines of the hypotheses it discharges.
proofTree :: (Exp -> String) -> [Line Exp] -> (Int -> Int) -> String
proofTree latex raw number = unlines $
  ["\\begin{prooftree}"] ++ node (last raw) ++ ["\\end{prooftree}"]
 where
  byLine = M.fromList [(line l, l) | l <- raw]
  hypo l = null (premisses l) && elem (rule (step l)) (context l)
  object l = headedBy identElem (formula (step l))
  premissLines l = [k | p <- premisses l, Just k <- [M.lookup p byLine]]
  subtree l = l : concatMap subtree (premissLines l)
  num l = "\\scriptstyle " ++ show (number (line l))
  fla l = latex (formula (step l))

  node l
    | hypo l = ["\\AxiomC{${" ++ num l ++ "}\\quad " ++ fla l ++ "$}"]
    | otherwise =
        let kids = filter (not . object) (premissLines l)
            dis = nub (map number (closes l))
        in (if null kids then ["\\AxiomC{}"] else concatMap node kids) ++
           [ "\\LeftLabel{$" ++ num l ++ "$}"
           , "\\RightLabel{$\\scriptstyle " ++ mathtt (printTree (rule (step l))) ++
               (if null dis then "" else "\\ (" ++ concat (intersperse "," (map show dis)) ++ ")") ++ "$}"
           , inference (length kids) ++ "{$" ++ fla l ++ "$}" ]

  inference n = case n of
    0 -> "\\UnaryInfC"
    1 -> "\\UnaryInfC"
    2 -> "\\BinaryInfC"
    3 -> "\\TrinaryInfC"
    4 -> "\\QuaternaryInfC"
    _ -> "\\QuinaryInfC"  --- bussproofs has at most five premisses

  mathtt s = "\\mathtt{" ++ concatMap (\c -> if c == '_' then "\\_" else [c]) s ++ "}"

----------------------------
-- printing
-----------------------------

-- a proof is a table of numbered lines: what is claimed, and why
prls :: (Line a -> [String]) -> [Line a] -> String
prls pr lns = unlines $
  "\\begin{tabular}{rp{0.62\\textwidth}l}" :
  [concat (intersperse " & " (pr ln)) ++ " \\\\" | ln <- lns] ++
  ["\\end{tabular}"] 

prle :: Line Exp -> [String]
prle ln
  | null (premisses ln) = [
      show (line ln) ++ ".",
      "\\verb#" ++ printTree (rule (step ln)) ++ " : " ++ printTree (formula (step ln)) ++ "#",
      "" ]
  | otherwise = [
      show (line ln) ++ ".",
      "\\verb#" ++ printTree (formula (step ln)) ++ "#",
      justification ln
      ]
  
prlu :: (GUnit -> String) -> Int -> Line GUnit -> [String]
prlu lin depth ln = [
  show (line ln) ++ ".",
  concat (replicate depth "\\hspace*{1.5em}") ++ lin (formula (step ln)),
  justification ln
  ]

-- why the line holds: the rule and the lines it is applied to, and the
-- hypotheses it closes
justification :: Line a -> String
justification ln
  | mark ln /= NoMark = ""                                          -- "we have n cases"
  | null (premisses ln) && elem (rule (step ln)) (context ln) = ""  -- a hypothesis or variable
  | null (premisses ln) = texttt (printTree (rule (step ln)))       -- a lemma without premisses
  | otherwise = unwords [
      texttt (printTree (rule (step ln))),
      concat (intersperse ", " (map show (premisses ln))),
      -- the lines of the hypotheses discharged, as in the trees
      case closes ln of
        [] -> ""
        dis -> "(" ++ concat (intersperse ", " (map show dis)) ++ ")"
      ]

texttt :: String -> String
texttt s = "\\texttt{" ++ concatMap (\c -> if c == '_' then "\\_" else [c]) s ++ "}"
  

---- TODO: pretty-printing on multiple lines
prt :: Exp -> String
prt exp = printTree exp

mathdisplay :: [Char] -> [Char]
mathdisplay s = "\\[" ++ s ++ "\\]"

verbatim :: String -> [Char]
verbatim s = "\\begin{verbatim}\n" ++ unlines (splitLines (words s)) ++ "\n\\end{verbatim}"

splitLines :: [String] -> [String]
splitLines ws = case splitAt 10 ws of
  (line, rest@(_:_)) -> unwords line : splitLines rest
  _ -> [unwords ws]

-- a document of the proofs, with the macros their sentences may use
prLatexFile :: [String] -> String -> String
prLatexFile macros string = unlines (
  ["\\documentclass{article}",
   "\\usepackage{amsfonts}",
   "\\usepackage{amssymb}",
   "\\usepackage{amsmath}",
   "\\usepackage[margin=2cm]{geometry}",  -- a proof line can be long
   "\\setlength\\parindent{0pt}",
   "\\setlength\\parskip{8pt}",
   "\\begin{document}"] ++
  macros ++
  [string,
   "\\end{document}"])

verbatimInline :: String -> String
verbatimInline s = "\\texttt{" ++ concatMap escape s ++ "}"
  where
    escape c = case c of
      '_' -> "\\_"
      '&' -> "\\&"
      '#' -> "\\#"
      '%' -> "\\%"
      '$' -> "\\$"
      '{' -> "\\{"
      '}' -> "\\}"
      '\\' -> "\\textbackslash{}"
      _ -> [c]

