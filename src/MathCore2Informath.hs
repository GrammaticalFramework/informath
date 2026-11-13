{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module MathCore2Informath where

import GodementNLG (godementVariants, godementVariantsSafe)
import Informath
import Environment
import BuildConstantTable (nlgTable)
import Semantics (appNLGDefs)

import Data.List (nub)
import Data.Monoid (Any(..))

type Opts = [String]

nlg :: Env -> GJmt -> [GJmt] --- Tree a -> [Tree a]
nlg env tree = case () of
  _ | elem "-mathcore" (flags env) -> [t]
  _ | isFlag "-godement" env -> godementvariants   -- not sampled: the caller chooses among them
  _  -> sample chosenVariants
  ---- TODO more option combinations
 where
   -- -roundtrip-safe: no step whose loss of information the semantics cannot
   -- restore (doc/nlg-and-semantics.md, Part 5)
   safe = isFlag "-roundtrip-safe" env
   t = unparenth tree
   ut = funKinds (uncoerce t)
   ft = flatten (extendStructure ut)
   afts = [(if safe then aggregateSafe else aggregate) ft]
   iafts = map insitu afts
   viafts = map varless iafts
   cviafts = concatMap (if safe then collectivizeSafe else collectivize) viafts
   ncviafts = map (scoped . implied . negated) cviafts  -- better do this at this late stage
   vncviafts = concatMap (\u -> filter (not safe ||| restorableVariation u) (variations u)) ncviafts
   uservariants = if safe then [] else concatMap (appNLGDefs (nlgTable (symbolTable env))) vncviafts
   godementvariants = concatMap (if safe then godementVariantsSafe else godementVariants) ncviafts
   (|||) a b x = a || b x

   sample ts = [t | (t, i) <- zip ts [0 ..], mod i fact == 0]
   fact = samplingFactor env
   chosenVariants = case () of
     _ | isFlag "-more-variants" env ->
       concat [[ft], afts, iafts, viafts, cviafts, ncviafts, vncviafts, uservariants]
     _ -> map letDeclared ncviafts

-- | Variables of a set given by a term are declared with a formula: "let
-- $x, y \\in G$" rather than "let $x$ and $y$ be elements of $G$", also
-- with a condition: "let $x, y \\in G$ such that P".
letDeclared :: Tree a -> Tree a
letDeclared t = case t of
  GVarsHypo (GListIdent xs) (GExpKind (GTermExp term)) ->
    GLetDeclarationHypo (GElemDeclaration (GListTerm [GIdentTerm x | x <- xs]) term)
  GListHypo hs -> GListHypo (concatMap declared hs)
  _ -> composOp letDeclared t
 where
  declared :: GHypo -> [GHypo]
  declared h = case h of
    GSuchThatHypo (GListIdent xs) prop (GExpKind (GTermExp term)) ->
      [GLetFormulaSuchThatHypo (GElemFormula (GListTerm [GIdentTerm x | x <- xs]) term) (letDeclared prop)]
    _ -> [letDeclared h]

unparenth :: Tree a -> Tree a
unparenth t = case t of
  GCoreAndProp a b -> GAndProp (GListProp (map unparenth [a, b]))
  GCoreOrProp a b -> GOrProp (GListProp (map unparenth [a, b]))
  GCoreIfProp a b -> GIfProp (unparenth a) (unparenth b)
  GCoreIffProp a b -> GIffProp (unparenth a) (unparenth b)
  _ -> composOp unparenth t

-- | Quantifiers over variables of an implicit type (#IMPLICIT) without their
-- kind: "for all x" for "for all mathematical objects x". Applied last, to the
-- Informath trees (AllProp, ExistProp) and to the MathCore ones for symbolic
-- LaTeX (CoreAllProp, CoreExistProp), as the other passes need the kind
-- ("every mathematical object is ..."). The kinds are compared uncoerced.
bareQuantifiers :: [GKind] -> Tree a -> Tree a
bareQuantifiers [] t = t
bareQuantifiers kinds t = case t of
  GAllProp (GListArgKind [GIdentsArgKind k xs]) p | implicit k -> GBareAllProp xs (bare p)
  GExistProp (GListArgKind [GIdentsArgKind k xs]) p | implicit k -> GBareExistProp xs (bare p)
  GCoreAllProp k x p | implicit k -> case bare p of
    GBareAllProp (GListIdent ys) q -> GBareAllProp (GListIdent (x : ys)) q
    q -> GBareAllProp (GListIdent [x]) q
  GCoreExistProp k x p | implicit k -> case bare p of
    GBareExistProp (GListIdent ys) q -> GBareExistProp (GListIdent (x : ys)) q
    q -> GBareExistProp (GListIdent [x]) q
  _ -> composOp bare t
 where
  bare :: Tree b -> Tree b
  bare = bareQuantifiers kinds
  implicit k = elem (uncoerce k) (map uncoerce kinds)

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


-- | A function kind whose argument and value kinds are sets given by
-- expressions: for one argument, "function from A to B" (FunExpKind), rather
-- than "function from elements of A to elements of B" (FunKind), with the
-- symbolic "$f \\in A \\rightarrow B$" as a variant when A and B are symbolic
-- (see variations); for more arguments, when they are all symbolic, the
-- symbolic function type $A \\rightarrow B \\rightarrow C$ (FunTypeTerm)
funKinds :: Tree a -> Tree a
funKinds t = case t of
  GFunKind (GListArgKind args) val
    | Just es <- mapM argExp args, Just v <- kindExp val ->
      let es' = map funKinds es
          v' = funKinds v
      in case (es', mapM termOf es', termOf v') of
        ([e], _, _) -> GFunExpKind e v'
        (_, Just ts, Just tv) -> GExpKind (GTermExp (foldr GFunTypeTerm tv ts))
        _ -> GFunKind (GListArgKind (map funKinds args)) (GExpKind v')
  _ -> composOp funKinds t
 where
  argExp :: GArgKind -> Maybe GExp
  argExp a = case a of
    GKindArgKind k -> kindExp k
    _ -> Nothing
  kindExp :: GKind -> Maybe GExp
  kindExp k = case k of
    GExpKind e -> Just e
    _ -> Nothing
  termOf :: GExp -> Maybe GTerm
  termOf e = case e of
    GTermExp tm -> Just tm
    _ -> Nothing

aggregate :: Tree a -> Tree a
aggregate = aggregateWith False

-- | aggregate for -roundtrip-safe: the hypotheses keep their order and number
-- (aggregateHyposSafe), as they are the arguments of the constant in Dedukti.
aggregateSafe :: Tree a -> Tree a
aggregateSafe = aggregateWith True

aggregateWith :: Bool -> Tree a -> Tree a
aggregateWith safe t = case t of
  GCoreNotProp prop -> case aggregateWith safe prop of
    GAdjProp adj x -> GNotAdjProp adj x
    aprop -> GCoreNotProp aprop
  GAndProp (GListProp props) ->
    case groupProps "and" (map (aggregateWith safe) props) of
      [p] -> p
      pp -> GAndProp (GListProp pp)
  GOrProp (GListProp props) ->
    -- not inside: "x and y are even" would be grouped to "x and y are even or odd"
    case groupProps "or" props of
      [p] -> p
      pp -> GOrProp (GListProp pp)
  -- a variable with a condition on it, a comparison: "for all real numbers
  -- $\\epsilon > 0$, P", "there exists a real number $\\delta > 0$ such that P"
  GCoreAllProp kind x (GIfProp cond body) | Just f <- bounding x cond ->
    boundedAll kind f (aggregateWith safe body)
  GCoreAllProp kind x (GCoreIfProp cond body) | Just f <- bounding x cond ->
    boundedAll kind f (aggregateWith safe body)
  GCoreExistProp kind x (GAndProp (GListProp [cond, body])) | Just f <- bounding x cond ->
    boundedExist kind f (aggregateWith safe body)
  GCoreExistProp kind x (GCoreAndProp cond body) | Just f <- bounding x cond ->
    boundedExist kind f (aggregateWith safe body)
  GCoreAllProp kind x prop -> case getAlls kind prop of
    (ys, body) -> GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent (x : ys))]) (aggregateWith safe body)
  GCoreExistProp kind x prop -> case getExists kind prop of
    (ys, body) -> GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent (x : ys))]) (aggregateWith safe body)
  GListHypo hypos -> GListHypo ((if safe then aggregateHyposSafe else aggregateHypos) hypos)
  _ -> composOp (aggregateWith safe) t

-- | Hypotheses aggregated: variables of the same kind are brought together,
-- as far as the type dependencies allow, and the propositions about them are
-- attached to them: "let $x$ and $y$ be even numbers" (AdjKindHypo) when each
-- variable has the same adjective, "let $x$ and $y$ be numbers such that $x >
-- y$" (SuchThatHypo) otherwise. Two remaining propositions are conjoined.
aggregateHypos :: [GHypo] -> [GHypo]
aggregateHypos = aggregateHyposWith False

-- | For -roundtrip-safe, the hypotheses are aggregated only so that the
-- semantics gives back the same hypotheses in the same order: no reordering
-- of variables (reorderVar), a class assumption made a ClassKind only right
-- after its variable and only for one variable, and an adjective kind only
-- with the variables in the order of their hypotheses. Propositions are
-- attached and paired, as the semantics splits "and" (semHypo), and a
-- hypothesis that is a conjunction is "both A and B".
aggregateHyposSafe :: [GHypo] -> [GHypo]
aggregateHyposSafe = aggregateHyposWith True

aggregateHyposWith :: Bool -> [GHypo] -> [GHypo]
aggregateHyposWith safe
  | safe = pairProps . attachProps . groupVars . nounKinds . concatMap (splitVars . oneConjunction)
  | otherwise = pairProps . attachProps . groupVars . foldl reorderVar [] . nounKinds . concatMap (splitVars . oneConjunction)
 where
   -- a hypothesis that is a conjunction is "both A and B", as the semantics
   -- reads a conjunction with "and" as several hypotheses (semHypo)
   oneConjunction :: GHypo -> GHypo
   oneConjunction h = case h of
     GPropHypo p -> GPropHypo (both p)
     GPropVarHypo x p -> GPropVarHypo x (both p)
     _ -> h
   both :: GProp -> GProp
   both p = case p of
     -- "both x and y are A", "x is both A and B"
     GAndProp (GListProp [_, _]) | GAdjProp adj (GAndExp (GListExp [x, y])) <- aggregate p ->
       GAdjProp adj (GBothAndExp x y)
     GAndProp (GListProp [_, _]) | GAdjProp (GAndAdj (GListAdj [a, b])) e <- aggregate p ->
       GAdjProp (GBothAndAdj a b) e
     GAndProp (GListProp [a, b]) -> GBothAndProp a b
     GAndProp (GListProp (a : bs@(_ : _))) -> GBothAndProp a (GAndProp (GListProp bs))
     _ -> p

   -- a set variable of which a later hypothesis predicates a noun gets the
   -- noun as its kind, and the hypothesis goes: the class assumptions of
   -- Lean come after all the types ("let R be a semiring" instead of "let R
   -- be a set ... assume that R is a semiring"); the kind is ClassKind, so
   -- that the semantics gives back the set and the assumption
   nounKinds :: [GHypo] -> [GHypo]
   nounKinds hs = case hs of
     GVarsHypo xs@(GListIdent [x]) kind : rest | kind == setKind ->
       case break (isNounHypo x) rest of
         (before, h : after) | Just (GNoun1Prop (GNounNoun1 n) _) <- propOfHypo h, not safe || null before ->
           GVarsHypo xs (GClassKind n) : nounKinds (before ++ after)
         _ -> GVarsHypo xs kind : nounKinds rest
     h : rest -> h : nounKinds rest
     [] -> []
   setKind = GNounKind (LexNoun "set_Noun")
   isNounHypo :: GIdent -> GHypo -> Bool
   isNounHypo x h = case propOfHypo h of
     Just (GNoun1Prop (GNounNoun1 _) (GTermExp (GIdentTerm y))) -> y == x
     _ -> False

   -- one variable at a time
   splitVars :: GHypo -> [GHypo]
   splitVars h = case h of
     GVarsHypo (GListIdent xs) kind -> [GVarsHypo (GListIdent [x]) kind | x <- xs]
     _ -> [h]

   -- a variable goes right after the last variable of its kind, if its kind
   -- depends on nothing declared after that; a proposition between them
   -- cannot depend on it, as it comes before it
   reorderVar :: [GHypo] -> GHypo -> [GHypo]
   reorderVar hs h = case h of
     GVarsHypo (GListIdent [_]) kind ->
       case [i | (i, GVarsHypo (GListIdent [_]) k) <- zip [0 ..] hs, k == kind] of
         [] -> hs ++ [h]
         is -> let (before, after) = splitAt (last is + 1) hs
               in if any (`elem` concatMap declaredIdents after) (identsIn kind)
                  then hs ++ [h]
                  else before ++ [h] ++ after
     _ -> hs ++ [h]

   groupVars :: [GHypo] -> [GHypo]
   groupVars hs = case hs of
     -- for -roundtrip-safe, not variables of a class, which the semantics
     -- gives as sets first and then their class assumptions
     GVarsHypo (GListIdent [x]) kind@(GClassKind _) : rest | safe ->
       GVarsHypo (GListIdent [x]) kind : groupVars rest
     GVarsHypo (GListIdent [x]) kind : rest ->
       let (ys, rest') = sameKind kind rest
       in GVarsHypo (GListIdent (x : ys)) kind : groupVars rest'
     h : rest -> h : groupVars rest
     [] -> []
   sameKind :: GKind -> [GHypo] -> ([GIdent], [GHypo])
   sameKind kind hs = case hs of
     GVarsHypo (GListIdent [y]) k : rest | k == kind ->
       let (ys, rest') = sameKind kind rest in (y : ys, rest')
     _ -> ([], hs)

   -- the propositions right after variables that mention some of them
   attachProps :: [GHypo] -> [GHypo]
   attachProps hs = case hs of
     GVarsHypo xs@(GListIdent vars) kind : rest ->
       let (props, rest') = propsAbout vars rest
       in case props of
            -- "such that not A and B" would be "both not A and B", one hypothesis
            p : _ : _ | isNegation p -> GVarsHypo xs kind : take (length props) rest ++ attachProps rest'
            _ -> attached xs kind props : attachProps rest'
     h : rest -> h : attachProps rest
     [] -> []
   propsAbout :: [GIdent] -> [GHypo] -> ([GProp], [GHypo])
   propsAbout vars hs = case hs of
     h : rest | Just p <- propOfHypo h, any (`elem` vars) (identsIn p) ->
       let (ps, rest') = propsAbout vars rest in (p : ps, rest')
     _ -> ([], hs)
   attached :: GListIdent -> GKind -> [GProp] -> GHypo
   attached xs@(GListIdent vars) kind props = case props of
     -- sets of which a noun is predicated have it as their kind: "let $G$ be
     -- a monoid", not "let $G$ be a set such that $G$ is a monoid"
     _ | not safe, kind == GNounKind (LexNoun "set_Noun"), Just (n, rest) <- nounOf vars props ->
       attached xs (GClassKind n) rest
     [] -> GVarsHypo xs kind
     -- "let $p$ and $q$ be prime natural numbers", also from "p and q are
     -- prime" as one hypothesis; not with a set given by a formula ("prime
     -- elements of N"), which is declared "let $p, q \in N$ such that ..."
     _ | not safe, not (isTermKind kind), GAdjProp adj _ : _ <- conjuncts,
         Just ys <- mapM (adjOf adj) conjuncts, sameElems ys vars, not safe || ys == vars ->
       GAdjKindHypo xs adj kind
     [p] -> GSuchThatHypo xs (aggregate p) kind
     _ -> GSuchThatHypo xs (aggregate (GAndProp (GListProp props))) kind
    where
     conjuncts = concatMap conjunctsOf props
   conjunctsOf :: GProp -> [GProp]
   conjunctsOf p = case p of
     GCoreAndProp a b -> conjunctsOf a ++ conjunctsOf b
     GAndProp (GListProp ps) -> concatMap conjunctsOf ps
     _ -> [p]
   isTermKind :: GKind -> Bool
   isTermKind k = case k of
     GExpKind (GTermExp _) -> True
     _ -> False
   -- a noun predicated of each of the variables, and the other propositions
   nounOf :: [GIdent] -> [GProp] -> Maybe (GNoun, [GProp])
   nounOf vars props = case [n | GNoun1Prop (GNounNoun1 n) _ <- props] of
     n : _ | all (\v -> any (isNounOf n v) props) vars ->
       Just (n, [p | p <- props, not (any (\v -> isNounOf n v p) vars)])
     _ -> Nothing
   isNounOf :: GNoun -> GIdent -> GProp -> Bool
   isNounOf n v p = case p of
     GNoun1Prop (GNounNoun1 m) (GTermExp (GIdentTerm y)) -> m == n && y == v
     _ -> False
   adjOf :: GAdj -> GProp -> Maybe GIdent
   adjOf adj p = case p of
     GAdjProp a (GTermExp (GIdentTerm y)) | a == adj -> Just y
     _ -> Nothing
   sameElems :: [GIdent] -> [GIdent] -> Bool
   sameElems ys vars = length ys == length vars && all (`elem` ys) vars

   -- the labels of propositions are dropped in NLG (varless)
   propOfHypo :: GHypo -> Maybe GProp
   propOfHypo h = case h of
     GPropHypo p -> Just p
     GPropVarHypo _ p -> Just p
     _ -> Nothing

   pairProps :: [GHypo] -> [GHypo]
   pairProps hs = case hs of
     -- not after a negation, which scoped would make "both not A and B",
     -- one hypothesis for the semantics
     GPropHypo a : GPropHypo b : rest | not (isNegation a) ->
       GPropHypo (aggregate (GAndProp (GListProp [a, b]))) : pairProps rest
     h : rest -> aggregate h : pairProps rest
     [] -> []

isNegation :: GProp -> Bool
isNegation p = case p of
  GCoreNotProp _ -> True
  _ -> False

-- | The identifiers occurring in a tree: its variables, and bound ones too.
identsIn :: Tree a -> [GIdent]
identsIn t = case t of
  GStrIdent _ -> [t]
  _ -> composOpMonoid identsIn t

-- | The identifiers that a hypothesis declares.
declaredIdents :: GHypo -> [GIdent]
declaredIdents h = case h of
  GVarsHypo (GListIdent xs) _ -> xs
  GVarHypo x _ -> [x]
  GPropVarHypo x _ -> [x]
  GLocalHypo local -> identsIn local
  _ -> []


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

-- | A bounded quantifier: over a set given by a symbol, "for all $\\epsilon > 0$ in
-- $\\mathbb{R}$"; else with its kind, "for all real numbers $\\epsilon > 0$"
boundedAll :: GKind -> GFormula -> GProp -> GProp
boundedAll kind f body = case kind of
  GExpKind e@(GTermExp _) -> GBoundedInAllProp f e body
  _ -> GAllProp (GListArgKind [GFormulaArgKind kind f]) body

boundedExist :: GKind -> GFormula -> GProp -> GProp
boundedExist kind f body = case kind of
  GExpKind e@(GTermExp _) -> GBoundedInExistProp f e body
  _ -> GExistProp (GListArgKind [GFormulaArgKind kind f]) body

-- | The condition of a bounded quantifier over x: a comparison of x with a
-- term without x (x > 0, n \\geq N, x \\in s)
bounding :: GIdent -> GProp -> Maybe GFormula
bounding x cond = case cond of
  GFormulaProp f@(GEquationFormula (GBinaryEquation c (GIdentTerm y) t))
    | y == x, notElem x (identsIn t), boundingCompar c -> Just f
  _ -> Nothing
 where
  boundingCompar :: GCompar -> Bool
  boundingCompar (LexCompar c) = elem c ["Lt_Compar", "Gt_Compar", "Leq_Compar", "Geq_Compar",
    "Neq_Compar", "element_Compar", "subset_Compar", "subseteq_Compar"]

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
    -- for all a, P and for all a, Q: for all a, P and Q
    GAllProp ks p : GAllProp ks' q : pp | conj == "and" && sameArgKinds ks ks' ->
      groups (GAllProp ks (andProps p q) : pp)
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


-- | A structure whose clauses begin with its carrier and a parent of the
-- carrier read as a noun ("M is a semigroup") is defined by extension: "a
-- monoid is a semigroup M with an element 1 of M such that P". The other
-- clauses are its new data ("with") and its new properties ("such that").
extendStructure :: Tree a -> Tree a
extendStructure t = case t of
  -- with parameters before the carrier: "let R be a semiring. a module over R is ..."
  GStructureJmt label (GListClause clauses) kind
    | (params@(_:_), GVarClause x _ : GPropClause p : rest) <- splitCarrier clauses,
      Just parent <- nounKind x p,
      fields@(_:_) <- [(y, k) | GVarClause y k <- rest],
      props@(_:_) <- [q | GPropClause q <- rest] ->
      GExtensionHyposJmt label (GListHypo (paramHypos params)) kind parent x
        (GListArgKind (groupFields fields)) (conj props)
  GStructureJmt label (GListClause (GVarClause x setk : GPropClause p : rest)) kind
    | Just parent <- parentKind p,
      null [y | GVarClause y k <- rest, k == setk] ->
      let fields = groupFields [(y, k) | GVarClause y k <- rest]
          props = [q | GPropClause q <- rest]
      in case (fields, props) of
        ([], []) -> t
        ([], _) -> GExtensionPropJmt label kind parent x (conj props)
        (_, []) -> GExtensionDataJmt label kind parent x (GListArgKind fields)
        _ -> GExtensionJmt label kind parent x (GListArgKind fields) (conj props)
     where
      parentKind = nounKind x
  -- the parent a structure of its kind (M : Str GroupS): "an abelian group is
  -- a group M such that ..."
  GStructureJmt label (GListClause (GVarClause x parent@(GNounKind n) : rest)) kind
    | n /= LexNoun "set_Noun" ->
      let fields = groupFields [(y, k) | GVarClause y k <- rest]
          props = [q | GPropClause q <- rest]
      in case (fields, props) of
        ([], []) -> t
        ([], _) -> GExtensionPropJmt label kind parent x (conj props)
        (_, []) -> GExtensionDataJmt label kind parent x (GListArgKind fields)
        _ -> GExtensionJmt label kind parent x (GListArgKind fields) (conj props)
  _ -> t
 where
  conj ps = case ps of
    [p] -> p
    _ -> GAndProp (GListProp ps)
  nounKind :: GIdent -> GProp -> Maybe GKind
  nounKind x prop = case prop of
    GNoun1Prop (GNounNoun1 n) (GTermExp (GIdentTerm y)) | y == x -> Just (GNounKind n)
    _ -> Nothing
  -- the carrier is the last set (the kind of the first clause), if a noun
  -- about it follows: the parameters before it ("let R be a semiring")
  splitCarrier :: [GClause] -> ([GClause], [GClause])
  splitCarrier cs = case cs of
    GVarClause _ setk : _ ->
      case [i | (i, GVarClause _ k : _) <- zip [(0 :: Int)..] (suffixes cs), k == setk] of
        [] -> ([], cs)
        is -> case splitAt (last is) cs of
          split@(_, GVarClause x _ : GPropClause p : _) | Just _ <- nounKind x p -> split
          _ -> ([], [])
    _ -> ([], cs)
  suffixes :: [GClause] -> [[GClause]]
  suffixes cs = case cs of
    [] -> []
    _ : rest -> cs : suffixes rest
  -- "let R be a semiring" for a set R with a hypothesis that R is a semiring
  paramHypos :: [GClause] -> [GHypo]
  paramHypos cs = case cs of
    GVarClause y _ : GPropClause q : rest | Just k <- nounKind y q ->
      GVarsHypo (GListIdent [y]) k : paramHypos rest
    GVarClause y k : rest -> GVarsHypo (GListIdent [y]) k : paramHypos rest
    GPropClause q : rest -> GPropHypo q : paramHypos rest
    [] -> []
  -- consecutive fields of the same kind together: "elements a and b of G"
  groupFields fs = case fs of
    (y, k) : rest ->
      let (same, others) = span ((== k) . snd) rest
      in GIdentsArgKind k (GListIdent (y : map fst same)) : groupFields others
    [] -> []

-- | The same variables of the same kinds. Not (==): the generated equality
-- of lists compares them with zip, so that [a, b] == [a, b, c].
sameArgKinds :: GListArgKind -> GListArgKind -> Bool
sameArgKinds (GListArgKind xs) (GListArgKind ys) =
  length xs == length ys && and (zipWith same xs ys)
 where
  same :: GArgKind -> GArgKind -> Bool
  same (GIdentsArgKind k (GListIdent is)) (GIdentsArgKind k' (GListIdent js)) =
    k == k' && length is == length js && and (zipWith (==) is js)
  same _ _ = False

andProps :: GProp -> GProp -> GProp
andProps p q = case (p, q) of
  (GAndProp (GListProp ps), GAndProp (GListProp qs)) -> GAndProp (GListProp (ps ++ qs))
  (GAndProp (GListProp ps), _) -> GAndProp (GListProp (ps ++ [q]))
  (_, GAndProp (GListProp qs)) -> GAndProp (GListProp (p : qs))
  _ -> GAndProp (GListProp [p, q])

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


-- | For -roundtrip-safe, a variation is kept if it keeps the number of
-- hypotheses (moving them into the conclusion changes the arguments of the
-- constant in Dedukti, F17) and makes no "\\dots" sum, which has no
-- semantics (F13).
restorableVariation :: Tree a -> Tree a -> Bool
restorableVariation orig var = hypoCount orig == hypoCount var && not (hasSum3dots var)
 where
  hypoCount :: Tree a -> Int
  hypoCount u = case u of
    GAxiomJmt _ (GListHypo hs) _ -> length hs
    _ -> -1
  hasSum3dots :: Tree a -> Bool
  hasSum3dots u = case u of
    Gsum3dots_Term _ _ _ -> True
    _ -> getAny (composOpMonoid hasSum3dots' u)
  hasSum3dots' :: Tree a -> Any
  hasSum3dots' = Any . hasSum3dots

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
  -- a conjunction of hypotheses stays one with "and", not "both A and B",
  -- which the semantics reads as one hypothesis (semHypo)
  GPropHypo (GAndProp (GListProp ps)) ->
    [GPropHypo (GAndProp (GListProp ps')) | ps' <- mapM variations ps]
  GSuchThatHypo xs (GAndProp (GListProp ps)) kind ->
    [GSuchThatHypo xs (GAndProp (GListProp ps')) kind' | ps' <- mapM variations ps, kind' <- variations kind]
  GVarsHypo (GListIdent xs) (GExpKind (GTermExp term)) ->
    [GLetDeclarationHypo (GElemDeclaration (GListTerm [GIdentTerm x | x <- xs]) term), tree]
  -- "let f be a function from A to B" or "let $f \\in A \\rightarrow B$"
  GVarsHypo (GListIdent xs) (GFunExpKind (GTermExp a) (GTermExp b)) ->
    [tree, GLetDeclarationHypo (GElemDeclaration (GListTerm [GIdentTerm x | x <- xs]) (GFunTypeTerm a b))]
  GFunExpKind (GTermExp a) (GTermExp b) ->
    tree : [GExpKind (GTermExp (GFunTypeTerm a b))]
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

-- | collectivize for -roundtrip-safe: only where the semantics gives the
-- same tree back: "x, y and z are equivalent" for x ~ y ∧ y ~ z (a chain, as
-- the semantics reads it), "the sum of a, b and c" for (a + b) + c (nested
-- to the left, as the semantics folds it).
collectivizeSafe :: Tree a -> [Tree a]
collectivizeSafe t = case [u | u <- collectivize t, undo u == t] of
  [] -> [t]
  us -> us
 where
  undo :: Tree a -> Tree a
  undo u = case u of
    GAdjECollProp adjc (GListExp es) | length es > 1 ->
      GAndProp (GListProp [GAdjEProp adjc x y | (x, y) <- zip es (tail es)])
    GFunCCollExp func (GListExp (e : es)) ->
      foldl (GFunCExp func) (undo e) (map undo es)
    _ -> composOp undo u

collectivize :: Tree a -> [Tree a]
collectivize t = case t of

  -- put together instances of an equivalence relation that have common elements
  GAndProp (GListProp props) -> maybe [t] return $ do
    (adjc, expss) <- commonRel props
    let nexps = GListExp (nub expss) --- removes repetitions of intermediate terms 
    return $ GAdjECollProp adjc nexps

  -- put together arguments of collective functions
  GFunCExp func x y -> do
    let args = collectArgs func [x, y]
    let margs = GListExp args
    if length args > 2
      then [GFunCCollExp func margs]
      else [GFunCExp func cx cy | cx <- collectivize x, cy <- collectivize y]

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


-- | "if A implies B, then C" for "if if A, then B, then C": in the antecedent
-- of an implication, on both sides of an equivalence and in a hypothesis
-- ("assume that A implies B"), an implication between two propositions that
-- can be read as formulas: formulas, propositional variables, and what the
-- connectives make of them ($\neg A$, $A \Rightarrow B$); also in a
-- conjunction or disjunction there if all of its parts are such implications,
-- as "P or Q implies R" would be read as (P or Q) implies R
implied :: Tree a -> Tree a
implied t = case t of
  GIfProp a b -> GIfProp (antecedent (implied a)) (implied b)
  GIffProp a b -> GIffProp (antecedent (implied a)) (antecedent (implied b))
  -- what is assumed reads as an antecedent: "assume that A implies B"
  GPropHypo p -> GPropHypo (assumed (implied p))
  GPropVarHypo x p -> GPropVarHypo x (assumed (implied p))
  GSuchThatHypo xs p k -> GSuchThatHypo xs (assumed (implied p)) k
  _ -> composOp implied t
 where
   antecedent :: GProp -> GProp
   antecedent p = maybe p id (implication p)
   -- the hypotheses merged into one are a conjunction, each part assumed:
   -- "assume that A implies B and A"
   assumed :: GProp -> GProp
   assumed p = case p of
     GAndProp (GListProp ps) -> GAndProp (GListProp (map antecedent ps))
     _ -> antecedent p
   implication :: GProp -> Maybe GProp
   implication p = case p of
     GIfProp a b -> GFormulaImpliesProp <$> asFormula a <*> asFormula b
     GAndProp (GListProp ps) -> GAndProp . GListProp <$> mapM implication ps
     GOrProp (GListProp ps) -> GOrProp . GListProp <$> mapM implication ps
     _ -> Nothing
   -- a proposition built from formulas and atoms by the connectives, as a
   -- formula: "if $\neg A$ implies $B$", "if $A \Rightarrow B$ implies $A$"
   asFormula :: GProp -> Maybe GFormula
   asFormula p = case p of
     GFormulaProp f -> Just f
     GIdentProp (GStrIdent s) -> Just (GMacroFormula (GStringMacro s))
     GFalseProp -> Just (GMacroFormula (GStringMacro (GString "\\bot")))
     GCoreNotProp a -> GNotFormula <$> asFormula a
     GIfProp a b -> GIfFormula <$> asFormula a <*> asFormula b
     GFormulaImpliesProp a b -> Just (GIfFormula a b)
     GIffProp a b -> GIffFormula <$> asFormula a <*> asFormula b
     GAndProp (GListProp ps) -> foldl1 GAndFormula <$> mapM asFormula ps
     GOrProp (GListProp ps) -> foldl1 GOrFormula <$> mapM asFormula ps
     _ -> Nothing

-- | The scope of the connectives made explicit, where the plain lists of
-- conjuncts and disjuncts would be ambiguous: a conjunction or disjunction of
-- two inside the other one, or negated, is "both A and B", "either A or B"
-- ("P or both Q and R", "it is not the case that either A or B"); and so is
-- one whose first part is a negation ("either it is not the case that A or
-- B", not to be read as the negation of "A or B"). A conjunction in a
-- conjunction (a disjunction in a disjunction) is part of its list.
scoped :: Tree a -> Tree a
scoped t = case t of
  GAndProp (GListProp ps) -> conj GBothAndProp GAndProp (map inner (concatMap ands (map scoped ps)))
  GOrProp (GListProp ps) -> conj GEitherOrProp GOrProp (map inner (concatMap ors (map scoped ps)))
  GCoreNotProp p -> GCoreNotProp (inner (scoped p))
  _ -> composOp scoped t
 where
   conj :: (GProp -> GProp -> GProp) -> (GListProp -> GProp) -> [GProp] -> GProp
   conj both list ps = case ps of
     [a@(GCoreNotProp _), b] -> both a b
     _ -> list (GListProp ps)
   ands, ors :: GProp -> [GProp]
   ands p = case p of
     GAndProp (GListProp qs) -> qs
     _ -> [p]
   ors p = case p of
     GOrProp (GListProp qs) -> qs
     _ -> [p]
   inner :: GProp -> GProp
   inner p = case p of
     GAndProp (GListProp [a, b]) -> GBothAndProp a b
     GOrProp (GListProp [a, b]) -> GEitherOrProp a b
     _ -> p

---- TODO: move more of negation from earlier stages here
negated :: Tree a -> Tree a
negated t = case t of
  GCoreNotProp (GAdjProp adj x) -> GNotAdjProp adj x
  GCoreNotProp (GAdj2Prop adj x y) -> GNotAdj2Prop adj x y
  GCoreNotProp (GAdjCProp adj x y) -> GNotAdjCProp adj (GListExp [x, y])
  GCoreNotProp (GAdjEProp adj x y) -> GNotAdjEProp adj (GListExp [x, y])
  GCoreNotProp (GNoun1Prop adj x) -> GNotNoun1Prop adj x
  GCoreNotProp (GNoun2Prop adj x y) -> GNotNoun2Prop adj x y
  GCoreNotProp (GNoun3Prop noun x y z) -> GNotNoun3Prop noun x y z
  GCoreNotProp (GVerbProp adj x) -> GNotVerbProp adj x
  GCoreNotProp (GVerb2Prop adj x y) -> GNotVerb2Prop adj x y
  GCoreNotProp (GAdvProp adv x) -> GNotAdvProp adv x
  GCoreNotProp (GAdv2Prop adv x y) -> GNotAdv2Prop adv x y
  GCoreNotProp (GAdvCProp adv x y) -> GNotAdvCProp adv (GListExp [x, y])
  _ -> composOp negated t

