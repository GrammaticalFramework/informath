{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Felix.Report.Location (pattern Nowhere)
import qualified Felix.Syntax.Abstract as Raw
import Felix2Informath
import qualified Felix.Workspace as Felix
import Informath
import Paths_informath (getDataFileName)
import System.Directory (withCurrentDirectory)
import System.Environment (getArgs, lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  arguments <- getArgs
  unless (null arguments)
    (fail "usage: felix-translation")
  fixture <- getDataFileName "test/felix-statements.tex"
  let fixtureDirectory = takeDirectory fixture
      fixtureProjectDirectory = takeDirectory fixtureDirectory
  parsed <- withNaprapocheLibrary fixtureDirectory
    (withCurrentDirectory fixtureProjectDirectory
      (Felix.parseWorkspace fixture))
  blocks <- either
    (fail . Text.unpack . Felix.renderAuthorityFreeParseError)
    pure
    parsed
  translation <- either fail pure (translateBlocks blocks)
  checkParsedFixture translation
  checkBlockPolicy
  checkAssumptions
  checkConnectivesAndQuantifiers
  checkRelationsAndChains
  checkNaturalVocabulary
  checkExpressions
  checkSingleVisitExpressionContexts

withNaprapocheLibrary :: FilePath -> IO a -> IO a
withNaprapocheLibrary library =
  bracket replace restore . const
 where
  replace = do
    previous <- lookupEnv "NAPROCHE_LIB"
    setEnv "NAPROCHE_LIB" library
    pure previous
  restore previous = case previous of
    Nothing -> unsetEnv "NAPROCHE_LIB"
    Just value -> setEnv "NAPROCHE_LIB" value

checkParsedFixture :: Translation -> IO ()
checkParsedFixture translation = do
  case translatedJudgements translation of
    [ axiomForall
      , axiomExists
      , GAxiomJmt claimLabel (GListHypo claimHypotheses)
          claimConclusion
      ] -> do
        checkAxiomRegression [axiomForall, axiomExists]
        assertLabel "felix_informath_claim" claimLabel
        case claimHypotheses of
          [ GVarsHypo (GListIdent [setIdentifier]) setKind'
            , GSupposePropHypo
                (GAppProp fallbackIdentifier (GOneExps fallbackSubject))
            ] -> do
              assertIdent "A" setIdentifier
              assertSetKind setKind'
              assertIdent "fixture_regular" fallbackIdentifier
              assertVariableExpression "A" fallbackSubject
          _ -> fail "the parsed claim did not retain its ordered hypotheses"
        assertEquality "A" "A" claimConclusion
    _ -> fail "the fixture did not produce two axioms followed by one claim"
  let summary = translationSummary translation
  assert "the fixture records two axioms" (emittedAxiomCount summary == 2)
  assert "the fixture records its claim kind"
    (emittedClaimCounts summary == Map.singleton Raw.Proposition 1)
  assert "the fixture records its abbreviation"
    (readAbbreviationCount summary == 1)
  assert "the fixture has no definitions" (readDefinitionCount summary == 0)
  assert "the fixture records its omitted proof" (omittedProofCount summary == 1)
  assert "the fixture records its symbolic fallback"
    (symbolicFallbackCounts summary
      == Map.singleton (rawMarker "fixture_regular") 1)

checkAxiomRegression :: [GJmt] -> IO ()
checkAxiomRegression judgements = case judgements of
  [ GAxiomJmt labelForall (GListHypo forallHypos)
      (GCoreAllProp forallKindX forallX
        (GCoreAllProp forallKindY forallY
          (GCoreAndProp equalityX equalityY)))
    , GAxiomJmt labelExists (GListHypo existsHypos)
      (GCoreExistProp existsKindX existsX
        (GCoreExistProp existsKindY existsY equalityXY))
    ] -> do
      assert "the universal axiom has no hypotheses" (null forallHypos)
      assert "the existential axiom has no hypotheses" (null existsHypos)
      assertLabel "felix_informath_forall" labelForall
      assertLabel "felix_informath_exists" labelExists
      mapM_ assertSetKind [forallKindX, forallKindY, existsKindX, existsKindY]
      assertIdent "x" forallX
      assertIdent "y" forallY
      assertIdent "x" existsX
      assertIdent "y" existsY
      assertEquality "x" "x" equalityX
      assertEquality "y" "y" equalityY
      assertEquality "x" "y" equalityXY
  _ -> fail "the Felix axiom fixture did not retain its two ordered judgements"

checkBlockPolicy :: IO ()
checkBlockPolicy = do
  basic <- translateOrFail
    [claimBlock "basic_claim" (equalityStatement "A" "A")]
  case translatedJudgements basic of
    [GAxiomJmt label (GListHypo []) proposition] -> do
      assertLabel "basic_claim" label
      assertGfEqual "a basic claim retains its equality"
        (equalityProp "A" "A") proposition
    _ -> fail "a basic Felix claim did not produce one judgement"
  assert "the basic claim is counted by kind"
    (emittedClaimCounts (translationSummary basic)
      == Map.singleton Raw.Proposition 1)

  let key = adjectiveKey "audited"
      leftDeclaration = adjectiveAbbreviation
        "left_audited" Raw.LeftAdjectiveSide key
      rightDeclaration = adjectiveAbbreviation
        "right_audited" Raw.RightAdjectiveSide key
  defined <- translateOrFail
    [ adjectiveDefinition "defined_audited" Raw.LeftAdjectiveSide key
    , adjectiveClaim "defined_use" Raw.LeftAdjectiveSide key
    ]
  case translatedJudgements defined of
    [GAxiomJmt _ _ proposition] ->
      assertApplicationMarker "defined_audited" proposition
    _ -> fail "an adjective definition did not resolve in a later claim"
  assert "the adjective definition is counted"
    (readDefinitionCount (translationSummary defined) == 1)

  idempotent <- translateOrFail [leftDeclaration, leftDeclaration]
  assert "idempotent adjective declarations are both read"
    (readAbbreviationCount (translationSummary idempotent) == 2)
  assertLeftContains "a same-side adjective conflict is rejected"
    "Felix abbreviation other_marker: conflicting adjective declaration"
    (translateBlocks
      [leftDeclaration
      , adjectiveAbbreviation "other_marker" Raw.LeftAdjectiveSide key
      ])
  assertLeftContains "a forward adjective use is rejected"
    "Felix claim forward_use: unsupported Felix unresolved user adjective"
    (translateBlocks
      [ adjectiveClaim "forward_use" Raw.LeftAdjectiveSide key
      , leftDeclaration
      ])

  sided <- translateOrFail
    [ leftDeclaration
    , rightDeclaration
    , adjectiveClaim "left_use" Raw.LeftAdjectiveSide key
    , adjectiveClaim "right_use" Raw.RightAdjectiveSide key
    ]
  case translatedJudgements sided of
    [ GAxiomJmt _ _ leftProposition
      , GAxiomJmt _ _ rightProposition
      ] -> do
        assertApplicationMarker "left_audited" leftProposition
        assertApplicationMarker "right_audited" rightProposition
    _ -> fail "opposite-side adjective declarations did not produce two claims"
  assert "opposite-side adjective declarations resolve independently"
    (symbolicFallbackCounts (translationSummary sided) == Map.fromList
      [(rawMarker "left_audited", 1), (rawMarker "right_audited", 1)])

  assertLeftContains "unsupported blocks retain their class and marker"
    "Felix signature unsupported_signature: unsupported top-level block"
    (translateBlocks [unsupportedSignatureBlock])

checkAssumptions :: IO ()
checkAssumptions = do
  let assumptions =
        [ Raw.AsmLetNoun (rawVariable "A" :| []) (nounPhrase "set" [])
        , Raw.AsmLetIn (rawVariable "x" :| []) (rawExpression "A")
        , Raw.AsmLetRelation
            (rawVariable "y" :| [rawVariable "z"])
            (rawRelation "subseteq")
            (rawExpression "A")
        , Raw.AsmSuppose (equalityStatement "A" "A")
        ]
      expected = GListHypo
        [ GVarsHypo (GListIdent [gIdent "A"]) setKind
        , GVarsHypo (GListIdent [gIdent "x"])
            (GExpKind (gExpression "A"))
        , GVarsHypo (GListIdent [gIdent "y", gIdent "z"]) setKind
        , GPropHypo (subseteqProp (gExpression "y") (gExpression "A"))
        , GPropHypo (subseteqProp (gExpression "z") (gExpression "A"))
        , GSupposePropHypo (equalityProp "A" "A")
        ]
  translation <- translateOrFail
    [claimBlockWith "assumptions" assumptions (equalityStatement "x" "x")]
  case translatedJudgements translation of
    [GAxiomJmt _ actual@(GListHypo hypotheses) _] -> do
      assert "all assumption forms retain their expanded order"
        (length hypotheses == 6)
      assertGfEqual "assumption translation" expected actual
    _ -> fail "the assumption test did not produce one judgement"

checkConnectivesAndQuantifiers :: IO ()
checkConnectivesAndQuantifiers = do
  let left = equalityStatement "A" "B"
      right = equalityStatement "B" "C"
      leftProp = equalityProp "A" "B"
      rightProp = equalityProp "B" "C"
      cases =
        [ (Raw.Conjunction, GCoreAndProp leftProp rightProp)
        , (Raw.Disjunction, GCoreOrProp leftProp rightProp)
        , (Raw.Implication, GCoreIfProp leftProp rightProp)
        , (Raw.Equivalence, GCoreIffProp leftProp rightProp)
        , (Raw.ExclusiveOr,
            GCoreAndProp
              (GCoreOrProp leftProp rightProp)
              (GCoreNotProp (GCoreAndProp leftProp rightProp)))
        ]
  forM_ cases $ \(connective, expected) -> do
    actual <- translateProposition
      (Raw.StmtConnected connective Nothing left right)
    assertGfEqual ("statement connective " ++ show connective) expected actual

  let formulaLeft = equalityFormula "A" "B"
      formulaRight = equalityFormula "B" "C"
  forM_ cases $ \(connective, expected) -> do
    actual <- translateProposition
      (Raw.StmtFormula
        (Raw.Connected Nowhere connective formulaLeft formulaRight))
    assertGfEqual ("formula connective " ++ show connective) expected actual

  let suchThat = equalityStatement "Z" "Z"
      body = equalityStatement "x" "y"
      quantified = Raw.SymbolicQuantified
        Nowhere Raw.Universally
        (rawVariable "x" :| [rawVariable "y"])
        (Raw.Bounded Nowhere Raw.Positive
          (rawRelation "elem") (rawExpression "A"))
        (Just suchThat)
        body
      guards = GCoreAndProp
        (GCoreAndProp
          (elementProp (gExpression "x") (gExpression "A"))
          (elementProp (gExpression "y") (gExpression "A")))
        (equalityProp "Z" "Z")
      expected = GCoreAllProp setKind (gIdent "x")
        (GCoreAllProp setKind (gIdent "y")
          (GCoreIfProp guards (equalityProp "x" "y")))
  actualQuantified <- translateProposition quantified
  assertGfEqual "bounded universal guards and binders" expected actualQuantified

  let typedBody = equalityFormula "x" "x"
      typedCases =
        [ (Raw.ConcreteSet, setKind)
        , ( Raw.ConcreteArrow Raw.ConcreteSet Raw.ConcreteSet
          , GFunKind (GListArgKind [GKindArgKind setKind]) setKind
          )
        ]
  forM_ typedCases $ \(concreteType, expectedKind) -> do
    actual <- translateProposition . Raw.StmtFormula $
      Raw.FormulaTypedQuantified Nowhere Raw.Universally
        (rawVariable "x" :| []) concreteType typedBody
    assertGfEqual ("typed quantifier " ++ show concreteType)
      (GCoreAllProp expectedKind (gIdent "x") (equalityProp "x" "x")) actual

checkRelationsAndChains :: IO ()
checkRelationsAndChains = do
  let relationCases =
        [ ("eq", equalityG (gExpression "x") (gExpression "y"))
        , ("neq", GAdj2Prop (LexAdj2 "Neq_Adj2")
            (gExpression "x") (gExpression "y"))
        , ("elem", elementProp (gExpression "x") (gExpression "y"))
        , ("notelem", GNoun2Prop (LexNoun2 "notelement_Noun2")
            (gExpression "x") (gExpression "y"))
        , ("subset", GNoun2Prop (LexNoun2 "subset_Noun2")
            (gExpression "x") (gExpression "y"))
        , ("subseteq", subseteqProp (gExpression "x") (gExpression "y"))
        , ("supseteq", GNoun2Prop (LexNoun2 "superseteq_Noun2")
            (gExpression "x") (gExpression "y"))
        ]
  forM_ relationCases $ \(marker, expected) -> do
    actual <- translateProposition (relationStatement marker Raw.Positive
      (rawExpression "x" :| []) (rawExpression "y" :| []))
    assertGfEqual ("relation " ++ marker) expected actual

  negative <- translateProposition (relationStatement "elem" Raw.Negative
    (rawExpression "x" :| [rawExpression "y"])
    (rawExpression "A" :| [rawExpression "B"]))
  assertGfEqual "negative relations negate every cross-product atom"
    (conjoinList
      [ GCoreNotProp (elementProp (gExpression "x") (gExpression "A"))
      , GCoreNotProp (elementProp (gExpression "x") (gExpression "B"))
      , GCoreNotProp (elementProp (gExpression "y") (gExpression "A"))
      , GCoreNotProp (elementProp (gExpression "y") (gExpression "B"))
      ]) negative

  let chain = Raw.ChainCons
        (rawExpression "x" :| []) Raw.Positive (rawRelation "eq")
        (Raw.ChainBase
          (rawExpression "m" :| []) Raw.Positive (rawRelation "elem")
          (rawExpression "z" :| []))
      expected = GCoreAndProp
        (equalityG (gExpression "x") (gExpression "m"))
        (elementProp (gExpression "m") (gExpression "z"))
  actualChain <- translateProposition
    (Raw.StmtFormula (Raw.FormulaChain chain))
  assertGfEqual "a relation chain reuses its middle group" expected actualChain

checkNaturalVocabulary :: IO ()
checkNaturalVocabulary = do
  let inhabitedKey = adjectiveKey "inhabited-surface"
      emptyKey = adjectiveKey "empty-surface"
      disjointKey = adjectiveKey "disjoint-surface"
      genericKey = adjectiveKey "generic-surface"
      declaration label key arguments =
        adjectiveAbbreviationWithArguments label Raw.LeftAdjectiveSide key arguments
      declarations =
        [ declaration "inhabited" inhabitedKey []
        , declaration "empty" emptyKey []
        , declaration "disjoint" disjointKey [rawVariable "B"]
        , declaration "generic_predicate" genericKey []
        ]
      assertions =
        [ ( "inhabited_claim"
          , adjectiveStatement (rawTerm "A" :| []) inhabitedKey []
          , GAdjProp (LexAdj "inhabited_Adj") (gExpression "A")
          )
        , ( "empty_claim"
          , adjectiveStatement (rawTerm "A" :| []) emptyKey []
          , GAdjProp (LexAdj "empty_Adj") (gExpression "A")
          )
        , ( "disjoint_claim"
          , adjectiveStatement (rawTerm "A" :| []) disjointKey [rawTerm "B"]
          , GAdjCProp (LexAdjC "disjoint_AdjC")
              (gExpression "A") (gExpression "B")
          )
        ]
  forM_ assertions $ \(label, statement, expected) -> do
    translation <- translateOrFail (declarations ++ [claimBlock label statement])
    assertGfEqual label expected (onlyClaimProposition translation)

  generic <- translateOrFail
    (declarations ++
      [claimBlock "generic_claim"
        (adjectiveStatement
          (rawTerm "A" :| [rawTerm "B"]) genericKey [])])
  assertGfEqual "a generic adjective maps over multiple subjects"
    (GCoreAndProp
      (GAppProp (gIdent "generic_predicate") (GOneExps (gExpression "A")))
      (GAppProp (gIdent "generic_predicate") (GOneExps (gExpression "B"))))
    (onlyClaimProposition generic)
  assert "one generic adjective node records one fallback"
    (symbolicFallbackCounts (translationSummary generic)
      == Map.singleton (rawMarker "generic_predicate") 1)

  nounAssertion <- translateProposition
    (Raw.StmtNoun (rawTerm "A" :| []) (nounPhrase "subsingleton" []))
  assertGfEqual "subsingleton noun assertion"
    (GNoun1Prop (GNounNoun1 (LexNoun "subsingleton_Noun"))
      (gExpression "A"))
    nounAssertion

  family <- translateOrFail
    [claimBlockWith "family_kind"
      [Raw.AsmLetNoun (rawVariable "F" :| [])
        (nounPhrase "family_of_subsets" [rawTerm "X"])]
      (equalityStatement "F" "F")]
  case translatedJudgements family of
    [GAxiomJmt _
      (GListHypo [GVarsHypo (GListIdent [identifier]) kind]) _] -> do
        assertIdent "F" identifier
        assertGfEqual "family_of_subsets kind"
          (GDepKind (LexDep "family_of_subsets_Dep") (gExpression "X")) kind
    _ -> fail "the family-of-subsets assumption did not produce one hypothesis"

checkExpressions :: IO ()
checkExpressions = do
  let a = rawExpression "A"
      b = rawExpression "B"
      targetCases =
        [ ("empty set", rawOperation "emptyset" [],
            GNameExp (LexName "emptyset_Name"))
        , ("union", rawOperation "union" [a, b],
            GFunCExp (LexFunC "union_FunC")
              (gExpression "A") (gExpression "B"))
        , ("intersection", rawOperation "inter" [a, b],
            GFunCExp (LexFunC "intersection_FunC")
              (gExpression "A") (gExpression "B"))
        , ("difference", rawOperation "setminus" [a, b],
            GFun2Exp (LexFun2 "difference_Fun2")
              (gExpression "A") (gExpression "B"))
        , ("cartesian product", rawOperation "times" [a, b],
            GFunCExp (LexFunC "cartesian_FunC")
              (gExpression "A") (gExpression "B"))
        , ("pair", rawOperation "pair" [a, b],
            GTermExp (GTupleTerm (GListTerm
              [GIdentTerm (gIdent "A"), GIdentTerm (gIdent "B")])))
        , ("first projection", rawOperation "fst" [a],
            GFunExp
              (GNounPrepFun
                (GAdjNounNoun
                  (LexAdj "first_Adj") (LexNoun "projection_Noun"))
                (LexPrep "of_Prep"))
              (gExpression "A"))
        , ("second projection", rawOperation "snd" [a],
            GFunExp
              (GNounPrepFun
                (GAdjNounNoun
                  (LexAdj "second_Adj") (LexNoun "projection_Noun"))
                (LexPrep "of_Prep"))
              (gExpression "A"))
        , ("finite set", Raw.ExprFiniteSet Nowhere (a :| [b]),
            GEnumSetExp
              (GManyExps (GListExp [gExpression "A", gExpression "B"])))
        , ("surface application", Raw.ExprOp Nowhere Raw.ApplySymbol [a, b],
            GAppExp (gExpression "A") (GOneExps (gExpression "B")))
        , ("higher application",
            Raw.ExprHigherApply Nowhere
              (Raw.TypedExpressionExpr a) (Raw.TypedExpressionExpr b),
            GAppExp (gExpression "A") (GOneExps (gExpression "B")))
        ]
  forM_ targetCases $ \(name, source, expected) -> do
    actual <- translateExpressionInClaim name source
    assertGfEqual name expected actual

  unary <- translateOrFail
    [expressionClaim "generic_unary" (rawOperation "mystery" [a])]
  assertGfEqual "unary symbolic application"
    (genericApplication "mystery" [gExpression "A"])
    (leftSideOfEquality unary)
  assert "unary fallback count"
    (symbolicFallbackCounts (translationSummary unary)
      == Map.singleton (rawMarker "mystery") 1)

  binary <- translateOrFail
    [expressionClaim "generic_binary" (rawOperation "mystery" [a, b])]
  assertGfEqual "binary symbolic application"
    (genericApplication "mystery" [gExpression "A", gExpression "B"])
    (leftSideOfEquality binary)

  assertLeftContains "known operators reserve their arity"
    "Felix claim bad_union: unsupported Felix operator union at the wrong arity"
    (translateBlocks [expressionClaim "bad_union" (rawOperation "union" [a])])
  assertLeftContains "unknown nullary operators are rejected"
    "Felix claim bad_nullary: unsupported Felix unknown nullary operator mystery"
    (translateBlocks
      [expressionClaim "bad_nullary" (rawOperation "mystery" [])])

checkSingleVisitExpressionContexts :: IO ()
checkSingleVisitExpressionContexts = do
  relationAssumption <- translateOrFail
    [claimBlockWith "relation_assumption"
      [Raw.AsmLetRelation
        (rawVariable "x" :| [rawVariable "y"])
        (rawRelation "subseteq")
        (rawOperation "scope" [rawExpression "A"])]
      (equalityStatement "x" "y")]
  assert "a relation-assumption target is translated once"
    (symbolicFallbackCounts (translationSummary relationAssumption)
      == Map.singleton (rawMarker "scope") 1)

  quantified <- translateOrFail
    [claimBlock "bounded"
      (Raw.SymbolicQuantified Nowhere Raw.Universally
        (rawVariable "x" :| [rawVariable "y"])
        (Raw.Bounded Nowhere Raw.Positive (rawRelation "elem")
          (rawOperation "bound_scope" [rawExpression "A"]))
        Nothing
        (equalityStatement "x" "y"))]
  assert "a multi-variable bound is translated once"
    (symbolicFallbackCounts (translationSummary quantified)
      == Map.singleton (rawMarker "bound_scope") 1)

  let chain = Raw.ChainCons
        (rawExpression "x" :| []) Raw.Positive (rawRelation "eq")
        (Raw.ChainBase
          (rawOperation "middle" [rawExpression "A"] :| [])
          Raw.Positive (rawRelation "elem")
          (rawExpression "z" :| []))
  chained <- translateOrFail
    [claimBlock "shared_middle"
      (Raw.StmtFormula (Raw.FormulaChain chain))]
  assert "a relation-chain middle is translated once"
    (symbolicFallbackCounts (translationSummary chained)
      == Map.singleton (rawMarker "middle") 1)

translateOrFail :: [Raw.Block] -> IO Translation
translateOrFail = either fail pure . translateBlocks

translateProposition :: Raw.Stmt -> IO GProp
translateProposition statement =
  onlyClaimProposition <$> translateOrFail [claimBlock "test_claim" statement]

translateExpressionInClaim :: String -> Raw.Expr -> IO GExp
translateExpressionInClaim label expression =
  leftSideOfEquality <$> translateOrFail [expressionClaim label expression]

onlyClaimProposition :: Translation -> GProp
onlyClaimProposition translation = case translatedJudgements translation of
  [GAxiomJmt _ _ proposition] -> proposition
  _ -> error "test invariant: expected one translated claim"

leftSideOfEquality :: Translation -> GExp
leftSideOfEquality translation = case onlyClaimProposition translation of
  GAdj2Prop (LexAdj2 "Eq_Adj2") left _right -> left
  _ -> error "test invariant: expected an equality proposition"

claimBlock :: String -> Raw.Stmt -> Raw.Block
claimBlock label = claimBlockWith label []

claimBlockWith :: String -> [Raw.Asm] -> Raw.Stmt -> Raw.Block
claimBlockWith label assumptions statement =
  Raw.BlockClaim Raw.Proposition Nowhere Nothing (rawMarker label)
    (Raw.Claim assumptions statement)

expressionClaim :: String -> Raw.Expr -> Raw.Block
expressionClaim label expression = claimBlock label
  (Raw.StmtFormula (Raw.FormulaChain
    (Raw.ChainBase (expression :| []) Raw.Positive (rawRelation "eq")
      (rawExpression "target" :| []))))

adjectiveClaim
  :: String -> Raw.AdjectiveSide -> Raw.AdjectiveSurfaceKey -> Raw.Block
adjectiveClaim label side key = claimBlock label
  (Raw.StmtVerbPhrase (rawTerm "A" :| [])
    (Raw.VPAdj (Raw.Adj Nowhere (userAdjective side key) [] :| [])))

adjectiveDefinition
  :: String -> Raw.AdjectiveSide -> Raw.AdjectiveSurfaceKey -> Raw.Block
adjectiveDefinition label side key =
  Raw.BlockDefn Nowhere Nothing (rawMarker label)
    (Raw.Defn []
      (Raw.DefnAdj (rawVariable "A")
        (Raw.Adj Nowhere (userAdjective side key) []))
      (equalityStatement "A" "A"))

adjectiveAbbreviation
  :: String -> Raw.AdjectiveSide -> Raw.AdjectiveSurfaceKey -> Raw.Block
adjectiveAbbreviation label side key =
  adjectiveAbbreviationWithArguments label side key []

adjectiveAbbreviationWithArguments
  :: String
  -> Raw.AdjectiveSide
  -> Raw.AdjectiveSurfaceKey
  -> [Raw.VarSymbol]
  -> Raw.Block
adjectiveAbbreviationWithArguments label side key arguments =
  Raw.BlockAbbr Nowhere Nothing (rawMarker label)
    (Raw.AbbreviationAdj (rawVariable "A")
      (Raw.Adj Nowhere (userAdjective side key) arguments)
      (equalityStatement "A" "A"))

adjectiveStatement
  :: NonEmpty Raw.Term
  -> Raw.AdjectiveSurfaceKey
  -> [Raw.Term]
  -> Raw.Stmt
adjectiveStatement subjects key arguments =
  Raw.StmtVerbPhrase subjects
    (Raw.VPAdj
      (Raw.Adj Nowhere
        (userAdjective Raw.LeftAdjectiveSide key) arguments :| []))

userAdjective
  :: Raw.AdjectiveSide -> Raw.AdjectiveSurfaceKey -> Raw.AdjectiveLexicalItem
userAdjective side = Raw.AdjectiveLexicalItem side . Raw.UserAdjectiveIdentity

adjectiveKey :: String -> Raw.AdjectiveSurfaceKey
adjectiveKey word = Raw.AdjectiveSurfaceKey
  (Raw.TokenCons (Raw.Word (Text.pack word)) Raw.End)

equalityStatement :: String -> String -> Raw.Stmt
equalityStatement left right = Raw.StmtFormula (equalityFormula left right)

equalityFormula :: String -> String -> Raw.Formula
equalityFormula left right = Raw.FormulaChain
  (Raw.ChainBase
    (rawExpression left :| []) Raw.Positive (rawRelation "eq")
    (rawExpression right :| []))

relationStatement
  :: String -> Raw.Sign -> NonEmpty Raw.Expr -> NonEmpty Raw.Expr -> Raw.Stmt
relationStatement marker sign left right = Raw.StmtFormula (Raw.FormulaChain
  (Raw.ChainBase left sign (rawRelation marker) right))

rawRelation :: String -> Raw.Relation
rawRelation marker = Raw.Relation Nowhere
  (Raw.RelationSymbol (Raw.Symbol (Text.pack marker))
    Raw.zeroParameterArity (rawMarker marker))
  []

rawOperation :: String -> [Raw.Expr] -> Raw.Expr
rawOperation marker = Raw.ExprOp Nowhere
  (Raw.MixfixItem Raw.End (rawMarker marker) Raw.NonAssoc)

rawTerm :: String -> Raw.Term
rawTerm = Raw.TermExpr . rawExpression

rawExpression :: String -> Raw.Expr
rawExpression = Raw.ExprVar . rawVariable

rawVariable :: String -> Raw.VarSymbol
rawVariable = Raw.NamedVarAt Nowhere . Text.pack

rawMarker :: String -> Raw.Marker
rawMarker = Raw.Marker . Text.pack

nounPhrase :: String -> [Raw.Term] -> Raw.NounPhrase Maybe
nounPhrase marker arguments =
  Raw.NounPhrase []
    (Raw.Noun Nowhere
      (Raw.LexicalItemSgPl (Raw.SgPl Raw.End Raw.End) (rawMarker marker))
      arguments)
    Nothing [] Nothing

unsupportedSignatureBlock :: Raw.Block
unsupportedSignatureBlock =
  Raw.BlockSig Nowhere Nothing (rawMarker "unsupported_signature") []
    (Raw.SignatureTypedConstant Nowhere
      (Raw.Word (Text.pack "unsupported")) Raw.ConcreteSet)

equalityProp :: String -> String -> GProp
equalityProp left right = equalityG
  (gExpression left) (gExpression right)

equalityG :: GExp -> GExp -> GProp
equalityG = GAdj2Prop (LexAdj2 "Eq_Adj2")

elementProp :: GExp -> GExp -> GProp
elementProp = GNoun2Prop (LexNoun2 "element_Noun2")

subseteqProp :: GExp -> GExp -> GProp
subseteqProp = GNoun2Prop (LexNoun2 "subseteq_Noun2")

gIdent :: String -> GIdent
gIdent = GStrIdent . GString

gExpression :: String -> GExp
gExpression = GTermExp . GIdentTerm . gIdent

genericApplication :: String -> [GExp] -> GExp
genericApplication marker arguments = case arguments of
  [argument] ->
    GAppExp (gExpression marker) (GOneExps argument)
  first : second : rest ->
    GAppExp (gExpression marker)
      (GManyExps (GListExp (first : second : rest)))
  [] -> error "test invariant: generic applications are nonempty"

setKind :: GKind
setKind = GNounKind (LexNoun "set_Noun")

variableExpression :: String -> GExp
variableExpression = gExpression

conjoinList :: [GProp] -> GProp
conjoinList = \case
  first : rest -> foldl GCoreAndProp first rest
  [] -> error "test invariant: conjunctions are nonempty"

assertApplicationMarker :: String -> GProp -> IO ()
assertApplicationMarker expected = \case
  GAppProp identifier (GOneExps subject) -> do
    assertIdent expected identifier
    assertVariableExpression "A" subject
  _ -> fail ("expected an application proposition for " ++ expected)

assertLeftContains :: String -> String -> Either String a -> IO ()
assertLeftContains message expected = \case
  Left actual -> assert
    (message ++ ": expected an error containing " ++ show expected
      ++ ", got " ++ show actual)
    (expected `isInfixOf` actual)
  Right _ -> fail (message ++ ": expected translation to fail")

assert :: String -> Bool -> IO ()
assert message condition = unless condition (fail message)

-- Generated Tree equality does not compare the lengths of list fields.
gfTreeEqual :: Gf a => a -> a -> Bool
gfTreeEqual expected actual = gf expected == gf actual

assertGfEqual :: Gf a => String -> a -> a -> IO ()
assertGfEqual message expected actual =
  assert message (gfTreeEqual expected actual)

assertLabel :: String -> GLabel -> IO ()
assertLabel expected label = case label of
  GIdentLabel identifier -> assertIdent expected identifier
  _ -> fail ("expected an identifier label for " ++ expected)

assertIdent :: String -> GIdent -> IO ()
assertIdent expected (GStrIdent (GString actual)) =
  assert ("expected identifier " ++ expected ++ ", got " ++ actual)
    (actual == expected)

assertSetKind :: GKind -> IO ()
assertSetKind kind = case kind of
  GNounKind (LexNoun name) ->
    assert ("expected set_Noun, got " ++ name) (name == "set_Noun")
  _ -> fail "expected a noun kind for a quantified set variable"

assertEquality :: String -> String -> GProp -> IO ()
assertEquality expectedLeft expectedRight proposition = case proposition of
  GAdj2Prop (LexAdj2 relation) left right -> do
    assert ("expected Eq_Adj2, got " ++ relation) (relation == "Eq_Adj2")
    assertVariableExpression expectedLeft left
    assertVariableExpression expectedRight right
  _ -> fail "expected a binary adjective equality proposition"

assertVariableExpression :: String -> GExp -> IO ()
assertVariableExpression expected expression = case expression of
  GTermExp (GIdentTerm identifier) -> assertIdent expected identifier
  _ -> fail ("expected the variable expression " ++ expected)
