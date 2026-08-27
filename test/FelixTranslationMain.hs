{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (unless)
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
  checkAxiomRegression (translatedPresentations translation)
  checkBlockPolicy

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

checkAxiomRegression :: [GPresentationJmt] -> IO ()
checkAxiomRegression presentations = case presentations of
  [ GFormalPresentationJmt
      (GAxiomJmt labelForall (GListHypo forallHypos)
      (GCoreAllProp forallKindX forallX
        (GCoreAllProp forallKindY forallY
          (GCoreAndProp equalityX equalityY))))
    , GFormalPresentationJmt
      (GAxiomJmt labelExists (GListHypo existsHypos)
      (GCoreExistProp existsKindX existsX
        (GCoreExistProp existsKindY existsY equalityXY)))
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
  _ -> fail "the Felix axiom fixture did not retain its two ordered presentations"

checkBlockPolicy :: IO ()
checkBlockPolicy = do
  basic <- translateOrFail
    [claimBlock "basic_claim" (equalityStatement "A" "A")]
  case translatedPresentations basic of
    [GClaimPresentationJmt label (GListHypo []) proposition] -> do
      assertLabel "basic_claim" label
      assertGfEqual "a basic claim retains its equality"
        (equalityProp "A" "A") proposition
    _ -> fail "a basic Felix claim did not produce one presentation claim"
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
  case translatedPresentations defined of
    [GClaimPresentationJmt _ _ proposition] ->
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
  case translatedPresentations sided of
    [ GClaimPresentationJmt _ _ leftProposition
      , GClaimPresentationJmt _ _ rightProposition
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

translateOrFail :: [Raw.Block] -> IO Translation
translateOrFail = either fail pure . translateBlocks

claimBlock :: String -> Raw.Stmt -> Raw.Block
claimBlock label statement =
  Raw.BlockClaim Raw.Proposition Nowhere Nothing (rawMarker label)
    (Raw.Claim [] statement)

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
  Raw.BlockAbbr Nowhere Nothing (rawMarker label)
    (Raw.AbbreviationAdj (rawVariable "A")
      (Raw.Adj Nowhere (userAdjective side key) [])
      (equalityStatement "A" "A"))

userAdjective
  :: Raw.AdjectiveSide -> Raw.AdjectiveSurfaceKey -> Raw.AdjectiveLexicalItem
userAdjective side = Raw.AdjectiveLexicalItem side . Raw.UserAdjectiveIdentity

adjectiveKey :: String -> Raw.AdjectiveSurfaceKey
adjectiveKey word = Raw.AdjectiveSurfaceKey
  (Raw.TokenCons (Raw.Word (Text.pack word)) Raw.End)

equalityStatement :: String -> String -> Raw.Stmt
equalityStatement left right = Raw.StmtFormula (Raw.FormulaChain
  (Raw.ChainBase
    (rawExpression left :| []) Raw.Positive rawEquality
    (rawExpression right :| [])))

rawEquality :: Raw.Relation
rawEquality = Raw.Relation Nowhere Raw.EqSymbol []

rawTerm :: String -> Raw.Term
rawTerm = Raw.TermExpr . rawExpression

rawExpression :: String -> Raw.Expr
rawExpression = Raw.ExprVar . rawVariable

rawVariable :: String -> Raw.VarSymbol
rawVariable = Raw.NamedVarAt Nowhere . Text.pack

rawMarker :: String -> Raw.Marker
rawMarker = Raw.Marker . Text.pack

unsupportedSignatureBlock :: Raw.Block
unsupportedSignatureBlock =
  Raw.BlockSig Nowhere Nothing (rawMarker "unsupported_signature") []
    (Raw.SignatureTypedConstant Nowhere
      (Raw.Word (Text.pack "unsupported")) Raw.ConcreteSet)

equalityProp :: String -> String -> GProp
equalityProp left right = GAdj2Prop (LexAdj2 "Eq_Adj2")
  (variableExpression left) (variableExpression right)

variableExpression :: String -> GExp
variableExpression = GTermExp . GIdentTerm . GStrIdent . GString

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
