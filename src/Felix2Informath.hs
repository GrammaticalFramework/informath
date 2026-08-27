{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Felix2Informath
  ( Translation (..)
  , TranslationSummary (..)
  , translateBlocks
  , renderTranslationSummary
  ) where

import Control.Monad (foldM)
import Control.Monad.Writer.Strict (WriterT (..), tell)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Felix.Syntax.Abstract as Raw
import Informath

data Translation = Translation
  { translatedPresentations :: [GPresentationJmt]
  , translationSummary :: TranslationSummary
  }

data TranslationSummary = TranslationSummary
  { emittedAxiomCount :: Int
  , emittedClaimCounts :: Map Raw.ClaimKind Int
  , readDefinitionCount :: Int
  , readAbbreviationCount :: Int
  , omittedProofCount :: Int
  , symbolicFallbackCounts :: Map Raw.Marker Int
  }
  deriving (Eq, Show)

type AdjectiveKey =
  (Raw.AdjectiveSide, Raw.AdjectiveSurfaceKey)

type AdjectiveEnvironment = Map AdjectiveKey Raw.Marker

type TranslateM = WriterT [Raw.Marker] (Either String)

data TranslationState = TranslationState
  { reversedPresentations :: [GPresentationJmt]
  , currentSummary :: TranslationSummary
  , adjectiveEnvironment :: AdjectiveEnvironment
  }

translateBlocks :: [Raw.Block] -> Either String Translation
translateBlocks blocks = do
  final <- foldM translateBlock initialTranslationState blocks
  pure Translation
    { translatedPresentations = reverse (reversedPresentations final)
    , translationSummary = currentSummary final
    }

renderTranslationSummary :: TranslationSummary -> String
renderTranslationSummary summary =
  "Felix: emitted " ++ show claimCount ++ " claims (" ++ claimDetails
    ++ ") and " ++ show (emittedAxiomCount summary) ++ " axioms; read "
    ++ show (readAbbreviationCount summary) ++ " abbreviations and "
    ++ show (readDefinitionCount summary) ++ " definitions; omitted "
    ++ show (omittedProofCount summary) ++ " proofs; symbolic fallbacks: "
    ++ fallbackDetails
 where
  claimCounts = Map.toAscList (emittedClaimCounts summary)
  claimCount = sum (map snd claimCounts)
  claimDetails = case claimCounts of
    [] -> "none"
    counts -> intercalate ", "
      [show kind ++ "=" ++ show count | (kind, count) <- counts]
  fallbackDetails = case Map.toAscList (symbolicFallbackCounts summary) of
    [] -> "none"
    counts -> intercalate ", "
      [markerText marker ++ "=" ++ show count | (marker, count) <- counts]

initialTranslationState :: TranslationState
initialTranslationState = TranslationState
  { reversedPresentations = []
  , currentSummary = emptyTranslationSummary
  , adjectiveEnvironment = Map.empty
  }

emptyTranslationSummary :: TranslationSummary
emptyTranslationSummary = TranslationSummary
  { emittedAxiomCount = 0
  , emittedClaimCounts = Map.empty
  , readDefinitionCount = 0
  , readAbbreviationCount = 0
  , omittedProofCount = 0
  , symbolicFallbackCounts = Map.empty
  }

translateBlock :: TranslationState -> Raw.Block -> Either String TranslationState
translateBlock state = \case
  Raw.BlockAxiom _location _title marker (Raw.Axiom assumptions statement) -> do
    (presentation, fallbacks) <- withBlockContext "axiom" marker
      (translateAxiom (adjectiveEnvironment state) marker assumptions statement)
    let recorded = recordPresentation presentation fallbacks state
        summary = currentSummary recorded
    pure recorded
      { currentSummary = summary
          { emittedAxiomCount = emittedAxiomCount summary + 1 }
      }
  Raw.BlockClaim kind _location _title marker (Raw.Claim assumptions statement) -> do
    (presentation, fallbacks) <- withBlockContext "claim" marker
      (translateClaim (adjectiveEnvironment state) marker assumptions statement)
    let recorded = recordPresentation presentation fallbacks state
        summary = currentSummary recorded
    pure recorded
      { currentSummary = summary
          { emittedClaimCounts = Map.insertWith (+) kind 1
              (emittedClaimCounts summary)
          }
      }
  Raw.BlockDefn _location _title marker definition -> do
    environment <- withContext "definition" marker
      (registerDefinition marker definition (adjectiveEnvironment state))
    let summary = currentSummary state
    pure state
      { currentSummary = summary
          { readDefinitionCount = readDefinitionCount summary + 1 }
      , adjectiveEnvironment = environment
      }
  Raw.BlockAbbr _location _title marker abbreviation -> do
    environment <- withContext "abbreviation" marker
      (registerAbbreviation marker abbreviation (adjectiveEnvironment state))
    let summary = currentSummary state
    pure state
      { currentSummary = summary
          { readAbbreviationCount = readAbbreviationCount summary + 1 }
      , adjectiveEnvironment = environment
      }
  Raw.BlockProof{} ->
    let summary = currentSummary state
    in pure state
      { currentSummary = summary
          { omittedProofCount = omittedProofCount summary + 1 }
      }
  Raw.BlockData _ _ marker _ -> unsupportedBlock "datatype" marker
  Raw.BlockInductive _ _ marker _ -> unsupportedBlock "inductive definition" marker
  Raw.BlockSig _ _ marker _ _ -> unsupportedBlock "signature" marker
  Raw.BlockStruct _ _ marker _ -> unsupportedBlock "structure" marker
  Raw.BlockInstance _ _ marker _ -> unsupportedBlock "instance" marker

recordPresentation
  :: GPresentationJmt -> [Raw.Marker] -> TranslationState -> TranslationState
recordPresentation presentation fallbacks state = state
  { reversedPresentations = presentation : reversedPresentations state
  , currentSummary = summary
      { symbolicFallbackCounts = foldr recordFallback
          (symbolicFallbackCounts summary) fallbacks
      }
  }
 where
  summary = currentSummary state
  recordFallback marker = Map.insertWith (+) marker 1

unsupportedBlock :: String -> Raw.Marker -> Either String a
unsupportedBlock constructor marker =
  Left ("Felix " ++ constructor ++ " " ++ markerText marker
    ++ ": unsupported top-level block")

withBlockContext
  :: String -> Raw.Marker -> TranslateM a -> Either String (a, [Raw.Marker])
withBlockContext constructor marker action =
  withContext constructor marker (runWriterT action)

withContext :: String -> Raw.Marker -> Either String a -> Either String a
withContext constructor marker = either
  (Left . (("Felix " ++ constructor ++ " " ++ markerText marker ++ ": ") ++))
  Right

registerDefinition
  :: Raw.Marker -> Raw.Defn -> AdjectiveEnvironment
  -> Either String AdjectiveEnvironment
registerDefinition marker definition environment = case definition of
  Raw.Defn _assumptions head' _statement ->
    registerDefinitionHead marker head' environment
  Raw.DefnFun{} -> Right environment
  Raw.DefnOp{} -> Right environment
  Raw.DefnTypedConstant{} -> Right environment

registerDefinitionHead
  :: Raw.Marker -> Raw.DefnHead -> AdjectiveEnvironment
  -> Either String AdjectiveEnvironment
registerDefinitionHead marker definitionHead environment =
  case definitionHead of
    Raw.DefnAdj _subject (Raw.Adj _location lexicalItem _arguments) ->
      registerAdjective marker lexicalItem environment
    Raw.DefnVerb{} -> Right environment
    Raw.DefnNoun{} -> Right environment
    Raw.DefnSymbolicPredicate{} -> Right environment
    Raw.DefnRel{} -> Right environment

registerAbbreviation
  :: Raw.Marker -> Raw.Abbreviation -> AdjectiveEnvironment
  -> Either String AdjectiveEnvironment
registerAbbreviation marker abbreviation environment = case abbreviation of
  Raw.AbbreviationAdj _subject (Raw.Adj _location lexicalItem _arguments) _statement ->
    registerAdjective marker lexicalItem environment
  Raw.AbbreviationVerb{} -> Right environment
  Raw.AbbreviationNoun{} -> Right environment
  Raw.AbbreviationRel{} -> Right environment
  Raw.AbbreviationFun{} -> Right environment
  Raw.AbbreviationEq{} -> Right environment

registerAdjective
  :: Raw.Marker -> Raw.AdjectiveLexicalItem -> AdjectiveEnvironment
  -> Either String AdjectiveEnvironment
registerAdjective marker lexicalItem environment = case userAdjectiveKey lexicalItem of
  Nothing -> Right environment
  Just key -> case Map.lookup key environment of
    Nothing -> Right (Map.insert key marker environment)
    Just registered
      | registered == marker -> Right environment
      | otherwise -> Left
          ("conflicting adjective declaration; already registered as "
            ++ markerText registered)

userAdjectiveKey :: Raw.AdjectiveLexicalItem -> Maybe AdjectiveKey
userAdjectiveKey lexicalItem = case Raw.adjectiveLexicalIdentity lexicalItem of
  Raw.IntrinsicAdjectiveIdentity{} -> Nothing
  Raw.UserAdjectiveIdentity key ->
    Just (Raw.adjectiveLexicalSide lexicalItem, key)

translateAxiom
  :: AdjectiveEnvironment -> Raw.Marker -> [Raw.Asm] -> Raw.Stmt
  -> TranslateM GPresentationJmt
translateAxiom environment marker assumptions statement = do
  ensureNoAssumptions assumptions
  proposition <- translateStatement environment statement
  pure (GFormalPresentationJmt
    (GAxiomJmt (markerLabel marker) (GListHypo []) proposition))

translateClaim
  :: AdjectiveEnvironment -> Raw.Marker -> [Raw.Asm] -> Raw.Stmt
  -> TranslateM GPresentationJmt
translateClaim environment marker assumptions statement = do
  ensureNoAssumptions assumptions
  proposition <- translateStatement environment statement
  pure (GClaimPresentationJmt
    (markerLabel marker) (GListHypo []) proposition)

ensureNoAssumptions :: [Raw.Asm] -> TranslateM ()
ensureNoAssumptions [] = pure ()
ensureNoAssumptions _ = unsupported "assumptions"

translateStatement :: AdjectiveEnvironment -> Raw.Stmt -> TranslateM GProp
translateStatement environment = \case
  Raw.StmtFormula formula -> translateFormula environment formula
  Raw.StmtVerbPhrase subjects verbPhrase ->
    translateVerbPhraseStatement environment subjects verbPhrase
  _ -> unsupported "statement"

translateFormula :: AdjectiveEnvironment -> Raw.Formula -> TranslateM GProp
translateFormula environment = \case
  Raw.FormulaChain chain -> translateChain environment chain
  Raw.Connected _location Raw.Conjunction left right ->
    GCoreAndProp
      <$> translateFormula environment left
      <*> translateFormula environment right
  Raw.FormulaQuantified _location quantifier variables Raw.Unbounded body -> do
    binder <- translateQuantifier quantifier
    identifiers <- translateVariables variables
    body' <- translateFormula environment body
    pure (foldr (binder setKind) body' identifiers)
  Raw.Connected{} -> unsupported "formula connective"
  Raw.FormulaQuantified{} -> unsupported "bounded quantification"
  _ -> unsupported "formula"

translateQuantifier
  :: Raw.Quantifier -> TranslateM (GKind -> GIdent -> GProp -> GProp)
translateQuantifier = \case
  Raw.Universally -> pure GCoreAllProp
  Raw.Existentially -> pure GCoreExistProp
  Raw.Nonexistentially -> unsupported "nonexistential quantifier"

translateChain :: AdjectiveEnvironment -> Raw.Chain -> TranslateM GProp
translateChain environment = \case
  Raw.ChainBase
      (left :| [])
      Raw.Positive
      (Raw.Relation _ Raw.EqSymbol [])
      (right :| []) ->
    GAdj2Prop (LexAdj2 "Eq_Adj2")
      <$> translateExpression environment left
      <*> translateExpression environment right
  Raw.ChainBase{} ->
    unsupported "relation: expected one positive, unparameterized equality"
  Raw.ChainCons{} -> unsupported "relation chain"

translateVerbPhraseStatement
  :: AdjectiveEnvironment -> NonEmpty Raw.Term -> Raw.VerbPhrase -> TranslateM GProp
translateVerbPhraseStatement environment subjects = \case
  Raw.VPAdj (adjective :| []) -> do
    subjects' <- traverse (translateTerm environment) subjects
    predicate <- translateAdjective environment adjective
    pure (conjoin (predicate <$> subjects'))
  Raw.VPAdj{} -> unsupported "verb phrase with multiple adjectives"
  _ -> unsupported "verb phrase"

translateAdjective
  :: AdjectiveEnvironment -> Raw.Adj -> TranslateM (GExp -> GProp)
translateAdjective environment (Raw.Adj _location lexicalItem arguments) = do
  marker <- resolveAdjective environment lexicalItem
  arguments' <- traverse (translateTerm environment) arguments
  tell [marker]
  pure (\subject -> GAppProp (markerIdent marker)
    (mkExps (subject :| arguments')))

resolveAdjective
  :: AdjectiveEnvironment -> Raw.AdjectiveLexicalItem -> TranslateM Raw.Marker
resolveAdjective environment lexicalItem =
  case Raw.adjectiveLexicalIdentity lexicalItem of
    Raw.IntrinsicAdjectiveIdentity _pattern marker -> pure marker
    Raw.UserAdjectiveIdentity surfaceKey ->
      case Map.lookup
          (Raw.adjectiveLexicalSide lexicalItem, surfaceKey) environment of
        Just marker -> pure marker
        Nothing -> unsupported "unresolved user adjective"

translateTerm :: AdjectiveEnvironment -> Raw.Term -> TranslateM GExp
translateTerm environment = \case
  Raw.TermExpr expression -> translateExpression environment expression
  _ -> unsupported "term"

translateExpression :: AdjectiveEnvironment -> Raw.Expr -> TranslateM GExp
translateExpression _environment = \case
  Raw.ExprVar variable -> variableExpression <$> translateVariable variable
  _ -> unsupported "expression: expected a named variable"

translateVariables :: NonEmpty Raw.VarSymbol -> TranslateM [GIdent]
translateVariables = traverse translateVariable . NonEmpty.toList

translateVariable :: Raw.VarSymbol -> TranslateM GIdent
translateVariable = \case
  Raw.NamedVarAt _location name ->
    pure (GStrIdent (GString (Text.unpack name)))
  Raw.FreshVarAt{} -> unsupported "fresh variable"

variableExpression :: GIdent -> GExp
variableExpression = GTermExp . GIdentTerm

mkExps :: NonEmpty GExp -> GExps
mkExps (expression :| []) = GOneExps expression
mkExps (first :| (second : rest)) =
  GManyExps (GListExp (first : second : rest))

conjoin :: NonEmpty GProp -> GProp
conjoin (proposition :| propositions) =
  foldl GCoreAndProp proposition propositions

markerIdent :: Raw.Marker -> GIdent
markerIdent (Raw.Marker marker) =
  GStrIdent (GString (Text.unpack marker))

markerLabel :: Raw.Marker -> GLabel
markerLabel = GIdentLabel . markerIdent

markerText :: Raw.Marker -> String
markerText (Raw.Marker marker) = Text.unpack marker

setKind :: GKind
setKind = GNounKind (LexNoun "set_Noun")

unsupported :: String -> TranslateM a
unsupported message = WriterT (Left ("unsupported Felix " ++ message))
