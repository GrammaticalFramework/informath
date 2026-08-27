{-# LANGUAGE GADTs #-}
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
  hypotheses <- translateAssumptions environment assumptions
  proposition <- translateStatement environment statement
  pure (GFormalPresentationJmt
    (GAxiomJmt (markerLabel marker) (GListHypo hypotheses) proposition))

translateClaim
  :: AdjectiveEnvironment -> Raw.Marker -> [Raw.Asm] -> Raw.Stmt
  -> TranslateM GPresentationJmt
translateClaim environment marker assumptions statement = do
  hypotheses <- translateAssumptions environment assumptions
  proposition <- translateStatement environment statement
  pure (GClaimPresentationJmt
    (markerLabel marker) (GListHypo hypotheses) proposition)

translateAssumptions :: AdjectiveEnvironment -> [Raw.Asm] -> TranslateM [GHypo]
translateAssumptions environment = fmap concat . traverse translateAssumption
 where
  translateAssumption = \case
    Raw.AsmSuppose statement ->
      pure . GSupposePropHypo <$> translateStatement environment statement
    Raw.AsmLetNoun variables nounPhrase -> do
      identifiers <- translateVariables variables
      kind <- translateNounKind environment nounPhrase
      pure [GVarsHypo (GListIdent identifiers) kind]
    Raw.AsmLetIn variables expression -> do
      identifiers <- translateVariables variables
      expression' <- translateExpression environment expression
      pure [GVarsHypo (GListIdent identifiers) (GExpKind expression')]
    Raw.AsmLetRelation variables relation expression -> do
      relationLeaf <- translateRelation relation
      identifiers <- translateVariables variables
      right <- translateExpression environment expression
      let relationPropositions =
            [GPropHypo (relationLeaf (variableExpression identifier) right)
            | identifier <- identifiers
            ]
      pure
        (GVarsHypo (GListIdent identifiers) setKind : relationPropositions)
    Raw.AsmLetThe{} -> unsupported "assumption AsmLetThe"
    Raw.AsmLetEq{} -> unsupported "assumption AsmLetEq"
    Raw.AsmLetStruct{} -> unsupported "assumption AsmLetStruct"

translateStatement :: AdjectiveEnvironment -> Raw.Stmt -> TranslateM GProp
translateStatement environment = \case
  Raw.StmtFormula formula -> translateFormula environment formula
  Raw.StmtConnected connective _location left right -> do
    left' <- translateStatement environment left
    right' <- translateStatement environment right
    combineConnective connective left' right'
  Raw.SymbolicQuantified _location quantifier variables bound suchThat body ->
    translateSymbolicQuantified environment quantifier variables bound suchThat body
  Raw.StmtVerbPhrase subjects verbPhrase ->
    translateVerbPhraseStatement environment subjects verbPhrase
  Raw.StmtNoun subjects nounPhrase ->
    translateNounStatement environment subjects nounPhrase
  Raw.StmtStruct{} -> unsupported "statement StmtStruct"
  Raw.StmtNeg{} -> unsupported "statement StmtNeg"
  Raw.StmtExists{} -> unsupported "statement StmtExists"
  Raw.StmtQuantPhrase{} -> unsupported "statement StmtQuantPhrase"
  Raw.StmtExactStruct{} -> unsupported "statement StmtExactStruct"

translateFormula :: AdjectiveEnvironment -> Raw.Formula -> TranslateM GProp
translateFormula environment = \case
  Raw.FormulaChain chain -> translateChain environment chain
  Raw.Connected _location connective left right -> do
    left' <- translateFormula environment left
    right' <- translateFormula environment right
    combineConnective connective left' right'
  Raw.FormulaQuantified _location quantifier variables bound body -> case bound of
    Raw.Unbounded -> do
      binder <- translateQuantifier quantifier
      identifiers <- translateVariables variables
      body' <- translateFormula environment body
      pure (foldr (binder setKind) body' identifiers)
    Raw.Bounded{} -> unsupported "bounded FormulaQuantified"
  Raw.FormulaTypedQuantified _location quantifier variables concreteType body -> do
    binder <- translateQuantifier quantifier
    identifiers <- translateVariables variables
    kind <- translateConcreteType concreteType
    body' <- translateFormula environment body
    pure (foldr (binder kind) body' identifiers)
  Raw.FormulaSetMap{} -> unsupported "formula FormulaSetMap"
  Raw.FormulaExpression{} -> unsupported "formula FormulaExpression"
  Raw.FormulaPredicate{} -> unsupported "formula FormulaPredicate"
  Raw.FormulaNeg{} -> unsupported "formula FormulaNeg"
  Raw.PropositionalConstant{} -> unsupported "propositional constant"

translateSymbolicQuantified
  :: AdjectiveEnvironment
  -> Raw.Quantifier
  -> NonEmpty Raw.VarSymbol
  -> Raw.Bound
  -> Maybe Raw.Stmt
  -> Raw.Stmt
  -> TranslateM GProp
translateSymbolicQuantified environment quantifier variables bound suchThat body =
  case quantifier of
    Raw.Universally -> translateWith GCoreAllProp GCoreIfProp
    Raw.Existentially -> translateWith GCoreExistProp GCoreAndProp
    Raw.Nonexistentially -> unsupported "nonexistential quantifier"
 where
  translateWith binder combineGuards = do
    identifiers <- translateVariables variables
    boundGuards <- translateBound environment identifiers bound
    suchThatGuards <- maybe (pure [])
      (fmap pure . translateStatement environment) suchThat
    body' <- translateStatement environment body
    let guarded = case NonEmpty.nonEmpty (boundGuards ++ suchThatGuards) of
          Nothing -> body'
          Just guards -> combineGuards (conjoin guards) body'
    pure (foldr (binder setKind) guarded identifiers)

translateBound
  :: AdjectiveEnvironment -> [GIdent] -> Raw.Bound -> TranslateM [GProp]
translateBound _environment _identifiers Raw.Unbounded = pure []
translateBound environment identifiers (Raw.Bounded _location sign relation expression) = do
  relationLeaf <- translateRelation relation
  right <- translateExpression environment expression
  pure
    [applySign sign (relationLeaf (variableExpression identifier) right)
    | identifier <- identifiers
    ]

translateQuantifier
  :: Raw.Quantifier -> TranslateM (GKind -> GIdent -> GProp -> GProp)
translateQuantifier = \case
  Raw.Universally -> pure GCoreAllProp
  Raw.Existentially -> pure GCoreExistProp
  Raw.Nonexistentially -> unsupported "nonexistential quantifier"

translateConcreteType :: Raw.ConcreteType -> TranslateM GKind
translateConcreteType = \case
  Raw.ConcreteSet -> pure setKind
  Raw.ConcreteArrow Raw.ConcreteSet Raw.ConcreteSet ->
    pure (GFunKind (GListArgKind [GKindArgKind setKind]) setKind)
  concreteType -> unsupported ("concrete type " ++ show concreteType)

combineConnective :: Raw.Connective -> GProp -> GProp -> TranslateM GProp
combineConnective connective left right = case connective of
  Raw.Conjunction -> pure (GCoreAndProp left right)
  Raw.Disjunction -> pure (GCoreOrProp left right)
  Raw.Implication -> pure (GCoreIfProp left right)
  Raw.Equivalence -> pure (GCoreIffProp left right)
  Raw.ExclusiveOr -> pure
    (GCoreAndProp
      (GCoreOrProp left right)
      (GCoreNotProp (GCoreAndProp left right)))
  Raw.NegatedDisjunction -> unsupported "connective NegatedDisjunction"

translateChain :: AdjectiveEnvironment -> Raw.Chain -> TranslateM GProp
translateChain environment chain = conjoin . snd <$> translateChainParts environment chain

translateChainParts
  :: AdjectiveEnvironment -> Raw.Chain
  -> TranslateM (NonEmpty GExp, NonEmpty GProp)
translateChainParts environment = \case
  Raw.ChainBase left sign relation right -> do
    left' <- traverse (translateExpression environment) left
    right' <- traverse (translateExpression environment) right
    propositions <- translateRelationGroup sign relation left' right'
    pure (left', propositions)
  Raw.ChainCons left sign relation rest -> do
    left' <- traverse (translateExpression environment) left
    (middle, restPropositions) <- translateChainParts environment rest
    propositions <- translateRelationGroup sign relation left' middle
    pure (left', propositions <> restPropositions)

translateRelationGroup
  :: Raw.Sign -> Raw.Relation -> NonEmpty GExp -> NonEmpty GExp
  -> TranslateM (NonEmpty GProp)
translateRelationGroup sign relation (left :| leftRest) (right :| rightRest) = do
  relationLeaf <- translateRelation relation
  let first = applySign sign (relationLeaf left right)
      rest =
        map (applySign sign . relationLeaf left) rightRest
          ++ concatMap
            (\nextLeft ->
              map (applySign sign . relationLeaf nextLeft) (right : rightRest))
            leftRest
  pure (first :| rest)

translateRelation :: Raw.Relation -> TranslateM (GExp -> GExp -> GProp)
translateRelation = \case
  Raw.Relation _location symbol parameters
    | not (null parameters) -> unsupported "parameterized relation occurrence"
    | Raw.parameterArityValue (Raw.relationSymbolParameterArity symbol) /= 0 ->
        unsupported "relation with nonzero declared parameter arity"
    | otherwise -> relationForMarker (Raw.relationSymbolMarker symbol)
  Raw.RelationExpr{} -> unsupported "relation expression"

relationForMarker :: Raw.Marker -> TranslateM (GExp -> GExp -> GProp)
relationForMarker marker = case markerText marker of
  "eq" -> pure (GAdj2Prop (LexAdj2 "Eq_Adj2"))
  "neq" -> pure (GAdj2Prop (LexAdj2 "Neq_Adj2"))
  "elem" -> pure (GNoun2Prop (LexNoun2 "element_Noun2"))
  "notelem" -> pure (GNoun2Prop (LexNoun2 "notelement_Noun2"))
  "subset" -> pure (GNoun2Prop (LexNoun2 "subset_Noun2"))
  "subseteq" -> pure (GNoun2Prop (LexNoun2 "subseteq_Noun2"))
  "supseteq" -> pure (GNoun2Prop (LexNoun2 "superseteq_Noun2"))
  relation -> unsupported ("relation marker " ++ relation)

applySign :: Raw.Sign -> GProp -> GProp
applySign Raw.Positive = id
applySign Raw.Negative = GCoreNotProp

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
  case (markerText marker, arguments') of
    ("inhabited", []) -> pure (GAdjProp (LexAdj "inhabited_Adj"))
    ("empty", []) -> pure (GAdjProp (LexAdj "empty_Adj"))
    ("disjoint", [argument]) ->
      pure (\subject -> GAdjCProp (LexAdjC "disjoint_AdjC") subject argument)
    ("inhabited", _) -> unsupported "adjective inhabited at the wrong arity"
    ("empty", _) -> unsupported "adjective empty at the wrong arity"
    ("disjoint", _) -> unsupported "adjective disjoint at the wrong arity"
    _ -> do
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

translateNounStatement
  :: AdjectiveEnvironment -> NonEmpty Raw.Term -> Raw.NounPhrase Maybe
  -> TranslateM GProp
translateNounStatement environment (subject :| []) nounPhrase = do
  subject' <- translateTerm environment subject
  case nounPhrase of
    Raw.NounPhrase [] (Raw.Noun _location item []) Nothing [] Nothing
      | markerText (Raw.lexicalItemSgPlMarker item) == "subsingleton" ->
          pure (GNoun1Prop (GNounNoun1 (LexNoun "subsingleton_Noun")) subject')
    _ -> unsupported "noun assertion shape"
translateNounStatement _environment _subjects _nounPhrase =
  unsupported "noun assertion with multiple subjects"

translateNounKind
  :: AdjectiveEnvironment -> Raw.NounPhrase Maybe -> TranslateM GKind
translateNounKind environment = \case
  Raw.NounPhrase [] (Raw.Noun _location item arguments) Nothing [] Nothing ->
    case (markerText (Raw.lexicalItemSgPlMarker item), arguments) of
      ("set", []) -> pure setKind
      ("family_of_subsets", [argument]) -> do
        argument' <- translateTerm environment argument
        pure (GDepKind (LexDep "family_of_subsets_Dep") argument')
      ("family_of_subsets", _) -> unsupported "family_of_subsets at the wrong arity"
      _ -> unsupported "noun kind marker"
  _ -> unsupported "noun kind shape"

translateTerm :: AdjectiveEnvironment -> Raw.Term -> TranslateM GExp
translateTerm environment = \case
  Raw.TermExpr expression -> translateExpression environment expression
  Raw.TermFun{} -> unsupported "term TermFun"
  Raw.TermIota{} -> unsupported "term TermIota"
  Raw.TermQuantified{} -> unsupported "term TermQuantified"

translateExpression :: AdjectiveEnvironment -> Raw.Expr -> TranslateM GExp
translateExpression environment = \case
  Raw.ExprVar variable -> variableExpression <$> translateVariable variable
  Raw.ExprFiniteSet _location expressions ->
    GEnumSetExp . mkExps <$> traverse (translateExpression environment) expressions
  Raw.ExprOp _location symbol arguments
    | symbol == Raw.ApplySymbol -> case arguments of
        [function, argument] -> do
          function' <- translateExpression environment function
          argument' <- translateExpression environment argument
          pure (GAppExp function' (GOneExps argument'))
        _ -> unsupported "apply operator at the wrong arity"
    | otherwise -> translateOperation environment (Raw.mixfixMarker symbol) arguments
  Raw.ExprHigherApply _location function argument -> case (function, argument) of
    (Raw.TypedExpressionExpr functionExpression,
      Raw.TypedExpressionExpr argumentExpression) -> do
        function' <- translateExpression environment functionExpression
        argument' <- translateExpression environment argumentExpression
        pure (GAppExp function' (GOneExps argument'))
    _ -> unsupported "formula-valued higher application"
  Raw.ExprInteger{} -> unsupported "expression ExprInteger"
  Raw.ExprTypedConstant{} -> unsupported "expression ExprTypedConstant"
  Raw.ExprLambda{} -> unsupported "expression ExprLambda"
  Raw.ExprSelect{} -> unsupported "expression ExprSelect"
  Raw.ExprStructOp{} -> unsupported "expression ExprStructOp"
  Raw.ExprSep{} -> unsupported "expression ExprSep"
  Raw.ExprReplace{} -> unsupported "expression ExprReplace"
  Raw.ExprReplacePred{} -> unsupported "expression ExprReplacePred"
  Raw.ExprStructAggregate{} -> unsupported "expression ExprStructAggregate"
  Raw.ExprStructReduct{} -> unsupported "expression ExprStructReduct"

translateOperation
  :: AdjectiveEnvironment -> Raw.Marker -> [Raw.Expr] -> TranslateM GExp
translateOperation environment marker arguments = case markerText marker of
  "emptyset" -> case arguments of
    [] -> pure (GNameExp (LexName "emptyset_Name"))
    _ -> wrongArity
  "union" -> binary (GFunCExp (LexFunC "union_FunC"))
  "inter" -> binary (GFunCExp (LexFunC "intersection_FunC"))
  "setminus" -> binary (GFun2Exp (LexFun2 "difference_Fun2"))
  "times" -> binary (GFunCExp (LexFunC "cartesian_FunC"))
  "pair" -> binary pairExpression
  _ -> case NonEmpty.nonEmpty arguments of
    Nothing -> unsupported ("unknown nullary operator " ++ markerText marker)
    Just nonemptyArguments -> do
      arguments' <- traverse (translateExpression environment) nonemptyArguments
      tell [marker]
      pure (GAppExp (GTermExp (GIdentTerm (markerIdent marker))) (mkExps arguments'))
 where
  binary constructor = case arguments of
    [left, right] -> do
      left' <- translateExpression environment left
      right' <- translateExpression environment right
      pure (constructor left' right')
    _ -> wrongArity
  wrongArity = unsupported
    ("operator " ++ markerText marker ++ " at the wrong arity")

pairExpression :: GExp -> GExp -> GExp
pairExpression left right =
  GTermExp (GTupleTerm (GListTerm (map expAsTerm [left, right])))

expAsTerm :: GExp -> GTerm
expAsTerm = \case
  GTermExp term -> term
  expression -> GTextualTerm expression

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
