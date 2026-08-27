{-# LANGUAGE PatternSynonyms #-}

module Felix2Informath
  ( translateBlocks
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import qualified Felix.Syntax.Abstract as Raw
import Informath

translateBlocks :: [Raw.Block] -> Either String [GJmt]
translateBlocks = traverse translateBlock

translateBlock :: Raw.Block -> Either String GJmt
translateBlock (Raw.BlockAxiom _location _title marker (Raw.Axiom assumptions statement))
  | null assumptions =
      GAxiomJmt (translateMarker marker) (GListHypo [])
        <$> translateStatement statement
  | otherwise = Left "unsupported Felix axiom: assumptions are not supported"
translateBlock _ = Left "unsupported Felix block: expected an axiom"

translateStatement :: Raw.Stmt -> Either String GProp
translateStatement (Raw.StmtFormula formula) = translateFormula formula
translateStatement _ = Left "unsupported Felix statement: expected a symbolic formula"

translateFormula :: Raw.Formula -> Either String GProp
translateFormula (Raw.FormulaChain chain) = translateChain chain
translateFormula (Raw.Connected _ Raw.Conjunction left right) =
  GCoreAndProp <$> translateFormula left <*> translateFormula right
translateFormula (Raw.FormulaQuantified _ quantifier variables Raw.Unbounded body) = do
  binder <- translateQuantifier quantifier
  identifiers <- traverse translateVariable variables
  body' <- translateFormula body
  pure (foldr (binder setKind) body' identifiers)
translateFormula (Raw.Connected _ _ _ _) =
  Left "unsupported Felix formula: only conjunction is supported"
translateFormula (Raw.FormulaQuantified _ _ _ _ _) =
  Left "unsupported Felix formula: bounded quantification is not supported"
translateFormula _ = Left "unsupported Felix formula"

translateQuantifier :: Raw.Quantifier -> Either String (GKind -> GIdent -> GProp -> GProp)
translateQuantifier Raw.Universally = Right GCoreAllProp
translateQuantifier Raw.Existentially = Right GCoreExistProp
translateQuantifier Raw.Nonexistentially =
  Left "unsupported Felix quantifier: nonexistential quantification is not supported"

translateChain :: Raw.Chain -> Either String GProp
translateChain
    (Raw.ChainBase
      (left :| [])
      Raw.Positive
      (Raw.Relation _ Raw.EqSymbol [])
      (right :| [])) =
  GAdj2Prop (LexAdj2 "Eq_Adj2")
    <$> translateExpression left
    <*> translateExpression right
translateChain (Raw.ChainBase _ _ _ _) =
  Left "unsupported Felix relation: expected one positive, unparameterized equality"
translateChain (Raw.ChainCons _ _ _ _) =
  Left "unsupported Felix relation: chains are not supported"

translateExpression :: Raw.Expr -> Either String GExp
translateExpression (Raw.ExprVar variable) =
  GTermExp . GIdentTerm <$> translateVariable variable
translateExpression _ = Left "unsupported Felix expression: expected a named variable"

translateVariable :: Raw.VarSymbol -> Either String GIdent
translateVariable (Raw.NamedVarAt _ name) =
  Right (GStrIdent (GString (Text.unpack name)))
translateVariable Raw.FreshVarAt{} =
  Left "unsupported Felix expression: fresh variables are not supported"

translateMarker :: Raw.Marker -> GLabel
translateMarker (Raw.Marker marker) =
  GIdentLabel (GStrIdent (GString (Text.unpack marker)))

setKind :: GKind
setKind = GNounKind (LexNoun "set_Noun")
