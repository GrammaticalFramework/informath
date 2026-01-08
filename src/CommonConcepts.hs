{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module CommonConcepts where

import Dedukti.AbsDedukti
import Informath
import qualified Data.Map as M
import Data.List (isSuffixOf)
import Data.Char

type CTree a = Informath.Tree a
type DTree a = Dedukti.AbsDedukti.Tree a

-- referring to mathbase.dk

identConj = QIdent "and"
identDisj = QIdent "or"
identImpl = QIdent "if"
identNeg = QIdent "not"
identEquiv = QIdent "iff"
identPi = QIdent "forall"
identSigma = QIdent "exists"

identNat =  QIdent "Nat"
identInt =  QIdent "Int"
identRat =  QIdent "Rat"
identReal =  QIdent "Real"
identPlus =  QIdent "plus"
identMinus =  QIdent "minus"
identTimes =  QIdent "times"
identDiv =  QIdent "div"
identEq =  QIdent "Eq"
identLt =  QIdent "Lt"
identGt =  QIdent "Gt"
identNeq =  QIdent "Neq"
identLeq =  QIdent "Leq"
identGeq =  QIdent "Geq"

-- Peano-style Nat constructors in BaseConstants.dk
identZero = QIdent "0"
identSucc = QIdent "succ"

-- these are to be peeled away
identProof = QIdent "Proof"
identElem = QIdent "Elem"

identSuchThat = QIdent "suchthat"

-- logical constants in base.dk
propFalse = EIdent (QIdent "false")
propAnd x y = EApp (EApp (EIdent identConj) x) y
propOr x y = EApp (EApp (EIdent identDisj) x) y
propImp x y = EApp (EApp (EIdent identImpl) x) y
propEquiv x y = EApp (EApp (EIdent identEquiv) x) y
propNeg x = EApp (EIdent identNeg) x

propPi kind pred = EApp (EApp (EIdent identPi) kind) pred
propSigma kind pred = EApp (EApp (EIdent identSigma) kind) pred

-- built-in types
typeProp = EIdent identProp
typeSet = EIdent identSet
typeType = EIdent identType 

identProp = QIdent "Prop"
identSet = QIdent "Set"
identType = QIdent "Type"

--- needed for typing conclusions of proofs
expTyped x t = EApp (EApp (EIdent (QIdent "typed")) x) t
expNegated x = EApp (EIdent (QIdent "neg")) x

-- lookup after annotation from dynamically loaded file
lookupConstantFull :: String -> Maybe (String, String, [String])
lookupConstantFull f = case splitConstant f of
  Right (_ : cat : fun : args) -> return (cat, fun, args)
  _ -> Nothing
  
-- lookup just fun and cat
lookupConstant :: String -> Maybe (String, String)
lookupConstant f = case splitConstant f of
  Right (_ : cat : fun : _) -> return (cat, fun)
  _ -> Nothing

-- split at # ; they can occur spuriously inside {|...|} but not elsewhere
splitConstant :: String -> Either String [String]
splitConstant f
  | isSuffixOf "|}" f = Left f
  | otherwise = case words (map (\c -> if c=='#' then ' ' else c) f) of
      ws@(_:_:_) -> Right ws
      _ -> Left f

stripConstant :: String -> String
stripConstant f = case splitConstant f of
  Left _ -> f
  Right (h:_) -> h

-- deal with {|ident|}
unescapeConstant :: String -> String
unescapeConstant s = case s of
  _ | take 2 s == "{|" -> drop 2 (take (length s - 2) s)
  _ -> s

escapeConstant :: String -> String
escapeConstant s = case s of
  _ | any (not . isIdentChar) s -> "{|" ++ s ++ "|}"
  _ -> s

isIdentChar :: Char -> Bool
isIdentChar c = or [isAlpha c, isDigit c, elem c ".'_"]

-- Dedukti representation of digits
digitFuns :: [String]
digitFuns = [nn, nd]
nn = "nn"
nd = "nd"

