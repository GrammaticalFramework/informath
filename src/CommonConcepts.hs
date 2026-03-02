{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module CommonConcepts where

import Dedukti.AbsDedukti
import Informath
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (isSuffixOf)
import Data.Char

type CTree a = Informath.Tree a
type DTree a = Dedukti.AbsDedukti.Tree a

-- Informath category symbols with arities

type SCat = String

-- all symbol-table enabled categories to value and argument cats
gfCatMap :: M.Map SCat (SCat, [SCat])
gfCatMap = M.union mainCatMap (M.union verbalCatMap symbolicCatMap)

mainCats :: S.Set SCat
mainCats = S.fromList ["Exp", "Kind", "Prop", "Proof", "ProofExp", "Unit", "Term", "Formula"]

mainCatMap :: M.Map SCat (SCat, [SCat])
mainCatMap = M.fromList [(c, (c, [])) | c <- S.toList mainCats]

-- lexical categories to value and argument cats
verbalCatMap :: M.Map SCat (SCat, [SCat])
verbalCatMap = M.fromList [
  ("Fam", ("Kind", ["Kind"])),
  ("Fam2", ("Kind", ["Kind", "Kind"])),
  ("Noun", ("Kind", [])),
  ("Dep", ("Kind", ["Exp"])),
  ("Dep2",("Kind", ["Exp", "Exp"])),
  ("DepC", ("Kind", ["Exp", "Exp"])),
  ("Adj", ("Prop", ["Exp"])),
  ("Verb", ("Prop", ["Exp"])),
  ("Noun1", ("Prop", ["Exp"])),
  ("Adj2", ("Prop", ["Exp", "Exp"])),
  ("AdjC", ("Prop", ["Exp", "Exp"])),
  ("AdjE", ("Prop", ["Exp", "Exp"])),
  ("Verb2", ("Prop", ["Exp", "Exp"])),
  ("Noun2", ("Prop", ["Exp", "Exp"])),
  ("Adj3", ("Prop", ["Exp", "Exp", "Exp"])),
  ("VerbC", ("Prop", ["Exp", "Exp"])),
  ("NounC", ("Prop", ["Exp", "Exp"])),
  ("Fun", ("Exp", ["Exp"])),
  ("Fun2", ("Exp", ["Exp", "Exp"])),
  ("FunC", ("Exp", ["Exp", "Exp"])),
  ("Name", ("Exp", [])),
  ("Binder", ("Exp", ["Ident", "Exp"])),
  ("Binder1", ("Exp", ["Kind", "Ident", "Exp"])),
  ("Binder2", ("Exp", ["Exp", "Exp", "Ident", "Exp"])),
  ("Label", ("ProofExp", []))
  ]

-- symbolic categories to verbal types (for type checking with Dedukti)
symbolicCatMap :: M.Map SCat (SCat, [SCat])
symbolicCatMap = M.fromList [
  ("Formula", ("Prop", [])),
  ("Term", ("Exp", [])),
  ("Compar", ("Prop", ["Exp", "Exp"])),
  ("Const", ("Exp", [])),
  ("Oper", ("Exp", ["Exp"])),
  ("Oper2", ("Exp", ["Exp", "Exp"]))
  ]

symbolicCats :: S.Set SCat
symbolicCats = S.fromList (M.keys symbolicCatMap)

verbalCats :: S.Set SCat
verbalCats = S.fromList (M.keys verbalCatMap)

kindCats :: S.Set SCat
kindCats = S.fromList $ "Kind" : [c | (c, ("Kind", _)) <- M.assocs verbalCatMap]

expCats :: S.Set SCat
expCats = S.fromList $ "Exp" : [c | (c, ("Exp", _)) <- M.assocs verbalCatMap]

propCats :: S.Set SCat
propCats = S.fromList $ "Prop" : [c | (c, ("Prop", _)) <- M.assocs verbalCatMap]

proofCats :: S.Set SCat 
proofCats = S.fromList ["Label", "Proof", "ProofExp", "Unit"]


-- referring to BaseConstants.dk

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
  | otherwise = case split '#' f of
      ws@(_:_:_) -> Right ws
      _ -> Left f

-- leave only the first part, which is a GF function (can be a complex term)
stripConstant :: String -> String
stripConstant f = case splitConstant f of
  Left _ -> f
  Right (h:_) -> h

--- Data.List.Split cannot be found...
split :: Char -> String -> [String]
split c cs = case break (==c) cs of
  ([], []) -> []
  (s,  []) -> [strip s]
  (s, _:s2) -> strip s : split c s2
 where
  strip = unwords . words

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

