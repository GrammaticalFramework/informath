{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module CommonConcepts where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (isSuffixOf)
import Data.Char
import Utils (split)

-- Informath category symbols with arities

type SCat = String

-- all symbol-table enabled categories to value and argument cats
gfCatMap :: M.Map SCat (SCat, [SCat])
gfCatMap = M.union mainCatMap (M.union verbalCatMap symbolicCatMap)

mainCats :: S.Set SCat
mainCats = S.fromList ["Exp", "Prop", "Kind", "Proof", "ProofExp", "Unit", "Term", "Formula"]

mainCatMap :: M.Map SCat (SCat, [SCat])
mainCatMap = M.fromList [(c, (c, [])) | c <- S.toList mainCats]

-- lexical categories to value and argument cats (See file grammars/Categories.gf)
verbalCatMap :: M.Map SCat (SCat, [SCat])
verbalCatMap = M.fromList [
  ("Noun", ("Kind", [])),
  ("Fam", ("Kind", ["Kind"])),
  ("Fam2", ("Kind", ["Kind", "Kind"])),
  ("Noun1", ("Prop", ["Exp"])),
  ("Noun2", ("Prop", ["Exp", "Exp"])),
  ("Noun3", ("Prop", ["Exp", "Exp", "Exp"])),
  ("NounC", ("Prop", ["Exp", "Exp"])),
  ("Adj", ("Prop", ["Exp"])),
  ("Adj2", ("Prop", ["Exp", "Exp"])),
  ("Adj3", ("Prop", ["Exp", "Exp", "Exp"])),
  ("AdjE", ("Prop", ["Exp", "Exp"])),
  ("AdjC", ("Prop", ["Exp", "Exp"])),
  ("Verb", ("Prop", ["Exp"])),
  ("Verb2", ("Prop", ["Exp", "Exp"])),
  ("VerbC", ("Prop", ["Exp", "Exp"])),
  ("Name", ("Exp", [])),
  ("Fun", ("Exp", ["Exp"])),
  ("Fun2", ("Exp", ["Exp", "Exp"])),
  ("FunC", ("Exp", ["Exp", "Exp"])),
  ("Label", ("ProofExp", [])),
  ("Dep", ("Kind", ["Exp"])),
  ("Dep2",("Kind", ["Exp", "Exp"])),
  ("DepC", ("Kind", ["Exp", "Exp"])),
  ("Adv", ("Prop", ["Exp"])),
  ("Adv2", ("Prop", ["Exp", "Exp"])),
  ("AdvC", ("Prop", ["Exp", "Exp"])),

  ("Binder",  ("Exp", ["Ident", "Exp"])),
  ("Binder1", ("Exp", ["Kind", "Ident", "Exp"])),
  ("Binder2", ("Exp", ["Exp", "Exp", "Ident", "Exp"]))
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
symbolicCats = S.fromList ("MACRO" : M.keys symbolicCatMap)

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


-- lookup after annotation from dynamically loaded file
lookupConstantFull :: String -> Maybe (String, String, [String], String)  -- last arg: profile
lookupConstantFull f = case splitConstant f of
  Right (_ : cat : fun : args) -> return (cat, fun, args, last ("":args))
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

