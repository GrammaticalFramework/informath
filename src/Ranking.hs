module Ranking (rankTreesAndStrings) where

import PGF
import Environment
import Lexing
import ParseInformath
import Data.List (sortOn)

-- ranking trees and their linearizations with a set of scores

data Scores = Scores {
  tree_length :: Int,
  tree_depth :: Int,
  characters :: Int,
  tokens :: Int,
  subsequent_dollars :: Int,
  initial_dollars :: Int,
  extra_parses :: Int
  } deriving Show

scoreString :: Env -> String -> Scores
scoreString env s = Scores {
  tree_length = 0, -- computed separately
  tree_depth = 0,  -- 
  characters = length s,
  tokens = length toks,
  subsequent_dollars = subdollars toks,
  initial_dollars = initdollars toks + if head toks == "$" then 1 else 0,
  extra_parses =  -- can be expensive; return #parses -1 
    if (ifFlag "-test-ambiguity" env)
    then maybe 0 ((+ (-1)) . length . take 3) (fst (parseJmt (cpgf env) (tolang env) jmt inds))
    else 1
  }
 where
   (inds, _) = indexTex (lextex s)
   Just jmt = readType "Jmt"
   toks = words (lextex s)
   subdollars ts = case ts of
     "$":"$":tt -> 1 + subdollars tt
     "$":",":"$":tt -> 1 + subdollars tt
     "$":".":"$":tt -> 1 + subdollars tt
     _:tt -> subdollars tt
     _ -> 0
   initdollars ts = case ts of
     ".":"$":tt -> 1 + initdollars tt
     _:tt -> initdollars tt
     _ -> 0

treeLength :: Expr -> Int
treeLength t = case unApp t of
  Just (f, ts@(_:_)) -> 1 + sum (map treeLength ts)
  _ -> 1

treeDepth :: Expr -> Int
treeDepth t = case unApp t of
  Just (f, ts@(_:_)) -> 1 + maximum (map treeDepth ts)
  _ -> 1

-- returns all scores and their sum
scoreTreeAndString :: Env -> (Expr, String) -> (Scores, Int)
scoreTreeAndString env (t, s) =
  let scores = (scoreString env s){
        tree_length = treeLength t, tree_depth = treeDepth t}
  in (scores,
      sum (map (\ (w, f) -> w * (f scores))
               (zip
	         (scoreWeights env)
	         [tree_length, tree_depth, characters, tokens,
                 subsequent_dollars, initial_dollars, extra_parses])))
		 

-- sorts trees from lowest to highest total score
rankTreesAndStrings :: Env -> [(Expr, String)] -> [((Expr, String), (Scores, Int))] 
rankTreesAndStrings env tss = sortOn (snd . snd) [(ts, scoreTreeAndString env ts) | ts <- tss]


