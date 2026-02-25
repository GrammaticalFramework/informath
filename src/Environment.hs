module Environment where

import Dedukti.AbsDedukti
import PGF
import BuildConstantTable
import Utils

import Data.List (intersperse, isPrefixOf)
import Data.Char (isDigit)

import qualified Data.Set as Set

import Debug.Trace (traceShowId, trace)

trac :: Show a => Env -> String -> a -> a 
trac env s a = tracs env (s ++ show a) a

tracs :: Env -> String -> a -> a 
tracs env s a = if isFlag "-debug" env then trace s a else a


type Flag = String

data Env = Env {
  flags :: [Flag],
  informathRoot :: String,
  grammar :: PGF,
  baseConstantModule :: Module,
  constantTable :: ConstantTable,
  conversionTable :: ConversionTable,
  backConstantTable :: BackConstantTable,
  reachableFunctions :: Set.Set CId,
  dropTable :: DropTable,
  macroTable :: MacroTable,
  formalisms :: [String],
  langs :: [Language],
  toLang :: Language,
  fromLang :: Language,
  toFormalism :: String,
  nbestNLG :: Maybe Int,
  scoreWeights :: [Int],
  morpho :: Morpho
  }

-------------------------------------------
-- low level auxiliaries

informathPrefix = "Informath"
english = "Eng"

relevantLanguages gr args = [
  lang |
    code <- commaSep (argValue "-languages" english args),
    let Just lang = readLanguage (informathPrefix ++ code),
    elem lang (PGF.languages gr)
  ]

argValue flag df args = case [f | f <- args, isPrefixOf flag f] of
  f:_ -> drop (length flag + 1) f   -- -<flag>=<value>
  _ -> df
  
argValues flag df args = commaSep (argValue flag df args)
  
argValueMaybeInt flag args = case argValue flag "nothing" args of
  v | all isDigit v -> Just (read v :: Int)
  _ -> Nothing

isFlag flag env = elem flag (flags env)

flagHasValue flag args = elem flag (map (takeWhile (/='=')) args)

ifArg flag args msg = if elem flag args then putStrLn msg else return ()

inputFileArg args = case [arg | arg <- args, head arg /= '-'] of
  [arg] -> Just (arg, fileSuffix arg)
  _ -> Nothing

inputFileArgs args = case [arg | arg <- args, head arg /= '-'] of
  arg@(f:fs) | and [fileSuffix g == fileSuffix f | g <- fs] -> Just (args, fileSuffix f)
  _ -> Nothing

