{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- top-level conversions between formats

module InformathAPI where

--import Core2Dedukti (jmt2dedukti)
--import Dedukti2Core
--import Environment
import Dedukti.PrintDedukti
import Dedukti.ParDedukti
import Dedukti.AbsDedukti
import Dedukti.ErrM
--import DeduktiOperations
--import ConstantData 
--import SpecialDeduktiConversions (specialDeduktiConversions)
------import Informath -- to be removed
--import Core2Informath (nlg)
--import Informath2Core (semantics)
--import ParseInformath (parseJmt)
--import Lexing
--import MkConstants (mkConstants)
--import qualified Dedukti2Agda as DA
--import qualified Dedukti2Rocq as DR
--import qualified Dedukti2Lean as DL
--import Ranking

import BuildConstantTable
import qualified DMC
import qualified MCI
import qualified IMC
import qualified MCD

import NextInformath
import PGF

import Data.List (partition, isSuffixOf, isPrefixOf, intersperse, sortOn)
------import System.Random
import Data.Char (isDigit, toUpper) --- low-level auxiliaries
--import System.Environment (getArgs)
--import System.IO
--import qualified Data.Map as M

-- default source files

grammarFile = "next/grammars/NextInformath.pgf"
baseConstantFile = "src/BaseConstants.dk"
constantTableFile = "next/constants.dkgf"


data Env = Env {
  grammar :: PGF,
  baseConstantModule :: Module,
  constantTable :: ConstantTable,
  nbestDedukti :: Maybe Int
  }

data Result = Result {
  origDedukti :: Jmt,
  annotDedukti :: [Jmt]
  }
  deriving Show

type Flag = String

dedukti2core :: Env -> Jmt -> [GJmt]
dedukti2core env = map DMC.jmt2core . annotateDedukti env

annotateDedukti :: Env -> DkTree a -> [DkTree a]
annotateDedukti env t =
  maybe id take (nbestDedukti env)
    (allAnnotateDkIdents (constantTable env) t)

readConstantTable :: PGF -> FilePath -> IO ConstantTable
readConstantTable = buildConstantTable

checkConstantTable :: Module -> PGF -> ConstantTable -> String
checkConstantTable mo gr ct = unlines (constantTableErrors mo gr ct)

processDeduktiModule :: Env -> [Flag] -> Module -> [Result]
processDeduktiModule env flags (MJmts jmts) = map (processJmt env flags) jmts

processJmt :: Env -> [Flag] -> Jmt -> Result
processJmt env flags jmt = Result {
  origDedukti = jmt,
  annotDedukti = annotateDedukti env jmt
  }

printResult :: [Flag] -> Result -> String
printResult flags result = unlines $
  "{" :
  mkJSONField "origDedukti" (printTree (origDedukti result)) :
  mkJSONListField "annotDedukti" [printTree jmt | jmt <- annotDedukti result] :
  ["}"]

readDeduktiModule :: FilePath -> IO Module
readDeduktiModule file = readFile file >>= return . parseDeduktiModule

parseDeduktiModule :: String -> Module
parseDeduktiModule s = case pModule (myLexer s) of
  Bad e -> error ("parse error: " ++ e)
  Ok mo -> mo
    
readGFGrammar :: FilePath -> IO PGF
readGFGrammar = readPGF

core2dedukti :: GJmt -> [Jmt]
core2dedukti jmt = [] ----

checkJmt :: Jmt -> Bool
checkJmt jmt = True ----

core2ext :: GJmt -> [GJmt]
core2ext jmt = [jmt] ---- baseline

ext2core :: GJmt -> GJmt
ext2core jmt = jmt ---- to be revised

core2nat :: PGF -> Language -> GJmt -> String
core2nat pgf lang jmt = undefined ----

nat2core :: PGF -> Language -> String -> Maybe GJmt
nat2core pgf lang str = Nothing ----

ext2nat :: PGF -> Language -> GJmt -> Maybe String
ext2nat pgf lang jmt = Nothing ----

nat2ext :: PGF -> Language -> String -> [GJmt]
nat2ext pgf lang str = []

---- agda2dedukti :: AJmt -> Jmt
---- lean2dedukti :: LJmt -> Jmt
---- rocq2dedukti :: RJmt -> Jmt

-- these are syntactic conversions, therefore total
---- dedukti2agda :: Jmt -> AJmt
---- dedukti2lean :: Jmt -> LJmt
---- dedukti2rocq :: Jmt -> RJmt

---- checkAgda :: AJmt -> Bool
---- checkLean :: LJmt -> Bool
---- checkRocq :: RJmt -> Bool


readEnv :: [String] -> IO Env
readEnv args = do
  mo <- readDeduktiModule (argValue "-base" baseConstantFile args)
  gr <- readGFGrammar (argValue "-grammar" grammarFile args)
  ct <- readConstantTable gr (argValue "-constants" constantTableFile args)
  let nbd = argValueMaybeInt "-nbestdk" args
  ifArg "-check-constant-table" args (checkConstantTable mo gr ct)
  return Env {
    grammar = gr,
    constantTable = ct,
    baseConstantModule = mo,
    nbestDedukti = nbd
    }


-------------------------------------------
-- low level auxiliaries

argValue flag df args = case [f | f <- args, isPrefixOf flag (tail f)] of
  f:_ -> drop (length flag + 2) f   -- -<flag>=<value>
  _ -> df
  
argValueMaybeInt flag args = case argValue flag "nothing" args of
  v | all isDigit v -> Just (read v :: Int)
  _ -> Nothing

ifArg flag args msg = if elem flag args then putStrLn msg else return ()

inputFileArgs args = case [arg | arg <- args, head arg /= '-'] of
  [arg] -> Just (arg, fileSuffix arg)
  _ -> Nothing

fileSuffix = reverse . takeWhile (/= '.') . reverse

mkJSONField :: String -> String -> String
mkJSONField key value = show key ++ ": " ++ stringJSON value

mkJSONListField :: String -> [String] -> String
mkJSONListField key values =
  show key ++ ": " ++ "[" ++ concat (intersperse ", " (map stringJSON values)) ++ "]"

stringJSON = quote . escape where
  quote s = "\"" ++ s ++ "\""
  escape s = case s of
    c:cs | elem c "\"\\" -> '\\':c:escape cs
    '\n':cs -> '\\':'n':escape cs
    c:cs -> c:escape cs
    _ -> s

