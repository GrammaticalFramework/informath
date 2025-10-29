{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- top-level conversions between formats

module TopConversions where

import Core2Dedukti (jmt2dedukti)
import Dedukti2Core
import Environment
import Dedukti.PrintDedukti
import Dedukti.ParDedukti
import Dedukti.AbsDedukti
import Dedukti.ErrM
import DeduktiOperations (
  identsInTypes, dropDefinitions, stripQualifiers, identTypes, ignoreCoercions,
  ignoreFirstArguments, eliminateLocalDefinitions, peano2int, applyConstantData, deduktiTokens)
import ConstantData (
  ConstantData, extractConstantData, lookBackConstantData, extractTargetConversions, convInfo, coercionFunctions)
import SpecialDeduktiConversions (specialDeduktiConversions)
----import Informath -- to be removed
import Core2Informath (nlg)
import Informath2Core (semantics)
import ParseInformath (parseJmt)
import Lexing
import MkConstants (mkConstants)
import qualified Dedukti2Agda as DA
import qualified Dedukti2Rocq as DR
import qualified Dedukti2Lean as DL
import Ranking

import BuildConstantTable -- next version
import qualified DMC
import qualified MCI
import NextInformath

import PGF

import Data.List (partition, isSuffixOf, isPrefixOf, intersperse, sortOn)
----import System.Random
import Data.Char (isDigit, toUpper)
import System.Environment (getArgs)
import System.IO
import qualified Data.Map as M


dedukti2core :: Jmt -> GJmt
dedukti2core = DMC.jmt2core

core2dedukti :: GJmt -> Jmt
core2dedukti jmt = undefined ----

checkJmt :: Jmt -> Bool
checkJmt jmt = True ----

core2ext :: GJmt -> [GJmt]
core2ext jmt = [jmt] ---- baseline

ext2core :: GJmt -> [GJmt]
ext2core jmt = [] ---- to be revised

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


