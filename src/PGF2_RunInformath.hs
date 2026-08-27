{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Core2Dedukti
import Dedukti2Core
import Dedukti.PrintDedukti
import Dedukti.ParDedukti
import Dedukti.AbsDedukti
import Dedukti.ErrM
import Informath -- superset of Core
import Core2Informath (nlg)
import Informath2Core (semantics)
import Lexing (lextex)
import ParseInformath (parseJmt)
import PGF2

import Data.List (partition)
import System.Random
import System.Environment (getArgs)

informathPGFFile = "grammars/Informath.pgf"
Just jmt = readType "Jmt"

data Env = Env {
 flags :: [String],
 cpgf :: PGF,
 eng  :: Concr
 }
--- random generation disabled for the time being

ifFlag x env = elem x (flags env)

main = do
  xx <- getArgs
  let (ff, yy) = partition ((== '-') . head) xx
  corepgf <- readPGF informathPGFFile
  let Just eng = Data.Map.lookup "InformathEng" (languages gr)
  let env = Env{flags = ff, cpgf = corepgf, eng = eng} 
  case yy of
    filename:_ -> do
      s <- readFile filename
      processDeduktiModule env s
    _ -> do
      g <- getStdGen
      let rs = generateRandomDepth g corepgf jmt (Just 4)
      loop env rs 0 ---- storing rs in env causes an infinite loop

loop :: Env -> [Expr] -> Int -> IO ()
loop env rs i = do
  putStr "> "
  ss <- getLine
  case ss of
    '?':s -> processCoreJmt env s
    "gr"  -> processCoreJmtTree env (rs !! i)
    '=':s -> roundtripDeduktiJmt env s
    _     -> processDeduktiJmt env ss
  loop env rs (i + 1)

processDeduktiModule :: Env -> String -> IO ()
processDeduktiModule env s = do
  case pModule (myLexer s) of
    Bad e -> putStrLn ("error: " ++ e)
    Ok (MJmts jmts) -> flip mapM_ jmts $ processDeduktiJmtTree env

processDeduktiJmt :: Env -> String -> IO ()
processDeduktiJmt env cs = do
  case pJmt (myLexer cs) of
    Bad e -> putStrLn ("error: " ++ e)
    Ok t -> processDeduktiJmtTree env t

roundtripDeduktiJmt :: Env -> String -> IO ()
roundtripDeduktiJmt env cs = do
  let gr = cpgf env
  let english = eng env
  case pJmt (myLexer cs) of
    Bad e -> putStrLn ("error: " ++ e)
    Ok t -> do
      putStrLn $ "#Dedukti: " ++ show t
      let gft = gf $ jmt2core t
      putStrLn $ "#Core: " ++ showExpr [] gft
      let lin = linearize english gft
      processCoreJmt env lin

processDeduktiJmtTree :: Env -> Jmt -> IO ()
processDeduktiJmtTree env t = do
  let gr = cpgf env
  let english = eng env
  putStrLn $ "#Dedukti: " ++ show t
  let ct = jmt2core t
  let gft = gf ct
  putStrLn $ "#Core: " ++ showExpr [] gft
  putStrLn $ linearize english gft
  convertCoreToForthel env ct

convertCoreToForthel :: Env -> GJmt -> IO ()
convertCoreToForthel env ct = do
  let fgr = cpgf env
  let english = eng env
  let fts = nlg ct
  let gffts = map gf fts
  flip mapM_ gffts $ \gfft -> do
    putStrLn $ "#Informath: " ++ showExpr [] gfft
    putStrLn $ linearize english gfft

processCoreJmt :: Env -> String -> IO ()
processCoreJmt env s = do
  let gr = cpgf env
  let english = eng env
  let ls = lextex s
  print ls
  let (mts, mst) = parseJmt english (startCat gr) ls
  print (length ts)
  case ts of
    [] -> putStrLn ("NO PARSE: " ++ ls)
    _:tt -> do
      if (length tt > 0) then (putStrLn "#AMBIGUOUS") else return ()
      flip mapM_ ts $ processCoreJmtTree env

processCoreJmtTree :: Env -> Expr -> IO ()
processCoreJmtTree env t = do
  let gr = cpgf env
  let english = eng env
  putStrLn $ "#Core: " ++ showExpr [] t
  putStrLn $ linearize english t
  let d = jmt2dedukti (fg t)
  putStrLn $ printTree d

