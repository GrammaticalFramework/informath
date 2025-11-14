{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Informath3 (main3) ---- to be deprecated

import Environment4
import InformathAPI

import System.Environment (getArgs) 

main = do
  xx <- getArgs
  if elem "-next" xx
  then main4 xx
  else main3 xx

main4 args = do
  env <- readEnv args 
  let mfile = inputFileArgs args
  case mfile of
    Just (file, "dk") -> do
      mo <- readDeduktiModule file
      let results = processDeduktiModule env mo
      mapM_ putStrLn (printResults env (concatMap (printGenResult env) results))
    Just (file, txt) | elem txt ["tex", "txt", "md"] -> do
      s <- readFile file 
      let results = processLatex env s
      mapM_ putStrLn (printResults env (concatMap (printParseResult env) results))
    Just (file, "dkgf") -> do
      (ct, _) <- readConstantTable (grammar env) file
      putStrLn (printConstantTable ct)
      putStrLn (checkConstantTable (baseConstantModule env) (grammar env) ct)
    _ -> putStrLn helpMsg4 
  

helpMsg4 = unlines [
  "usage: RunInformath -next <option>* <file>.(dk|tex|dkgf)",
  "Options: ", 
  "  -mathcore",
  "  -json",
  "  -nbest=<int>",
  "  -to-lang=<lang>",
  "  -from-lang=<lang>",
  "  -to-formalism=<formalism>",
  "  -translate",
  "  -no-unlex",
  "  -to-latex-doc",
  "  -variations"
  ]
  


