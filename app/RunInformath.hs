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
      mapM_ (putStrLn . printGenResult env) results
    Just (file, "tex") -> do
      s <- readFile file
      let results = processLatex env s
      mapM_ (putStrLn . printParseResult env) results
    _ -> putStrLn helpMsg4


helpMsg4 = unlines [
  "usage: RunInformath -next <option>* <file>.(dk|tex)",
  "Options: ",
  "  -variations",
  "  -json",
  "  -nbestdk=<int>",
  "  -nbest=<int>",
  "  -tolang=<lang>",
  "  -fromlang=<lang>"
  ]
 