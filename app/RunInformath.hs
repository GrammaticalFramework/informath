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
  let mfile = inputFileArg args
  case mfile of
    Just (file, "dk") | any (flip elem args) ["-idents", "-unknown-idents"] -> do
      mo <- readDeduktiModule [file]
      mapM_ putStrLn (showFreqs (identsInDedukti env mo)) 
    Just (file, "dk") -> do
      mo <- readDeduktiModule [file]
      let results = processDeduktiModule env mo
      mapM_ putStrLn (printResults env (concatMap (printGenResult env) results))
    Just (file, txt) | elem txt ["tex", "txt", "md"] && elem "-unknown-words" args -> do
      s <- readFile file 
      mapM_ putStrLn (showFreqs (unknownWordsInTex env s))
    Just (file, txt) | elem txt ["tex", "txt", "md"] -> do
      s <- readFile file 
      let results = processLatex env s
      mapM_ putStrLn (printResults env (concatMap (printParseResult env) results))
    Just (file, "dkgf") -> do
      (ct, _) <- readConstantTable (grammar env) [file]
      putStrLn (printConstantTable ct)
      putStrLn (checkConstantTable (baseConstantModule env) (grammar env) ct)
    _ -> putStrLn helpMsg4 


helpMsg4 = unlines [
  "usage: RunInformath -next <option>* <file>.(dk|dkgf|tex|txt|md|...)",
  "",
  "Depending on file suffix, the following is done:",
  just ".dk" "convert to natural language or to another formalism",
  just ".dkgf" "check the consistency of Dedukti to GF mapping",
  just ".tex|.txt|.md" "parse and convert to Dedukti or another formalism",
  "Output is written to standard output.",
  "Input is read line by line, except for .dk files",
  "Options: ",
  just "-base <file.dk>+" ("base Dedukti constants, default " ++ baseConstantFile),
  just "-constants <file.dkgf>+" ("map from Dedukti to GF, default " ++ constantTableFile),
  just "-grammar <file.pgf>" ("GF grammar used, default " ++ grammarFile),
  just "-mathcore" "generate only the mathcore text",
  just "-json" "show full information in jsonl",
  just "-nbest=<int>" "show <int> best NLG results",
  just "-to-lang=<lang>" "linearize to <lang>",
  just "-from-lang=<lang>" "parse from <lang>",
  just "-to-formalism=<formalism>" "convert to <formalism>",
  just "-translate" "translate text without parsing parts in $...$",
  just "-no-unlex" "linearize to tokens separated by spaces",
  just "-to-latex-doc" "print valid LaTeX doc with preamble",
  just "-variations" "show all variations",
  just "-parallel-data" "print parallel data in jsonl",
  just "-idents" "show frequency table of non-variable idents in .dk file",
  just "-unknown-idents" "show idents in .dk file not in constant table",
  just "-unknown-words" "show idents in text file not in grammar",
  just "-test-ambiguity" "test ambiguity when ranking NLG results"
  ]
 where
   just opt expl = concat ["  ", opt, replicate (28 - length opt) ' ', expl]
  


