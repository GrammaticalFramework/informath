{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Environment
import InformathAPI
import Utils (showFreqs)

---- import InformathServer --- TODO-server

import System.Environment (getArgs)
import System.IO (stdout, hFlush)

main = do
  xx <- getArgs
----  if elem "-server" xx  --- TODO-server
----  then informathServer xx  --- TODO-server
----  else
  main4 xx 

main4 args = if elem "-help" args then putStrLn helpMsg4 else do
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
    Just (file, txt) | elem txt ["tex", "txt", "md"] && elem "-show-functions" args -> do
      s <- readFile file 
      mapM_ putStrLn (showFreqs (unknownWordsInTex env s))
    Just (file, txt) | elem txt ["tex", "txt", "md"] -> do
      s <- readFile file 
      let results = processLatex env s
      mapM_ putStrLn (printResults env (concatMap (printParseResult env) results))
    Just (file, "dkgf") -> do
      (ct, _, _, mt) <- readConstantTable (grammar env) [file]
      putStrLn (printConstantTable ct)
      putStrLn (checkConstantTable (baseConstantModule env) (grammar env) mt ct)
      
    Nothing | elem "-loop" args -> do
      loopInformath env
    Nothing | elem "-find-gf" args -> do
      s <- getContents
      mapM_ putStrLn [unwords (w : ":" : fs) | (w, fs) <- map (findGFFunctions env) (words s)]
    Nothing | elem "-linearize" args -> do
      s <- getContents
      mapM_ (putStrLn . readGFtree2nat env) (lines s)
    Nothing | elem "-all-gf-functions" args -> do
      mapM_ putStrLn (showGFFunctions env)
    Nothing | elem "-parse-example" args -> do
      s <- getContents
      mapM_ putStrLn (parseFunExample env s)
    Nothing | elem "-formalize" args -> do
      s <- getContents 
      let results = processLatex env s
      mapM_ putStrLn (printResults env (concatMap (printParseResult env) results))
    Nothing -> do
      mo <- getContents >>= return . parseDeduktiModule
      let results = processDeduktiModule env mo
      mapM_ putStrLn (printResults env (concatMap (printGenResult env) results))
      
    _ -> putStrLn helpMsg4 

helpMsg4 = unlines [
  "usage: RunInformath <option>* <file>.(dk|dkgf|tex|txt|md|...)*",
  "",
  "If no file is given, read standard input and process as following:",
  just "-formalize" "formalize the string like a .txt or .tex file",
  just "-loop" "start a loop with input in either dedukti or '?' followed by tex",
  just "[no flag]" "informalize the string like a .dk file",
  "If a file is given, do the following depending on the file suffix:",
  "",
  just ".dk" "convert to natural language or to another formalism",
  just ".dkgf" "check the consistency of Dedukti to GF mapping",
  just ".tex|.txt|.md" "parse line by line and convert to Dedukti or another formalism",
  "",
  "Output is written to standard output.",
  "Input is read line by line, except for .dk files",
  "Options: ",
  "",
  "* Source files for building the environment:",
  "",
  just "-base=<file.dk>+" ("base Dedukti constants, default " ++ baseConstantFile),
  just "-symboltables=<file.dkgf>+" ("map from Dedukti to GF, replacing the default " ++ constantTableFile),
  just "-add-symboltables=<file.dkgf>+" ("map from Dedukti to GF,  added to" ++ constantTableFile),
  just "-grammar=<file.pgf>" ("GF grammar used, default " ++ grammarFile),
  "",
  "* Translating from Dedukti:",
  "",
  just "-mathcore" "generate only the mathcore text",
  just "-variations" "show all variations",
  just "-nbest=<int>" "show <int> best NLG results",
  just "-to-latex-doc" "print valid LaTeX doc with preamble",
  just "-weights=<ints>" "weights of scores, default 1,1,1,1,1,1,1",
  just "-no-ranking" "do not rank the NLG results (which can be expensive)",
  just "-test-ambiguity" "test ambiguity when ranking NLG results (can be very slow)",
  just "-parallel-data" "print complete parallel data in jsonl",
  just "-to-lang=<lang>" "linearize to natural language <lang>",
  just "-to-formalism=<formalism>" "convert to <formalism> instead of natural language",
  "",
  "* Translating from informal language (line by line):",
  "",
  just "-from-lang=<lang>" "parse from <lang>",
  just "-translate" "translate text without parsing parts in $...$",
  just "-unknown-words" "show words in text file not in grammar",
  just "-find-gf" "shows GF functions that match each word in standard input",
  just "-linearize" "linearize GF trees given line by line in standard input",
  just "-all-gf-functions" "show all GF functions with their types",
  just "-parse-example" "parse example candidate lexical item",
  just "-failures" "show lines that fail to parse",
  just "-macroidents" "allow backslash in math mode identifiers (can be expensive, to be fixed)",
  "",
  "* General output options:",
  "",
  just "-json" "show full information in jsonl (same as -v)",
  just "-v" "show full information in jsonl (same as -json)",
  just "-no-unlex" "linearize to tokens separated by spaces",
  just "-dedukti-tokens" "print Dedukti code with tokens separated by spaces",
  just "-help" "show this help message",
  "",
  "* Analysing and converting Dedukti:",
  "",
  just "-idents" "show frequency table of non-variable idents in .dk file",
  just "-unknown-idents" "show idents in .dk file not in constant table",
  just "-drop-definitions" "drop definiens parts of judgements",
  just "-drop-qualifs" "drop qualifiers of identifiers",
  just "-hide-arguments" "hide arguments in accordance with the constant table",
  just "-peano2int" "convert succ/0 expressions to sequences of digits"
  ]
 where
   just opt expl = concat ["  ", opt, replicate (28 - length opt) ' ', expl]


loopInformath env = do
  putStr "> "
  hFlush stdout
  s <- getLine
  case s of
    '?':cs -> do
      let results = processLatex env cs
      mapM_ putStrLn (printResults env (concatMap (printParseResult env) results))
    _ -> do
      let mmo = parseDeduktiModuleErrorFree s
      let results = maybe [] (processDeduktiModule env) mmo
      mapM_ putStrLn (printResults env (concatMap (printGenResult env) results))
  loopInformath env

