{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Environment
import InformathAPI
import Utils (showFreqs, fileSuffix, dictValues)

---- import InformathServer --- TODO-server

import System.Environment (getArgs, getEnv)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeBaseName, takeFileName)
import Control.Monad (filterM)
import System.Process (readProcessWithExitCode)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified HtmlDoc as H
import Dedukti.AbsDedukti
import System.IO (stdout, stderr, hFlush, hPutStrLn)
import Dedukti.PrintDedukti (printTree)

main :: IO ()
main = do
  xx <- getArgs
----  if elem "-server" xx  --- TODO-server
----  then informathServer xx  --- TODO-server
----  else
  case invalidArgs xx of
    xs@(_:_) -> putStrLn ("invalid arguments: " ++ unwords xs ++ "; see -help")
    _ ->  main4 xx

main4 :: [String] -> IO ()
main4 args0 = if elem "-help" args0 then mapM_ putStrLn helpMsg4 else
  case (argValue "-from-formalism" "NONE" args, inputFileArg args) of
    ("NONE", _) -> readEnv args >>= \env -> runWith env args
    (formalism, Just (file, _)) -> fromFormalism formalism file args
    _ -> putStrLn "-from-formalism needs a source file"
 where
  -- an HTML document shows the languages of -to-lang
  args = if elem "-to-html-doc" args0 && not (flagHasValue "-languages" args0)
         then args0 ++ ["-languages=" ++ intercalate "," (htmlLanguages args0)] else args0

-- | -from-formalism: the source file is exported to Dedukti by a script in
-- scripts/<formalism>/, its identifiers are converted by the #CONV lines read
-- backwards, and the Dedukti is processed as a .dk file would be; with the symbol
-- tables made by the export (-hide.dkgf), into -export-dir or a temporary directory.
fromFormalism :: String -> FilePath -> [String] -> IO ()
fromFormalism formalism file args = do
  root <- getEnv "INFORMATH_ROOT"
  dir <- if flagHasValue "-export-dir" args then return (argValue "-export-dir" "." args)
         else freshExportDir file
  (dk, tables) <- exportFromFormalism root formalism file dir
  let proofsWanted = any (`elem` args) ["-proof-text", "-to-html-doc"]
      -- the proof texts as on the index pages, unless some of these is given
      pflags = words "-proof-indent -proof-goals -proof-merge-hypos -proof-no-cite"
      proofFlags = if proofsWanted && not (any (`elem` args) pflags) then pflags else []
      args1 = addSymbolTableFlags tables
                ([a | a <- args, a /= file, takeWhile (/= '=') a /= "-from-formalism"] ++ proofFlags)
  env0 <- readEnv args1
  mo <- readDeduktiModule [dk]
  let conv = fromFormalismConversion env0 formalism
      mo' = conv mo
      stem = take (length dk - 3) dk
  -- the base of the proof terms: natural deduction, the eliminators of Agda's data
  -- types, the constants used, and the statements (as propositions, if the export has them)
  env <- if not proofsWanted then return env0 else do
    prop <- doesFileExist (stem ++ "-prop.dk")
    extra <- filterM doesFileExist [root </> "share" </> "nd.dk", dir </> "eliminators.dk", stem ++ "-deps.dk",
                                    if prop then stem ++ "-prop.dk" else dk]
    MJmts more <- if null extra then return (MJmts []) else fmap conv (readDeduktiModule extra)
    let MJmts base = baseConstantModule env0
    return env0 { baseConstantModule = MJmts (base ++ more) }
  if proofsWanted then fromFormalismProofs env formalism file dir stem conv mo' args1 else do
    -- the converted module in a file of its own, if the conversion changed it
    dk' <- if mo' == mo then return dk else do
      let f = take (length dk - 3) dk ++ "-converted.dk"
      writeFile f (printTree mo')
      return f
    hPutStrLn stderr ("-from-formalism: " ++ dk' ++ (if null tables then "" else " with " ++ unwords tables))
    runWith env (args1 ++ [dk'])

-- | With -proof-text, the proof texts of the theorems; with -to-html-doc, an HTML
-- document of the declarations and their proofs. The proofs are the proof terms
-- of the export (<M>-proofs.dk), or its tactic proofs (<M>-tactics.json, Rocq) as
-- proof units by scripts/rocq/rocq-tactic-units.py.
fromFormalismProofs :: Env -> String -> FilePath -> FilePath -> FilePath -> (Module -> Module) -> Module -> [String] -> IO ()
fromFormalismProofs env formalism file dir stem conv (MJmts jmts) args = do
  root <- getEnv "INFORMATH_ROOT"
  let mod = takeBaseName stem
  hasTerms <- doesFileExist (stem ++ "-proofs.dk")
  terms <- if not hasTerms then return [] else do
    pm <- fmap conv (readDeduktiModule [stem ++ "-proofs.dk"])
    rs <- termProofTexts env pm
    sequence_ [hPutStrLn stderr ("no proof text: " ++ c ++ " (failed or over the time limit)") | (QIdent c, Nothing) <- rs]
    return [(c, l) | (QIdent c, Just l) <- rs]
  hasTactics <- doesFileExist (stem ++ "-tactics.json")
  tactics <- if not hasTactics then return [] else do
    _ <- readProcessWithExitCode "python3" [root </> "scripts" </> "rocq" </> "rocq-tactic-units.py", mod, dir] ""
    let units = dir </> (mod ++ "-tactic-units.json")
    ok <- doesFileExist units
    if not ok then return [] else do
      s <- readFile units
      native <- readNativeNames (stem ++ ".json")
      return [(M.findWithDefault n n native, l) | (n, l) <- tacticProofTexts env s]
  let proofs = terms ++ tactics
  if elem "-to-html-doc" args then do
    sources <- readSourceInfo (stem ++ ".json")
    let title = takeBaseName file
        intro = "Informalized by RunInformath from " ++ takeFileName file ++ " (-from-formalism=" ++ formalism ++
                "): its declarations exported to Dedukti, read in " ++ intercalate ", " (map languageNameOf (htmlLanguages args)) ++
                " and as formulas, and the proofs of its theorems as proof texts."
    page <- htmlDocument env title intro (Just formalism) sources (M.fromList proofs)
              (includedDefs env jmts)
    putStr page
  else putStr (proofTextDocument env (map snd proofs))
 where
  languageNameOf = H.languageName

runWith :: Env -> [String] -> IO ()
runWith env args = do
  let mfile = inputFileArg args
  case mfile of
    Just (file, "dk") | any (flip elem args) ["-idents", "-unknown-idents"] -> do
      mo <- readDeduktiModule [file]
      mapM_ putStrLn (showFreqs (identsInDedukti env mo)) 
    Just (file, "json") | elem "-tactic-proof" args -> do
      s <- readFile file
      putStrLn (showTacticProofs env s)
    Just (file, "dk") | elem "-check-nlg-semantics" args -> do
      mo <- readDeduktiModule [file]
      checkNLGSemantics env mo >>= putStr
    Just (file, "dk") | elem "-proof-text" args -> do
      mo <- readDeduktiModule [file]
      let base = baseConstantModule env
      showProofDemoSafe env base mo >>= putStrLn
    Just (file, "dk") | elem "-to-html-doc" args -> do
      MJmts jmts <- readDeduktiModule [file]
      page <- htmlDocument env (takeBaseName file)
                ("Informalized by RunInformath from " ++ takeFileName file ++ ".") Nothing M.empty M.empty
                (includedDefs env jmts)
      putStr page
    Just (file, "dk") | elem (toFormalism env) ["agda", "lean", "rocq"] -> do
      mo <- readDeduktiModule [file]
      mapM_ putStrLn (printFormalismModule env (toFormalism env) mo)
    Just (file, "dk") -> do
      mo <- readDeduktiModule [file]
      let results = processDeduktiModule env mo
      mapM_ putStrLn (printResults env (concatMap (printGenResult env) results))
    Just (file, "dktex") -> do
      ss <- readFile file >>= return . lines
      mapM_ putStrLn (transEmbeddedDedukti env ss)
    Just (file, "gft") -> do
      ss <- readFile file >>= return . filter (not . null) . lines
      let results = map (processGFTree env . readGFtree) ss
      mapM_ putStrLn (printResults env (concatMap (printGenResult env) results))
    Just (file, txt) | elem txt ["tex", "txt", "md"] && elem "-unknown-words" args -> do
      s <- readFile file 
      mapM_ putStrLn (showFreqs (unknownWordsInTex env s))
    Just (file, txt) | elem txt ["dkgf"] && elem "-unknown-words" args -> do
      s <- readFile file 
      mapM_ putStrLn (showFreqs (unknownWordsInTex env (unlines (dictValues s))))
    Just (file, txt) | elem txt ["tex", "txt", "md"] -> do
      s <- readFile file 
      results <- typecheckParseResults env (processLatex env s)
      mapM_ putStrLn (printResults env (concatMap (printParseResult env) results))
    Just (file, "dkgf") | any (flip elem args) ["-try-symboltable", "-keep-ok-entries"] -> do
      ss <- readFile file >>= return . lines
      let rs = tryParseSymbolTable env ss
      mapM_ putStrLn rs
    Just (file, "dkgf") -> do
      st <- readSymbolTable (grammar env) (fromLang env) [file]
      putStrLn (printSymbolTable st)
      putStrLn (unlines ["## " ++ line |
        line <- checkSymbolTable (baseConstantModule env) (grammar env) st])
      
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
      mapM_ (putStrLn . unlines . parseFunExample env) (lines s)
    Nothing | elem "-formalize" args -> do
      s <- getContents 
      results <- typecheckParseResults env (processLatex env s)
      mapM_ putStrLn (printResults env (concatMap (printParseResult env) results))
    Nothing | elem "-from-gf-trees" args -> do
      ss <- getContents >>= return . filter (not . null) . lines
      let results = map (processGFTree env . readGFtree) ss
      mapM_ putStrLn (printResults env (concatMap (printGenResult env) results))
    Nothing -> do
      mo <- getContents >>= return . parseDeduktiModule
      let results = processDeduktiModule env mo
      mapM_ putStrLn (printResults env (concatMap (printGenResult env) results))
      
    _ -> mapM_ putStrLn helpMsg4 

helpMsg4 :: [String]
helpMsg4 = [
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
  just ".dktex" "convert embedded Dedukti code in begin/end{dedukti} environments",
  just ".gft" "read GF trees line by line, informalize or -to-formalism=dedukti|...",
  just ".tex|.txt|.md" "parse line by line and convert to Dedukti or another formalism",
  just ".agda|.lean|.v" "with -from-formalism, export to Dedukti and convert as a .dk file",
  "",
  "Output is written to standard output.",
  "Input is read line by line, except for .dk files",
  "Options: ",
  "",
  "* Source files for building the environment:",
  "",
  just "-base=<file.dk>+" ("base Dedukti constants, default " ++ baseConstantFile),
  just "-symboltables=<file.dkgf>+" ("map from Dedukti to GF, replacing the default " ++ constantTableFile),
  just "-add-symboltables=<file.dkgf>+" ("map from Dedukti to GF,  added to " ++ constantTableFile),
  just "-grammar=<file.pgf>" ("GF grammar used, default " ++ engGrammarFile ++ " or (if -to-lang or -for-lang or -symboltable-lang is not Eng) " ++ fullGrammarFile),
  just "-symboltable-lang=<lang>" "the language in which symbol table is parsed, default Eng",
  "",
  "* Translating from Agda, Lean and Rocq (source files exported to Dedukti by scripts/<formalism>/):",
  "",
  just "-from-formalism=<formalism>" "read the file as agda, lean or rocq, exported to Dedukti and then read as a .dk file; its identifiers converted by the #CONV lines of the symbol tables, read backwards; with -proof-text, the proof texts of its theorems (proof terms of Lean and Agda, tactic proofs of Rocq)",
  just "-proof-timeout=<seconds>" "with -from-formalism and -proof-text or -to-html-doc, the time limit of each proof text, default 30",
  just "-export-dir=<dir>" "with -from-formalism, write the export (the .dk file, its symbol table of hidden arguments, ...) in <dir>, default a temporary directory",
  "",
  "* Translating from Dedukti:",
  "",
  just "-synonyms=<int>" "the maximum number of verbal constant synonyms considered with -variations (default all) otherwise 1",
  just "-symbolics=<int>" "the maximum number of symbolic constant synonyms considered with -variations, (default all) otherwise 1",
  just "-max-annotations=<int>" "the maximum number of symbolic/verbal mixes kept of each subformula, default 256 (all with -variations); 0 = all",
  just "-mathcore" "generate only the mathcore text",
  just "-variations" "show all variations",
  just "-nbest=<int>" "show <int> best NLG results, default show all",
  just "-sampling=<int>" "sampling factor of NLG results before ranking, default 2 (take every 2nd)", 
  just "-more-variants" "generate some more NLG variants",
  just "-roundtrip-safe" "only NLG steps whose loss of information the semantics restores (doc/nlg-and-semantics.md, Part 5)",
  just "-trees" "with each natural language result, print the constant and the GF tree, separated by tabs",
  just "-godement" "generate also Godement-style variants (said to be, typed variables, ...)",
  just "-force-symbolic" "use maximally symbolic variants, even if this needs verbatim Dedukti constants",
  just "-to-latex-doc" "print valid LaTeX doc with preamble",
  just "-to-html-doc" "print an HTML document: each declaration with its Dedukti, its readings in the languages of -to-lang (a list: -to-lang=Eng,Fre), its formula, and with -from-formalism its source and the proof texts of theorems, with folds",
  just "-weights=<ints>" "weights of scores, default 1,1,1,1,1,1,1",
  just "-no-ranking" "do not rank the NLG results (which can be expensive)",
  just "-test-ambiguity" "test ambiguity when ranking NLG results (can be very slow)",
  just "-parallel-data" "print complete parallel data in jsonl",
  just "-proof-text" "print proof texts (experimental), needs -base=<rules>.dk",
  just "-check-nlg-semantics" "check that the semantics of each NLG variant of each statement includes the statement (doc/nlg-and-semantics.md)",
  just "-tactic-proof" "print proof texts of proofs given as lists of proof units in a .json file (from tactic proofs)",
  just "-proof-terms" "with -proof-text, also show the proof terms and their Dedukti lines",
  just "-proof-text-dedukti" "with -proof-text, also show the lines of the proof text with their formulas in Dedukti",
  just "-proof-trees" "with -proof-text, also show the proofs as natural deduction trees (bussproofs)",
  just "-proof-indent" "with -proof-text, indent each line by the assumptions open at it",
  just "-proof-goals" "with -proof-text, state the goal of each assumption: to prove that B, assume that A",
  just "-typing" "typings as in type theory: let x : A, then c : T, or then there is an identification c : T if the type family of T has #TYPENOUN in the symbol table",
  just "-statement-labels" "keep the label of a hypothesis that the statement refers to: assume that x = y (p), as of an identification p",
  just "-show-implicit-hypotheses" "introduce the variables of the types that the symbol table declares #IMPLICIT, in statements and proof texts",
  just "-proof-no-cite" "with -proof-text, name no theorem in the text (\"by ex25_set, B\"): the margin cites every rule",
  just "-proof-merge-hypos" "with -proof-text, the variables and hypotheses of a scope in one line: let x be an A such that B",
  just "-to-lang=<lang>" "linearize to natural language <lang>, default Eng",
  just "-languages=<langs>" "with -json, rank and show only these languages (e.g. Eng,Fre), default all; with -proof-text, a proof text in each",
  just "-to-formalism=<formalism>" "convert to <formalism> instead of natural language",
  just "-to-symbolic-latex" "convert to standard logical notation in LaTeX, with the symbolic notations of the symbol tables",
  "",
  "* Translating from informal language (line by line):",
  "",
  just "-from-lang=<lang>" "parse from <lang>, default Eng",
  just "-translate" "translate text without parsing parts in $...$",
  just "-translate-core" "print each line followed by the verbalizations of its MathCore semantics",
  just "-typecheck" "keep only the readings whose Dedukti type-checks with dk, on top of -base",
  just "-parse-only" "return GF syntax trees, also parsing the parts in $...$",
  just "-include-unreachable" "include trees with functions unreachable from symbol table",
  just "-unknown-words" "show words in text file not in grammar",
  just "-find-gf" "shows GF functions that match each word in standard input",
  just "-linearize" "linearize GF trees given line by line in standard input",
  just "-from-gf-trees" "treat input as GF trees (also combinable with -to-formalism=dedukti)",
  just "-all-gf-functions" "show all GF functions with their types",
  just "-parse-example" "parse example candidate lexical item (includes unreachables)",
  just "-try-symboltable" "try to build a symbol table from a .dkgf file, report errors line by line",
  just "-keep-ok-entries" "print a symbol table with just the OK entries", 
  just "-failures" "show lines that fail to parse",
  "",
  "* General output options:",
  "",
  just "-json" "show full information in jsonl (same as -v)",
  just "-v" "show full information in jsonl (same as -json)",
  just "-vs" "show a bit less than full information in jsonl",
  just "-no-back-to-dedukti" "with -json, leave out the translation of the readings back to Dedukti",
  just "-no-unlex" "linearize to tokens separated by spaces",
  just "-dedukti-tokens" "print Dedukti code with tokens separated by spaces",
  just "-help" "show this help message",
  just "-debug" "show debugging information (for Haskell developers)",
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

invalidArgs :: [[Char]] -> [[Char]]
invalidArgs xx =
  [x | x@('-':_) <- xx, notElem (takeWhile (/='=') x) validOptions] ++
  [x | x <- xx, head x /= '-', notElem (fileSuffix x) validFileSuffixes]
 where
  validOptions = [takeWhile (/='=') o | o:_ <- map words helpMsg4]
  validFileSuffixes = words "dk dkgf dktex gft tex txt md json agda lean v"

loopInformath :: Env -> IO b
loopInformath env = do
  putStr "> "
  hFlush stdout
  s <- getLine
  case s of
    '?':cs -> do
      results <- typecheckParseResults env (processLatex env cs)
      mapM_ putStrLn (printResults env (concatMap (printParseResult env) results))
    _ -> do
      let mmo = parseDeduktiModuleErrorFree s
      let results = maybe [] (processDeduktiModule env) mmo
      mapM_ putStrLn (printResults env (concatMap (printGenResult env) results))
  loopInformath env

