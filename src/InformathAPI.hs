{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{-| This file defines top-level conversions between different formats.
The main directions are from Dedukti to Agda, Lean, and Rocq on the formal side
and English, French, German, and Swedish on the natural language side.
Also translations directly between natural languages are possible.
-}

module InformathAPI where

import Dedukti.PrintDedukti
import Dedukti.ParDedukti
import Dedukti.AbsDedukti
import Dedukti.ErrM
import DeduktiOperations
import ParseInformath (parseJmt, unindexGFTrees)
import Lexing (lextex, indexTex, unlextex)
import qualified Dedukti2Agda as DA
import qualified Dedukti2Rocq as DR
import qualified Dedukti2Lean as DL
import DeduktiRules (emptyRuleContext)

import Ranking
import Environment
import BuildConstantTable
import AnnotateDedukti (harmonizeJmt, annotateDkIdents, DkTree)
import qualified Dedukti2MathCore as DMC
import qualified MathCore2Informath as MCI
import qualified Informath2MathCore as IMC
import qualified MathCore2Dedukti as MCD
import qualified NLGCheck as NC
import Semantics (appNLGDefs)
import Utils (commaSepInts, setnub, unindexString)
import Utils (encodeJSON, mkJSONObject, mkJSONField, stringJSON, mkJSONListField)
import Utils (toLatexDoc, transInEnv, frequencyTable, escapeTextUnderscores)

import ProofText(proofDemo, proofDemoFile, proofDemoPieces, prLatexFile, verbatimInline, dropImplicitHypos, implicitPositions, hypoVarsAt)

import Informath
import PGF

import Data.List (intersperse, sortOn, nub, isSuffixOf, intercalate)
import Data.Char (isDigit)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.JSON (JSValue)
import qualified Text.JSON as J
import System.Environment(getEnv)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (getTemporaryDirectory, removeFile, createDirectoryIfMissing, listDirectory, doesFileExist)
import System.IO (openTempFile, hClose, hPutStr, stderr)
import System.FilePath ((</>), takeBaseName, dropExtension)
import Control.Exception (try, evaluate, SomeException)
import Control.Monad (foldM, forM)
import System.Timeout (timeout)
import qualified HtmlDoc as H


-- * The environment

{- |
The environment, of type Env, is a large record of data and methods that affect
the conversions and the way they are displayed. It is defined in the module Environment,
which is not exported by the API. Most of the functions here presuppose an Env, which
is read using a list of flags. This list can empty, which results in a default environment.

Flags are strings of the forms -<option> of -<flag>=<value>.
The actual flags can be seen with RunInformath -help.
The same flags can be written in the [Flag] list when calling readEnv as an API function.
-}

informathRootVar :: String
informathRootVar = "INFORMATH_ROOT"

readEnv :: [Flag] -> IO Env
readEnv args = do
  root <- getEnv informathRootVar
  mo <- readDeduktiModule (argValues "-base" (root ++ "/" ++ baseConstantFile) args)
  gr <- readGFGrammar (argValue "-grammar" (root ++ "/" ++ grammarFile args) args)
  let symboltables =
        if flagHasValue "-add-symboltables" args
        then (root ++ "/" ++ constantTableFile) : argValues "-add-symboltables" "" args
        else argValues "-symboltables" (root ++ "/" ++ constantTableFile) args
  let fro = mkLanguage gr (argValue "-from-lang" english args)
  let sym = mkLanguage gr (argValue "-symboltable-lang" english args)
  symt <- readSymbolTable gr sym symboltables
  ifArg "-check-constant-table" args (unlines (checkSymbolTable mo gr symt))
  return Env {
    flags = args,
    informathRoot = root,
    grammar = gr,
    symbolTable = symt,
    reachableFunctions = reachableGFFunctions (backConstantTable symt),
    baseConstantModule = mo,
    formalisms = words "agda dedukti lean rocq",
    -- the languages ranked and shown: those of -languages, or all but LaTeX
    langs = if flagHasValue "-languages" args
            then relevantLanguages gr args
            else filter (/= latexLanguage) (languages gr),
    -- the first of -to-lang, which can be a list for -to-html-doc
    toLang = mkLanguage gr (head (argValues "-to-lang" english args ++ [english])),
    toFormalism = argValue "-to-formalism" "NONE" args,
    fromLang = fro,
    symbolTableLang = sym,
    nbestNLG = argValueMaybeInt "-nbest" args,
    scoreWeights = commaSepInts (argValue "-weights" "1,1,1,1,1,1,1,1,1" args),
    samplingFactor = read (argValue "-sampling" "2" args), 
    morpho = buildMorpho gr fro
    }

-- | The symbolic LaTeX concrete syntax, which is not a natural language:
-- it linearizes only MathCore, and is used with -to-symbolic-latex.
latexLanguage :: Language
latexLanguage = mkCId (informathPrefix ++ "Latex")

-- | direct access to parts of the symbol table

constantTableEnv :: Env -> ConstantTable
constantTableEnv = constantTable . symbolTable
backConstantTableEnv :: Env -> BackConstantTable
backConstantTableEnv = backConstantTable . symbolTable
conversionTableEnv :: Env -> ConversionTable
conversionTableEnv = conversionTable . symbolTable
dropTableEnv :: Env -> DropTable
dropTableEnv = dropTable . symbolTable
macroTableEnv :: Env -> MacroTable
macroTableEnv = macroTable . symbolTable
semanticsTableEnv = semanticsTable . symbolTable
nlgTableEnv = nlgTable . symbolTable
builtinSetEnv :: Env -> S.Set QIdent
builtinSetEnv = builtinSet . symbolTable

-- ** Low-level access to data sources

{- | The most important data sources are a GF grammar (file .pgf)
and a ConstantTable (file .dkgf).
Both of these can be customized and passed as values of flags.
The following functions read them directly, but need hardly be called explicitly.
-}

-- | To read the GF grammar from a .pgf file.    
readGFGrammar :: FilePath -> IO PGF
readGFGrammar = readPGF

-- | To read a symbol table from a .dkgf file. 
readSymbolTable :: PGF -> Language -> [FilePath] -> IO SymbolTable
readSymbolTable pgf lang dkgfs = do
  ls <- mapM readFile dkgfs >>= return . concatMap lines 
  return $ buildSymbolTable pgf lang ls

-- | To construct a concrete syntax name from a 3-letter language code.
mkLanguage :: PGF -> String -> Language
mkLanguage pgf code = case readLanguage (informathPrefix ++ code) of
  Just lang | elem lang (languages pgf) -> lang
  _ -> error ("not a valid language: " ++ code)

-- * Default source files, which can be changed in Env flags (see RunInformath -help)

engGrammarFile = "share/InformathEng.pgf" 
fullGrammarFile = "share/InformathFull.pgf" 
baseConstantFile = "share/baseconstants.dk"  
constantTableFile = "share/baseconstants.dkgf"

-- | Select the English-only grammar (default) or the full grammar (if -from-lang, -to-lang
--   or -symboltable-lang is other than Eng, or -languages has another language than Eng).
--   This can be overridden with the @-grammar=\<file\>.pgf@ flag.
grammarFile :: [Flag] -> FilePath
grammarFile args = case (argValue "-from-lang" english args,
                         argValue "-to-lang" english args,
                         argValue "-symboltable-lang" english args,
                         argValues "-languages" english args) of
  ("Eng", "Eng", "Eng", ls) | all (== "Eng") ls -> engGrammarFile
  _ -> fullGrammarFile

-- * Main types involved

-- GF abstract syntax tree: from BuildConstantTable
-- type GFTree = Expr

-- | Dedukti judgement
type DkJmt = Jmt   

-- * Main conversion steps

-- | The whole line of generation from Dedukti to formal and natural languages.

processDeduktiModule :: Env -> Module -> [GenResult]
processDeduktiModule env (MJmts jmts) = map (processJmt env) (includedDefs env jmts)

-- | The whole line of parsing latex code and converting to Dedukti.
-- This assumes that parsing units are single lines not starting with a backslash \ or % 

processLatex :: Env -> String -> [ParseResult]
processLatex env = map (processLatexLine env) . filter parsable . map uncomment . lines
 where
   parsable line = not (null (words line))
   uncomment line = case line of
     c:d:cs | c == '\\' -> c : d : uncomment cs -- preserve \% 
     '%' : cs -> uncomment cs
     c : cs -> c : uncomment cs
     _ -> line

-- | The judgements to show: not the declarations of the constants listed by
-- #EXCLUDE, nor the rewrite rules that define them (a block of rules keeps
-- its other rules, and is left out if none remain).
includedDefs :: Env -> [Jmt] -> [Jmt]
includedDefs env = concatMap include
 where
   excluded c = S.member c (excludedConstants (symbolTable env))
   include j = case j of
     JRules rules -> case filter (not . maybe False excluded . ruleHead) rules of
       [] -> []
       rs -> [JRules rs]
     _ | maybe False excluded (declaredIdent j) -> []
     _ -> [j]

-- * Conversion outcomes

-- | The result of conversion from Dedukti, with intermediate phases available for debugging.

data GenResult = GenResult {
  originalDedukti  :: Jmt,
  annotatedDedukti :: [Jmt],
  coreGF           :: [GFTree],
  nlgResults       :: [(Language, [((GFTree, String), (Scores, Int))])],
  backToDedukti    :: [Jmt]  --- | for debugging NLG and semantics
  }

-- | The result of conversion from informal Latex text, with intermediate phases for debugging.
---- | TODO: complete formalResults with type checking in Dedukt.

data ParseResult = ParseResult {
  originalLine  :: String,
  lexedLine     :: String,
  termIndex     :: [String],
  indexedLine   :: String,
  parseMessage  :: String,
  unknownWords  :: [String],
  parseResults  :: [GFTree],
  unindexedResults :: [GFTree],
  formalResults :: [(GFTree, GFTree, GFTree, [Jmt])], -- | parsed, unindexed, normalized
  transResults  :: [String], -- | back-translation or translation to another language
  typecheckMessage :: String -- | with -typecheck: what type-checking did to the readings
  }


-- * The conversions in more detail

-- | Processing a single Dedukti judgement.

processJmt :: Env -> Jmt -> GenResult
processJmt env djmt =
  if toFormalism env /= "NONE"
  then dummyGenResult (applyDeduktiConversions env djmt)
  else
    let
      cjmt = applyDeduktiConversions env djmt
      jmts = annotateDedukti env (dropImplicitHypos env cjmt)
      hidden = implicitPositions env cjmt
      core = [DMC.hideHypoVars (hypoVarsAt hidden j) (dedukti2core j) | j <- jmts]
      exts = concatMap (core2ext env) core
      bare = MCI.bareQuantifiers (implicitKinds env)
      nlgs = setnub $ map (gf . bare) $ exts
      vars = if elem "-variations" (flags env) then id else (take 1) 
      best = maybe vars take (nbestNLG env)
      nlglins lang = [(tree, gftree2nat env lang tree) | tree <- nlgs]
      nlgranks = [(lang, best (rankGFTreesAndNat env (unlex env) (nlglins lang))) | lang <- langs env]
    in GenResult {
      originalDedukti = djmt,
      annotatedDedukti = jmts,
      coreGF = map (gf . bare) core,
      nlgResults = nlgranks,
      backToDedukti = setnub (concatMap (gjmt2dedukti env) exts)
      }

-- | The empirical check of doc/nlg-and-semantics.md: for each judgement and
-- each of its MathCore trees t (at most four annotations), every NLG variant
-- t', attributed to the first stage that makes it, with the semantics of t'.
-- A semantics that fails (an error in the Haskell code) gives no trees, and
-- the variant counts as lost.
checkNLGSemantics :: Env -> Module -> IO String
checkNLGSemantics env (MJmts jmts) = do
  results <- mapM one [(jmtName j, t) | j <- includedDefs env jmts, t <- cores j]
  return (NC.report results)
 where
  one (name, t) = do
    vs <- mapM (\(s, v) -> do
                 r <- try (evaluate (forceAll (ext2core env v))) :: IO (Either SomeException [GJmt])
                 let (sems, note) = either (\e -> ([], " [semantics failed: " ++ take 100 (show e) ++ "]")) (\x -> (x, "")) r
                 return (s, v, gftree2nat env (toLang env) (gf v) ++ note, sems))
               (firstStage (NC.nlgStages (isFlag "-roundtrip-safe" env) (appNLGDefs (nlgTable (symbolTable env))) bare t))
    return (name, t, vs)
  forceAll xs = length (concatMap (showExpr [] . gf) xs) `seq` xs
  bare = MCI.bareQuantifiers (implicitKinds env)
  cores dj =
    let cjmt = applyDeduktiConversions env dj
        js = take 4 (annotateDedukti env (dropImplicitHypos env cjmt))
        hidden = implicitPositions env cjmt
    in [DMC.hideHypoVars (hypoVarsAt hidden j) (dedukti2core j) | j <- js]
  firstStage :: [(NC.Stage, [GJmt])] -> [(NC.Stage, GJmt)]
  firstStage stages = go [] [(s, v) | (s, vs) <- stages, v <- vs]
   where
    go seen svs = case svs of
      (s, v) : rest | notElem v seen -> (s, v) : go (v : seen) rest
                    | otherwise -> go seen rest
      [] -> []
  jmtName :: Jmt -> String
  jmtName j = case j of
    JStatic (QIdent c) _ -> c
    JDef (QIdent c) _ _ -> c
    JThm (QIdent c) _ _ -> c
    JInj (QIdent c) _ _ -> c
    _ -> "(rules)"

-- | The kinds of the implicit types (#IMPLICIT), as MathCore says them
implicitKinds :: Env -> [GKind]
-- no bare quantifiers with -roundtrip-safe: they have no semantics yet (F13)
implicitKinds env | isFlag "-roundtrip-safe" env = []
implicitKinds env = [DMC.cleanup (DMC.exp2kind k) |
  not (isFlag "-show-implicit-hypotheses" env),
  s <- implicitTypes (symbolTable env),
  Right t <- [pExp (myLexer s)],
  k <- annotateDkIdents env t]

-- | When just converting form Dk to another formalism, no GF is needed.
dummyGenResult :: Jmt -> GenResult
dummyGenResult jmt = GenResult jmt [jmt] undefined [] []


-- | Get GenResult from a GF tree instead of a Dedukti Jmt

processGFTree :: Env -> GFTree -> GenResult
processGFTree env gft =
    let
      jmts = []
      core = [gft]
      exts = concatMap (core2ext env . fg) core
      nlgs = setnub $ map gf $ exts
      vars = if elem "-variations" (flags env) then id else (take 1) 
      best = maybe vars take (nbestNLG env)
      nlglins lang = [(tree, gftree2nat env lang tree) | tree <- nlgs]
      nlgranks = [(lang, best (rankGFTreesAndNat env (unlex env) (nlglins lang))) | lang <- langs env]
      backs = setnub (concatMap (gjmt2dedukti env) exts)
    in GenResult {
      originalDedukti = head (gjmt2dedukti env (fg gft)),
      annotatedDedukti = jmts,
      coreGF = core,
      nlgResults = nlgranks,
      backToDedukti = backs
      }


-- | Processing a single line of LaTeX.

processLatexLine :: Env -> String -> ParseResult
processLatexLine env s =
  let
    trans = isFlag "-translate" env
    transCore = isFlag "-translate-core" env
    parseonly = isFlag "-parse-only" env
    ls = lextex s
    (ils, tindex) = indexTex ls
    Just jmt = readType "Jmt"
    (mts, msg) = parseJmt env jmt ils
    ts = maybe [] id mts
    -- each parse with the trees of its $...$ parts, in all their readings
    tuts = [(t, ut) | t <- ts, ut <- unindexGFTrees env tindex t]
    uts = nub (map snd tuts)
    -- with -translate-core: the line followed by the verbalizations of its MathCore semantics
    cores = nub [gftree2nat env (toLang env) (gf ct) | ut <- uts, ct <- ext2core env (fg ut)]
    coreTranslations =
      if null ts
      then ["\\textbf{No parse:} " ++ s]
      else s : ["$\\Rightarrow$ " ++ unlex env c | c <- cores]
  in ParseResult {
    typecheckMessage = "",
    originalLine = s,
    lexedLine = ls,
    termIndex = tindex,
    indexedLine = ils,
    parseMessage = msg,
    unknownWords = missingWords env ils,
    parseResults = ts,
    unindexedResults = uts,
    formalResults =
      if trans || parseonly
      then []
      else [
        (t, ut, gf ct, gjmt2dedukti env ct) |
          (t, ut) <- tuts,
          let fut = tracs env ("FUT.") (fg ut),
          ct <- ext2core env fut
          ],
    transResults =
      if transCore
      then coreTranslations
      else [unindexString tindex (unlex env (gftree2nat env (toLang env) t)) | t <- ts]
    }


-- * Dedukti-internal onversions

-- | Selected by flags in the environment, see -help.

applyDeduktiConversions :: Env -> DkTree a -> DkTree a
applyDeduktiConversions env t = foldl (flip ($)) t fs where
  fs :: [DkTree a -> DkTree a]
  fs = [f | (flag, f) <- [
          ("-peano2int", peano2int),
          ("-drop-qualifs", stripQualifiers),
          ("-drop-definitions", dropDefinitions),
          ("-hide-arguments", ignoreFirstArguments (dropTableEnv env))
         ], isFlag flag env
       ]

-- * Phases of the conversion pipeline

-- | Annotate Dedukti with GF information.
annotateDedukti :: Env -> Jmt -> [Jmt]
annotateDedukti env t = annotateDkIdents env (harmonizeJmt t)

-- | From annotated Dedukti to MathCore.
dedukti2core :: Jmt -> GJmt
dedukti2core = DMC.jmt2core

-- | Check type mismatches and missing items; performed as a part of building Env
checkSymbolTable :: Module -> PGF -> SymbolTable -> [String]
checkSymbolTable = symbolTableErrors

printSymbolTable :: SymbolTable -> String ---- TODO show complete information
printSymbolTable st = unlines $ [
  showConstantTable (constantTable st),
  "# semantics table keys: " ++ show (M.keys (semanticsTable st)),
  "# NLG table keys: " ++ show (M.keys (nlgTable st))
  ] ++
  macroCommands (macroTable st)

printSymbolTableLong :: SymbolTable -> String
printSymbolTableLong = showConstantTableLong . constantTable


-- * Printing conversions

-- ** Conversions starting from Dedukti

-- | With or without intermediate phases, as selected by flags in Env.
printGenResult :: Env -> GenResult -> [String]
printGenResult env result = case 0 of
  _ | toFormalism env /= "NONE" ->
    [printFormalismJmt env (toFormalism env) (originalDedukti result)]
  _ | isFlag "-to-symbolic-latex" env -> printSymbolicLatex env result
  _ | isFlag "-json" env || any (flip isFlag env) ["-v", "-vs"] -> [showJsonGenResult env result]
  _ | isFlag "-parallel-data" env -> [showParallelData env result] 
  _ -> printNLGOutput env result


-- | Standard logical notation in LaTeX, linearized directly from MathCore.
printSymbolicLatex :: Env -> GenResult -> [String]
printSymbolicLatex env result = vars (nub [
  unlex env (gftree2nat env (mkLanguage (grammar env) "Latex") tree) | tree <- coreGF result])
 where
   vars = if isFlag "-variations" env then id else take 1

-- | Just the final NLG results.
printNLGOutput :: Env -> GenResult -> [String]
printNLGOutput env result = case (lookup (toLang env) (nlgResults result)) of
  Just phrases | isFlag "-trees" env ->
    [constant ++ "\t" ++ str ++ "\t" ++ showExpr [] tree | ((tree, str), _) <- phrases]
  Just phrases -> map (snd . fst) phrases
  _ -> error $ "language not available: " ++ (showCId (toLang env)) ++
               ". Available values: " ++ unwords (map showCId (langs env))
 where
   -- the name of the Dedukti constant, for -trees
   constant = case words (printTree (originalDedukti result)) of
     "def" : c : _ -> c
     c : _ -> c
     [] -> ""

showJsonGenResult :: Env -> GenResult -> String
showJsonGenResult env result = encodeJSON $ mkJSONObject $ [
    mkJSONField "originalDedukti" (stringJSON (printDeduktiEnv env (originalDedukti result))),
    mkJSONListField "annotatedDedukti" (map (stringJSON . printDeduktiEnv env) (annotatedDedukti result)),
    mkJSONListField "coreGF" (map (stringJSON . showExpr []) (coreGF result))] ++
    if isFlag "-vs" env then [] else [
    mkJSONField "nlgResults" (mkJSONObject [
      mkJSONListField (showCId lang) (map (printRank) ranks) | (lang, ranks) <- nlgResults result])] ++
    if isFlag "-no-back-to-dedukti" env then [] else [
    mkJSONListField "backToDedukti" [stringJSON (printDeduktiEnv env jmt) | jmt <- backToDedukti result]
    ]

-- | The scores for each tree an string, in JSON.
printRank :: ((GFTree, String), (Scores, Int)) -> JSValue
printRank ((tree, str), (scores, rank)) = mkJSONObject [
  mkJSONField "tree" (stringJSON (showExpr [] tree)),
  mkJSONField "lin" (stringJSON str),
  mkJSONField "scores" (stringJSON (show scores)),
  mkJSONField "penalty" (stringJSON (show rank))
  ]

-- | Parallel data, usable for extracting pairs for trainingan an LLN.
showParallelData :: Env -> GenResult -> String
showParallelData env result = encodeJSON $ mkJSONObject $ [
    mkJSONField formalism
      (stringJSON (printFormalismJmt env formalism (originalDedukti result)))
        | formalism <- formalisms env
  ] ++ [  
    mkJSONListField (showCId lang) (map (stringJSON . snd . fst) ranks)
      | (lang, ranks) <- nlgResults result
  ]

-- | Converting Dedukti code to different formalisms.
printFormalismJmt :: Env -> String -> Jmt -> String
printFormalismJmt env formalism jmt = case formalism of
  "agda" -> dedukti2agda env jmt
  "lean" -> dedukti2lean env jmt
  "rocq" -> dedukti2rocq env jmt
  "dedukti" -> printDeduktiEnv env jmt
  f -> error $ "formalism not available: " ++ f ++ ". Available values: " ++ unwords (formalisms env)


-- | These are syntactic conversions, therefore total but may fail to typecheck in the targets.
-- | Notice: imports may have to be added to the generated files.

dedukti2agda :: Env -> Jmt -> String
dedukti2agda env jmt = unlines (DA.transJmtCtx emptyRuleContext (formalismConversion env "agda" jmt))

dedukti2lean :: Env -> Jmt -> String
dedukti2lean env jmt = unlines (DL.transJmtCtx emptyRuleContext (formalismConversion env "lean" jmt))

dedukti2rocq :: Env -> Jmt -> String
dedukti2rocq env jmt = unlines (DR.transJmtCtx emptyRuleContext (formalismConversion env "rocq" jmt))

formalismConversion :: Env -> String -> Jmt -> Jmt
formalismConversion env formalism = maybe id alphaConvert (M.lookup formalism (conversionTableEnv env))

-- | Converting a whole Dedukti module to a formalism. Unlike 'printFormalismJmt',
-- this can translate rewrite rules, which need information from the rest of the module.
printFormalismModule :: Env -> String -> Module -> [String]
printFormalismModule env formalism (MJmts jmts) = case formalism of
  "agda" -> DA.printAgdaModule cjmts
  "lean" -> DL.printLeanModule cjmts
  "rocq" -> DR.printRocqModule cjmts
  _ -> concatMap (printGenResult env) (processDeduktiModule env (MJmts jmts))
 where
  cjmts = map (convert . applyDeduktiConversions env) (filter (not . isFramework) (includedDefs env jmts))
  -- the declarations of the framework, which the translations interpret
  isFramework jmt = case jmt of
    JStatic c _ -> elem c framework
    JDef c _ _ -> elem c framework
    _ -> False
  framework = map QIdent ["Set", "Elem", "Prop", "Proof"]
  table = M.findWithDefault M.empty formalism (conversionTableEnv env)
  -- wildcards in rule patterns are kept, as the translations of rules treat them
  convert jmt = case jmt of
    JRules _ -> alphaConvert (M.delete (QIdent "_") table) jmt
    _ -> alphaConvert table jmt

-- * Reading the source code of other formalisms

-- | The formalisms whose source files can be read (-from-formalism), with the
-- scripts in scripts/ that export them to Dedukti, and their file suffixes.
sourceFormalisms :: [(String, (FilePath, String))]
sourceFormalisms = [
  ("agda", ("agda/agda-export.py", ".agda")),
  ("lean", ("lean/lean-export.py", ".lean")),
  ("rocq", ("rocq/rocq-export.py", ".v"))
  ]

-- | Export a source file of Agda, Lean or Rocq to Dedukti in a directory, by
-- python3 scripts/<formalism>/<formalism>-export.py <file> -out=<dir>.
-- The result: the Dedukti file of the module, and the symbol tables made with
-- it (<M>-hide.dkgf, the implicit arguments that the readings hide).
-- The messages of the script go to standard error.
exportFromFormalism :: FilePath -> String -> FilePath -> FilePath -> IO (FilePath, [FilePath])
exportFromFormalism root formalism file dir = do
  script <- case lookup formalism sourceFormalisms of
    Just (s, _) -> return (root </> "scripts" </> s)
    Nothing -> error ("-from-formalism: not available: " ++ formalism ++ ". Available values: " ++
                      unwords (map fst sourceFormalisms))
  createDirectoryIfMissing True dir
  res <- try (readProcessWithExitCode "python3" [script, file, "-out=" ++ dir] "")
           :: IO (Either SomeException (ExitCode, String, String))
  case res of
    Left e -> error ("-from-formalism needs python3: " ++ show e)
    Right (code, out, err) -> do
      hPutStr stderr (out ++ err)
      if code /= ExitSuccess then error ("export of " ++ file ++ " failed: " ++ script) else return ()
  -- the module is named after the file (Agda: its module name, ending with it)
  let stem = takeBaseName file
  files <- listDirectory dir
  dk <- case [f | f <- files, f == stem ++ ".dk"] ++ [f | f <- files, ("." ++ stem ++ ".dk") `isSuffixOf` f] of
    f : _ -> return (dir </> f)
    [] -> error ("no " ++ stem ++ ".dk in " ++ dir)
  let hide = dropExtension dk ++ "-hide.dkgf"
  hasHide <- doesFileExist hide
  return (dk, [hide | hasHide])

-- | A fresh directory for an export, in the temporary directory.
freshExportDir :: FilePath -> IO FilePath
freshExportDir file = do
  tmp <- getTemporaryDirectory
  (path, h) <- openTempFile tmp ("informath-" ++ takeBaseName file)
  hClose h
  removeFile path
  createDirectoryIfMissing True path
  return path

-- | The flags with symbol tables added to -symboltables or -add-symboltables.
addSymbolTableFlags :: [FilePath] -> [Flag] -> [Flag]
addSymbolTableFlags [] args = args
addSymbolTableFlags tables args
  | flagHasValue "-symboltables" args = map (extend "-symboltables") args
  | flagHasValue "-add-symboltables" args = map (extend "-add-symboltables") args
  | otherwise = args ++ ["-add-symboltables=" ++ intercalate "," tables]
 where
  extend flag arg =
    if takeWhile (/= '=') arg == flag then arg ++ "," ++ intercalate "," tables else arg

-- | The identifiers of a formalism converted to Dedukti's by the #CONV lines of the
-- symbol tables, read backwards (#CONV <formalism> <Dedukti> <native>). The
-- framework (Set, Elem, Prop, Proof, Type, _) is not converted: the exports
-- write it as Informath has it.
fromFormalismConversion :: Env -> String -> Module -> Module
fromFormalismConversion env formalism = alphaConvert inverse
 where
  table = M.findWithDefault M.empty formalism (conversionTableEnv env)
  inverse = M.fromList [(n, d) | (d, n) <- M.toList table, notElem d framework, notElem n framework]
  framework = map QIdent ["Set", "Elem", "Prop", "Proof", "Type", "_"]

-- * Proof texts and HTML documents of exported source files

-- | The proof texts of the proof terms of a module (its definitions), each made
-- on its own within a time limit (-proof-timeout=<seconds>, default 30): the
-- LaTeX of each, or Nothing if it fails or takes too long. The base of the proofs
-- is the -base of the environment.
termProofTexts :: Env -> Module -> IO [(QIdent, Maybe String)]
termProofTexts env (MJmts jmts) = forM [(c, j) | j <- jmts, Just c <- [proofName j]] $ \(c, j) -> do
  let pieces = proofDemoPieces env (baseConstantModule env) (MJmts [j]) (proofLinearizers env) (proofTreePrinter env)
      s = concatMap snd pieces
  r <- timeout (seconds * 1000000) (try (evaluate (length s)) :: IO (Either SomeException Int))
  return (c, case r of
    Just (Right _) | not (null pieces) -> Just s
    _ -> Nothing)
 where
  seconds = maybe 30 id (argValueMaybeInt "-proof-timeout" (flags env))
  proofName :: Jmt -> Maybe QIdent
  proofName j = case j of
    JDef c _ (MEExp _) -> Just c
    JThm c _ (MEExp _) -> Just c
    _ -> Nothing

-- | The proof texts of the tactic proofs of a file of proof units (-tactic-proof),
-- one for each proof, by its name ("coq", "lean" or "name").
tacticProofTexts :: Env -> String -> [(String, String)]
tacticProofTexts env s = case J.decode s of
  J.Ok (J.JSArray proofs) -> [(nameOf p, showTacticProofs env (J.encode (J.JSArray [p]))) | p <- proofs]
  _ -> []
 where
  nameOf p = case p of
    J.JSObject o -> case [J.fromJSString v | k <- ["dk", "coq", "lean", "name"],
                                              Just (J.JSString v) <- [lookup k (J.fromJSObject o)]] of
      v : _ -> v
      [] -> ""
    _ -> ""

-- | The native names of the declarations of an export (<M>.json of scripts/*/):
-- native name ("coq", "lean" or "agda") -> Dedukti name.
readNativeNames :: FilePath -> IO (M.Map String String)
readNativeNames file = do
  exists <- doesFileExist file
  if not exists then return M.empty else do
    s <- readFile file
    return $ case J.decode s of
      J.Ok (J.JSArray ds) -> M.fromList [(J.fromJSString n, J.fromJSString dk) |
        J.JSObject o <- ds, Just (J.JSString dk) <- [lookup "dk" (J.fromJSObject o)],
        k <- ["coq", "lean", "agda"], Just (J.JSString n) <- [lookup k (J.fromJSObject o)]]
      _ -> M.empty

-- | The declarations without their unnamed hypotheses of propositions,
-- (_ : Proof (C X)) ->: the type class instances that the Lean export states in
-- theorems but leaves out of their applications, so that the theorems are rules
-- whose premisses -proof-text reads off their types (as in mathlib-informath).
dropInstanceHypotheses :: Module -> Module
dropInstanceHypotheses (MJmts jmts) = MJmts (map drop jmts)
 where
  drop :: Jmt -> Jmt
  drop j = case j of
    JStatic c t -> JStatic c (dropT t)
    JDef c (MTExp t) e -> JDef c (MTExp (dropT t)) e
    JThm c (MTExp t) e -> JThm c (MTExp (dropT t)) e
    JInj c (MTExp t) e -> JInj c (MTExp (dropT t)) e
    _ -> j
  dropT :: Exp -> Exp
  dropT t = case t of
    EFun h body | isInstance h -> dropT body
    EFun h body -> EFun h (dropT body)
    _ -> t
  isInstance :: Dedukti.AbsDedukti.Hypo -> Bool
  isInstance h = case h of
    HParVarExp (QIdent "_") (EApp (EIdent (QIdent "Proof")) _) -> True
    HVarExp (QIdent "_") (EApp (EIdent (QIdent "Proof")) _) -> True
    _ -> False

-- | A LaTeX document of proof texts.
proofTextDocument :: Env -> [String] -> String
proofTextDocument = proofDemoFile

-- | What the export of a source file says of a declaration: its kind, docstring,
-- and source text.
data SourceInfo = SourceInfo {
  siKind :: String,
  siDoc :: String,
  siSource :: String
  }

-- | The declarations of the JSON file of an export (<M>.json of scripts/*/), by Dedukti name.
readSourceInfo :: FilePath -> IO (M.Map QIdent SourceInfo)
readSourceInfo file = do
  exists <- doesFileExist file
  if not exists then return M.empty else do
    s <- readFile file
    return $ case J.decode s of
      J.Ok (J.JSArray ds) -> M.fromList [(QIdent dk, SourceInfo (field "kind" o) (field "doc" o) (field "source" o)) |
                                          J.JSObject o <- ds, let dk = field "dk" o, not (null dk)]
      _ -> M.empty
 where
  field k o = case lookup k (J.fromJSObject o) of
    Just (J.JSString v) -> J.fromJSString v
    _ -> ""

-- | The languages of an HTML document: those of -to-lang, which may be a list (Eng,Fre).
htmlLanguages :: [Flag] -> [String]
htmlLanguages args = argValues "-to-lang" english args

-- | An HTML document of a module (-to-html-doc): its declarations with their
-- source (if the export gives it), Dedukti, readings in the languages of
-- -to-lang, formula, and proof texts (by the names of the declarations, as LaTeX
-- of -proof-text or -tactic-proof), the proofs with folds by scripts/proof_folds.py.
htmlDocument :: Env -> String -> String -> Maybe String -> M.Map QIdent SourceInfo -> M.Map String String
             -> [Jmt] -> IO String
htmlDocument env title intro formalism sources proofs jmts = do
  let groups = grouped jmts
      langCodes = [drop (length informathPrefix) (showCId l) | l <- langs env]
      entries0 = map (entry langCodes) groups
      -- the proof tables of the entries: (entry, language, rows)
      tables = [(i, l, rows) | (i, g) <- zip [0 :: Int ..] groups,
                               Just latex <- [M.lookup (groupName g) proofs],
                               (l, rows) <- zip langCodes (H.latexTableRows latex), not (null rows)]
  (htmlTables, css, js) <- foldTables (informathRoot env) [(l, rows) | (_, l, rows) <- tables]
  let withProofs = [e { H.deProofs = [(l, t) | ((k, l, _), t) <- zip tables htmlTables, k == i] } |
                    (i, e) <- zip [0 ..] entries0]
  return (H.htmlPage title intro macrosJson css js withProofs)
 where
  -- a declaration with the rules that follow it for the same constant
  grouped :: [Jmt] -> [[Jmt]]
  grouped js = case js of
    j : rest -> let (rs, others) = span (isRulesOf (declName j)) rest in (j : rs) : grouped others
    [] -> []
  isRulesOf :: Maybe QIdent -> Jmt -> Bool
  isRulesOf c j = case j of
    JRules rules -> c /= Nothing && all (\r -> ruleHead r == c) rules
    _ -> False
  declName :: Jmt -> Maybe QIdent
  declName j = case j of
    JStatic c _ -> Just c
    JDef c _ _ -> Just c
    JThm c _ _ -> Just c
    JInj c _ _ -> Just c
    JRules (r : _) -> ruleHead r
    _ -> Nothing
  groupName :: [Jmt] -> String
  groupName g = maybe "" (\(QIdent c) -> c) (declName (head g))

  entry langCodes g =
    let c = declName (head g)
        info = c >>= flip M.lookup sources
        results = map (processJmt env) g
        reading l = unwords [r | res <- results, Just ((_, r) : _) <- [lookup l [(lc, map fst rs) |
                              (lang, rs) <- nlgResults res, let lc = drop (length informathPrefix) (showCId lang)]]]
    in H.DocEntry {
      H.deName = maybe "" (\(QIdent n) -> n) c,
      H.deKind = maybe (kindOf (head g)) siKind info,
      H.deDoc = maybe "" siDoc info,
      H.deSource = case (formalism, info) of
        (Just f, Just i) | not (null (siSource i)) -> Just (f, siSource i)
        _ -> Nothing,
      H.deDedukti = unlines (map (printDeduktiEnv env) g),
      H.deReadings = [(l, reading l) | l <- langCodes],
      H.deFormula = unwords (concatMap (take 1 . printSymbolicLatex env) results),
      H.deProofs = []
      }
  kindOf :: Jmt -> String
  kindOf j = case j of
    JThm _ _ _ -> "theorem"
    JDef _ _ (MEExp _) -> "definition"
    JDef _ _ _ -> "definition"
    JStatic _ t | isProofType t -> "axiom"
    _ -> "declaration"
  isProofType :: Exp -> Bool
  isProofType t = case snd (splitType t) of
    EApp (EIdent (QIdent "Proof")) _ -> True
    _ -> False

  -- MathJax macros from the symbol tables' \newcommand lines
  macrosJson = J.encode (J.makeObj [(drop 1 c, if n == 0 then J.showJSON d else J.showJSON [J.showJSON d, J.showJSON n]) |
                                    (c, (n, d)) <- M.assocs (macroTable (symbolTable env))])

-- | Proof tables with folds, by python3 scripts/proof_folds.py --json: the HTML of
-- each, and the CSS and JavaScript of the folds.
foldTables :: FilePath -> [(String, [[String]])] -> IO ([String], String, String)
foldTables root proofs = do
  let input = J.encode (J.makeObj [("proofs", J.JSArray [J.makeObj [("lang", J.showJSON l), ("rows", J.showJSON rows)] |
                                                          (l, rows) <- proofs])])
  res <- try (readProcessWithExitCode "python3" [root </> "scripts" </> "proof_folds.py", "--json"] input)
           :: IO (Either SomeException (ExitCode, String, String))
  case res of
    Right (ExitSuccess, out, _) -> case J.decode out of
      J.Ok (J.JSObject o) -> return (strings "tables" o, string "css" o, string "js" o)
      _ -> fallback
    Right (_, _, err) -> hPutStr stderr err >> fallback
    Left e -> hPutStr stderr (show e) >> fallback
 where
  strings k o = case lookup k (J.fromJSObject o) of
    Just (J.JSArray vs) -> [J.fromJSString v | J.JSString v <- vs]
    _ -> []
  string k o = case lookup k (J.fromJSObject o) of
    Just (J.JSString v) -> J.fromJSString v
    _ -> ""
  -- without python3: the rows as a plain table
  fallback = return ([plain rows | (_, rows) <- proofs], "", "")
  plain rows = "<table class=\"proof\">" ++ concat ["<tr>" ++ concat ["<td>" ++ H.escapeHtml c ++ "</td>" | c <- r] ++ "</tr>" | r <- rows] ++ "</table>"

-- | TODO: type-check a Dedukti judgement.
checkJmt :: Jmt -> Bool
checkJmt _ = True ----

-- | With -typecheck: keep only the readings of a parsed line whose Dedukti
-- type-checks with dk, on top of the base constants (-base). If no reading of
-- the line type-checks, all are kept, and marked as ill-typed.
typecheckParseResults :: Env -> [ParseResult] -> IO [ParseResult]
typecheckParseResults env results
  | not (isFlag "-typecheck" env) = return results
  | otherwise = do
      tmp <- getTemporaryDirectory
      -- a file of its own, as several RunInformath may run at the same time
      (file, h) <- openTempFile tmp "informath-typecheck.dk"
      hClose h
      let base = printTree (baseConstantModule env)
      (rs, _) <- foldM (check base file) ([], M.empty) results
      _ <- try (removeFile file) :: IO (Either SomeException ())
      return (reverse rs)
 where
   check base file (done, memo) r = do
     (checked, memo') <- foldM (checkReading base file) ([], memo) (formalResults r)
     let good = [(t, ut, ct, js) | (t, ut, ct, js) <- reverse checked, not (null js)]
         r' = case () of
           _ | null (formalResults r) -> r
           _ | null good -> r {typecheckMessage = "no reading type-checks"}
           _ -> r {formalResults = good, typecheckMessage =
                     show (length (concat [js | (_, _, _, js) <- good])) ++ " of " ++
                     show (length (nub (concat [js | (_, _, _, js) <- formalResults r]))) ++
                     " readings type-check"}
     return (r' : done, memo')
   checkReading base file (acc, memo) (t, ut, ct, jmts) = do
     (oks, memo') <- foldM (wellTyped base file) ([], memo) jmts
     return ((t, ut, ct, [j | (j, True) <- zip jmts (reverse oks)]) : acc, memo')
   wellTyped base file (oks, memo) jmt = do
     let s = printTree jmt
     case M.lookup s memo of
       Just ok -> return (ok : oks, memo)
       Nothing -> do
         writeFile file (base ++ "\n" ++ s ++ "\n")
         res <- try (readProcessWithExitCode "dk" ["check", file] "")
                  :: IO (Either SomeException (ExitCode, String, String))
         ok <- case res of
           Right (code, _, _) -> return (code == ExitSuccess)
           Left e -> error ("-typecheck needs dk, the Dedukti checker, on the path: " ++ show e)
         return (ok : oks, M.insert s ok memo)

-- ** Conversions starting from natural language

-- | Print the parse results with all intermediate phases.
printParseResult :: Env -> ParseResult -> [String]
printParseResult env result = case 0 of
  _ | toFormalism env /= "NONE" ->
    [printFormalismJmt env (toFormalism env) jmt | (_,_,_,jmts) <- formalResults result, jmt <- nub jmts]
  _ | isFlag "-translate" env || isFlag "-translate-core" env ->
    transResults result
  _ | isFlag "-parse-only" env ->
    map (showExpr []) (unindexedResults result)
  _ | isFlag "-failures" env ->
    if null (parseResults (result))
    then [originalLine result]
    else []
  _ | isFlag "-results" env ->
    if null (parseResults (result))
    then ["FAILURE: " ++ originalLine result]
    else ["SUCCESS: " ++ originalLine result]
  _ | isFlag "-json" env || isFlag "-v" env -> [encodeJSON $ mkJSONObject [
    mkJSONField "originalLine" (stringJSON (originalLine result)),
    mkJSONField "lexedLine" (stringJSON (lexedLine result)),
    mkJSONListField "termIndex" (map stringJSON (termIndex result)),
    mkJSONField "indexedLine" (stringJSON (indexedLine result)),
    mkJSONField "parseMessage" (stringJSON (parseMessage result)),
    mkJSONField "typecheckMessage" (stringJSON (typecheckMessage result)),
    mkJSONListField "unknownWords" (map stringJSON (unknownWords result)),
    mkJSONListField "formalResults" (map (finalParseResult env) (formalResults result))
    ]]
  _ -> printDeduktiOutput env result

-- | Print just the resulting Dedukti code.
printDeduktiOutput :: Env -> ParseResult -> [String]
printDeduktiOutput env result =
  nub [mark ++ printDeduktiEnv env jmt | (_,_,_,jmts) <- formalResults result, jmt <- jmts]
 where
   -- with -typecheck, when no reading of the line type-checks
   mark = if typecheckMessage result == "no reading type-checks" then "(; ILL-TYPED ;) " else ""

-- | Print both GF trees and resulting Dedukti, in JSON. 
finalParseResult :: Env -> (GFTree, GFTree, GFTree, [Jmt]) -> JSValue
finalParseResult env (t, ut, ct, jmts) = mkJSONObject [
  mkJSONField "parseTree" (stringJSON (showExpr [] t)),
  mkJSONField "unindexedTree" (stringJSON (showExpr [] ut)),
  mkJSONField "coreTree" (stringJSON (showExpr [] ct)),
  mkJSONListField "dedukti" (map (stringJSON . printDeduktiEnv env) jmts)
  ]


-- ** General printing facilities

-- | Results are printed line by line, or with a Latex preamble in a document environment. 
printResults :: Env -> [String] -> [String]
printResults env ss = 
  if isFlag "-to-latex-doc" env
  then toLatexDoc (macroCommands (macroTableEnv env)) (intersperse "" (map escapeTextUnderscores (nub ss)))
  else ss

-- | To print Dedukti under environment options, given in flags. 
printDeduktiEnv :: Print a => Env -> a -> String
printDeduktiEnv env t =
  if (isFlag "-dedukti-tokens" env)
  then unwords (deduktiTokens (printTree t))
  else printTree t

-- | to translate 
transEmbeddedDedukti :: Env -> [String] -> [String]
transEmbeddedDedukti env = transInEnv "dedukti" transDkEnv where
  transDkEnv (_ : ls) = trans (unlines (init ls))
  trans = unlines . intersperse "" . concatMap (printGenResult env) . processDeduktiModule env .  parseDeduktiModule

-- ** Seldom explicitly needed one-step conversion.

core2ext :: Env -> GJmt -> [GJmt]
core2ext env jmt = MCI.nlg env jmt

-- | Ranking linearizations, given as tokens separated by spaces, which are
-- returned unlexed with the given function.
rankGFTreesAndNat :: Env -> (String -> String) -> [(Expr, String)] -> [((Expr, String), (Scores, Int))]
rankGFTreesAndNat = rankTreesAndLins

ext2core :: Env -> GJmt -> [GJmt]
ext2core env = IMC.semantics (semanticsTableEnv env)

gjmt2dedukti :: Env -> GJmt -> [Jmt] 
gjmt2dedukti env = concatMap (MCD.jmt2dedukti (backConstantTableEnv env) (dropTableEnv env)) . ext2core env

core2dedukti :: Env -> GJmt -> [Jmt]
core2dedukti env = MCD.jmt2dedukti (backConstantTableEnv env) (dropTableEnv env)


-- * Reading input for processing

-- | Tp read a Dedukti module from one or more files; in translations, only from one file.
readDeduktiModule :: [FilePath] -> IO Module
readDeduktiModule files = mapM readFile files >>= return . parseDeduktiModule . unlines

-- | To parse a Dedukti file into its AST.
parseDeduktiModule :: String -> Module
parseDeduktiModule s = case pModule (myLexer s) of
  Bad e -> error ("parse error: " ++ e)
  Ok mo -> defacStatic mo

-- | To parse a Dedukti file into its AST.
parseDeduktiModuleErrorFree :: String -> Maybe Module
parseDeduktiModuleErrorFree s = case pModule (myLexer s) of
  Bad _ -> Nothing
  Ok mo -> return (defacStatic mo)

-- | To linearize a GF tree.
gftree2nat :: Env -> Language -> GFTree -> String
gftree2nat env lang tree = linearize (grammar env) lang tree

-- | To unlex in a latex-like style, overridded by flag -no-unlex.
unlex :: Env -> String -> String
unlex env s = if (isFlag "-no-unlex" env) then s else unlextex s

-- * Statistics about data.

-- | Statistics of unknown identifiers in Dedukti (not listed in .dkgf)
identsInDedukti :: Env -> Module -> [(String, Int)]
identsInDedukti env mo = map stringify (mfilter freqs) where
  stringify (c, i) = (printDeduktiEnv env c, i)
  freqs = sortOn (\ (_,i) -> -i) (M.toList (identsInTypes mo))
  mfilter = if elem "-unknown-idents" (flags env) then filter (notInTable . fst) else id 
  notInTable qid = M.notMember qid (constantTableEnv env) && not (all isDigit (printDeduktiEnv env qid))


-- | Statistics of unknown words in text (not recognized in .pgf).
unknownWordsInTex :: Env -> String -> [(String, Int)]
unknownWordsInTex env = frequencyTable . missingWords env . lextex

-- | List of missing words on a line.
missingWords :: Env -> String -> [String]
missingWords env = morphoMissing (morpho env) . tokens . lextex
 where
   tokens = ordinary [] . words
   ordinary acc ws = case ws of
     "$" : ww -> case break (=="$") ww of (_, _:ww2) -> ordinary acc ww2 ; _ -> acc
     "$$" : ww -> case break (=="$$") ww of (_, _:ww2) -> ordinary acc ww2 ; _ -> acc
     w : ww -> ordinary (w:acc) ww
     _ -> acc

-- | show candidate GF functions that linearize to a given word
findGFFunctions :: Env -> String -> (String, [String])
findGFFunctions env w = (w, nub [showCId f | (f, _) <- lookupMorpho (morpho env) w])

-- | To linearize a GF tree (given as string) to a string
readGFtree2nat :: Env -> String -> String
readGFtree2nat env s = linearize (grammar env) (toLang env) (readGFtree s)

-- | To read a GF tree from a string
readGFtree :: String -> GFTree
readGFtree s = case readExpr s of
  Just tree -> tree
  Nothing -> error ("ERROR: not a valid tree: " ++ s)

-- | To show all GF functions with their types, one per line
showGFFunctions :: Env -> [String]
showGFFunctions env = [unwords ([showCId f, ":", showType [] t, "\t"] ++ map showLang (langs f))
                         | f <- functions gr, Just t <- [functionType gr f]]
  where
    gr = grammar env
    misses = [(lang, S.fromList (missingLins gr lang)) | lang <- languages gr]
    langs f = [lang | lang <- languages gr, S.notMember f (maybe S.empty id (lookup lang misses))]
    showLang = drop 9 . showCId  -- InformathEng -> Eng


-- | To parse an example to a tree
parseFunExample :: Env -> String -> [String]
parseFunExample env s = map showFunProfile (parseExample (grammar env) (symbolTableLang env) s)

-- | lexical functions reachable from the current symbol table
reachableGFFunctions :: BackConstantTable -> S.Set CId
reachableGFFunctions bt = S.fromList [f | t <-  M.keys bt, f <- ids t] where
  ids t = case unApp t of
    Just (f, xs) -> f : concatMap ids xs
    _ -> []


-- | try parse a symbol table line by line (only annotations for the time being), report success or failure
tryParseSymbolTable :: Env -> [String] -> [String]
tryParseSymbolTable env ls = map tryParse ils
  where
    ils = zip [1..] ls
    tryParse (i, s) = case tryParseConstantTableEntry (grammar env) (fromLang env) s of
      Ok _ | isFlag "-keep-ok-entries" env -> s
      Ok _ -> unwords ["#line", show i, "OK:", s]
      Bad m -> unwords ["#line", show i, "BAD:", s, "ERROR:", m]

-- | Proof texts of tactic proofs, given as lists of proof units in JSON:
-- [{"name": n, "steps": [{"unit": u, "by": tactic, ...}]}], where u is
--   "hypos" with "hypos": [[x, type]]  -- let x be ..., assume that ...
--   "conclusion" with "prop": P        -- P; with "method": m, "by m, P", where m is
--                                         definition, rewriting, simplification,
--                                         computation or routine
--   "enough" with "prop": P            -- it is enough to show that P
--   "verify" with "prop": P            -- first verify that P (a nested proof follows)
--   "cases", "induction", "obvious"
-- and the types and propositions are Dedukti expressions. Each proof is
-- printed as by -proof-text: a numbered table of lines in each language.
showTacticProofs :: Env -> String -> String
showTacticProofs env s = case J.decode s of
  J.Error e -> error ("tactic proof file: " ++ e)
  J.Ok (J.JSArray proofs) ->
    prLatexFile (macroCommands (macroTable (symbolTable env))) $
      unlines $ intersperse "\n\n" (map oneTacticProof proofs)
  J.Ok _ -> error "tactic proof file: a list of proofs expected"
 where
   langsShown = if flagHasValue "-languages" (flags env) then langs env else [toLang env]

   oneTacticProof p = unlines $ intersperse "\n\n" (
     ["\\subsection*{" ++ verbatimInline (str (field "name" p)) ++ "}"] ++
     [table lang [(i, (depth st, stepUnit st), str (field "by" st)) | (i, st) <- zip [1 :: Int ..] (list (field "steps" p))]
       | lang <- langsShown] ++
     ["\\clearpage"])

   table lang rows = unlines $
     "\\begin{tabular}{rp{0.62\\textwidth}l}" :
     [show i ++ ". & " ++ indent d ++ unit2nat lang u ++ " & " ++ verbatimInline by ++ " \\\\" | (i, (d, u), by) <- rows] ++
     ["\\end{tabular}"]

   -- with -proof-indent, a step is indented by the assumptions open at it
   -- ("depth", from tactic-units.py), as the lines of -proof-text
   depth st = case field "depth" st of
     J.JSRational _ r | isFlag "-proof-indent" env -> round r
     _ -> 0 :: Int
   indent d = concat (replicate d "\\hspace*{1.5em}")

   -- a step whose Dedukti does not parse is shown as obvious
   stepUnit st = either (const GObviousConclusion) id (stepUnitE st)
   stepUnitE st = case str (field "unit" st) of
     "hypos" -> do
       hs <- sequence [fmap (HVarExp (QIdent (str x))) (dk (str t))
                      | J.JSArray [x, t] <- list (field "hypos" st)]
       let hypos = GListHypo (DMC.hypos2hypos [HVarExp x (ann t) | HVarExp x t <- hs])
       -- with -proof-goals, the goal of an assumption: "to prove that B, assume that A"
       case field "goal" st of
         J.JSString g | isFlag "-proof-goals" env -> do
           ge <- dk (J.fromJSString g)
           return (GGoalHyposUnit hypos (DMC.exp2prop (ann ge)))
         _ -> return (GHyposUnit hypos)
     "conclusion" -> do
       p <- fmap (DMC.exp2prop . ann) (dk (str (field "prop" st)))
       -- how the step proves it, if the tactic says: "by rewriting, P"
       return $ case lookup (str (field "method" st)) methods of
         Just hence -> GHencePropConclusion hence p
         Nothing -> GConclusionUnit p
     "enough" -> fmap (GEnoughGoal . DMC.exp2prop . ann) (dk (str (field "prop" st)))
     "verify" -> fmap (GFirstVerifyGoal . DMC.exp2prop . ann) (dk (str (field "prop" st)))
     "cases" -> return GCasesGoal
     "induction" -> return GInductionGoal
     _ -> return GObviousConclusion

   methods = [("definition", GbyDefinitionHence), ("rewriting", GbyRewritingHence),
              ("simplification", GbySimplificationHence), ("computation", GbyComputationHence),
              ("routine", GbyRoutineHence)]

   ann e = head (annotateDkIdents env e ++ [e])
   dk :: String -> Either String Exp
   dk t = case pExp (myLexer t) of
     Ok e -> Right e
     Bad m -> Left m

   field k v = case v of
     J.JSObject o -> maybe J.JSNull id (lookup k (J.fromJSObject o))
     _ -> J.JSNull
   str v = case v of
     J.JSString j -> J.fromJSString j
     _ -> ""
   list v = case v of
     J.JSArray vs -> vs
     _ -> []

   unit2nat lang u = unlex env (gftree2nat env lang (gf (best u)))
   best u = MCI.bareQuantifiers (implicitKinds env)
              (head ([u' | GUnitJmt u' <- core2ext env (GUnitJmt u)] ++ [u]))

--- proof text demo, experimental
showProofDemo :: Env -> Module -> Module -> String
showProofDemo env base mo = proofDemo env base mo (proofLinearizers env) (proofTreePrinter env)

-- | The proof texts of a module as showProofDemo, but each proof evaluated on
-- its own: a proof whose text cannot be made (an error in its conversion)
-- gets a section with the comment "% PROOF FAILED: ..." instead of a table,
-- and the proofs after it are still written
showProofDemoSafe :: Env -> Module -> Module -> IO String
showProofDemoSafe env base mo = do
  pieces <- mapM safe (proofDemoPieces env base mo (proofLinearizers env) (proofTreePrinter env))
  return (proofDemoFile env pieces)
 where
  safe (t, s) = do
    r <- try (evaluate (foldr seq () s)) :: IO (Either SomeException ())
    return $ case r of
      Right () -> s
      Left e -> unlines [ "\\subsection*{" ++ verbatimInline (printTree t) ++ "}"
                        , "% PROOF FAILED: " ++ unwords (lines (take 300 (show e)))
                        , "\\clearpage" ]

proofTreePrinter :: Env -> Maybe (Exp -> String)
proofTreePrinter env = if isFlag "-proof-trees" env then Just formula else Nothing
 where
   -- the formulas of the trees in symbolic LaTeX (-proof-trees), as with -to-symbolic-latex
   formula e = gftree2nat senv (mkLanguage (grammar env) "Latex")
                 (gf (MCI.bareQuantifiers kinds (DMC.exp2prop (head (annotateDkIdents senv e)))))
   senv = env{flags = "-to-symbolic-latex" : flags env}
   kinds = implicitKinds env

proofLinearizers :: Env -> [GUnit -> String]
proofLinearizers env = map unit2nat proofLangs
 where
   -- one proof text for each language of -languages, or for -to-lang
   proofLangs = if flagHasValue "-languages" (flags env) then langs env else [toLang env]

   unit2nat :: Language -> GUnit -> String
   unit2nat lang u = unlex env (gftree2nat env lang (gf (best u)))

   -- the lines keep the labels that ProofText kept, those the proof refers to
   best u = MCI.bareQuantifiers kinds (head [u | GUnitJmt u <- core2ext lenv (GUnitJmt u)])
   lenv = env{flags = "-proof-labels" : flags env}
   kinds = implicitKinds env

