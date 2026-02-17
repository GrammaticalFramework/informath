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
import ParseInformath (parseJmt, unindexGFTree)
import Lexing
import qualified Dedukti2Agda as DA
import qualified Dedukti2Rocq as DR
import qualified Dedukti2Lean as DL

import Ranking
import Environment
import BuildConstantTable
import qualified Dedukti2MathCore as DMC
import qualified MathCore2Informath as MCI
import qualified Informath2MathCore as IMC
import qualified MathCore2Dedukti as MCD
import Utils

import Informath
import PGF

import Data.List (partition, isSuffixOf, isPrefixOf, intersperse, sortOn, nub)
import Data.Char (isDigit, toUpper)
import qualified Data.Map as M
import qualified Data.Set as S


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

readEnv :: [Flag] -> IO Env
readEnv args = do
  mo <- readDeduktiModule (argValues "-base" baseConstantFile args)
  gr <- readGFGrammar (argValue "-grammar" grammarFile args)
  (ct, cvt, dt, mt) <- readConstantTable gr (argValues "-constants" constantTableFile args)
  let fro = mkLanguage gr (argValue "-from-lang" english args)
  ifArg "-check-constant-table" args (checkConstantTable mo gr mt ct)
  return Env {
    flags = args,
    grammar = gr,
    constantTable = ct,
    conversionTable = cvt,
    synonymConstantTableNLG = buildSynonymConstantTableNLG ct,
    synonymConstantTableSem = buildSynonymConstantTableSem ct,
    backConstantTable = buildBackConstantTable ct,
    baseConstantModule = mo,
    dropTable = dt,
    macroTable = mt,
    formalisms = words "agda dedukti lean rocq",
    langs = languages gr, ---- | relevantLanguages gr args,
    toLang = mkLanguage gr (argValue "-to-lang" english args),
    toFormalism = argValue "-to-formalism" "NONE" args,
    fromLang = fro,
    nbestNLG = argValueMaybeInt "-nbest" args,
    scoreWeights = commaSepInts (argValue "-weights" "1,1,1,1,1,1,1,1,1" args),
    morpho = buildMorpho gr fro
    }


-- ** Low-level access to data sources

{- | The most important data sources are a GF grammar (file .pgf)
and a ConstantTable (file .dkgf).
Both of these can be customized and passed as values of flags.
The following functions read them directly, but need hardly be called explicitly.
-}

-- | To read the GF grammar from a .pgf file.    
readGFGrammar :: FilePath -> IO PGF
readGFGrammar = readPGF

-- | To read a constant table from a .dkgf file. 
readConstantTable :: PGF -> [FilePath] -> IO (ConstantTable, ConversionTable, DropTable, MacroTable)
readConstantTable = buildConstantTable

-- | To construct a concrete syntax name from a 3-letter language code.
mkLanguage :: PGF -> String -> Language
mkLanguage pgf code = case readLanguage (informathPrefix ++ code) of
  Just lang | elem lang (languages pgf) -> lang
  _ -> error ("not a valid language: " ++ code)

-- * Default source files, which can be changed in Env flags (see RunInformath -help)

grammarFile = "grammars/Informath.pgf" 
baseConstantFile = "src/BaseConstants.dk"  
constantTableFile = "src/baseconstants.dkgf"

-- * Main types involved

-- | GF abstract syntax tree
type GFTree = Expr

-- | Dedukti judgement
type DkJmt = Jmt   

-- * Main conversion steps

-- | The whole line of generation from Dedukti to formal and natural languages.

processDeduktiModule :: Env -> Module -> [GenResult]
processDeduktiModule env (MJmts jmts) = map (processJmt env) jmts

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


-- * Conversion outcomes

-- | The result of conversion from Dedukti, with intermediate phases available for debugging.

data GenResult = GenResult {
  originalDedukti  :: Jmt,
  annotatedDedukti :: Jmt,
  coreGF           :: GFTree,
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
  formalResults :: [(GFTree, GFTree, GFTree, [Jmt])], -- | parsed, unindexed, normalized
  transResults  :: [String] -- | back-translation or translation to another language
  }


-- * The conversions in more detail

-- | Processing a single Dedukti judgement.

processJmt :: Env -> Jmt -> GenResult
processJmt env djmt =
  if toFormalism env /= "NONE"
  then dummyGenResult (applyDeduktiConversions env djmt)
  else
    let
      jmt = annotateDedukti env (applyDeduktiConversions env djmt)
      core = dedukti2core jmt
      exts = core2ext env core
      nlgs = setnub $ map gf $ exts
      vars = if elem "-variations" (flags env) then id else (take 1) 
      best = maybe vars take (nbestNLG env)
      nlglins lang = [(tree, unlex env (gftree2nat env lang tree)) | tree <- nlgs]
      nlgranks = [(lang, best (rankGFTreesAndNat env (nlglins lang))) | lang <- langs env]
    in GenResult {
      originalDedukti = djmt,
      annotatedDedukti = jmt,
      coreGF = gf core,
      nlgResults = nlgranks,
      backToDedukti = setnub (concatMap (gjmt2dedukti env) exts)
      }

-- | When just converting form Dk to another formalism, no GF is needed.
dummyGenResult :: Jmt -> GenResult
dummyGenResult jmt = GenResult jmt jmt undefined [] []


-- | Processing a single line of LaTeX.

processLatexLine :: Env -> String -> ParseResult
processLatexLine env s =
  let
    gr = grammar env
    trans = isFlag "-translate" env
    macroidents = isFlag "-macroidents" env
    ls = lextex s
    (ils, tindex) = indexTex ls
    Just jmt = readType "Jmt"
    (mts, msg) = parseJmt macroidents gr (fromLang env) jmt ils
    ts = maybe [] id mts
  in ParseResult {
    originalLine = s,
    lexedLine = ls,
    termIndex = tindex,
    indexedLine = ils,
    parseMessage = msg,
    unknownWords = missingWords env ils,
    parseResults = ts,
    formalResults = if trans then [] else [
      (t, ut, gf ct, gjmt2dedukti env ct) |
        t <- ts,
        let ut = unindexGFTree macroidents gr (fromLang env) tindex t,
        let ct = ext2core (fg ut)
      ],
    transResults = [
      unindexString tindex
        (unlex env (gftree2nat env (toLang env) t)) | t <- ts]
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
	  ("-hide-arguments", ignoreFirstArguments (dropTable env))
         ], isFlag flag env
       ]

-- * Phases of the conversion pipeline

-- | Annotate Dedukti with GF information.
annotateDedukti :: Env -> Jmt -> Jmt
annotateDedukti env t = annotateDkIdents (constantTable env) (dropTable env) t

-- | From annotated Dedukti to MathCore.
dedukti2core :: Jmt -> GJmt
dedukti2core = DMC.jmt2core

checkConstantTable :: Module -> PGF -> MacroTable -> ConstantTable -> String
checkConstantTable mo gr mt ct = unlines (constantTableErrors mo gr mt ct)

printConstantTable :: ConstantTable -> String
printConstantTable = showConstantTable


-- * Printing conversions

-- ** Conversions starting from Dedukti

-- | With or without intermediate phases, as selected by flags in Env.
printGenResult :: Env -> GenResult -> [String]
printGenResult env result = case 0 of
  _ | toFormalism env /= "NONE" ->
    [printFormalismJmt env (toFormalism env) (originalDedukti result)]
  _ | isFlag "-json" env || isFlag "-v" env -> [showJsonGenResult env result]
  _ | isFlag "-parallel-data" env -> [showParallelData env result] 
  _ -> printNLGOutput env result


-- | Just the filan NLG results.
printNLGOutput :: Env -> GenResult -> [String]
printNLGOutput env result = case (lookup (toLang env) (nlgResults result)) of
  Just phrases -> map (snd . fst) phrases
  _ -> error $ "language not available: " ++ (showCId (toLang env)) ++
               ". Available values: " ++ unwords (map showCId (langs env))

showJsonGenResult :: Env -> GenResult -> String
showJsonGenResult env result = encodeJSON $ mkJSONObject [
    mkJSONField "originalDedukti" (stringJSON (printDeduktiEnv env (originalDedukti result))),
    mkJSONField "annotatedDedukti" (stringJSON (printDeduktiEnv env (annotatedDedukti result))),
    mkJSONField "coreGF" (stringJSON (showExpr [] (coreGF result))),
    mkJSONField "nlgResults" (mkJSONObject [
      mkJSONListField (showCId lang) (map (stringJSON . printRank) ranks) | (lang, ranks) <- nlgResults result]),
    mkJSONListField "backToDedukti" [stringJSON (printDeduktiEnv env jmt) | jmt <- backToDedukti result]
    ]

-- | The scores for each tree an string, in JSON.
printRank :: ((GFTree, String), (Scores, Int)) -> String
printRank ((tree, str), (scores, rank)) = encodeJSON $ mkJSONObject [
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
dedukti2agda env jmt = unlines [DA.printAgdaJmts (DA.transJmt (conv jmt))] where
  conv = maybe id alphaConvert (M.lookup "agda" (conversionTable env))

dedukti2lean :: Env -> Jmt -> String
dedukti2lean env jmt = unlines [DL.printLeanJmt (DL.transJmt (conv jmt))] where
  conv = maybe id alphaConvert (M.lookup "lean" (conversionTable env))

dedukti2rocq :: Env -> Jmt -> String
dedukti2rocq env jmt = unlines [DR.printRocqJmt (DR.transJmt (conv jmt))] where
  conv = maybe id alphaConvert (M.lookup "rocq" (conversionTable env))

-- | TODO: type-check a Dedukti judgement.
checkJmt :: Jmt -> Bool
checkJmt jmt = True ----

-- ** Conversions starting from natural language

-- | Print the parse results with all intermediate phases.
printParseResult :: Env -> ParseResult -> [String]
printParseResult env result = case 0 of
  _ | toFormalism env /= "NONE" ->
    [printFormalismJmt env (toFormalism env) jmt | (_,_,_,jmts) <- formalResults result, jmt <- jmts]
  _ | isFlag "-translate" env ->
    transResults result
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
    mkJSONListField "unknownWords" (map stringJSON (unknownWords result)),
    mkJSONListField "formalResults" (map (stringJSON . printFinalParseResult env) (formalResults result))
    ]]
  _ -> printDeduktiOutput env result

-- | Print just the resulting Dedukti code.
printDeduktiOutput :: Env -> ParseResult -> [String]
printDeduktiOutput env result =
  [printDeduktiEnv env jmt | (_,_,_,jmts) <- formalResults result, jmt <- jmts]

-- | Print both GF trees and resulting Dedukti, in JSON. 
printFinalParseResult :: Env -> (GFTree, GFTree, GFTree, [Jmt]) -> String
printFinalParseResult env (t, ut, ct, jmts) = encodeJSON $ mkJSONObject [
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
  then toLatexDoc (macroCommands (macroTable env)) (intersperse "" ss)
  else ss

-- | To print Dedukti under environment options, given in flags. 
printDeduktiEnv :: Print a => Env -> a -> String
printDeduktiEnv env t =
  if (isFlag "-dedukti-tokens" env)
  then unwords (deduktiTokens (printTree t))
  else printTree t


-- ** Seldom explicitly needed one-step conversion.

core2ext :: Env -> GJmt -> [GJmt]
core2ext env jmt = MCI.nlg env jmt

rankGFTreesAndNat :: Env -> [(Expr, String)] -> [((Expr, String), (Scores, Int))]
rankGFTreesAndNat = rankTreesAndStrings

ext2core :: GJmt -> GJmt
ext2core = IMC.semantics

gjmt2dedukti :: Env -> GJmt -> [Jmt] 
gjmt2dedukti env = MCD.jmt2dedukti (backConstantTable env) (dropTable env) . ext2core

core2dedukti :: Env -> GJmt -> [Jmt]
core2dedukti env = MCD.jmt2dedukti (backConstantTable env) (dropTable env)


-- * Reading input for processing

-- | Tp read a Dedukti module from one or more files; in translations, only from one file.
readDeduktiModule :: [FilePath] -> IO Module
readDeduktiModule files = mapM readFile files >>= return . parseDeduktiModule . unlines

-- | To parse a Dedukti file into its AST.
parseDeduktiModule :: String -> Module
parseDeduktiModule s = case pModule (myLexer s) of
  Bad e -> error ("parse error: " ++ e)
  Ok mo -> mo

-- | To parse a Dedukti file into its AST.
parseDeduktiModuleErrorFree :: String -> Maybe Module
parseDeduktiModuleErrorFree s = case pModule (myLexer s) of
  Bad e -> Nothing
  Ok mo -> return mo

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
  notInTable qid = M.notMember qid (constantTable env) && not (all isDigit (printDeduktiEnv env qid))


-- | Statistics of unknown words in text (not recognized in .pgf).
unknownWordsInTex :: Env -> String -> [(String, Int)]
unknownWordsInTex env = frequencyTable . missingWords env . lextex

-- | List of missing words on a line.
missingWords :: Env -> String -> [String]
missingWords env = morphoMissing (morpho env) . words . lextex
 where
   tokens = ordinary [] . words
   ordinary acc ws = case ws of
     "$" : ww -> let (_, _:ww2) = break (=="$") ww in ordinary acc ww2
     "$$" : ww -> let (_, _:ww2) = break (=="$$") ww in ordinary acc ww2
     w : ww -> ordinary (w:acc) ww
     _ -> acc

-- | show candidate GF functions that linearize to a given word
findGFFunctions :: Env -> String -> (String, [String])
findGFFunctions env w = (w, nub [showCId f | (f, _) <- lookupMorpho (morpho env) w])

-- | To linearize a GF tree (given as string) to a string
readGFtree2nat :: Env -> String -> String
readGFtree2nat env s = case readExpr s of
  Just tree -> linearize (grammar env) (toLang env) tree
  Nothing -> ("ERROR: not a valid tree: " ++ s)

-- | To show all GF functions with their types, one per line
showGFFunctions :: Env -> [String]
showGFFunctions env = [unwords ([showCId f, ":", showType [] t, "\t"] ++ map showLang (langs f))
                         | f <- functions gr, Just t <- [functionType gr f]]
  where
    gr = grammar env
    misses = [(lang, S.fromList (missingLins gr lang)) | lang <- languages gr]
    langs f = [lang | lang <- languages gr, S.notMember f (maybe S.empty id (lookup lang misses))]
    showLang = drop 9 . showCId  -- InformathEng -> Eng


