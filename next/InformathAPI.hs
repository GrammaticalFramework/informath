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
import DeduktiOperations (alphaConvert, identsInTypes)
--import ConstantData 
--import SpecialDeduktiConversions (specialDeduktiConversions)
------import Informath -- to be removed
--import Core2Informath (nlg)
--import Informath2Core (semantics)
import ParseInformath (parseJmt, unindexGFTree)
import Lexing
--import MkConstants (mkConstants)
import qualified Dedukti2Agda as DA
import qualified Dedukti2Rocq as DR
import qualified Dedukti2Lean as DL

import Ranking4
import Environment4
import BuildConstantTable
--import qualified DMC
import qualified Dedukti2MathCore as DMC
import qualified MCI
import qualified IMC
import qualified MCD
import Utils

import NextInformath
import PGF

import Data.List (partition, isSuffixOf, isPrefixOf, intersperse, sortOn)
------import System.Random
import Data.Char (isDigit, toUpper) --- low-level auxiliaries
import qualified Data.Map as M

-- default source files

grammarFile = "next/grammars/NextInformath.pgf"
baseConstantFile = "src/BaseConstants.dk"
constantTableFile = "next/constants.dkgf"

-- main types involved

type GFTree = Expr
type DkTree a = Dedukti.AbsDedukti.Tree a
type DkJmt = Jmt


-- result of conversion from Dedukti, with intermediate phases for debugging

data GenResult = GenResult {
  originalDedukti  :: Jmt,
  annotatedDedukti :: Jmt,
  coreGF           :: GFTree,
  nlgResults       :: [(Language, [((GFTree, String), (Scores, Int))])],
  backToDedukti    :: [Jmt]  --- for debugging NLG and semantics
  }

-- when just converting form Dk to another formalism
dummyGenResult :: Jmt -> GenResult
dummyGenResult jmt = GenResult jmt jmt undefined [] []

printResults :: Env -> [String] -> [String]
printResults env ss = 
  if isFlag "-to-latex-doc" env
  then toLatexDoc (intersperse "" ss)
  else ss

-- conversion that produces the whole line of generation from Dedukti

processDeduktiModule :: Env -> Module -> [GenResult]
processDeduktiModule env (MJmts jmts) = map (processJmt env) jmts

processJmt :: Env -> Jmt -> GenResult
processJmt env djmt =
  if toFormalism env /= "NONE"
  then dummyGenResult djmt
  else
    let
      flag = flags env
      jmt = annotateDedukti env djmt
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


-- result of conversion from informal Latex text, with intermediate phases for debugging
---- TODO: complete formalResults with type checking in Dedukti

data ParseResult = ParseResult {
  originalLine  :: String,
  lexedLine     :: String,
  termIndex     :: [String],
  indexedLine   :: String,
  parseMessage  :: String,
  unknownWords  :: [String],
  parseResults  :: [GFTree],
  formalResults :: [(GFTree, GFTree, GFTree, [Jmt])], -- parsed, unindexed, normalized
  transResults  :: [String] -- back-translation or translation to another language
  }


-- conversion that produces the whole line of parsing latex code and converting to Dedukti
-- so far assuming that parsing units are single lines not starting with \ or %

processLatex :: Env -> String -> [ParseResult]
processLatex env = map (processLatexLine env) . filter parsable . lines
 where
   parsable line = not (null line) && notElem (head line) "\\%" 

processLatexLine :: Env -> String -> ParseResult
processLatexLine env s =
  let
    gr = grammar env
    trans = isFlag "-translate" env
    ls = lextex s
    (ils, tindex) = indexTex ls
    Just jmt = readType "Jmt"
    (mts, msg) = parseJmt gr (fromLang env) jmt ils
    ts = maybe [] id mts
  in ParseResult {
    originalLine = s,
    lexedLine = lextex s,
    termIndex = tindex,
    indexedLine = ils,
    parseMessage = msg,
    unknownWords = missingWords env ils,
    parseResults = ts,
    formalResults = if trans then [] else [
      (t, ut, gf ct, gjmt2dedukti env ct) |
        t <- ts,
        let ut = unindexGFTree gr (fromLang env) tindex t,
        let ct = ext2core (fg ut)
      ],
    transResults = [
      unindexString tindex
        (unlex env (gftree2nat env (toLang env) t)) | t <- ts]
    }

-- conversions

dedukti2core :: Jmt -> GJmt
dedukti2core = DMC.jmt2core

annotateDedukti :: Env -> Jmt -> Jmt
annotateDedukti env t = annotateDkIdents (constantTable env) t

readConstantTable :: PGF -> [FilePath] -> IO (ConstantTable, ConversionTable)
readConstantTable = buildConstantTable

checkConstantTable :: Module -> PGF -> ConstantTable -> String
checkConstantTable mo gr ct = unlines (constantTableErrors mo gr ct)

printConstantTable :: ConstantTable -> String
printConstantTable = showConstantTable

printGenResult :: Env -> GenResult -> [String]
printGenResult env result = case 0 of
  _ | toFormalism env /= "NONE" ->
    [printFormalismJmt env (toFormalism env) (originalDedukti result)]
  _ | isFlag "-json" env -> [showJsonGenResult env result]
  _ | isFlag "-parallel-data" env -> [showParallelData env result] 
  _ -> printNLGOutput env result

printRank :: ((GFTree, String), (Scores, Int)) -> String
printRank ((tree, str), (scores, rank)) = mkJSONObject [
  mkJSONField "tree" (stringJSON (showExpr [] tree)),
  mkJSONField "lin" (stringJSON str),
  mkJSONField "scores" (stringJSON (show scores)),
  mkJSONField "penalty" (stringJSON (show rank))
  ]

printNLGOutput :: Env -> GenResult -> [String]
printNLGOutput env result = case (lookup (toLang env) (nlgResults result)) of
  Just phrases -> map (snd . fst) phrases
  _ -> error $ "language not available: " ++ (showCId (toLang env)) ++
               ". Available values: " ++ unwords (map showCId (langs env))

showJsonGenResult :: Env -> GenResult -> String
showJsonGenResult env result = mkJSONObject [
    mkJSONField "originalDedukti" (stringJSON (printTree (originalDedukti result))),
    mkJSONField "annotatedDedukti" (stringJSON (printTree (annotatedDedukti result))),
    mkJSONField "coreGF" (stringJSON (showExpr [] (coreGF result))),
    mkJSONField "nlgResults" (mkJSONObject [
      mkJSONListField (showCId lang) (map printRank ranks) | (lang, ranks) <- nlgResults result]),
    mkJSONListField "backToDedukti" [stringJSON (printTree jmt) | jmt <- backToDedukti result]
    ]

showParallelData :: Env -> GenResult -> String
showParallelData env result = mkJSONObject $ [
    mkJSONField formalism (stringJSON (printFormalismJmt env formalism (originalDedukti result)))
      | formalism <- formalisms env
  ] ++ [  
    mkJSONListField (showCId lang) (map (stringJSON . snd . fst) ranks)
      | (lang, ranks) <- nlgResults result
  ]

printFormalismJmt :: Env -> String -> Jmt -> String
printFormalismJmt env formalism jmt = case formalism of
  "agda" -> dedukti2agda env jmt
  "lean" -> dedukti2lean env jmt
  "rocq" -> dedukti2rocq env jmt
  "dedukti" -> printTree jmt
  f -> error $ "formalism not available: " ++ f ++ ". Available values: " ++ unwords (formalisms env)

printParseResult :: Env -> ParseResult -> [String]
printParseResult env result = case 0 of
  _ | toFormalism env /= "NONE" ->
    [printFormalismJmt env (toFormalism env) jmt | (_,_,_,jmts) <- formalResults result, jmt <- jmts]
  _ | isFlag "-translate" env ->
    transResults result
  _ | isFlag "-json" env -> [mkJSONObject [
    mkJSONField "originalLine" (stringJSON (originalLine result)),
    mkJSONField "lexedLine" (stringJSON (lexedLine result)),
    mkJSONListField "termIndex" (map stringJSON (termIndex result)),
    mkJSONField "indexedLine" (stringJSON (indexedLine result)),
    mkJSONField "parseMessage" (stringJSON (parseMessage result)),
    mkJSONListField "unknownWords" (map stringJSON (unknownWords result)),
    mkJSONListField "formalResults" (map printFinalParseResult (formalResults result))
    ]]
  _ -> printDeduktiOutput env result

printDeduktiOutput :: Env -> ParseResult -> [String]
printDeduktiOutput env result =
  [printTree jmt | (_,_,_,jmts) <- formalResults result, jmt <- jmts]

printFinalParseResult :: (GFTree, GFTree, GFTree, [Jmt]) -> String
printFinalParseResult (t, ut, ct, jmts) = mkJSONObject [
  mkJSONField "parseTree" (stringJSON (showExpr [] t)),
  mkJSONField "unindexedTree" (stringJSON (showExpr [] ut)),
  mkJSONField "coreTree" (stringJSON (showExpr [] ct)),
  mkJSONListField "dedukti" (map (stringJSON . printTree) jmts)
  ]

-- read base constants from one or more files; in translations, only one file
readDeduktiModule :: [FilePath] -> IO Module
readDeduktiModule files = mapM readFile files >>= return . parseDeduktiModule . unlines

parseDeduktiModule :: String -> Module
parseDeduktiModule s = case pModule (myLexer s) of
  Bad e -> error ("parse error: " ++ e)
  Ok mo -> mo
    
readGFGrammar :: FilePath -> IO PGF
readGFGrammar = readPGF

mkLanguage :: PGF -> String -> Language
mkLanguage pgf code = case readLanguage (informathPrefix ++ code) of
  Just lang | elem lang (languages pgf) -> lang
  _ -> error ("not a valid language: " ++ code)

checkJmt :: Jmt -> Bool
checkJmt jmt = True ----

core2ext :: Env -> GJmt -> [GJmt]
core2ext env jmt = MCI.nlg env jmt

gftree2nat :: Env -> Language -> GFTree -> String
gftree2nat env lang tree = linearize (grammar env) lang tree

unlex :: Env -> String -> String
unlex env s = if (isFlag "-no-unlex" env) then s else unlextex s

rankGFTreesAndNat :: Env -> [(Expr, String)] -> [((Expr, String), (Scores, Int))]
rankGFTreesAndNat = rankTreesAndStrings

ext2core :: GJmt -> GJmt
ext2core = IMC.semantics

gjmt2dedukti :: Env -> GJmt -> [Jmt] 
gjmt2dedukti env = MCD.jmt2dedukti (backConstantTable env) . ext2core

nat2core :: PGF -> Language -> String -> Maybe GJmt
nat2core pgf lang str = Nothing ----

missingWords :: Env -> String -> [String]
missingWords env = morphoMissing (morpho env) . tokens
 where
   tokens = ordinary [] . words
   ordinary acc ws = case ws of
     "$" : ww -> let (_, _:ww2) = break (=="$") ww in ordinary acc ww2
     "$$" : ww -> let (_, _:ww2) = break (=="$$") ww in ordinary acc ww2
     w : ww -> ordinary (w:acc) ww
     _ -> acc

ext2nat :: PGF -> Language -> GJmt -> Maybe String
ext2nat pgf lang jmt = Nothing ----

nat2ext :: PGF -> Language -> String -> [GJmt]
nat2ext pgf lang str = []

core2dedukti :: Env -> GJmt -> [Jmt]
core2dedukti env = MCD.jmt2dedukti (backConstantTable env)

-- these are syntactic conversions, therefore total
-- necessary imports have to be added to the generated files

dedukti2agda :: Env -> Jmt -> String
dedukti2agda env jmt = unlines [DA.printAgdaJmts (DA.transJmt (conv jmt))] where
  conv = maybe id alphaConvert (M.lookup "agda" (conversionTable env))

dedukti2lean :: Env -> Jmt -> String
dedukti2lean env jmt = unlines [DL.printLeanJmt (DL.transJmt (conv jmt))] where
  conv = maybe id alphaConvert (M.lookup "lean" (conversionTable env))

dedukti2rocq :: Env -> Jmt -> String
dedukti2rocq env jmt = unlines [DR.printRocqJmt (DR.transJmt (conv jmt))] where
  conv = maybe id alphaConvert (M.lookup "rocq" (conversionTable env))


---- checkAgda :: AJmt -> Bool
---- checkLean :: LJmt -> Bool
---- checkRocq :: RJmt -> Bool

---- agda2dedukti :: AJmt -> Jmt
---- lean2dedukti :: LJmt -> Jmt
---- rocq2dedukti :: RJmt -> Jmt

-- statistics
identsInDedukti :: Env -> Module -> [(String, Int)]
identsInDedukti env mo = map stringify (mfilter freqs) where
  stringify (c, i) = (printTree c, i)
  freqs = sortOn (\ (_,i) -> -i) (M.toList (identsInTypes mo))
  mfilter = if elem "-unknown-idents" (flags env) then filter (notInTable . fst) else id 
  notInTable qid = M.notMember qid (constantTable env) && not (all isDigit (printTree qid))

unknownWordsInTex :: Env -> String -> [(String, Int)]
unknownWordsInTex env = frequencyTable . missingWords env . lextex 

showFreqs :: [(String, Int)] -> [String]
showFreqs = map (\ (c, n) -> c ++ "\t" ++ show n)

readEnv :: [Flag] -> IO Env
readEnv args = do
  mo <- readDeduktiModule (argValues "-base" baseConstantFile args)
  gr <- readGFGrammar (argValue "-grammar" grammarFile args)
  (ct, cvt) <- readConstantTable gr (argValues "-constants" constantTableFile args)
  let fro = mkLanguage gr (argValue "-from-lang" english args)
  ifArg "-check-constant-table" args (checkConstantTable mo gr ct)
  return Env {
    flags = args,
    grammar = gr,
    constantTable = ct,
    conversionTable = cvt,
    synonymConstantTableNLG = buildSynonymConstantTableNLG ct,
    synonymConstantTableSem = buildSynonymConstantTableSem ct,
    backConstantTable = buildBackConstantTable ct,
    baseConstantModule = mo,
    formalisms = words "agda dedukti lean rocq",
    langs = languages gr, ---- relevantLanguages gr args,
    toLang = mkLanguage gr (argValue "-to-lang" english args),
    toFormalism = argValue "-to-formalism" "NONE" args,
    fromLang = fro,
    nbestNLG = argValueMaybeInt "-nbest" args,
    scoreWeights = commaSepInts (argValue "-weights" "1,1,1,1,1,1,1,1,1" args),
    morpho = buildMorpho gr fro
    }



