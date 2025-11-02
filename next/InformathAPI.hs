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
--import DeduktiOperations
--import ConstantData 
--import SpecialDeduktiConversions (specialDeduktiConversions)
------import Informath -- to be removed
--import Core2Informath (nlg)
--import Informath2Core (semantics)
import ParseInformath (parseJmt, unindexGFTree)
import Lexing
--import MkConstants (mkConstants)
--import qualified Dedukti2Agda as DA
--import qualified Dedukti2Rocq as DR
--import qualified Dedukti2Lean as DL

import Ranking4
import Environment4
import BuildConstantTable
import qualified DMC
import qualified MCI
import qualified IMC
import qualified MCD

import NextInformath
import PGF

import Data.List (nub, partition, isSuffixOf, isPrefixOf, intersperse, sortOn)
------import System.Random
--import Data.Char (isDigit, toUpper) --- low-level auxiliaries
--import System.Environment (getArgs)
--import System.IO
--import qualified Data.Map as M

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
  annotatedDedukti :: [Jmt],
  coreGF           :: [GFTree],
  nlgResults       :: [(Language, [((GFTree, String), (Scores, Int))])]
  }


-- conversion that produces the whole line of generation from Dedukti

processDeduktiModule :: Env -> Module -> [GenResult]
processDeduktiModule env (MJmts jmts) = map (processJmt env) jmts

processJmt :: Env -> Jmt -> GenResult
processJmt env jmt =
  let
    flag = flags env
    jmts = annotateDedukti env jmt
    cores = map dedukti2core jmts
    nlgs = nub $ map gf $ concatMap (core2ext env) cores
    best = maybe id take (nbestNLG env)
    nlglins lang = [(tree, unlex env (gftree2nat env lang tree)) | tree <- nlgs]
    nlgranks = [(lang, best (rankGFTreesAndNat env (nlglins lang))) | lang <- langs env]
  in GenResult {
    originalDedukti = jmt,
    annotatedDedukti = jmts,
    coreGF = map gf cores,
    nlgResults = nlgranks
    }


-- result of conversion from informal Latex text, with intermediate phases for debugging

data ParseResult = ParseResult {
  originalLine  :: String,
  lexedLine     :: String,
  termIndex     :: [String],
  indexedLine   :: String,
  parseMessage  :: String,
  unknownWords  :: [String],
  parseResults  :: [(GFTree, GFTree, GFTree, [Jmt])] -- parsed, unindexed, normalized
  }

----correctDeduktiResults :: ParseResult -> [Jmt]
----correctDeduktiResults result = [jmt | (_, _, _, jmts) <- parseResults result, (jmt, True) <- jmts]

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
    unknownWords = morphoMissing (morpho env) (words ils),
    parseResults = [
      (t, ut, gf ct, MCD.jmt2dedukti (backConstantTable env) ct) |
        t <- ts,
        let ut = unindexGFTree gr (fromLang env) tindex t,
        let ct = ext2core (fg ut)
      ]
    }

-- conversions

dedukti2core :: Jmt -> GJmt
dedukti2core = DMC.jmt2core

annotateDedukti :: Env -> Jmt -> [Jmt]
annotateDedukti env t =
  maybe id take (nbestDedukti env)
    (allAnnotateDkIdents (constantTable env) t)

readConstantTable :: PGF -> FilePath -> IO ConstantTable
readConstantTable = buildConstantTable

checkConstantTable :: Module -> PGF -> ConstantTable -> String
checkConstantTable mo gr ct = unlines (constantTableErrors mo gr ct)

printGenResult :: Env -> GenResult -> String
printGenResult env result = case 0 of
  _ | isFlag "-json" env -> mkJSONObject [
  
    mkJSONField "originalDedukti" (stringJSON (printTree (originalDedukti result))),
    mkJSONListField "annotatedDedukti" [stringJSON (printTree jmt) | jmt <- annotatedDedukti result],
    mkJSONListField "coreGF" [stringJSON (showExpr [] t) | t <- coreGF result],
    mkJSONListField "nlgResults" [
      mkJSONListField (showCId lang) (map printRank ranks) | (lang, ranks) <- nlgResults result]

    ]
  _ -> unlines $ printNLGOutput env result

printRank :: ((GFTree, String), (Scores, Int)) -> String
printRank ((tree, str), (scores, rank)) = mkJSONObject [
  mkJSONField "tree" (stringJSON (showExpr [] tree)),
  mkJSONField "lin" (stringJSON str),
  mkJSONField "scores" (stringJSON (show scores)),
  mkJSONField "penalty" (stringJSON (show rank))
  ]

printNLGOutput :: Env -> GenResult -> [String]
printNLGOutput env result =
  maybe ["language not available"] (map (snd . fst))
    (lookup (toLang env) (nlgResults result))


printParseResult :: Env -> ParseResult -> String
printParseResult env result = case 0 of
  _ | isFlag "-json" env -> mkJSONObject [
    mkJSONField "originalLine" (stringJSON (originalLine result)),
    mkJSONField "lexedLine" (stringJSON (lexedLine result)),
    mkJSONListField "termIndex" (map stringJSON (termIndex result)),
    mkJSONField "indexedLine" (stringJSON (indexedLine result)),
    mkJSONField "parseMessage" (stringJSON (parseMessage result)),
    mkJSONListField "unknownWords" (map stringJSON (unknownWords result)),
    mkJSONListField "parseResults" (map printFinalParseResult (parseResults result))
    ]
  _ -> unlines $ printDeduktiOutput env result

printDeduktiOutput :: Env -> ParseResult -> [String]
printDeduktiOutput env result = [printTree jmt | (_,_,_,jmts) <- parseResults result, jmt <- jmts]

printFinalParseResult :: (GFTree, GFTree, GFTree, [Jmt]) -> String
printFinalParseResult (t, ut, ct, jmts) = mkJSONObject [
  mkJSONField "parseTree" (stringJSON (showExpr [] t)),
  mkJSONField "unindexedTree" (stringJSON (showExpr [] ut)),
  mkJSONField "coreTree" (stringJSON (showExpr [] ct)),
  mkJSONListField "dedukti" (map (stringJSON . printTree) jmts)
  ]


readDeduktiModule :: FilePath -> IO Module
readDeduktiModule file = readFile file >>= return . parseDeduktiModule

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
core2ext env jmt = MCI.nlg (flags env) jmt

gftree2nat :: Env -> Language -> GFTree -> String
gftree2nat env lang tree = linearize (grammar env) lang tree

unlex :: Env -> String -> String
unlex env s = if (isFlag "-no-unlex" env) then s else unlextex s

rankGFTreesAndNat :: Env -> [(Expr, String)] -> [((Expr, String), (Scores, Int))]
rankGFTreesAndNat = rankTreesAndStrings

ext2core :: GJmt -> GJmt
ext2core = IMC.semantics

nat2core :: PGF -> Language -> String -> Maybe GJmt
nat2core pgf lang str = Nothing ----

ext2nat :: PGF -> Language -> GJmt -> Maybe String
ext2nat pgf lang jmt = Nothing ----

nat2ext :: PGF -> Language -> String -> [GJmt]
nat2ext pgf lang str = []

core2dedukti :: Env -> GJmt -> [Jmt]
core2dedukti env = MCD.jmt2dedukti (backConstantTable env)

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


readEnv :: [Flag] -> IO Env
readEnv args = do
  mo <- readDeduktiModule (argValue "-base" baseConstantFile args)
  gr <- readGFGrammar (argValue "-grammar" grammarFile args)
  ct <- readConstantTable gr (argValue "-constants" constantTableFile args)
  let fro = mkLanguage gr (argValue "-fromlang" english args)
  ifArg "-check-constant-table" args (checkConstantTable mo gr ct)
  return Env {
    flags = args,
    grammar = gr,
    constantTable = ct,
    backConstantTable = constantTableBack ct,
    baseConstantModule = mo,
    langs = relevantLanguages gr args,
    toLang = mkLanguage gr (argValue "-tolang" english args),
    fromLang = fro,
    nbestDedukti = argValueMaybeInt "-nbestdk" args,
    nbestNLG = argValueMaybeInt "-nbest" args,
    scoreWeights = commaSepInts (argValue "weights" "1,1,1,1,1,1,1,1,1" args),
    morpho = buildMorpho gr fro
    }



