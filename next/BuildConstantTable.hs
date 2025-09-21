----module BuildConstantTable where

module Main where  ---- these only for indep testing
import Dedukti.ParDedukti
import Dedukti.LexDedukti
import Dedukti.PrintDedukti
import Dedukti.ErrM

import Dedukti.AbsDedukti
import PGF
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (partition)


constant_table_file = "constants.dkgf"
pgf_file = "grammars/Informath.pgf"
dk_file = "../src/BaseConstants.dk"

type Fun = CId
type Cat = CId

symbolicCats :: S.Set Cat
symbolicCats = S.fromList [mkCId c | c <- words "Formula Term Compar"]


type ConstantTable = M.Map QIdent ConstantTableEntry


data ConstantTableEntry = ConstantTableEntry {
  primary  :: (Fun, Type),
  symbolics :: [(Fun, Type)],
  synonyms :: [(Fun, Type)]
  }


allGFFuns :: ConstantTable -> QIdent -> [(Fun, Type)]
allGFFuns table qident = maybe [] merge $ M.lookup qident table where
  merge entry = primary entry : symbolics entry ++ synonyms entry


printConstantTable :: ConstantTable -> String
printConstantTable = concat . map prEntry . M.toList where
  prEntry :: (QIdent, ConstantTableEntry) -> String
  prEntry (QIdent q, entry) =
    unlines $ [
      q ++ ":"
      ] ++
      map ("  " ++) [
        "primary: " ++ prTyping (primary entry),
        "symbolics: " ++ unwords (map prTyping (symbolics entry)),
        "synonyms: " ++ unwords (map prTyping (synonyms entry))
      ]
  prTyping (fun, typ) = showCId fun ++ " : " ++ showType [] typ ++ " ;"


buildConstantTable :: FilePath -> Module -> PGF -> IO ConstantTable
buildConstantTable dkgf dk pgf = do
  entrylines <- readFile dkgf >>= return . map words . filter (not . null) . lines
  let table = M.fromList [
        (QIdent qid, mkConstantTableEntry pgf (map mkCId gfids)) | qid:gfids <- entrylines]
  checkConstantTable dk table
  return table


mkConstantTableEntry :: PGF -> [Fun] -> ConstantTableEntry
mkConstantTableEntry pgf (fun:funs) = ConstantTableEntry {
  primary = (fun, funtype fun),
  symbolics = [(f, typ) | (f, typ) <- symbs],
  synonyms = [(f, typ) | (f, typ) <- syns]
  }
 where
   funtype fun = maybe (error ("cannot infer type of " ++ showCId fun)) id (functionType pgf fun)
   (symbs, syns) = partition (isSymbolic . snd) [(f, funtype f) | f <- funs]
   isSymbolic typ = case unType typ of
     (_, cat, _) -> S.member cat symbolicCats


mismatchingTypes :: DkType -> Type -> Bool
mismatchingTypes dktyp gftyp = arityMismatch dktyp (unType gftyp) where
  arityMismatch (dkhypos, _) (gfhypos, cid, _) = length dkhypos /= arity gfhypos cid
  arity gfhypos cid = case showCId cid of
    s | elem s (words "Adj Verb Fun Fam Noun1") -> 1
    s | elem s (words "Adj2 Verb2 Noun2 Fun2 Fam2 Compar") -> 2
    s | elem s (words "Adj3") -> 3
    _ -> length gfhypos
    

 ---- TODO: check more than arity
 

checkConstantTable :: Module -> ConstantTable -> IO ()
checkConstantTable dk table = do
  let funs = deduktiFunctions dk
  let missing = [fun | (fun, _) <- funs, M.notMember fun table]
  mapM_ putStrLn $ ["MISSING IN TABLE: " ++ printTree fun | fun <- missing]
  let mismatches = [(dkfun, gffun) |
                      (dkfun, dktyp) <- funs,
		      (gffun, gftyp) <- allGFFuns table dkfun,
		      mismatchingTypes dktyp gftyp]
  mapM_ putStrLn $ ["MISMATCHING TYPES: " ++ printTree dkfun ++ " <> " ++ showCId gffun |
                      (dkfun, gffun) <- mismatches]
		      


type DkType = ([Dedukti.AbsDedukti.Hypo], Exp)

deduktiFunctions :: Module -> [(QIdent, DkType)]
deduktiFunctions (MJmts jmts) = concatMap getFun jmts where

  getFun :: Jmt -> [(QIdent, DkType)]
  getFun jmt = case jmt of
    JStatic fun typ -> return (fun, getDkType typ)
    JDef fun (MTExp typ) _ -> return (fun, getDkType typ)
    JInj fun (MTExp typ) _ -> return (fun, getDkType typ)
    JThm fun (MTExp typ) _ -> return (fun, getDkType typ)
    _ -> []
    
  getDkType :: Exp -> DkType
  getDkType typ = case typ of
    EFun hypo exp -> case getDkType exp of
      (hypos, val) -> (hypo:hypos, val)
    _ -> ([], typ)


--- for independent testing

parseDeduktiModule :: String -> IO Module
parseDeduktiModule s = do
  case pModule (myLexer s) of
    Bad e -> error ("parse error: " ++ e)
    Ok mo -> return mo

main = do
  pgf <- readPGF pgf_file
  dk <- readFile dk_file >>= parseDeduktiModule
  table <- buildConstantTable constant_table_file dk pgf
  putStrLn $ printConstantTable table




