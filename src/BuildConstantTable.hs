{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module BuildConstantTable where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti

import CommonConcepts
import DeduktiOperations

import PGF
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (partition, nub)


constant_table_file = "constants.dkgf"
pgf_file = "grammars/Informath.pgf"
dk_file = "../src/BaseConstants.dk"

type Fun = CId
type Cat = CId

symbolicCats :: S.Set Cat
symbolicCats = S.fromList [mkCId c | c <- words "Formula Term Compar Const Oper Oper2"]


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
    s | elem s (words "Adj Verb Fun Fam Noun1 Oper") -> 1
    s | elem s (words "Adj2 Verb2 Noun2 Fun2 Fam2 Compar Oper2") -> 2
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


type DkTree a = Dedukti.AbsDedukti.Tree a

-- annotate idents with cats and funs
annotateDkIdents :: ConstantTable -> DkTree a -> [DkTree a]
annotateDkIdents table t = nub (symbs t ++ verbs t) where

  symbs :: forall a. DkTree a -> [DkTree a]
  symbs t = case t of
    EApp _ _ -> case splitApp t of
      (EIdent c, xs) -> [foldl EApp (EIdent ac) xx |
	  ac <- strictAnnotId symbolics c,
	  xx <- sequence (map symbs xs)
	  ]
    EIdent c -> [EIdent ac | ac <- strictAnnotId symbolics c]
    _ -> composOpM symbs t
    
  verbs :: forall a. DkTree a -> [DkTree a]
  verbs t = case t of
    EApp _ _ -> case splitApp t of
      (EIdent c, xs) -> [foldl EApp (EIdent ac) xx |
	  ac <- annotId verbals c,
	  xx <- sequence (map (\x -> symbs x ++ verbs x) xs)
	  ]
    EIdent c -> [EIdent ac | ac <- annotId verbals c]
    EAbs b exp -> [EAbs b aexp | aexp <- verbs exp]
    _ -> composOpM verbs t

  annotId get c = case M.lookup c table of
    Just entry -> withDefault c [annotIdent c t f | (f, t) <- get entry]
    _ -> [c]
    
  strictAnnotId get c = case M.lookup c table of
    Just entry -> [annotIdent c t f | (f, t) <- get entry]
    _ -> []

  annotIdent :: QIdent -> Type -> Fun -> QIdent
  annotIdent (QIdent s) t f = QIdent (s ++ "&" ++ dk (valCat t) ++ "&" ++ dk f)

  verbals e = primary e : symbolics e

  withDefault d vs = if null vs then [d] else vs
  valCat t = case unType t of (_, c, _) -> c
  dk c = showCId c


-- deciding the kind of a new constant
guessGFCat :: QIdent -> Exp -> String
guessGFCat ident@(QIdent c) typ =
  let
    (hypos, val) = splitType typ
    arity = length hypos
  in case lookupConstant c of
    Just (cat, _) -> cat
    _ -> case splitApp val of
      (EIdent f, _) | f == identProp -> case arity of
        0 -> "Name" --- not really
        1 -> "Adj"
        2 -> "Adj2"
        3 -> "Adj3"
        _ -> "Fun"
      (EIdent f, _) | elem f [identSet, identType] -> case arity of
        0 -> "Noun"
        1 -> "Fam"
        _ -> "Fam2"
      (EIdent f, _) | f == identElem -> case arity of
        0 -> "Name"
        1 -> "Fun"
        _ -> "Fun2"
      (EIdent f, _) | f == identProof -> "Label"
      _ -> "UnresolvedConstant_" ++ c --- error ("Unresolved constant " ++ c)
