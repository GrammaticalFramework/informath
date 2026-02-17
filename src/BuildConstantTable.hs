{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module BuildConstantTable where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti

import CommonConcepts
import DeduktiOperations

import PGF

import Utils

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (partition, sortOn, sort, groupBy, intersperse)

type Fun = CId
type Cat = CId
type Formalism = String

-- preposition-extended symbol table: Adj+toPrep -> Adj2
type FunPrep = (CId, [CId])

showFunPrep :: FunPrep -> String
showFunPrep (f, ps) = concat $ intersperse "+" $ map showCId (f:ps)

readFunPrep :: String -> FunPrep
readFunPrep ff = case words (map (\c -> if c=='+' then ' ' else c) ff) of
  f:fs -> (mkCId f, map mkCId fs)

splitFunPrep :: String -> (String, [String])
splitFunPrep ff = case words (map (\c -> if c=='+' then ' ' else c) ff) of
  f:fs -> (f, fs)

funPrepQIdent :: (String, [String]) -> QIdent
funPrepQIdent = QIdent . funPrepString

funPrepString :: (String, [String]) -> String
funPrepString (f, ps) = concat (intersperse "+" (f:ps))

------

mainCats :: S.Set Cat
mainCats = S.fromList [mkCId c | c <- words
  "Exp Prop Kind Proof ProofExp"
  ]

symbolicCats :: S.Set Cat
symbolicCats = S.fromList [mkCId c | c <- words
  "Formula Term Compar Const Oper Oper2 MACRO"  --- MACRO is not a cat in GF
  ]

verbalCats :: S.Set Cat
verbalCats = S.fromList [mkCId c | c <- words
  "Name Noun Noun1 Noun2 Fam Fam2 Adj Adj2 Adj3 AdjC AdjE Fun Fun2 FunC Verb Verb2 Label"
  ]

-- conversion from Dk to GF, with synonyms and category information
type ConstantTable = M.Map QIdent ConstantTableEntry

-- conversion from Dk to other formalisms
type ConversionTable = M.Map Formalism (M.Map QIdent QIdent)

-- conversions in Dk that drop a number of initial arguments
type DropTable = M.Map QIdent Int

data ConstantTableEntry = ConstantTableEntry {
  primary  :: (FunPrep, Type),
  symbolics :: [(FunPrep, Type)],
  synonyms :: [(FunPrep, Type)]
  }


allGFFuns :: ConstantTable -> QIdent -> [(FunPrep, Type)]
allGFFuns table qident = maybe [] merge $ M.lookup qident table where
  merge entry = primary entry : symbolics entry ++ synonyms entry


showConstantTable :: ConstantTable -> String
showConstantTable = concat . map prEntry . M.toList where
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
  prTyping (fun, typ) = showFunPrep fun ++ " : " ++ showType [] typ ++ " ;"

-- looking for synonyms of primary constants in NLG
--- strings, which are arguments of Lex* constructors
type SFun = String
type SCat = String

type SynonymConstantTableNLG = M.Map SFun ([(SFun, SCat)], [(SFun, SCat)])

buildSynonymConstantTableNLG :: ConstantTable -> SynonymConstantTableNLG
buildSynonymConstantTableNLG table = M.fromList [
  (showFunPrep fun, (sfcs, vfcs)) | 
    (_, entry) <- M.toList table,
    let fun = fst (primary entry),
    let sfcs = [(showFunPrep f, showCId (valCat typ)) | (f, typ) <- symbolics entry],
    let vfcs = [(showFunPrep f, showCId (valCat typ)) | (f, typ) <- synonyms entry]
  ]

-- looking for core constants for their synonyms in semantics
type SynonymConstantTableSem = M.Map SFun [(SFun, SCat)]

buildSynonymConstantTableSem :: ConstantTable -> SynonymConstantTableSem
buildSynonymConstantTableSem table = M.fromListWith (++) [
  (showFunPrep fun, [fc]) | 
    (_, entry) <- M.toList table,
    (fun, _) <- symbolics entry ++ synonyms entry,
    let (f, typ) = primary entry,
    let fc = (showFunPrep f, showCId (valCat typ))
  ]

type BackConstantTable = M.Map QIdent [QIdent] -- maps "Adj+Prep" ident to original "Dk" idents

buildBackConstantTable :: ConstantTable -> BackConstantTable
buildBackConstantTable table = M.fromListWith (++) [
  (QIdent (showFunPrep fun), [qid]) | 
    (qid, entry) <- M.toList table,
    fun <- map fst (primary entry : symbolics entry ++ synonyms entry)
  ]

printBackTable ::  BackConstantTable -> String
printBackTable = unlines . map prEntry . M.toList where
  prEntry :: (QIdent, [QIdent]) -> String
  prEntry (QIdent f, qids) = f ++ ": " ++ unwords [g | QIdent g <- qids]

buildConstantTable :: PGF -> [FilePath] -> IO (ConstantTable, ConversionTable, DropTable)
buildConstantTable pgf dkgfs = do
  entrylines <- mapM readFile dkgfs >>= return . filter (not . null) . map words . concatMap lines
  let constantlines = filter isConstantEntry entrylines
  let conversionlines = filter isConversion entrylines
  let droplines = filter isDrop entrylines
  let constantTable = M.fromList [
        (QIdent qid, mkConstantTableEntry pgf (map readFunPrep gfids)) | qid:gfids <- constantlines]
  let conversionTable = M.fromList [
        (form, M.fromList [(QIdent d, QIdent f) | _:d:f:_ <- fids]) |
	    fids@((form:_):_) <- groupBy (\x y -> head x == head y) (sort (map tail conversionlines))]
  let dropTable = M.fromList [(QIdent c, read n) | _:c:n:_ <- droplines]
  return (constantTable, conversionTable, dropTable)
 where
   isConstantEntry line = head (head line) /= '#'
   isConversion line = head line == "#CONV"
   isDrop line = head line == "#DROP"


mkConstantTableEntry :: PGF -> [FunPrep] -> ConstantTableEntry
mkConstantTableEntry pgf (funps:funs) = ConstantTableEntry {
  primary = (funps, funtype funps),
  symbolics = [(f, typ) | (f, typ) <- symbs],
  synonyms = [(f, typ) | (f, typ) <- syns]
  }
 where
 
   funtype (fun, ps) = case functionType pgf fun of
     Just typ -> case ps of
     {- -- don't change the type here, but in Dedukti2MathCore
       [_] -> case showType [] typ of
         "Adj" -> mkType [] (mkCId "Adj2") []
         "Noun" -> mkType [] (mkCId "Fun") []
	 sty -> error ("preposition-extended lexicon: expected Adj or Noun, found " ++ sty)
      -}
       _ -> typ
     _ -> case showCId fun of
       '\'':'\\':_ -> mkType [] (mkCId "MACRO") []
       _ -> error ("cannot infer type of " ++ showCId fun)
     
   (symbs, syns) = partition (isSymbolic . snd) [(f, funtype f) | f <- funs]
   isSymbolic typ = case unType typ of
     (_, cat, _) -> S.member cat symbolicCats


mismatchingTypes :: DkType -> Type -> Bool
mismatchingTypes dktyp gftyp = arityMismatch dktyp (unType gftyp) where
  arityMismatch (dkhypos, _) (gfhypos, cid, _) = dkArity dkhypos /= gfArity gfhypos cid
  gfArity gfhypos cid = case showCId cid of
    s | elem s (words "Adj Verb Fun Fam Noun1 Oper") -> 1
    s | elem s (words "Adj2 Verb2 Noun2 Fun2 Fam2 Compar Oper2 FunC AdjC AdjE") -> 2  ---- C can be >2
    s | elem s (words "Adj3") -> 3
    _ -> length gfhypos
  dkArity dkhypos = foldl (+) 0 (map hypoArity dkhypos)
  hypoArity hypo = maybe 1 ((+1) . length . fst . splitType) (hypo2type hypo) -- for HOAS
    

 ---- TODO: check more than arity

constantTableErrors :: Module -> PGF -> ConstantTable -> [String]
constantTableErrors dk pgf table = 
  let funs = deduktiFunctions dk
      missing = [fun | (fun, _) <- funs, M.notMember fun table]
      mismatches = [(dkfun, gffun) |
                      (dkfun, dktyp) <- funs,
		      (gffun, gftyp) <- allGFFuns table dkfun,
		      mismatchingTypes dktyp gftyp]
  in 
    ["MISSING IN TABLE: " ++ printTree fun | fun <- missing] ++
    ["MISMATCHING TYPES: " ++ printTree dkfun ++ " <> " ++ showFunPrep gffun |
                      (dkfun, gffun) <- mismatches]
		      


type DkType = ([Dedukti.AbsDedukti.Hypo], Exp)

deduktiFunctions :: Module -> [(QIdent, DkType)]
deduktiFunctions (MJmts jmts) = concatMap getFun jmts where

  getFun :: Jmt -> [(QIdent, DkType)]
  getFun jmt = case jmt of
    JStatic fun typ -> return (fun, splitType typ)
    JDef fun (MTExp typ) _ -> return (fun, splitType typ)
    JInj fun (MTExp typ) _ -> return (fun, splitType typ)
    JThm fun (MTExp typ) _ -> return (fun, splitType typ)
    _ -> []
    
type DkTree a = Dedukti.AbsDedukti.Tree a

-- annotate idents with cats and funs, just the primary
annotateDkIdents :: ConstantTable -> DropTable -> DkTree a -> DkTree a
annotateDkIdents table drops = annot [] . ignoreFirstArguments drops where

  annot :: forall a. [QIdent] -> DkTree a -> DkTree a
  annot bounds t = case t of
    QIdent _ | notElem t bounds -> annotId t
    EAbs b exp -> EAbs (annot bounds b) (annot (bind2ident b : bounds) exp)
    EFun h exp -> EFun (annot bounds h) (annot (hypo2topvars h ++ bounds) exp)
    
    -- don't annotate bound variables
    BVar _ -> t
    BTyped v ty -> BTyped v (annot bounds ty)
    HVarExp v ty -> HVarExp v (annot bounds ty)
    HParVarExp v ty -> HParVarExp v (annot bounds ty)
    HLetExp v ty -> HLetExp v (annot bounds ty)
    HLetTyped v ty exp -> HLetTyped v (annot bounds ty) (annot bounds exp)

    _ -> composOp (annot bounds) t

  annotId c = case M.lookup c table of
    Just entry -> annotIdent c (primary entry)
    _ -> c

annotIdent :: QIdent -> (FunPrep, Type) -> QIdent
annotIdent (QIdent s) (f, t) =
  QIdent $ concat $ intersperse "#" $ [s, dk (valCat t), dkp f] ++ map dk (argCats t)
    where
      dk c = showCId c
      dkp f = showFunPrep f

valCat :: Type -> Cat
valCat t = case unType t of (_, c, _) -> c

argCats :: Type -> [Cat]
argCats t = case unType t of (hs, _, _) -> [valCat h | (_, _, h) <- hs]

-- deciding the kind of a new constant introduced in a judgement
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
        _ -> "AdjC"
      (EIdent f, _) | elem f [identSet, identType] -> case arity of
        0 -> "Noun"
        1 -> "Fam"
        _ -> "Fam2"
      (EIdent f, _) | f == identElem -> case arity of
        0 -> "Name"
        1 -> "Fun"
        2 -> "Fun2"
        _ -> "FunC"
      (EIdent f, _) | f == identProof -> "Label"
      _ -> "Label"  --- default, assuming proof labels are often not linearized 
      

