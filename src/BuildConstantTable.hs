{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module BuildConstantTable where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti

import CommonConcepts
import DeduktiOperations
import ParseInformath (parseExample)

import PGF

import Utils

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (partition, sortOn, sort, groupBy, intersperse)

type GFTree = PGF.Tree
type Fun = GFTree
type Cat = CId
type Formalism = String

showGFTree :: GFTree -> String
showGFTree = showExpr []

--- OK to fail, because it should stop compilation
parseGFTree :: PGF -> Language -> String -> GFTree
parseGFTree pgf lang s = case s of
  '"':cs -> case parseExample pgf lang (init cs) of
    [] -> error $ "cannot parse example: " ++ s
    t:_ -> extract t ---- TODO: if many parses?
  _ -> readGFTree s
 where
   extract t = case unApp t of
     Just (_, ex:_) -> ex
     _ -> error $ "cannot get example from: " ++ showGFTree t 

readGFTree :: String -> GFTree
readGFTree s = case s of
  '\\':_ -> mkApp (mkCId s) []
  _ -> maybe (error ("cannot parse as GFTree: " ++ s)) id (readExpr s)

------

mainCats :: S.Set Cat
mainCats = S.fromList [mkCId c | c <- words
  "Exp Prop Kind Proof ProofExp Unit"
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

-- definitions of macros to be converted to \newcommand in LaTeX
type MacroTable = M.Map String (Int, String)

data ConstantTableEntry = ConstantTableEntry {
  primary   ::  (Fun, Type),
  symbolics :: [(Fun, Type)],
  synonyms  :: [(Fun, Type)]
  }

allGFFuns :: ConstantTable -> QIdent -> [(Fun, Type)]
allGFFuns table qident = maybe [] merge $ M.lookup qident table where
  merge entry = primary entry : symbolics entry ++ synonyms entry

-- shown in the form that can be parsed as a constant table
showConstantTable :: ConstantTable -> String
showConstantTable = unlines . map prEntry . M.toList where
  prEntry :: (QIdent, ConstantTableEntry) -> String
  prEntry (QIdent q, entry) =
    unwords $ [q, ":"] ++
    intersperse "|" (map (showGFTree . fst) (primary entry : synonyms entry ++ symbolics entry))

-- shown in a longer format; not currently used
showConstantTableLong :: ConstantTable -> String
showConstantTableLong = concat . map prEntry . M.toList where
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
  prTyping (fun, typ) = showGFTree fun ++ " : " ++ showType [] typ ++ " ;"


type BackConstantTable = M.Map Fun [QIdent] -- maps GF idents to original "Dk" idents

buildBackConstantTable :: ConstantTable -> BackConstantTable
buildBackConstantTable table = M.fromListWith (++) [
  (fun, [qid]) | 
    (qid, entry) <- M.toList table,
    fun <- map fst (primary entry : symbolics entry ++ synonyms entry)
  ]

---- TODO: make this accessible from RunInformath
printBackTable ::  BackConstantTable -> String
printBackTable = unlines . map prEntry . M.toList where
  prEntry :: (Fun, [QIdent]) -> String
  prEntry (f, qids) = showGFTree f ++ ": " ++ unwords [g | QIdent g <- qids]

buildConstantTable :: PGF -> Language -> [FilePath] -> IO (ConstantTable, ConversionTable, DropTable, MacroTable)
buildConstantTable pgf lang dkgfs = do
  entrylines <- mapM readFile dkgfs >>= return . filter (not . null) . map words . concatMap lines
  let constantlines = filter isConstantEntry entrylines
  let conversionlines = filter isConversion entrylines
  let droplines = filter isDrop entrylines
  let macrolines = filter isMacro entrylines
  let constantTable = M.fromList [
        (QIdent qid, mkConstantTableEntry pgf (map (parseGFTree pgf lang) gfids)) |
                     qid:gfids@(_:_) <- map (splitEntry . unwords) constantlines]
  let conversionTable = M.fromList [
        (form, M.fromList [(QIdent d, QIdent f) | _:d:f:_ <- fids]) |
	    fids@((form:_):_) <- groupBy (\x y -> head x == head y) (sort (map tail conversionlines))]
  let dropTable = M.fromList [(QIdent c, read n) | _:c:n:_ <- droplines]
  let macroTable = M.fromList [(c, (read n, unwords rest)) | _:c:n:rest <- macrolines]
  return (constantTable, conversionTable, dropTable, macroTable)
 where
   isConstantEntry line = head (head line) /= '#'
   isConversion line = head line == "#CONV"
   isDrop line = head line == "#DROP"
   isMacro line = head line == "#MACRO"

splitEntry s = case split '|' s of
  fg : ws -> split ':' fg ++ ws
  _ -> []

--- Data.List.Split cannot be found...
split :: Char -> String -> [String]
split c cs = case break (==c) cs of
  ([], []) -> []
  (s,  []) -> [strip s]
  (s, _:s2) -> strip s : split c s2

strip = unwords . words

mkConstantTableEntry :: PGF -> [Fun] -> ConstantTableEntry
mkConstantTableEntry _ [] = error "constant table entry cannot be empty"
mkConstantTableEntry pgf (fun : funs) = ConstantTableEntry {
  primary = (fun, funtype pgf fun),
  symbolics = [(f, typ) | (f, typ) <- symbs],
  synonyms = [(f, typ) | (f, typ) <- syns]
  }
 where
 
   funtype fun = inferFunType pgf

   (symbs, syns) = partition (isSymbolic . snd) [(f, funtype pgf f) | f <- funs]
   isSymbolic typ = case unType typ of
     (_, cat, _) -> S.member cat symbolicCats


-- PGF: inferExpr :: PGF -> Expr -> Either TcError (Expr, Type)

inferFunType :: PGF -> Fun -> Type
inferFunType pgf fun = case inferExpr pgf fun of
  Right (_, typ) -> typ
  _ -> case showGFTree fun of
     '\'':'\\':_ -> mkType [] (mkCId "MACRO") []
     _ -> error ("cannot infer type of " ++ showGFTree fun)


type DkType = ([Dedukti.AbsDedukti.Hypo], Exp)

mismatchingTypes :: MacroTable -> DkType -> Type -> Fun -> Maybe (Int, Int)
mismatchingTypes mt dktyp gftyp fun = arityMismatch dktyp (unType gftyp) where
  arityMismatch (dkhypos, _) (gfhypos, cid, _) = if dka /= gfa then Just (dka, gfa) else Nothing
    where
      dka = dkArity dkhypos
      gfa = gfArity gfhypos cid
  gfArity gfhypos cid = case showCId cid of
    s | elem s (words "Adj Verb Fun Fam Noun1 Oper") -> 1
    s | elem s (words "Adj2 Verb2 Noun2 Fun2 Fam2 Compar Oper2 FunC AdjC AdjE") -> 2
    s | elem s (words "Adj3") -> 3
    s | elem s (words "MACRO") ->   --- '\\foo' -> \foo 
      maybe 0 fst (M.lookup (tail (filter (/='\'') (showGFTree fun))) mt)
    _ -> length gfhypos
  dkArity dkhypos = foldl (+) 0 (map hypoArity dkhypos)
  hypoArity hypo = maybe 1 ((+1) . length . fst . splitType) (hypo2type hypo) -- for HOAS
    

 ---- TODO: check more than arity

constantTableErrors :: Module -> PGF -> MacroTable -> ConstantTable -> [String]
constantTableErrors dk pgf mt table = 
  let funs = deduktiFunctions dk
      missing = [fun | (fun, _) <- funs, M.notMember fun table]
      mismatches = [((dkfun, gffun), (e, f)) |
                      (dkfun, dktyp) <- funs,
		      (gffun, gftyp) <- allGFFuns table dkfun,
		      Just (e, f) <- [mismatchingTypes mt dktyp gftyp gffun]]
  in 
    ["MISSING IN TABLE: " ++ printTree fun | fun <- missing] ++
    [unwords ["MISMATCHING TYPES:", printTree dkfun, show e, "<>", showGFTree gffun, show f] |
                      ((dkfun, gffun), (e, f)) <- mismatches]

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

-- annotate idents with cats and funs, just the primary ; used only internally
annotateDkIdents :: ConstantTable -> DropTable -> DkTree a -> DkTree a
annotateDkIdents table drops = annot [] . ignoreFirstArguments drops where

  -- don't annotate bound variables: they override constants
  annot :: forall a. [QIdent] -> DkTree a -> DkTree a
  annot bounds t = case t of
    QIdent _ | notElem t bounds -> annotId t
    EAbs b exp -> EAbs (annot bounds b) (annot (bind2ident b : bounds) exp)
    EFun h exp -> EFun (annot bounds h) (annot (hypo2topvars h ++ bounds) exp)    
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

annotIdent :: QIdent -> (Fun, Type) -> QIdent
annotIdent (QIdent s) (f, t) =
  QIdent $ concat $ intersperse "#" $ [s, dk (valCat t), dkp f] ++ map dk (argCats t)
    where
      dk c = showCId c
      dkp f = showGFTree f

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


macroCommands :: MacroTable -> [String]
macroCommands t = [concat ["\\newcommand{", c, "}", arity n, "{", d, "}"] | (c, (n, d)) <- M.assocs t]
  where
    arity n = if n==0 then "" else "[" ++ show n ++ "]"
