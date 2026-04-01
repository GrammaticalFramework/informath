{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module BuildConstantTable where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti

import CommonConcepts
import DeduktiOperations
import Lexing (lextex)

import PGF

import Utils

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (partition, sortOn, sort, groupBy, intersperse)
import Data.Char (isDigit)

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
    t:_ -> t ---- TODO: if many parses?
  _ -> readGFTree s

parseExample :: PGF -> Language -> String -> [Expr]
parseExample pgf lang =
    map extract . parse pgf lang (maybe undefined id (readType "Example")) . lexex
  where
   extract t = case unApp t of
     Just (_, ex:_) -> ex
     _ -> error $ "cannot get example from: " ++ showGFTree t

   lexex s = case s of
     c:' ':cs -> c : ' ' :lextex cs  -- don't uncap first letter
     c:cs -> c : lextex cs  -- don't uncap first letter
     _ -> s


readGFTree :: String -> GFTree
readGFTree s = case s of
  '\\':_ -> mkApp (mkCId s) []
  _ -> maybe (error ("cannot parse as GFTree: " ++ s)) id (readExpr s)

------

-- conversion from Dk to GF, with synonyms and category information
type ConstantTable = M.Map QIdent ConstantTableEntry

-- conversion from Dk to other formalisms
type ConversionTable = M.Map Formalism (M.Map QIdent QIdent)

-- definitions of macros to be converted to \newcommand in LaTeX
type MacroTable = M.Map String (Int, String)

data ConstantTableEntry = ConstantTableEntry {
  primary   ::  (Fun, (Type, Profile)),
  symbolics :: [(Fun, (Type, Profile))],
  synonyms  :: [(Fun, (Type, Profile))]
  }

allGFFuns :: ConstantTable -> QIdent -> [(Fun, (Type, Profile))]
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
  prTyping (fun, (typ, prof)) = showGFTree fun ++ " : " ++ showType [] typ ++ " " ++ showProfile prof ++ " ;"


type BackConstantTable = M.Map Fun [(QIdent, Profile)] -- maps GF idents to original "Dk" idents

type BuiltinSet = S.Set QIdent -- built-in constants not expected in ConstantTable

buildBackConstantTable :: ConstantTable -> BackConstantTable
buildBackConstantTable table = M.fromListWith (++) [
  (fun, [(qid, prof)]) | 
    (qid, entry) <- M.toList table,
    (fun, (_, prof)) <- primary entry : symbolics entry ++ synonyms entry
  ]

---- TODO: make this accessible from RunInformath
printBackTable ::  BackConstantTable -> String
printBackTable = unlines . map prEntry . M.toList where
  prEntry :: (Fun, [(QIdent, Profile)]) -> String
  prEntry (f, qids) = showGFTree f ++ ": " ++ unwords [g ++ showProfile prof | (QIdent g, prof) <- qids]

buildConstantTable :: PGF -> Language -> [String] ->
    (ConstantTable, ConversionTable, MacroTable, BuiltinSet)
buildConstantTable pgf lang ls =
    (constantTable, conversionTable, macroTable, builtinSet)
  where
    entrylines = filter (not . null) (map words ls)
    constantlines = filter isConstantEntry entrylines
    conversionlines = filter isConversion entrylines
    macrolines = filter isMacro entrylines
    builtinlines = filter isBuiltin entrylines
    constantTable = M.fromList [
        (QIdent qid, mkConstantTableEntry pgf (map funprof gfids)) |
                         qid:gfids@(_:_) <- map (splitEntry . unwords) constantlines]
    conversionTable = M.fromList [
        (form, M.fromList [(QIdent d, QIdent f) | _:d:f:_ <- fids]) |
	    fids@((form:_):_) <- groupBy (\x y -> head x == head y) (sort (map tail conversionlines))]
    macroTable = M.fromList [(c, (read n, d)) | _:rest <- macrolines, let [c, n, d] = splitNewcommand (unwords rest)]
    builtinSet = S.fromList [QIdent c | _:cs <- builtinlines, c <- cs]

    funprof s = case break (=='[') s of
      (c, [])  -> (parseGFTree pgf lang c, NoProfile)
      (c, _:p) -> (parseGFTree pgf lang c, readProfile (init p)) -- fun, profile is fun [...]
     
    isConstantEntry line = head (head line) /= '#'
    isConversion line = head line == "#CONV"
    isMacro line = head line == "#MACRO"
    isBuiltin line = head line == "#BUILTIN"

splitEntry s = case split '|' s of
  fg : ws -> split ':' fg ++ ws
  _ -> []

-- works on \newcommand{\foo}[n]{def}, also \renewcommand and with no [n]
splitNewcommand :: String -> [String]
splitNewcommand s = case break (=='{') s of
  (_, _:rest) -> case break (=='}') rest of
    (macro, _:mrest) -> macro : case mrest of
      '[':nrest -> case break (==']') nrest of
        (n@(_:_), _:drest) | all isDigit n -> n : [init (tail drest)]
      _ -> "0" : [init (tail mrest)]
    _ -> error ("expected valid newcommand, found: " ++ s)
  _ -> error ("expected valid newcommand, found: " ++ s)


mkConstantTableEntry :: PGF -> [(Fun, Profile)] -> ConstantTableEntry
mkConstantTableEntry _ [] = error "constant table entry cannot be empty"
mkConstantTableEntry pgf ((fun, prof) : funs) = ConstantTableEntry {
  primary = (fun, (funtype pgf fun, prof)),
  symbolics = [(f, typ) | (f, typ) <- symbs],
  synonyms = [(f, typ) | (f, typ) <- syns]
  }
 where
 
   funtype fun = inferFunType pgf

   (symbs, syns) = partition (isSymbolic . fst . snd) [(f, (funtype pgf f, p)) | (f, p) <- funs]
   isSymbolic typ = case unType typ of
     (_, cat, _) | showCId cat == "MACRO" -> True 
     (_, cat, _) -> S.member (showCId cat) symbolicCats


-- PGF: inferExpr :: PGF -> Expr -> Either TcError (Expr, Type)

inferFunType :: PGF -> Fun -> Type
inferFunType pgf fun = case inferExpr pgf fun of
  Right (_, typ) -> typ
  _ -> case showGFTree fun of
     '\'':'\\':_ -> mkType [] (mkCId "MACRO") []
     _ -> error ("when building symbol table entry: cannot infer type of " ++ showGFTree fun)


type DkType = ([Dedukti.AbsDedukti.Hypo], Exp)

mismatchingTypes :: MacroTable -> DkType -> Type -> Fun -> Maybe ((String, Int), ([String], Int))
mismatchingTypes mt dktyp gftyp fun = arityMismatch dktyp (unType gftyp) where

  arityMismatch (dkhypos, typ) (gfhypos, cid, _) =
    let (dka, gfa) = (dkArity dkhypos typ, (gfCats cid, gfArity gfhypos cid)) in
    if not (compatible dka gfa)
    then Just (dka, gfa)
    else Nothing
    
  compatible (dkcat, dar) (gfcats, gar) = dar == gar && elem dkcat gfcats
  
  gfArity gfhypos cid = case showCId cid of
    "MACRO" -> maybe 0 fst (M.lookup (tail (filter (/='\'') (showGFTree fun))) mt)
    c | S.member c mainCats -> length gfhypos
    c -> case M.lookup c gfCatMap of
      Just (_, args) -> length args
      _ -> length gfhypos
      
  gfCats cid = case showCId cid of
    "MACRO" -> S.toList mainCats --- uncertain; any may work
    c -> case M.lookup c gfCatMap of
       Just ("Exp", _) -> ["Exp", "Kind"]
       Just ("Kind", _) -> ["Exp", "Kind"]
       Just (val, _) -> [val]
       _ -> ["UNKNOWN-GF"] ---
       
  dkArity dkhypos typ = (valCat typ, foldl (+) 0 (map hypoArity dkhypos))
  valCat typ = case fst (splitApp typ) of
    EIdent f | f == identProp -> "Prop"
    EIdent f | elem f [identSet, identType] -> "Kind"
    EIdent f | f == identElem -> "Exp"
    EIdent f | f == identProof -> "Proof"
    _ -> "UNKNOWN-DK" ---- default, but not accurate

  hypoArity hypo = maybe 1 ((+1) . length . fst . splitType) (hypo2type hypo) -- for HOAS
    
constantTableErrors :: Module -> PGF -> MacroTable -> ConstantTable -> BuiltinSet -> [String]
constantTableErrors dk pgf mt table bset = 
  let funs = deduktiFunctions dk
      missing = [fun | (fun, _) <- funs, M.notMember fun table, S.notMember fun bset]
      mismatches = [((dkfun, gffun), (e, f)) |
                      (dkfun, (hypos, valtype)) <- funs,
		      let dktyp = (hypos, valtype),
		      (gffun, (gftyp, prof)) <- allGFFuns table dkfun,    ----- TODO: deal with prof
		      Just (e, f) <- [mismatchingTypes mt dktyp gftyp gffun]]
  in 
    ["MISSING IN TABLE: " ++ printTree fun | fun <- missing] ++
    [unwords ["MISMATCHING TYPES:", printTree dkfun,
              show e, "<>", showGFTree gffun, show f] |
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
annotateDkIdents :: ConstantTable -> DkTree a -> DkTree a
annotateDkIdents table = annot [] where

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

annotIdent :: QIdent -> (Fun, (Type, Profile)) -> QIdent
annotIdent (QIdent s) (f, (t, p)) =
  QIdent $ concat $ intersperse "#" $ [s, dk (valCat t), dkp f] ++ map dk (argCats t) ++ [showProfile p]
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
      (EIdent (QIdent f), _) -> case takeWhile (/='#') f of
        k | QIdent k == identProp -> "Prop"
        k | elem (QIdent k) [identSet, identType] -> "Kind"
        k | QIdent k == identElem -> "Exp"
        k | QIdent k == identProof -> "Label"
        _ -> "Label" 

macroCommands :: MacroTable -> [String]
macroCommands t = [concat ["\\newcommand{", c, "}", arity n, "{", d, "}"] | (c, (n, d)) <- M.assocs t]
  where
    arity n = if n==0 then "" else "[" ++ show n ++ "]"

