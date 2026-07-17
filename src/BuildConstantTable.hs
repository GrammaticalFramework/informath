{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module BuildConstantTable where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import Dedukti.ErrM

import CommonConcepts
import DeduktiOperations
import Lexing (lextex)
import Semantics

import PGF

import Utils

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (partition, sortOn, sort, groupBy, intersperse)
import Data.Char (isDigit, isAlpha)

type GFTree = PGF.Tree
type Fun = GFTree
type FunProfile = (Fun, Profile)
type Cat = CId
type Formalism = String

showGFTree :: GFTree -> String
showGFTree = showExpr []

showFunProfile :: FunProfile -> String
showFunProfile (f, p) = showGFTree f ++ showProfile p

-- OK to fail, because it should stop compilation
parseFunProfile :: PGF -> Language -> Maybe Int -> String -> FunProfile
parseFunProfile pgf lang mdrop s = case tryParseFunProfile pgf lang mdrop s of
  Ok fp -> fp
  Bad s -> error s

-- used for parsing line by line
tryParseFunProfile :: PGF -> Language -> Maybe Int -> String -> Err FunProfile
tryParseFunProfile pgf lang mdrop s = case s of
  '"':cs -> case parseExample pgf lang (init cs) of
    [] -> Bad $ "cannot parse example: " ++ s
    tp@(t, p):_ -> case (mdrop, p) of   ---- TODO: if many parses?
      (Nothing, _) -> return tp
      (Just k, NoProfile) -> return (t, DropProfile k)
      _ -> Bad $ "conflicting profile information in " ++ s
  _ -> return (readGFTree s, maybe NoProfile DropProfile mdrop)


-- no separate drop table assumed here; could be done by preproc in general
tryParseConstantTableEntry :: PGF -> Language -> String -> Err (QIdent, ConstantTableEntry)
tryParseConstantTableEntry pgf lang s = do
  let qid:gids = splitEntry s
  let (latexs, gfids) = partition ((=='$') . head) gids
  let macros = [macroName qid i | (_, i) <-  zip latexs [0..]]
  annots <- mapM (tryParseFunProfile pgf lang Nothing) (gfids ++ macros) -- Nothing as no separate drop table
  let entry = mkConstantTableEntry pgf annots
  return (QIdent qid, entry)

parseExample :: PGF -> Language -> String -> [FunProfile]
parseExample pgf lang =
    map extract . parse pgf lang (maybe undefined id (readType "Example")) . lexex
  where
   extract t = case unApp t of
     Just (_, ex:args) -> case getProfile args of
       Just p -> (ex, p)
       _ -> (ex, NoProfile)
     _ -> error $ "cannot get example from: " ++ showGFTree t

   lexex s = case s of
     c:' ':cs -> c : ' ' :lextex cs  -- don't uncap first letter
     c:cs | isAlpha c -> c : lextex cs -- don't uncap first letter
     _ -> lextex s

   getProfile args = do
     let iargs = [i | Just (f, [x]) <- map unApp args, showCId f == "IntArgument", Just i <- [unInt x]]
     if length iargs == length args
       then return (PermProfile iargs)
       else Nothing


readGFTree :: String -> GFTree
readGFTree s = case s of
  '\\':_ -> mkApp (mkCId s) []
  _ -> maybe (error ("cannot parse as GFTree: " ++ s)) id (readExpr s)

------

data SymbolTable = SymbolTable {
  constantTable :: ConstantTable,
  backConstantTable :: BackConstantTable,
  conversionTable :: ConversionTable,
  dropTable :: DropTable,
  macroTable :: MacroTable,
  semanticsTable :: SemDefs,
  nlgTable :: NLGDefs,
  builtinSet :: S.Set QIdent
  }

-- conversion from Dk to GF, with synonyms and category information
type ConstantTable = M.Map QIdent ConstantTableEntry

-- conversion from Dk to other formalisms
type ConversionTable = M.Map Formalism (M.Map QIdent QIdent)

-- conversions in Dk that drop a number of initial arguments
--- redundant with profiles, except for proofs
type DropTable = M.Map QIdent Int

-- definitions of macros to be converted to \newcommand in LaTeX
type MacroTable = M.Map String (Int, String)

data ConstantTableEntry = ConstantTableEntry {
  primary   :: (FunProfile, Type),
  symbolics :: [(FunProfile, Type)],
  synonyms  :: [(FunProfile, Type)]
  }

allGFFuns :: ConstantTable -> QIdent -> [(FunProfile, Type)]
allGFFuns table qident = maybe [] merge $ M.lookup qident table where
  merge entry = primary entry : symbolics entry ++ synonyms entry

-- shown in the form that can be parsed as a constant table
showConstantTable :: ConstantTable -> String
showConstantTable = unlines . map prEntry . M.toList where
  prEntry :: (QIdent, ConstantTableEntry) -> String
  prEntry (QIdent q, entry) =
    unwords $ [q, ":"] ++
    intersperse "|" (map (showFunProfile . fst) (primary entry : synonyms entry ++ symbolics entry))

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
  prTyping (funprof, typ) = showFunProfile funprof ++ " : " ++ showType [] typ ++ " ;"


-- maps GF idents to original "Dk" idents; the Profile is the original one, to be applied backwards
type BackConstantTable = M.Map Fun [(QIdent, Profile)] 

type BuiltinSet = S.Set QIdent -- built-in constants not expected in ConstantTable

buildBackConstantTable :: ConstantTable -> BackConstantTable
buildBackConstantTable table = M.fromListWith (++) [
  (fun, [(qid, profile)]) | 
    (qid, entry) <- M.toList table,
    (fun, profile) <- map fst (primary entry : symbolics entry ++ synonyms entry)
  ]

---- TODO: make this accessible from RunInformath
printBackTable ::  BackConstantTable -> String
printBackTable = unlines . map prEntry . M.toList where
  prEntry :: (Fun, [(QIdent, Profile)]) -> String
  prEntry (f, qids) = showGFTree f ++ ": " ++ unwords [g ++ showProfile prof | (QIdent g, prof) <- qids]

buildSymbolTable :: PGF -> Language -> [String] -> SymbolTable
buildSymbolTable pgf lang ls = SymbolTable {
  constantTable = constantTable,
  backConstantTable = backConstantTable,
  conversionTable = conversionTable,
  dropTable = dropTable,
  macroTable = macroTable,
  semanticsTable = semanticsTable,
  nlgTable = nlgTable,
  builtinSet = builtinSet
  }
 where
    entrylines = filter (not . null) (map words ls)
    constantlines = filter isConstantEntry entrylines
    conversionlines = filter isConversion entrylines
    droplines = filter isDrop entrylines
    macrolines = filter isMacro entrylines
    builtinlines = filter isBuiltin entrylines
    semanticslines = filter isSemantics entrylines
    nlglines = filter isNLG entrylines
    constantTable = M.fromList [
        (QIdent qid, mkConstantTableEntry pgf (map (parseFunProfile pgf lang (ifDrop (QIdent qid))) (gfids ++ macros))) |
                     qid:gids@(_:_) <- map (splitEntry . unwords) constantlines,
           let (latexs, gfids) = partition ((=='$') . head) gids,
           let macros = [macroName qid i | (_, i) <-  zip latexs [0..]]
           ]
    conversionTable = M.fromList [
        (form, M.fromList [(QIdent d, QIdent f) | _:d:f:_ <- fids]) |
       fids@((form:_):_) <- groupBy (\x y -> head x == head y) (sort (map tail conversionlines))]
    backConstantTable = buildBackConstantTable constantTable
    dropTable = M.fromList [(QIdent c, read n) | _:c:n:_ <- droplines]
    ifDrop qid = M.lookup qid dropTable --- copy dropTable entry to profile
    macroTable = M.fromList (
        [(c, (read n, d)) | _:rest <- macrolines, let [c, n, d] = splitNewcommand (unwords rest)] ++
        [mkMacro qid gid i |
          qid:gids <- map (splitEntry . unwords) constantlines,
          (gid, i) <- zip [gid | gid@('$':_) <- gids] [0..]])
    builtinSet = S.fromList [QIdent c | _:cs <- builtinlines, c <- cs]
    semanticsTable = M.fromList [readSemDef (unwords ws) | _:ws <- semanticslines]
    nlgTable = M.fromListWith (++) [(c, [f]) | _:ws <- nlglines, let (c, f) = readSemDef (unwords ws)]
    
    isConstantEntry line = head (head line) /= '#'
    isConversion line = head line == "#CONV"
    isDrop line = head line == "#DROP"
    isMacro line = head line == "#MACRO"
    isBuiltin line = head line == "#BUILTIN"
    isSemantics line = head line == "#SEMANTICS"
    isNLG line = head line == "#NLG"

splitEntry s = case splitOutside '$'  '|' s of
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


mkMacro :: String -> String -> Int -> (String, (Int, String))
mkMacro qid s i = (macroName qid i, (maximum (0 : args s), init (tail s)))
 where
   args s = case s of
     '#':c:cs | isDigit c -> read [c] : args cs --- only one-digit arguments
     c:cs -> args cs
     _ -> []

macroName :: String -> Int -> String
macroName c i = "\\" ++ filter isAlpha c ++ "MACRo" ++ replicate i 'I'

mkConstantTableEntry :: PGF -> [FunProfile] -> ConstantTableEntry
mkConstantTableEntry _ [] = error "constant table entry cannot be empty"
mkConstantTableEntry pgf (funp@(fun, prof) : funps) = ConstantTableEntry {
  primary = (funp, funtype pgf fun),
  symbolics = [(f, typ) | (f, typ) <- symbs],
  synonyms = [(f, typ) | (f, typ) <- syns]
  }
 where
 
   funtype fun = inferFunType pgf

   (symbs, syns) = partition (isSymbolic . snd) [(fp, funtype pgf f) | fp@(f,_) <- funps]
   isSymbolic typ = case unType typ of
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
       Just ("Exp", _) -> ["Exp", "Kind", "ProofExp", "Proof"]
       Just ("Kind", _) -> ["Exp", "Kind", "Prop"]
       Just ("Proof", _) -> ["Exp", "Kind", "ProofExp", "Proof"]
       Just ("Prop", _) -> ["Exp", "Kind", "Prop"]
       Just (val, _) -> [val]
       _ -> ["UNKNOWN-GF"] ---
       
  dkArity dkhypos typ = (valCat typ, foldl (+) 0 (map hypoArity dkhypos))
  valCat typ = case fst (splitApp typ) of
    EIdent f | f == identProp -> "Prop"
    EIdent f | elem f [identSet, identType] -> "Kind"
    EIdent f | f == identElem -> "Exp"
    EIdent f | f == identProof -> "Proof"
    _ -> "Prop" --- UNKNOWN-DK" --- default, but not accurate

  hypoArity hypo = maybe 1 ((+1) . length . fst . splitType) (hypo2type hypo) -- for HOAS
    
symbolTableErrors :: Module -> PGF -> SymbolTable -> [String]
symbolTableErrors dk pgf st =
  let dt = dropTable st
      mt = macroTable st
      ct = constantTable st
      bset = builtinSet st 
      funs = deduktiFunctions dk
      missing = [fun | (fun, _) <- funs, M.notMember fun ct, S.notMember fun bset]
      mismatches = [((dkfun, gffun), (e, f)) |
                      (dkfun, (hypos, valtype)) <- funs,
            let drops = maybe 0 id (M.lookup dkfun dt),
            let dktyp = (drop drops hypos, valtype),
            ((gffun, profile), gftyp) <- allGFFuns ct dkfun,
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

-- annotate Dk idents with cats and funs ; used only internally
annotateDkIdents :: Maybe Int -> Maybe Int -> ConstantTable -> DropTable -> DkTree a -> [DkTree a]
annotateDkIdents msyns msymbs table drops =
                               checkSymbolics .
                               annot []
                ---- . ignoreFirstArguments drops -- done with profile
 where
  -- don't annotate bound variables: they override constants
  annot :: forall a. [QIdent] -> DkTree a -> [DkTree a]
  annot bounds t = case t of
    EApp fun arg -> case splitApp t of
      (EIdent c, args) | notElem c bounds -> [appProfile p (foldl EApp (EIdent f) aargs) | (f, p) <- annotId c, aargs <- sequence (map (annot bounds) args)]
      _ -> [EApp afun aarg | afun <- annot bounds fun, aarg <- annot bounds arg]
    QIdent _ | notElem t bounds -> map fst (annotId t)
    EAbs b exp -> [EAbs b2 exp2 | b2 <- annot bounds b, exp2 <- annot (bind2ident b : bounds) exp]
    EFun h exp -> [EFun h2 exp2 | h2 <- annot bounds h, exp2 <- annot (hypo2topvars h ++ bounds) exp]    
    BVar _ -> [t]
    BTyped v ty -> [BTyped v ty2 | ty2 <- annot bounds ty]
    HVarExp v ty -> [HVarExp v ty2 | ty2 <- annot bounds ty]
    HParVarExp v ty -> [HParVarExp v ty2 | ty2 <- annot bounds ty]
    HLetExp v ty -> [HLetExp v ty2 | ty2 <- annot bounds ty]
    HLetTyped v ty exp -> [HLetTyped v ty2 exp2 | ty2 <- annot bounds ty, exp2 <- annot bounds exp]
    _ -> composOpM (annot bounds) t

  tkSyns = maybe id take msyns
  tkSymbs = maybe id take msymbs

  annotId c = case M.lookup c table of
    Just entry -> [annotIdent c (maybe 0 id (M.lookup c drops)) fpt |
                       fpt <- tkSyns  (primary entry : synonyms entry) ++
                                tkSymbs (symbolics entry)]
    _ -> [(c, NoProfile)]

  checkSymbolics :: [DkTree a] -> [DkTree a]
  checkSymbolics ts = [t | t <- ts, not (badSymb t)] ---- null (badSymbolics t)]

  -- bad symbolics are subtrees with symbolic root and at least one verbal subtree
  badSymb :: DkTree a -> Bool
  badSymb t = case t of
    EApp _ _ -> case splitApp t of
      (EIdent (QIdent c), ts) -> case lookupConstant c of
        Just (cat, _) | S.member cat symbolicCats ->
          not (null
           [u | u <- ts, not (null [k | QIdent k <- identsInTree u,
            Just (kat, _) <- [lookupConstant k], S.member kat verbalCats])])
        _  -> any badSymb ts
      (f, ts) -> any badSymb (f : ts)
    EAbs b exp -> badSymb b || badSymb exp
    EFun h exp -> badSymb h || badSymb exp
    BTyped v ty -> badSymb ty
    HVarExp v ty -> badSymb ty
    HParVarExp v ty -> badSymb ty
    HLetExp v ty -> badSymb ty
    HLetTyped v ty exp -> badSymb ty || badSymb exp
    JDef _ mt me -> badSymb mt || badSymb me
    JStatic _ ty -> badSymb ty
    JThm _ mt me -> badSymb mt || badSymb me
    JInj _ mt me -> badSymb mt || badSymb me
    JRules rules -> any badSymb rules
    RRule pattbinds patt exp -> any badSymb pattbinds || badSymb patt || badSymb exp
    MTExp exp -> badSymb exp
    MEExp exp -> badSymb exp
    ---- TODO patt
    _ -> False


  badSymbolics :: DkTree a -> [DkTree a]
  badSymbolics t = case t of
    EApp _ _ -> case splitApp t of
      (EIdent (QIdent c), ts) -> case lookupConstant c of
        Just (cat, _) | S.member cat symbolicCats ->
          [u | u <- ts, not (null [k | QIdent k <- identsInTree u,
            Just (kat, _) <- [lookupConstant k], S.member kat verbalCats])]
        _  -> concatMap badSymbolics ts
      (f, ts) -> concatMap badSymbolics (f : ts)
    _ -> composOpM badSymbolics t

annotIdent :: QIdent -> Int -> (FunProfile, Type) -> (QIdent, Profile)
annotIdent (QIdent s) d ((f, p), t) =
  (QIdent $ concat $ intersperse "#" $ [s, dk (valCat t), dkp f] ++ map dk (argCats t) ++ [show d], p)
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
