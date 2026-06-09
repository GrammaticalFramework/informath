module Main where
---- module Proofs where

import Dedukti.AbsDedukti hiding (Tree)
import Dedukti.PrintDedukti hiding (prt)
import Dedukti.ParDedukti
import Dedukti.LexDedukti
import qualified Dedukti.ErrM as DE

import DeduktiOperations

import Data.List (intersperse, nub, nubBy, sortOn)
import qualified Data.Map as M
import System.Environment (getArgs)

-- experiment with Jan von Plato 2017. "From Gentzen to Jaskowski and Back:
-- Algorithmic Translation of Derivations Between the Two Main Systems of Natural Deduction."
-- code partly borrowed from https://github.com/aarneranta/PESCA
--
-- main datatypes:
--
--   Term  (Martin-Löf style, from Dedukti)
--   Tree  (Gentzen style)
--   Lines (Jaskowski/Prawitz style)
--
-- main conversions:
--
--   Term -> Tree
--   Term -> Lines
--   Lines -> Tree
--   Lines -> Term  -- TODO
--
-- first a demo; do runghc Deduction.hs >pr.tex ; pdflatex pr.tex
--

main = do
  base:exx:drop:_ <- getArgs -- filenames
  mo <- readDeduktiModule [base]
  let dkmap = identTypes mo
  MJmts ps <- readDeduktiModule [exx]
  dropmap <- readDropMap drop
  putStrLn $ prLatexFile $ unlines $ intersperse "\n\n" [
    expDemo dkmap dropmap t e | JThm _ (MTExp t) (MEExp e) <- ps]

readDeduktiModule :: [FilePath] -> IO Module
readDeduktiModule files = mapM readFile files >>= return . parseDeduktiModule . unlines

-- | To parse a Dedukti file into its AST.
parseDeduktiModule :: String -> Module
parseDeduktiModule s = case pModule (myLexer s) of
  DE.Bad e -> error ("parse error: " ++ e)
  DE.Ok mo -> mo

readDropMap :: FilePath -> IO (M.Map QIdent Int)
readDropMap file = readFile file >>= return . M.fromList . map mkEntry . map words . lines
  where
    mkEntry (x:i:_) = (QIdent x, read i)

expDemo dkmap dropmap typ exp = unlines $ intersperse "\n\n" [
    "\\subsection*{From term to lines and back}"
    , "Original proved theorem"
    , verbatim (printTree typ)
    , "Original proof exp"
    , verbatim (printTree exp)
    , "Annotated proof term"
    , verbatim (printTree term)
    , "Generated deduction tree"
    , prst dkmap (term2tree term)
    , "Generated linear proof" ++ testEq (term2tree term) (lines2steptree linesterm)
    , prls linesterm
    , "\\clearpage"
    ]
  where
    term = ignoreFirstArguments dropmap (typeAnnotate dkmap [] typ exp)
    linesterm = term2lines term

testEq x y = if x==y then " OK" else " TODO"


-------------------------------
-- data types and constructors
-------------------------------

-- proof steps (lines on trees)

data Step = Step {
  hypovar :: QIdent,      -- relevant only for hypotheses
  formula :: Exp,         -- formula assumed or concluded
  rule    :: QIdent,      -- the rule that is used
  discharged :: [QIdent]  -- hypolabels of discharged formulas
  }
  deriving (Show, Eq)
  
mkStep li fo ru di = Step li fo ru di

-- proof lines in Jaskowski-style notation

data Line = Line {
  line      :: Int,      -- line number
  context   :: [QIdent], -- labels of open hypotheses
  premisses :: [Int],    -- line numbers of premisses
  step :: Step           -- the main content of the line
  }
  deriving (Show, Eq)

mkLine li co fo ru prs di = Line li co prs (mkStep noIdent fo ru di)
mkHypoLine li fo ru hy = Line li [hy] [] (mkStep hy fo ru [])

noIdent = QIdent "" ---- 

-- rose trees (in general)

data Tree a = Tree {
  root :: a,
  subtrees :: [Tree a]
  }
  deriving (Show, Eq)

-- conversions

nodes :: Tree a -> [a]
nodes (Tree a ts) = a : concatMap nodes ts

maptree :: (a -> b) -> Tree a -> Tree b
maptree f (Tree a ts) = Tree (f a) (map (maptree f) ts)

lines2linetree :: [Line] -> Tree Line
lines2linetree ls = ltr (last ls) where
  ltr concl = Tree concl [ltr (ls !! (prem-1)) | prem <- premisses concl]

lines2steptree :: [Line] -> Tree Step
lines2steptree = maptree step . lines2linetree

-----------------------------------------
-- type annotation
-----------------------------------------

typeAnnotate :: M.Map QIdent Exp -> [(QIdent, Exp)] -> Exp -> Exp -> Exp
typeAnnotate mo cont typ exp = case exp of
  EApp _ _ -> case splitApp exp of
    (EIdent fun, args) ->
      let
        (hs, body) = splitType (look fun)
        vars = [case hypo2topvars h of {v:_ -> v; [] -> QIdent "_"} | h <- hs] -- one var per hypo, aligned 1:1 with args
	apptyp = subst (zip vars args) (map fst cont) body
        newargs = [typeAnnotate mo cont (subst (zip vars args) (map fst cont) ty) arg | (h, arg) <- zip hs args, Just ty <- [hypo2type h]]
      in ETyped (foldl EApp (EIdent fun) newargs) apptyp
  EIdent fun -> ETyped exp (look fun)
  EAbs bind body -> 
    let
      vartyp = case bind of
        BTyped v ty -> ty
        BVar v -> case typ of
          EFun h _ -> case hypo2type h of
	    Just ty -> ty
	    _ -> error ("no type of bound var in " ++ printTree exp)
      bindvar = bind2var bind
      bodytyp = case typ of
        EFun h val -> subst (zip (hypo2vars h) [EIdent bindvar]) (map fst cont) val
	_ -> error ("incorrect type of abstraction " ++ printTree exp)
    in EAbs (BTyped bindvar vartyp)
            (typeAnnotate mo ((bindvar, vartyp): cont) bodytyp body)

 where
   look fun = case lookup fun cont of
     Just ty -> ty
     _ -> case M.lookup fun mo of
        Just ty -> ty
        _ -> typ


subst :: [(QIdent, Exp)] -> [QIdent] -> Exp -> Exp
subst gamma bs e = case e of
  EIdent x {- | notElem x bs -} -> case lookup x gamma of
    Just v -> v
    _ -> e
  EApp f a -> case subst gamma bs f of
    EAbs bind body -> subst [(bind2ident bind, subst gamma bs a)] bs body -- beta-reduce redex from substituted higher-order arg
    f' -> EApp f' (subst gamma bs a)
  EAbs (BTyped x ty) a -> EAbs (BTyped x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EAbs b a -> EAbs b (subst gamma (bind2ident b : bs) a)
  EFun (HVarExp x ty) a -> EFun (HVarExp x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EFun (HParVarExp x ty) a -> EFun (HParVarExp x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EFun h a -> EFun h (subst gamma (hypo2vars h ++ bs) a)
  _ -> e


-----------------------
-- proof trees
----------------------

term2tree :: Exp -> Tree Step
term2tree exp = case exp of
  ETyped e typ -> case splitApp e of
    (EIdent fun, args) ->
      Tree (mkStep noIdent typ fun (concatMap absIdents args)) (concatMap argTrees args)
  EAbs _ t -> term2tree t
  _ -> error ("term2tree " ++ printTree exp)
 where
  -- an argument contributes a leaf for each Elem-typed variable it binds (an eigenvariable),
  -- followed by the subtree for the argument itself
  argTrees arg = [Tree (mkStep noIdent t v []) [] | (v, t) <- elemBinders arg] ++ [term2tree arg]

-- the Elem-typed variables bound at the top of an abstraction (eigenvariables, e.g. x : Elem A)
elemBinders :: Exp -> [(QIdent, Exp)]
elemBinders arg = [(v, t) | BTyped v t <- fst (splitAbs arg), isElemType t]

-- an object-language type Elem A (as opposed to a Proof of a proposition)
isElemType :: Exp -> Bool
isElemType e = case splitApp e of
  (EIdent (QIdent "Elem"), _) -> True
  _ -> False


-----------------------
-- linear proofs
----------------------

term2lines :: Exp -> [Line]
term2lines =
    compress 0 [] [] .
    ps 1 []             
      where
 -- generate lines starting with this line number and context
 ps :: Int -> [QIdent] -> Exp -> [Line]
 ps ln cont proof = case proof of -- next line number, its context 

   ETyped e typ -> case splitApp e of
    (EIdent fun, args) ->
      let
         (argss, prems, lnConcl) = psArgs ln cont args
      in concat argss ++
         [mkLine lnConcl cont typ fun (nub prems) (concatMap absIdents args)]
   EAbs _ _ -> case splitAbs proof of
     (binds, body) -> ps ln (cont ++ map bind2ident binds) body

   _ -> error ("term2tree " ++ printTree proof)

 -- process arguments left to right, threading line numbers; each argument yields its lines
 -- (eigenvariable lines for its Elem-typed binders, then its own derivation) together with the
 -- line numbers to cite as premisses (those eigenvariables and the argument's conclusion)
 psArgs :: Int -> [QIdent] -> [Exp] -> ([[Line]], [Int], Int)
 psArgs n cont [] = ([], [], n)
 psArgs n cont (a:as) =
   let (ls, prem) = psArg n cont a
       (lss, prems, nf) = psArgs (nextline n ls) cont as
   in (ls : lss, prem ++ prems, nf)

 psArg :: Int -> [QIdent] -> Exp -> ([Line], [Int])
 psArg n cont arg = case arg of
   EAbs _ _ ->
     let (binds, body) = splitAbs arg
         cont' = cont ++ map bind2ident binds
         elemvs = [(v, t) | BTyped v t <- binds, isElemType t]
         eigenLines = [mkLine (n + i) cont' t v [] [] | (i, (v, t)) <- zip [0 ..] elemvs]
         bodyLines = ps (n + length eigenLines) cont' body
     in (eigenLines ++ bodyLines,
         map line eigenLines ++ [lastline bodyLines | not (null bodyLines)])
   _ ->
     let ls = ps n cont arg
     in (ls, [lastline ls | not (null ls)])

 lastline = line . last
 nextline ln p = if null p then ln else lastline p + 1

 -- compress lines by dropping repeated hypotheses and renumbering lines accordingly
 compress :: Int -> [(Int, Int)] -> [((QIdent, Exp), Int)] -> [Line] -> [Line]
 compress gaps relines rehypos ls = case ls of
   ln : rest | isHypoLine ln ->
     let key = (rule (step ln), formula (step ln)) -- a hypothesis is identified by its label and formula
     in case lookup key rehypos of
          Just k ->  -- old hypothesis: drop this line, re-point its number to the first occurrence
            compress (gaps + 1) ((line ln, k) : relines) rehypos rest
          _ ->       -- new hypothesis: keep it, renumbering to close earlier gaps
            let nln = line ln - gaps
            in ln{line = nln} :
               compress gaps ((line ln, nln) : relines) ((key, nln) : rehypos) rest
   ln : rest ->
     let nln = line ln - gaps
         ds  = discharged (step ln) -- hypotheses closed here may be reused (as new) below
         rehypos' = [r | r@((h, _), _) <- rehypos, notElem h ds]
     in renumberLine nln relines ln :
        compress gaps ((line ln, nln) : relines) rehypos' rest
   _ -> ls

 -- a hypothesis line just cites an open assumption: no premisses, rule is a context variable
 isHypoLine ln = null (premisses ln) && elem (rule (step ln)) (context ln)

 -- change the line number and re-point all references to premiss line numbers
 renumberLine num relines ln = ln {
    premisses = [maybe p id (lookup p relines) | p <- premisses ln],
    line = num
    }

----------------------------
-- printing
-----------------------------

prls :: [Line] -> String
prls lns = unlines $
  "\\[" :
  "\\begin{array}{llllll}" :
  [unwords (intersperse "&" (prl ln)) ++ "\\\\" | ln <- lns] ++
  ["\\end{array}", "\\]"] 

prl :: Line -> [String]
prl ln
  | null (premisses ln) && isElemType (formula (step ln)) =  -- object variable assumption: "x : Elem A" on one line
      [ concat (intersperse "," (map printTree (context ln))),
        show (line ln) ++ ".",
        "\\verb#" ++ printTree (rule (step ln)) ++ " : " ++ printTree (formula (step ln)) ++ "#",
        "", "", "" ]
  | otherwise = [
---  concat (replicate (length (context ln)) "\\mid"),
  concat (intersperse "," (map printTree (context ln))),
  show (line ln) ++ ".",
  "\\verb#" ++ printTree (formula (step ln)) ++ "#",
  printTree (rule (step ln)),
  concat (intersperse ", " (map show (premisses ln))),
  let dis = discharged (step ln)
    in if null dis then "" else "[" ++ concat (intersperse ", " (map printTree dis)) ++ "]"
  ]
  
prs :: Step -> [String]
prs st = [
  printTree (hypovar st) ++ ".",
  "\\texttt{" ++ printTree (formula st) ++ "}", -- typewriter, matching the \verb in linear proofs (\verb is illegal inside \infer args)
  printTree (rule st),
  concat (map ((","++) . printTree) (discharged st))
  ]

prlt :: Tree Line -> String
prlt = mathdisplay . pr  where
  pr (Tree a ts) = case ts of
    [] -> unwords (prl a)
    _ -> "\\infer{" ++ unwords (prl a) ++ "}{" ++ unwords (intersperse "&" (map pr ts)) ++ "}"

prst :: M.Map QIdent Exp -> Tree Step -> String
prst rules = mathdisplay . pr where
  pr (Tree a ts) = case ts of
    []
      | isElemType (formula a) ->   -- an object variable, e.g. x : Elem A: variable beside the type, on one line
          concat ["\\texttt{", printTree (rule a), " : ", printTree (formula a), "}"]
      | M.member (rule a) rules ->  -- a nullary rule/axiom (e.g. even0): show its name, not a discharge
          concat ["\\infer[{\\scriptstyle ", prs a !! 2, "}]{", prs a !! 1, "}{}"]
      | otherwise ->                -- a discharged hypothesis: its label sits above the formula
          concat ["\\discharge{", prs a !! 2, "}{", prs a !! 1, "}"]
    _ -> concat ["\\infer[{\\scriptstyle ", prs a !! 2, prs a !! 3, "}]{",
                 prs a !! 1, "}{", unwords (intersperse "&" (map pr ts)), "}"]

---- TODO: pretty-printing on multiple lines
prt :: Exp -> String
prt exp = printTree exp

mathdisplay s = "\\[" ++ s ++ "\\]"

verbatim s = "\\begin{verbatim}\n" ++ unlines (splitLines (words s)) ++ "\n\\end{verbatim}"

splitLines ws = case splitAt 10 ws of
  (line, rest@(_:_)) -> unwords line : splitLines rest
  _ -> [unwords ws]

prLatexFile string = unlines [
  "\\documentstyle[proof]{article}",
  "\\setlength{\\parskip}{2mm}",
  "\\setlength{\\parindent}{0mm}",
  "\\newcommand{\\discharge}[2]{\\begin{array}[b]{c} #1 \\\\ #2 \\end{array}}",
  "\\begin{document}",
  string,
  "\\end{document}"
  ]

{-
---- from DeduktiOperations

-- collect types of idents
identTypes :: Module -> M.Map QIdent Exp
identTypes (MJmts jmts) = M.fromList (concatMap idtyp jmts) where
  idtyp :: Jmt -> [(QIdent, Exp)]
  idtyp jmt = case jmt of
    JDef qident (MTExp typ) _ -> [(qident, typ)]
    JThm qident (MTExp typ) _ -> [(qident, typ)]
    JInj qident (MTExp typ) _ -> [(qident, typ)]
    JStatic qident typ -> [(qident, typ)]
    _ -> []


bind2ident :: Bind -> QIdent
bind2ident bind = case bind of
  BVar var -> var
  BTyped var _ -> var
  BLet var _ _ -> var

splitApp :: Exp -> (Exp, [Exp])
splitApp exp = case exp of
  EApp fun arg -> case splitApp fun of
    (_, [])   -> (fun, [arg])
    (f, args) -> (f, args ++ [arg])
  _ -> (exp, [])

splitAbs :: Exp -> ([Bind], Exp)
splitAbs exp = case exp of
  EAbs bind body -> case splitAbs body of
    ([], _) -> ([bind], body)
    (binds, rest) -> (bind:binds, rest)
  _ -> ([], exp)

hypo2vars :: Hypo -> [QIdent]
hypo2vars hypo = maybe [] typevars (hypo2type hypo) ++ hypo2topvars hypo where
  typevars :: Exp -> [QIdent]
  typevars ty = case ty of
    EFun h body -> hypo2topvars h ++ typevars body
    _ -> []

hypo2type :: Hypo -> Maybe Exp
hypo2type hypo = case hypo of
  HVarExp v e -> Just e
  HParVarExp v e -> Just e
  HLetExp v _ -> Nothing
  HLetTyped v e _ -> Just e
  HExp e -> Just e

hypo2topvars :: Hypo -> [QIdent]
hypo2topvars hypo = case hypo of
  HVarExp v _ -> [v]
  HParVarExp v _ -> [v]
  HLetExp v _ -> [v]
  HLetTyped v _ _ -> [v]
  HExp v -> []

hypo2var :: Hypo -> QIdent
hypo2var hypo = case hypo of
  HVarExp v _ -> v
  HParVarExp v _ -> v
  HLetExp v _ -> v
  HLetTyped v _ _ -> v
  HExp v -> QIdent "_"

splitType :: Exp -> ([Hypo], Exp)
splitType exp = case exp of
  EFun hypo body -> case splitType body of
    ([], _) -> ([hypo], body)
    (hypos, rest) -> (hypo:hypos, rest)
  _ -> ([], exp)

-}