module Main where
---- module Proofs where

import Dedukti.AbsDedukti hiding (Tree)
import Dedukti.PrintDedukti hiding (prt)
import Dedukti.ParDedukti
import Dedukti.LexDedukti
import qualified Dedukti.ErrM as DE

---import DeduktiOperations

import Data.List (intersperse, nub, nubBy, sortOn)

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


import qualified Data.Map as M

import System.Environment (getArgs)

main = do
  base:exx:_ <- getArgs -- filenames
  mo <- readDeduktiModule [base]
  let dkmap = dkFunctionMap mo
  mapM_ putStrLn (printFunctionMap dkmap)
  MJmts ps <- readDeduktiModule [exx]
  putStrLn $ prLatexFile $ unlines $ intersperse "\n\n" [
    expDemo dkmap e | JThm _ _ (MEExp e) <- ps]


readDeduktiModule :: [FilePath] -> IO Module
readDeduktiModule files = mapM readFile files >>= return . parseDeduktiModule . unlines

-- | To parse a Dedukti file into its AST.
parseDeduktiModule :: String -> Module
parseDeduktiModule s = case pModule (myLexer s) of
  DE.Bad e -> error ("parse error: " ++ e)
  DE.Ok mo -> mo


mainz = do
  putStrLn $ prLatexFile $ unlines $ intersperse "\n\n" [
    ]
{-
linesDemo ex = unlines $ intersperse "\n\n" [
    "\\subsection*{From lines to term and back}"
    , "Original linear proof"
    , prls ex
    , "Generated deduction tree"
    , prst (lines2steptree ex)
    , "Proof term generated from the tree" ++ testEq 1 0
    , mathdisplay (prt termex)
    , "Deduction tree generated from the proof term" ++ testEq (lines2steptree ex) (term2tree termex)
    , prst (term2tree termex)
    , "Linear proof generated from the proof term" ++ testEq ex (term2lines termex)
    , prls (term2lines termex)
    , "\\clearpage"
    ]
   where termex = lines2term ex
-}

expDemo dkmap exp = unlines $ intersperse "\n\n" [
    "\\subsection*{From term to lines and back}"
    , "Original proof exp"
    , verbatim (printTree exp)
    , "Converted proof term"
    , mathdisplay (prt term)
    , "Generated deduction tree"
    , prst (term2tree term)
    , "Generated linear proof" ++ testEq (term2tree term) (lines2steptree linesterm)
    , prls linesterm
----    , "Deduction tree generated from the linear proof" ++ testEq (term2tree term) (lines2steptree linesterm)
----    , prst (lines2steptree linesterm)
----    , "Proof term generated from the linear proof"
----    , mathdisplay (prt (lines2term linesterm))
    , "\\clearpage"
    ]
  where
----    term = exp2term dkmap (snd (splitAbs exp))
    term = exp2term dkmap exp
    linesterm = term2lines term

testEq x y = if x==y then " OK" else " TODO"


-------------------------------
-- data types and constructors
-------------------------------

-- logical formulas

type Formula = (Exp, Exp) --- Exp
type Var = QIdent
type Label = QIdent

-- proof steps (lines on trees)

data Step = Step {
  hyponumber :: Var,  -- relevant only for hypotheses
  formula :: Formula,   -- formula assumed or concluded
  rule :: Label,     -- the rule that is used
  discharged :: [Var] -- hypolabels of discharged formulas
  }
  deriving (Show, Eq)
  
mkStep li fo ru di = Step li fo ru di

-- proof lines in Jaskowski-style notation

data Line = Line {
  line :: Int,        -- line number
  context :: [Var],   -- labels of open hypotheses
  premisses :: [Int], -- line numbers of premisses
  step :: Step        -- the main content of this line
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
-- read Dk module to a proof function map
-----------------------------------------

-- map from arguments to value and type
type Function = [Exp] -> (Exp, Exp)

mkFunction :: QIdent -> Exp -> Function
mkFunction c exp = \es -> val es
 where
  (hypos, body) = splitType exp
  vars = map hypo2var hypos
  val es =
---  case body of
----    EIdent (QIdent "Prop") -> foldl EApp (EIdent c) (take (length hypos) es)  
    (foldl EApp (EIdent c) (take (length hypos) es), subst (zip vars es) [] body)

  subst :: [(QIdent, Exp)] -> [QIdent] -> Exp -> Exp
  subst gamma bs e = case e of
    EIdent x | notElem x bs -> case lookup x gamma of
      Just v -> v
      _ -> e
    EApp f a -> EApp (subst gamma bs f) (subst gamma bs a)
    EAbs b a -> EAbs b (subst gamma (bind2ident b : bs) a)
    EFun h a -> EFun h (subst gamma (hypo2vars h ++ bs) a)
    _ -> e


dkFunctionMap :: Module -> M.Map QIdent Function
dkFunctionMap mo = M.mapWithKey mkFunction (identTypes mo)

printFunctionMap :: M.Map QIdent Function -> [String]
printFunctionMap fm = [
  "% " ++ printTree c ++ " : " ++
    printTree v ++ " : " ++ printTree t
    | (c, f) <- M.toList fm,
      let (v, t) = f [EIdent (QIdent ("#" ++ show i)) | i <- [1..]]
  ]

exp2term ::  M.Map QIdent Function -> Exp -> Term
exp2term dmap exp = case exp of
  EApp _ _ -> case splitApp exp of
    (EIdent c, args) -> case M.lookup c dmap of
      Just f -> app c (map (exp2term dmap) args) f
      _ -> -- Dk exp ----
        error ("cannot apply " ++ printTree c)
    _ -> -- Dk exp ----
        error ("cannot build application from " ++ printTree exp)
  EAbs _ _ -> case splitAbs exp of
    (xs, body) -> Abs (map bind2ident xs) (exp2term dmap body)
  EIdent x -> Hyp x (EIdent x, exp) ---- TODO infer type of x
  _ -> Dk exp ----


-----------------------
-- filtering display
-----------------------

isProofFormula :: Formula -> Bool
isProofFormula f = case f of
  (_, EApp (EIdent (QIdent "Proof")) _) -> True
  _ -> False

prf :: Formula -> String
prf (exp, typ) = -- printTree exp ++ " : " ++
                 printTree typ



-----------------------
-- proof terms
----------------------

data Term =
    App Label [Formula] [Term] Function
  | Abs [Var] Term
  | Hyp Var Formula
  | Ass Var Formula
  | Dk Exp --- 
-- the last argument of App tells how to combine the argument formulas for display

---- idea: subformulas of conclusions could be derived from subderivations 
app :: Label -> [Term] -> Function -> Term
app label terms conn = App label (map conclusion terms) terms conn

conclusion :: Term -> Formula
conclusion term = case term of
  App _ _ ts c -> c (map (fst . conclusion) ts)
  Abs _ t -> conclusion t
  Hyp x f -> f
  Ass c f -> f

-- conversions

term2tree :: Term -> Tree Step
term2tree term = case term of
  App rule ps ts c -> Tree (mkStep noIdent (c (map fst ps)) rule (concatMap bindings ts))
                           (filter (isProofFormula . formula . root) (map term2tree ts))
  Abs xs t -> term2tree t
  Hyp x a -> Tree (mkStep x a (QIdent "hypo") []) []
  Ass x a -> Tree (mkStep x a (QIdent "ass") []) []

bindings :: Term -> [Var]
bindings t = case t of
    Abs xs _ -> xs
    _ -> []


term2lines :: Term -> [Line]
term2lines =
    compress 0 [] [] .
    ps 1 []             
      where
 -- generate lines starting with this line number and context
 ps :: Int -> [Var] -> Term -> [Line]
 ps ln cont proof = case proof of -- next line number, its context 

   Ass var formula ->
     [] ---- mkHypoLine ln formula (QIdent "ass") var]

   Hyp var formula ->
     [] ---- mkHypoLine ln formula (QIdent "hypo") var]
     
   App label fs pts conn ->              
     let
         pss = psfold cont (pts, ln)
	 ln3 = nextline ln (concat pss)
	 concl = conn (map fst fs)
     in concat pss ++
         if isProofFormula concl
          then [mkLine ln3 cont concl label (nub (map lastline pss)) (concatMap bindings pts)]
	  else []
     
   Abs xs t -> ps ln (cont ++ xs) t

 psfold :: [Var] -> ([Term], Int) -> [[Line]]
 psfold cont (pts, n) = case pts of
   p : pp -> case
     ps n cont p of
       [] -> psfold cont (pp, n)
       ls -> ls : psfold cont (pp, nextline n ls)
   [] -> []

 lastline = line . last
 nextline ln p = if null p then ln else lastline p + 1

 -- compress lines by dropping repetitions of hypotheses and renumbering lines
 compress :: Int -> [(Int, Int)] -> [(Var, (Var, Int))] -> [Line] -> [Line]
 compress gaps relines rehypos ls = case ls of
   ln : rest | elem (rule (step ln)) [QIdent "hypo", QIdent "ass"] ->
     case (hyponumber (step ln)) of
       h -> case lookup h rehypos of
         Just (x, k) ->  -- old hypothesis: add gap and re-point line number to first occurrence
	   compress (gaps + 1) ((line ln, k) : relines) rehypos rest
         _ ->       -- new hypothesis: update its line number and hypo number to new line number 
	   let nln = line ln - gaps
	       vnln = QIdent (printTree h ++ show nln) ---- h TODO
	   in ln{
	         line = nln,
		 step = (step ln){hyponumber=vnln},
                 context = vnln : tail (context ln)
		 } :
	      compress gaps ((line ln, nln):relines) ((h, (vnln, nln)) : rehypos) rest
   ln : rest ->
           renumberLine (line ln - gaps) relines rehypos ln :
           compress gaps ((line ln, line ln -gaps):relines) rehypos rest
   _ -> ls 

 -- change the line number and all references to other line numbers
 renumberLine num relines rehypos ln = ln {
    premisses = [maybe p id (lookup p relines) | p <- premisses ln],
    context = [maybe p fst (lookup p rehypos) | p <- context ln],
    line = num,
    step = (step ln){discharged = [maybe p fst (lookup p rehypos) | p <- discharged (step ln)]}
    }
{-
lines2term :: [Line] -> Term
lines2term = tree2term . lines2steptree

---- TODO
tree2term :: Tree Step -> Term
tree2term (Tree s ts) = case (rule s, discharged s) of
  ("hypo", _) -> hypo (hyponumber s) (formula s) 
  ("ass", _) -> ass (hyponumber s) (formula s) ---- TODO: sequent lhs
  (_, xs@(_:_)) -> app (rule s) (map (Abs xs . tree2term) ts) (const (formula s))
  _ -> app (rule s) (map tree2term ts) (const (formula s))
-}

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
prl ln = [
---  concat (replicate (length (context ln)) "\\mid"),
  concat (intersperse "," (map printTree (context ln))),
  show (line ln) ++ ".",
  prf (formula (step ln)),
  printTree (rule (step ln)),
  concat (intersperse ", " (map show (premisses ln))),
  let dis = discharged (step ln)
    in if null dis then "" else "[" ++ concat (intersperse ", " (map printTree dis)) ++ "]"
  ]
  
prs :: Step -> [String]
prs st = [
  printTree (hyponumber st) ++ ".",
  prf (formula st),
  printTree (rule st),
  concat (map ((","++) . printTree) (discharged st))
  ]

prlt :: Tree Line -> String
prlt = mathdisplay . pr  where
  pr (Tree a ts) = case ts of
    [] -> unwords (prl a)
    _ -> "\\infer{" ++ unwords (prl a) ++ "}{" ++ unwords (intersperse "&" (map pr ts)) ++ "}"

prst :: Tree Step -> String
prst = mathdisplay . pr where
  pr (Tree a ts) = case ts of
    [] -> concat ["\\discharge{", prs a !! 0, "}{", prs a !! 1, "}"]
    _ -> concat ["\\infer[{\\scriptstyle ", prs a !! 2, prs a !! 3, "}]{",
                 prs a !! 1, "}{", unwords (intersperse "&" (map pr ts)), "}"]

---- TODO: pretty-printing on multiple lines
prt :: Term -> String
prt term = case term of
  App label ps ts c -> printTree label ++ parenth (unwords (intersperse "," (map prt ts)))
----  App label ps ts c -> printTree label ++ parenth (unwords (intersperse "," (map prf ps ++ map prt ts)))
  Abs xs t -> parenth (unwords ("\\lambda" : map prvar xs ++  [".", prt t]))
  Hyp x a -> prvar x
  Ass x a -> prcons x
  Dk e -> parenth ("Dk " ++ printTree e)
 where
  parenth s = "(" ++ s ++ ")"
  prvar x = printTree x
  prcons i = printTree i

mathdisplay s = "\\[" ++ s ++ "\\]"

verbatim s = "\\begin{verbatim}\n" ++ s ++ "\n\\end{verbatim}"

prLatexFile string = unlines [
  "\\documentstyle[proof]{article}",
  "\\setlength{\\parskip}{2mm}",
  "\\setlength{\\parindent}{0mm}",
  "\\newcommand{\\discharge}[2]{\\begin{array}[b]{c} #1 \\\\ #2 \\end{array}}",
  "\\begin{document}",
  string,
  "\\end{document}"
  ]

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

