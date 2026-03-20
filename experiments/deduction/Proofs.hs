module Proofs where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti

import Data.List (intersperse, nub, nubBy, sortOn)

-- experiment with Jan von Plato 2017. "From Gentzen to Jaskowski and Back:
-- Algorithmic Translation of Derivations Between the Two Main Systems of Natural Deduction."
-- code partly borrowed from https://github.com/aarneranta/PESCA
--
-- main datatypes:
--
--   Tree  (Martin-Löf style)
--   Term  (Gentzen style)
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
  putStrLn $ prLatexFile $ unlines $ intersperse "\n\n" [
    linesDemo exLines1,
    linesDemo exLines2,
    termDemo exTerm1,
    termDemo exTerm2, 
    termDemo exTerm3,
    termDemo exTerm4,
    termDemo exTerm5
    ]

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

termDemo term = unlines $ intersperse "\n\n" [
    "\\subsection*{From term to lines and back}"
    , "Original proof term"
    , mathdisplay (prt term)
    , "Generated deduction tree"
    , prst (term2tree term)
    , "Generated linear proof" ++ testEq (term2tree term) (lines2steptree linesterm)
    , prls linesterm
    , "Deduction tree generated from the linear proof" ++ testEq (term2tree term) (lines2steptree linesterm)
    , prst (lines2steptree linesterm)
    , "Proof term generated from the linear proof"
    , mathdisplay (prt (lines2term linesterm))
    , "\\clearpage"
    ]
  where linesterm = term2lines term

testEq x y = if x==y then " OK" else " TODO"

-------------------------------
-- data types and constructors
-------------------------------

-- logical formulas

type Formula = Exp
type Var = String
type Label = String

-- proof steps (lines on trees)

data Step = Step {
  hypovar :: Var,  -- relevant only for hypotheses
  formula :: Formula,   -- formula assumed or concluded
  rule :: Label,     -- the rule that is used
  discharged :: [Var] -- hypolabels of discharged formulas
  }
  deriving (Show, Eq)
  
mkStep li fo ru di = Step li fo ru di

-- proof lines in Jaskowski-style notation

data Line = Line {
  line :: Int,        -- line number
  context :: [Var], -- labels of open hypotheses
  premisses :: [Int], -- line numbers of premisses
  step :: Step        -- the main content of this line
  }
  deriving (Show, Eq)

mkLine li co fo ru prs di = Line li co prs (mkStep 0 fo ru di)
mkHypoLine li fo ru hy = Line li [hy] [] (mkStep hy fo ru [])

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



-----------------------
-- proof terms
----------------------

data Term =
    App Label [Formula] [Term] ([Formula] -> Formula)
  | Abs [Var] Term
  | Hyp Var Formula
  | Ass Var Formula
-- the last argument of App tells how to combine the argument formulas for display

---- idea: subformulas of conclusions could be derived from subderivations 
app :: Label -> [Term] -> ([Formula] -> Formula) -> Term
app label terms conn = App label (map conclusion terms) terms conn

conclusion :: Term -> Formula
conclusion term = case term of
  App _ _ ts c -> c (map conclusion ts)
  Abs _ t -> conclusion t
  Hyp _ f -> f
  Ass _ f -> f

-- conversions

term2tree :: Term -> Tree Step
term2tree term = case term of
  App rule ps ts c -> Tree (mkStep 0 (c ps) rule (concatMap bindings ts)) (map term2tree ts)
  Abs xs t -> term2tree t
  Hyp x a -> Tree (mkStep x a "hypo" []) []
  Ass x a -> Tree (mkStep x a "ass" []) []

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
 ps :: Int -> [Int] -> Term -> [Line]
 ps ln cont proof = case proof of -- next line number, its context 

   Ass int formula ->
     [mkHypoLine ln formula "ass" int]

   Hyp int formula ->
     [mkHypoLine ln formula "hypo" int] -- line int with hyponumber ??
     
   App label fs pts conn ->              
     let
         pss = psfold cont (pts, ln)
	 ln3 = nextline ln (concat pss)
     in concat pss ++
          [mkLine ln3 cont (conn fs) label (nub (map lastline pss)) (concatMap bindings pts)]
     
   Abs xs t -> ps ln (cont ++ xs) t

 psfold :: [Int] -> ([Term], Int) -> [[Line]]
 psfold cont (pts, n) = case pts of
   p : pp -> case
     ps n cont p of
       [] -> psfold cont (pp, n)
       ls -> ls : psfold cont (pp, nextline n ls)
   [] -> []

 lastline = line . last
 nextline ln p = if null p then ln else lastline p + 1

 -- compress lines by dropping repetitions of hypotheses and renumbering lines
 compress :: Int -> [(Int, Int)] -> [(Int, Int)] -> [Line] -> [Line]
 compress gaps relines rehypos ls = case ls of
   ln : rest | elem (rule (step ln)) ["hypo", "ass"] ->
     case (hyponumber (step ln)) of
       h -> case lookup h rehypos of
         Just k ->  -- old hypothesis: add gap and re-point line number to first occurrence
	   compress (gaps + 1) ((line ln, k) : relines) rehypos rest
         _ ->       -- new hypothesis: update its line number and hypo number to new line number 
	   let nln = line ln - gaps
	   in ln{
	         line = nln,
		 step = (step ln){hyponumber=nln},
		 context = nln : tail (context ln)
		 } :
	      compress gaps ((line ln, nln):relines) ((h, nln) : rehypos) rest
   ln : rest ->
           renumberLine (line ln - gaps) relines rehypos ln :
           compress gaps ((line ln, line ln -gaps):relines) rehypos rest
   _ -> ls 

 -- change the line number and all references to other line numbers
 renumberLine num relines rehypos ln = ln {
    premisses = [maybe p id (lookup p relines) | p <- premisses ln],
    context = [maybe p id (lookup p rehypos) | p <- context ln],
    line = num,
    step = (step ln){discharged = [maybe p id (lookup p rehypos) | p <- discharged (step ln)]}
    }

lines2term :: [Line] -> Term
lines2term = tree2term . lines2steptree

---- TODO
tree2term :: Tree Step -> Term
tree2term (Tree s ts) = case (rule s, discharged s) of
  ("hypo", _) -> hypo (hyponumber s) (formula s) 
  ("ass", _) -> ass (hyponumber s) (formula s) ---- TODO: sequent lhs
  (_, xs@(_:_)) -> app (rule s) (map (Abs xs . tree2term) ts) (const (formula s))
  _ -> app (rule s) (map tree2term ts) (const (formula s))


----------------------------
-- printing
-----------------------------

prf :: Formula -> String
prf = pr 0 where
  pr n f = case f of
    And a b -> parenth 3 n (pr 3 a ++ " \\& " ++ pr 4 b)
    Or a b -> parenth 2 n (pr 2 a ++ " \\vee " ++ pr 3 b)
    If a b -> parenth 1 n (pr 2 a ++ " \\supset " ++ pr 2 b)
    Not a -> parenth 4 n ("\\neg " ++ pr 4 a)
    Falsum -> "\\bot"
    Atom s es -> s ++ if null es then "" else parenth 1 0 (concat (intersperse "," (map prexp es)))

parenth k n f = if k >= n then f else "(" ++ f ++ ")"

prexp :: Exp -> String
prexp e = case e of
  Var i -> "x" ++ show i
  Funapp f es -> f ++ parenth 1 0 (concat (intersperse "," (map prexp es)))


prls :: [Line] -> String
prls lns = unlines $
  "\\[" :
  "\\begin{array}{llllll}" :
  [unwords (intersperse "&" (prl ln)) ++ "\\\\" | ln <- lns] ++
  ["\\end{array}", "\\]"] 

prl :: Line -> [String]
prl ln = [
---  concat (replicate (length (context ln)) "\\mid"),
  concat (intersperse "," (map show (context ln))),
  show (line ln) ++ ".",
  prf (formula (step ln)),
  rule (step ln),
  concat (intersperse ", " (map show (premisses ln))),
  let dis = discharged (step ln)
    in if null dis then "" else "[" ++ concat (intersperse ", " (map show dis)) ++ "]"
  ]
  
prs :: Step -> [String]
prs st = [
  show (hyponumber st) ++ ".",
  prf (formula st),
  rule st,
  concat (map ((","++) . show) (discharged st))
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
  App label ps ts c -> label ++ parenth (unwords (intersperse "," (map prf ps ++ map prt ts)))
  Abs xs t -> parenth (unwords ("\\lambda" : map prvar xs ++  [".", prt t]))
  Hyp x a -> prvar x
  Ass x a -> prcons x
 where
  parenth s = "(" ++ s ++ ")"
  prvar i = "h_" ++ show i
  prcons i = "c_" ++ show i

mathdisplay s = "\\[" ++ s ++ "\\]"

prLatexFile string = unlines [
  "\\documentstyle[proof]{article}",
  "\\setlength{\\parskip}{2mm}",
  "\\setlength{\\parindent}{0mm}",
  "\\newcommand{\\discharge}[2]{\\begin{array}[b]{c} #1 \\\\ #2 \\end{array}}",
  "\\begin{document}",
  string,
  "\\end{document}"
  ]

