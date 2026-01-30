module Deduction where

import Data.List (intersperse, nub, sortOn)

-- experiment with Jan von Plato 2017. "From Gentzen to Jaskowski and Back:
-- Algorithmic Translation of Derivations Between the Two Main Systems of Natural Deduction."
-- code partly borrowed from https://github.com/aarneranta/PESCA

main = do
  putStrLn $ prLatexFile $ unlines $ intersperse "\n\n" [
    "From lines to tree 1",
    linesDemo exLines1,
    "From lines to tree 2",
    linesDemo exLines2,
    "From tree to lines 1 (TODO)",
    treeDemo exTree1,
    "From tree to lines 2 (TODO)",
    treeDemo exTree2
    ]

linesDemo ex = unlines $ intersperse "\n\n" [
    prls ex,
    prlt tex,
    prp (linetree2proof tex)
    ]
  where tex = lines2linetree ex

treeDemo proof = unlines $ intersperse "\n\n" [
    prp proof,
    prlt linetree,
    prls (linetree2lines linetree)
    ]
  where linetree = proof2linetree proof

-------------------------------
-- data types and constructors
-------------------------------

data Formula =
    And Formula Formula
  | Or Formula Formula
  | If Formula Formula
  | Not Formula
  | Falsum
  | Atom String
  deriving (Show, Eq)

data Tree a = Tree {
  root :: a,
  subtrees :: [Tree a]
  }
  deriving (Show, Eq)

data Rule = Rule {
  label :: String,
  proves :: [Formula] -> Formula,
  discharges :: [Int],
  hyponum :: Int --- default 0, not shown
  }

type Proof = Tree Step

data Step = Step {
  line :: Int,  --- shown only for hypotheses
  formula :: Formula,
  rule :: String,
  discharged :: [Int]
  }
  deriving (Show, Eq)

data Line = Line {
  context :: [Int],
  premisses :: [Int],
  step :: Step
  }
  deriving (Show, Eq)

mkStep li fo ru di = Step li fo ru di
mkLine li co fo ru prs di = Line co prs (mkStep li fo ru di)

mkRule lab n f =
  Rule lab (\fs -> if length fs == n then (f (take n fs)) else (error ("arity of " ++ lab))) [] 0 

applyRule :: Rule -> [Proof] -> Proof
applyRule rule proofs = Tree concl proofs where
  concl = (mkStep 0 (proves rule (map (formula . root) proofs)) (label rule) (discharges rule)){
             line = hyponum rule}

-----------------------------------
-- example rules: natural deduction
-----------------------------------

andI  = applyRule (mkRule "\\& I" 2 (\ [a, b] -> And a b))
andE1 = applyRule (mkRule "\\& E1" 1 (\ [And a b] -> a))
andE2 = applyRule (mkRule "\\& E1" 1 (\ [And a b] -> b))
ifI   = \a i -> applyRule ((mkRule "\\supset I" 1 (\ [b] -> If a b)){discharges=[i]})
ifE   = applyRule (mkRule "\\supset E" 2 (\ [a, b] -> b))
notI  = \a i -> applyRule ((mkRule "\\neg I" 1 (\ [_] -> Not a)){discharges=[i]})
notE  = applyRule (mkRule "\\neg E" 2 (\ [a, b] -> Falsum))
hypo  = \a i -> applyRule ((mkRule "hypo" 0 (\_ -> a)){hyponum=i})


------------------------------
-- conversions
------------------------------

nodes :: Tree a -> [a]
nodes (Tree a ts) = a : concatMap nodes ts

maptree :: (a -> b) -> Tree a -> Tree b
maptree f (Tree a ts) = Tree (f a) (map (maptree f) ts)

lines2linetree :: [Line] -> Tree Line
lines2linetree ls = ltr (last ls) where
  ltr concl = Tree concl [ltr (ls !! (prem-1)) | prem <- premisses concl]

linetree2proof :: Tree Line -> Proof
linetree2proof (Tree l ts) = Tree (step l) (map linetree2proof ts)

proof2linetree :: Proof -> Tree Line
proof2linetree tree = prl $ renumber startnumber tree where
  prl (Tree s ts) = Tree (premnumber ts s) (map prl ts)
  startnumber = maximum (map line (nodes tree)) + 1
  renumber n t = t ---- TODO
  premnumber ts s = Line{context=[],  premisses = map (line . root) ts, step=s}

linetree2lines :: Tree Line -> [Line]
linetree2lines = nub . sortOn (line . step) . nodes


----------------------------
-- printing
-----------------------------

prf :: Formula -> String
prf = pr 0 where
  pr n f = case f of
    And a b -> par 3 n (pr 3 a ++ " \\& " ++ pr 4 b)
    Or a b -> par 2 n (pr 2 a ++ " \\vee " ++ pr 3 b)
    If a b -> par 1 n (pr 2 a ++ " \\supset " ++ pr 2 b)
    Not a -> par 4 n ("\\neg " ++ pr 4 a)
    Falsum -> "\\bot"
    Atom s -> s
  par k n f = if k >= n then f else "(" ++ f ++ ")"


prls :: [Line] -> String
prls lns = unlines $
  "\\[" :
  "\\begin{array}{llllll}" :
  [unwords (intersperse "&" (prl ln)) ++ "\\\\" | ln <- lns] ++
  ["\\end{array}", "\\]"] 

prl :: Line -> [String]
prl ln = [
  concat (replicate (length (context ln)) "\\mid"),
  show (line (step ln)) ++ ".",
  prf (formula (step ln)),
  rule (step ln),
  concat (intersperse ", " (map show (premisses ln))),
  let dis = discharged (step ln)
    in if null dis then "" else "[" ++ concat (intersperse ", " (map show dis)) ++ "]"
  ]
  
prs :: Step -> [String]
prs st = [
  show (line st) ++ ".",
  prf (formula st),
  rule st,
  concat (map ((","++) . show) (discharged st))
  ]

prlt :: Tree Line -> String
prlt = mathdisplay . pr  where
  pr (Tree a ts) = case ts of
    [] -> unwords (prl a)
    _ -> "\\infer{" ++ unwords (prl a) ++ "}{" ++ unwords (intersperse "&" (map pr ts)) ++ "}"

prp :: Proof -> String
prp = mathdisplay . pr where
  pr (Tree a ts) = case ts of
    [] -> concat ["\\discharge{", prs a !! 0, "}{", prs a !! 1, "}"]
    _ -> concat ["\\infer[{\\scriptstyle ", prs a !! 2, prs a !! 3, "}]{",
                 prs a !! 1, "}{", unwords (intersperse "&" (map pr ts)), "}"]

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

---------------------------
-- examples
---------------------------

aA = Atom "A"
aB = Atom "B"
aC = Atom "C"

exLines1 :: [Line]
exLines1 = [
  mkLine 1 [] (If aA aB) "ass" [] [],
  mkLine 2 [2] (And aA (Not aB)) "hypo" [] [],
  mkLine 3 [2] aA "\\& E1" [2] [],
  mkLine 4 [2] (Not aB) "\\& E2" [2] [],
  mkLine 5 [2] aB "\\supset E" [1, 3] [],
  mkLine 6 [2] Falsum "\\supset E" [4, 5] [],
  mkLine 7 [] (Not (And aA (Not aB))) "\\supset I" [6] [2]
  ]

exLines2 =
  [line{context = 1 : context line} | line <- exLines1] ++
  [mkLine 8 [] (If (If aA aB) (Not (And aA (Not aB)))) "\\supset I" [7] [1]]

exTree1 =
  ifI (And aA aB) 1 [andI [andE2 [hypo (And aA aB) 1 []], andE1 [hypo (And aA aB) 1 []]]]

exTree2 =
  ifI (And aA aB) 3 [
    andI [
      notI (Not aA) 1 [
        notE [
          hypo (Not aA) 1 [],
          andE1 [hypo (And aA aB) 3 []]
	  ]
	],
      notI (Not aB) 2 [
        notE [
	  hypo (Not aB) 2 [],
	  andE2 [hypo (And aA aB) 3 []]
	  ]
	]
      ]
    ]


{-

infixr 5 +++
infixr 5 ++++

a +++ b  = a ++ " "  ++ b
a ++++ b = a ++ "\n" ++ b

-- hard-coded natural deduction from PESCA
data Proof =
    AndI Formula Formula Proof Proof
  | AndE1 Formula Formula Proof
  | AndE2 Formula Formula Proof
  | OrI1 Formula Formula Proof
  | OrI2 Formula Formula Proof
  | OrE Formula Formula Formula Proof (Int, Proof) (Int, Proof)
  | IfI Formula Formula (Int, Proof)
  | IfE Formula Formula Proof Proof
  | NotI Formula (Int, Proof)
  | NotE Formula Proof Proof
  | FalsumE Formula Proof
  | Hypo Int Formula
  | Assumption Formula
  deriving (Show, Eq)



-- printing to LaTeX proof.sty trees
prt :: Proof -> String
prt proof = case proof of
   Assumption formula ->
     prf formula
   Hypo int formula -> 
     "\\discharge{" ++ show int ++ "}{" ++ prf formula ++ "}"
   AndI  f1 f2 p1 p2 ->
     "\\infer[{\\scriptstyle \\&I}]{" ++++
     prf (And f1 f2) +++ "}{" ++++
     prt p1 ++++ "&" ++++ prt p2 ++++ "}"
   AndE1  f1 f2 p1 ->
     "\\infer[{\\scriptstyle \\vee I1}]{" ++++
     prf f1 +++ "}{" ++++
     prt p1 ++++ "}"
   AndE2  f1 f2 p1 ->
     "\\infer[{\\scriptstyle \\vee I1}]{" ++++
     prf f2 +++ "}{" ++++
     prt p1 ++++ "}"
   OrI1   f1 f2 p1 ->
     "\\infer[{\\scriptstyle \\vee I1}]{" ++++
     prf (Or f1 f2) +++ "}{" ++++
     prt p1 ++++ "}"
   OrI2   f1 f2 p1 ->
     "\\infer[{\\scriptstyle \\vee I2}]{" ++++
     prf (Or f1 f2) +++ "}{" ++++
     prt p1 ++++ "}"
   OrE   f1 f2 f3 p1 (x, p2) (y, p3) ->
     "\\infer[{\\scriptstyle \\vee E," +++ show x ++"," +++ show y +++ "}]{" ++++
     prf f3 +++ "}{" ++++
     prt p1 ++++ "&" ++++ prt p2 ++++ "&" ++++ prt p3 ++++ "}"
   IfI   f1 f2 (x, p1) ->
     "\\infer[{\\scriptstyle \\supset I," +++ show x +++ "}]{" ++++
     prf (If f1 f2) +++ "}{" ++++
     prt p1 ++++ "}"
   IfE   f1 f2 p1 p2 ->
     "\\infer[{\\scriptstyle MP}]{" ++++
     prf f2 +++ "}{" ++++
     prt p1 ++++ "&" ++++ prt p2 ++++ "}"
   FalsumE   f1 p1 ->
     "\\infer[{\\scriptstyle \\bot E}]{" ++++
     prf f1 +++ "}{" ++++
     prt p1 ++++ "}"
-}

