module Deduction where

import Data.List (intersperse)

-- experiment with Jan von Plato 2017. "From Gentzen to Jaskowski and Back:
-- Algorithmic Translation of Derivations Between the Two Main Systems of Natural Deduction."
-- code partly borrowed from https://github.com/aarneranta/PESCA

infixr 5 +++
infixr 5 ++++

a +++ b  = a ++ " "  ++ b
a ++++ b = a ++ "\n" ++ b

aA = Atom "A"
aB = Atom "B"
aC = Atom "C"

main = do
  putStrLn $ prLatexFile $ prls exLines1 ++ "\n\n" ++ prlst exLines1

data Formula =
    And Formula Formula
  | Or Formula Formula
  | If Formula Formula
  | Not Formula
  | Falsum
  | Atom String
  deriving (Show, Eq)

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

data Line = Line {
  line :: Int,
  context :: [Int],
  formula :: Formula,
  rule :: String,
  premisses :: [Int],
  discharged :: [Int]
  }
  deriving (Show, Eq)

mkLine = Line

exLines1 :: [Line]
exLines1 = [
  mkLine 1 [] (If aA aB) "Assumption" [] [],
  mkLine 2 [2] (And aA (Not aB)) "Hypo" [] [],
  mkLine 3 [2] aA "AndE1" [2] [],
  mkLine 4 [2] (Not aB) "AndE2" [2] [],
  mkLine 5 [2] aB "IfE" [1, 3] [],
  mkLine 6 [2] Falsum "IfE" [4, 5] [],
  mkLine 7 [] (Not (And aA (Not aB))) "IfI" [6] [2]
  ]


data Tree a = Tree a [Tree a]
  deriving (Show, Eq)


lines2tree :: [Line] -> Tree Line
lines2tree ls = ltr (last ls) where
  ltr concl = Tree concl [ltr (ls !! (prem-1)) | prem <- premisses concl]


--proof2lines :: Proof -> [Line]

prf :: Formula -> String
prf = pr 0 where
  pr n f = case f of
    And a b -> par 3 n (pr 3 a ++ " \\& " ++ pr 4 b)
    Or a b -> par 2 n (pr 2 a ++ " \\vee " ++ pr 3 b)
    If a b -> par 1 n (pr 2 a ++ " \\supset " ++ pr 2 b)
    Not a -> par 4 n ("\\sim " ++ pr 4 a)
    Falsum -> "\\bot"
    Atom s -> s
  par k n f = if k >= n then f else "(" ++ f ++ ")"

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


prls :: [Line] -> String
prls lns = unlines $
  "\\[" :
  "\\begin{array}{llllll}" :
  [unwords (intersperse "&" (prl ln)) ++ "\\\\" | ln <- lns] ++
  ["\\end{array}", "\\]"] 

prl :: Line -> [String]
prl ln = [
  concat (replicate (length (context ln)) "\\mid"),
  show (line ln) ++ ".",
  prf (formula ln),
  "\\mbox{" ++ rule ln ++ "}",
  concat (intersperse ", " (map show (premisses ln))),
  if null (discharged ln) then "" else "[" ++ concat (intersperse ", " (map show (discharged ln))) ++ "]"
  ]

prlst :: [Line] -> String
prlst = mathdisplay . pr . lines2tree where
  pr (Tree a ts) = case ts of
    [] -> unwords (prl a)
    _ -> "\\infer{" ++ unwords (prl a) ++ "}{" ++ unwords (intersperse "&" (map pr ts)) ++ "}"

mathdisplay s = "\\[" ++ s ++ "\\]"


prLatexFile string =
 "\\documentstyle[proof]{article}" ++++
 "\\setlength{\\parskip}{2mm}" ++++
 "\\setlength{\\parindent}{0mm}" ++++
 "\\newcommand{\\discharge}[2]{\\begin{array}[b]{c} #1 \\\\ #2 \\end{array}}" ++++
 "\\begin{document}" ++++ 
 string ++++ 
 "\\end{document}"
