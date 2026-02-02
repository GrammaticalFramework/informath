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
    proofDemo exProof1 ,
    "From tree to lines 2 (TODO)",
    proofDemo exProof2, {- 
    "From tree to lines 3 (TODO)",
    proofDemo exProof3 -}
    "From tree to lines 4 (TODO)",
    proofDemo exProof4
    ]

linesDemo ex = unlines $ intersperse "\n\n" [
    prls ex,
    prlt tex,
    prst (linetree2steptree tex)
    ]
  where tex = lines2linetree ex

proofDemo proof = unlines $ intersperse "\n\n" [
    show proof,
    prst (pst proof),
    prls (pls proof)
    ]

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
fStep fo ru = mkStep 0 fo ru []

mkLine li co fo ru prs di = Line co prs (mkStep li fo ru di)

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

linetree2steptree :: Tree Line -> Tree Step
linetree2steptree = maptree step

linetree2lines :: Tree Line -> [Line]
linetree2lines = nub . sortOn (line . step) . nodes


------------------------------------------
-- hard-coded natural deduction from PESCA
------------------------------------------

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

pst :: Proof -> Tree Step
pst proof = case proof of
   Assumption formula ->
     Tree (mkStep 0 formula "ass" []) []
   Hypo int formula -> 
     Tree (mkStep int formula "hypo" []) []
   AndI  f1 f2 p1 p2 ->
     Tree (fStep (And f1 f2) "\\& I") [pst p1, pst p2]
   AndE1  f1 f2 p1 ->
     Tree (fStep f1 "\\& E1") [pst p1]
   AndE2  f1 f2 p1 ->
     Tree (fStep f2 "\\& E2") [pst p1]
   OrI1   f1 f2 p1 ->
     Tree (fStep (Or f1 f2) "\\vee I1") [pst p1]
   OrI2   f1 f2 p1 ->
     Tree (fStep (Or f1 f2) "\\vee I2") [pst p1]
   OrE   f1 f2 f3 p1 (x, p2) (y, p3) ->
     Tree (mkStep 0 f3 "\\vee E" [x, y]) [pst p1, pst p2, pst p3]
   IfI   f1 f2 (x, p1) ->
     Tree (mkStep 0 (If f1 f2) "\\supset I" [x]) [pst p1]
   IfE   f1 f2 p1 p2 ->
     Tree (fStep f2 "\\supset E") [pst p1, pst p2]
   NotI   f1 (x, p1) ->
     Tree (mkStep 0 (Not f1) "\\neg I" [x]) [pst p1]
   NotE   f1 p1 p2 ->
     Tree (fStep Falsum "\\not E") [pst p1, pst p2]
   FalsumE   f1 p1 ->
     Tree (fStep f1 "\\bot E") [pst p1]

pls :: Proof -> [Line]
pls = nub . ps 1 [] where  -- line number, context
 ps :: Int -> [Int] -> Proof -> [Line]
 ps ln cont proof = case proof of
   Assumption formula ->
     [mkLine ln cont formula "ass" [] []]
   Hypo int formula -> 
     [mkLine ln (cont) formula "hypo" [] []]
   AndI f1 f2 p1 p2 ->
     let ps1 = ps ln cont p1
	 ps2 = ps (lastline ps1 + 1) cont p2
	 ln3 = lastline ps2 + 1
	 cont3 = cont
     in concat [ps1, ps2, [mkLine ln3 cont3 (And f1 f2) "\\& I" [lastline ps1, lastline ps2] []]]
   AndE1 f1 f2 p1 ->
     let ps1 = ps ln cont p1
	 ln3 = lastline ps1 + 1
	 cont3 = cont
     in concat [ps1, [mkLine ln3 cont3 f1 "\\& E1" [lastline ps1] []]]
   AndE2 f1 f2 p1 ->
     let ps1 = ps ln cont p1
	 ln3 = lastline ps1 + 1
	 cont3 = cont
     in concat [ps1, [mkLine ln3 cont3 f2 "\\& E2" [lastline ps1] []]]
   OrI1 f1 f2 p1 ->
     let ps1 = ps ln cont p1
	 ln3 = lastline ps1 + 1
	 cont3 = cont -- context (last ps1)
     in concat [ps1, [mkLine ln3 cont3 (Or f1 f2) "\\vee I1" [lastline ps1] []]]
   OrI2 f1 f2 p1 ->
     let ps1 = ps ln cont p1
	 ln3 = lastline ps1 + 1
	 cont3 = cont
     in concat [ps1, [mkLine ln3 cont3 (Or f1 f2) "\\vee I2" [lastline ps1] []]]
     
   OrE   f1 f2 f3 p1 (x, p2) (y, p3) ->
     let ps1 = ps ln cont p1
	 ps2 = ps (lastline ps1 + 1) (x:cont) p2
	 ps3 = ps (lastline ps2 + 1) (y:cont) p3
	 ln3 = lastline ps3 + 1
     in concat [ps1, ps2, ps3, [mkLine ln3 cont f3 "\\vee E" (map lastline [ps1, ps2, ps3]) [x, y]]]

   IfI f1 f2 (x, p1) ->
     let ps1 = ps ln (x:cont) p1
	 ln3 = lastline ps1 + 1
	 cont1 = tail (context (last ps1))
     in concat [ps1, [mkLine ln3 cont1 (If f1 f2) "\\supset E" [lastline ps1] [x]]]
{-
   IfE f1 f2 p1 p2 ->
     concat [pls p1, pls p2, [mkLine 0 [] f2 "\\supset E" [] []]]
   NotI   f1 (x, p1) ->
     concat [pls p1, [mkLine 0 [] (Not f1) "\\neg I" [] [x]]]
   NotE   f1 p1 p2 ->
     concat [pls p1, pls p2, [mkLine 0 [] Falsum "\\neg E" [] []]]
   FalsumE   f1 p1 ->
     concat [pls p1, [mkLine 0 [] f1 "\\bot E" [] []]] 
-}
 lastline = line . step . last

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

prst :: Tree Step -> String
prst = mathdisplay . pr where
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

exProof1 =
  IfI (And aA aB) (And aB aA)
    (1, (AndI aB aA
      (AndE2 aA aB (Hypo 1 (And aA aB)))
      (AndE1 aA aB (Hypo 1 (And aA aB)))))

exProof2 =
  IfI aA (If aB (And aA aB))
    (1, (IfI aB (And aA aB)
      (2, (AndI aA aB
        (Hypo 1 aA)
	(Hypo 2 aB)))))

exProof3 =
  IfI (And aA aB) (And (Not (Not aA)) (Not (Not aB)))
    (3, AndI (Not (Not aA)) (Not (Not aB))
      (NotI (Not aA) 
        (1, NotE (Not aA)
	  (Hypo 1 (Not aA))
	  (AndE1 aA aB (Hypo 3 (And aA aB)))))
      (NotI (Not aB) 
        (2, NotE (Not aB)
	  (Hypo 2 (Not aB))
	  (AndE1 aA aB (Hypo 3 (And aA aB))))))

exProof4 =
  IfI (Or aA aB) (Or aB aA)
    (1, OrE aA aB (Or aB aA)
      (Hypo 1 (Or aA aB))
      (2, (OrI2 aB aA (Hypo 2 aA)))
      (3, (OrI1 aB aA (Hypo 3 aB))))



{-
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
-}

{-

-----------------------------------
-- building Tree Step directly
-----------------------------------

data Rule = Rule {
  label :: String,
  proves :: [Formula] -> Formula,
  discharges :: [Int],
  hyponum :: Int --- default 0, not shown
  }

mkRule lab n f =
  Rule lab (\fs -> if length fs == n then (f (take n fs)) else (error ("arity of " ++ lab))) [] 0 

applyRule :: Rule -> [Tree Step] -> Tree Step
applyRule rule proofs = Tree concl proofs where
  concl = (mkStep 0 (proves rule (map (formula . root) proofs)) (label rule) (discharges rule)){
             line = hyponum rule}

andI  = applyRule (mkRule "\\& I" 2 (\ [a, b] -> And a b))
andE1 = applyRule (mkRule "\\& E1" 1 (\ [And a b] -> a))
andE2 = applyRule (mkRule "\\& E1" 1 (\ [And a b] -> b))
ifI   = \a i -> applyRule ((mkRule "\\supset I" 1 (\ [b] -> If a b)){discharges=[i]})
ifE   = applyRule (mkRule "\\supset E" 2 (\ [a, b] -> b))
notI  = \a i -> applyRule ((mkRule "\\neg I" 1 (\ [_] -> Not a)){discharges=[i]})
notE  = applyRule (mkRule "\\neg E" 2 (\ [a, b] -> Falsum))
hypo  = \a i -> applyRule ((mkRule "hypo" 0 (\_ -> a)){hyponum=i})


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
-}

{-

infixr 5 +++
infixr 5 ++++

a +++ b  = a ++ " "  ++ b
a ++++ b = a ++ "\n" ++ b

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

