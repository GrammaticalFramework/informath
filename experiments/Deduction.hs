module Deduction where

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
    termDemo exTerm4
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

data Formula =
    And Formula Formula
  | Or Formula Formula
  | If Formula Formula
  | Not Formula
  | Falsum
  | Atom String
  deriving (Show, Eq)

-- proof steps (lines on trees)

data Step = Step {
  hyponumber :: Int,  -- relevant only for hypotheses
  formula :: Formula, -- formula assumed or concluded
  rule :: String,     -- the rule that is used
  discharged :: [Int] -- hyponumbers of discharged formulas
  }
  deriving (Show, Eq)
  
mkStep li fo ru di = Step li fo ru di

-- proof lines in Jaskowski-style notation

data Line = Line {
  line :: Int,        -- line number
  context :: [Int],   -- numbers of open hypotheses
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
    App String [Formula] [Term] ([Formula] -> Formula)
  | Abs [Int] Term
  | Hyp Int Formula
  | Ass Int Formula
-- the last argument of App tells how to combine the argument formulas for display

---- idea: subformulas of conclusions could be derived from subderivations 
app :: String -> [Term] -> ([Formula] -> Formula) -> Term
app label terms conn = App label (map conclusion terms) terms conn

conclusion :: Term -> Formula
conclusion term = case term of
  App _ _ ts c -> c (map conclusion ts)
  Abs _ t -> conclusion t
  Hyp _ f -> f
  Ass _ f -> f

-- macros for natural deduction

andI :: Formula -> Formula -> Term -> Term -> Term
andI a b p q = App "\\& I" [a, b] [p, q] (\ [x, y] -> And x y)

andE1 :: Formula -> Formula -> Term -> Term
andE1 a b p = App "\\& E1" [a, b] [p] (\ [x, y] -> x)

andE2 :: Formula -> Formula -> Term -> Term
andE2 a b p = App "\\& E2" [a, b] [p] (\ [x, y] -> y)

orI1 :: Formula -> Formula -> Term -> Term
orI1 a b p = App "\\vee I1" [a, b] [p] (\ [x, y] -> Or x y)

orI2 :: Formula -> Formula -> Term -> Term
orI2 a b p = App "\\vee I2" [a, b] [p] (\ [x, y] -> Or x y)

orE :: Formula -> Formula -> Formula -> Term -> (Int, Term) -> (Int, Term) -> Term
orE a b c r (x, p) (y, q) = App "\\vee E" [a, b, c] [r, Abs [x] p, Abs [y] q]  (\ [x, y, z] -> z)

ifI :: Formula -> Formula -> (Int, Term) -> Term
ifI a b (x, p) = App "\\supset I" [a, b] [Abs [x] p]  (\ [x, y] -> If x y)

ifE :: Formula -> Formula -> Term -> Term -> Term
ifE a b p q = App "\\supset E" [a, b] [p, q] (\ [x, y] -> y)

notI :: Formula -> (Int, Term) -> Term
notI a (x, p) = App "\\neg I" [a] [Abs [x] p]  (\ [x] -> Not x)

notE :: Formula -> Term -> Term -> Term
notE a p q = App "\\neg E" [a] [p, q] (\ [x] -> Falsum)

hypo :: Int -> Formula -> Term
hypo x a = Hyp x a

ass :: Int -> Formula -> Term
ass x a = Ass x a


-- conversions

term2tree :: Term -> Tree Step
term2tree term = case term of
  App label ps ts c -> Tree (mkStep 0 (c ps) label (concatMap bindings ts)) (map term2tree ts)
  Abs xs t -> term2tree t
  Hyp x a -> Tree (mkStep x a "hypo" []) []
  Ass x a -> Tree (mkStep x a "ass" []) []

bindings :: Term -> [Int]
bindings t = case t of
    Abs xs _ -> xs
    _ -> []

term2lines :: Term -> [Line]
term2lines =
    compress 0 [] [] .
    ps 1 [] where  -- line number, context
 ps :: Int -> [Int] -> Term -> [Line]
 ps ln cont proof = case proof of -- next line number, its context 

   Ass int formula ->
     [mkHypoLine ln formula "ass" int]

   Hyp int formula ->
     [mkHypoLine ln formula "hypo" int] -- line int with hyponumber ??
     
   App label fs pts conn ->              
     let ---- TODO: generalize this to a fold
         ps1 = case pts of
	    p1:_ -> ps ln cont p1       -- context does not change
	 ps2 = case pts of
	    p1:p2:_ -> ps (nextline ln ps1) cont p2 -- next subtree starts from next line
	    _ -> []
	 ps3 = case pts of
	    p1:p2:p3:_ -> ps (nextline ln ps2) cont p3
	    _ -> []
	 pss = filter (not . null) [ps1, ps2, ps3]
	 ln3 = nextline ln (concat pss)
     in concat pss ++
          [mkLine ln3 cont (conn fs) label (nub (map lastline pss)) (concatMap bindings pts)]
     
   Abs xs t -> ps ln (cont ++ xs) t

 lastline = line . last
 nextline ln p = if null p then ln else lastline p + 1

 -- compress lines by dropping repetitions of hypotheses (creates gaps in numbering)
 compress :: Int -> [(Int, Int)] -> [(Int, Int)] -> [Line] -> [Line]
 compress gaps relines rehypos ls = case ls of
   ln : rest | elem (rule (step ln)) ["hypo", "ass"] ->
     case (hyponumber (step ln)) of
       h -> case lookup h rehypos of
         Just k ->
	   compress (gaps + 1) ((line ln, k) : relines) rehypos rest -- omit repeated hypothesis, add gap
         _ ->
	   let nln = line ln - gaps
	   in ln{line = nln, step = (step ln){hyponumber=nln}, context= nln : tail (context ln)} :
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

---------------------------
-- examples
---------------------------

aA = Atom "A"
aB = Atom "B"
aC = Atom "C"

exLines1 :: [Line]
exLines1 = [
  mkHypoLine 1 (If aA aB) "ass" 1,
  mkHypoLine 2 (And aA (Not aB)) "hypo" 2,
  mkLine 3 [2] aA "\\& E1" [2] [],
  mkLine 4 [2] (Not aB) "\\& E2" [2] [],
  mkLine 5 [2] aB "\\supset E" [1, 3] [],
  mkLine 6 [2] Falsum "\\neg E" [4, 5] [],
  mkLine 7 [] (Not (And aA (Not aB))) "\\neg I" [6] [2]
  ]

exLines2 = [
  mkHypoLine 1 (If aA aB) "hypo" 1,
  mkHypoLine 2 (And aA (Not aB)) "hypo" 2,
  mkLine 3 [1,2] aA "\\& E1" [2] [],
  mkLine 4 [1,2] (Not aB) "\\& E2" [2] [],
  mkLine 5 [1,2] aB "\\supset E" [1, 3] [],
  mkLine 6 [1,2] Falsum "\\neg E" [4, 5] [],
  mkLine 7 [1] (Not (And aA (Not aB))) "\\neg I" [6] [2],
  mkLine 8 [] (If (If aA aB) (Not (And aA (Not aB)))) "\\supset I" [7] [1]
  ]
  
exTerm1 =
  ifI (And aA aB) (And aB aA)
    (1, (andI aB aA
      (andE2 aA aB (hypo 1 (And aA aB)))
      (andE1 aA aB (hypo 1 (And aA aB)))))

exTerm2 =
  ifI aA (If aB (And aA aB))
    (1, (ifI aB (And aA aB)
      (2, (andI aA aB
        (hypo 1 aA)
	(hypo 2 aB)))))

exTerm3 =
  ifI (And aA aB) (And (Not (Not aA)) (Not (Not aB)))
    (3, andI (Not (Not aA)) (Not (Not aB))
      (notI (Not aA) 
        (1, notE (aA)
	  (hypo 1 (Not aA))
	  (andE1 aA aB (hypo 3 (And aA aB)))))
      (notI (Not aB) 
        (2, notE (aB)
	  (hypo 2 (Not aB))
	  (andE2 aA aB (hypo 3 (And aA aB))))))

exTerm4 =
  ifI (Or aA aB) (Or aB aA)
    (1, orE aA aB (Or aB aA)
      (hypo 1 (Or aA aB))
      (2, (orI2 aB aA (hypo 2 aA)))
      (3, (orI1 aB aA (hypo 3 aB))))




{-
------------------ intermediate attempts, no more needed -------------


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

fStep fo ru = mkStep 0 fo ru []

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
   IfE f1 f2 p1 p2 ->
     let ps1 = ps ln cont p1
	 ps2 = ps (lastline ps1 + 1) cont p2
	 ln3 = lastline ps2 + 1
	 cont3 = cont
     in concat [ps1, ps2, [mkLine ln3 cont3 f2 "\\supset E" [lastline ps1, lastline ps2] []]]     
   NotI   f1 (x, p1) ->
     let ps1 = ps ln (x:cont) p1
	 ln3 = lastline ps1 + 1
	 cont1 = tail (context (last ps1))
     in concat [ps1, [mkLine ln3 cont1 (Not f1) "\\neg I" [lastline ps1] [x]]]
   NotE   f1 p1 p2 ->
     let ps1 = ps ln cont p1
	 ps2 = ps (lastline ps1 + 1) cont p2
	 ln3 = lastline ps2 + 1
	 cont3 = cont
     in concat [ps1, ps2, [mkLine ln3 cont3 Falsum "\\neg E" [lastline ps1, lastline ps2] []]]     
   FalsumE f1 p1 ->
     let ps1 = ps ln cont p1
	 ln3 = lastline ps1 + 1
	 cont1 = cont
     in concat [ps1, [mkLine ln3 cont1 f1 "\\bot E" [lastline ps1] []]]
 lastline = line . step . last

proofDemo proof = unlines $ intersperse "\n\n" [
    show proof,
    prst (pst proof),
    prls (pls proof)
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

{-
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

-}

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
