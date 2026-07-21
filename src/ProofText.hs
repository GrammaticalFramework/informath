{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}

module ProofText where

import Dedukti.AbsDedukti hiding (Tree)
import Dedukti.PrintDedukti hiding (prt)
import Dedukti.ParDedukti
import Dedukti.LexDedukti
import qualified Dedukti.ErrM as DE

import DeduktiOperations
import BuildConstantTable
import Environment
import AnnotateDedukti

import Informath
import Dedukti2MathCore (exp2prop, hypos2hypos)

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
--   Tree  (Gentzen style)  -- not used here
--   Lines (Jaskowski/Prawitz style)
--
-- main conversions:
--
--   Term -> Lines
--   Lines -> Term  -- TODO
--

proofDemo :: Env -> Module -> Module -> (GUnit -> String) -> String
proofDemo env base (MJmts proofs) informalize = 
  prLatexFile $ unlines $ intersperse "\n\n" [
    oneProof env dkmap informalize t e | JThm _ (MTExp t) (MEExp e) <- proofs]
 where
   dkmap = identTypes base

oneProof :: Env -> M.Map QIdent Exp -> (GUnit -> String) -> Exp -> Exp -> String  
oneProof env dkmap lin typ exp = unlines $ intersperse "\n\n" [
    "\\subsection*{From term to lines and back}"
    , "Original proved theorem"
    , verbatim (printTree typ)
    , "Original proof exp"
    , verbatim (printTree exp)
    , "Generated linear proof"
    , prls prle linesterm
    , "Generated linear proof with informalized steps"
    , prls (prlu lin) (map (line2unitline env dkmap) linesterm)
    , "\\clearpage"
    ]
  where
    term = ignoreFirstArguments (dropTable (symbolTable env)) (typeAnnotate dkmap [] typ exp)
    linesterm = term2lines term
    


line2unitline :: Env ->  M.Map QIdent Exp -> Line Exp -> Line GUnit
line2unitline env dkmap line = line {
  step = (step line){formula = unit}
  }
 where
   unit = case rule (step line) of
     h | constant h -> GConclusionUnit (exp2prop fla)
     h -> GHyposUnit (GListHypo (hypos2hypos [HVarExp h fla]))
   fla = head (annotateDedukti env (formula (step line)))

   annotateDedukti env t = annotateDkIdents msyns msymbs (constantTable (symbolTable env)) M.empty t  -- no dropTable again
    where
      msyns = argValueMaybeInt "-synonyms" (flags env)
      msymbs = argValueMaybeInt "-symbolics" (flags env)

   constant h = maybe False (const True) (M.lookup h dkmap)


-------------------------------
-- data types and constructors
-------------------------------

-- proof steps (lines on trees)

data Step a = Step {
  hypovar :: QIdent,      -- relevant only for hypotheses
  formula :: a,           -- formula assumed or concluded
  rule    :: QIdent,      -- the rule that is used
  discharged :: [QIdent]  -- hypolabels of discharged formulas
  }
  deriving (Show, Eq)
  
mkStep li fo ru di = Step li fo ru di

-- proof lines in Jaskowski-style notation

data Line a = Line {
  line      :: Int,      -- line number
  context   :: [QIdent], -- labels of open hypotheses
  premisses :: [Int],    -- line numbers of premisses
  step :: Step a         -- the main content of the line
  }
  deriving (Show, Eq)

mkLine li co fo ru prs di = Line li co prs (mkStep noIdent fo ru di)
mkHypoLine li fo ru hy = Line li [hy] [] (mkStep hy fo ru [])

noIdent = QIdent "#NOIDENT" ---- 

-- an object-language type Elem A (as opposed to a Proof of a proposition)
isElemType :: Exp -> Bool
isElemType e = case splitApp e of
  (EIdent _, _) -> True
  _ -> False


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
        BTyped _ ty -> ty
        BVar _ -> case typ of
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

{- -- now in AnnotateDedukti
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
-}

-----------------------
-- linear proofs
----------------------

term2lines :: Exp -> [Line Exp]
term2lines =
    compress 0 [] [] .
    ps 1 []             
      where
 -- generate lines starting with this line number and context
 ps :: Int -> [QIdent] -> Exp -> [Line Exp]
 ps ln cont proof = case proof of -- next line number, its context 

   ETyped e typ -> case splitApp e of
    (EIdent fun, args) ->
      let
         (argss, prems, lnConcl) = psArgs ln cont args
      in concat argss ++
         [mkLine lnConcl cont typ fun (nub prems) (concatMap absIdents args)]
   EAbs _ _ -> case splitAbs proof of
     (binds, body) -> ps ln (cont ++ map bind2ident binds) body

   _ -> error ("term2lines " ++ printTree proof)

 -- process arguments left to right, threading line numbers; each argument yields its lines
 -- (eigenvariable lines for its Elem-typed binders, then its own derivation) together with the
 -- line numbers to cite as premisses (those eigenvariables and the argument's conclusion)
 psArgs :: Int -> [QIdent] -> [Exp] -> ([[Line Exp]], [Int], Int)
 psArgs n _ [] = ([], [], n)
 psArgs n cont (a:as) =
   let (ls, prem) = psArg n cont a
       (lss, prems, nf) = psArgs (nextline n ls) cont as
   in (ls : lss, prem ++ prems, nf)

 psArg :: Int -> [QIdent] -> Exp -> ([Line Exp], [Int])
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
 compress :: Int -> [(Int, Int)] -> [((QIdent, Exp), Int)] -> [Line Exp] -> [Line Exp]
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

prls :: (Line a -> [String]) -> [Line a] -> String
prls pr lns = unlines $
  "\\[" :
  "\\begin{array}{llllll}" :
  [unwords (intersperse "&" (pr ln)) ++ "\\\\" | ln <- lns] ++
  ["\\end{array}", "\\]"] 

prle :: Line Exp -> [String]
prle ln
-- object variable assumption: "x : Elem A" on one line
---- TODO isElemType for other types than Exp; should this even be tested here?
  | null (premisses ln) {- && isElemType (formula (step ln)) -} =  
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
  
prlu :: (GUnit -> String) -> Line GUnit -> [String]
prlu lin ln
-- object variable assumption: "x : Elem A" on one line
---- TODO isElemType for other types than Exp; should this even be tested here?
  | null (premisses ln) =
      [ concat (intersperse "," (map printTree (context ln))),
        show (line ln) ++ ".",
        "\\mbox{" ++ lin (formula (step ln)) ++"}",
        "", "", "" ]
  | otherwise = [
      concat (intersperse "," (map printTree (context ln))),
      show (line ln) ++ ".",
      "\\mbox{" ++ lin (formula (step ln)) ++"}",
      printTree (rule (step ln)),
      concat (intersperse ", " (map show (premisses ln))),
      let dis = discharged (step ln)
        in if null dis then "" else "[" ++ concat (intersperse ", " (map printTree dis)) ++ "]"
  ]
  

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

