{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2Agda where

import Dedukti.AbsDedukti
import qualified Agda.AbsAgda as A

import qualified Agda.PrintAgda as PrA

import DeduktiOperations (getNumber, splitApp, isWildIdent)
import DeduktiTheoryAPI
import DeduktiRules

import qualified Data.Map as M

-- skeleton copied from bnfc-generated SkelDedukti

import Dedukti.ErrM
import Dedukti.PrintDedukti (printTree)

printRule :: Rule -> String
printRule = unwords . words . printTree

baseconstants = "BaseConstants"

type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

printAgdaJmts :: [A.Jmt] -> String 
printAgdaJmts = init . unlines . map (unwords . words . init . PrA.printTree)
  -- init to remove artefact ; and last \n; unwords . words to remove the newlines around { }

transModule :: Module -> A.Module
transModule t = case t of
  MJmts jmts -> A.MJmts (concatMap transJmt jmts)

-- | Translating a module: rules are translated as REWRITE rules
-- except in the easy cases, where they become definitions (see "DeduktiRules").
-- Rules between proofs are omitted: they hold by the definitional
-- proof irrelevance of Prop.
printAgdaModule :: [Jmt] -> [String]
printAgdaModule jmts = header ++ concatMap (transJmtCtx ctx) pjmts
  where
    (pjmts, ctx) = prepareRules jmts
    header = if rcHasRewrites ctx
      then ["{-# OPTIONS --rewriting #-}",
            "open import Agda.Builtin.Equality",
            "open import Agda.Builtin.Equality.Rewrite",
            ""]
      else []

transJmtCtx :: RuleContext -> Jmt -> [String]
transJmtCtx ctx t = case t of
  JRules rules -> concatMap (transRuleCtx ctx) rules
  _ -> [printAgdaJmts (transJmt t), ""]

transRuleCtx :: RuleContext -> Rule -> [String]
transRuleCtx ctx rule = case ruleKind ctx eq of
  RKRewrite -> [printAgdaJmts [
      A.JPost name (quantify (reVars eq) (A.EIdEqU (transExp (reLhs eq)) (transExp (reRhs eq)))),
      A.JRewrite [name]
      ], ""]
  kind -> ["-- " ++ omitted kind ++ ": " ++ printRule rule, ""]
 where
  eq = ruleEquationIn ctx rule
  A.AIdent c = transQIdent (reHead eq)
  name = A.AIdent (c ++ "-" ++ show (M.findWithDefault 0 rule (rcNumbers ctx)))
  quantify vars body = case vars of
    [] -> body
    (x, Just typ) : vs -> A.EFun (A.HBracketVarExp [transVar x] (transExp typ)) (quantify vs body)
    _ -> case span ((== Nothing) . snd) vars of
      (untyped, vs) -> A.EAllImpl (map (transVar . fst) untyped) (quantify vs body)

omitted :: RuleKind -> String
omitted kind = case kind of
  RKProof -> "omitted, holds by proof irrelevance"
  _ -> "not translated, Elem erased"

transJmt :: Jmt -> [A.Jmt]
transJmt t = case t of
  JStatic qident exp ->
    [A.JPost (transQIdent qident) (transExp exp)]
  JDef qident (MTExp typ) (MEExp exp) ->
    [A.JTyp (transQIdent qident) [] (transExp typ),
     A.JDef (A.PVar (A.VIdent (transQIdent qident))) (transExp exp)]
  JDef qident (MTExp typ) MENone ->
    [A.JPost (transQIdent qident) (transExp typ)]
  JInj qident mtyp mexp -> transJmt (JDef qident mtyp mexp)  
  JThm qident mtyp mexp -> transJmt (JDef qident mtyp mexp)  
  JRules rules -> [] -- needs a context, see transJmtCtx

transExp :: Exp -> A.Exp
transExp t = case t of
  EIdent qident -> A.EIdent (transQIdent qident)
  EApp exp0 exp1 -> case splitApp t of
    (EIdent (QIdent c), [arg]) | elem c [dkElem, dkProof] -> transExp arg
    (fun@(EIdent (QIdent n)), args) | elem n [nn, nd] -> case getNumber fun args of
        Just s -> A.EIdent (A.AIdent s) --- no A.EInt
        _ -> A.EApp (transExp exp0) (transExp exp1)
    _ -> A.EApp (transExp exp0) (transExp exp1)
  EAbs bind exp -> A.EAbs (transBind bind) (transExp exp)
  EFun hypo exp -> A.EFun (transHypo hypo) (transExp exp)

transBind :: Bind -> A.Bind
transBind t = case t of
  BVar var -> A.BVar [transVar var]
  BTyped var exp -> A.BTyped [transVar var] (transExp exp)

transVar :: QIdent -> A.Var
transVar t = case t of
  _ | isWildIdent t -> A.VWild
  _ -> A.VIdent (transQIdent t)

transHypo :: Hypo -> A.Hypo
transHypo t = case t of
  HExp exp -> A.HExp (transExp exp)
  HVarExp var exp -> A.HVarExp [transVar var] (transExp exp)
  HParVarExp var exp -> A.HVarExp [transVar var] (transExp exp)

transPatt :: Patt -> A.Patt
transPatt t = case t of
  PVar var -> A.PVar (transVar var)
  PBracket patt -> transPatt patt ---
  PApp patt0 patt1 -> A.PApp (transPatt patt0) (transPatt patt1)
--  PBind bind patt -> failure t

transQIdent :: QIdent -> A.AIdent
transQIdent t = case t of
  QIdent str -> A.AIdent (map (\c -> if c=='_' then '8' else c) str) --- replace _
  ---- TODO avoid clashes

processDeduktiModule :: Module -> IO ()
processDeduktiModule (MJmts jmts) = do
  putStrLn ("open import " ++ baseconstants ++ "\n") 
  flip mapM_ jmts processDeduktiJmtTree

processDeduktiJmtTree :: Jmt -> IO ()
processDeduktiJmtTree t = do 
  putStrLn $
    map (\c -> if c==';' then '\n' else c) $  --- to remove bnfc layout artefact ;
      PrA.printTree $ transJmt t
  
