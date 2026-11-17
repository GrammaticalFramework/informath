{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2Lean where

import Dedukti.AbsDedukti
import qualified Lean.AbsLean as L

import qualified Lean.PrintLean as PrL

import DeduktiOperations
import DeduktiTheoryAPI
import DeduktiRules
import DeduktiStructures

import qualified Data.Map as M
import Dedukti.PrintDedukti (printTree)

-- skeleton copied from bnfc-generated SkelDedukti

import Dedukti.ErrM

baseconstants = "BaseConstants"

type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

printLeanJmt :: L.Jmt -> String 
printLeanJmt = init . PrL.printTree -- init to remove artefact ;

transModule :: Module -> L.Module
transModule t = case t of
  MJmts jmts -> L.MJmts (map transJmt jmts)

-- | Translating a module. Lean has no rewrite rules: the rules are
-- translated as equations (simp lemmas), except in the easy cases, where they
-- become definitions (see "DeduktiRules"). Rules between proofs are omitted:
-- they hold by the definitional proof irrelevance of Prop.
-- Informath structures become Lean structures (see "DeduktiStructures").
printLeanModule :: [Jmt] -> [String]
printLeanModule jmts = header ++ concatMap trans pjmts
  where
    -- definitions using axioms cannot be compiled
    header = ["noncomputable section", ""]
    (sjmts, records) = prepareStructures jmts
    (pjmts, ctx) = prepareRules sjmts
    trans t = case t of
      JStatic c _ | Just r <- M.lookup c records ->
        map printLeanJmt (transStructure (levels M.! srKind r) r) ++ [""]
      _ -> transJmtCtx ctx t
    levels = foldl (\ ls r -> M.insert (srKind r) (recordLevel ls r) ls) M.empty
      [r | JStatic c _ <- pjmts, Just r <- [M.lookup c records]]

-- | A structure, and exporting its constructor and fields, so that they
-- can be used as the constructor and the selectors were.
transStructure :: Int -> StructureRecord -> [L.Jmt]
transStructure level r = [
    L.JStructure name universe con [L.FField (transQIdent f) (transExp t) | (f, t) <- srFields r],
    L.JExport name (con : [transQIdent f | (f, _) <- srFields r])
    ]
  where
    name = transQIdent (srKind r)
    con = transQIdent (srConstructor r)
    universe = if level > 0 then L.EApp (L.EIdent (L.LIdent "Type")) (L.EInt 1) else L.EIdent (L.LIdent "Type")

transJmtCtx :: RuleContext -> Jmt -> [String]
transJmtCtx ctx t = case t of
  JRules rules -> concatMap (transRuleCtx ctx) rules
  _ -> [printLeanJmt (transJmt t), ""]

transRuleCtx :: RuleContext -> Rule -> [String]
transRuleCtx ctx rule = case ruleKind ctx eq of
  RKRewrite -> [printLeanJmt (
      L.JSimpAxiom name [] (quantify (reVars eq) (equation (transExp (reLhs eq)) (transExp (reRhs eq))))), ""]
  RKProof -> ["-- omitted, holds by proof irrelevance: " ++ printRule rule, ""]
  _ -> ["-- not translated, Elem erased: " ++ printRule rule, ""]
 where
  eq = ruleEquationIn ctx rule
  L.LIdent c = transQIdent (reHead eq)
  name = L.LIdent (c ++ "_" ++ show (M.findWithDefault 0 rule (rcNumbers ctx)))
  -- if the type of the head is dependent, the two sides may have types that
  -- are equal only modulo rules: then heterogeneous equality
  equation a b = if isDependent then L.EApp (L.EApp (L.EIdent (L.LIdent "HEq")) a) b else L.EEq a b
  -- (a dependency on an argument that the rule matches by a non-variable pattern)
  isDependent = case M.lookup (reHead eq) (rcTypes ctx) of
    Just typ -> let (hypos, val) = splitType typ
                    args = snd (splitApp (reLhs eq))
                in or [elem x (identsInTree val) |
                       (h, arg) <- zip hypos args, x <- hypo2topvars h, not (isVariable arg)]
    _ -> False
  isVariable a = case a of
    EIdent x -> elem x (map fst (reVars eq))
    _ -> False
  quantify vars body = case vars of
    [] -> body
    (x, Just typ) : vs -> L.EAll [transVar x] (transExp typ) (quantify vs body)
    _ -> case span ((== Nothing) . snd) vars of
      (untyped, vs) -> L.EAllU (map (transVar . fst) untyped) (quantify vs body)

printRule :: Rule -> String
printRule = unwords . words . printTree

transJmt :: Jmt -> L.Jmt
transJmt t = case t of
  JStatic qident exp ->
    let (hypos, typ) = splitType exp
    in L.JAxiom (transQIdent qident) (transHypos MENone hypos) (transExp typ)
  JDef qident (MTExp typ) (MEExp exp) ->
    let (hypos, vtyp) = splitType typ
    in L.JDef (transQIdent qident) (transHypos (MEExp exp) hypos) (transExp vtyp) (transExp (stripAbs hypos exp))
  JThm qident (MTExp typ) (MEExp exp) ->
    let (hypos, vtyp) = splitType typ
    in L.JThm (transQIdent qident) (transHypos (MEExp exp) hypos) (transExp vtyp) (transExp (stripAbs hypos exp))
  JDef qident (MTExp typ) MENone ->
    transJmt (JStatic qident typ)
  JInj qident mtyp mexp -> transJmt (JDef qident mtyp mexp)
  JThm qident mtyp MENone -> transJmt (JDef qident mtyp MENone)
  -- JRules need a context: see transJmtCtx

transExp :: Exp -> L.Exp
transExp t = case t of
  EIdent qident -> L.EIdent (transQIdent qident)
  EApp exp0 exp1 -> case splitApp t of
    (EIdent (QIdent c), [arg]) | elem c [dkElem, dkProof] -> transExp arg
    (f, [arg]) | isStructureKind f -> transExp arg  -- Str K is the structure K
    (fun@(EIdent (QIdent n)), args) | elem n [nn, nd] -> case getNumber fun args of
        Just s -> L.EIdent (L.LIdent s) --- no L.EInt
        _ -> L.EApp (transExp exp0) (transExp exp1)
    (EIdent c, [a, b]) | c == identConj -> L.EAnd (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identDisj -> L.EOr (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identImpl -> L.EIf (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identEquiv -> L.EIff (transExp a) (transExp b)
    (EIdent c, [a]) | c == identNot -> L.ENot (transExp a)
    (EIdent c, [a, b]) | c == identEq -> L.EEq (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identNeq -> L.ENeq (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identLt -> L.ELt (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identGt -> L.EGt (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identLeq -> L.ELeq (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identGeq -> L.EGeq (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identPlus -> L.EPlus (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identMinus -> L.EMinus (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identTimes -> L.ETimes (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identDiv -> L.EDiv (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identPi -> case b of
      EAbs bind body -> L.EAll [transBind bind] (transExp a) (transExp body)
    (EIdent c, [a, b]) | c == identSigma -> case b of
      EAbs bind body -> L.EExist [transBind bind] (transExp a) (transExp body)
      _ -> L.EApp (transExp exp0) (transExp exp1)
    _ -> L.EApp (transExp exp0) (transExp exp1)
  EAbs bind exp -> L.EAbs [transBind bind] (transExp exp)
  EFun hypo@(HVarExp _ _) exp -> L.EFunDep (transHypo hypo) (transExp exp)
  EFun hypo@(HParVarExp _ _) exp -> L.EFunDep (transHypo hypo) (transExp exp)
  EFun (HExp typ) exp -> L.EFun (transExp typ) (transExp exp)

transBind :: Bind -> L.LIdent
transBind t = case t of
  BVar var -> transVar var
  BTyped var _ -> transVar var

transVar :: QIdent -> L.LIdent
transVar t = case t of
  _ | isWildIdent t -> L.LIdent "x__" --- ?
  _ -> transQIdent t

transHypos :: MExp -> [Hypo] -> [L.Hypo]
transHypos mexp hypos = compress (map transHypo vhypos)
  where
    vhypos = case mexp of
      MENone -> hypos
      _ -> addVarsToHypos mexp hypos
    
    compress :: [L.Hypo] -> [L.Hypo]
    compress hs = case hs of
      h@(L.HVarExp vs exp) : hh -> case span ((== exp) . hypoType) hh of
        (hh1@(_:_), hh2) -> L.HVarExp (vs ++ concatMap hypoVars hh1) exp : compress hh2
        ([], _) -> h : compress hh
      [] -> []

    hypoType :: L.Hypo -> L.Exp
    hypoType (L.HVarExp _ exp) = exp
    
    hypoVars :: L.Hypo -> [L.LIdent]
    hypoVars (L.HVarExp vars _ ) = vars

transHypo :: Hypo -> L.Hypo
transHypo t = case t of
  HExp exp -> L.HVarExp [L.LIdent "_"] (transExp exp)
  HVarExp var exp -> L.HVarExp [transVar var] (transExp exp)
  HParVarExp var exp -> L.HVarExp [transVar var] (transExp exp)

transQIdent :: QIdent -> L.LIdent
transQIdent t = case t of
  QIdent str@('{' : '|' : _) -> L.LIdent (plainIdentName str) -- {|*|} as star
  QIdent str -> L.LIdent str ---- not quite the same ident syntax ; reserved idents in Lean!

processDeduktiModule :: Module -> IO ()
processDeduktiModule (MJmts jmts) = do
  flip mapM_ jmts processDeduktiJmtTree

processDeduktiJmtTree :: Jmt -> IO ()
processDeduktiJmtTree t = do 
  putStrLn $
    map (\c -> if c==';' then '\n' else c) $  --- to remove bnfc layout artefact ;
      PrL.printTree $ transJmt t

