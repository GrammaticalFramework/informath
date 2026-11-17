{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2Rocq where

import Dedukti.AbsDedukti
import qualified Rocq.AbsRocq as C

import qualified Rocq.PrintRocq as PrC

import DeduktiOperations
import DeduktiTheoryAPI
import DeduktiRules
import DeduktiStructures

import qualified Data.Map as M
import qualified Data.Set as S
import Dedukti.PrintDedukti (printTree)

-- skeleton copied from bnfc-generated SkelDedukti

import Dedukti.ErrM

baseconstants = "BaseConstants"

type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

printRocqJmt :: C.Jmt -> String
printRocqJmt = PrC.printTree

transModule :: Module -> C.Module
transModule t = case t of
  MJmts jmts -> C.MJmts (map transJmt jmts)

-- | Translating a module: rules become Rewrite Rules, which need
-- rocq compile -allow-rewrite-rules, except in the easy cases, where they
-- become definitions (see "DeduktiRules"). The constants in left-hand sides
-- are declared as Symbols. Rules between proofs are omitted.
printRocqModule :: [Jmt] -> [String]
-- Informath structures become records (see "DeduktiStructures").
printRocqModule jmts = header ++ concatMap trans pjmts
  where
    (sjmts, records) = prepareStructures jmts
    (pjmts, ctx) = prepareRules sjmts
    trans t = case t of
      JStatic c _ | Just r <- M.lookup c records ->
        [unwords (words (printRocqJmt (transRecord r))), ""]
      _ -> transJmtCtx ctx t
    header = if rcHasRewrites ctx
      then ["(* rewrite rules: compile with rocq compile -allow-rewrite-rules *)", ""]
      else []

transJmtCtx :: RuleContext -> Jmt -> [String]
transJmtCtx ctx t = case t of
  JRules rules ->
    let eqs = [(r, ruleEquation r) | r <- rules]
        rewrites = [eq | (_, eq) <- eqs, ruleKind ctx eq == RKRewrite]
        others = [(r, k) | (r, eq) <- eqs, let k = ruleKind ctx eq, k /= RKRewrite]
        name = case [(r, eq) | (r, eq) <- eqs, ruleKind ctx eq == RKRewrite] of
          (r, eq) : _ -> let C.CIdent c = transIdent (reHead eq) in
            C.CIdent (c ++ "_rules" ++ show (M.findWithDefault 0 r (rcNumbers ctx)))
          _ -> C.CIdent "rules"
    in ["(* " ++ omitted k ++ ": " ++ printRule r ++ " *)" | (r, k) <- others] ++
       [printRocqJmt (C.JRewrite name (map (transRewrite . metaVariables) rewrites)) | not (null rewrites)] ++
       [""]
  JStatic c typ | S.member c (rcSymbols ctx) -> [printRocqJmt (C.JSymbol (transIdent c) (transExpProp typ)), ""]
  JDef c (MTExp typ) MENone | S.member c (rcSymbols ctx) -> transJmtCtx ctx (JStatic c typ)
  JInj c (MTExp typ) MENone | S.member c (rcSymbols ctx) -> transJmtCtx ctx (JStatic c typ)
  _ -> [printRocqJmt (transJmt t), ""]
 where
  -- wildcards are kept as holes _, which Rocq infers
  transRewrite eq = C.RRewrite (transExp (holes eq (reLhs eq))) (transExp (reRhs eq))
  holes eq = substExp (M.fromList [(QIdent ('?' : w), EIdent (QIdent "_")) | QIdent w <- reWilds eq])
  omitted k = case k of
    RKProof -> "omitted, between proofs"
    _ -> "not translated, Elem erased"

-- | A record: its fields are global projections, usable as the selectors were.
transRecord :: StructureRecord -> C.Jmt
transRecord r = C.JRecord (transIdent (srKind r)) (C.EIdent (C.CIdent "Type")) (transIdent (srConstructor r))
  [C.FField (transIdent f) (transExpProp t) | (f, t) <- srFields r]

printRule :: Rule -> String
printRule = unwords . words . printTree

transJmt :: Jmt -> C.Jmt
transJmt t = case t of
  JStatic qident exp ->
    C.JAxiom (transIdent qident) (transExpProp exp)
  JDef qident (MTExp typ) (MEExp exp) ->
    let (hypos, vtyp) = splitType typ
    in C.JDef (transIdent qident) (transHypos (MEExp exp) hypos) (transExp vtyp) (transExp (stripAbs hypos exp))
  JThm qident (MTExp typ) (MEExp exp) ->
    let (hypos, vtyp) = splitType typ
    in C.JThm (transIdent qident) (transHypos (MEExp exp) hypos) (transExp vtyp) (transExp (stripAbs hypos exp))
  JDef qident (MTExp typ) MENone ->
    transJmt (JStatic qident typ)
  JInj qident mtyp mexp -> transJmt (JDef qident mtyp mexp)
  JThm qident mtyp MENone -> transJmt (JDef qident mtyp MENone)
  -- JRules need a context: see transJmtCtx


transExp :: Exp -> C.Exp
transExp t = case t of
  EIdent (QIdent s@('?' : _)) -> C.EMeta (C.MIdent s)
  EIdent qident -> C.EIdent (transIdent qident)
  EApp exp0 exp1 -> case splitApp t of
    (EIdent (QIdent c), [arg]) | elem c [dkElem, dkProof] -> transExp arg
    (f, [arg]) | isStructureKind f -> transExp arg  -- Str K is the structure K
    (fun@(EIdent (QIdent n)), args) | elem n [nn, nd] -> case getNumber fun args of
        Just s -> C.EInt (read s) --- no C.EInt
        _ -> C.EApp (transExp exp0) (transExp exp1)
    (EIdent c, [a, b]) | c == identConj -> C.EAnd (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identDisj -> C.EOr (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identImpl -> C.EIf (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identEquiv -> C.EIff (transExp a) (transExp b)
    (EIdent c, [a])    | c == identNot -> C.ENot (transExp a)
    (EIdent c, [a, b]) | c == identEq -> C.EEq (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identLt -> C.ELt (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identGt -> C.EGt (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identPlus -> C.EPlus (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identMinus -> C.EMinus (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identTimes -> C.ETimes (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identDiv -> C.EDiv (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identPi -> case b of
      EAbs bind body -> C.EAll [transBind bind] (transExp a) (transExp body)
      _ -> C.EApp (transExp exp0) (transExp exp1)
    (EIdent c, [a, b]) | c == identSigma -> case b of
      EAbs bind body -> C.EExist [transBind bind] (transExp a) (transExp body)
      _ -> C.EApp (transExp exp0) (transExp exp1)
    _ -> C.EApp (transExp exp0) (transExp exp1)
  EAbs bind exp -> C.EAbs [transBind bind] (transExp exp)
  EFun (HVarExp var typ) exp -> C.EAll [transIdent var] (transExp typ) (transExp exp)
  EFun (HParVarExp var typ) exp -> C.EAll [transIdent var] (transExp typ) (transExp exp)
  EFun (HExp typ) exp -> C.EIf (transExp typ) (transExp exp)

transExpProp :: Exp -> C.Exp
transExpProp t = case t of
  EFun (HVarExp var typ) exp -> C.EAll [transIdent var] (transExp typ) (transExpProp exp)
  EFun (HParVarExp var typ) exp -> transExpProp (EFun (HVarExp var typ) exp)
  _ -> transExp t

transBind :: Bind -> C.CIdent
transBind t = case t of
  BVar var -> transIdent var
  BTyped var _ -> transIdent var

transHypos :: MExp -> [Hypo] -> [C.Hypo]
transHypos mexp hypos = compress (map transHypo vhypos)
  where
    vhypos = addVarsToHypos mexp hypos

    compress :: [C.Hypo] -> [C.Hypo]
    compress hs = case hs of
      h@(C.HVarExp vs exp) : hh -> case span ((== exp) . hypoType) hh of
        (hh1@(_:_), hh2) -> C.HVarExp (vs ++ concatMap hypoVars hh1) exp : compress hh2
        ([], _) -> h : compress hh
      [] -> []

    hypoType :: C.Hypo -> C.Exp
    hypoType (C.HVarExp _ exp) = exp

    hypoVars :: C.Hypo -> [C.CIdent]
    hypoVars (C.HVarExp vars _ ) = vars

transHypo :: Hypo -> C.Hypo
transHypo t = case t of
--  HExp exp -> C.HExp (transExp exp) -- not reached due to addVarsToHypos
  HVarExp var exp -> C.HVarExp [transIdent var] (transExp exp)
  HParVarExp var exp -> C.HVarExp [transIdent var] (transExp exp)

transIdent :: QIdent -> C.CIdent
transIdent t = case t of
  QIdent str@('{' : '|' : _) -> C.CIdent (plainIdentName str) -- {|*|} as star
  QIdent str -> C.CIdent str ---- not quite the same ident syntax ; reserved idents in Rocq!

processDeduktiModule :: Module -> IO ()
processDeduktiModule (MJmts jmts) = do
      flip mapM_ jmts processDeduktiJmtTree

processDeduktiJmtTree :: Jmt -> IO ()
processDeduktiJmtTree t = do
  putStrLn $
    map (\c -> if c==';' then '\n' else c) $  --- to remove bnfc layout artefact ;
      PrC.printTree $ transJmt t


