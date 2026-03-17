{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2Lean where

import Dedukti.AbsDedukti
import qualified Lean.AbsLean as L

import qualified Lean.PrintLean as PrL

import DeduktiOperations
import CommonConcepts

import System.Environment (getArgs)

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
---  JRules rules -> map transRule rules

---transRule :: Rule -> L.Jmt
---transRule t = case t of
---  RRule qidents_ patt exp -> L.JDef (transPatt patt) (transExp exp)

transExp :: Exp -> L.Exp
transExp t = case t of
  EIdent qident -> L.EIdent (transQIdent qident)
  EApp exp0 exp1 -> case splitApp t of
    (fun@(EIdent (QIdent c)), [arg]) | elem c ["Elem", "Proof"] -> transExp arg
    (fun@(EIdent (QIdent n)), args) | elem n ["nn", "nd"] -> case getNumber fun args of
        Just s -> L.EIdent (L.LIdent s) --- no L.EInt
	_ -> L.EApp (transExp exp0) (transExp exp1)
    (EIdent c, [a, b]) | c == identConj -> L.EAnd (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identDisj -> L.EOr (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identImpl -> L.EIf (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identEquiv -> L.EIff (transExp a) (transExp b)
    (EIdent c, [a]) | c == identNeg -> L.ENot (transExp a)
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
  EFun hypo@(HExp typ) exp -> L.EFun (transExp typ) (transExp exp)

transBind :: Bind -> L.LIdent
transBind t = case t of
  BVar var -> transVar var
  BTyped var exp -> transVar var

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
  QIdent str -> L.LIdent str ---- not quite the same ident syntax ; reserved idents in Lean!

processDeduktiModule :: Module -> IO ()
processDeduktiModule mo@(MJmts jmts) = do
  flip mapM_ jmts processDeduktiJmtTree

processDeduktiJmtTree :: Jmt -> IO ()
processDeduktiJmtTree t = do 
  putStrLn $
    map (\c -> if c==';' then '\n' else c) $  --- to remove bnfc layout artefact ;
      PrL.printTree $ transJmt t

