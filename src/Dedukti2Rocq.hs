{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2Rocq where

import Dedukti.AbsDedukti
import qualified Rocq.AbsRocq as C

import qualified Rocq.PrintRocq as PrC

import DeduktiOperations
import CommonConcepts

import System.Environment (getArgs)

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

transJmt :: Jmt -> C.Jmt
transJmt t = case t of
  JStatic qident exp ->
    C.JAxiom (transIdent qident) (transExpProp exp)
  JDef qident (MTExp typ) (MEExp exp) ->
    let (hypos, vtyp) = splitType typ
    in C.JDef (transIdent qident) (transHypos (Just exp) hypos) (transExp vtyp) (transExp (stripAbs hypos exp))
  JThm qident (MTExp typ) (MEExp exp) ->
    let (hypos, vtyp) = splitType typ
    in C.JThm (transIdent qident) (transHypos (Just exp) hypos) (transExp vtyp) (transExp (stripAbs hypos exp))
  JDef qident (MTExp typ) MENone ->
    transJmt (JStatic qident typ)
  JInj qident mtyp mexp -> transJmt (JDef qident mtyp mexp)
---  JRules rules -> map transRule rules

---transRule :: Rule -> C.Jmt
---transRule t = case t of
---  RRule qidents_ patt exp -> C.JDef (transPatt patt) (transExp exp)

transExp :: Exp -> C.Exp
transExp t = case t of
  EIdent qident -> C.EIdent (transIdent qident)
  EApp exp0 exp1 -> case splitApp t of
    (fun@(EIdent (QIdent c)), [arg]) | elem c ["Elem", "Proof"] -> transExp arg
    (fun@(EIdent (QIdent n)), args) | elem n ["nn", "nd"] -> case getNumber fun args of
        Just s -> C.EInt (read s) --- no C.EInt
	_ -> C.EApp (transExp exp0) (transExp exp1)
    (EIdent c, [a, b]) | c == identConj -> C.EAnd (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identDisj -> C.EOr (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identImpl -> C.EIf (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identEquiv -> C.EIff (transExp a) (transExp b)
    (EIdent c, [a])    | c == identNeg -> C.ENot (transExp a)
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
  EFun hypo@(HVarExp _ _) exp -> C.EFunDep (transHypo hypo) (transExp exp)
  EFun hypo@(HParVarExp _ _) exp -> C.EFunDep (transHypo hypo) (transExp exp)
  EFun hypo@(HExp typ) exp -> C.EIf (transExp typ) (transExp exp)

transExpProp :: Exp -> C.Exp
transExpProp t = case t of
  EFun (HVarExp var typ) exp -> C.EAll [transIdent var] (transExp typ) (transExpProp exp)
  EFun (HParVarExp var typ) exp -> transExpProp (EFun (HVarExp var typ) exp)
  _ -> transExp t

transBind :: Bind -> C.CIdent
transBind t = case t of
  BVar var -> transIdent var
  BTyped var exp -> transIdent var

transHypos :: Maybe Exp -> [Hypo] -> [C.Hypo]
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
  QIdent str -> C.CIdent str ---- not quite the same ident syntax ; reserved idents in Rocq!

processDeduktiModule :: Module -> IO ()
processDeduktiModule mo@(MJmts jmts) = do
      flip mapM_ jmts processDeduktiJmtTree

processDeduktiJmtTree :: Jmt -> IO ()
processDeduktiJmtTree t = do
  putStrLn $
    map (\c -> if c==';' then '\n' else c) $  --- to remove bnfc layout artefact ;
      PrC.printTree $ transJmt t


