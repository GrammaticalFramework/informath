{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Megalodon2Dedukti where

import qualified Dedukti.AbsDedukti as D
import qualified Dedukti.PrintDedukti as P
import AbsMegalodon

megalodon2dedukti :: Doc -> String
megalodon2dedukti = P.printTree . doc2module

doc2module :: Doc -> D.Module
doc2module doc = case doc of
  DJmts jmts -> D.MJmts (map jmt2jmt jmts)

jmt2jmt :: Jmt -> D.Jmt
jmt2jmt jmt = case jmt of
  JAxiom ident exp -> D.JStatic (ident2ident ident) (exp2exp exp)
  JTheorem ident exp -> D.JStatic (ident2ident ident) (exp2exp exp)
  JDefinition ident typ exp ->
    D.JDef (ident2ident ident) (D.MTExp (exp2exp typ)) (D.MEExp (exp2exp exp))
  JParameter ident exp -> D.JStatic (ident2ident ident) (exp2exp exp)
  JHypothesis ident exp -> D.JStatic (ident2ident ident) (exp2exp exp)
  JStep step -> D.JDef (D.QIdent "step") D.MTNone (D.MEExp (step2exp step))
  _ -> D.JDef (D.QIdent "TODO_Jmt") D.MTNone D.MENone

-- deep embedding of steps
step2exp :: Step -> D.Exp
step2exp step = case step of
  SHAssume ident exp -> wrap "HAssume" [D.EIdent (ident2ident ident), exp2exp exp]
  STLet ident exp -> wrap "TLet" [D.EIdent (ident2ident ident), exp2exp exp]
  SClaim ident exp -> wrap "Claim" [D.EIdent (ident2ident ident), exp2exp exp]
  STDLet ident typ exp -> wrap "TDLet" [D.EIdent (ident2ident ident), exp2exp typ, exp2exp exp]
  SLet exp -> wrap "Let" [exp2exp exp]
  SExact exp -> wrap "Exact" [exp2exp exp]
  SApply exp -> wrap "Apply" [exp2exp exp]
  SProve exp -> wrap "Prove" [exp2exp exp]
  SWitness exp -> wrap "Witness" [exp2exp exp]
  SRewrite exp -> wrap "Rewrite" [exp2exp exp]
  SARewrite exp -> wrap "ARewrite" [exp2exp exp]
  SAssume vars -> wrap "Assume" [D.EIdent (var2ident var) | var <- vars] --- variable #args
  SSet ident exp -> wrap "SSet" [D.EIdent (ident2ident ident), exp2exp exp]
  STSet ident typ exp -> wrap "STSet" [D.EIdent (ident2ident ident), exp2exp typ, exp2exp exp]
  SIdent ident -> wrap "SIdent" [D.EIdent (ident2ident ident)]
  SQed -> wrap "SQed" []
  SPlus step0 -> wrap "SPlus" [step2exp step0]
  SMinus step0 -> wrap "SMinus" [step2exp step0]
  SStar step0 -> wrap "SStar" [step2exp step0]
  _ -> D.EIdent (D.QIdent "TODO_Step")

exp2exp :: Exp -> D.Exp
exp2exp exp = case exp of
  EIdent ident -> D.EIdent (ident2ident ident)
  EInt int -> D.EIdent (D.QIdent (show int)) --- should be exploded
  ESet -> D.EIdent (D.QIdent "set")
  EQuest -> D.EIdent (D.QIdent "{|?|}")
  ECompr a b -> wrap "Compr" [exp2exp a, exp2exp b]
  EEnum exps -> wrap "Enum" [exp2exp e | e <- exps] --- variable #args
  EApp fun arg -> D.EApp (exp2exp fun) (exp2exp arg)
  EEq x y -> wrap "Eq" [exp2exp x, exp2exp y]
  ECEq x y -> wrap "CEq" [exp2exp x, exp2exp y]
  ECIn x y -> wrap "CIn" [exp2exp x, exp2exp y]

  EForall bind exp -> foldr D.EFun (exp2exp exp) (bind2hypos bind)
  ENForall bind exp -> wrap "not" [foldr D.EFun (exp2exp exp) (bind2hypos bind)]
  EExists bind exp -> foldr (binder "exists") (exp2exp exp) (bind2binds bind)
  ENExists bind exp -> wrap "not" [foldr (binder "exists") (exp2exp exp) (bind2binds bind)]
  EOrs bind exp -> foldr (binder "Ors") (exp2exp exp) (bind2binds bind) --- \/_ ; find out meaning
  EArrow a b -> D.EFun (D.HExp (exp2exp a)) (exp2exp b)
  EFun bind exp -> foldr D.EAbs (exp2exp exp) (bind2binds bind)

  E_BinderP_Pi bind exp -> foldr (binder "Pi_") (exp2exp exp) (bind2binds bind)
  E_BinderP_Sigma bind exp -> foldr (binder "Sigma_") (exp2exp exp) (bind2binds bind)
  
  _ -> D.EIdent (D.QIdent "TODO_Exp")

bind2hypos :: Bind -> [D.Hypo]
bind2hypos bind = case bind of
  BTyping vars exp -> [D.HVarExp (var2ident var) dexp | var <- vars, let dexp = exp2exp exp]
  BCIn vars exp -> [D.HVarExp (var2ident var) dexp | var <- vars, let dexp = exp2exp exp]
  _ -> [D.HExp (D.EIdent (D.QIdent "TODO_BindHypo"))]
  
bind2binds :: Bind -> [D.Bind]
bind2binds bind = case bind of
  BTyping vars exp -> [D.BTyped (var2ident var) dexp | var <- vars, let dexp = exp2exp exp]
  BIdents vars -> [D.BVar (var2ident var) | var <- vars]
  _ -> [D.BVar (D.QIdent "TODO_BindBind")]
  
var2ident :: Var -> D.QIdent
var2ident var = case var of
  VIdent ident -> ident2ident ident
  VWild -> D.QIdent "_"

ident2ident :: Ident -> D.QIdent
ident2ident (Ident s) = D.QIdent s

binder :: String -> D.Bind -> D.Exp -> D.Exp
binder b bind exp = wrap b [D.EAbs bind exp]  --- domain included in bind?

-- EOrs bind exp -> foldr dkOrs (exp2exp exp) (bind2binds bind)
dkOrs :: D.Bind -> D.Exp -> D.Exp
dkOrs bind exp = wrap "ors" [D.EAbs bind exp]  --- domain included in bind?

wrap s xs = foldl D.EApp (D.EIdent (D.QIdent s)) xs
uni s = wrap s []
eUnit = wrap "UNIT" []
