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
  _ -> D.EIdent (D.QIdent "TODO_Step")

exp2exp :: Exp -> D.Exp
exp2exp exp = case exp of
  EIdent ident -> D.EIdent (ident2ident ident)
  EInt int -> D.EIdent (D.QIdent (show int)) ---
  ESet -> D.EIdent (D.QIdent "set")

  EEq x y -> wrap "Eq" [exp2exp x, exp2exp y]
  ECEq x y -> wrap "CEq" [exp2exp x, exp2exp y]
  ECIn x y -> wrap "CIn" [exp2exp x, exp2exp y]

  EApp fun arg -> D.EApp (exp2exp fun) (exp2exp arg)
  EForall bind exp -> foldr D.EFun (exp2exp exp) (bind2hypos bind)
  ENForall bind exp -> wrap "not" [foldr D.EFun (exp2exp exp) (bind2hypos bind)]
  EArrow a b -> D.EFun (D.HExp (exp2exp a)) (exp2exp b)
  EFun bind exp -> foldr D.EAbs (exp2exp exp) (bind2binds bind)
  _ -> D.EIdent (D.QIdent "TODO_Exp")

bind2hypos :: Bind -> [D.Hypo]
bind2hypos bind = case bind of
  BTyping vars exp -> [D.HVarExp (var2ident var) dexp | var <- vars, let dexp = exp2exp exp]
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

wrap s xs = foldl D.EApp (D.EIdent (D.QIdent s)) xs
uni s = wrap s []
eUnit = wrap "UNIT" []
