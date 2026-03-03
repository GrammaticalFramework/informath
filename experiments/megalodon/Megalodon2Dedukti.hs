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
  JDefinition ident typ exp -> D.JDef (ident2ident ident) (D.MTExp (exp2exp typ)) (D.MEExp (exp2exp exp))
  _ -> D.JDef (D.QIdent "TODO_Jmt") D.MTNone D.MENone

exp2exp :: Exp -> D.Exp
exp2exp exp = case exp of
  EIdent ident -> D.EIdent (ident2ident ident)
  EApp fun arg -> D.EApp (exp2exp fun) (exp2exp arg)
  EForall bind exp -> foldr D.EFun (exp2exp exp) (bind2hypos bind)
  EArrow a b -> D.EFun (D.HExp (exp2exp a)) (exp2exp b)
  _ -> D.EIdent (D.QIdent "TODO_Exp")

bind2hypos :: Bind -> [D.Hypo]
bind2hypos bind = case bind of
  BTyping vars exp -> [D.HVarExp (var2ident var) dexp | var <- vars, let dexp = exp2exp exp]
  _ -> [D.HExp (D.EIdent (D.QIdent "TODO_Bind"))]
  
var2ident :: Var -> D.QIdent
var2ident var = case var of
  VIdent ident -> ident2ident ident
  VWild -> D.QIdent "_"

ident2ident :: Ident -> D.QIdent
ident2ident (Ident s) = D.QIdent s

