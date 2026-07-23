{-# OPTIONS -Wno-missing-signatures #-}

module DeduktiTheoryAPI where

import Dedukti.AbsDedukti

-- referring to BaseConstants.dk

identFalse = QIdent "false"
identConj = QIdent "and"
identDisj = QIdent "or"
identImpl = QIdent "if"
identPi = QIdent "forall"
identSigma = QIdent "exists"
identNot = QIdent "not"
identEquiv = QIdent "iff"

identDig = QIdent "Dig"
identNat =  QIdent "Nat"
identInt =  QIdent "Int"
identRat =  QIdent "Rat"
identReal =  QIdent "Real"
identnat2int = QIdent "nat2int"
identNat2real = QIdent "nat2real"
identInt2real = QIdent "int2real"
identRat2real = QIdent "rat2real"
identPlus =  QIdent "plus"
identMinus =  QIdent "minus"
identTimes =  QIdent "times"
identDiv =  QIdent "div"
identPow = QIdent "pow"
identNeg = QIdent "neg"
identEq =  QIdent "Eq"
identLt =  QIdent "Lt"
identGt =  QIdent "Gt"
identNeq =  QIdent "Neq"
identLeq =  QIdent "Leq"
identGeq =  QIdent "Geq"

-- Peano-style Nat constructors in BaseConstants.dk
identZero = QIdent "0"
identSucc = QIdent "succ"

-- these are to be peeled away
dkProof = "Proof"
dkElem  = "Elem"
identProof = QIdent dkProof
identElem = QIdent dkElem

identSuchThat = QIdent "suchthat"

-- logical constants in BaseConstants.dk
propFalse = EIdent identFalse
propAnd x y = EApp (EApp (EIdent identConj) x) y
propOr x y = EApp (EApp (EIdent identDisj) x) y
propImp x y = EApp (EApp (EIdent identImpl) x) y
propEquiv x y = EApp (EApp (EIdent identEquiv) x) y
propNot x = EApp (EIdent identNot) x

propPi kind predi = EApp (EApp (EIdent identPi) kind) predi
propSigma kind predi = EApp (EApp (EIdent identSigma) kind) predi

-- built-in types
typeProp = EIdent identProp
typeSet = EIdent identSet
typeType = EIdent identType 

identProp = QIdent "Prop"
identSet = QIdent "Set"
identType = QIdent "Type"

--- needed for typing conclusions of proofs
expTyped x t = EApp (EApp (EIdent (QIdent "typed")) x) t
expNegated x = EApp (EIdent identNeg) x

-- Dedukti representation of digits in BaseConstants.dk
digitFuns :: [String]
digitFuns = [nn, nd]
nn = "nn"
nd = "nd"

identNn = QIdent nn
identNd = QIdent nd

-- Dedukti representation of lists in BaseConstants.dk
identNil = QIdent "nil"
identCons = QIdent "cons"

-- Constants related to set in BaseConstants.dk
identEnumset = QIdent "enumset"



type DTree a = Dedukti.AbsDedukti.Tree a

