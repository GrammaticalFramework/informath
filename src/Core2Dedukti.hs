{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core2Dedukti where

import Dedukti.AbsDedukti
import Informath -- superset of MathCore
import CommonConcepts
import DeduktiOperations

import Data.Char
import qualified Data.Map as M


jmt2dedukti :: M.Map String String -> GJmt -> Jmt
jmt2dedukti lb = eliminateLocalDefinitions . applyLookBack lb . jmt2jmt

applyLookBack ::  M.Map String String -> Dedukti.AbsDedukti.Tree a -> Dedukti.AbsDedukti.Tree a
applyLookBack mb t = case t of
  QIdent s -> maybe t QIdent $ M.lookup s mb
  _ -> Dedukti.AbsDedukti.composOp (applyLookBack mb) t  

jmt2jmt :: GJmt -> Jmt
jmt2jmt jment = case jment of
  GAxiomJmt label (GListHypo hypos) prop ->
    JStatic
      (label2ident label)
      (foldr EFun (prop2dedukti prop) (concatMap hypo2dedukti hypos))
  GThmJmt label (GListHypo hypos) prop proof ->
    JDef
      (label2ident label)
      (MTExp (foldr EFun (prop2dedukti prop) (concatMap hypo2dedukti hypos)))
      (MEExp (proof2dedukti proof))
  GDefPropJmt label_ (GListHypo hypos) prop df ->
    JDef
      (prop2deduktiIdent prop)
      (MTExp (foldr EFun typeProp (concatMap hypo2dedukti hypos)))
      (MEExp (prop2dedukti df))
  GDefKindJmt label_ (GListHypo hypos) kind df ->
    JDef
      (kind2ident kind)
      (MTExp (foldr EFun typeType (concatMap hypo2dedukti hypos)))
      (MEExp (kind2dedukti df))
  GDefExpJmt label_ (GListHypo hypos) exp kind df ->
    JDef
      (exp2ident exp)
      (MTExp (foldr EFun (kind2dedukti kind) (concatMap hypo2dedukti hypos)))
      (MEExp (exp2dedukti df))
  GAxiomPropJmt label_ (GListHypo hypos) prop ->
    JStatic
      (prop2deduktiIdent prop)
      (foldr EFun typeProp (concatMap hypo2dedukti hypos))
  GAxiomKindJmt label_ (GListHypo hypos) kind ->
    JStatic
      (kind2ident kind)
      (foldr EFun typeType (concatMap hypo2dedukti hypos))
  GAxiomExpJmt label_ (GListHypo hypos) exp kind ->
    JStatic
      (exp2ident exp)
      (foldr EFun (kind2dedukti kind) (concatMap hypo2dedukti hypos))
  GRewriteJmt (GListRule rules) -> JRules (map rule2dedukti rules)
  GDefUntypedExpJmt label_ exp df ->
    JDef
      (exp2ident exp)
      MTNone
      (MEExp (exp2dedukti df))

rule2dedukti :: GRule -> Rule
rule2dedukti rule = case rule of
  GRewriteRule (GListIdent idents) patt exp ->
    RRule (map (PBVar . ident2ident) idents) (exp2deduktiPatt patt) (exp2dedukti exp)
  GNoVarRewriteRule patt exp ->
    rule2dedukti (GRewriteRule (GListIdent []) patt exp)

prop2dedukti :: GProp -> Exp
prop2dedukti prop = case prop of
  GProofProp p -> EApp (EIdent (QIdent "Proof")) (prop2dedukti p)
  GFalseProp -> propFalse
  GIdentProp ident -> EIdent (ident2ident ident)
  GAndProp (GListProp props) -> foldl1 propAnd (map prop2dedukti props)
  GOrProp (GListProp props) -> foldl1 propOr (map prop2dedukti props)
  GIfProp a b -> propImp (prop2dedukti a) (prop2dedukti b)
  GNotProp a -> propNeg (prop2dedukti a)
  GIffProp a b -> propEquiv (prop2dedukti a) (prop2dedukti b)
  GAllProp (GListArgKind argkinds) prop ->
    foldr
      (\ (exp, var) y -> propPi exp (EAbs (BVar var) y))
        (prop2dedukti prop)
        (concatMap argkind2dedukti argkinds)
  GExistProp (GListArgKind argkinds) prop ->
    foldr
      (\ (exp, var) y -> propSigma exp (EAbs (BVar var) y))
        (prop2dedukti prop)
        (concatMap argkind2dedukti argkinds)
  GAppProp ident exps ->
    foldl1 EApp ((EIdent (ident2ident ident)) : map exp2dedukti (exps2list exps))
  GAdjProp (GPred3Adj (LexPred3 rel) b c) a ->
    foldl EApp (EIdent (QIdent (rel))) (map exp2dedukti [a, b, c])
  GAdjProp (GReladjAdj (LexReladj rel) b) a ->
    foldl EApp (EIdent (QIdent (rel))) (map exp2dedukti [a, b])
  GAdjProp (GComparAdj (LexCompar rel) b) a ->
    foldl EApp (EIdent (QIdent (rel))) (map exp2dedukti [a, b])
  GAdjProp (LexAdj adj) exp ->
    EApp (EIdent (QIdent (adj))) (exp2dedukti exp)
  GVerbProp (LexVerb verb) exp ->
    EApp (EIdent (QIdent (verb))) (exp2dedukti exp)
  GRelverbProp (LexRelverb verb) x y ->
    EApp (EApp (EIdent (QIdent (verb))) (exp2dedukti x)) (exp2dedukti y)
  GRelnounProp (LexRelnoun noun) x y ->
    EApp (EApp (EIdent (QIdent (noun))) (exp2dedukti x)) (exp2dedukti y)
  GComparnounProp (LexComparnoun noun) x y ->
    EApp (EApp (EIdent (QIdent (noun))) (exp2dedukti x)) (exp2dedukti y)
  GIndexedFormulaProp (GInt i) -> EIdent (unresolvedIndexIdent i)
  _ -> eUndefined ---- TODO complete Informath2Core

hypo2dedukti :: GHypo -> [Hypo]
hypo2dedukti hypo = case hypo of
  GVarsHypo (GListIdent idents) kind ->
    [HVarExp (ident2ident ident) (kind2dedukti kind) | ident <- idents]
  GPropHypo prop ->
    [HExp (prop2dedukti prop)]
  GIndexedLetFormulaHypo (GInt i) ->
    [HExp (EIdent (unresolvedIndexIdent i))]
  GLocalHypo local ->
    [local2dedukti local]

local2dedukti :: GLocal -> Hypo
local2dedukti local = case local of
  GLetLocal ident kind exp ->
    HLetTyped (ident2ident ident) (kind2dedukti kind) (exp2dedukti exp)
  GBareLetLocal ident exp ->
    HLetExp (ident2ident ident) (exp2dedukti exp)

argkind2dedukti :: GArgKind -> [(Exp, QIdent)]
argkind2dedukti argkind = case argkind of
  GIdentsArgKind kind (GListIdent idents) ->
    let dkind = kind2dedukti kind
    in [(dkind, ident2ident ident) | ident <- idents]
  GIndexedFormulaArgKind (GInt i) ->
    [(EIdent (unresolvedIndexIdent i), unresolvedIndexIdent i)]

kind2dedukti :: GKind -> Exp
kind2dedukti kind = case kind of
  GElemKind k -> EApp (EIdent (QIdent "Elem")) (kind2dedukti k)
  GTermKind (GTIdent ident) -> EIdent (ident2ident ident)
  GSetKind (LexSet s) -> EIdent (QIdent (s))
  GSuchThatKind ident kind prop ->
    propSigma
      (kind2dedukti kind)
      (EAbs (BVar (ident2ident ident))
            (prop2dedukti prop))
  GFamKind (LexFam fam) exp ->
    EApp (EIdent (QIdent fam)) (kind2dedukti exp)
  GFam2Kind (LexFam fam) exp1 exp2 ->
    EApp (EApp (EIdent (QIdent fam)) (kind2dedukti exp1)) (kind2dedukti exp2)
  GAppKind ident exps ->
    foldl1 EApp (EIdent (ident2ident ident) : map exp2dedukti (exps2list exps))
  ---- still assuming GF fun is Dedukti ident
  GNounKind (LexNoun noun) ->
    EIdent (QIdent (noun))
  _ -> eUndefined ---- TODO

exp2dedukti :: GExp -> Exp
exp2dedukti exp = case exp of
  GTermExp (GTNumber (GInt n)) -> int2exp n
  GTermExp (GTIdent ident) -> EIdent (ident2ident ident)
  GAppExp exp exps ->
    foldl1 EApp (map exp2dedukti (exp : (exps2list exps)))
  GAbsExp (GListIdent idents) exp ->
    foldr
      (\x y -> EAbs (BVar (ident2ident x)) y)
      (exp2dedukti exp)
      idents
  GNameExp (LexName name) ->
    EIdent (QIdent (name))
  GConstExp (LexConst name) ->
    EIdent (QIdent (name))
  GFunListExp (LexFun fun) (GOneExps x) ->
    EApp (EIdent (QIdent (fun))) (exp2dedukti x)
  GFunListExp (LexFun fun) (GAddExps x (GOneExps y)) ->
    EApp (EApp (EIdent (QIdent (fun))) (exp2dedukti x)) (exp2dedukti y)
  GOperListExp (LexOper oper) (GOneExps x) ->
    EApp (EIdent (QIdent (oper))) (exp2dedukti x)
  GOperListExp (LexOper oper) (GAddExps x (GOneExps y)) ->
    EApp (EApp (EIdent (QIdent (oper))) (exp2dedukti x)) (exp2dedukti y)
  GKindExp kind -> kind2dedukti kind
  GIndexedTermExp (GInt i) -> EIdent (unresolvedIndexIdent i)
  GEnumSetExp exps -> EApp (EIdent (QIdent "enumset")) (list2enum (map exp2dedukti (exps2list exps)))
  GSigmaExp i m n f ->
    EApp (EApp (EApp (EIdent (QIdent "sigma")) (exp2dedukti m)) (exp2dedukti n)) (EAbs (BVar (ident2ident i)) (exp2dedukti f))
  _ -> eUndefined ---- TODO

exp2deduktiPatt :: GExp -> Patt
exp2deduktiPatt exp = case exp of
  GTermExp (GTIdent ident) -> PVar (ident2ident ident)
{- ----
  GAppExp exp (GListExp exps) ->
    foldl1 EApp (map exp2dedukti (exp : exps))
  GAbsExp (GListIdent idents) exp ->
    foldr
      (\x y -> EAbs (BVar (ident2ident x)) y)
      (exp2dedukti exp)
      idents
  GNameExp (LexName name) ->
    EIdent (QIdent (name))
-}

proof2dedukti :: GProof -> Exp
proof2dedukti proof = case proof of
  GAppProof proofexp (GListProof proofs) ->
    foldl1 EApp (proofexp2exp proofexp : map proof2dedukti proofs)
----  GAbsProof hypos proof ->
----  GLabelProofExp label -> 

proofexp2exp :: GProofExp -> Exp
proofexp2exp proofexp = case proofexp of
  GLabelProofExp label -> EIdent (label2ident label)

ident2ident :: GIdent -> QIdent
ident2ident ident = case ident of
  GStrIdent (GString s) -> QIdent (escapeConstant s)

exp2ident :: GExp -> QIdent
exp2ident exp = case exp of
  GTermExp (GTIdent ident) -> ident2ident ident
  _ -> QIdent (takeWhile isAlpha (show (gf exp))) ---- TODO

label2ident :: GLabel -> QIdent
label2ident label = case label of
  LexLabel s -> QIdent (s)
  GStrLabel (GString s) -> QIdent s

kind2ident :: GKind -> QIdent
kind2ident kind = case kind of
  GTermKind (GTIdent ident) -> ident2ident ident
  _ -> QIdent (takeWhile isAlpha (show (gf kind))) ---- TODO

prop2deduktiIdent :: GProp -> QIdent
prop2deduktiIdent prop = case prop of
  GIdentProp (GStrIdent (GString s)) -> QIdent s
  _ -> QIdent (takeWhile isAlpha (show (gf prop))) ---- TODO

eUndefined :: Exp
eUndefined = EIdent (QIdent "UNDEFINED")

exps2list :: GExps -> [GExp]
exps2list exps = case exps of
  GOneExps exp -> [exp]
  GAddExps exp exps -> exp : exps2list exps

