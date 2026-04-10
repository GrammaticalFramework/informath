{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2MathCore where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import Informath -- superset of MathCore
import CommonConcepts
import DeduktiOperations
import BuildConstantTable
import SpecialConstants

import Data.Char
import qualified Data.Set as S

-- clean-up of remaining annotated idents
jmt2core :: Jmt -> GJmt
jmt2core = cleanup . jmt2jmt . introduceLocalDefinitions where

cleanup :: Informath.Tree a -> Informath.Tree a
cleanup t = case t of
  GStrIdent (GString s) -> GStrIdent (GString (unescapeConstant (stripConstant s)))
  GIdentLabel ident -> GIdentLabel (cleanup ident)
  _ -> Informath.composOp cleanup t

jmt2jmt :: Jmt -> GJmt
jmt2jmt jmt = case jmt of
  JDef ident MTNone (MEExp exp) ->
    GDefUntypedExpJmt (LexLabel "definitionLabel") (ident2exp ident) (exp2exp exp)
  JDef ident@(QIdent sident) (MTExp typ) meexp ->
    let (hypos, kind) = splitType typ
	cat = guessGFCat ident typ
	vhypos = addVarsToHypos meexp hypos
        chypos = hypos2hypos vhypos
	ghypos = GListHypo chypos
	hvars  = (concatMap hypo2vars vhypos)
	shvars = case lookupConstantFull sident of
	  Just (_, _, _, d) -> drop d hvars 
	  _ -> hvars
	definiendum = foldl EApp (EIdent ident) (map EIdent shvars)
    in case cat of
	  _ | S.member cat proofCats -> case meexp of
	    MEExp exp -> GThmJmt   (ident2label ident) ghypos (exp2prop kind) (exp2proof exp)
	    _ ->         GAxiomJmt (ident2label ident) ghypos (exp2prop kind)
	    
          _ | S.member cat expCats -> case meexp of
	    MEExp exp -> GDefExpJmt   definitionLabel ghypos (exp2exp definiendum) (exp2kind kind)
	                                (exp2exp (stripAbs hypos exp))
	    _ ->         GAxiomExpJmt definitionLabel ghypos (exp2exp definiendum) (exp2kind kind) 

          _ | S.member cat kindCats -> case meexp of
	    MEExp exp -> GDefKindJmt   definitionLabel ghypos (exp2kind definiendum) (exp2kind exp)
	    _ ->         GAxiomKindJmt definitionLabel ghypos (exp2kind definiendum)

          _ | S.member cat propCats -> case meexp of
	    MEExp exp -> GDefPropJmt   definitionLabel ghypos (exp2prop definiendum) (exp2prop exp)
	    _ ->         GAxiomPropJmt definitionLabel ghypos (exp2prop definiendum)

          _ -> error ("cannot convert category " ++ cat)

  JStatic ident typ -> jmt2jmt (JDef ident (MTExp typ) MENone)
  JInj ident mtyp mexp -> jmt2jmt (JDef ident mtyp mexp)
  JThm ident mtyp mexp -> jmt2jmt (JDef ident mtyp mexp) 
  JRules rules -> GRewriteJmt (GListRule (map rule2rule rules))  
  _ -> error ("not yet: " ++ printTree jmt)

definitionLabel :: GLabel
definitionLabel = LexLabel "definitionLabel"

axiomLabel :: GLabel
axiomLabel = LexLabel "axiomLabel"

axiomUndefLabel :: QIdent -> GLabel
axiomUndefLabel ident = GIdentLabel (GStrIdent (GString ("Undefined_" ++ show ident)))

fgTree :: Gf a => String -> a
fgTree = fg . readGFTree

annotateExp :: QIdent -> GExp -> GExp
annotateExp qid exp = GAnnotateExp (ident2ident qid) exp

annotateKind :: QIdent -> GKind -> GKind
annotateKind qid exp = GAnnotateKind (ident2ident qid) exp

annotateProp :: QIdent -> GProp -> GProp
annotateProp qid exp = GAnnotateProp (ident2ident qid) exp

annotateProof :: QIdent -> GProof -> GProof
annotateProof qid exp = GAnnotateProof (ident2ident qid) exp

annotateProofExp :: QIdent -> GProofExp -> GProofExp
annotateProofExp qid exp = GAnnotateProofExp (ident2ident qid) exp

funListExp :: QIdent -> [Exp] -> GExp
funListExp ident exps = annotateExp ident $ case ident of
  QIdent s -> case (lookupConstant s, exps) of
    (Just ("Name", c), []) -> GNameExp (fgTree c)
    (Just ("Fun", c), [x]) -> GFunExp (fgTree c) (exp2exp x)
    (Just ("Fun2", c), [x, y]) -> GFun2Exp (fgTree c) (exp2exp x) (exp2exp y)
    (Just ("FunC", c), [x, y]) -> GFunCExp (fgTree c) (exp2exp x) (exp2exp y)
    (Just ("Binder", c), [EAbs b y]) -> GBinderExp (fgTree c) (bind2coreIdent b) (exp2exp y) 
    (Just ("Binder1", c), [x, EAbs b y]) -> GBinder1Exp (fgTree c) (exp2kind x) (bind2coreIdent b) (exp2exp y) 
    (Just ("Binder2", c), [x, z, EAbs b y]) -> GBinder2Exp (fgTree c) (exp2exp x) (exp2exp z) (bind2coreIdent b) (exp2exp y)
    (Just (c, _), _) | S.member c kindCats -> GKindExp (funListKind ident exps)
----NEXT    (Just (c, _), _) | S.member c symbolicCats -> GTermExp (funListTerm ident exps)
    _ -> case exps of
      [] -> ident2exp ident
      _:_ -> GAppExp (ident2exp ident) (gExps (map exp2exp exps))

funListKind :: QIdent -> [Exp] -> GKind
funListKind ident exps = annotateKind ident $ case ident of
  QIdent s -> case (lookupConstant s, exps) of
    (Just ("Noun", c), []) -> GNounKind (fgTree c)
    (Just ("Fam",  c), [x]) -> GFamKind (fgTree c) (exp2kind x) 
    (Just ("Fam2", c), [x, y]) -> GFam2Kind (fgTree c) (exp2kind x) (exp2kind y)
    (Just ("Dep",  c), [x]) -> GDepKind (fgTree c) (exp2exp x) 
    (Just ("Dep2", c), [x, y]) -> GDep2Kind (fgTree c) (exp2exp x) (exp2exp y)
    (Just ("DepC", c), [x, y]) -> GDepCKind (fgTree c) (exp2exp x) (exp2exp y)
    (Just (c, _), _) | S.member c expCats -> GExpKind (funListExp ident exps)
----NEXT    (Just (c, _), _) | S.member c symbolicCats -> GTermKind (GTermExp (funListTerm ident exps))
    _ -> case exps of
      [] -> ident2kind ident
      _:_ -> GExpKind (GAppExp (ident2exp ident) (gExps (map exp2exp exps)))


funListProp :: QIdent -> [Exp] -> GProp
funListProp ident exps = annotateProp ident $ case ident of
  QIdent s -> case (lookupConstant s, map exp2exp exps) of
    (Just ("Adj", c), [x]) -> GAdjProp (fgTree c) x
    (Just ("Adj2", c), [x, y]) -> GAdj2Prop (fgTree c) x y
    (Just ("AdjC", c), [x, y]) -> GAdjCProp (fgTree c) x y
    (Just ("AdjE", c), [x, y]) -> GAdjEProp (fgTree c) x y
    (Just ("Adj3", c), [x, y, z]) -> GAdj3Prop (fgTree c) x y z
    (Just ("Verb", c), [x]) -> GVerbProp (fgTree c) x
    (Just ("Verb2", c), [x, y]) -> GVerb2Prop (fgTree c) x y
    (Just ("VerbC", c), [x, y]) -> GVerbCProp (fgTree c) x y
    (Just ("Noun1", c), [x]) -> GNoun1Prop (fgTree c) x
    (Just ("Noun2", c), [x, y]) -> GNoun2Prop (fgTree c) x y
    (Just ("NounC", c), [x, y]) -> GNounCProp (fgTree c) x y
    (Just (c, _), _) | S.member c kindCats -> GExistKindProp (funListKind ident exps)
    _ -> case exps of
      [] -> GIdentProp (GStrIdent (GString s))
      _:_ -> GAppProp (GStrIdent (GString s)) (gExps (map exp2exp exps)) ---- TODO: this causes "Gt holds for ..." etc

hypoIdents :: GHypo -> [GIdent]
hypoIdents hypo = case hypo of
  GVarsHypo (GListIdent idents) kind_ -> idents
  GBareVarsHypo (GListIdent idents) -> idents
  _ -> []

hypos2hypos :: [Hypo] -> [GHypo]
hypos2hypos hypos = case hypos of
  HVarExp x p : hs | catExp p == "Prop" -> GPropHypo (exp2prop p) : hypos2hypos hs
  hypo@(HVarExp var kind) : hh -> case getVarsHypos kind hh of
    ([], _) -> GVarsHypo (GListIdent [ident2ident var]) (exp2kind kind) : hypos2hypos hh
    (xs, hs) -> GVarsHypo (GListIdent (map ident2ident (var:xs))) (exp2kind kind) : hypos2hypos hs
  HParVarExp var kind : hh -> hypos2hypos (HVarExp var kind : hh) 
  HExp prop : hh -> GPropHypo (exp2prop prop) : hypos2hypos hh
  HLetExp ident exp : hh -> GLocalHypo (GBareLetLocal (ident2ident ident) (exp2exp exp)) : hypos2hypos hh
  HLetTyped ident typ exp : hh -> GLocalHypo (GLetLocal (ident2ident ident) (exp2kind typ) (exp2exp exp)) : hypos2hypos hh
  [] -> []
 where
   getVarsHypos :: Exp -> [Hypo] -> ([QIdent], [Hypo])
   getVarsHypos kind hh = case hh of
     HVarExp x k : hs | k == kind ->
       let (xs, hhs) = getVarsHypos kind hs
       in (x:xs, hhs)
     HParVarExp x k : hs -> getVarsHypos kind (HVarExp x k : hs)
     _ -> ([], hh)

hypo2coreArgKind :: Hypo -> GArgKind
hypo2coreArgKind hypo = case hypo of
  HVarExp var kind | isWildIdent var -> 
    GKindArgKind (exp2kind kind)
  HVarExp var kind -> 
    GIdentsArgKind (exp2kind kind) (GListIdent [ident2ident var]) 
  HParVarExp var kind -> 
    hypo2coreArgKind (HVarExp var kind)
  HExp kind -> 
    GKindArgKind (exp2kind kind)

rule2rule :: Rule -> GRule
rule2rule rule = case rule of
  RRule [] patt exp ->
    GNoVarRewriteRule (patt2exp patt) (exp2exp exp)
  RRule pattbinds patt exp ->
    GRewriteRule
      (GListIdent (map ident2ident (pattbindIdents pattbinds)))
      (patt2exp patt) (exp2exp exp)

exp2kind :: Exp -> GKind
exp2kind exp = case specialDedukti2Informath callBacks exp of
 Just (expr, "Kind") -> fg expr
 Just (expr, "Exp") -> GExpKind (fg expr)
 _ -> case exp of
    EApp (EIdent f) x | f == identElem -> GElemKind (exp2kind x)
    EApp _ _ -> case splitApp exp of
     (fun, args) -> case fun of
        EIdent ident -> funListKind ident args
    EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level
      Just ("Noun", c) -> annotateKind ident $ GNounKind (fgTree c)
      Just _ -> funListKind ident []
      _ -> ident2kind ident ---- TODO funListKind ident []
    EFun _ _ -> case splitType exp of
      (hypos, body) ->
         GFunKind (GListArgKind (map hypo2coreArgKind hypos)) (exp2kind body)
    _ -> GExpKind (exp2exp exp)


exp2prop :: Exp -> GProp
exp2prop exp = case specialDedukti2Informath callBacks exp of
  Just (expr, "Prop") -> fg expr
  Just (expr, "Kind") -> GExistKindProp (fg expr)
  _ -> case exp of
    EIdent ident -> funListProp ident [] ---- GIdentProp (ident2ident ident)
    EApp (EIdent f) x | f == identProof -> GProofProp (exp2prop x)
    EApp _ _ -> case splitApp exp of
     (fun, args) -> case fun of
        EIdent ident -> funListProp ident args
    EFun _ _ -> case splitType exp of
      (hypos, exp) ->
        GAllProp (GListArgKind (map hypo2coreArgKind hypos)) (exp2prop exp)
    EAbs _ _ -> case splitAbs exp of
      (binds, body) -> (exp2prop body) ---- TODO find way to express binds here


callBacks :: CallBacks
callBacks = CallBacks {
  callBind = gf . bind2coreIdent,
  callIdent = gf . findExpIdent,
  callExp  = gf . exp2exp,
  callKind = gf . exp2kind,
  callProp = gf . exp2prop,
  callProof = gf . exp2proof
  }

findExpIdent :: Exp -> GIdent
findExpIdent exp = case exp of
  EIdent x -> ident2ident x
  _ -> error $ "no ident from Exp " ++ printTree exp

exp2exp :: Exp -> GExp
exp2exp exp = case specialDedukti2Informath callBacks exp of
  Just (expr, "Exp") -> fg expr
  Just (expr, "Kind") -> GKindExp (fg expr)
  _ -> case exp of
    EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level 
      Just ("Name", c) -> annotateExp ident $ GNameExp (fgTree c)
      Just _ -> funListExp ident []
      _ -> ident2exp ident

    EApp _ _ -> case splitApp exp of
      (fun, args) -> case fun of
   {-
      EIdent (QIdent "enumset") | length args == 1 -> case enum2list (head args) of
        Just exps@(_:_) -> GEnumSetExp (gExps (map exp2exp exps))
	Just [] -> GNameExp (LexName "emptyset_Name")
	_ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))
   -}
        EIdent (QIdent n) | elem n digitFuns -> case getNumber fun args of
          Just s -> GTermExp (GNumberTerm (GInt (read s)))
	  _ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))
        EIdent ident@(QIdent f) -> funListExp ident args
        _ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))      
    EAbs _ _ -> case splitAbs exp of
      (binds, body) -> GAbsExp (GListIdent (map bind2coreIdent binds)) (exp2exp body)
    EFun _ _ -> 
      case splitType exp of
        (hypos, valexp) ->
          GKindExp (GFunKind (GListArgKind (map hypo2coreArgKind hypos)) (exp2kind valexp))
    _ -> error ("not yet exp2exp: " ++ printTree exp)


exp2proof :: Exp -> GProof
exp2proof exp = case specialDedukti2Informath callBacks exp of
  Just (expr, "Proof") -> fg expr
  _ -> case exp of
    EIdent ident -> GAppProof (GLabelProofExp (ident2label ident)) (GListProof []) 
    EApp _ _ -> case splitApp exp of
      (fun, args) ->
        GAppProof (exp2proofExp fun) (GListProof (map exp2proof args))
    EAbs _ _ -> case splitAbs exp of
      (binds, body) -> GAbsProof (GListHypo (map bind2coreHypo binds)) (exp2proof body)
    _ -> GAppProof (GLabelProofExp
           (ident2label (QIdent ("{|ERROR_exp2proof" ++ printTree exp ++ "|}")))) (GListProof []) 
----    _ -> error ("not yet exp2proof: " ++ printTree exp)

exp2proofExp :: Exp -> GProofExp
exp2proofExp exp = case exp of
  EIdent ident -> GLabelProofExp (ident2label ident)
  EApp _ _ -> case splitApp exp of
    (fun, args) ->
      GAppProofExp (exp2proofExp fun) (gExps (map exp2exp args))
  EAbs _ _ -> case splitAbs exp of
    (binds, body) -> GAbsProofExp (GListHypo (map bind2coreHypo binds)) (exp2proofExp body)
  _ -> error ("not yet exp2proofExp: " ++ printTree exp)

patt2exp :: Patt -> GExp
patt2exp = exp2exp . patt2dexp where
  patt2dexp :: Patt -> Exp
  patt2dexp patt = case patt of
    PVar ident -> EIdent ident
    PApp _ _ -> case splitPatt patt of
      (fun, args) -> case fun of
        PVar ident ->
          foldl EApp (EIdent ident) (map patt2dexp args)
    PBracket p -> patt2dexp p --- ?
    PBind bind p -> EAbs bind (patt2dexp p)

ident2ident :: QIdent -> GIdent
ident2ident ident = case ident of
  QIdent s -> GStrIdent (GString s)

ident2exp :: QIdent -> GExp
ident2exp ident = case ident of
  QIdent [d] | isDigit d -> GTermExp (GNumberTerm (GInt (read [d])))
  QIdent s -> case lookupConstant s of
    Just ("Name", c) -> annotateExp ident $ GNameExp (fgTree c)
    _ -> GTermExp (GIdentTerm (ident2ident ident))

ident2label :: QIdent -> GLabel
ident2label ident = case ident of
  QIdent s -> case lookupConstant s of
    Just ("Label", c) -> fgTree c
    _ -> GIdentLabel (ident2ident ident)

ident2kind :: QIdent -> GKind
ident2kind ident = case ident of
  QIdent s -> case lookupConstant s of
    Just ("Noun", c) -> annotateKind ident $ GNounKind (fgTree c)
    _ -> GExpKind (GTermExp (GIdentTerm (ident2ident ident)))

bind2coreIdent :: Bind -> GIdent
bind2coreIdent = ident2ident . bind2ident

-- needed in proofs by abstraction
bind2coreHypo :: Bind -> GHypo
bind2coreHypo bind = case bind of
  BTyped x exp | isWildIdent x ->
    GPropHypo (exp2prop exp)  
  BTyped var exp ->
    GVarsHypo (GListIdent [ident2ident var]) (exp2kind exp)  
  BVar var ->  
    GBareVarsHypo (GListIdent [ident2ident var])

