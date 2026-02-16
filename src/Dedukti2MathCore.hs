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
  JDef ident (MTExp typ) meexp ->
    let mexp = case meexp of
          MEExp exp -> Just exp
          _ -> Nothing
    in case (splitType typ, guessGFCat ident typ) of
      ((hypos, kind), c) -> 
        let vhypos = addVarsToHypos mexp hypos
            chypos = hypos2hypos vhypos
	in case () of
	  _ | elem c ["Label", "Proof", "Unit"] -> 
            (maybe GAxiomJmt
	        (\exp x y z -> GThmJmt x y z (exp2proof exp)) mexp)
              (ident2label ident)
              (GListHypo (hypos2hypos hypos))
              (exp2prop kind)
          _ | elem c ["Fam", "Fam2", "Noun", "Kind"] ->
            (maybe (GAxiomKindJmt axiomLabel)
	        (\exp x y -> GDefKindJmt definitionLabel x y (exp2kind exp)) mexp)
              (GListHypo chypos)
	      (exp2kind (foldl EApp (EIdent ident) (map EIdent (concatMap hypo2vars vhypos))))
          _ | elem c ["Fun", "Fun2", "FunC", "Exp", "Name", "Unknown"] ->
            (maybe (GAxiomExpJmt axiomLabel)
	        (\exp x y z -> GDefExpJmt definitionLabel x y z (exp2exp (stripAbs hypos exp))) mexp)
              (GListHypo chypos)
              (exp2exp (foldl EApp (EIdent ident) (map EIdent (concatMap hypo2vars vhypos))))
              (exp2kind kind)
          _ | elem c ["Adj", "Verb", "Noun1", "Adj2", "AdjC", "AdjE",
	              "Verb2", "Noun2", "Adj3", "Prop"] ->
            (maybe (GAxiomPropJmt axiomLabel)
	        (\exp x y -> GDefPropJmt definitionLabel x y (exp2prop exp)) mexp)
              (GListHypo chypos)
	      (exp2prop (foldl EApp (EIdent ident) (map EIdent (concatMap hypo2vars vhypos))))
          _ -> error ("cannot convert category " ++ c)

  JStatic ident typ ->
    jmt2jmt (JDef ident (MTExp typ) MENone)
  JInj ident mtyp mexp ->
    jmt2jmt (JDef ident mtyp mexp)
  JThm ident mtyp mexp ->
    jmt2jmt (JDef ident mtyp mexp) 
  JRules rules -> GRewriteJmt (GListRule (map rule2rule rules))  
  _ -> error ("not yet: " ++ printTree jmt)

definitionLabel :: GLabel
definitionLabel = LexLabel "definitionLabel"

axiomLabel :: GLabel
axiomLabel = LexLabel "axiomLabel"

axiomUndefLabel :: QIdent -> GLabel
axiomUndefLabel ident = GIdentLabel (GStrIdent (GString ("Undefined_" ++ show ident)))

funListExp :: QIdent -> [GExp] -> GExp
funListExp ident exps = case ident of
  QIdent s -> case (lookupConstant s, exps) of
    (Just ("Name", c), []) -> GNameExp (LexName c)
    (Just ("Fun", c), [x]) -> GFunExp (LexFun c) x
    (Just ("Fun2", c), [x, y]) -> GFun2Exp (LexFun2 c) x y
    (Just ("FunC", c), [x, y]) -> GFunCExp (LexFunC c) x y
    (Just ("Noun", c), [x]) -> case splitFunPrep c of
      (f, [p]) -> GFunExp (GNounPrepFun (LexNoun f) (LexPrep p)) x
    (Just ("Noun", c), [x, y]) -> case splitFunPrep c of
      (f, [p, q]) -> GFun2Exp (GNounPrepFun2 (LexNoun f) (LexPrep p) (LexPrep q)) x y
      (f, [p])   -> GFunCExp (GNounPrepFunC (LexNoun f) (LexPrep p)) x y
      _ -> error ("incorrect Fun2/FunC: " ++ c)
    _ -> case exps of
      [] -> ident2exp ident
      _:_ -> GAppExp (ident2exp ident) (gExps exps)

funListKind :: QIdent -> [Exp] -> GKind
funListKind ident exps = case ident of
  QIdent s -> case (lookupConstant s, exps) of
    (Just ("Noun", c), []) -> case splitFunPrep c of
      (f, [a]) -> GNounKind (GNounAdjNoun (LexNoun f) (LexAdj a))
      (f, []) -> GNounKind (LexNoun c)
    (Just ("Noun", c), [x]) -> case splitFunPrep c of
      (f, [p]) -> GFamKind (GNounPrepFam (LexNoun f) (LexPrep p)) (exp2kind x)
      _ -> error ("incorrect Fam: " ++ c)
    (Just ("Noun", c), [x, y]) -> case splitFunPrep c of
      (f, [p, q]) -> GFam2Kind (GNounPrepFam2 (LexNoun f) (LexPrep p) (LexPrep q))
                        (exp2kind x) (exp2kind y)
      _ -> error ("incorrect Fam2: " ++ c)
    (Just ("Fam",  c), [x]) -> GFamKind (LexFam c) (exp2kind x) 
    (Just ("Fam2", c), [x, y]) -> GFam2Kind (LexFam2 c) (exp2kind x) (exp2kind y)
    _ -> case exps of
      [] -> ident2kind ident
      _:_ -> GAppKind (ident2ident ident) (gExps (map exp2exp exps))


funListProp :: QIdent -> [GExp] -> GProp
funListProp ident exps = case ident of
  QIdent s -> case (lookupConstant s, exps) of
    (Just ("Adj", c), [x]) ->
      GAdjProp (LexAdj c) x
    (Just ("Adj", c), [x, y]) -> case splitFunPrep c of
      (f, ["C"]) -> GAdjCProp (GAdjAdjC (LexAdj f)) x y
      (f, ["E"]) -> GAdjEProp (GAdjAdjE (LexAdj f)) x y
      (f, [p]) -> GAdj2Prop (GAdjPrepAdj2 (LexAdj f) (LexPrep p)) x y
      _ -> error ("incorrect Adj2/AdjC/AdjE: " ++ c)
    (Just ("Adj2", c), [x, y]) ->
      GAdj2Prop (LexAdj2 c) x y
    (Just ("AdjC", c), [x, y]) ->
      GAdjCProp (LexAdjC c) x y
    (Just ("AdjE", c), [x, y]) ->
      GAdjEProp (LexAdjE c) x y
    (Just ("Adj3", c), [x, y, z]) ->
      GAdj3Prop (LexAdj3 c) x y z
    (Just ("Verb", c), [x]) ->
      GVerbProp (LexVerb c) x
    (Just ("Verb2", c), [x, y]) ->
      GVerb2Prop (LexVerb2 c) x y
    (Just ("Noun1", c), [x]) ->
      GNoun1Prop (LexNoun1 c) x
    (Just ("Noun2", c), [x, y]) ->
      GNoun2Prop (LexNoun2 c) x y
    _ -> case exps of
      [] -> GIdentProp (GStrIdent (GString s))
      _:_ -> GAppProp (GStrIdent (GString s)) (gExps exps) ---- TODO: this causes "Gt holds for ..." etc

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
 Just expr -> fg expr
 _ -> case exp of
    EApp (EIdent f) x | f == identElem -> GElemKind (exp2kind x)
    EApp _ _ -> case splitApp exp of
     (fun, args) -> case fun of
        EIdent ident -> funListKind ident args
    EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level
      Just ("Noun", c) -> GNounKind (LexNoun c)
      _ -> ident2kind ident
    EFun _ _ -> case splitType exp of
      (hypos, body) ->
         GFunKind (GListArgKind (map hypo2coreArgKind hypos)) (exp2kind body)
    _ -> error $ "exp2kind not defined for " ++ show exp


exp2prop :: Exp -> GProp
exp2prop exp = case specialDedukti2Informath callBacks exp of
  Just expr -> fg expr
  _ -> case exp of
    EIdent ident -> GIdentProp (ident2ident ident)
    EApp (EIdent f) x | f == identProof -> GProofProp (exp2prop x)
    EApp _ _ -> case splitApp exp of
     (fun, args) -> case fun of
        EIdent ident -> funListProp ident (map exp2exp args)
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
  Just expr -> fg expr
  _ -> case exp of
    EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level 
      Just ("Name", c) -> GNameExp (LexName c)
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
        EIdent ident@(QIdent f) -> funListExp ident (map exp2exp args)
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
  Just expr -> fg expr
  _ -> case exp of
    EIdent ident -> GAppProof (GLabelProofExp (ident2label ident)) (GListProof []) 
    EApp _ _ -> case splitApp exp of
      (fun, args) ->
        GAppProof (exp2proofExp fun) (GListProof (map exp2proof args))
    EAbs _ _ -> case splitAbs exp of
      (binds, body) -> GAbsProof (GListHypo (map bind2coreHypo binds)) (exp2proof body)

exp2proofExp :: Exp -> GProofExp
exp2proofExp exp = case exp of
  EIdent ident -> GLabelProofExp (ident2label ident)
  EApp _ _ -> case splitApp exp of
    (fun, args) ->
      GAppProofExp (exp2proofExp fun) (gExps (map exp2exp args))
  EAbs _ _ -> case splitAbs exp of
    (binds, body) -> GAbsProofExp (GListHypo (map bind2coreHypo binds)) (exp2proofExp body)

patt2exp :: Patt -> GExp
patt2exp patt = case patt of
  PVar ident -> ident2exp ident
  PApp _ _ -> case splitPatt patt of
    (fun, args) -> case fun of
      PVar ident ->
        funListExp ident (map patt2exp args)
  PBracket p -> patt2exp p --- ?
  PBind bind p -> GAbsExp (GListIdent [bind2coreIdent bind]) (patt2exp p) --- splitAbs?

ident2ident :: QIdent -> GIdent
ident2ident ident = case ident of
  QIdent s -> GStrIdent (GString s)

ident2exp :: QIdent -> GExp
ident2exp ident = case ident of
  QIdent [d] | isDigit d -> GTermExp (GNumberTerm (GInt (read [d])))
  QIdent s -> case lookupConstant s of
    Just ("Name", c) -> GNameExp (LexName c)
    _ -> GTermExp (GIdentTerm (ident2ident ident))

ident2label :: QIdent -> GLabel
ident2label ident = case ident of
  QIdent s -> case lookupConstant s of
    Just ("Label", c) -> LexLabel c
    _ -> GIdentLabel (ident2ident ident)

ident2kind :: QIdent -> GKind
ident2kind ident = case ident of
  QIdent s -> case lookupConstant s of
    Just ("Noun", c) -> GNounKind (LexNoun c)
    _ -> GTermKind (GIdentTerm (ident2ident ident))

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

