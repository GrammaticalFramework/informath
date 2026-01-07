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
jmt2core = cleanup . jmt2jmt where
  cleanup :: Informath.Tree a -> Informath.Tree a
  cleanup t = case t of
    GStrIdent (GString s) -> GStrIdent (GString (unescapeConstant (stripConstant s)))
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
      ((hypos, kind), c) | elem c ["Label"] -> 
        (maybe GAxiomJmt (\exp x y z -> GThmJmt x y z (exp2proof exp)) mexp)
          (ident2label ident)
          (GListHypo (hypos2hypos hypos))
          (exp2prop kind)
      ((hypos, kind), c) | elem c ["Noun"] -> 
          (maybe (GAxiomKindJmt axiomLabel)
               (\exp x y -> GDefKindJmt definitionLabel x y (exp2kind exp)) mexp)
            (GListHypo (hypos2hypos hypos))
            (ident2kind ident)
      ((hypos, kind), c) | elem c ["Fam", "Fam2"] ->
        let chypos = hypos2hypos (addVarsToHypos mexp hypos)
        in (maybe (GAxiomKindJmt axiomLabel)
	        (\exp x y -> GDefKindJmt definitionLabel x y (exp2kind exp)) mexp)
             (GListHypo chypos)
	     (funListKind ident (map id (concatMap hypoIdents chypos)))
      ((hypos, kind), c) | elem c ["Name", "Unknown"] ->
          (maybe (GAxiomExpJmt axiomLabel)
	         (\exp x y z -> GDefExpJmt definitionLabel x y z (exp2exp exp)) mexp)
            (GListHypo (hypos2hypos hypos))
	    (ident2exp ident)
            (exp2kind kind)
      ((hypos, kind), c) | elem c ["Fun", "Fun2", "FunC"] ->
        let chypos = hypos2hypos (addVarsToHypos mexp hypos)
        in (maybe (GAxiomExpJmt axiomLabel)
	          (\exp x y z -> GDefExpJmt definitionLabel x y z (exp2exp (stripAbs hypos exp))) mexp)
             (GListHypo chypos)
             (funListExp ident (map (GTermExp . GIdentTerm) (concatMap hypoIdents chypos)))
             (exp2kind kind)
      ((hypos, kind), c) | elem c ["Adj", "Verb", "Noun1", "Adj2", "AdjC", "AdjE", "Verb2", "Noun2", "Adj3"] ->
        let chypos = hypos2hypos  (addVarsToHypos mexp hypos)
        in (maybe (GAxiomPropJmt axiomLabel)
	        (\exp x y -> GDefPropJmt definitionLabel x y (exp2prop exp)) mexp)
             (GListHypo chypos)
	     (funListProp ident (map (GTermExp . GIdentTerm) (concatMap hypoIdents chypos)))
      ((hypos, kind), c) -> ----error ("cannot convert category " ++ c)      
        let chypos = hypos2hypos (addVarsToHypos mexp hypos)
        in (maybe (GAxiomExpJmt axiomLabel)
	          (\exp x y z -> GDefExpJmt definitionLabel x y z (exp2exp (stripAbs hypos exp))) mexp)
             (GListHypo chypos)
             (funListExp ident (map (GTermExp . GIdentTerm) (concatMap hypoIdents chypos)))
             (exp2kind kind)

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
axiomUndefLabel ident = GStrLabel (GString ("Undefined_" ++ show ident))

funListExp :: QIdent -> [GExp] -> GExp
funListExp ident exps = case ident of
  QIdent s -> case (lookupConstant s, exps) of
    (Just ("Name", c), []) -> GNameExp (LexName c)
    (Just ("Fun", c), [x]) -> GFunExp (LexFun c) x
    (Just ("Fun2", c), [x, y]) -> GFun2Exp (LexFun2 c) x y
    (Just ("FunC", c), [x, y]) -> GFunCExp (LexFunC c) x y
    _ -> case exps of
      [] -> ident2exp ident
      _:_ -> GAppExp (ident2exp ident) (gExps exps)

funListKind :: QIdent -> [GIdent] -> GKind
funListKind ident exps = case ident of
  QIdent s -> case (lookupConstant s, map (GTermKind . GIdentTerm) exps) of
    (Just ("Noun", c), []) -> GNounKind (LexNoun c)
    (Just ("Fam",  c), [x]) -> GFamKind (LexFam c) x
    (Just ("Fam2", c), [x, y]) -> GFam2Kind (LexFam2 c) x y
    _ -> case exps of
      [] -> ident2kind ident
      _:_ -> GAppKind (ident2ident ident) (gExps (map (GTermExp . GIdentTerm) exps))

funListProp :: QIdent -> [GExp] -> GProp
funListProp ident exps = case ident of
  QIdent s -> case lookupConstant s of
    Just ("Adj", c) | length exps == 1 ->
      GAdjProp (LexAdj c) (exps !! 0)
    Just ("Adj2", c) | length exps == 2 ->
      GAdj2Prop (LexAdj2 c) (exps !! 0) (exps !! 1)
    Just ("AdjC", c) | length exps == 2 ->
      GAdjCProp (LexAdjC c) (exps !! 0) (exps !! 1)
    Just ("AdjE", c) | length exps == 2 ->
      GAdjEProp (LexAdjE c) (exps !! 0) (exps !! 1)
    Just ("Adj3", c) | length exps == 3 ->
      GAdj3Prop (LexAdj3 c) (exps !! 0) (exps !! 1) (exps !! 2)
    Just ("Verb", c) | length exps == 1 ->
      GVerbProp (LexVerb c) (exps !! 0)
    Just ("Verb2", c) | length exps == 2 ->
      GVerb2Prop (LexVerb2 c) (exps !! 0) (exps !! 1)
    Just ("Noun1", c) | length exps == 1 ->
      GNoun1Prop (LexNoun1 c) (exps !! 0)
    Just ("Noun2", c) | length exps == 2 ->
      GNoun2Prop (LexNoun2 c) (exps !! 0) (exps !! 1)
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
exp2kind exp = case exp of
  EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level
    Just ("Noun", c) -> GNounKind (LexNoun c)
    _ -> ident2kind ident
  EApp (EIdent f) x | f == identElem -> GElemKind (exp2kind x)
  EApp _ _ -> case splitApp exp of
    (EIdent ident@(QIdent s), xs) -> case lookupConstant s of
      Just ("Fam", c) -> case xs of
        [x] -> GFamKind (LexFam c) (exp2kind x)
      Just ("Fam2", c) -> case xs of
        [x, y] -> GFam2Kind (LexFam2 c) (exp2kind x) (exp2kind y)
	_ -> GAppKind (ident2ident ident) (gExps (map exp2exp xs))
      _ -> GAppKind (ident2ident ident) (gExps (map exp2exp xs))
    (fun, args) -> case fun of
      EIdent ident ->
        GAppKind (ident2ident ident) (gExps (map exp2exp args))
  EFun _ _ -> 
    case splitType exp of
      (hypos, valexp) ->
        GFunKind (GListArgKind (map hypo2coreArgKind hypos)) (exp2kind valexp)
  _ -> error $ "exp2kind not defined for " ++ show exp

exp2prop :: Exp -> GProp
exp2prop exp = case exp of
  _ | exp == propFalse -> GFalseProp
  EIdent ident -> GIdentProp (ident2ident ident)
  EApp (EIdent f) x | f == identProof -> GProofProp (exp2prop x)
  EApp _ _ -> case splitApp exp of
    (fun, args) -> case fun of
      EIdent conn | conn == identConj -> case args of
        [a, b] -> GCoreAndProp (exp2prop a) (exp2prop b) 
      EIdent conn | conn == identDisj -> case args of
        [a, b] -> GCoreOrProp (exp2prop a) (exp2prop b) 
      EIdent conn | conn == identImpl -> case args of
        [a, b] -> GCoreIfProp (exp2prop a) (exp2prop b) 
      EIdent conn | conn == identEquiv -> case args of
        [a, b] -> GCoreIffProp (exp2prop a) (exp2prop b) 
      EIdent conn | conn == identSigma -> case args of ---- TODO: add cases with eta expansion
        [kind, EAbs bind prop] ->
          GCoreExistProp (exp2kind kind) (ident2ident (bind2var bind)) (exp2prop prop)
      EIdent conn | conn == identPi -> case args of
        [kind, EAbs bind prop] ->
          GCoreAllProp (exp2kind kind) (ident2ident (bind2var bind)) (exp2prop prop)
      EIdent conn | conn == identNeg -> case args of
        [a] -> GCoreNotProp (exp2prop a)
      EIdent ident -> funListProp ident (map exp2exp args)
  EFun _ _ -> case splitType exp of
    (hypos, exp) ->
      GAllProp (GListArgKind (map hypo2coreArgKind hypos)) (exp2prop exp)
  EAbs _ _ -> case splitAbs exp of
    (binds, body) -> (exp2prop body) ---- TODO find way to express binds here

exp2exp :: Exp -> GExp
exp2exp exp = case specialDedukti2Informath bind2coreIdent exp2exp exp of
  Just gexp -> gexp
  _ -> case exp of
    EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level
      Just ("Name", c) -> GNameExp (LexName c)
      _ -> ident2exp ident
  
    EApp _ _ -> case splitApp exp of
      (fun, args) -> case fun of
   {-
      EIdent (QIdent "sigma") | length args == 3 ->
        let [m, n, EAbs b f] = args
        in GSigmaExp (bind2coreIdent b) (exp2exp m) (exp2exp n) (exp2exp f)  
      EIdent (QIdent "series") | length args == 2 ->
        let [m, EAbs b f] = args
        in GSeriesExp (bind2coreIdent b) (exp2exp m) (exp2exp f)  
      EIdent (QIdent "integral") | length args == 3 ->
        let [m, n, EAbs b f] = args
        in GIntegralExp (bind2coreIdent b) (exp2exp m) (exp2exp n) (exp2exp f)  
      EIdent (QIdent "enumset") | length args == 1 -> case enum2list (head args) of
        Just exps@(_:_) -> GEnumSetExp (gExps (map exp2exp exps))
	Just [] -> GNameExp (LexName "emptyset_Name")
	_ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))
   -}
        EIdent (QIdent n) | elem n digitFuns -> case getNumber fun args of
          Just s -> GTermExp (GNumberTerm (GInt (read s)))
	  _ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))
        EIdent ident@(QIdent f) -> case (f, args) of
          _ -> case (lookupConstant f, args) of
            (Just ("Fun", c), [exp]) -> GFunExp (LexFun c) (exp2exp exp)     
            (Just ("Fun2", c), [x, y]) -> GFun2Exp (LexFun2 c) (exp2exp x) (exp2exp y)     
            (Just ("FunC", c), [x, y]) -> GFunCExp (LexFunC c) (exp2exp x) (exp2exp y)     
            _ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))
        _ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))
      
    EAbs _ _ -> case splitAbs exp of
      (binds, body) -> GAbsExp (GListIdent (map bind2coreIdent binds)) (exp2exp body)
    EFun _ _ -> 
      case splitType exp of
        (hypos, valexp) ->
          GKindExp (GFunKind (GListArgKind (map hypo2coreArgKind hypos)) (exp2kind valexp))
    _ -> error ("not yet exp2exp: " ++ printTree exp)


exp2proof :: Exp -> GProof
exp2proof exp = case exp of
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
    _ -> GStrLabel (GString s)

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

