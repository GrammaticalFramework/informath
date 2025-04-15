{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2Core where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import Informath -- superset of MathCore
import CommonConcepts
import DeduktiOperations

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
    in case (splitType typ, guessCat ident typ) of
      ((hypos, kind), c) | elem c ["Label"] -> 
        (maybe GAxiomJmt (\exp x y z -> GThmJmt x y z (exp2proof exp)) mexp)
          (ident2label ident)
          (GListHypo (hypos2hypos hypos))
          (exp2prop kind)
      ((hypos, kind), c) | elem c ["Noun", "Set"] -> 
          (maybe (GAxiomKindJmt axiomLabel)
               (\exp x y -> GDefKindJmt definitionLabel x y (exp2kind exp)) mexp)
            (GListHypo (hypos2hypos hypos))
            (ident2kind ident)
      ((hypos, kind), c) | elem c ["Name", "Const", "Unknown"] ->
          (maybe (GAxiomExpJmt axiomLabel)
	         (\exp x y z -> GDefExpJmt definitionLabel x y z (exp2exp exp)) mexp)
            (GListHypo (hypos2hypos hypos))
	    (ident2exp ident)
            (exp2kind kind)
      ((hypos, kind), c) | elem c ["Fun", "Oper"] ->
        let chypos = hypos2hypos (addVarsToHypos hypos)
        in (maybe (GAxiomExpJmt axiomLabel)
	          (\exp x y z -> GDefExpJmt definitionLabel x y z (exp2exp exp)) mexp)
             (GListHypo chypos)
             (funListExp ident (map (GTermExp . GTIdent) (concatMap hypoIdents chypos)))
             (exp2kind kind)
      ((hypos, kind), c) | elem c ["Reladj", "Compar", "Relverb", "Relnoun", "Comparnoun", "Pred3"] ->
        let chypos = hypos2hypos  (addVarsToHypos hypos)
        in (maybe (GAxiomPropJmt axiomLabel)
	        (\exp x y -> GDefPropJmt definitionLabel x y (exp2prop exp)) mexp)
             (GListHypo chypos)
	     (funListProp ident (map (GTermExp . GTIdent) (concatMap hypoIdents chypos)))
      ((hypos, kind), c) | elem c ["Adj", "Verb"] ->
        let chypos = hypos2hypos  (addVarsToHypos hypos)
        in (maybe (GAxiomPropJmt axiomLabel)
	        (\exp x y -> GDefPropJmt definitionLabel x y (exp2prop exp)) mexp)
             (GListHypo chypos)
	     (funListProp ident (map (GTermExp . GTIdent) (concatMap hypoIdents chypos)))
      ((hypos, kind), _) -> -- def of "Unknown" ident is interpreted as a theorem
        GAxiomExpJmt axiomLabel
          (GListHypo (hypos2hypos hypos)) (ident2exp ident)
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

funListExp :: QIdent -> [GExp] -> GExp
funListExp ident exps = case ident of
  QIdent s -> case lookupConstant s of
    Just ("Fun", c) ->
      GFunListExp (LexFun c) (gExps exps)
    Just ("Oper", c) ->
      GOperListExp (LexOper c) (gExps exps)
    _ -> case exps of
      [] -> ident2exp ident
      _:_ -> GAppExp (ident2exp ident) (gExps exps)

funListProp :: QIdent -> [GExp] -> GProp
funListProp ident exps = case ident of
  QIdent s -> case lookupConstant s of
    Just ("Adj", c) | length exps == 1 ->
      GAdjProp (LexAdj c) (exps !! 0)
    Just ("Verb", c) | length exps == 1 ->
      GVerbProp (LexVerb c) (exps !! 0)
    Just ("Pred3", c) | length exps == 3 ->
      GAdjProp (GPred3Adj (LexPred3 c) (exps !! 1) (exps !! 2)) (exps !! 0)
    Just ("Reladj", c) | length exps == 2 ->
      GAdjProp (GReladjAdj (LexReladj c) (exps !! 1)) (exps !! 0)
    Just ("Relverb", c) | length exps == 2 ->
      GRelverbProp (LexRelverb c) (exps !! 0) (exps !! 1)
    Just ("Relnoun", c) | length exps == 2 ->
      GRelnounProp (LexRelnoun c) (exps !! 0) (exps !! 1)
    Just ("Comparnoun", c) | length exps == 2 ->
      GComparnounProp (LexComparnoun c) (exps !! 0) (exps !! 1)
    Just ("Compar", c) | length exps == 2 ->
      GAdjProp (GComparAdj (LexCompar c) (exps !! 1)) (exps !! 0)
    _ -> case exps of
      [] -> GIdentProp (GStrIdent (GString s))
      _:_ -> GAppProp (GStrIdent (GString s)) (gExps exps)

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
  HExp prop : hh -> 
    GPropHypo (exp2prop prop) : hypos2hypos hh
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
    Just ("Set", c) -> GSetKind (LexSet c)
    _ -> ident2kind ident
  EApp (EIdent f) x | f == identElem -> GElemKind (exp2kind x)
  EApp _ _ -> case splitApp exp of
    (EIdent ident@(QIdent s), xs) -> case lookupConstant s of
      Just ("Fam", c) -> case xs of
        [x] -> GFamKind (LexFam c) (exp2kind x)
        [x, y] -> GFam2Kind (LexFam c) (exp2kind x) (exp2kind y)
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
      EIdent conn | conn == identConj -> case splitIdent conn exp of
        exps -> GAndProp (GListProp (map exp2prop exps))
      EIdent conn | conn == identDisj -> case splitIdent conn exp of
        exps -> GOrProp (GListProp (map exp2prop exps))
      EIdent conn | conn == identImpl -> case args of
        [a, b] -> GIfProp (exp2prop a) (exp2prop b) 
      EIdent conn | conn == identEquiv -> case args of
        [a, b] -> GIffProp (exp2prop a) (exp2prop b) 
      EIdent conn | conn == identSigma -> case args of
        [kind, EAbs bind prop] ->
          GExistProp
            (GListArgKind [hypo2coreArgKind (HVarExp (bind2var bind) kind)])
            (exp2prop prop)
      EIdent conn | conn == identPi -> case args of
        [kind, EAbs bind prop] ->
          GAllProp
            (GListArgKind [hypo2coreArgKind (HVarExp (bind2var bind) kind)])
            (exp2prop prop)
      EIdent conn | conn == identNeg -> case args of
        [a] -> case exp2prop a of
          GAdjProp adj x -> GNotAdjProp adj x
          GVerbProp verb x -> GNotVerbProp verb x
          GRelverbProp verb x y -> GNotRelverbProp verb x y
          GRelnounProp noun x y -> GNotRelnounProp noun x y
          p -> GNotProp p
      EIdent ident -> funListProp ident (map exp2exp args)
  EFun _ _ -> case splitType exp of
    (hypos, exp) ->
      GAllProp (GListArgKind (map hypo2coreArgKind hypos)) (exp2prop exp)
  EAbs _ _ -> case splitAbs exp of
    (binds, body) -> (exp2prop body) ---- TODO find way to express binds here

exp2exp :: Exp -> GExp
exp2exp exp = case exp of
  EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level
    Just ("Name", c) -> GNameExp (LexName c)
    Just ("Const", c) -> GConstExp (LexConst c)
    _ -> ident2exp ident
  EApp _ _ -> case splitApp exp of
    (fun, args) -> case fun of
----      EIdent (QIdent "enumset") enum ->
      EIdent (QIdent n) | elem n digitFuns -> case getNumber fun args of
        Just s -> GTermExp (GTNumber (GInt (read s)))
	_ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))
      EIdent ident@(QIdent f) -> case (f, args) of
        _ -> case (lookupConstant f, args) of
          (Just ("Fun", c), exps) -> GFunListExp (LexFun c) (gExps (map exp2exp exps))     
          (Just ("Oper", c), exps) -> GOperListExp (LexOper c) (gExps (map exp2exp exps))     
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
  QIdent s -> case lookupConstant s of
    Just ("Name", c) -> GNameExp (LexName c)
    Just ("Const", c) -> GConstExp (LexConst c)
    _ -> GTermExp (GTIdent (ident2ident ident))

ident2label :: QIdent -> GLabel
ident2label ident = case ident of
  QIdent s -> case lookupConstant s of
    Just ("Label", c) -> LexLabel c
    _ -> GStrLabel (GString s)

ident2kind :: QIdent -> GKind
ident2kind ident = case ident of
  QIdent s -> case lookupConstant s of
    Just ("Noun", c) -> GNounKind (LexNoun c)
    Just ("Set", c) -> GSetKind (LexSet c)
    _ -> GTermKind (GTIdent (ident2ident ident))

bind2coreIdent :: Bind -> GIdent
bind2coreIdent bind = case bind of
  BVar var -> ident2ident var
  BTyped var exp -> ident2ident var ---- add typed binding to Core?

-- needed in proofs by abstraction
bind2coreHypo :: Bind -> GHypo
bind2coreHypo bind = case bind of
  BTyped x exp | isWildIdent x ->
    GPropHypo (exp2prop exp)  
  BTyped var exp ->
    GVarsHypo (GListIdent [ident2ident var]) (exp2kind exp)  
  BVar var ->  
    GBareVarsHypo (GListIdent [ident2ident var])

gExps :: [GExp] -> GExps
gExps exps = foldr GAddExps (GOneExps (last exps)) (init exps)

