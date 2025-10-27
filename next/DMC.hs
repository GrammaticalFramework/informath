{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module DMC where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import NextInformath -- superset of MathCore
import CommonConcepts
import DeduktiOperations
import BuildConstantTable

import Data.Char
import Data.List(isPrefixOf)

import qualified Data.Map as M

-- clean-up of remaining annotated idents
jmt2core :: Jmt -> GJmt
jmt2core = cleanup . jmt2jmt where
  cleanup :: NextInformath.Tree a -> NextInformath.Tree a
  cleanup t = case t of
    GStrIdent (GString s) -> GStrIdent (GString (unescapeConstant (stripConstant s)))
    _ -> NextInformath.composOp cleanup t

jmt2jmt :: Jmt -> GJmt
jmt2jmt jmt = case jmt of
  JDef ident MTNone (MEExp exp) ->
    GDefUntypedExpJmt (LexLabel "definitionLabel") (ident2exp ident) (exp2exp exp)
  JDef ident (MTExp typ) meexp ->
    let mexp = case meexp of
          MEExp exp -> Just exp
          _ -> Nothing
	(hypos, kind) = splitType typ
	chypos = hypos2hypos (addVarsToHypos mexp hypos)
    in case ((hypos, kind), guessGFCat ident typ) of
    
    -- verbal constants
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
      ((hypos, kind), c) | elem c ["Name", "Fun", "Fun2"] ->
        (maybe (GAxiomExpJmt axiomLabel)
	          (\exp x y z -> GDefExpJmt definitionLabel x y z (exp2exp (stripAbs hypos exp))) mexp)
             (GListHypo chypos)
             (funListExp ident (map (GTermExp . GIdentTerm) (concatMap hypoIdents chypos)))
             (exp2kind kind)
      ((hypos, kind), c) | elem c ["Adj", "Verb", "Noun1", "Adj2", "Verb2", "Noun2", "Adj3"] ->
        (maybe (GAxiomPropJmt axiomLabel)
	        (\exp x y -> GDefPropJmt definitionLabel x y (exp2prop exp)) mexp)
             (GListHypo chypos)
	     (funListProp ident (map (GTermExp . GIdentTerm) (concatMap hypoIdents chypos)))
	     
    -- symbolic constants
      ((hypos, kind), c) | elem c ["Const", "Oper", "Oper2"] ->
        (maybe (GAxiomExpJmt axiomLabel)
	          (\exp x y z -> GDefExpJmt definitionLabel x y z (exp2exp (stripAbs hypos exp))) mexp)
             (GListHypo chypos)
             (GTermExp (funListTerm ident (map GIdentTerm (concatMap hypoIdents chypos))))
             (exp2kind kind)
      ((hypos, kind), c) | elem c ["Compar"] ->
        (maybe (GAxiomPropJmt axiomLabel)
	        (\exp x y -> GDefPropJmt definitionLabel x y (exp2prop exp)) mexp)
             (GListHypo chypos)
	     (GFormulaProp (funListFormula ident (map GIdentTerm (concatMap hypoIdents chypos))))
	     
    -- "Unknown" constants not in symbol tables, treated as Exp
      ((hypos, kind), _) -> 
        (maybe (GAxiomExpJmt axiomLabel)
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

funListExp :: QIdent -> [GExp] -> GExp
funListExp ident exps = case ident of
  QIdent s -> case (lookupConstant s, exps) of
    (Just ("Name", c), []) -> GNameExp (LexName c)
    (Just ("Fun", c), [x]) -> GFunExp (LexFun c) x
    (Just ("Fun2", c), [x, y]) -> GFun2Exp (LexFun2 c) x y
    _ -> case exps of
      [] -> ident2exp ident
      _:_ -> GAppExp (ident2exp ident) (gExps exps)

funListTerm :: QIdent -> [GTerm] -> GTerm
funListTerm ident exps = case ident of
  QIdent s -> case (lookupConstant s, exps) of
    (Just ("Const", c), []) -> GConstTerm (LexConst c)
    (Just ("Oper", c), [x]) -> GOperTerm (LexOper c) x
    (Just ("Oper2", c), [x, y]) -> GOper2Term (LexOper2 c) x y
    _ -> case exps of
      [] -> GIdentTerm (ident2ident ident)
      _:_ -> GAppFunctionTerm (GIdentFunction (ident2ident ident)) (GListTerm exps)

funListProp :: QIdent -> [GExp] -> GProp
funListProp ident exps = case ident of
  QIdent s -> case lookupConstant s of
    Just ("Adj", c) | length exps == 1 ->
      GAdjProp (LexAdj c) (exps !! 0)
    Just ("Adj2", c) | length exps == 2 ->
      GAdj2Prop (LexAdj2 c) (exps !! 0) (exps !! 1)
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
      _:_ -> GAppProp (GStrIdent (GString s)) (gExps exps)

funListFormula :: QIdent -> [GTerm] -> GFormula
funListFormula ident terms = case ident of
  QIdent s -> case (lookupConstant s, terms) of
    (Just ("Compar", c), [x, y]) ->
      GEquationFormula (GBinaryEquation (LexCompar c) x y)
  _ -> error ("cannot get formula from " ++ show ident) 


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
      EIdent conn | conn == identSigma -> case args of
        [kind, EAbs bind prop] ->
          GCoreExistProp (ident2ident (bind2var bind)) (exp2kind kind) (exp2prop prop)
      EIdent conn | conn == identPi -> case args of
        [kind, EAbs bind prop] ->
          GCoreAllProp (ident2ident (bind2var bind)) (exp2kind kind) (exp2prop prop)
      EIdent conn | conn == identNeg -> case args of
        [a] -> case exp2prop a of
          GAdjProp adj x -> GNotAdjProp adj x
          GVerbProp verb x -> GNotVerbProp verb x
          GVerb2Prop verb x y -> GNotVerb2Prop verb x y
          GNoun2Prop noun x y -> GNotNoun2Prop noun x y
          p -> GNotProp p
      EIdent ident@(QIdent f) -> case exp2formula exp of
	 Just formula -> GFormulaProp formula
	 _ -> funListProp ident (map exp2exp args)
----      _ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))
  EFun _ _ -> case splitType exp of
    (hypos, exp) ->
      GAllProp (GListArgKind (map hypo2coreArgKind hypos)) (exp2prop exp)
  EAbs _ _ -> case splitAbs exp of
    (binds, body) -> (exp2prop body) ---- TODO find way to express binds here

exp2formula :: Exp -> Maybe GFormula
exp2formula exp = case splitApp exp of
  (fun@(EIdent (ident@(QIdent s))), args) -> do
    terms <- mapM exp2term args
    let (cat, c) = maybe ("Unknown", s) id (lookupConstant s)
    case (cat, terms) of
      ("Compar", [x, y]) -> return $ GEquationFormula (GBinaryEquation (LexCompar c) x y)
----      (_, []) -> return $ GIdentTerm (ident2ident ident)
      _ -> Nothing
  _ -> Nothing


exp2exp :: Exp -> GExp
exp2exp exp = case exp of
  EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level
    Just ("Name", c) -> GNameExp (LexName c)
    _ -> ident2exp ident
    
  EApp _ _ -> case splitApp exp of
    (fun, args) -> case fun of

      --- special constants. ---- TODO make this declarative
      EIdent (QIdent s) | isPrefixOf "sigma&" s &&length args == 3 ->
        let [m, n, EAbs b f] = args
        in GSigmaExp (bind2coreIdent b) (exp2exp m) (exp2exp n) (exp2exp f)  
      EIdent (QIdent s) | isPrefixOf "series&" s && length args == 2 ->
        let [m, EAbs b f] = args
        in GSeriesExp (bind2coreIdent b) (exp2exp m) (exp2exp f)  
      EIdent (QIdent s) | isPrefixOf "integral" s && length args == 3 ->
        let [m, n, EAbs b f] = args
        in GIntegralExp (bind2coreIdent b) (exp2exp m) (exp2exp n) (exp2exp f)  
      EIdent (QIdent "enumset") | length args == 1 -> case enum2list (head args) of
        Just exps@(_:_) -> GEnumSetExp (gExps (map exp2exp exps))
	Just [] -> GTermExp (GConstTerm (LexConst "emptyset_Const"))
	_ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))
    
      EIdent ident@(QIdent f) -> case lookupConstant f of
	Just (cat, c) | elem cat ["Name", "Fun", "Fun2"] -> funListExp ident (map exp2exp args)
	_ -> case exp2term exp of
	  Just term -> GTermExp term
	  _ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))
      _ -> GAppExp (exp2exp fun) (gExps (map exp2exp args))
      
  EAbs _ _ -> case splitAbs exp of
    (binds, body) -> GAbsExp (GListIdent (map bind2coreIdent binds)) (exp2exp body)
  EFun _ _ -> 
    case splitType exp of
      (hypos, valexp) ->
        GKindExp (GFunKind (GListArgKind (map hypo2coreArgKind hypos)) (exp2kind valexp))
  _ -> error ("not yet exp2exp: " ++ printTree exp)


exp2term :: Exp -> Maybe GTerm
exp2term exp = case splitApp exp of
  (fun@(EIdent (ident@(QIdent s))), args) -> case getNumber fun args of
    Just s -> return (GNumberTerm (GInt (read s)))
    _ -> case fun of
      EIdent (QIdent s) | isPrefixOf "sigma&" s && length args == 3 -> do
        let [m, n, EAbs b f] = args
        tm <- exp2term m
        tn <- exp2term n
        tf <- exp2term f
        return $ Gsigma_Term (bind2coreIdent b) tm tn tf

      _ -> do
        terms <- mapM exp2term args
        let (cat, c) = maybe ("Unknown", s) id (lookupConstant s)
        case (cat, terms) of
          ("Const", []) -> return $ GConstTerm (LexConst c)
          ("Oper", [x]) -> return $ GOperTerm (LexOper c) x
          ("Oper2", [x, y]) -> return $ GOper2Term (LexOper2 c) x y
          (_, []) -> return $ GIdentTerm (ident2ident ident)
          _ -> Nothing
  _ -> Nothing


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

gExps :: [GExp] -> GExps
gExps exps = foldr GAddExps (GOneExps (last exps)) (init exps)

