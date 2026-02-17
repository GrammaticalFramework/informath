{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module MathCore2Dedukti where

import Dedukti.AbsDedukti
import Informath -- superset of MathCore
import CommonConcepts
import DeduktiOperations
import BuildConstantTable 
import PGF (showExpr)

import Data.Char
import Data.List (intersperse)
import qualified Data.Map as M


jmt2dedukti :: BackConstantTable -> DropTable -> GJmt -> [Jmt]
jmt2dedukti lb dt =
  map eliminateLocalDefinitions .
  map (restoreFirstArguments dt) .
  applyLookBack lb .
  jmt2jmt

-- this is where the GF identifier ambiguity is resolved
applyLookBack ::  BackConstantTable -> Dedukti.AbsDedukti.Tree a -> [Dedukti.AbsDedukti.Tree a]
applyLookBack mb t = case t of
  QIdent s -> maybe [t] id $ M.lookup (QIdent (unescape s)) mb
  _ -> Dedukti.AbsDedukti.composOpM (applyLookBack mb) t  
 where
  unescape s = case s of
    '{':'|':cs -> "'\\" ++ init (init cs) ++ "'" -- for macros ; --- cumbersome
    _ -> s
    

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
  GUnitJmt unit ->
    JDef
      (QIdent "unit")
      MTNone
      (MEExp (unit2exp unit))

rule2dedukti :: GRule -> Rule
rule2dedukti rule = case rule of
  GRewriteRule (GListIdent idents) patt exp ->
    RRule (map (PBVar . ident2ident) idents) (exp2deduktiPatt patt) (exp2dedukti exp)
  GNoVarRewriteRule patt exp ->
    rule2dedukti (GRewriteRule (GListIdent []) patt exp)


unit2exp :: GUnit -> Exp
unit2exp unit = case unit of
  GBeginEnvironmentUnit (LexEnvironment s) label -> wrap "BeginEnvironmentUnit" [EIdent (QIdent s)]
  GBeginProofMethodUnit label method_ -> wrap "BeginProofMethodUnit" [EIdent (label2ident label)]
  GCaseGoal prop -> wrap "CaseGoal" [prop2dedukti prop]
  GCasesGoal -> uni "CasesGoal"
  GEndEnvironmentUnit (LexEnvironment s) -> wrap "EndAbbreviationUnit" [EIdent (QIdent s)] ;
  GEnoughGoal prop -> wrap "EnoughGoal" [prop2dedukti prop]
  GFirstVerifyGoal prop -> wrap "FirstVerifyGoal" [prop2dedukti prop]
  GFollowsPropConclusion prop -> wrap "FollowsPropConclusion" [prop2dedukti prop]
  GHyposAssumption (GListHypo hypos) -> wrap "HyposAssumptiop" [foldr EFun eUnit (concatMap hypo2dedukti hypos)]
  GIdentExpAssumption exp ident -> wrap "IdentExpAssumptiopn" [exp2dedukti exp, EIdent (ident2ident ident)]
  GIdentKindAssumption kind ident -> wrap "IdentKindAssumptiopn" [kind2dedukti kind, EIdent (ident2ident ident)]
  GInductionGoal -> uni "InductionGoal"
  GLabelConclusion label -> wrap "LabelConclution" [EIdent (label2ident label)]
  GObviousConclusion -> uni "ObviousConclusion"
  GPropAssumption prop -> wrap "PropAssumption" [prop2dedukti prop]
  GPropConclusion hence prop -> wrap "PropConclusion" [prop2dedukti prop]
  GPropLabelConclusion hence prop label -> wrap "PropLabelConclusion" [prop2dedukti prop, EIdent (label2ident label)]
  GSinceConclusion a b -> wrap "SinceConclusion" [prop2dedukti a, prop2dedukti b]
  GSinceGoal a b -> wrap "SinceGoal" [prop2dedukti a, prop2dedukti b]
  _ -> eUnit
 where
  wrap s xs = foldl EApp (EIdent (QIdent s)) xs
  uni s = wrap s []
  eUnit = wrap "UNIT" []

prop2dedukti :: GProp -> Exp
prop2dedukti prop = case prop of
  GProofProp p -> EApp (EIdent (QIdent "Proof")) (prop2dedukti p)
  GFalseProp -> propFalse
  GIdentProp ident -> EIdent (ident2ident ident)
  GCoreAndProp a b -> foldl1 propAnd (map prop2dedukti [a, b])
  GCoreOrProp a b -> foldl1 propOr (map prop2dedukti [a, b])
  GCoreIfProp a b -> propImp (prop2dedukti a) (prop2dedukti b)
  GCoreNotProp a -> propNeg (prop2dedukti a)
  GCoreIffProp a b -> propEquiv (prop2dedukti a) (prop2dedukti b)
  GCoreAllProp kind ident prop ->
    propPi (kind2dedukti kind) (EAbs (BVar (ident2ident ident)) (prop2dedukti prop)) 
  GCoreExistProp kind ident prop ->
    propSigma (kind2dedukti kind) (EAbs (BVar (ident2ident ident)) (prop2dedukti prop)) 
  GAppProp ident exps ->
    foldl1 EApp ((EIdent (ident2ident ident)) : map exp2dedukti (exps2list exps))
  GAdj3Prop (LexAdj3 rel) a b c ->
    foldl EApp (EIdent (QIdent (rel))) (map exp2dedukti [a, b, c])
    
  GAdj2Prop (LexAdj2 rel) a b ->
    foldl EApp (EIdent (QIdent (rel))) (map exp2dedukti [a, b])
  GAdj2Prop (GAdjPrepAdj2 (LexAdj rel) (LexPrep prep)) a b ->
    foldl EApp (EIdent (funPrepQIdent (rel, [prep]))) (map exp2dedukti [a, b])

  GAdjCProp (LexAdjC rel) a b ->
    foldl EApp (EIdent (QIdent (rel))) (map exp2dedukti [a, b])
  GAdjCProp (GAdjAdjC (LexAdj rel)) a b ->
    foldl EApp (EIdent (funPrepQIdent (rel, ["C"]))) (map exp2dedukti [a, b])
    
  GAdjEProp (LexAdjE rel) a b ->
    foldl EApp (EIdent (QIdent (rel))) (map exp2dedukti [a, b])
  GAdjCProp (GAdjAdjE (LexAdj rel)) a b ->
    foldl EApp (EIdent (funPrepQIdent (rel, ["E"]))) (map exp2dedukti [a, b])
    
  GAdjProp (LexAdj adj) exp ->
    EApp (EIdent (QIdent (adj))) (exp2dedukti exp)
  GVerbProp (LexVerb verb) exp ->
    EApp (EIdent (QIdent (verb))) (exp2dedukti exp)
  GVerb2Prop (LexVerb2 verb) x y ->
    EApp (EApp (EIdent (QIdent (verb))) (exp2dedukti x)) (exp2dedukti y)
  GNoun2Prop (LexNoun2 noun) x y ->
    EApp (EApp (EIdent (QIdent (noun))) (exp2dedukti x)) (exp2dedukti y)
  GIndexedFormulaProp (GInt i) -> EIdent (unresolvedIndexIdent i)
  GFormulaProp formula -> formula2dedukti formula
  _ -> eUndefinedDebug prop ---- TODO complete Informath2Core

formula2dedukti :: GFormula -> Exp
formula2dedukti formula = case formula of
----  GElemFormula terms term ->
  GEquationFormula (GBinaryEquation (LexCompar compar) term1 term2) ->
    foldl EApp (EIdent (QIdent compar)) (map term2dedukti [term1, term2])
  ---- modulo_Formula : Term -> Term -> Term -> Formula
  GMacroFormula ident terms -> foldl EApp (EIdent (ident2ident ident)) (map term2dedukti (termsList terms)) 
  _ -> eUndefinedDebug formula ----

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
  _ ->
    [HExp eUndefined]

local2dedukti :: GLocal -> Hypo
local2dedukti local = case local of
  GLetLocal ident kind exp ->
    HLetTyped (ident2ident ident) (kind2dedukti kind) (exp2dedukti exp)
  GBareLetLocal ident exp ->
    HLetExp (ident2ident ident) (exp2dedukti exp)

argkind2dedukti :: GArgKind -> [(Exp, QIdent)]
argkind2dedukti argkind = case argkind of
  GIdentArgKind kind ident ->
    let dkind = kind2dedukti kind
    in [(dkind, ident2ident ident)]
  GIdentsArgKind kind (GListIdent idents) ->
    let dkind = kind2dedukti kind
    in [(dkind, ident2ident ident) | ident <- idents]
  GIndexedDeclarationArgKind (GInt i) ->
    [(EIdent (unresolvedIndexIdent i), unresolvedIndexIdent i)]

kind2dedukti :: GKind -> Exp
kind2dedukti kind = case kind of
  GElemKind k -> EApp (EIdent (QIdent "Elem")) (kind2dedukti k)
  GTermKind (GIdentTerm ident) -> EIdent (ident2ident ident)
  {- ----
  GSuchThatKind kind ident prop ->
    propSigma
      (kind2dedukti kind)
      (EAbs (BVar (ident2ident ident))
            (prop2dedukti prop))
	    -}
  GFamKind (LexFam fam) exp ->
    EApp (EIdent (QIdent fam)) (kind2dedukti exp)
  GFam2Kind (LexFam fam) exp1 exp2 ->
    EApp (EApp (EIdent (QIdent fam)) (kind2dedukti exp1)) (kind2dedukti exp2)
  GAppKind ident exps ->
    foldl1 EApp (EIdent (ident2ident ident) : map exp2dedukti (exps2list exps))
  GNounKind (LexNoun noun) ->
    EIdent (QIdent (noun))
  _ -> eUndefinedDebug kind ---- TODO

exp2dedukti :: GExp -> Exp
exp2dedukti exp = case exp of
  GAppExp exp exps ->
    foldl1 EApp (map exp2dedukti (exp : (exps2list exps)))
  GAbsExp (GListIdent idents) exp ->
    foldr
      (\x y -> EAbs (BVar (ident2ident x)) y)
      (exp2dedukti exp)
      idents
  GNameExp (LexName name) ->
    EIdent (QIdent (name))
  GTermExp term -> term2dedukti term
  GFunExp (LexFun f) x -> appIdent f (map exp2dedukti [x])

  GFunExp (GNounPrepFun (LexNoun noun) (LexPrep prep)) x -> appQIdent (funPrepQIdent (noun, [prep])) (map exp2dedukti [x])


  GFun2Exp (LexFun2 f) x y -> appIdent f (map exp2dedukti [x, y])
  GFunCExp (LexFunC f) x y -> appIdent f (map exp2dedukti [x, y])
  GIndexedTermExp (GInt i) -> EIdent (unresolvedIndexIdent i)
  GEnumSetExp exps -> EApp (EIdent (QIdent "enumset")) (list2enum (map exp2dedukti (exps2list exps)))
  GSigmaExp m n i f ->
    EApp (EApp (EApp (EIdent (QIdent "sigma")) (exp2dedukti m)) (exp2dedukti n)) (EAbs (BVar (ident2ident i)) (exp2dedukti f)) ---- TODO: in SpecialConstants
  _ -> eUndefinedDebug exp ---- TODO

term2dedukti :: GTerm -> Exp
term2dedukti term = case term of
  GConstTerm (LexConst name) ->
    EIdent (QIdent (name))
  GOperTerm (LexOper oper) x ->
    appIdent oper [term2dedukti x]
  GOper2Term (LexOper2 oper) x y ->
    appIdent oper (map term2dedukti [x, y])
  GNumberTerm (GInt n) -> int2exp n
  GIdentTerm ident -> EIdent (ident2ident ident)
  GMacroTerm ident terms -> foldl EApp (EIdent (ident2ident ident)) (map term2dedukti (termsList terms)) 
  _ -> eUndefinedDebug term ---- TODO

termsList :: GTerms -> [GTerm]
termsList terms = case terms of
  GAddTerms t tt -> t : termsList terms
  GOneTerms t -> [t]

exp2deduktiPatt :: GExp -> Patt
exp2deduktiPatt exp = case exp of
  GTermExp (GIdentTerm ident) -> PVar (ident2ident ident)
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
  GTermExp (GIdentTerm ident) -> ident2ident ident
  _ -> QIdent (takeWhile isAlpha (show (gf exp))) ---- TODO

label2ident :: GLabel -> QIdent
label2ident label = case label of
  LexLabel s -> QIdent (s)
  GIdentLabel ident -> ident2ident ident
  GcrefLabel ident -> ident2ident ident

kind2ident :: GKind -> QIdent
kind2ident kind = case kind of
  GTermKind (GIdentTerm ident) -> ident2ident ident
  _ -> QIdent (takeWhile isAlpha (show (gf kind))) ---- TODO

prop2deduktiIdent :: GProp -> QIdent
prop2deduktiIdent prop = case prop of
  GIdentProp (GStrIdent (GString s)) -> QIdent s
  _ -> QIdent (takeWhile isAlpha (show (gf prop))) ---- TODO

eUndefined :: Exp
eUndefined = EIdent (QIdent "UNDEFINED")

eUndefinedDebug :: Gf a => a -> Exp
eUndefinedDebug t =
  EIdent (QIdent (concat (intersperse "_" ("{|" : "UNDEFINED" : words (showExpr [] (gf t)))) ++ "|}"))

appIdent :: String -> [Exp] -> Exp
appIdent f exps = foldl EApp (EIdent (QIdent f)) exps

appQIdent :: QIdent -> [Exp] -> Exp
appQIdent f exps = foldl EApp (EIdent f) exps


--- also in MCI
exps2list :: GExps -> [GExp]
exps2list exps = case exps of
  GOneExps e -> [e]
  GManyExps (GListExp es) -> es

