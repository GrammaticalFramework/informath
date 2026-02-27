{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module Informath where

import Control.Monad.Identity
import Control.Monad
import Data.Monoid
import PGF hiding (Tree)

----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

type GAdj = Tree GAdj_
data GAdj_
type GAdj2 = Tree GAdj2_
data GAdj2_
type GAdj3 = Tree GAdj3_
data GAdj3_
type GAdjC = Tree GAdjC_
data GAdjC_
type GAdjE = Tree GAdjE_
data GAdjE_
type GAdverb = Tree GAdverb_
data GAdverb_
type GArgKind = Tree GArgKind_
data GArgKind_
type GArgument = Tree GArgument_
data GArgument_
type GCompar = Tree GCompar_
data GCompar_
type GConst = Tree GConst_
data GConst_
type GDeclaration = Tree GDeclaration_
data GDeclaration_
type GEnvironment = Tree GEnvironment_
data GEnvironment_
type GEquation = Tree GEquation_
data GEquation_
type GExample = Tree GExample_
data GExample_
type GExp = Tree GExp_
data GExp_
type GExps = Tree GExps_
data GExps_
type GFam = Tree GFam_
data GFam_
type GFam2 = Tree GFam2_
data GFam2_
type GFilename = Tree GFilename_
data GFilename_
type GFormula = Tree GFormula_
data GFormula_
type GFun = Tree GFun_
data GFun_
type GFun2 = Tree GFun2_
data GFun2_
type GFunC = Tree GFunC_
data GFunC_
type GFunction = Tree GFunction_
data GFunction_
type GHence = Tree GHence_
data GHence_
type GHypo = Tree GHypo_
data GHypo_
type GIdent = Tree GIdent_
data GIdent_
type GJmt = Tree GJmt_
data GJmt_
type GKind = Tree GKind_
data GKind_
type GKindArgument = Tree GKindArgument_
data GKindArgument_
type GLabel = Tree GLabel_
data GLabel_
type GListAdj = Tree GListAdj_
data GListAdj_
type GListArgKind = Tree GListArgKind_
data GListArgKind_
type GListExp = Tree GListExp_
data GListExp_
type GListHypo = Tree GListHypo_
data GListHypo_
type GListIdent = Tree GListIdent_
data GListIdent_
type GListProof = Tree GListProof_
data GListProof_
type GListProp = Tree GListProp_
data GListProp_
type GListRule = Tree GListRule_
data GListRule_
type GListTerm = Tree GListTerm_
data GListTerm_
type GListUnit = Tree GListUnit_
data GListUnit_
type GLocal = Tree GLocal_
data GLocal_
type GMacro = Tree GMacro_
data GMacro_
type GMethod = Tree GMethod_
data GMethod_
type GName = Tree GName_
data GName_
type GNoun = Tree GNoun_
data GNoun_
type GNoun1 = Tree GNoun1_
data GNoun1_
type GNoun2 = Tree GNoun2_
data GNoun2_
type GNounC = Tree GNounC_
data GNounC_
type GOper = Tree GOper_
data GOper_
type GOper2 = Tree GOper2_
data GOper2_
type GPrep = Tree GPrep_
data GPrep_
type GProof = Tree GProof_
data GProof_
type GProofExp = Tree GProofExp_
data GProofExp_
type GProp = Tree GProp_
data GProp_
type GProperName = Tree GProperName_
data GProperName_
type GRule = Tree GRule_
data GRule_
type GTerm = Tree GTerm_
data GTerm_
type GTitle = Tree GTitle_
data GTitle_
type GUnit = Tree GUnit_
data GUnit_
type GVerb = Tree GVerb_
data GVerb_
type GVerb2 = Tree GVerb2_
data GVerb2_
type GVerbC = Tree GVerbC_
data GVerbC_
type GCoercion = Tree GCoercion_
data GCoercion_
type GTerms = Tree GTerms_
data GTerms_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GAdj2Adj :: GAdj2 -> GExp -> Tree GAdj_
  GAdj3Adj :: GAdj3 -> GExp -> GExp -> Tree GAdj_
  GAdverbAdjAdj :: GAdverb -> GAdj -> Tree GAdj_
  GAndAdj :: GListAdj -> Tree GAdj_
  GBothAndAdj :: GAdj -> GAdj -> Tree GAdj_
  GEitherOrAdj :: GAdj -> GAdj -> Tree GAdj_
  GOrAdj :: GListAdj -> Tree GAdj_
  LexAdj :: String -> Tree GAdj_
  GAdjPrepAdj2 :: GAdj -> GPrep -> Tree GAdj2_
  LexAdj2 :: String -> Tree GAdj2_
  GAdjPrepAdj3 :: GAdj -> GPrep -> GPrep -> Tree GAdj3_
  LexAdj3 :: String -> Tree GAdj3_
  GAdjAdjC :: GAdj -> Tree GAdjC_
  LexAdjC :: String -> Tree GAdjC_
  GAdjAdjE :: GAdj -> Tree GAdjE_
  LexAdjE :: String -> Tree GAdjE_
  Galmost_everywhere_Adverb :: Tree GAdverb_
  Geverywhere_Adverb :: Tree GAdverb_
  Guniformly_Adverb :: Tree GAdverb_
  GBareIdentsArgKind :: GListIdent -> Tree GArgKind_
  GDeclarationArgKind :: GDeclaration -> Tree GArgKind_
  GIdentArgKind :: GKind -> GIdent -> Tree GArgKind_
  GIdentsArgKind :: GKind -> GListIdent -> Tree GArgKind_
  GIndexedDeclarationArgKind :: GInt -> Tree GArgKind_
  GKindArgKind :: GKind -> Tree GArgKind_
  GX_Argument :: Tree GArgument_
  GY_Argument :: Tree GArgument_
  GZ_Argument :: Tree GArgument_
  LexCompar :: String -> Tree GCompar_
  LexConst :: String -> Tree GConst_
  GElemDeclaration :: GListTerm -> GTerm -> Tree GDeclaration_
  GFunctionDeclaration :: GIdent -> GTerm -> GTerm -> Tree GDeclaration_
  LexEnvironment :: String -> Tree GEnvironment_
  GBinaryEquation :: GCompar -> GTerm -> GTerm -> Tree GEquation_
  GChainEquation :: GCompar -> GTerm -> GEquation -> Tree GEquation_
  GAdj2Example :: GAdj2 -> GArgument -> GArgument -> Tree GExample_
  GAdj3Example :: GAdj3 -> GArgument -> GArgument -> GArgument -> Tree GExample_
  GAdjCExample :: GAdjC -> GArgument -> GArgument -> Tree GExample_
  GAdjEExample :: GAdjE -> GArgument -> GArgument -> Tree GExample_
  GAdjExample :: GAdj -> GArgument -> Tree GExample_
  GFam2Example :: GFam2 -> GKindArgument -> GKindArgument -> Tree GExample_
  GFamExample :: GFam -> GKindArgument -> Tree GExample_
  GFun2Example :: GFun2 -> GArgument -> GArgument -> Tree GExample_
  GFunCExample :: GFunC -> GArgument -> GArgument -> Tree GExample_
  GFunExample :: GFun -> GArgument -> Tree GExample_
  GLabelExample :: GLabel -> Tree GExample_
  GNameExample :: GName -> Tree GExample_
  GNoun1Example :: GNoun1 -> GArgument -> Tree GExample_
  GNoun2Example :: GNoun2 -> GArgument -> GArgument -> Tree GExample_
  GNounCExample :: GNounC -> GArgument -> GArgument -> Tree GExample_
  GNounExample :: GNoun -> Tree GExample_
  GVerb2Example :: GVerb2 -> GArgument -> GArgument -> Tree GExample_
  GVerbCExample :: GVerbC -> GArgument -> GArgument -> Tree GExample_
  GVerbExample :: GVerb -> GArgument -> Tree GExample_
  GAbsExp :: GListIdent -> GExp -> Tree GExp_
  GAllIdentsKindExp :: GListIdent -> GKind -> Tree GExp_
  GAllKindExp :: GKind -> Tree GExp_
  GAndExp :: GListExp -> Tree GExp_
  GAnnotateExp :: GIdent -> GExp -> Tree GExp_
  GAppExp :: GExp -> GExps -> Tree GExp_
  GBothAndExp :: GExp -> GExp -> Tree GExp_
  GCoercionExp :: GCoercion -> GExp -> Tree GExp_
  GCoreAbsExp :: GIdent -> GExp -> Tree GExp_
  GEitherOrExp :: GExp -> GExp -> Tree GExp_
  GEnumSetExp :: GExps -> Tree GExp_
  GEveryIdentKindExp :: GIdent -> GKind -> Tree GExp_
  GEveryKindExp :: GKind -> Tree GExp_
  GFun2Exp :: GFun2 -> GExp -> GExp -> Tree GExp_
  GFunCCollExp :: GFunC -> GListExp -> Tree GExp_
  GFunCExp :: GFunC -> GExp -> GExp -> Tree GExp_
  GFunExp :: GFun -> GExp -> Tree GExp_
  GIndefIdentKindExp :: GIdent -> GKind -> Tree GExp_
  GIndefKindExp :: GKind -> Tree GExp_
  GIndexedTermExp :: GInt -> Tree GExp_
  GIntegralExp :: GExp -> GExp -> GIdent -> GExp -> Tree GExp_
  GKindExp :: GKind -> Tree GExp_
  GNameExp :: GName -> Tree GExp_
  GNoIdentsKindExp :: GListIdent -> GKind -> Tree GExp_
  GNoKindExp :: GKind -> Tree GExp_
  GOrExp :: GListExp -> Tree GExp_
  GSeriesExp :: GExp -> GIdent -> GExp -> Tree GExp_
  GSigmaExp :: GExp -> GExp -> GIdent -> GExp -> Tree GExp_
  GSomeIdentsKindExp :: GListIdent -> GKind -> Tree GExp_
  GSomeKindExp :: GKind -> Tree GExp_
  GTermExp :: GTerm -> Tree GExp_
  GTypedExp :: GExp -> GKind -> Tree GExp_
  GManyExps :: GListExp -> Tree GExps_
  GOneExps :: GExp -> Tree GExps_
  GNounPrepFam :: GNoun -> GPrep -> Tree GFam_
  LexFam :: String -> Tree GFam_
  GNounPrepFam2 :: GNoun -> GPrep -> GPrep -> Tree GFam2_
  LexFam2 :: String -> Tree GFam2_
  GStringFilename :: GString -> Tree GFilename_
  GApp1MacroFormula :: GMacro -> GTerm -> Tree GFormula_
  GApp2MacroFormula :: GMacro -> GTerm -> GTerm -> Tree GFormula_
  GApp3MacroFormula :: GMacro -> GTerm -> GTerm -> GTerm -> Tree GFormula_
  GElemFormula :: GListTerm -> GTerm -> Tree GFormula_
  GEquationFormula :: GEquation -> Tree GFormula_
  GMacroFormula :: GMacro -> Tree GFormula_
  Gmodulo_Formula :: GTerm -> GTerm -> GTerm -> Tree GFormula_
  GNounPrepFun :: GNoun -> GPrep -> Tree GFun_
  LexFun :: String -> Tree GFun_
  GNounPrepFun2 :: GNoun -> GPrep -> GPrep -> Tree GFun2_
  LexFun2 :: String -> Tree GFun2_
  GNounPrepFunC :: GNoun -> GPrep -> Tree GFunC_
  LexFunC :: String -> Tree GFunC_
  GDerivativeFunction :: GFunction -> Tree GFunction_
  GIdentFunction :: GIdent -> Tree GFunction_
  GafortioriHence :: Tree GHence_
  GaltogetherHence :: Tree GHence_
  GhenceHence :: Tree GHence_
  GinParticularHence :: Tree GHence_
  GnoHence :: Tree GHence_
  GthenHence :: Tree GHence_
  GthusHence :: Tree GHence_
  GweConcludeHence :: Tree GHence_
  GBareVarHypo :: GIdent -> Tree GHypo_
  GBareVarsHypo :: GListIdent -> Tree GHypo_
  GIndexedLetFormulaHypo :: GInt -> Tree GHypo_
  GLetDeclarationHypo :: GDeclaration -> Tree GHypo_
  GLetFormulaHypo :: GFormula -> Tree GHypo_
  GLocalHypo :: GLocal -> Tree GHypo_
  GPropHypo :: GProp -> Tree GHypo_
  GSupposePropHypo :: GProp -> Tree GHypo_
  GVarHypo :: GIdent -> GKind -> Tree GHypo_
  GVarsHypo :: GListIdent -> GKind -> Tree GHypo_
  GStrIdent :: GString -> Tree GIdent_
  GAxiomExpJmt :: GLabel -> GListHypo -> GExp -> GKind -> Tree GJmt_
  GAxiomJmt :: GLabel -> GListHypo -> GProp -> Tree GJmt_
  GAxiomKindJmt :: GLabel -> GListHypo -> GKind -> Tree GJmt_
  GAxiomPropJmt :: GLabel -> GListHypo -> GProp -> Tree GJmt_
  GDefExpJmt :: GLabel -> GListHypo -> GExp -> GKind -> GExp -> Tree GJmt_
  GDefKindJmt :: GLabel -> GListHypo -> GKind -> GKind -> Tree GJmt_
  GDefPropJmt :: GLabel -> GListHypo -> GProp -> GProp -> Tree GJmt_
  GDefUntypedExpJmt :: GLabel -> GExp -> GExp -> Tree GJmt_
  GDefinedAdjJmt :: GLabel -> GListHypo -> GExp -> GAdj -> GProp -> Tree GJmt_
  GRewriteJmt :: GListRule -> Tree GJmt_
  GThmJmt :: GLabel -> GListHypo -> GProp -> GProof -> Tree GJmt_
  GUnitJmt :: GUnit -> Tree GJmt_
  GWeDefineAdjJmt :: GLabel -> GListHypo -> GExp -> GAdj -> GProp -> Tree GJmt_
  GAdjKind :: GAdj -> GKind -> Tree GKind_
  GAnnotateKind :: GIdent -> GKind -> Tree GKind_
  GAppKind :: GIdent -> GExps -> Tree GKind_
  GElemKind :: GKind -> Tree GKind_
  GFam2Kind :: GFam2 -> GKind -> GKind -> Tree GKind_
  GFamKind :: GFam -> GKind -> Tree GKind_
  GFunKind :: GListArgKind -> GKind -> Tree GKind_
  GIdentKind :: GIdent -> Tree GKind_
  GNounKind :: GNoun -> Tree GKind_
  GSuchThatKind :: GKind -> GIdent -> GProp -> Tree GKind_
  GTermKind :: GTerm -> Tree GKind_
  GA_KindArgument :: Tree GKindArgument_
  GB_KindArgument :: Tree GKindArgument_
  GDefNounLabel :: GNoun -> Tree GLabel_
  GIdentLabel :: GIdent -> Tree GLabel_
  GNounIdentLabel :: GNoun -> GIdent -> Tree GLabel_
  GNounIntLabel :: GNoun -> GInt -> Tree GLabel_
  GNounLabel :: GNoun -> Tree GLabel_
  GNounOfNounLabel :: GNoun -> GNoun -> Tree GLabel_
  GProperNameNounLabel :: GProperName -> GNoun -> Tree GLabel_
  GcrefLabel :: GIdent -> Tree GLabel_
  LexLabel :: String -> Tree GLabel_
  GListAdj :: [GAdj] -> Tree GListAdj_
  GListArgKind :: [GArgKind] -> Tree GListArgKind_
  GListExp :: [GExp] -> Tree GListExp_
  GListHypo :: [GHypo] -> Tree GListHypo_
  GListIdent :: [GIdent] -> Tree GListIdent_
  GListProof :: [GProof] -> Tree GListProof_
  GListProp :: [GProp] -> Tree GListProp_
  GListRule :: [GRule] -> Tree GListRule_
  GListTerm :: [GTerm] -> Tree GListTerm_
  GListUnit :: [GUnit] -> Tree GListUnit_
  GBareLetLocal :: GIdent -> GExp -> Tree GLocal_
  GLetLocal :: GIdent -> GKind -> GExp -> Tree GLocal_
  GStringMacro :: GString -> Tree GMacro_
  GStringMethod :: GString -> Tree GMethod_
  GDefNounName :: GNoun -> Tree GName_
  GProperNameNounName :: GProperName -> GNoun -> Tree GName_
  LexName :: String -> Tree GName_
  GAdjNounNoun :: GAdj -> GNoun -> Tree GNoun_
  GNounNounNoun :: GNoun -> GNoun -> Tree GNoun_
  GProperNameNounNoun :: GProperName -> GNoun -> Tree GNoun_
  LexNoun :: String -> Tree GNoun_
  GNounNoun1 :: GNoun -> Tree GNoun1_
  LexNoun1 :: String -> Tree GNoun1_
  GNounPrepNoun2 :: GNoun -> GPrep -> Tree GNoun2_
  LexNoun2 :: String -> Tree GNoun2_
  GNounNounC :: GNoun -> Tree GNounC_
  LexNounC :: String -> Tree GNounC_
  LexOper :: String -> Tree GOper_
  LexOper2 :: String -> Tree GOper2_
  LexPrep :: String -> Tree GPrep_
  GAbsProof :: GListHypo -> GProof -> Tree GProof_
  GAnnotateProof :: GIdent -> GProof -> Tree GProof_
  GAppProof :: GProofExp -> GListProof -> Tree GProof_
  GNatIndProof :: GIdent -> GProp -> GProof -> GIdent -> GIdent -> GProof -> Tree GProof_
  GUnitsProof :: GListUnit -> Tree GProof_
  GandElProof :: GProp -> GProp -> GProof -> Tree GProof_
  GandErProof :: GProp -> GProp -> GProof -> Tree GProof_
  GandIProof :: GProp -> GProp -> GProof -> GProof -> Tree GProof_
  GevenSuccProof :: GExp -> GProof -> Tree GProof_
  GevenZeroProof :: Tree GProof_
  GexistsEProof :: GKind -> GIdent -> GProp -> GProp -> GProof -> GIdent -> GIdent -> GProof -> Tree GProof_
  GexistsIProof :: GKind -> GIdent -> GProp -> GExp -> GProof -> Tree GProof_
  GfalseEProof :: GProp -> GProof -> Tree GProof_
  GforallEProof :: GKind -> GIdent -> GProp -> GProof -> GExp -> Tree GProof_
  GforallIProof :: GKind -> GIdent -> GProp -> GIdent -> GProof -> Tree GProof_
  GhypoProof :: GProp -> GIdent -> Tree GProof_
  GifEProof :: GProp -> GProp -> GProof -> GProof -> Tree GProof_
  GifIProof :: GProp -> GProp -> GIdent -> GProof -> Tree GProof_
  GoddSuccProof :: GExp -> GProof -> Tree GProof_
  GorEProof :: GProp -> GProp -> GProp -> GProof -> GIdent -> GProof -> GIdent -> GProof -> Tree GProof_
  GorIlProof :: GProp -> GProp -> GProof -> Tree GProof_
  GorIrProof :: GProp -> GProp -> GProof -> Tree GProof_
  GreflProof :: GKind -> GExp -> Tree GProof_
  GAbsProofExp :: GListHypo -> GProofExp -> Tree GProofExp_
  GAnnotateProofExp :: GIdent -> GProofExp -> Tree GProofExp_
  GAppProofExp :: GProofExp -> GExps -> Tree GProofExp_
  GLabelProofExp :: GLabel -> Tree GProofExp_
  GAdj2Prop :: GAdj2 -> GExp -> GExp -> Tree GProp_
  GAdj3Prop :: GAdj3 -> GExp -> GExp -> GExp -> Tree GProp_
  GAdjCCollProp :: GAdjC -> GListExp -> Tree GProp_
  GAdjCProp :: GAdjC -> GExp -> GExp -> Tree GProp_
  GAdjECollProp :: GAdjE -> GListExp -> Tree GProp_
  GAdjEProp :: GAdjE -> GExp -> GExp -> Tree GProp_
  GAdjProp :: GAdj -> GExp -> Tree GProp_
  GAllProp :: GListArgKind -> GProp -> Tree GProp_
  GAndProp :: GListProp -> Tree GProp_
  GAnnotateProp :: GIdent -> GProp -> Tree GProp_
  GAppProp :: GIdent -> GExps -> Tree GProp_
  GBothAndProp :: GProp -> GProp -> Tree GProp_
  GCoreAllProp :: GKind -> GIdent -> GProp -> Tree GProp_
  GCoreAndProp :: GProp -> GProp -> Tree GProp_
  GCoreExistProp :: GKind -> GIdent -> GProp -> Tree GProp_
  GCoreIfProp :: GProp -> GProp -> Tree GProp_
  GCoreIffProp :: GProp -> GProp -> Tree GProp_
  GCoreNotProp :: GProp -> Tree GProp_
  GCoreOrProp :: GProp -> GProp -> Tree GProp_
  GDisplayFormulaProp :: GFormula -> Tree GProp_
  GEitherOrProp :: GProp -> GProp -> Tree GProp_
  GExistNoProp :: GListArgKind -> GProp -> Tree GProp_
  GExistProp :: GListArgKind -> GProp -> Tree GProp_
  GFalseProp :: Tree GProp_
  GFormulaImpliesProp :: GFormula -> GFormula -> Tree GProp_
  GFormulaProp :: GFormula -> Tree GProp_
  GIdentProp :: GIdent -> Tree GProp_
  GIfProp :: GProp -> GProp -> Tree GProp_
  GIffIffProp :: GProp -> GProp -> Tree GProp_
  GIffProp :: GProp -> GProp -> Tree GProp_
  GIndexedFormulaProp :: GInt -> Tree GProp_
  GKindProp :: GExp -> GKind -> Tree GProp_
  GNoArticleExistProp :: GArgKind -> GProp -> Tree GProp_
  GNoCommaAllProp :: GListArgKind -> GProp -> Tree GProp_
  GNoCommaExistProp :: GListArgKind -> GProp -> Tree GProp_
  GNotAdj2Prop :: GAdj2 -> GExp -> GExp -> Tree GProp_
  GNotAdjCProp :: GAdjC -> GListExp -> Tree GProp_
  GNotAdjEProp :: GAdjE -> GListExp -> Tree GProp_
  GNotAdjProp :: GAdj -> GExp -> Tree GProp_
  GNotNoun1Prop :: GNoun1 -> GExp -> Tree GProp_
  GNotNoun2Prop :: GNoun2 -> GExp -> GExp -> Tree GProp_
  GNotNounCProp :: GNounC -> GListExp -> Tree GProp_
  GNotVerb2Prop :: GVerb2 -> GExp -> GExp -> Tree GProp_
  GNotVerbCProp :: GVerbC -> GListExp -> Tree GProp_
  GNotVerbProp :: GVerb -> GExp -> Tree GProp_
  GNoun1Prop :: GNoun1 -> GExp -> Tree GProp_
  GNoun2Prop :: GNoun2 -> GExp -> GExp -> Tree GProp_
  GNounCProp :: GNounC -> GExp -> GExp -> Tree GProp_
  GOnlyIfProp :: GProp -> GProp -> Tree GProp_
  GOrProp :: GListProp -> Tree GProp_
  GPostQuantProp :: GProp -> GExp -> Tree GProp_
  GProofProp :: GProp -> Tree GProp_
  GVerb2Prop :: GVerb2 -> GExp -> GExp -> Tree GProp_
  GVerbCProp :: GVerbC -> GExp -> GExp -> Tree GProp_
  GVerbProp :: GVerb -> GExp -> Tree GProp_
  GWeHaveProp :: GProp -> Tree GProp_
  GsameParityProp :: GExp -> GExp -> Tree GProp_
  GAccor_ProperName :: Tree GProperName_
  GAckermann_ProperName :: Tree GProperName_
  GAdams_ProperName :: Tree GProperName_
  GAdinkra_ProperName :: Tree GProperName_
  GAhlfors_ProperName :: Tree GProperName_
  GAlbert_ProperName :: Tree GProperName_
  GAlexander_ProperName :: Tree GProperName_
  GAlexandroff_ProperName :: Tree GProperName_
  GAlexandrov_ProperName :: Tree GProperName_
  GAndrásfai_ProperName :: Tree GProperName_
  GAnosov_ProperName :: Tree GProperName_
  GArf_ProperName :: Tree GProperName_
  GAronszajn_ProperName :: Tree GProperName_
  GArtin_ProperName :: Tree GProperName_
  GAschbacher_ProperName :: Tree GProperName_
  GAshtekar_ProperName :: Tree GProperName_
  GAtiyah_ProperName :: Tree GProperName_
  GAzumaya_ProperName :: Tree GProperName_
  GBaer_ProperName :: Tree GProperName_
  GBanach_ProperName :: Tree GProperName_
  GBarnes_ProperName :: Tree GProperName_
  GBayes_ProperName :: Tree GProperName_
  GBeltrami_ProperName :: Tree GProperName_
  GBendixson_ProperName :: Tree GProperName_
  GBenini_ProperName :: Tree GProperName_
  GBenktander_ProperName :: Tree GProperName_
  GBerge_ProperName :: Tree GProperName_
  GBerkeley_ProperName :: Tree GProperName_
  GBernoulli_ProperName :: Tree GProperName_
  GBesov_ProperName :: Tree GProperName_
  GBhattacharyya_ProperName :: Tree GProperName_
  GBianchi_ProperName :: Tree GProperName_
  GBidiakis_ProperName :: Tree GProperName_
  GBirkhoff_ProperName :: Tree GProperName_
  GBlanuša_ProperName :: Tree GProperName_
  GBlum_ProperName :: Tree GProperName_
  GBochner_ProperName :: Tree GProperName_
  GBockstein_ProperName :: Tree GProperName_
  GBorel_ProperName :: Tree GProperName_
  GBott_ProperName :: Tree GProperName_
  GBrandt_ProperName :: Tree GProperName_
  GBrauer_ProperName :: Tree GProperName_
  GBrauner_ProperName :: Tree GProperName_
  GBregman_ProperName :: Tree GProperName_
  GBrinkmann_ProperName :: Tree GProperName_
  GBruhat_ProperName :: Tree GProperName_
  GBruijn_ProperName :: Tree GProperName_
  GBurnside_ProperName :: Tree GProperName_
  GCalkin_ProperName :: Tree GProperName_
  GCamden_ProperName :: Tree GProperName_
  GCantor_ProperName :: Tree GProperName_
  GCarleson_ProperName :: Tree GProperName_
  GCarlyle_ProperName :: Tree GProperName_
  GCarmichael_ProperName :: Tree GProperName_
  GCarnot_ProperName :: Tree GProperName_
  GCarol_ProperName :: Tree GProperName_
  GCartan_ProperName :: Tree GProperName_
  GCarter_ProperName :: Tree GProperName_
  GCartier_ProperName :: Tree GProperName_
  GCauchy_ProperName :: Tree GProperName_
  GCayley_ProperName :: Tree GProperName_
  GChang_ProperName :: Tree GProperName_
  GChebyshev_ProperName :: Tree GProperName_
  GCheeger_ProperName :: Tree GProperName_
  GChevalley_ProperName :: Tree GProperName_
  GClebsch_ProperName :: Tree GProperName_
  GClifford_ProperName :: Tree GProperName_
  GCoates_ProperName :: Tree GProperName_
  GCohen_ProperName :: Tree GProperName_
  GColin_ProperName :: Tree GProperName_
  GColinear_ProperName :: Tree GProperName_
  GColombeau_ProperName :: Tree GProperName_
  GCox_ProperName :: Tree GProperName_
  GCoxeter_ProperName :: Tree GProperName_
  GCremona_ProperName :: Tree GProperName_
  GCullen_ProperName :: Tree GProperName_
  GCunningham_ProperName :: Tree GProperName_
  GDade_ProperName :: Tree GProperName_
  GDamm_ProperName :: Tree GProperName_
  GDarboux_ProperName :: Tree GProperName_
  GDe_ProperName :: Tree GProperName_
  GDedekind_ProperName :: Tree GProperName_
  GDehn_ProperName :: Tree GProperName_
  GDelambre_ProperName :: Tree GProperName_
  GDeligne_ProperName :: Tree GProperName_
  GDelta_ProperName :: Tree GProperName_
  GDescartes_ProperName :: Tree GProperName_
  GDieudonné_ProperName :: Tree GProperName_
  GDini_ProperName :: Tree GProperName_
  GDirac_ProperName :: Tree GProperName_
  GDirichlet_ProperName :: Tree GProperName_
  GDolbeault_ProperName :: Tree GProperName_
  GDrinfeld_ProperName :: Tree GProperName_
  GDuflo_ProperName :: Tree GProperName_
  GDunkl_ProperName :: Tree GProperName_
  GDunn_ProperName :: Tree GProperName_
  GDyck_ProperName :: Tree GProperName_
  GDynkin_ProperName :: Tree GProperName_
  GDürer_ProperName :: Tree GProperName_
  GEarth_ProperName :: Tree GProperName_
  GEberlein_ProperName :: Tree GProperName_
  GErdős_ProperName :: Tree GProperName_
  GErrera_ProperName :: Tree GProperName_
  GEuler_ProperName :: Tree GProperName_
  GFatou_ProperName :: Tree GProperName_
  GFeigenbaum_ProperName :: Tree GProperName_
  GFejér_ProperName :: Tree GProperName_
  GFermat_ProperName :: Tree GProperName_
  GFeynman_ProperName :: Tree GProperName_
  GFibonacci_ProperName :: Tree GProperName_
  GFock_ProperName :: Tree GProperName_
  GFolkman_ProperName :: Tree GProperName_
  GFoster_ProperName :: Tree GProperName_
  GFranklin_ProperName :: Tree GProperName_
  GFrattini_ProperName :: Tree GProperName_
  GFraïssé_ProperName :: Tree GProperName_
  GFredholm_ProperName :: Tree GProperName_
  GFriedman_ProperName :: Tree GProperName_
  GFrobenius_ProperName :: Tree GProperName_
  GFréchet_ProperName :: Tree GProperName_
  GFrölicher_ProperName :: Tree GProperName_
  GGalerkin_ProperName :: Tree GProperName_
  GGalois_ProperName :: Tree GProperName_
  GGerstenhaber_ProperName :: Tree GProperName_
  GGewirtz_ProperName :: Tree GProperName_
  GGibbs_ProperName :: Tree GProperName_
  GGibrat_ProperName :: Tree GProperName_
  GGiuga_ProperName :: Tree GProperName_
  GGolomb_ProperName :: Tree GProperName_
  GGosset_ProperName :: Tree GProperName_
  GGoursat_ProperName :: Tree GProperName_
  GGrassmann_ProperName :: Tree GProperName_
  GGreen_ProperName :: Tree GProperName_
  GGrinberg_ProperName :: Tree GProperName_
  GGromov_ProperName :: Tree GProperName_
  GGrothendieck_ProperName :: Tree GProperName_
  GGrötzsch_ProperName :: Tree GProperName_
  GHadwiger_ProperName :: Tree GProperName_
  GHalin_ProperName :: Tree GProperName_
  GHall_ProperName :: Tree GProperName_
  GHamel_ProperName :: Tree GProperName_
  GHanoi_ProperName :: Tree GProperName_
  GHardy_ProperName :: Tree GProperName_
  GHarries_ProperName :: Tree GProperName_
  GHarrop_ProperName :: Tree GProperName_
  GHart_ProperName :: Tree GProperName_
  GHatzel_ProperName :: Tree GProperName_
  GHausdorff_ProperName :: Tree GProperName_
  GHawkes_ProperName :: Tree GProperName_
  GHeawood_ProperName :: Tree GProperName_
  GHecke_ProperName :: Tree GProperName_
  GHeisenberg_ProperName :: Tree GProperName_
  GHellinger_ProperName :: Tree GProperName_
  GHenson_ProperName :: Tree GProperName_
  GHerbrand_ProperName :: Tree GProperName_
  GHerschel_ProperName :: Tree GProperName_
  GHeyting_ProperName :: Tree GProperName_
  GHilbert_ProperName :: Tree GProperName_
  GHill_ProperName :: Tree GProperName_
  GHochschild_ProperName :: Tree GProperName_
  GHodge_ProperName :: Tree GProperName_
  GHoffman_ProperName :: Tree GProperName_
  GHolt_ProperName :: Tree GProperName_
  GHosoya_ProperName :: Tree GProperName_
  GHurwitz_ProperName :: Tree GProperName_
  GIkeda_ProperName :: Tree GProperName_
  GIwasawa_ProperName :: Tree GProperName_
  GJaccard_ProperName :: Tree GProperName_
  GJacobi_ProperName :: Tree GProperName_
  GJacobson_ProperName :: Tree GProperName_
  GJaffard_ProperName :: Tree GProperName_
  GJohnson_ProperName :: Tree GProperName_
  GJordan_ProperName :: Tree GProperName_
  GJulia_ProperName :: Tree GProperName_
  GJónsson_ProperName :: Tree GProperName_
  GKakeya_ProperName :: Tree GProperName_
  GKalman_ProperName :: Tree GProperName_
  GKan_ProperName :: Tree GProperName_
  GKane_ProperName :: Tree GProperName_
  GKaprekar_ProperName :: Tree GProperName_
  GKaroubi_ProperName :: Tree GProperName_
  GKasch_ProperName :: Tree GProperName_
  GKeith_ProperName :: Tree GProperName_
  GKempe_ProperName :: Tree GProperName_
  GKent_ProperName :: Tree GProperName_
  GKhinchin_ProperName :: Tree GProperName_
  GKittell_ProperName :: Tree GProperName_
  GKleene_ProperName :: Tree GProperName_
  GKlein_ProperName :: Tree GProperName_
  GKleisli_ProperName :: Tree GProperName_
  GKneser_ProperName :: Tree GProperName_
  GKnödel_ProperName :: Tree GProperName_
  GKodaira_ProperName :: Tree GProperName_
  GKolmogorov_ProperName :: Tree GProperName_
  GKorovkin_ProperName :: Tree GProperName_
  GKoszul_ProperName :: Tree GProperName_
  GKrein_ProperName :: Tree GProperName_
  GKripke_ProperName :: Tree GProperName_
  GKronecker_ProperName :: Tree GProperName_
  GKrylov_ProperName :: Tree GProperName_
  GKummer_ProperName :: Tree GProperName_
  GKynea_ProperName :: Tree GProperName_
  GKünneth_ProperName :: Tree GProperName_
  GLagrange_ProperName :: Tree GProperName_
  GLaue_ProperName :: Tree GProperName_
  GLawvere_ProperName :: Tree GProperName_
  GLebesgue_ProperName :: Tree GProperName_
  GLegendre_ProperName :: Tree GProperName_
  GLeibniz_ProperName :: Tree GProperName_
  GLevenshtein_ProperName :: Tree GProperName_
  GLevi_ProperName :: Tree GProperName_
  GLeyland_ProperName :: Tree GProperName_
  GLie_ProperName :: Tree GProperName_
  GLindelöf_ProperName :: Tree GProperName_
  GLipschitz_ProperName :: Tree GProperName_
  GLissajous_ProperName :: Tree GProperName_
  GLiverTox_ProperName :: Tree GProperName_
  GLjubljana_ProperName :: Tree GProperName_
  GLoewner_ProperName :: Tree GProperName_
  GLondon_ProperName :: Tree GProperName_
  GLorentz_ProperName :: Tree GProperName_
  GLoupekine_ProperName :: Tree GProperName_
  GLovász_ProperName :: Tree GProperName_
  GLucas_ProperName :: Tree GProperName_
  GLyapunov_ProperName :: Tree GProperName_
  GMacdonald_ProperName :: Tree GProperName_
  GMahalanobis_ProperName :: Tree GProperName_
  GMahler_ProperName :: Tree GProperName_
  GMandelbrot_ProperName :: Tree GProperName_
  GManin_ProperName :: Tree GProperName_
  GMarkov_ProperName :: Tree GProperName_
  GMazur_ProperName :: Tree GProperName_
  GMcGee_ProperName :: Tree GProperName_
  GMcKay_ProperName :: Tree GProperName_
  GMcLaughlin_ProperName :: Tree GProperName_
  GMeredith_ProperName :: Tree GProperName_
  GMeringer_ProperName :: Tree GProperName_
  GMersenne_ProperName :: Tree GProperName_
  GMeyniel_ProperName :: Tree GProperName_
  GMiller_ProperName :: Tree GProperName_
  GMilnor_ProperName :: Tree GProperName_
  GMinkowski_ProperName :: Tree GProperName_
  GMontel_ProperName :: Tree GProperName_
  GMoore_ProperName :: Tree GProperName_
  GMorgan_ProperName :: Tree GProperName_
  GMorse_ProperName :: Tree GProperName_
  GMoser_ProperName :: Tree GProperName_
  GMotzkin_ProperName :: Tree GProperName_
  GMumford_ProperName :: Tree GProperName_
  GMunn_ProperName :: Tree GProperName_
  GMöbius_ProperName :: Tree GProperName_
  GMünchhausen_ProperName :: Tree GProperName_
  GNagata_ProperName :: Tree GProperName_
  GNakajima_ProperName :: Tree GProperName_
  GNarayana_ProperName :: Tree GProperName_
  GNeumann_ProperName :: Tree GProperName_
  GNewton_ProperName :: Tree GProperName_
  GOckham_ProperName :: Tree GProperName_
  GPaley_ProperName :: Tree GProperName_
  GPareto_ProperName :: Tree GProperName_
  GParrondo_ProperName :: Tree GProperName_
  GPatricia_ProperName :: Tree GProperName_
  GPeano_ProperName :: Tree GProperName_
  GPearcey_ProperName :: Tree GProperName_
  GPenrose_ProperName :: Tree GProperName_
  GPerkel_ProperName :: Tree GProperName_
  GPerrin_ProperName :: Tree GProperName_
  GPetersen_ProperName :: Tree GProperName_
  GPetri_ProperName :: Tree GProperName_
  GPettis_ProperName :: Tree GProperName_
  GPicard_ProperName :: Tree GProperName_
  GPippard_ProperName :: Tree GProperName_
  GPoincaré_ProperName :: Tree GProperName_
  GPoisson_ProperName :: Tree GProperName_
  GPoussin_ProperName :: Tree GProperName_
  GPtolemy_ProperName :: Tree GProperName_
  GPuig_ProperName :: Tree GProperName_
  GRado_ProperName :: Tree GProperName_
  GRadon_ProperName :: Tree GProperName_
  GRajchman_ProperName :: Tree GProperName_
  GRamanujan_ProperName :: Tree GProperName_
  GRees_ProperName :: Tree GProperName_
  GReeve_ProperName :: Tree GProperName_
  GReinhardt_ProperName :: Tree GProperName_
  GRham_ProperName :: Tree GProperName_
  GRiemann_ProperName :: Tree GProperName_
  GRiesel_ProperName :: Tree GProperName_
  GRiesz_ProperName :: Tree GProperName_
  GRobbins_ProperName :: Tree GProperName_
  GRobertson_ProperName :: Tree GProperName_
  GRosenbrock_ProperName :: Tree GProperName_
  GRoyle_ProperName :: Tree GProperName_
  GSchauder_ProperName :: Tree GProperName_
  GSchläfli_ProperName :: Tree GProperName_
  GSchreier_ProperName :: Tree GProperName_
  GSchrödinger_ProperName :: Tree GProperName_
  GSchubert_ProperName :: Tree GProperName_
  GSchur_ProperName :: Tree GProperName_
  GSchwartz_ProperName :: Tree GProperName_
  GSchwarz_ProperName :: Tree GProperName_
  GScott_ProperName :: Tree GProperName_
  GSelberg_ProperName :: Tree GProperName_
  GSerre_ProperName :: Tree GProperName_
  GShannon_ProperName :: Tree GProperName_
  GShelah_ProperName :: Tree GProperName_
  GShimura_ProperName :: Tree GProperName_
  GShrikhande_ProperName :: Tree GProperName_
  GSiegel_ProperName :: Tree GProperName_
  GSierpinski_ProperName :: Tree GProperName_
  GSitter_ProperName :: Tree GProperName_
  GSlater_ProperName :: Tree GProperName_
  GSmith_ProperName :: Tree GProperName_
  GSousselier_ProperName :: Tree GProperName_
  GSpencer_ProperName :: Tree GProperName_
  GStark_ProperName :: Tree GProperName_
  GSteenrod_ProperName :: Tree GProperName_
  GSteinitz_ProperName :: Tree GProperName_
  GStepanoff_ProperName :: Tree GProperName_
  GStiefel_ProperName :: Tree GProperName_
  GStirling_ProperName :: Tree GProperName_
  GStone_ProperName :: Tree GProperName_
  GSuslin_ProperName :: Tree GProperName_
  GSuzuki_ProperName :: Tree GProperName_
  GSylow_ProperName :: Tree GProperName_
  GSylvester_ProperName :: Tree GProperName_
  GSzekeres_ProperName :: Tree GProperName_
  GSárközy_ProperName :: Tree GProperName_
  GSørensen_ProperName :: Tree GProperName_
  GTaleb_ProperName :: Tree GProperName_
  GTarski_ProperName :: Tree GProperName_
  GTate_ProperName :: Tree GProperName_
  GTaylor_ProperName :: Tree GProperName_
  GThabit_ProperName :: Tree GProperName_
  GThom_ProperName :: Tree GProperName_
  GThomassen_ProperName :: Tree GProperName_
  GThompson_ProperName :: Tree GProperName_
  GThue_ProperName :: Tree GProperName_
  GTime_ProperName :: Tree GProperName_
  GTrofimov_ProperName :: Tree GProperName_
  GTrémaux_ProperName :: Tree GProperName_
  GTurán_ProperName :: Tree GProperName_
  GTutte_ProperName :: Tree GProperName_
  GTweedie_ProperName :: Tree GProperName_
  GTychonoff_ProperName :: Tree GProperName_
  GUrysohn_ProperName :: Tree GProperName_
  GVandermonde_ProperName :: Tree GProperName_
  GVerdière_ProperName :: Tree GProperName_
  GVickrey_ProperName :: Tree GProperName_
  GWallman_ProperName :: Tree GProperName_
  GWalther_ProperName :: Tree GProperName_
  GWeibull_ProperName :: Tree GProperName_
  GWeierstrass_ProperName :: Tree GProperName_
  GWeil_ProperName :: Tree GProperName_
  GWeingarten_ProperName :: Tree GProperName_
  GWells_ProperName :: Tree GProperName_
  GWeyl_ProperName :: Tree GProperName_
  GWhewell_ProperName :: Tree GProperName_
  GWieferich_ProperName :: Tree GProperName_
  GWitt_ProperName :: Tree GProperName_
  GWoodall_ProperName :: Tree GProperName_
  GWoodin_ProperName :: Tree GProperName_
  GYoneda_ProperName :: Tree GProperName_
  GZamfirescu_ProperName :: Tree GProperName_
  GZaslavskii_ProperName :: Tree GProperName_
  GZassenhaus_ProperName :: Tree GProperName_
  GZeisel_ProperName :: Tree GProperName_
  GZhegalkin_ProperName :: Tree GProperName_
  GZinbiel_ProperName :: Tree GProperName_
  GZorn_ProperName :: Tree GProperName_
  GZuckerman_ProperName :: Tree GProperName_
  GŁojasiewicz_ProperName :: Tree GProperName_
  GŁukasiewicz_ProperName :: Tree GProperName_
  GNoVarRewriteRule :: GExp -> GExp -> Tree GRule_
  GRewriteRule :: GListIdent -> GExp -> GExp -> Tree GRule_
  GApp1MacroTerm :: GMacro -> GTerm -> Tree GTerm_
  GApp2MacroTerm :: GMacro -> GTerm -> GTerm -> Tree GTerm_
  GApp3MacroTerm :: GMacro -> GTerm -> GTerm -> GTerm -> Tree GTerm_
  GAppFunctionTerm :: GFunction -> GListTerm -> Tree GTerm_
  GComprehensionTerm :: GTerm -> GTerm -> GFormula -> Tree GTerm_
  GConstTerm :: GConst -> Tree GTerm_
  GEnumSetTerm :: GListTerm -> Tree GTerm_
  GIdentTerm :: GIdent -> Tree GTerm_
  GMacroTerm :: GMacro -> Tree GTerm_
  GNumberTerm :: GInt -> Tree GTerm_
  GOper2Term :: GOper2 -> GTerm -> GTerm -> Tree GTerm_
  GOperTerm :: GOper -> GTerm -> Tree GTerm_
  GParenthTerm :: GTerm -> Tree GTerm_
  GTextbfTerm :: GTerm -> Tree GTerm_
  Gintegral_Term :: GTerm -> GTerm -> GIdent -> GTerm -> Tree GTerm_
  Gseries_Term :: GTerm -> GIdent -> GTerm -> Tree GTerm_
  Gsigma_Term :: GTerm -> GTerm -> GIdent -> GTerm -> Tree GTerm_
  Gsum3dots_Term :: GTerm -> GTerm -> GTerm -> Tree GTerm_
  Gtimes_Term :: GTerm -> GTerm -> Tree GTerm_
  GStringTitle :: GString -> Tree GTitle_
  GBeginEnvironmentUnit :: GEnvironment -> GLabel -> Tree GUnit_
  GBeginProofMethodUnit :: GLabel -> GMethod -> Tree GUnit_
  GCaseGoal :: GProp -> GIdent -> Tree GUnit_
  GCasesGoal :: Tree GUnit_
  GEndEnvironmentUnit :: GEnvironment -> Tree GUnit_
  GEnoughGoal :: GProp -> Tree GUnit_
  GFirstVerifyGoal :: GProp -> Tree GUnit_
  GFollowsPropConclusion :: GProp -> Tree GUnit_
  GHyposAssumption :: GListHypo -> Tree GUnit_
  GIdentExpAssumption :: GExp -> GIdent -> Tree GUnit_
  GIdentKindAssumption :: GKind -> GIdent -> Tree GUnit_
  GImportUnit :: GString -> Tree GUnit_
  GInductionGoal :: Tree GUnit_
  GLabelConclusion :: GLabel -> Tree GUnit_
  GLabelUnit :: GLabel -> Tree GUnit_
  GObviousConclusion :: Tree GUnit_
  GPropAssumption :: GProp -> GLabel -> Tree GUnit_
  GPropConclusion :: GHence -> GProp -> Tree GUnit_
  GPropLabelConclusion :: GHence -> GProp -> GLabel -> Tree GUnit_
  GSectionUnit :: GTitle -> GLabel -> Tree GUnit_
  GSinceConclusion :: GProp -> GProp -> Tree GUnit_
  GSinceGoal :: GProp -> GProp -> Tree GUnit_
  GSubsectionUnit :: GTitle -> GLabel -> Tree GUnit_
  GVerbPrepNounVerb :: GVerb -> GPrep -> GNoun -> Tree GVerb_
  LexVerb :: String -> Tree GVerb_
  GVerbPrepVerb2 :: GVerb -> GPrep -> Tree GVerb2_
  LexVerb2 :: String -> Tree GVerb2_
  GVerbVerbC :: GVerb -> Tree GVerbC_
  LexVerbC :: String -> Tree GVerbC_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GAdj2Adj x1 x2,GAdj2Adj y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAdj3Adj x1 x2 x3,GAdj3Adj y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAdverbAdjAdj x1 x2,GAdverbAdjAdj y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAndAdj x1,GAndAdj y1) -> and [ x1 == y1 ]
    (GBothAndAdj x1 x2,GBothAndAdj y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GEitherOrAdj x1 x2,GEitherOrAdj y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GOrAdj x1,GOrAdj y1) -> and [ x1 == y1 ]
    (LexAdj x,LexAdj y) -> x == y
    (GAdjPrepAdj2 x1 x2,GAdjPrepAdj2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexAdj2 x,LexAdj2 y) -> x == y
    (GAdjPrepAdj3 x1 x2 x3,GAdjPrepAdj3 y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (LexAdj3 x,LexAdj3 y) -> x == y
    (GAdjAdjC x1,GAdjAdjC y1) -> and [ x1 == y1 ]
    (LexAdjC x,LexAdjC y) -> x == y
    (GAdjAdjE x1,GAdjAdjE y1) -> and [ x1 == y1 ]
    (LexAdjE x,LexAdjE y) -> x == y
    (Galmost_everywhere_Adverb,Galmost_everywhere_Adverb) -> and [ ]
    (Geverywhere_Adverb,Geverywhere_Adverb) -> and [ ]
    (Guniformly_Adverb,Guniformly_Adverb) -> and [ ]
    (GBareIdentsArgKind x1,GBareIdentsArgKind y1) -> and [ x1 == y1 ]
    (GDeclarationArgKind x1,GDeclarationArgKind y1) -> and [ x1 == y1 ]
    (GIdentArgKind x1 x2,GIdentArgKind y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIdentsArgKind x1 x2,GIdentsArgKind y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIndexedDeclarationArgKind x1,GIndexedDeclarationArgKind y1) -> and [ x1 == y1 ]
    (GKindArgKind x1,GKindArgKind y1) -> and [ x1 == y1 ]
    (GX_Argument,GX_Argument) -> and [ ]
    (GY_Argument,GY_Argument) -> and [ ]
    (GZ_Argument,GZ_Argument) -> and [ ]
    (LexCompar x,LexCompar y) -> x == y
    (LexConst x,LexConst y) -> x == y
    (GElemDeclaration x1 x2,GElemDeclaration y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFunctionDeclaration x1 x2 x3,GFunctionDeclaration y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (LexEnvironment x,LexEnvironment y) -> x == y
    (GBinaryEquation x1 x2 x3,GBinaryEquation y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GChainEquation x1 x2 x3,GChainEquation y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAdj2Example x1 x2 x3,GAdj2Example y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAdj3Example x1 x2 x3 x4,GAdj3Example y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GAdjCExample x1 x2 x3,GAdjCExample y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAdjEExample x1 x2 x3,GAdjEExample y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAdjExample x1 x2,GAdjExample y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFam2Example x1 x2 x3,GFam2Example y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GFamExample x1 x2,GFamExample y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFun2Example x1 x2 x3,GFun2Example y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GFunCExample x1 x2 x3,GFunCExample y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GFunExample x1 x2,GFunExample y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GLabelExample x1,GLabelExample y1) -> and [ x1 == y1 ]
    (GNameExample x1,GNameExample y1) -> and [ x1 == y1 ]
    (GNoun1Example x1 x2,GNoun1Example y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNoun2Example x1 x2 x3,GNoun2Example y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GNounCExample x1 x2 x3,GNounCExample y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GNounExample x1,GNounExample y1) -> and [ x1 == y1 ]
    (GVerb2Example x1 x2 x3,GVerb2Example y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GVerbCExample x1 x2 x3,GVerbCExample y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GVerbExample x1 x2,GVerbExample y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAbsExp x1 x2,GAbsExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAllIdentsKindExp x1 x2,GAllIdentsKindExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAllKindExp x1,GAllKindExp y1) -> and [ x1 == y1 ]
    (GAndExp x1,GAndExp y1) -> and [ x1 == y1 ]
    (GAnnotateExp x1 x2,GAnnotateExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAppExp x1 x2,GAppExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GBothAndExp x1 x2,GBothAndExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCoercionExp x1 x2,GCoercionExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCoreAbsExp x1 x2,GCoreAbsExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GEitherOrExp x1 x2,GEitherOrExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GEnumSetExp x1,GEnumSetExp y1) -> and [ x1 == y1 ]
    (GEveryIdentKindExp x1 x2,GEveryIdentKindExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GEveryKindExp x1,GEveryKindExp y1) -> and [ x1 == y1 ]
    (GFun2Exp x1 x2 x3,GFun2Exp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GFunCCollExp x1 x2,GFunCCollExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFunCExp x1 x2 x3,GFunCExp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GFunExp x1 x2,GFunExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIndefIdentKindExp x1 x2,GIndefIdentKindExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIndefKindExp x1,GIndefKindExp y1) -> and [ x1 == y1 ]
    (GIndexedTermExp x1,GIndexedTermExp y1) -> and [ x1 == y1 ]
    (GIntegralExp x1 x2 x3 x4,GIntegralExp y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GKindExp x1,GKindExp y1) -> and [ x1 == y1 ]
    (GNameExp x1,GNameExp y1) -> and [ x1 == y1 ]
    (GNoIdentsKindExp x1 x2,GNoIdentsKindExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNoKindExp x1,GNoKindExp y1) -> and [ x1 == y1 ]
    (GOrExp x1,GOrExp y1) -> and [ x1 == y1 ]
    (GSeriesExp x1 x2 x3,GSeriesExp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GSigmaExp x1 x2 x3 x4,GSigmaExp y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GSomeIdentsKindExp x1 x2,GSomeIdentsKindExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSomeKindExp x1,GSomeKindExp y1) -> and [ x1 == y1 ]
    (GTermExp x1,GTermExp y1) -> and [ x1 == y1 ]
    (GTypedExp x1 x2,GTypedExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GManyExps x1,GManyExps y1) -> and [ x1 == y1 ]
    (GOneExps x1,GOneExps y1) -> and [ x1 == y1 ]
    (GNounPrepFam x1 x2,GNounPrepFam y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexFam x,LexFam y) -> x == y
    (GNounPrepFam2 x1 x2 x3,GNounPrepFam2 y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (LexFam2 x,LexFam2 y) -> x == y
    (GStringFilename x1,GStringFilename y1) -> and [ x1 == y1 ]
    (GApp1MacroFormula x1 x2,GApp1MacroFormula y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GApp2MacroFormula x1 x2 x3,GApp2MacroFormula y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GApp3MacroFormula x1 x2 x3 x4,GApp3MacroFormula y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GElemFormula x1 x2,GElemFormula y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GEquationFormula x1,GEquationFormula y1) -> and [ x1 == y1 ]
    (GMacroFormula x1,GMacroFormula y1) -> and [ x1 == y1 ]
    (Gmodulo_Formula x1 x2 x3,Gmodulo_Formula y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GNounPrepFun x1 x2,GNounPrepFun y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexFun x,LexFun y) -> x == y
    (GNounPrepFun2 x1 x2 x3,GNounPrepFun2 y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (LexFun2 x,LexFun2 y) -> x == y
    (GNounPrepFunC x1 x2,GNounPrepFunC y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexFunC x,LexFunC y) -> x == y
    (GDerivativeFunction x1,GDerivativeFunction y1) -> and [ x1 == y1 ]
    (GIdentFunction x1,GIdentFunction y1) -> and [ x1 == y1 ]
    (GafortioriHence,GafortioriHence) -> and [ ]
    (GaltogetherHence,GaltogetherHence) -> and [ ]
    (GhenceHence,GhenceHence) -> and [ ]
    (GinParticularHence,GinParticularHence) -> and [ ]
    (GnoHence,GnoHence) -> and [ ]
    (GthenHence,GthenHence) -> and [ ]
    (GthusHence,GthusHence) -> and [ ]
    (GweConcludeHence,GweConcludeHence) -> and [ ]
    (GBareVarHypo x1,GBareVarHypo y1) -> and [ x1 == y1 ]
    (GBareVarsHypo x1,GBareVarsHypo y1) -> and [ x1 == y1 ]
    (GIndexedLetFormulaHypo x1,GIndexedLetFormulaHypo y1) -> and [ x1 == y1 ]
    (GLetDeclarationHypo x1,GLetDeclarationHypo y1) -> and [ x1 == y1 ]
    (GLetFormulaHypo x1,GLetFormulaHypo y1) -> and [ x1 == y1 ]
    (GLocalHypo x1,GLocalHypo y1) -> and [ x1 == y1 ]
    (GPropHypo x1,GPropHypo y1) -> and [ x1 == y1 ]
    (GSupposePropHypo x1,GSupposePropHypo y1) -> and [ x1 == y1 ]
    (GVarHypo x1 x2,GVarHypo y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GVarsHypo x1 x2,GVarsHypo y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GStrIdent x1,GStrIdent y1) -> and [ x1 == y1 ]
    (GAxiomExpJmt x1 x2 x3 x4,GAxiomExpJmt y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GAxiomJmt x1 x2 x3,GAxiomJmt y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAxiomKindJmt x1 x2 x3,GAxiomKindJmt y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAxiomPropJmt x1 x2 x3,GAxiomPropJmt y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GDefExpJmt x1 x2 x3 x4 x5,GDefExpJmt y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (GDefKindJmt x1 x2 x3 x4,GDefKindJmt y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GDefPropJmt x1 x2 x3 x4,GDefPropJmt y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GDefUntypedExpJmt x1 x2 x3,GDefUntypedExpJmt y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GDefinedAdjJmt x1 x2 x3 x4 x5,GDefinedAdjJmt y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (GRewriteJmt x1,GRewriteJmt y1) -> and [ x1 == y1 ]
    (GThmJmt x1 x2 x3 x4,GThmJmt y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GUnitJmt x1,GUnitJmt y1) -> and [ x1 == y1 ]
    (GWeDefineAdjJmt x1 x2 x3 x4 x5,GWeDefineAdjJmt y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (GAdjKind x1 x2,GAdjKind y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAnnotateKind x1 x2,GAnnotateKind y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAppKind x1 x2,GAppKind y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GElemKind x1,GElemKind y1) -> and [ x1 == y1 ]
    (GFam2Kind x1 x2 x3,GFam2Kind y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GFamKind x1 x2,GFamKind y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFunKind x1 x2,GFunKind y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIdentKind x1,GIdentKind y1) -> and [ x1 == y1 ]
    (GNounKind x1,GNounKind y1) -> and [ x1 == y1 ]
    (GSuchThatKind x1 x2 x3,GSuchThatKind y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GTermKind x1,GTermKind y1) -> and [ x1 == y1 ]
    (GA_KindArgument,GA_KindArgument) -> and [ ]
    (GB_KindArgument,GB_KindArgument) -> and [ ]
    (GDefNounLabel x1,GDefNounLabel y1) -> and [ x1 == y1 ]
    (GIdentLabel x1,GIdentLabel y1) -> and [ x1 == y1 ]
    (GNounIdentLabel x1 x2,GNounIdentLabel y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNounIntLabel x1 x2,GNounIntLabel y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNounLabel x1,GNounLabel y1) -> and [ x1 == y1 ]
    (GNounOfNounLabel x1 x2,GNounOfNounLabel y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GProperNameNounLabel x1 x2,GProperNameNounLabel y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GcrefLabel x1,GcrefLabel y1) -> and [ x1 == y1 ]
    (LexLabel x,LexLabel y) -> x == y
    (GListAdj x1,GListAdj y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListArgKind x1,GListArgKind y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListExp x1,GListExp y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListHypo x1,GListHypo y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListIdent x1,GListIdent y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListProof x1,GListProof y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListProp x1,GListProp y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListRule x1,GListRule y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListTerm x1,GListTerm y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListUnit x1,GListUnit y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GBareLetLocal x1 x2,GBareLetLocal y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GLetLocal x1 x2 x3,GLetLocal y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GStringMacro x1,GStringMacro y1) -> and [ x1 == y1 ]
    (GStringMethod x1,GStringMethod y1) -> and [ x1 == y1 ]
    (GDefNounName x1,GDefNounName y1) -> and [ x1 == y1 ]
    (GProperNameNounName x1 x2,GProperNameNounName y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexName x,LexName y) -> x == y
    (GAdjNounNoun x1 x2,GAdjNounNoun y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNounNounNoun x1 x2,GNounNounNoun y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GProperNameNounNoun x1 x2,GProperNameNounNoun y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexNoun x,LexNoun y) -> x == y
    (GNounNoun1 x1,GNounNoun1 y1) -> and [ x1 == y1 ]
    (LexNoun1 x,LexNoun1 y) -> x == y
    (GNounPrepNoun2 x1 x2,GNounPrepNoun2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexNoun2 x,LexNoun2 y) -> x == y
    (GNounNounC x1,GNounNounC y1) -> and [ x1 == y1 ]
    (LexNounC x,LexNounC y) -> x == y
    (LexOper x,LexOper y) -> x == y
    (LexOper2 x,LexOper2 y) -> x == y
    (LexPrep x,LexPrep y) -> x == y
    (GAbsProof x1 x2,GAbsProof y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAnnotateProof x1 x2,GAnnotateProof y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAppProof x1 x2,GAppProof y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNatIndProof x1 x2 x3 x4 x5 x6,GNatIndProof y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (GUnitsProof x1,GUnitsProof y1) -> and [ x1 == y1 ]
    (GandElProof x1 x2 x3,GandElProof y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GandErProof x1 x2 x3,GandErProof y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GandIProof x1 x2 x3 x4,GandIProof y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GevenSuccProof x1 x2,GevenSuccProof y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GevenZeroProof,GevenZeroProof) -> and [ ]
    (GexistsEProof x1 x2 x3 x4 x5 x6 x7 x8,GexistsEProof y1 y2 y3 y4 y5 y6 y7 y8) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 , x7 == y7 , x8 == y8 ]
    (GexistsIProof x1 x2 x3 x4 x5,GexistsIProof y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (GfalseEProof x1 x2,GfalseEProof y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GforallEProof x1 x2 x3 x4 x5,GforallEProof y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (GforallIProof x1 x2 x3 x4 x5,GforallIProof y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (GhypoProof x1 x2,GhypoProof y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GifEProof x1 x2 x3 x4,GifEProof y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GifIProof x1 x2 x3 x4,GifIProof y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GoddSuccProof x1 x2,GoddSuccProof y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GorEProof x1 x2 x3 x4 x5 x6 x7 x8,GorEProof y1 y2 y3 y4 y5 y6 y7 y8) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 , x7 == y7 , x8 == y8 ]
    (GorIlProof x1 x2 x3,GorIlProof y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GorIrProof x1 x2 x3,GorIrProof y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GreflProof x1 x2,GreflProof y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAbsProofExp x1 x2,GAbsProofExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAnnotateProofExp x1 x2,GAnnotateProofExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAppProofExp x1 x2,GAppProofExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GLabelProofExp x1,GLabelProofExp y1) -> and [ x1 == y1 ]
    (GAdj2Prop x1 x2 x3,GAdj2Prop y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAdj3Prop x1 x2 x3 x4,GAdj3Prop y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GAdjCCollProp x1 x2,GAdjCCollProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAdjCProp x1 x2 x3,GAdjCProp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAdjECollProp x1 x2,GAdjECollProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAdjEProp x1 x2 x3,GAdjEProp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAdjProp x1 x2,GAdjProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAllProp x1 x2,GAllProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAndProp x1,GAndProp y1) -> and [ x1 == y1 ]
    (GAnnotateProp x1 x2,GAnnotateProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAppProp x1 x2,GAppProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GBothAndProp x1 x2,GBothAndProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCoreAllProp x1 x2 x3,GCoreAllProp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GCoreAndProp x1 x2,GCoreAndProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCoreExistProp x1 x2 x3,GCoreExistProp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GCoreIfProp x1 x2,GCoreIfProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCoreIffProp x1 x2,GCoreIffProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCoreNotProp x1,GCoreNotProp y1) -> and [ x1 == y1 ]
    (GCoreOrProp x1 x2,GCoreOrProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GDisplayFormulaProp x1,GDisplayFormulaProp y1) -> and [ x1 == y1 ]
    (GEitherOrProp x1 x2,GEitherOrProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GExistNoProp x1 x2,GExistNoProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GExistProp x1 x2,GExistProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFalseProp,GFalseProp) -> and [ ]
    (GFormulaImpliesProp x1 x2,GFormulaImpliesProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFormulaProp x1,GFormulaProp y1) -> and [ x1 == y1 ]
    (GIdentProp x1,GIdentProp y1) -> and [ x1 == y1 ]
    (GIfProp x1 x2,GIfProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIffIffProp x1 x2,GIffIffProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIffProp x1 x2,GIffProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIndexedFormulaProp x1,GIndexedFormulaProp y1) -> and [ x1 == y1 ]
    (GKindProp x1 x2,GKindProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNoArticleExistProp x1 x2,GNoArticleExistProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNoCommaAllProp x1 x2,GNoCommaAllProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNoCommaExistProp x1 x2,GNoCommaExistProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNotAdj2Prop x1 x2 x3,GNotAdj2Prop y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GNotAdjCProp x1 x2,GNotAdjCProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNotAdjEProp x1 x2,GNotAdjEProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNotAdjProp x1 x2,GNotAdjProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNotNoun1Prop x1 x2,GNotNoun1Prop y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNotNoun2Prop x1 x2 x3,GNotNoun2Prop y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GNotNounCProp x1 x2,GNotNounCProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNotVerb2Prop x1 x2 x3,GNotVerb2Prop y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GNotVerbCProp x1 x2,GNotVerbCProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNotVerbProp x1 x2,GNotVerbProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNoun1Prop x1 x2,GNoun1Prop y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNoun2Prop x1 x2 x3,GNoun2Prop y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GNounCProp x1 x2 x3,GNounCProp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GOnlyIfProp x1 x2,GOnlyIfProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GOrProp x1,GOrProp y1) -> and [ x1 == y1 ]
    (GPostQuantProp x1 x2,GPostQuantProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GProofProp x1,GProofProp y1) -> and [ x1 == y1 ]
    (GVerb2Prop x1 x2 x3,GVerb2Prop y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GVerbCProp x1 x2 x3,GVerbCProp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GVerbProp x1 x2,GVerbProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GWeHaveProp x1,GWeHaveProp y1) -> and [ x1 == y1 ]
    (GsameParityProp x1 x2,GsameParityProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAccor_ProperName,GAccor_ProperName) -> and [ ]
    (GAckermann_ProperName,GAckermann_ProperName) -> and [ ]
    (GAdams_ProperName,GAdams_ProperName) -> and [ ]
    (GAdinkra_ProperName,GAdinkra_ProperName) -> and [ ]
    (GAhlfors_ProperName,GAhlfors_ProperName) -> and [ ]
    (GAlbert_ProperName,GAlbert_ProperName) -> and [ ]
    (GAlexander_ProperName,GAlexander_ProperName) -> and [ ]
    (GAlexandroff_ProperName,GAlexandroff_ProperName) -> and [ ]
    (GAlexandrov_ProperName,GAlexandrov_ProperName) -> and [ ]
    (GAndrásfai_ProperName,GAndrásfai_ProperName) -> and [ ]
    (GAnosov_ProperName,GAnosov_ProperName) -> and [ ]
    (GArf_ProperName,GArf_ProperName) -> and [ ]
    (GAronszajn_ProperName,GAronszajn_ProperName) -> and [ ]
    (GArtin_ProperName,GArtin_ProperName) -> and [ ]
    (GAschbacher_ProperName,GAschbacher_ProperName) -> and [ ]
    (GAshtekar_ProperName,GAshtekar_ProperName) -> and [ ]
    (GAtiyah_ProperName,GAtiyah_ProperName) -> and [ ]
    (GAzumaya_ProperName,GAzumaya_ProperName) -> and [ ]
    (GBaer_ProperName,GBaer_ProperName) -> and [ ]
    (GBanach_ProperName,GBanach_ProperName) -> and [ ]
    (GBarnes_ProperName,GBarnes_ProperName) -> and [ ]
    (GBayes_ProperName,GBayes_ProperName) -> and [ ]
    (GBeltrami_ProperName,GBeltrami_ProperName) -> and [ ]
    (GBendixson_ProperName,GBendixson_ProperName) -> and [ ]
    (GBenini_ProperName,GBenini_ProperName) -> and [ ]
    (GBenktander_ProperName,GBenktander_ProperName) -> and [ ]
    (GBerge_ProperName,GBerge_ProperName) -> and [ ]
    (GBerkeley_ProperName,GBerkeley_ProperName) -> and [ ]
    (GBernoulli_ProperName,GBernoulli_ProperName) -> and [ ]
    (GBesov_ProperName,GBesov_ProperName) -> and [ ]
    (GBhattacharyya_ProperName,GBhattacharyya_ProperName) -> and [ ]
    (GBianchi_ProperName,GBianchi_ProperName) -> and [ ]
    (GBidiakis_ProperName,GBidiakis_ProperName) -> and [ ]
    (GBirkhoff_ProperName,GBirkhoff_ProperName) -> and [ ]
    (GBlanuša_ProperName,GBlanuša_ProperName) -> and [ ]
    (GBlum_ProperName,GBlum_ProperName) -> and [ ]
    (GBochner_ProperName,GBochner_ProperName) -> and [ ]
    (GBockstein_ProperName,GBockstein_ProperName) -> and [ ]
    (GBorel_ProperName,GBorel_ProperName) -> and [ ]
    (GBott_ProperName,GBott_ProperName) -> and [ ]
    (GBrandt_ProperName,GBrandt_ProperName) -> and [ ]
    (GBrauer_ProperName,GBrauer_ProperName) -> and [ ]
    (GBrauner_ProperName,GBrauner_ProperName) -> and [ ]
    (GBregman_ProperName,GBregman_ProperName) -> and [ ]
    (GBrinkmann_ProperName,GBrinkmann_ProperName) -> and [ ]
    (GBruhat_ProperName,GBruhat_ProperName) -> and [ ]
    (GBruijn_ProperName,GBruijn_ProperName) -> and [ ]
    (GBurnside_ProperName,GBurnside_ProperName) -> and [ ]
    (GCalkin_ProperName,GCalkin_ProperName) -> and [ ]
    (GCamden_ProperName,GCamden_ProperName) -> and [ ]
    (GCantor_ProperName,GCantor_ProperName) -> and [ ]
    (GCarleson_ProperName,GCarleson_ProperName) -> and [ ]
    (GCarlyle_ProperName,GCarlyle_ProperName) -> and [ ]
    (GCarmichael_ProperName,GCarmichael_ProperName) -> and [ ]
    (GCarnot_ProperName,GCarnot_ProperName) -> and [ ]
    (GCarol_ProperName,GCarol_ProperName) -> and [ ]
    (GCartan_ProperName,GCartan_ProperName) -> and [ ]
    (GCarter_ProperName,GCarter_ProperName) -> and [ ]
    (GCartier_ProperName,GCartier_ProperName) -> and [ ]
    (GCauchy_ProperName,GCauchy_ProperName) -> and [ ]
    (GCayley_ProperName,GCayley_ProperName) -> and [ ]
    (GChang_ProperName,GChang_ProperName) -> and [ ]
    (GChebyshev_ProperName,GChebyshev_ProperName) -> and [ ]
    (GCheeger_ProperName,GCheeger_ProperName) -> and [ ]
    (GChevalley_ProperName,GChevalley_ProperName) -> and [ ]
    (GClebsch_ProperName,GClebsch_ProperName) -> and [ ]
    (GClifford_ProperName,GClifford_ProperName) -> and [ ]
    (GCoates_ProperName,GCoates_ProperName) -> and [ ]
    (GCohen_ProperName,GCohen_ProperName) -> and [ ]
    (GColin_ProperName,GColin_ProperName) -> and [ ]
    (GColinear_ProperName,GColinear_ProperName) -> and [ ]
    (GColombeau_ProperName,GColombeau_ProperName) -> and [ ]
    (GCox_ProperName,GCox_ProperName) -> and [ ]
    (GCoxeter_ProperName,GCoxeter_ProperName) -> and [ ]
    (GCremona_ProperName,GCremona_ProperName) -> and [ ]
    (GCullen_ProperName,GCullen_ProperName) -> and [ ]
    (GCunningham_ProperName,GCunningham_ProperName) -> and [ ]
    (GDade_ProperName,GDade_ProperName) -> and [ ]
    (GDamm_ProperName,GDamm_ProperName) -> and [ ]
    (GDarboux_ProperName,GDarboux_ProperName) -> and [ ]
    (GDe_ProperName,GDe_ProperName) -> and [ ]
    (GDedekind_ProperName,GDedekind_ProperName) -> and [ ]
    (GDehn_ProperName,GDehn_ProperName) -> and [ ]
    (GDelambre_ProperName,GDelambre_ProperName) -> and [ ]
    (GDeligne_ProperName,GDeligne_ProperName) -> and [ ]
    (GDelta_ProperName,GDelta_ProperName) -> and [ ]
    (GDescartes_ProperName,GDescartes_ProperName) -> and [ ]
    (GDieudonné_ProperName,GDieudonné_ProperName) -> and [ ]
    (GDini_ProperName,GDini_ProperName) -> and [ ]
    (GDirac_ProperName,GDirac_ProperName) -> and [ ]
    (GDirichlet_ProperName,GDirichlet_ProperName) -> and [ ]
    (GDolbeault_ProperName,GDolbeault_ProperName) -> and [ ]
    (GDrinfeld_ProperName,GDrinfeld_ProperName) -> and [ ]
    (GDuflo_ProperName,GDuflo_ProperName) -> and [ ]
    (GDunkl_ProperName,GDunkl_ProperName) -> and [ ]
    (GDunn_ProperName,GDunn_ProperName) -> and [ ]
    (GDyck_ProperName,GDyck_ProperName) -> and [ ]
    (GDynkin_ProperName,GDynkin_ProperName) -> and [ ]
    (GDürer_ProperName,GDürer_ProperName) -> and [ ]
    (GEarth_ProperName,GEarth_ProperName) -> and [ ]
    (GEberlein_ProperName,GEberlein_ProperName) -> and [ ]
    (GErdős_ProperName,GErdős_ProperName) -> and [ ]
    (GErrera_ProperName,GErrera_ProperName) -> and [ ]
    (GEuler_ProperName,GEuler_ProperName) -> and [ ]
    (GFatou_ProperName,GFatou_ProperName) -> and [ ]
    (GFeigenbaum_ProperName,GFeigenbaum_ProperName) -> and [ ]
    (GFejér_ProperName,GFejér_ProperName) -> and [ ]
    (GFermat_ProperName,GFermat_ProperName) -> and [ ]
    (GFeynman_ProperName,GFeynman_ProperName) -> and [ ]
    (GFibonacci_ProperName,GFibonacci_ProperName) -> and [ ]
    (GFock_ProperName,GFock_ProperName) -> and [ ]
    (GFolkman_ProperName,GFolkman_ProperName) -> and [ ]
    (GFoster_ProperName,GFoster_ProperName) -> and [ ]
    (GFranklin_ProperName,GFranklin_ProperName) -> and [ ]
    (GFrattini_ProperName,GFrattini_ProperName) -> and [ ]
    (GFraïssé_ProperName,GFraïssé_ProperName) -> and [ ]
    (GFredholm_ProperName,GFredholm_ProperName) -> and [ ]
    (GFriedman_ProperName,GFriedman_ProperName) -> and [ ]
    (GFrobenius_ProperName,GFrobenius_ProperName) -> and [ ]
    (GFréchet_ProperName,GFréchet_ProperName) -> and [ ]
    (GFrölicher_ProperName,GFrölicher_ProperName) -> and [ ]
    (GGalerkin_ProperName,GGalerkin_ProperName) -> and [ ]
    (GGalois_ProperName,GGalois_ProperName) -> and [ ]
    (GGerstenhaber_ProperName,GGerstenhaber_ProperName) -> and [ ]
    (GGewirtz_ProperName,GGewirtz_ProperName) -> and [ ]
    (GGibbs_ProperName,GGibbs_ProperName) -> and [ ]
    (GGibrat_ProperName,GGibrat_ProperName) -> and [ ]
    (GGiuga_ProperName,GGiuga_ProperName) -> and [ ]
    (GGolomb_ProperName,GGolomb_ProperName) -> and [ ]
    (GGosset_ProperName,GGosset_ProperName) -> and [ ]
    (GGoursat_ProperName,GGoursat_ProperName) -> and [ ]
    (GGrassmann_ProperName,GGrassmann_ProperName) -> and [ ]
    (GGreen_ProperName,GGreen_ProperName) -> and [ ]
    (GGrinberg_ProperName,GGrinberg_ProperName) -> and [ ]
    (GGromov_ProperName,GGromov_ProperName) -> and [ ]
    (GGrothendieck_ProperName,GGrothendieck_ProperName) -> and [ ]
    (GGrötzsch_ProperName,GGrötzsch_ProperName) -> and [ ]
    (GHadwiger_ProperName,GHadwiger_ProperName) -> and [ ]
    (GHalin_ProperName,GHalin_ProperName) -> and [ ]
    (GHall_ProperName,GHall_ProperName) -> and [ ]
    (GHamel_ProperName,GHamel_ProperName) -> and [ ]
    (GHanoi_ProperName,GHanoi_ProperName) -> and [ ]
    (GHardy_ProperName,GHardy_ProperName) -> and [ ]
    (GHarries_ProperName,GHarries_ProperName) -> and [ ]
    (GHarrop_ProperName,GHarrop_ProperName) -> and [ ]
    (GHart_ProperName,GHart_ProperName) -> and [ ]
    (GHatzel_ProperName,GHatzel_ProperName) -> and [ ]
    (GHausdorff_ProperName,GHausdorff_ProperName) -> and [ ]
    (GHawkes_ProperName,GHawkes_ProperName) -> and [ ]
    (GHeawood_ProperName,GHeawood_ProperName) -> and [ ]
    (GHecke_ProperName,GHecke_ProperName) -> and [ ]
    (GHeisenberg_ProperName,GHeisenberg_ProperName) -> and [ ]
    (GHellinger_ProperName,GHellinger_ProperName) -> and [ ]
    (GHenson_ProperName,GHenson_ProperName) -> and [ ]
    (GHerbrand_ProperName,GHerbrand_ProperName) -> and [ ]
    (GHerschel_ProperName,GHerschel_ProperName) -> and [ ]
    (GHeyting_ProperName,GHeyting_ProperName) -> and [ ]
    (GHilbert_ProperName,GHilbert_ProperName) -> and [ ]
    (GHill_ProperName,GHill_ProperName) -> and [ ]
    (GHochschild_ProperName,GHochschild_ProperName) -> and [ ]
    (GHodge_ProperName,GHodge_ProperName) -> and [ ]
    (GHoffman_ProperName,GHoffman_ProperName) -> and [ ]
    (GHolt_ProperName,GHolt_ProperName) -> and [ ]
    (GHosoya_ProperName,GHosoya_ProperName) -> and [ ]
    (GHurwitz_ProperName,GHurwitz_ProperName) -> and [ ]
    (GIkeda_ProperName,GIkeda_ProperName) -> and [ ]
    (GIwasawa_ProperName,GIwasawa_ProperName) -> and [ ]
    (GJaccard_ProperName,GJaccard_ProperName) -> and [ ]
    (GJacobi_ProperName,GJacobi_ProperName) -> and [ ]
    (GJacobson_ProperName,GJacobson_ProperName) -> and [ ]
    (GJaffard_ProperName,GJaffard_ProperName) -> and [ ]
    (GJohnson_ProperName,GJohnson_ProperName) -> and [ ]
    (GJordan_ProperName,GJordan_ProperName) -> and [ ]
    (GJulia_ProperName,GJulia_ProperName) -> and [ ]
    (GJónsson_ProperName,GJónsson_ProperName) -> and [ ]
    (GKakeya_ProperName,GKakeya_ProperName) -> and [ ]
    (GKalman_ProperName,GKalman_ProperName) -> and [ ]
    (GKan_ProperName,GKan_ProperName) -> and [ ]
    (GKane_ProperName,GKane_ProperName) -> and [ ]
    (GKaprekar_ProperName,GKaprekar_ProperName) -> and [ ]
    (GKaroubi_ProperName,GKaroubi_ProperName) -> and [ ]
    (GKasch_ProperName,GKasch_ProperName) -> and [ ]
    (GKeith_ProperName,GKeith_ProperName) -> and [ ]
    (GKempe_ProperName,GKempe_ProperName) -> and [ ]
    (GKent_ProperName,GKent_ProperName) -> and [ ]
    (GKhinchin_ProperName,GKhinchin_ProperName) -> and [ ]
    (GKittell_ProperName,GKittell_ProperName) -> and [ ]
    (GKleene_ProperName,GKleene_ProperName) -> and [ ]
    (GKlein_ProperName,GKlein_ProperName) -> and [ ]
    (GKleisli_ProperName,GKleisli_ProperName) -> and [ ]
    (GKneser_ProperName,GKneser_ProperName) -> and [ ]
    (GKnödel_ProperName,GKnödel_ProperName) -> and [ ]
    (GKodaira_ProperName,GKodaira_ProperName) -> and [ ]
    (GKolmogorov_ProperName,GKolmogorov_ProperName) -> and [ ]
    (GKorovkin_ProperName,GKorovkin_ProperName) -> and [ ]
    (GKoszul_ProperName,GKoszul_ProperName) -> and [ ]
    (GKrein_ProperName,GKrein_ProperName) -> and [ ]
    (GKripke_ProperName,GKripke_ProperName) -> and [ ]
    (GKronecker_ProperName,GKronecker_ProperName) -> and [ ]
    (GKrylov_ProperName,GKrylov_ProperName) -> and [ ]
    (GKummer_ProperName,GKummer_ProperName) -> and [ ]
    (GKynea_ProperName,GKynea_ProperName) -> and [ ]
    (GKünneth_ProperName,GKünneth_ProperName) -> and [ ]
    (GLagrange_ProperName,GLagrange_ProperName) -> and [ ]
    (GLaue_ProperName,GLaue_ProperName) -> and [ ]
    (GLawvere_ProperName,GLawvere_ProperName) -> and [ ]
    (GLebesgue_ProperName,GLebesgue_ProperName) -> and [ ]
    (GLegendre_ProperName,GLegendre_ProperName) -> and [ ]
    (GLeibniz_ProperName,GLeibniz_ProperName) -> and [ ]
    (GLevenshtein_ProperName,GLevenshtein_ProperName) -> and [ ]
    (GLevi_ProperName,GLevi_ProperName) -> and [ ]
    (GLeyland_ProperName,GLeyland_ProperName) -> and [ ]
    (GLie_ProperName,GLie_ProperName) -> and [ ]
    (GLindelöf_ProperName,GLindelöf_ProperName) -> and [ ]
    (GLipschitz_ProperName,GLipschitz_ProperName) -> and [ ]
    (GLissajous_ProperName,GLissajous_ProperName) -> and [ ]
    (GLiverTox_ProperName,GLiverTox_ProperName) -> and [ ]
    (GLjubljana_ProperName,GLjubljana_ProperName) -> and [ ]
    (GLoewner_ProperName,GLoewner_ProperName) -> and [ ]
    (GLondon_ProperName,GLondon_ProperName) -> and [ ]
    (GLorentz_ProperName,GLorentz_ProperName) -> and [ ]
    (GLoupekine_ProperName,GLoupekine_ProperName) -> and [ ]
    (GLovász_ProperName,GLovász_ProperName) -> and [ ]
    (GLucas_ProperName,GLucas_ProperName) -> and [ ]
    (GLyapunov_ProperName,GLyapunov_ProperName) -> and [ ]
    (GMacdonald_ProperName,GMacdonald_ProperName) -> and [ ]
    (GMahalanobis_ProperName,GMahalanobis_ProperName) -> and [ ]
    (GMahler_ProperName,GMahler_ProperName) -> and [ ]
    (GMandelbrot_ProperName,GMandelbrot_ProperName) -> and [ ]
    (GManin_ProperName,GManin_ProperName) -> and [ ]
    (GMarkov_ProperName,GMarkov_ProperName) -> and [ ]
    (GMazur_ProperName,GMazur_ProperName) -> and [ ]
    (GMcGee_ProperName,GMcGee_ProperName) -> and [ ]
    (GMcKay_ProperName,GMcKay_ProperName) -> and [ ]
    (GMcLaughlin_ProperName,GMcLaughlin_ProperName) -> and [ ]
    (GMeredith_ProperName,GMeredith_ProperName) -> and [ ]
    (GMeringer_ProperName,GMeringer_ProperName) -> and [ ]
    (GMersenne_ProperName,GMersenne_ProperName) -> and [ ]
    (GMeyniel_ProperName,GMeyniel_ProperName) -> and [ ]
    (GMiller_ProperName,GMiller_ProperName) -> and [ ]
    (GMilnor_ProperName,GMilnor_ProperName) -> and [ ]
    (GMinkowski_ProperName,GMinkowski_ProperName) -> and [ ]
    (GMontel_ProperName,GMontel_ProperName) -> and [ ]
    (GMoore_ProperName,GMoore_ProperName) -> and [ ]
    (GMorgan_ProperName,GMorgan_ProperName) -> and [ ]
    (GMorse_ProperName,GMorse_ProperName) -> and [ ]
    (GMoser_ProperName,GMoser_ProperName) -> and [ ]
    (GMotzkin_ProperName,GMotzkin_ProperName) -> and [ ]
    (GMumford_ProperName,GMumford_ProperName) -> and [ ]
    (GMunn_ProperName,GMunn_ProperName) -> and [ ]
    (GMöbius_ProperName,GMöbius_ProperName) -> and [ ]
    (GMünchhausen_ProperName,GMünchhausen_ProperName) -> and [ ]
    (GNagata_ProperName,GNagata_ProperName) -> and [ ]
    (GNakajima_ProperName,GNakajima_ProperName) -> and [ ]
    (GNarayana_ProperName,GNarayana_ProperName) -> and [ ]
    (GNeumann_ProperName,GNeumann_ProperName) -> and [ ]
    (GNewton_ProperName,GNewton_ProperName) -> and [ ]
    (GOckham_ProperName,GOckham_ProperName) -> and [ ]
    (GPaley_ProperName,GPaley_ProperName) -> and [ ]
    (GPareto_ProperName,GPareto_ProperName) -> and [ ]
    (GParrondo_ProperName,GParrondo_ProperName) -> and [ ]
    (GPatricia_ProperName,GPatricia_ProperName) -> and [ ]
    (GPeano_ProperName,GPeano_ProperName) -> and [ ]
    (GPearcey_ProperName,GPearcey_ProperName) -> and [ ]
    (GPenrose_ProperName,GPenrose_ProperName) -> and [ ]
    (GPerkel_ProperName,GPerkel_ProperName) -> and [ ]
    (GPerrin_ProperName,GPerrin_ProperName) -> and [ ]
    (GPetersen_ProperName,GPetersen_ProperName) -> and [ ]
    (GPetri_ProperName,GPetri_ProperName) -> and [ ]
    (GPettis_ProperName,GPettis_ProperName) -> and [ ]
    (GPicard_ProperName,GPicard_ProperName) -> and [ ]
    (GPippard_ProperName,GPippard_ProperName) -> and [ ]
    (GPoincaré_ProperName,GPoincaré_ProperName) -> and [ ]
    (GPoisson_ProperName,GPoisson_ProperName) -> and [ ]
    (GPoussin_ProperName,GPoussin_ProperName) -> and [ ]
    (GPtolemy_ProperName,GPtolemy_ProperName) -> and [ ]
    (GPuig_ProperName,GPuig_ProperName) -> and [ ]
    (GRado_ProperName,GRado_ProperName) -> and [ ]
    (GRadon_ProperName,GRadon_ProperName) -> and [ ]
    (GRajchman_ProperName,GRajchman_ProperName) -> and [ ]
    (GRamanujan_ProperName,GRamanujan_ProperName) -> and [ ]
    (GRees_ProperName,GRees_ProperName) -> and [ ]
    (GReeve_ProperName,GReeve_ProperName) -> and [ ]
    (GReinhardt_ProperName,GReinhardt_ProperName) -> and [ ]
    (GRham_ProperName,GRham_ProperName) -> and [ ]
    (GRiemann_ProperName,GRiemann_ProperName) -> and [ ]
    (GRiesel_ProperName,GRiesel_ProperName) -> and [ ]
    (GRiesz_ProperName,GRiesz_ProperName) -> and [ ]
    (GRobbins_ProperName,GRobbins_ProperName) -> and [ ]
    (GRobertson_ProperName,GRobertson_ProperName) -> and [ ]
    (GRosenbrock_ProperName,GRosenbrock_ProperName) -> and [ ]
    (GRoyle_ProperName,GRoyle_ProperName) -> and [ ]
    (GSchauder_ProperName,GSchauder_ProperName) -> and [ ]
    (GSchläfli_ProperName,GSchläfli_ProperName) -> and [ ]
    (GSchreier_ProperName,GSchreier_ProperName) -> and [ ]
    (GSchrödinger_ProperName,GSchrödinger_ProperName) -> and [ ]
    (GSchubert_ProperName,GSchubert_ProperName) -> and [ ]
    (GSchur_ProperName,GSchur_ProperName) -> and [ ]
    (GSchwartz_ProperName,GSchwartz_ProperName) -> and [ ]
    (GSchwarz_ProperName,GSchwarz_ProperName) -> and [ ]
    (GScott_ProperName,GScott_ProperName) -> and [ ]
    (GSelberg_ProperName,GSelberg_ProperName) -> and [ ]
    (GSerre_ProperName,GSerre_ProperName) -> and [ ]
    (GShannon_ProperName,GShannon_ProperName) -> and [ ]
    (GShelah_ProperName,GShelah_ProperName) -> and [ ]
    (GShimura_ProperName,GShimura_ProperName) -> and [ ]
    (GShrikhande_ProperName,GShrikhande_ProperName) -> and [ ]
    (GSiegel_ProperName,GSiegel_ProperName) -> and [ ]
    (GSierpinski_ProperName,GSierpinski_ProperName) -> and [ ]
    (GSitter_ProperName,GSitter_ProperName) -> and [ ]
    (GSlater_ProperName,GSlater_ProperName) -> and [ ]
    (GSmith_ProperName,GSmith_ProperName) -> and [ ]
    (GSousselier_ProperName,GSousselier_ProperName) -> and [ ]
    (GSpencer_ProperName,GSpencer_ProperName) -> and [ ]
    (GStark_ProperName,GStark_ProperName) -> and [ ]
    (GSteenrod_ProperName,GSteenrod_ProperName) -> and [ ]
    (GSteinitz_ProperName,GSteinitz_ProperName) -> and [ ]
    (GStepanoff_ProperName,GStepanoff_ProperName) -> and [ ]
    (GStiefel_ProperName,GStiefel_ProperName) -> and [ ]
    (GStirling_ProperName,GStirling_ProperName) -> and [ ]
    (GStone_ProperName,GStone_ProperName) -> and [ ]
    (GSuslin_ProperName,GSuslin_ProperName) -> and [ ]
    (GSuzuki_ProperName,GSuzuki_ProperName) -> and [ ]
    (GSylow_ProperName,GSylow_ProperName) -> and [ ]
    (GSylvester_ProperName,GSylvester_ProperName) -> and [ ]
    (GSzekeres_ProperName,GSzekeres_ProperName) -> and [ ]
    (GSárközy_ProperName,GSárközy_ProperName) -> and [ ]
    (GSørensen_ProperName,GSørensen_ProperName) -> and [ ]
    (GTaleb_ProperName,GTaleb_ProperName) -> and [ ]
    (GTarski_ProperName,GTarski_ProperName) -> and [ ]
    (GTate_ProperName,GTate_ProperName) -> and [ ]
    (GTaylor_ProperName,GTaylor_ProperName) -> and [ ]
    (GThabit_ProperName,GThabit_ProperName) -> and [ ]
    (GThom_ProperName,GThom_ProperName) -> and [ ]
    (GThomassen_ProperName,GThomassen_ProperName) -> and [ ]
    (GThompson_ProperName,GThompson_ProperName) -> and [ ]
    (GThue_ProperName,GThue_ProperName) -> and [ ]
    (GTime_ProperName,GTime_ProperName) -> and [ ]
    (GTrofimov_ProperName,GTrofimov_ProperName) -> and [ ]
    (GTrémaux_ProperName,GTrémaux_ProperName) -> and [ ]
    (GTurán_ProperName,GTurán_ProperName) -> and [ ]
    (GTutte_ProperName,GTutte_ProperName) -> and [ ]
    (GTweedie_ProperName,GTweedie_ProperName) -> and [ ]
    (GTychonoff_ProperName,GTychonoff_ProperName) -> and [ ]
    (GUrysohn_ProperName,GUrysohn_ProperName) -> and [ ]
    (GVandermonde_ProperName,GVandermonde_ProperName) -> and [ ]
    (GVerdière_ProperName,GVerdière_ProperName) -> and [ ]
    (GVickrey_ProperName,GVickrey_ProperName) -> and [ ]
    (GWallman_ProperName,GWallman_ProperName) -> and [ ]
    (GWalther_ProperName,GWalther_ProperName) -> and [ ]
    (GWeibull_ProperName,GWeibull_ProperName) -> and [ ]
    (GWeierstrass_ProperName,GWeierstrass_ProperName) -> and [ ]
    (GWeil_ProperName,GWeil_ProperName) -> and [ ]
    (GWeingarten_ProperName,GWeingarten_ProperName) -> and [ ]
    (GWells_ProperName,GWells_ProperName) -> and [ ]
    (GWeyl_ProperName,GWeyl_ProperName) -> and [ ]
    (GWhewell_ProperName,GWhewell_ProperName) -> and [ ]
    (GWieferich_ProperName,GWieferich_ProperName) -> and [ ]
    (GWitt_ProperName,GWitt_ProperName) -> and [ ]
    (GWoodall_ProperName,GWoodall_ProperName) -> and [ ]
    (GWoodin_ProperName,GWoodin_ProperName) -> and [ ]
    (GYoneda_ProperName,GYoneda_ProperName) -> and [ ]
    (GZamfirescu_ProperName,GZamfirescu_ProperName) -> and [ ]
    (GZaslavskii_ProperName,GZaslavskii_ProperName) -> and [ ]
    (GZassenhaus_ProperName,GZassenhaus_ProperName) -> and [ ]
    (GZeisel_ProperName,GZeisel_ProperName) -> and [ ]
    (GZhegalkin_ProperName,GZhegalkin_ProperName) -> and [ ]
    (GZinbiel_ProperName,GZinbiel_ProperName) -> and [ ]
    (GZorn_ProperName,GZorn_ProperName) -> and [ ]
    (GZuckerman_ProperName,GZuckerman_ProperName) -> and [ ]
    (GŁojasiewicz_ProperName,GŁojasiewicz_ProperName) -> and [ ]
    (GŁukasiewicz_ProperName,GŁukasiewicz_ProperName) -> and [ ]
    (GNoVarRewriteRule x1 x2,GNoVarRewriteRule y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRewriteRule x1 x2 x3,GRewriteRule y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GApp1MacroTerm x1 x2,GApp1MacroTerm y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GApp2MacroTerm x1 x2 x3,GApp2MacroTerm y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GApp3MacroTerm x1 x2 x3 x4,GApp3MacroTerm y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GAppFunctionTerm x1 x2,GAppFunctionTerm y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComprehensionTerm x1 x2 x3,GComprehensionTerm y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GConstTerm x1,GConstTerm y1) -> and [ x1 == y1 ]
    (GEnumSetTerm x1,GEnumSetTerm y1) -> and [ x1 == y1 ]
    (GIdentTerm x1,GIdentTerm y1) -> and [ x1 == y1 ]
    (GMacroTerm x1,GMacroTerm y1) -> and [ x1 == y1 ]
    (GNumberTerm x1,GNumberTerm y1) -> and [ x1 == y1 ]
    (GOper2Term x1 x2 x3,GOper2Term y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GOperTerm x1 x2,GOperTerm y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GParenthTerm x1,GParenthTerm y1) -> and [ x1 == y1 ]
    (GTextbfTerm x1,GTextbfTerm y1) -> and [ x1 == y1 ]
    (Gintegral_Term x1 x2 x3 x4,Gintegral_Term y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Gseries_Term x1 x2 x3,Gseries_Term y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Gsigma_Term x1 x2 x3 x4,Gsigma_Term y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Gsum3dots_Term x1 x2 x3,Gsum3dots_Term y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Gtimes_Term x1 x2,Gtimes_Term y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GStringTitle x1,GStringTitle y1) -> and [ x1 == y1 ]
    (GBeginEnvironmentUnit x1 x2,GBeginEnvironmentUnit y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GBeginProofMethodUnit x1 x2,GBeginProofMethodUnit y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCaseGoal x1 x2,GCaseGoal y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCasesGoal,GCasesGoal) -> and [ ]
    (GEndEnvironmentUnit x1,GEndEnvironmentUnit y1) -> and [ x1 == y1 ]
    (GEnoughGoal x1,GEnoughGoal y1) -> and [ x1 == y1 ]
    (GFirstVerifyGoal x1,GFirstVerifyGoal y1) -> and [ x1 == y1 ]
    (GFollowsPropConclusion x1,GFollowsPropConclusion y1) -> and [ x1 == y1 ]
    (GHyposAssumption x1,GHyposAssumption y1) -> and [ x1 == y1 ]
    (GIdentExpAssumption x1 x2,GIdentExpAssumption y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIdentKindAssumption x1 x2,GIdentKindAssumption y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GImportUnit x1,GImportUnit y1) -> and [ x1 == y1 ]
    (GInductionGoal,GInductionGoal) -> and [ ]
    (GLabelConclusion x1,GLabelConclusion y1) -> and [ x1 == y1 ]
    (GLabelUnit x1,GLabelUnit y1) -> and [ x1 == y1 ]
    (GObviousConclusion,GObviousConclusion) -> and [ ]
    (GPropAssumption x1 x2,GPropAssumption y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPropConclusion x1 x2,GPropConclusion y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPropLabelConclusion x1 x2 x3,GPropLabelConclusion y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GSectionUnit x1 x2,GSectionUnit y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSinceConclusion x1 x2,GSinceConclusion y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSinceGoal x1 x2,GSinceGoal y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSubsectionUnit x1 x2,GSubsectionUnit y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GVerbPrepNounVerb x1 x2 x3,GVerbPrepNounVerb y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (LexVerb x,LexVerb y) -> x == y
    (GVerbPrepVerb2 x1 x2,GVerbPrepVerb2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexVerb2 x,LexVerb2 y) -> x == y
    (GVerbVerbC x1,GVerbVerbC y1) -> and [ x1 == y1 ]
    (LexVerbC x,LexVerbC y) -> x == y
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GAdj where
  gf (GAdj2Adj x1 x2) = mkApp (mkCId "Adj2Adj") [gf x1, gf x2]
  gf (GAdj3Adj x1 x2 x3) = mkApp (mkCId "Adj3Adj") [gf x1, gf x2, gf x3]
  gf (GAdverbAdjAdj x1 x2) = mkApp (mkCId "AdverbAdjAdj") [gf x1, gf x2]
  gf (GAndAdj x1) = mkApp (mkCId "AndAdj") [gf x1]
  gf (GBothAndAdj x1 x2) = mkApp (mkCId "BothAndAdj") [gf x1, gf x2]
  gf (GEitherOrAdj x1 x2) = mkApp (mkCId "EitherOrAdj") [gf x1, gf x2]
  gf (GOrAdj x1) = mkApp (mkCId "OrAdj") [gf x1]
  gf (LexAdj x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "Adj2Adj" -> GAdj2Adj (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "Adj3Adj" -> GAdj3Adj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "AdverbAdjAdj" -> GAdverbAdjAdj (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "AndAdj" -> GAndAdj (fg x1)
      Just (i,[x1,x2]) | i == mkCId "BothAndAdj" -> GBothAndAdj (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "EitherOrAdj" -> GEitherOrAdj (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "OrAdj" -> GOrAdj (fg x1)

      Just (i,[]) -> LexAdj (showCId i)
      _ -> error ("no Adj " ++ show t)

instance Gf GAdj2 where
  gf (GAdjPrepAdj2 x1 x2) = mkApp (mkCId "AdjPrepAdj2") [gf x1, gf x2]
  gf (LexAdj2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjPrepAdj2" -> GAdjPrepAdj2 (fg x1) (fg x2)

      Just (i,[]) -> LexAdj2 (showCId i)
      _ -> error ("no Adj2 " ++ show t)

instance Gf GAdj3 where
  gf (GAdjPrepAdj3 x1 x2 x3) = mkApp (mkCId "AdjPrepAdj3") [gf x1, gf x2, gf x3]
  gf (LexAdj3 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "AdjPrepAdj3" -> GAdjPrepAdj3 (fg x1) (fg x2) (fg x3)

      Just (i,[]) -> LexAdj3 (showCId i)
      _ -> error ("no Adj3 " ++ show t)

instance Gf GAdjC where
  gf (GAdjAdjC x1) = mkApp (mkCId "AdjAdjC") [gf x1]
  gf (LexAdjC x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AdjAdjC" -> GAdjAdjC (fg x1)

      Just (i,[]) -> LexAdjC (showCId i)
      _ -> error ("no AdjC " ++ show t)

instance Gf GAdjE where
  gf (GAdjAdjE x1) = mkApp (mkCId "AdjAdjE") [gf x1]
  gf (LexAdjE x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AdjAdjE" -> GAdjAdjE (fg x1)

      Just (i,[]) -> LexAdjE (showCId i)
      _ -> error ("no AdjE " ++ show t)

instance Gf GAdverb where
  gf Galmost_everywhere_Adverb = mkApp (mkCId "almost_everywhere_Adverb") []
  gf Geverywhere_Adverb = mkApp (mkCId "everywhere_Adverb") []
  gf Guniformly_Adverb = mkApp (mkCId "uniformly_Adverb") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "almost_everywhere_Adverb" -> Galmost_everywhere_Adverb 
      Just (i,[]) | i == mkCId "everywhere_Adverb" -> Geverywhere_Adverb 
      Just (i,[]) | i == mkCId "uniformly_Adverb" -> Guniformly_Adverb 


      _ -> error ("no Adverb " ++ show t)

instance Gf GArgKind where
  gf (GBareIdentsArgKind x1) = mkApp (mkCId "BareIdentsArgKind") [gf x1]
  gf (GDeclarationArgKind x1) = mkApp (mkCId "DeclarationArgKind") [gf x1]
  gf (GIdentArgKind x1 x2) = mkApp (mkCId "IdentArgKind") [gf x1, gf x2]
  gf (GIdentsArgKind x1 x2) = mkApp (mkCId "IdentsArgKind") [gf x1, gf x2]
  gf (GIndexedDeclarationArgKind x1) = mkApp (mkCId "IndexedDeclarationArgKind") [gf x1]
  gf (GKindArgKind x1) = mkApp (mkCId "KindArgKind") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "BareIdentsArgKind" -> GBareIdentsArgKind (fg x1)
      Just (i,[x1]) | i == mkCId "DeclarationArgKind" -> GDeclarationArgKind (fg x1)
      Just (i,[x1,x2]) | i == mkCId "IdentArgKind" -> GIdentArgKind (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "IdentsArgKind" -> GIdentsArgKind (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "IndexedDeclarationArgKind" -> GIndexedDeclarationArgKind (fg x1)
      Just (i,[x1]) | i == mkCId "KindArgKind" -> GKindArgKind (fg x1)


      _ -> error ("no ArgKind " ++ show t)

instance Gf GArgument where
  gf GX_Argument = mkApp (mkCId "X_Argument") []
  gf GY_Argument = mkApp (mkCId "Y_Argument") []
  gf GZ_Argument = mkApp (mkCId "Z_Argument") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "X_Argument" -> GX_Argument 
      Just (i,[]) | i == mkCId "Y_Argument" -> GY_Argument 
      Just (i,[]) | i == mkCId "Z_Argument" -> GZ_Argument 


      _ -> error ("no Argument " ++ show t)

instance Gf GCompar where
  gf (LexCompar x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexCompar (showCId i)
      _ -> error ("no Compar " ++ show t)

instance Gf GConst where
  gf (LexConst x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexConst (showCId i)
      _ -> error ("no Const " ++ show t)

instance Gf GDeclaration where
  gf (GElemDeclaration x1 x2) = mkApp (mkCId "ElemDeclaration") [gf x1, gf x2]
  gf (GFunctionDeclaration x1 x2 x3) = mkApp (mkCId "FunctionDeclaration") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ElemDeclaration" -> GElemDeclaration (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "FunctionDeclaration" -> GFunctionDeclaration (fg x1) (fg x2) (fg x3)


      _ -> error ("no Declaration " ++ show t)

instance Gf GEnvironment where
  gf (LexEnvironment x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexEnvironment (showCId i)
      _ -> error ("no Environment " ++ show t)

instance Gf GEquation where
  gf (GBinaryEquation x1 x2 x3) = mkApp (mkCId "BinaryEquation") [gf x1, gf x2, gf x3]
  gf (GChainEquation x1 x2 x3) = mkApp (mkCId "ChainEquation") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "BinaryEquation" -> GBinaryEquation (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "ChainEquation" -> GChainEquation (fg x1) (fg x2) (fg x3)


      _ -> error ("no Equation " ++ show t)

instance Gf GExample where
  gf (GAdj2Example x1 x2 x3) = mkApp (mkCId "Adj2Example") [gf x1, gf x2, gf x3]
  gf (GAdj3Example x1 x2 x3 x4) = mkApp (mkCId "Adj3Example") [gf x1, gf x2, gf x3, gf x4]
  gf (GAdjCExample x1 x2 x3) = mkApp (mkCId "AdjCExample") [gf x1, gf x2, gf x3]
  gf (GAdjEExample x1 x2 x3) = mkApp (mkCId "AdjEExample") [gf x1, gf x2, gf x3]
  gf (GAdjExample x1 x2) = mkApp (mkCId "AdjExample") [gf x1, gf x2]
  gf (GFam2Example x1 x2 x3) = mkApp (mkCId "Fam2Example") [gf x1, gf x2, gf x3]
  gf (GFamExample x1 x2) = mkApp (mkCId "FamExample") [gf x1, gf x2]
  gf (GFun2Example x1 x2 x3) = mkApp (mkCId "Fun2Example") [gf x1, gf x2, gf x3]
  gf (GFunCExample x1 x2 x3) = mkApp (mkCId "FunCExample") [gf x1, gf x2, gf x3]
  gf (GFunExample x1 x2) = mkApp (mkCId "FunExample") [gf x1, gf x2]
  gf (GLabelExample x1) = mkApp (mkCId "LabelExample") [gf x1]
  gf (GNameExample x1) = mkApp (mkCId "NameExample") [gf x1]
  gf (GNoun1Example x1 x2) = mkApp (mkCId "Noun1Example") [gf x1, gf x2]
  gf (GNoun2Example x1 x2 x3) = mkApp (mkCId "Noun2Example") [gf x1, gf x2, gf x3]
  gf (GNounCExample x1 x2 x3) = mkApp (mkCId "NounCExample") [gf x1, gf x2, gf x3]
  gf (GNounExample x1) = mkApp (mkCId "NounExample") [gf x1]
  gf (GVerb2Example x1 x2 x3) = mkApp (mkCId "Verb2Example") [gf x1, gf x2, gf x3]
  gf (GVerbCExample x1 x2 x3) = mkApp (mkCId "VerbCExample") [gf x1, gf x2, gf x3]
  gf (GVerbExample x1 x2) = mkApp (mkCId "VerbExample") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "Adj2Example" -> GAdj2Example (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "Adj3Example" -> GAdj3Example (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "AdjCExample" -> GAdjCExample (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "AdjEExample" -> GAdjEExample (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "AdjExample" -> GAdjExample (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "Fam2Example" -> GFam2Example (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "FamExample" -> GFamExample (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "Fun2Example" -> GFun2Example (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "FunCExample" -> GFunCExample (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "FunExample" -> GFunExample (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "LabelExample" -> GLabelExample (fg x1)
      Just (i,[x1]) | i == mkCId "NameExample" -> GNameExample (fg x1)
      Just (i,[x1,x2]) | i == mkCId "Noun1Example" -> GNoun1Example (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "Noun2Example" -> GNoun2Example (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "NounCExample" -> GNounCExample (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "NounExample" -> GNounExample (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "Verb2Example" -> GVerb2Example (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "VerbCExample" -> GVerbCExample (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "VerbExample" -> GVerbExample (fg x1) (fg x2)


      _ -> error ("no Example " ++ show t)

instance Gf GExp where
  gf (GAbsExp x1 x2) = mkApp (mkCId "AbsExp") [gf x1, gf x2]
  gf (GAllIdentsKindExp x1 x2) = mkApp (mkCId "AllIdentsKindExp") [gf x1, gf x2]
  gf (GAllKindExp x1) = mkApp (mkCId "AllKindExp") [gf x1]
  gf (GAndExp x1) = mkApp (mkCId "AndExp") [gf x1]
  gf (GAnnotateExp x1 x2) = mkApp (mkCId "AnnotateExp") [gf x1, gf x2]
  gf (GAppExp x1 x2) = mkApp (mkCId "AppExp") [gf x1, gf x2]
  gf (GBothAndExp x1 x2) = mkApp (mkCId "BothAndExp") [gf x1, gf x2]
  gf (GCoercionExp x1 x2) = mkApp (mkCId "CoercionExp") [gf x1, gf x2]
  gf (GCoreAbsExp x1 x2) = mkApp (mkCId "CoreAbsExp") [gf x1, gf x2]
  gf (GEitherOrExp x1 x2) = mkApp (mkCId "EitherOrExp") [gf x1, gf x2]
  gf (GEnumSetExp x1) = mkApp (mkCId "EnumSetExp") [gf x1]
  gf (GEveryIdentKindExp x1 x2) = mkApp (mkCId "EveryIdentKindExp") [gf x1, gf x2]
  gf (GEveryKindExp x1) = mkApp (mkCId "EveryKindExp") [gf x1]
  gf (GFun2Exp x1 x2 x3) = mkApp (mkCId "Fun2Exp") [gf x1, gf x2, gf x3]
  gf (GFunCCollExp x1 x2) = mkApp (mkCId "FunCCollExp") [gf x1, gf x2]
  gf (GFunCExp x1 x2 x3) = mkApp (mkCId "FunCExp") [gf x1, gf x2, gf x3]
  gf (GFunExp x1 x2) = mkApp (mkCId "FunExp") [gf x1, gf x2]
  gf (GIndefIdentKindExp x1 x2) = mkApp (mkCId "IndefIdentKindExp") [gf x1, gf x2]
  gf (GIndefKindExp x1) = mkApp (mkCId "IndefKindExp") [gf x1]
  gf (GIndexedTermExp x1) = mkApp (mkCId "IndexedTermExp") [gf x1]
  gf (GIntegralExp x1 x2 x3 x4) = mkApp (mkCId "IntegralExp") [gf x1, gf x2, gf x3, gf x4]
  gf (GKindExp x1) = mkApp (mkCId "KindExp") [gf x1]
  gf (GNameExp x1) = mkApp (mkCId "NameExp") [gf x1]
  gf (GNoIdentsKindExp x1 x2) = mkApp (mkCId "NoIdentsKindExp") [gf x1, gf x2]
  gf (GNoKindExp x1) = mkApp (mkCId "NoKindExp") [gf x1]
  gf (GOrExp x1) = mkApp (mkCId "OrExp") [gf x1]
  gf (GSeriesExp x1 x2 x3) = mkApp (mkCId "SeriesExp") [gf x1, gf x2, gf x3]
  gf (GSigmaExp x1 x2 x3 x4) = mkApp (mkCId "SigmaExp") [gf x1, gf x2, gf x3, gf x4]
  gf (GSomeIdentsKindExp x1 x2) = mkApp (mkCId "SomeIdentsKindExp") [gf x1, gf x2]
  gf (GSomeKindExp x1) = mkApp (mkCId "SomeKindExp") [gf x1]
  gf (GTermExp x1) = mkApp (mkCId "TermExp") [gf x1]
  gf (GTypedExp x1 x2) = mkApp (mkCId "TypedExp") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AbsExp" -> GAbsExp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AllIdentsKindExp" -> GAllIdentsKindExp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "AllKindExp" -> GAllKindExp (fg x1)
      Just (i,[x1]) | i == mkCId "AndExp" -> GAndExp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "AnnotateExp" -> GAnnotateExp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AppExp" -> GAppExp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "BothAndExp" -> GBothAndExp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "CoercionExp" -> GCoercionExp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "CoreAbsExp" -> GCoreAbsExp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "EitherOrExp" -> GEitherOrExp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "EnumSetExp" -> GEnumSetExp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "EveryIdentKindExp" -> GEveryIdentKindExp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "EveryKindExp" -> GEveryKindExp (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "Fun2Exp" -> GFun2Exp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "FunCCollExp" -> GFunCCollExp (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "FunCExp" -> GFunCExp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "FunExp" -> GFunExp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "IndefIdentKindExp" -> GIndefIdentKindExp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "IndefKindExp" -> GIndefKindExp (fg x1)
      Just (i,[x1]) | i == mkCId "IndexedTermExp" -> GIndexedTermExp (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "IntegralExp" -> GIntegralExp (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1]) | i == mkCId "KindExp" -> GKindExp (fg x1)
      Just (i,[x1]) | i == mkCId "NameExp" -> GNameExp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "NoIdentsKindExp" -> GNoIdentsKindExp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "NoKindExp" -> GNoKindExp (fg x1)
      Just (i,[x1]) | i == mkCId "OrExp" -> GOrExp (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "SeriesExp" -> GSeriesExp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "SigmaExp" -> GSigmaExp (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "SomeIdentsKindExp" -> GSomeIdentsKindExp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "SomeKindExp" -> GSomeKindExp (fg x1)
      Just (i,[x1]) | i == mkCId "TermExp" -> GTermExp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "TypedExp" -> GTypedExp (fg x1) (fg x2)


      _ -> error ("no Exp " ++ show t)

instance Gf GExps where
  gf (GManyExps x1) = mkApp (mkCId "ManyExps") [gf x1]
  gf (GOneExps x1) = mkApp (mkCId "OneExps") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ManyExps" -> GManyExps (fg x1)
      Just (i,[x1]) | i == mkCId "OneExps" -> GOneExps (fg x1)


      _ -> error ("no Exps " ++ show t)

instance Gf GFam where
  gf (GNounPrepFam x1 x2) = mkApp (mkCId "NounPrepFam") [gf x1, gf x2]
  gf (LexFam x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "NounPrepFam" -> GNounPrepFam (fg x1) (fg x2)

      Just (i,[]) -> LexFam (showCId i)
      _ -> error ("no Fam " ++ show t)

instance Gf GFam2 where
  gf (GNounPrepFam2 x1 x2 x3) = mkApp (mkCId "NounPrepFam2") [gf x1, gf x2, gf x3]
  gf (LexFam2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "NounPrepFam2" -> GNounPrepFam2 (fg x1) (fg x2) (fg x3)

      Just (i,[]) -> LexFam2 (showCId i)
      _ -> error ("no Fam2 " ++ show t)

instance Gf GFilename where
  gf (GStringFilename x1) = mkApp (mkCId "StringFilename") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "StringFilename" -> GStringFilename (fg x1)


      _ -> error ("no Filename " ++ show t)

instance Gf GFormula where
  gf (GApp1MacroFormula x1 x2) = mkApp (mkCId "App1MacroFormula") [gf x1, gf x2]
  gf (GApp2MacroFormula x1 x2 x3) = mkApp (mkCId "App2MacroFormula") [gf x1, gf x2, gf x3]
  gf (GApp3MacroFormula x1 x2 x3 x4) = mkApp (mkCId "App3MacroFormula") [gf x1, gf x2, gf x3, gf x4]
  gf (GElemFormula x1 x2) = mkApp (mkCId "ElemFormula") [gf x1, gf x2]
  gf (GEquationFormula x1) = mkApp (mkCId "EquationFormula") [gf x1]
  gf (GMacroFormula x1) = mkApp (mkCId "MacroFormula") [gf x1]
  gf (Gmodulo_Formula x1 x2 x3) = mkApp (mkCId "modulo_Formula") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "App1MacroFormula" -> GApp1MacroFormula (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "App2MacroFormula" -> GApp2MacroFormula (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "App3MacroFormula" -> GApp3MacroFormula (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "ElemFormula" -> GElemFormula (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "EquationFormula" -> GEquationFormula (fg x1)
      Just (i,[x1]) | i == mkCId "MacroFormula" -> GMacroFormula (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "modulo_Formula" -> Gmodulo_Formula (fg x1) (fg x2) (fg x3)


      _ -> error ("no Formula " ++ show t)

instance Gf GFun where
  gf (GNounPrepFun x1 x2) = mkApp (mkCId "NounPrepFun") [gf x1, gf x2]
  gf (LexFun x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "NounPrepFun" -> GNounPrepFun (fg x1) (fg x2)

      Just (i,[]) -> LexFun (showCId i)
      _ -> error ("no Fun " ++ show t)

instance Gf GFun2 where
  gf (GNounPrepFun2 x1 x2 x3) = mkApp (mkCId "NounPrepFun2") [gf x1, gf x2, gf x3]
  gf (LexFun2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "NounPrepFun2" -> GNounPrepFun2 (fg x1) (fg x2) (fg x3)

      Just (i,[]) -> LexFun2 (showCId i)
      _ -> error ("no Fun2 " ++ show t)

instance Gf GFunC where
  gf (GNounPrepFunC x1 x2) = mkApp (mkCId "NounPrepFunC") [gf x1, gf x2]
  gf (LexFunC x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "NounPrepFunC" -> GNounPrepFunC (fg x1) (fg x2)

      Just (i,[]) -> LexFunC (showCId i)
      _ -> error ("no FunC " ++ show t)

instance Gf GFunction where
  gf (GDerivativeFunction x1) = mkApp (mkCId "DerivativeFunction") [gf x1]
  gf (GIdentFunction x1) = mkApp (mkCId "IdentFunction") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "DerivativeFunction" -> GDerivativeFunction (fg x1)
      Just (i,[x1]) | i == mkCId "IdentFunction" -> GIdentFunction (fg x1)


      _ -> error ("no Function " ++ show t)

instance Gf GHence where
  gf GafortioriHence = mkApp (mkCId "afortioriHence") []
  gf GaltogetherHence = mkApp (mkCId "altogetherHence") []
  gf GhenceHence = mkApp (mkCId "henceHence") []
  gf GinParticularHence = mkApp (mkCId "inParticularHence") []
  gf GnoHence = mkApp (mkCId "noHence") []
  gf GthenHence = mkApp (mkCId "thenHence") []
  gf GthusHence = mkApp (mkCId "thusHence") []
  gf GweConcludeHence = mkApp (mkCId "weConcludeHence") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "afortioriHence" -> GafortioriHence 
      Just (i,[]) | i == mkCId "altogetherHence" -> GaltogetherHence 
      Just (i,[]) | i == mkCId "henceHence" -> GhenceHence 
      Just (i,[]) | i == mkCId "inParticularHence" -> GinParticularHence 
      Just (i,[]) | i == mkCId "noHence" -> GnoHence 
      Just (i,[]) | i == mkCId "thenHence" -> GthenHence 
      Just (i,[]) | i == mkCId "thusHence" -> GthusHence 
      Just (i,[]) | i == mkCId "weConcludeHence" -> GweConcludeHence 


      _ -> error ("no Hence " ++ show t)

instance Gf GHypo where
  gf (GBareVarHypo x1) = mkApp (mkCId "BareVarHypo") [gf x1]
  gf (GBareVarsHypo x1) = mkApp (mkCId "BareVarsHypo") [gf x1]
  gf (GIndexedLetFormulaHypo x1) = mkApp (mkCId "IndexedLetFormulaHypo") [gf x1]
  gf (GLetDeclarationHypo x1) = mkApp (mkCId "LetDeclarationHypo") [gf x1]
  gf (GLetFormulaHypo x1) = mkApp (mkCId "LetFormulaHypo") [gf x1]
  gf (GLocalHypo x1) = mkApp (mkCId "LocalHypo") [gf x1]
  gf (GPropHypo x1) = mkApp (mkCId "PropHypo") [gf x1]
  gf (GSupposePropHypo x1) = mkApp (mkCId "SupposePropHypo") [gf x1]
  gf (GVarHypo x1 x2) = mkApp (mkCId "VarHypo") [gf x1, gf x2]
  gf (GVarsHypo x1 x2) = mkApp (mkCId "VarsHypo") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "BareVarHypo" -> GBareVarHypo (fg x1)
      Just (i,[x1]) | i == mkCId "BareVarsHypo" -> GBareVarsHypo (fg x1)
      Just (i,[x1]) | i == mkCId "IndexedLetFormulaHypo" -> GIndexedLetFormulaHypo (fg x1)
      Just (i,[x1]) | i == mkCId "LetDeclarationHypo" -> GLetDeclarationHypo (fg x1)
      Just (i,[x1]) | i == mkCId "LetFormulaHypo" -> GLetFormulaHypo (fg x1)
      Just (i,[x1]) | i == mkCId "LocalHypo" -> GLocalHypo (fg x1)
      Just (i,[x1]) | i == mkCId "PropHypo" -> GPropHypo (fg x1)
      Just (i,[x1]) | i == mkCId "SupposePropHypo" -> GSupposePropHypo (fg x1)
      Just (i,[x1,x2]) | i == mkCId "VarHypo" -> GVarHypo (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "VarsHypo" -> GVarsHypo (fg x1) (fg x2)


      _ -> error ("no Hypo " ++ show t)

instance Gf GIdent where
  gf (GStrIdent x1) = mkApp (mkCId "StrIdent") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "StrIdent" -> GStrIdent (fg x1)


      _ -> error ("no Ident " ++ show t)

instance Gf GJmt where
  gf (GAxiomExpJmt x1 x2 x3 x4) = mkApp (mkCId "AxiomExpJmt") [gf x1, gf x2, gf x3, gf x4]
  gf (GAxiomJmt x1 x2 x3) = mkApp (mkCId "AxiomJmt") [gf x1, gf x2, gf x3]
  gf (GAxiomKindJmt x1 x2 x3) = mkApp (mkCId "AxiomKindJmt") [gf x1, gf x2, gf x3]
  gf (GAxiomPropJmt x1 x2 x3) = mkApp (mkCId "AxiomPropJmt") [gf x1, gf x2, gf x3]
  gf (GDefExpJmt x1 x2 x3 x4 x5) = mkApp (mkCId "DefExpJmt") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (GDefKindJmt x1 x2 x3 x4) = mkApp (mkCId "DefKindJmt") [gf x1, gf x2, gf x3, gf x4]
  gf (GDefPropJmt x1 x2 x3 x4) = mkApp (mkCId "DefPropJmt") [gf x1, gf x2, gf x3, gf x4]
  gf (GDefUntypedExpJmt x1 x2 x3) = mkApp (mkCId "DefUntypedExpJmt") [gf x1, gf x2, gf x3]
  gf (GDefinedAdjJmt x1 x2 x3 x4 x5) = mkApp (mkCId "DefinedAdjJmt") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (GRewriteJmt x1) = mkApp (mkCId "RewriteJmt") [gf x1]
  gf (GThmJmt x1 x2 x3 x4) = mkApp (mkCId "ThmJmt") [gf x1, gf x2, gf x3, gf x4]
  gf (GUnitJmt x1) = mkApp (mkCId "UnitJmt") [gf x1]
  gf (GWeDefineAdjJmt x1 x2 x3 x4 x5) = mkApp (mkCId "WeDefineAdjJmt") [gf x1, gf x2, gf x3, gf x4, gf x5]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3,x4]) | i == mkCId "AxiomExpJmt" -> GAxiomExpJmt (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "AxiomJmt" -> GAxiomJmt (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "AxiomKindJmt" -> GAxiomKindJmt (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "AxiomPropJmt" -> GAxiomPropJmt (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "DefExpJmt" -> GDefExpJmt (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "DefKindJmt" -> GDefKindJmt (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "DefPropJmt" -> GDefPropJmt (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "DefUntypedExpJmt" -> GDefUntypedExpJmt (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "DefinedAdjJmt" -> GDefinedAdjJmt (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1]) | i == mkCId "RewriteJmt" -> GRewriteJmt (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "ThmJmt" -> GThmJmt (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1]) | i == mkCId "UnitJmt" -> GUnitJmt (fg x1)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "WeDefineAdjJmt" -> GWeDefineAdjJmt (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)


      _ -> error ("no Jmt " ++ show t)

instance Gf GKind where
  gf (GAdjKind x1 x2) = mkApp (mkCId "AdjKind") [gf x1, gf x2]
  gf (GAnnotateKind x1 x2) = mkApp (mkCId "AnnotateKind") [gf x1, gf x2]
  gf (GAppKind x1 x2) = mkApp (mkCId "AppKind") [gf x1, gf x2]
  gf (GElemKind x1) = mkApp (mkCId "ElemKind") [gf x1]
  gf (GFam2Kind x1 x2 x3) = mkApp (mkCId "Fam2Kind") [gf x1, gf x2, gf x3]
  gf (GFamKind x1 x2) = mkApp (mkCId "FamKind") [gf x1, gf x2]
  gf (GFunKind x1 x2) = mkApp (mkCId "FunKind") [gf x1, gf x2]
  gf (GIdentKind x1) = mkApp (mkCId "IdentKind") [gf x1]
  gf (GNounKind x1) = mkApp (mkCId "NounKind") [gf x1]
  gf (GSuchThatKind x1 x2 x3) = mkApp (mkCId "SuchThatKind") [gf x1, gf x2, gf x3]
  gf (GTermKind x1) = mkApp (mkCId "TermKind") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjKind" -> GAdjKind (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AnnotateKind" -> GAnnotateKind (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AppKind" -> GAppKind (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ElemKind" -> GElemKind (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "Fam2Kind" -> GFam2Kind (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "FamKind" -> GFamKind (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "FunKind" -> GFunKind (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "IdentKind" -> GIdentKind (fg x1)
      Just (i,[x1]) | i == mkCId "NounKind" -> GNounKind (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "SuchThatKind" -> GSuchThatKind (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "TermKind" -> GTermKind (fg x1)


      _ -> error ("no Kind " ++ show t)

instance Gf GKindArgument where
  gf GA_KindArgument = mkApp (mkCId "A_KindArgument") []
  gf GB_KindArgument = mkApp (mkCId "B_KindArgument") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "A_KindArgument" -> GA_KindArgument 
      Just (i,[]) | i == mkCId "B_KindArgument" -> GB_KindArgument 


      _ -> error ("no KindArgument " ++ show t)

instance Gf GLabel where
  gf (GDefNounLabel x1) = mkApp (mkCId "DefNounLabel") [gf x1]
  gf (GIdentLabel x1) = mkApp (mkCId "IdentLabel") [gf x1]
  gf (GNounIdentLabel x1 x2) = mkApp (mkCId "NounIdentLabel") [gf x1, gf x2]
  gf (GNounIntLabel x1 x2) = mkApp (mkCId "NounIntLabel") [gf x1, gf x2]
  gf (GNounLabel x1) = mkApp (mkCId "NounLabel") [gf x1]
  gf (GNounOfNounLabel x1 x2) = mkApp (mkCId "NounOfNounLabel") [gf x1, gf x2]
  gf (GProperNameNounLabel x1 x2) = mkApp (mkCId "ProperNameNounLabel") [gf x1, gf x2]
  gf (GcrefLabel x1) = mkApp (mkCId "crefLabel") [gf x1]
  gf (LexLabel x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "DefNounLabel" -> GDefNounLabel (fg x1)
      Just (i,[x1]) | i == mkCId "IdentLabel" -> GIdentLabel (fg x1)
      Just (i,[x1,x2]) | i == mkCId "NounIdentLabel" -> GNounIdentLabel (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NounIntLabel" -> GNounIntLabel (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "NounLabel" -> GNounLabel (fg x1)
      Just (i,[x1,x2]) | i == mkCId "NounOfNounLabel" -> GNounOfNounLabel (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ProperNameNounLabel" -> GProperNameNounLabel (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "crefLabel" -> GcrefLabel (fg x1)

      Just (i,[]) -> LexLabel (showCId i)
      _ -> error ("no Label " ++ show t)

instance Gf GListAdj where
  gf (GListAdj [x1,x2]) = mkApp (mkCId "BaseAdj") [gf x1, gf x2]
  gf (GListAdj (x:xs)) = mkApp (mkCId "ConsAdj") [gf x, gf (GListAdj xs)]
  fg t =
    GListAdj (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAdj" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAdj" -> fg x1 : fgs x2


      _ -> error ("no ListAdj " ++ show t)

instance Gf GListArgKind where
  gf (GListArgKind [x1]) = mkApp (mkCId "BaseArgKind") [gf x1]
  gf (GListArgKind (x:xs)) = mkApp (mkCId "ConsArgKind") [gf x, gf (GListArgKind xs)]
  fg t =
    GListArgKind (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1]) | i == mkCId "BaseArgKind" -> [fg x1]
      Just (i,[x1,x2]) | i == mkCId "ConsArgKind" -> fg x1 : fgs x2


      _ -> error ("no ListArgKind " ++ show t)

instance Gf GListExp where
  gf (GListExp [x1,x2]) = mkApp (mkCId "BaseExp") [gf x1, gf x2]
  gf (GListExp (x:xs)) = mkApp (mkCId "ConsExp") [gf x, gf (GListExp xs)]
  fg t =
    GListExp (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseExp" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsExp" -> fg x1 : fgs x2


      _ -> error ("no ListExp " ++ show t)

instance Gf GListHypo where
  gf (GListHypo []) = mkApp (mkCId "BaseHypo") []
  gf (GListHypo (x:xs)) = mkApp (mkCId "ConsHypo") [gf x, gf (GListHypo xs)]
  fg t =
    GListHypo (fgs t) where
     fgs t = case unApp t of
      Just (i,[]) | i == mkCId "BaseHypo" -> []
      Just (i,[x1,x2]) | i == mkCId "ConsHypo" -> fg x1 : fgs x2


      _ -> error ("no ListHypo " ++ show t)

instance Gf GListIdent where
  gf (GListIdent [x1]) = mkApp (mkCId "BaseIdent") [gf x1]
  gf (GListIdent (x:xs)) = mkApp (mkCId "ConsIdent") [gf x, gf (GListIdent xs)]
  fg t =
    GListIdent (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1]) | i == mkCId "BaseIdent" -> [fg x1]
      Just (i,[x1,x2]) | i == mkCId "ConsIdent" -> fg x1 : fgs x2


      _ -> error ("no ListIdent " ++ show t)

instance Gf GListProof where
  gf (GListProof []) = mkApp (mkCId "BaseProof") []
  gf (GListProof (x:xs)) = mkApp (mkCId "ConsProof") [gf x, gf (GListProof xs)]
  fg t =
    GListProof (fgs t) where
     fgs t = case unApp t of
      Just (i,[]) | i == mkCId "BaseProof" -> []
      Just (i,[x1,x2]) | i == mkCId "ConsProof" -> fg x1 : fgs x2


      _ -> error ("no ListProof " ++ show t)

instance Gf GListProp where
  gf (GListProp [x1,x2]) = mkApp (mkCId "BaseProp") [gf x1, gf x2]
  gf (GListProp (x:xs)) = mkApp (mkCId "ConsProp") [gf x, gf (GListProp xs)]
  fg t =
    GListProp (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseProp" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsProp" -> fg x1 : fgs x2


      _ -> error ("no ListProp " ++ show t)

instance Gf GListRule where
  gf (GListRule [x1]) = mkApp (mkCId "BaseRule") [gf x1]
  gf (GListRule (x:xs)) = mkApp (mkCId "ConsRule") [gf x, gf (GListRule xs)]
  fg t =
    GListRule (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1]) | i == mkCId "BaseRule" -> [fg x1]
      Just (i,[x1,x2]) | i == mkCId "ConsRule" -> fg x1 : fgs x2


      _ -> error ("no ListRule " ++ show t)

instance Gf GListTerm where
  gf (GListTerm [x1]) = mkApp (mkCId "BaseTerm") [gf x1]
  gf (GListTerm (x:xs)) = mkApp (mkCId "ConsTerm") [gf x, gf (GListTerm xs)]
  fg t =
    GListTerm (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1]) | i == mkCId "BaseTerm" -> [fg x1]
      Just (i,[x1,x2]) | i == mkCId "ConsTerm" -> fg x1 : fgs x2


      _ -> error ("no ListTerm " ++ show t)

instance Gf GListUnit where
  gf (GListUnit []) = mkApp (mkCId "BaseUnit") []
  gf (GListUnit (x:xs)) = mkApp (mkCId "ConsUnit") [gf x, gf (GListUnit xs)]
  fg t =
    GListUnit (fgs t) where
     fgs t = case unApp t of
      Just (i,[]) | i == mkCId "BaseUnit" -> []
      Just (i,[x1,x2]) | i == mkCId "ConsUnit" -> fg x1 : fgs x2


      _ -> error ("no ListUnit " ++ show t)

instance Gf GLocal where
  gf (GBareLetLocal x1 x2) = mkApp (mkCId "BareLetLocal") [gf x1, gf x2]
  gf (GLetLocal x1 x2 x3) = mkApp (mkCId "LetLocal") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BareLetLocal" -> GBareLetLocal (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "LetLocal" -> GLetLocal (fg x1) (fg x2) (fg x3)


      _ -> error ("no Local " ++ show t)

instance Gf GMacro where
  gf (GStringMacro x1) = mkApp (mkCId "StringMacro") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "StringMacro" -> GStringMacro (fg x1)


      _ -> error ("no Macro " ++ show t)

instance Gf GMethod where
  gf (GStringMethod x1) = mkApp (mkCId "StringMethod") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "StringMethod" -> GStringMethod (fg x1)


      _ -> error ("no Method " ++ show t)

instance Gf GName where
  gf (GDefNounName x1) = mkApp (mkCId "DefNounName") [gf x1]
  gf (GProperNameNounName x1 x2) = mkApp (mkCId "ProperNameNounName") [gf x1, gf x2]
  gf (LexName x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "DefNounName" -> GDefNounName (fg x1)
      Just (i,[x1,x2]) | i == mkCId "ProperNameNounName" -> GProperNameNounName (fg x1) (fg x2)

      Just (i,[]) -> LexName (showCId i)
      _ -> error ("no Name " ++ show t)

instance Gf GNoun where
  gf (GAdjNounNoun x1 x2) = mkApp (mkCId "AdjNounNoun") [gf x1, gf x2]
  gf (GNounNounNoun x1 x2) = mkApp (mkCId "NounNounNoun") [gf x1, gf x2]
  gf (GProperNameNounNoun x1 x2) = mkApp (mkCId "ProperNameNounNoun") [gf x1, gf x2]
  gf (LexNoun x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjNounNoun" -> GAdjNounNoun (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NounNounNoun" -> GNounNounNoun (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ProperNameNounNoun" -> GProperNameNounNoun (fg x1) (fg x2)

      Just (i,[]) -> LexNoun (showCId i)
      _ -> error ("no Noun " ++ show t)

instance Gf GNoun1 where
  gf (GNounNoun1 x1) = mkApp (mkCId "NounNoun1") [gf x1]
  gf (LexNoun1 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "NounNoun1" -> GNounNoun1 (fg x1)

      Just (i,[]) -> LexNoun1 (showCId i)
      _ -> error ("no Noun1 " ++ show t)

instance Gf GNoun2 where
  gf (GNounPrepNoun2 x1 x2) = mkApp (mkCId "NounPrepNoun2") [gf x1, gf x2]
  gf (LexNoun2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "NounPrepNoun2" -> GNounPrepNoun2 (fg x1) (fg x2)

      Just (i,[]) -> LexNoun2 (showCId i)
      _ -> error ("no Noun2 " ++ show t)

instance Gf GNounC where
  gf (GNounNounC x1) = mkApp (mkCId "NounNounC") [gf x1]
  gf (LexNounC x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "NounNounC" -> GNounNounC (fg x1)

      Just (i,[]) -> LexNounC (showCId i)
      _ -> error ("no NounC " ++ show t)

instance Gf GOper where
  gf (LexOper x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexOper (showCId i)
      _ -> error ("no Oper " ++ show t)

instance Gf GOper2 where
  gf (LexOper2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexOper2 (showCId i)
      _ -> error ("no Oper2 " ++ show t)

instance Gf GPrep where
  gf (LexPrep x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexPrep (showCId i)
      _ -> error ("no Prep " ++ show t)

instance Gf GProof where
  gf (GAbsProof x1 x2) = mkApp (mkCId "AbsProof") [gf x1, gf x2]
  gf (GAnnotateProof x1 x2) = mkApp (mkCId "AnnotateProof") [gf x1, gf x2]
  gf (GAppProof x1 x2) = mkApp (mkCId "AppProof") [gf x1, gf x2]
  gf (GNatIndProof x1 x2 x3 x4 x5 x6) = mkApp (mkCId "NatIndProof") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (GUnitsProof x1) = mkApp (mkCId "UnitsProof") [gf x1]
  gf (GandElProof x1 x2 x3) = mkApp (mkCId "andElProof") [gf x1, gf x2, gf x3]
  gf (GandErProof x1 x2 x3) = mkApp (mkCId "andErProof") [gf x1, gf x2, gf x3]
  gf (GandIProof x1 x2 x3 x4) = mkApp (mkCId "andIProof") [gf x1, gf x2, gf x3, gf x4]
  gf (GevenSuccProof x1 x2) = mkApp (mkCId "evenSuccProof") [gf x1, gf x2]
  gf GevenZeroProof = mkApp (mkCId "evenZeroProof") []
  gf (GexistsEProof x1 x2 x3 x4 x5 x6 x7 x8) = mkApp (mkCId "existsEProof") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7, gf x8]
  gf (GexistsIProof x1 x2 x3 x4 x5) = mkApp (mkCId "existsIProof") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (GfalseEProof x1 x2) = mkApp (mkCId "falseEProof") [gf x1, gf x2]
  gf (GforallEProof x1 x2 x3 x4 x5) = mkApp (mkCId "forallEProof") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (GforallIProof x1 x2 x3 x4 x5) = mkApp (mkCId "forallIProof") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (GhypoProof x1 x2) = mkApp (mkCId "hypoProof") [gf x1, gf x2]
  gf (GifEProof x1 x2 x3 x4) = mkApp (mkCId "ifEProof") [gf x1, gf x2, gf x3, gf x4]
  gf (GifIProof x1 x2 x3 x4) = mkApp (mkCId "ifIProof") [gf x1, gf x2, gf x3, gf x4]
  gf (GoddSuccProof x1 x2) = mkApp (mkCId "oddSuccProof") [gf x1, gf x2]
  gf (GorEProof x1 x2 x3 x4 x5 x6 x7 x8) = mkApp (mkCId "orEProof") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7, gf x8]
  gf (GorIlProof x1 x2 x3) = mkApp (mkCId "orIlProof") [gf x1, gf x2, gf x3]
  gf (GorIrProof x1 x2 x3) = mkApp (mkCId "orIrProof") [gf x1, gf x2, gf x3]
  gf (GreflProof x1 x2) = mkApp (mkCId "reflProof") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AbsProof" -> GAbsProof (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AnnotateProof" -> GAnnotateProof (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AppProof" -> GAppProof (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "NatIndProof" -> GNatIndProof (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1]) | i == mkCId "UnitsProof" -> GUnitsProof (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "andElProof" -> GandElProof (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "andErProof" -> GandErProof (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "andIProof" -> GandIProof (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "evenSuccProof" -> GevenSuccProof (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "evenZeroProof" -> GevenZeroProof 
      Just (i,[x1,x2,x3,x4,x5,x6,x7,x8]) | i == mkCId "existsEProof" -> GexistsEProof (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7) (fg x8)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "existsIProof" -> GexistsIProof (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2]) | i == mkCId "falseEProof" -> GfalseEProof (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "forallEProof" -> GforallEProof (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "forallIProof" -> GforallIProof (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2]) | i == mkCId "hypoProof" -> GhypoProof (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "ifEProof" -> GifEProof (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "ifIProof" -> GifIProof (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "oddSuccProof" -> GoddSuccProof (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4,x5,x6,x7,x8]) | i == mkCId "orEProof" -> GorEProof (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7) (fg x8)
      Just (i,[x1,x2,x3]) | i == mkCId "orIlProof" -> GorIlProof (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "orIrProof" -> GorIrProof (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "reflProof" -> GreflProof (fg x1) (fg x2)


      _ -> error ("no Proof " ++ show t)

instance Gf GProofExp where
  gf (GAbsProofExp x1 x2) = mkApp (mkCId "AbsProofExp") [gf x1, gf x2]
  gf (GAnnotateProofExp x1 x2) = mkApp (mkCId "AnnotateProofExp") [gf x1, gf x2]
  gf (GAppProofExp x1 x2) = mkApp (mkCId "AppProofExp") [gf x1, gf x2]
  gf (GLabelProofExp x1) = mkApp (mkCId "LabelProofExp") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AbsProofExp" -> GAbsProofExp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AnnotateProofExp" -> GAnnotateProofExp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AppProofExp" -> GAppProofExp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "LabelProofExp" -> GLabelProofExp (fg x1)


      _ -> error ("no ProofExp " ++ show t)

instance Gf GProp where
  gf (GAdj2Prop x1 x2 x3) = mkApp (mkCId "Adj2Prop") [gf x1, gf x2, gf x3]
  gf (GAdj3Prop x1 x2 x3 x4) = mkApp (mkCId "Adj3Prop") [gf x1, gf x2, gf x3, gf x4]
  gf (GAdjCCollProp x1 x2) = mkApp (mkCId "AdjCCollProp") [gf x1, gf x2]
  gf (GAdjCProp x1 x2 x3) = mkApp (mkCId "AdjCProp") [gf x1, gf x2, gf x3]
  gf (GAdjECollProp x1 x2) = mkApp (mkCId "AdjECollProp") [gf x1, gf x2]
  gf (GAdjEProp x1 x2 x3) = mkApp (mkCId "AdjEProp") [gf x1, gf x2, gf x3]
  gf (GAdjProp x1 x2) = mkApp (mkCId "AdjProp") [gf x1, gf x2]
  gf (GAllProp x1 x2) = mkApp (mkCId "AllProp") [gf x1, gf x2]
  gf (GAndProp x1) = mkApp (mkCId "AndProp") [gf x1]
  gf (GAnnotateProp x1 x2) = mkApp (mkCId "AnnotateProp") [gf x1, gf x2]
  gf (GAppProp x1 x2) = mkApp (mkCId "AppProp") [gf x1, gf x2]
  gf (GBothAndProp x1 x2) = mkApp (mkCId "BothAndProp") [gf x1, gf x2]
  gf (GCoreAllProp x1 x2 x3) = mkApp (mkCId "CoreAllProp") [gf x1, gf x2, gf x3]
  gf (GCoreAndProp x1 x2) = mkApp (mkCId "CoreAndProp") [gf x1, gf x2]
  gf (GCoreExistProp x1 x2 x3) = mkApp (mkCId "CoreExistProp") [gf x1, gf x2, gf x3]
  gf (GCoreIfProp x1 x2) = mkApp (mkCId "CoreIfProp") [gf x1, gf x2]
  gf (GCoreIffProp x1 x2) = mkApp (mkCId "CoreIffProp") [gf x1, gf x2]
  gf (GCoreNotProp x1) = mkApp (mkCId "CoreNotProp") [gf x1]
  gf (GCoreOrProp x1 x2) = mkApp (mkCId "CoreOrProp") [gf x1, gf x2]
  gf (GDisplayFormulaProp x1) = mkApp (mkCId "DisplayFormulaProp") [gf x1]
  gf (GEitherOrProp x1 x2) = mkApp (mkCId "EitherOrProp") [gf x1, gf x2]
  gf (GExistNoProp x1 x2) = mkApp (mkCId "ExistNoProp") [gf x1, gf x2]
  gf (GExistProp x1 x2) = mkApp (mkCId "ExistProp") [gf x1, gf x2]
  gf GFalseProp = mkApp (mkCId "FalseProp") []
  gf (GFormulaImpliesProp x1 x2) = mkApp (mkCId "FormulaImpliesProp") [gf x1, gf x2]
  gf (GFormulaProp x1) = mkApp (mkCId "FormulaProp") [gf x1]
  gf (GIdentProp x1) = mkApp (mkCId "IdentProp") [gf x1]
  gf (GIfProp x1 x2) = mkApp (mkCId "IfProp") [gf x1, gf x2]
  gf (GIffIffProp x1 x2) = mkApp (mkCId "IffIffProp") [gf x1, gf x2]
  gf (GIffProp x1 x2) = mkApp (mkCId "IffProp") [gf x1, gf x2]
  gf (GIndexedFormulaProp x1) = mkApp (mkCId "IndexedFormulaProp") [gf x1]
  gf (GKindProp x1 x2) = mkApp (mkCId "KindProp") [gf x1, gf x2]
  gf (GNoArticleExistProp x1 x2) = mkApp (mkCId "NoArticleExistProp") [gf x1, gf x2]
  gf (GNoCommaAllProp x1 x2) = mkApp (mkCId "NoCommaAllProp") [gf x1, gf x2]
  gf (GNoCommaExistProp x1 x2) = mkApp (mkCId "NoCommaExistProp") [gf x1, gf x2]
  gf (GNotAdj2Prop x1 x2 x3) = mkApp (mkCId "NotAdj2Prop") [gf x1, gf x2, gf x3]
  gf (GNotAdjCProp x1 x2) = mkApp (mkCId "NotAdjCProp") [gf x1, gf x2]
  gf (GNotAdjEProp x1 x2) = mkApp (mkCId "NotAdjEProp") [gf x1, gf x2]
  gf (GNotAdjProp x1 x2) = mkApp (mkCId "NotAdjProp") [gf x1, gf x2]
  gf (GNotNoun1Prop x1 x2) = mkApp (mkCId "NotNoun1Prop") [gf x1, gf x2]
  gf (GNotNoun2Prop x1 x2 x3) = mkApp (mkCId "NotNoun2Prop") [gf x1, gf x2, gf x3]
  gf (GNotNounCProp x1 x2) = mkApp (mkCId "NotNounCProp") [gf x1, gf x2]
  gf (GNotVerb2Prop x1 x2 x3) = mkApp (mkCId "NotVerb2Prop") [gf x1, gf x2, gf x3]
  gf (GNotVerbCProp x1 x2) = mkApp (mkCId "NotVerbCProp") [gf x1, gf x2]
  gf (GNotVerbProp x1 x2) = mkApp (mkCId "NotVerbProp") [gf x1, gf x2]
  gf (GNoun1Prop x1 x2) = mkApp (mkCId "Noun1Prop") [gf x1, gf x2]
  gf (GNoun2Prop x1 x2 x3) = mkApp (mkCId "Noun2Prop") [gf x1, gf x2, gf x3]
  gf (GNounCProp x1 x2 x3) = mkApp (mkCId "NounCProp") [gf x1, gf x2, gf x3]
  gf (GOnlyIfProp x1 x2) = mkApp (mkCId "OnlyIfProp") [gf x1, gf x2]
  gf (GOrProp x1) = mkApp (mkCId "OrProp") [gf x1]
  gf (GPostQuantProp x1 x2) = mkApp (mkCId "PostQuantProp") [gf x1, gf x2]
  gf (GProofProp x1) = mkApp (mkCId "ProofProp") [gf x1]
  gf (GVerb2Prop x1 x2 x3) = mkApp (mkCId "Verb2Prop") [gf x1, gf x2, gf x3]
  gf (GVerbCProp x1 x2 x3) = mkApp (mkCId "VerbCProp") [gf x1, gf x2, gf x3]
  gf (GVerbProp x1 x2) = mkApp (mkCId "VerbProp") [gf x1, gf x2]
  gf (GWeHaveProp x1) = mkApp (mkCId "WeHaveProp") [gf x1]
  gf (GsameParityProp x1 x2) = mkApp (mkCId "sameParityProp") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "Adj2Prop" -> GAdj2Prop (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "Adj3Prop" -> GAdj3Prop (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "AdjCCollProp" -> GAdjCCollProp (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "AdjCProp" -> GAdjCProp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "AdjECollProp" -> GAdjECollProp (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "AdjEProp" -> GAdjEProp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "AdjProp" -> GAdjProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AllProp" -> GAllProp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "AndProp" -> GAndProp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "AnnotateProp" -> GAnnotateProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AppProp" -> GAppProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "BothAndProp" -> GBothAndProp (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "CoreAllProp" -> GCoreAllProp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "CoreAndProp" -> GCoreAndProp (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "CoreExistProp" -> GCoreExistProp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "CoreIfProp" -> GCoreIfProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "CoreIffProp" -> GCoreIffProp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "CoreNotProp" -> GCoreNotProp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "CoreOrProp" -> GCoreOrProp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "DisplayFormulaProp" -> GDisplayFormulaProp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "EitherOrProp" -> GEitherOrProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ExistNoProp" -> GExistNoProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ExistProp" -> GExistProp (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "FalseProp" -> GFalseProp 
      Just (i,[x1,x2]) | i == mkCId "FormulaImpliesProp" -> GFormulaImpliesProp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "FormulaProp" -> GFormulaProp (fg x1)
      Just (i,[x1]) | i == mkCId "IdentProp" -> GIdentProp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "IfProp" -> GIfProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "IffIffProp" -> GIffIffProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "IffProp" -> GIffProp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "IndexedFormulaProp" -> GIndexedFormulaProp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "KindProp" -> GKindProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NoArticleExistProp" -> GNoArticleExistProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NoCommaAllProp" -> GNoCommaAllProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NoCommaExistProp" -> GNoCommaExistProp (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "NotAdj2Prop" -> GNotAdj2Prop (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "NotAdjCProp" -> GNotAdjCProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NotAdjEProp" -> GNotAdjEProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NotAdjProp" -> GNotAdjProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NotNoun1Prop" -> GNotNoun1Prop (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "NotNoun2Prop" -> GNotNoun2Prop (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "NotNounCProp" -> GNotNounCProp (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "NotVerb2Prop" -> GNotVerb2Prop (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "NotVerbCProp" -> GNotVerbCProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NotVerbProp" -> GNotVerbProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Noun1Prop" -> GNoun1Prop (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "Noun2Prop" -> GNoun2Prop (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "NounCProp" -> GNounCProp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "OnlyIfProp" -> GOnlyIfProp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "OrProp" -> GOrProp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PostQuantProp" -> GPostQuantProp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ProofProp" -> GProofProp (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "Verb2Prop" -> GVerb2Prop (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "VerbCProp" -> GVerbCProp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "VerbProp" -> GVerbProp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "WeHaveProp" -> GWeHaveProp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "sameParityProp" -> GsameParityProp (fg x1) (fg x2)


      _ -> error ("no Prop " ++ show t)

instance Gf GProperName where
  gf GAccor_ProperName = mkApp (mkCId "Accor_ProperName") []
  gf GAckermann_ProperName = mkApp (mkCId "Ackermann_ProperName") []
  gf GAdams_ProperName = mkApp (mkCId "Adams_ProperName") []
  gf GAdinkra_ProperName = mkApp (mkCId "Adinkra_ProperName") []
  gf GAhlfors_ProperName = mkApp (mkCId "Ahlfors_ProperName") []
  gf GAlbert_ProperName = mkApp (mkCId "Albert_ProperName") []
  gf GAlexander_ProperName = mkApp (mkCId "Alexander_ProperName") []
  gf GAlexandroff_ProperName = mkApp (mkCId "Alexandroff_ProperName") []
  gf GAlexandrov_ProperName = mkApp (mkCId "Alexandrov_ProperName") []
  gf GAndrásfai_ProperName = mkApp (mkCId "Andrásfai_ProperName") []
  gf GAnosov_ProperName = mkApp (mkCId "Anosov_ProperName") []
  gf GArf_ProperName = mkApp (mkCId "Arf_ProperName") []
  gf GAronszajn_ProperName = mkApp (mkCId "Aronszajn_ProperName") []
  gf GArtin_ProperName = mkApp (mkCId "Artin_ProperName") []
  gf GAschbacher_ProperName = mkApp (mkCId "Aschbacher_ProperName") []
  gf GAshtekar_ProperName = mkApp (mkCId "Ashtekar_ProperName") []
  gf GAtiyah_ProperName = mkApp (mkCId "Atiyah_ProperName") []
  gf GAzumaya_ProperName = mkApp (mkCId "Azumaya_ProperName") []
  gf GBaer_ProperName = mkApp (mkCId "Baer_ProperName") []
  gf GBanach_ProperName = mkApp (mkCId "Banach_ProperName") []
  gf GBarnes_ProperName = mkApp (mkCId "Barnes_ProperName") []
  gf GBayes_ProperName = mkApp (mkCId "Bayes_ProperName") []
  gf GBeltrami_ProperName = mkApp (mkCId "Beltrami_ProperName") []
  gf GBendixson_ProperName = mkApp (mkCId "Bendixson_ProperName") []
  gf GBenini_ProperName = mkApp (mkCId "Benini_ProperName") []
  gf GBenktander_ProperName = mkApp (mkCId "Benktander_ProperName") []
  gf GBerge_ProperName = mkApp (mkCId "Berge_ProperName") []
  gf GBerkeley_ProperName = mkApp (mkCId "Berkeley_ProperName") []
  gf GBernoulli_ProperName = mkApp (mkCId "Bernoulli_ProperName") []
  gf GBesov_ProperName = mkApp (mkCId "Besov_ProperName") []
  gf GBhattacharyya_ProperName = mkApp (mkCId "Bhattacharyya_ProperName") []
  gf GBianchi_ProperName = mkApp (mkCId "Bianchi_ProperName") []
  gf GBidiakis_ProperName = mkApp (mkCId "Bidiakis_ProperName") []
  gf GBirkhoff_ProperName = mkApp (mkCId "Birkhoff_ProperName") []
  gf GBlanuša_ProperName = mkApp (mkCId "'Blanuša_ProperName'") []
  gf GBlum_ProperName = mkApp (mkCId "Blum_ProperName") []
  gf GBochner_ProperName = mkApp (mkCId "Bochner_ProperName") []
  gf GBockstein_ProperName = mkApp (mkCId "Bockstein_ProperName") []
  gf GBorel_ProperName = mkApp (mkCId "Borel_ProperName") []
  gf GBott_ProperName = mkApp (mkCId "Bott_ProperName") []
  gf GBrandt_ProperName = mkApp (mkCId "Brandt_ProperName") []
  gf GBrauer_ProperName = mkApp (mkCId "Brauer_ProperName") []
  gf GBrauner_ProperName = mkApp (mkCId "Brauner_ProperName") []
  gf GBregman_ProperName = mkApp (mkCId "Bregman_ProperName") []
  gf GBrinkmann_ProperName = mkApp (mkCId "Brinkmann_ProperName") []
  gf GBruhat_ProperName = mkApp (mkCId "Bruhat_ProperName") []
  gf GBruijn_ProperName = mkApp (mkCId "Bruijn_ProperName") []
  gf GBurnside_ProperName = mkApp (mkCId "Burnside_ProperName") []
  gf GCalkin_ProperName = mkApp (mkCId "Calkin_ProperName") []
  gf GCamden_ProperName = mkApp (mkCId "Camden_ProperName") []
  gf GCantor_ProperName = mkApp (mkCId "Cantor_ProperName") []
  gf GCarleson_ProperName = mkApp (mkCId "Carleson_ProperName") []
  gf GCarlyle_ProperName = mkApp (mkCId "Carlyle_ProperName") []
  gf GCarmichael_ProperName = mkApp (mkCId "Carmichael_ProperName") []
  gf GCarnot_ProperName = mkApp (mkCId "Carnot_ProperName") []
  gf GCarol_ProperName = mkApp (mkCId "Carol_ProperName") []
  gf GCartan_ProperName = mkApp (mkCId "Cartan_ProperName") []
  gf GCarter_ProperName = mkApp (mkCId "Carter_ProperName") []
  gf GCartier_ProperName = mkApp (mkCId "Cartier_ProperName") []
  gf GCauchy_ProperName = mkApp (mkCId "Cauchy_ProperName") []
  gf GCayley_ProperName = mkApp (mkCId "Cayley_ProperName") []
  gf GChang_ProperName = mkApp (mkCId "Chang_ProperName") []
  gf GChebyshev_ProperName = mkApp (mkCId "Chebyshev_ProperName") []
  gf GCheeger_ProperName = mkApp (mkCId "Cheeger_ProperName") []
  gf GChevalley_ProperName = mkApp (mkCId "Chevalley_ProperName") []
  gf GClebsch_ProperName = mkApp (mkCId "Clebsch_ProperName") []
  gf GClifford_ProperName = mkApp (mkCId "Clifford_ProperName") []
  gf GCoates_ProperName = mkApp (mkCId "Coates_ProperName") []
  gf GCohen_ProperName = mkApp (mkCId "Cohen_ProperName") []
  gf GColin_ProperName = mkApp (mkCId "Colin_ProperName") []
  gf GColinear_ProperName = mkApp (mkCId "Colinear_ProperName") []
  gf GColombeau_ProperName = mkApp (mkCId "Colombeau_ProperName") []
  gf GCox_ProperName = mkApp (mkCId "Cox_ProperName") []
  gf GCoxeter_ProperName = mkApp (mkCId "Coxeter_ProperName") []
  gf GCremona_ProperName = mkApp (mkCId "Cremona_ProperName") []
  gf GCullen_ProperName = mkApp (mkCId "Cullen_ProperName") []
  gf GCunningham_ProperName = mkApp (mkCId "Cunningham_ProperName") []
  gf GDade_ProperName = mkApp (mkCId "Dade_ProperName") []
  gf GDamm_ProperName = mkApp (mkCId "Damm_ProperName") []
  gf GDarboux_ProperName = mkApp (mkCId "Darboux_ProperName") []
  gf GDe_ProperName = mkApp (mkCId "De_ProperName") []
  gf GDedekind_ProperName = mkApp (mkCId "Dedekind_ProperName") []
  gf GDehn_ProperName = mkApp (mkCId "Dehn_ProperName") []
  gf GDelambre_ProperName = mkApp (mkCId "Delambre_ProperName") []
  gf GDeligne_ProperName = mkApp (mkCId "Deligne_ProperName") []
  gf GDelta_ProperName = mkApp (mkCId "Delta_ProperName") []
  gf GDescartes_ProperName = mkApp (mkCId "Descartes_ProperName") []
  gf GDieudonné_ProperName = mkApp (mkCId "Dieudonné_ProperName") []
  gf GDini_ProperName = mkApp (mkCId "Dini_ProperName") []
  gf GDirac_ProperName = mkApp (mkCId "Dirac_ProperName") []
  gf GDirichlet_ProperName = mkApp (mkCId "Dirichlet_ProperName") []
  gf GDolbeault_ProperName = mkApp (mkCId "Dolbeault_ProperName") []
  gf GDrinfeld_ProperName = mkApp (mkCId "Drinfeld_ProperName") []
  gf GDuflo_ProperName = mkApp (mkCId "Duflo_ProperName") []
  gf GDunkl_ProperName = mkApp (mkCId "Dunkl_ProperName") []
  gf GDunn_ProperName = mkApp (mkCId "Dunn_ProperName") []
  gf GDyck_ProperName = mkApp (mkCId "Dyck_ProperName") []
  gf GDynkin_ProperName = mkApp (mkCId "Dynkin_ProperName") []
  gf GDürer_ProperName = mkApp (mkCId "Dürer_ProperName") []
  gf GEarth_ProperName = mkApp (mkCId "Earth_ProperName") []
  gf GEberlein_ProperName = mkApp (mkCId "Eberlein_ProperName") []
  gf GErdős_ProperName = mkApp (mkCId "'Erdős_ProperName'") []
  gf GErrera_ProperName = mkApp (mkCId "Errera_ProperName") []
  gf GEuler_ProperName = mkApp (mkCId "Euler_ProperName") []
  gf GFatou_ProperName = mkApp (mkCId "Fatou_ProperName") []
  gf GFeigenbaum_ProperName = mkApp (mkCId "Feigenbaum_ProperName") []
  gf GFejér_ProperName = mkApp (mkCId "Fejér_ProperName") []
  gf GFermat_ProperName = mkApp (mkCId "Fermat_ProperName") []
  gf GFeynman_ProperName = mkApp (mkCId "Feynman_ProperName") []
  gf GFibonacci_ProperName = mkApp (mkCId "Fibonacci_ProperName") []
  gf GFock_ProperName = mkApp (mkCId "Fock_ProperName") []
  gf GFolkman_ProperName = mkApp (mkCId "Folkman_ProperName") []
  gf GFoster_ProperName = mkApp (mkCId "Foster_ProperName") []
  gf GFranklin_ProperName = mkApp (mkCId "Franklin_ProperName") []
  gf GFrattini_ProperName = mkApp (mkCId "Frattini_ProperName") []
  gf GFraïssé_ProperName = mkApp (mkCId "Fraïssé_ProperName") []
  gf GFredholm_ProperName = mkApp (mkCId "Fredholm_ProperName") []
  gf GFriedman_ProperName = mkApp (mkCId "Friedman_ProperName") []
  gf GFrobenius_ProperName = mkApp (mkCId "Frobenius_ProperName") []
  gf GFréchet_ProperName = mkApp (mkCId "Fréchet_ProperName") []
  gf GFrölicher_ProperName = mkApp (mkCId "Frölicher_ProperName") []
  gf GGalerkin_ProperName = mkApp (mkCId "Galerkin_ProperName") []
  gf GGalois_ProperName = mkApp (mkCId "Galois_ProperName") []
  gf GGerstenhaber_ProperName = mkApp (mkCId "Gerstenhaber_ProperName") []
  gf GGewirtz_ProperName = mkApp (mkCId "Gewirtz_ProperName") []
  gf GGibbs_ProperName = mkApp (mkCId "Gibbs_ProperName") []
  gf GGibrat_ProperName = mkApp (mkCId "Gibrat_ProperName") []
  gf GGiuga_ProperName = mkApp (mkCId "Giuga_ProperName") []
  gf GGolomb_ProperName = mkApp (mkCId "Golomb_ProperName") []
  gf GGosset_ProperName = mkApp (mkCId "Gosset_ProperName") []
  gf GGoursat_ProperName = mkApp (mkCId "Goursat_ProperName") []
  gf GGrassmann_ProperName = mkApp (mkCId "Grassmann_ProperName") []
  gf GGreen_ProperName = mkApp (mkCId "Green_ProperName") []
  gf GGrinberg_ProperName = mkApp (mkCId "Grinberg_ProperName") []
  gf GGromov_ProperName = mkApp (mkCId "Gromov_ProperName") []
  gf GGrothendieck_ProperName = mkApp (mkCId "Grothendieck_ProperName") []
  gf GGrötzsch_ProperName = mkApp (mkCId "Grötzsch_ProperName") []
  gf GHadwiger_ProperName = mkApp (mkCId "Hadwiger_ProperName") []
  gf GHalin_ProperName = mkApp (mkCId "Halin_ProperName") []
  gf GHall_ProperName = mkApp (mkCId "Hall_ProperName") []
  gf GHamel_ProperName = mkApp (mkCId "Hamel_ProperName") []
  gf GHanoi_ProperName = mkApp (mkCId "Hanoi_ProperName") []
  gf GHardy_ProperName = mkApp (mkCId "Hardy_ProperName") []
  gf GHarries_ProperName = mkApp (mkCId "Harries_ProperName") []
  gf GHarrop_ProperName = mkApp (mkCId "Harrop_ProperName") []
  gf GHart_ProperName = mkApp (mkCId "Hart_ProperName") []
  gf GHatzel_ProperName = mkApp (mkCId "Hatzel_ProperName") []
  gf GHausdorff_ProperName = mkApp (mkCId "Hausdorff_ProperName") []
  gf GHawkes_ProperName = mkApp (mkCId "Hawkes_ProperName") []
  gf GHeawood_ProperName = mkApp (mkCId "Heawood_ProperName") []
  gf GHecke_ProperName = mkApp (mkCId "Hecke_ProperName") []
  gf GHeisenberg_ProperName = mkApp (mkCId "Heisenberg_ProperName") []
  gf GHellinger_ProperName = mkApp (mkCId "Hellinger_ProperName") []
  gf GHenson_ProperName = mkApp (mkCId "Henson_ProperName") []
  gf GHerbrand_ProperName = mkApp (mkCId "Herbrand_ProperName") []
  gf GHerschel_ProperName = mkApp (mkCId "Herschel_ProperName") []
  gf GHeyting_ProperName = mkApp (mkCId "Heyting_ProperName") []
  gf GHilbert_ProperName = mkApp (mkCId "Hilbert_ProperName") []
  gf GHill_ProperName = mkApp (mkCId "Hill_ProperName") []
  gf GHochschild_ProperName = mkApp (mkCId "Hochschild_ProperName") []
  gf GHodge_ProperName = mkApp (mkCId "Hodge_ProperName") []
  gf GHoffman_ProperName = mkApp (mkCId "Hoffman_ProperName") []
  gf GHolt_ProperName = mkApp (mkCId "Holt_ProperName") []
  gf GHosoya_ProperName = mkApp (mkCId "Hosoya_ProperName") []
  gf GHurwitz_ProperName = mkApp (mkCId "Hurwitz_ProperName") []
  gf GIkeda_ProperName = mkApp (mkCId "Ikeda_ProperName") []
  gf GIwasawa_ProperName = mkApp (mkCId "Iwasawa_ProperName") []
  gf GJaccard_ProperName = mkApp (mkCId "Jaccard_ProperName") []
  gf GJacobi_ProperName = mkApp (mkCId "Jacobi_ProperName") []
  gf GJacobson_ProperName = mkApp (mkCId "Jacobson_ProperName") []
  gf GJaffard_ProperName = mkApp (mkCId "Jaffard_ProperName") []
  gf GJohnson_ProperName = mkApp (mkCId "Johnson_ProperName") []
  gf GJordan_ProperName = mkApp (mkCId "Jordan_ProperName") []
  gf GJulia_ProperName = mkApp (mkCId "Julia_ProperName") []
  gf GJónsson_ProperName = mkApp (mkCId "Jónsson_ProperName") []
  gf GKakeya_ProperName = mkApp (mkCId "Kakeya_ProperName") []
  gf GKalman_ProperName = mkApp (mkCId "Kalman_ProperName") []
  gf GKan_ProperName = mkApp (mkCId "Kan_ProperName") []
  gf GKane_ProperName = mkApp (mkCId "Kane_ProperName") []
  gf GKaprekar_ProperName = mkApp (mkCId "Kaprekar_ProperName") []
  gf GKaroubi_ProperName = mkApp (mkCId "Karoubi_ProperName") []
  gf GKasch_ProperName = mkApp (mkCId "Kasch_ProperName") []
  gf GKeith_ProperName = mkApp (mkCId "Keith_ProperName") []
  gf GKempe_ProperName = mkApp (mkCId "Kempe_ProperName") []
  gf GKent_ProperName = mkApp (mkCId "Kent_ProperName") []
  gf GKhinchin_ProperName = mkApp (mkCId "Khinchin_ProperName") []
  gf GKittell_ProperName = mkApp (mkCId "Kittell_ProperName") []
  gf GKleene_ProperName = mkApp (mkCId "Kleene_ProperName") []
  gf GKlein_ProperName = mkApp (mkCId "Klein_ProperName") []
  gf GKleisli_ProperName = mkApp (mkCId "Kleisli_ProperName") []
  gf GKneser_ProperName = mkApp (mkCId "Kneser_ProperName") []
  gf GKnödel_ProperName = mkApp (mkCId "Knödel_ProperName") []
  gf GKodaira_ProperName = mkApp (mkCId "Kodaira_ProperName") []
  gf GKolmogorov_ProperName = mkApp (mkCId "Kolmogorov_ProperName") []
  gf GKorovkin_ProperName = mkApp (mkCId "Korovkin_ProperName") []
  gf GKoszul_ProperName = mkApp (mkCId "Koszul_ProperName") []
  gf GKrein_ProperName = mkApp (mkCId "Krein_ProperName") []
  gf GKripke_ProperName = mkApp (mkCId "Kripke_ProperName") []
  gf GKronecker_ProperName = mkApp (mkCId "Kronecker_ProperName") []
  gf GKrylov_ProperName = mkApp (mkCId "Krylov_ProperName") []
  gf GKummer_ProperName = mkApp (mkCId "Kummer_ProperName") []
  gf GKynea_ProperName = mkApp (mkCId "Kynea_ProperName") []
  gf GKünneth_ProperName = mkApp (mkCId "Künneth_ProperName") []
  gf GLagrange_ProperName = mkApp (mkCId "Lagrange_ProperName") []
  gf GLaue_ProperName = mkApp (mkCId "Laue_ProperName") []
  gf GLawvere_ProperName = mkApp (mkCId "Lawvere_ProperName") []
  gf GLebesgue_ProperName = mkApp (mkCId "Lebesgue_ProperName") []
  gf GLegendre_ProperName = mkApp (mkCId "Legendre_ProperName") []
  gf GLeibniz_ProperName = mkApp (mkCId "Leibniz_ProperName") []
  gf GLevenshtein_ProperName = mkApp (mkCId "Levenshtein_ProperName") []
  gf GLevi_ProperName = mkApp (mkCId "Levi_ProperName") []
  gf GLeyland_ProperName = mkApp (mkCId "Leyland_ProperName") []
  gf GLie_ProperName = mkApp (mkCId "Lie_ProperName") []
  gf GLindelöf_ProperName = mkApp (mkCId "Lindelöf_ProperName") []
  gf GLipschitz_ProperName = mkApp (mkCId "Lipschitz_ProperName") []
  gf GLissajous_ProperName = mkApp (mkCId "Lissajous_ProperName") []
  gf GLiverTox_ProperName = mkApp (mkCId "LiverTox_ProperName") []
  gf GLjubljana_ProperName = mkApp (mkCId "Ljubljana_ProperName") []
  gf GLoewner_ProperName = mkApp (mkCId "Loewner_ProperName") []
  gf GLondon_ProperName = mkApp (mkCId "London_ProperName") []
  gf GLorentz_ProperName = mkApp (mkCId "Lorentz_ProperName") []
  gf GLoupekine_ProperName = mkApp (mkCId "Loupekine_ProperName") []
  gf GLovász_ProperName = mkApp (mkCId "Lovász_ProperName") []
  gf GLucas_ProperName = mkApp (mkCId "Lucas_ProperName") []
  gf GLyapunov_ProperName = mkApp (mkCId "Lyapunov_ProperName") []
  gf GMacdonald_ProperName = mkApp (mkCId "Macdonald_ProperName") []
  gf GMahalanobis_ProperName = mkApp (mkCId "Mahalanobis_ProperName") []
  gf GMahler_ProperName = mkApp (mkCId "Mahler_ProperName") []
  gf GMandelbrot_ProperName = mkApp (mkCId "Mandelbrot_ProperName") []
  gf GManin_ProperName = mkApp (mkCId "Manin_ProperName") []
  gf GMarkov_ProperName = mkApp (mkCId "Markov_ProperName") []
  gf GMazur_ProperName = mkApp (mkCId "Mazur_ProperName") []
  gf GMcGee_ProperName = mkApp (mkCId "McGee_ProperName") []
  gf GMcKay_ProperName = mkApp (mkCId "McKay_ProperName") []
  gf GMcLaughlin_ProperName = mkApp (mkCId "McLaughlin_ProperName") []
  gf GMeredith_ProperName = mkApp (mkCId "Meredith_ProperName") []
  gf GMeringer_ProperName = mkApp (mkCId "Meringer_ProperName") []
  gf GMersenne_ProperName = mkApp (mkCId "Mersenne_ProperName") []
  gf GMeyniel_ProperName = mkApp (mkCId "Meyniel_ProperName") []
  gf GMiller_ProperName = mkApp (mkCId "Miller_ProperName") []
  gf GMilnor_ProperName = mkApp (mkCId "Milnor_ProperName") []
  gf GMinkowski_ProperName = mkApp (mkCId "Minkowski_ProperName") []
  gf GMontel_ProperName = mkApp (mkCId "Montel_ProperName") []
  gf GMoore_ProperName = mkApp (mkCId "Moore_ProperName") []
  gf GMorgan_ProperName = mkApp (mkCId "Morgan_ProperName") []
  gf GMorse_ProperName = mkApp (mkCId "Morse_ProperName") []
  gf GMoser_ProperName = mkApp (mkCId "Moser_ProperName") []
  gf GMotzkin_ProperName = mkApp (mkCId "Motzkin_ProperName") []
  gf GMumford_ProperName = mkApp (mkCId "Mumford_ProperName") []
  gf GMunn_ProperName = mkApp (mkCId "Munn_ProperName") []
  gf GMöbius_ProperName = mkApp (mkCId "Möbius_ProperName") []
  gf GMünchhausen_ProperName = mkApp (mkCId "Münchhausen_ProperName") []
  gf GNagata_ProperName = mkApp (mkCId "Nagata_ProperName") []
  gf GNakajima_ProperName = mkApp (mkCId "Nakajima_ProperName") []
  gf GNarayana_ProperName = mkApp (mkCId "Narayana_ProperName") []
  gf GNeumann_ProperName = mkApp (mkCId "Neumann_ProperName") []
  gf GNewton_ProperName = mkApp (mkCId "Newton_ProperName") []
  gf GOckham_ProperName = mkApp (mkCId "Ockham_ProperName") []
  gf GPaley_ProperName = mkApp (mkCId "Paley_ProperName") []
  gf GPareto_ProperName = mkApp (mkCId "Pareto_ProperName") []
  gf GParrondo_ProperName = mkApp (mkCId "Parrondo_ProperName") []
  gf GPatricia_ProperName = mkApp (mkCId "Patricia_ProperName") []
  gf GPeano_ProperName = mkApp (mkCId "Peano_ProperName") []
  gf GPearcey_ProperName = mkApp (mkCId "Pearcey_ProperName") []
  gf GPenrose_ProperName = mkApp (mkCId "Penrose_ProperName") []
  gf GPerkel_ProperName = mkApp (mkCId "Perkel_ProperName") []
  gf GPerrin_ProperName = mkApp (mkCId "Perrin_ProperName") []
  gf GPetersen_ProperName = mkApp (mkCId "Petersen_ProperName") []
  gf GPetri_ProperName = mkApp (mkCId "Petri_ProperName") []
  gf GPettis_ProperName = mkApp (mkCId "Pettis_ProperName") []
  gf GPicard_ProperName = mkApp (mkCId "Picard_ProperName") []
  gf GPippard_ProperName = mkApp (mkCId "Pippard_ProperName") []
  gf GPoincaré_ProperName = mkApp (mkCId "Poincaré_ProperName") []
  gf GPoisson_ProperName = mkApp (mkCId "Poisson_ProperName") []
  gf GPoussin_ProperName = mkApp (mkCId "Poussin_ProperName") []
  gf GPtolemy_ProperName = mkApp (mkCId "Ptolemy_ProperName") []
  gf GPuig_ProperName = mkApp (mkCId "Puig_ProperName") []
  gf GRado_ProperName = mkApp (mkCId "Rado_ProperName") []
  gf GRadon_ProperName = mkApp (mkCId "Radon_ProperName") []
  gf GRajchman_ProperName = mkApp (mkCId "Rajchman_ProperName") []
  gf GRamanujan_ProperName = mkApp (mkCId "Ramanujan_ProperName") []
  gf GRees_ProperName = mkApp (mkCId "Rees_ProperName") []
  gf GReeve_ProperName = mkApp (mkCId "Reeve_ProperName") []
  gf GReinhardt_ProperName = mkApp (mkCId "Reinhardt_ProperName") []
  gf GRham_ProperName = mkApp (mkCId "Rham_ProperName") []
  gf GRiemann_ProperName = mkApp (mkCId "Riemann_ProperName") []
  gf GRiesel_ProperName = mkApp (mkCId "Riesel_ProperName") []
  gf GRiesz_ProperName = mkApp (mkCId "Riesz_ProperName") []
  gf GRobbins_ProperName = mkApp (mkCId "Robbins_ProperName") []
  gf GRobertson_ProperName = mkApp (mkCId "Robertson_ProperName") []
  gf GRosenbrock_ProperName = mkApp (mkCId "Rosenbrock_ProperName") []
  gf GRoyle_ProperName = mkApp (mkCId "Royle_ProperName") []
  gf GSchauder_ProperName = mkApp (mkCId "Schauder_ProperName") []
  gf GSchläfli_ProperName = mkApp (mkCId "Schläfli_ProperName") []
  gf GSchreier_ProperName = mkApp (mkCId "Schreier_ProperName") []
  gf GSchrödinger_ProperName = mkApp (mkCId "Schrödinger_ProperName") []
  gf GSchubert_ProperName = mkApp (mkCId "Schubert_ProperName") []
  gf GSchur_ProperName = mkApp (mkCId "Schur_ProperName") []
  gf GSchwartz_ProperName = mkApp (mkCId "Schwartz_ProperName") []
  gf GSchwarz_ProperName = mkApp (mkCId "Schwarz_ProperName") []
  gf GScott_ProperName = mkApp (mkCId "Scott_ProperName") []
  gf GSelberg_ProperName = mkApp (mkCId "Selberg_ProperName") []
  gf GSerre_ProperName = mkApp (mkCId "Serre_ProperName") []
  gf GShannon_ProperName = mkApp (mkCId "Shannon_ProperName") []
  gf GShelah_ProperName = mkApp (mkCId "Shelah_ProperName") []
  gf GShimura_ProperName = mkApp (mkCId "Shimura_ProperName") []
  gf GShrikhande_ProperName = mkApp (mkCId "Shrikhande_ProperName") []
  gf GSiegel_ProperName = mkApp (mkCId "Siegel_ProperName") []
  gf GSierpinski_ProperName = mkApp (mkCId "Sierpinski_ProperName") []
  gf GSitter_ProperName = mkApp (mkCId "Sitter_ProperName") []
  gf GSlater_ProperName = mkApp (mkCId "Slater_ProperName") []
  gf GSmith_ProperName = mkApp (mkCId "Smith_ProperName") []
  gf GSousselier_ProperName = mkApp (mkCId "Sousselier_ProperName") []
  gf GSpencer_ProperName = mkApp (mkCId "Spencer_ProperName") []
  gf GStark_ProperName = mkApp (mkCId "Stark_ProperName") []
  gf GSteenrod_ProperName = mkApp (mkCId "Steenrod_ProperName") []
  gf GSteinitz_ProperName = mkApp (mkCId "Steinitz_ProperName") []
  gf GStepanoff_ProperName = mkApp (mkCId "Stepanoff_ProperName") []
  gf GStiefel_ProperName = mkApp (mkCId "Stiefel_ProperName") []
  gf GStirling_ProperName = mkApp (mkCId "Stirling_ProperName") []
  gf GStone_ProperName = mkApp (mkCId "Stone_ProperName") []
  gf GSuslin_ProperName = mkApp (mkCId "Suslin_ProperName") []
  gf GSuzuki_ProperName = mkApp (mkCId "Suzuki_ProperName") []
  gf GSylow_ProperName = mkApp (mkCId "Sylow_ProperName") []
  gf GSylvester_ProperName = mkApp (mkCId "Sylvester_ProperName") []
  gf GSzekeres_ProperName = mkApp (mkCId "Szekeres_ProperName") []
  gf GSárközy_ProperName = mkApp (mkCId "Sárközy_ProperName") []
  gf GSørensen_ProperName = mkApp (mkCId "Sørensen_ProperName") []
  gf GTaleb_ProperName = mkApp (mkCId "Taleb_ProperName") []
  gf GTarski_ProperName = mkApp (mkCId "Tarski_ProperName") []
  gf GTate_ProperName = mkApp (mkCId "Tate_ProperName") []
  gf GTaylor_ProperName = mkApp (mkCId "Taylor_ProperName") []
  gf GThabit_ProperName = mkApp (mkCId "Thabit_ProperName") []
  gf GThom_ProperName = mkApp (mkCId "Thom_ProperName") []
  gf GThomassen_ProperName = mkApp (mkCId "Thomassen_ProperName") []
  gf GThompson_ProperName = mkApp (mkCId "Thompson_ProperName") []
  gf GThue_ProperName = mkApp (mkCId "Thue_ProperName") []
  gf GTime_ProperName = mkApp (mkCId "Time_ProperName") []
  gf GTrofimov_ProperName = mkApp (mkCId "Trofimov_ProperName") []
  gf GTrémaux_ProperName = mkApp (mkCId "Trémaux_ProperName") []
  gf GTurán_ProperName = mkApp (mkCId "Turán_ProperName") []
  gf GTutte_ProperName = mkApp (mkCId "Tutte_ProperName") []
  gf GTweedie_ProperName = mkApp (mkCId "Tweedie_ProperName") []
  gf GTychonoff_ProperName = mkApp (mkCId "Tychonoff_ProperName") []
  gf GUrysohn_ProperName = mkApp (mkCId "Urysohn_ProperName") []
  gf GVandermonde_ProperName = mkApp (mkCId "Vandermonde_ProperName") []
  gf GVerdière_ProperName = mkApp (mkCId "Verdière_ProperName") []
  gf GVickrey_ProperName = mkApp (mkCId "Vickrey_ProperName") []
  gf GWallman_ProperName = mkApp (mkCId "Wallman_ProperName") []
  gf GWalther_ProperName = mkApp (mkCId "Walther_ProperName") []
  gf GWeibull_ProperName = mkApp (mkCId "Weibull_ProperName") []
  gf GWeierstrass_ProperName = mkApp (mkCId "Weierstrass_ProperName") []
  gf GWeil_ProperName = mkApp (mkCId "Weil_ProperName") []
  gf GWeingarten_ProperName = mkApp (mkCId "Weingarten_ProperName") []
  gf GWells_ProperName = mkApp (mkCId "Wells_ProperName") []
  gf GWeyl_ProperName = mkApp (mkCId "Weyl_ProperName") []
  gf GWhewell_ProperName = mkApp (mkCId "Whewell_ProperName") []
  gf GWieferich_ProperName = mkApp (mkCId "Wieferich_ProperName") []
  gf GWitt_ProperName = mkApp (mkCId "Witt_ProperName") []
  gf GWoodall_ProperName = mkApp (mkCId "Woodall_ProperName") []
  gf GWoodin_ProperName = mkApp (mkCId "Woodin_ProperName") []
  gf GYoneda_ProperName = mkApp (mkCId "Yoneda_ProperName") []
  gf GZamfirescu_ProperName = mkApp (mkCId "Zamfirescu_ProperName") []
  gf GZaslavskii_ProperName = mkApp (mkCId "Zaslavskii_ProperName") []
  gf GZassenhaus_ProperName = mkApp (mkCId "Zassenhaus_ProperName") []
  gf GZeisel_ProperName = mkApp (mkCId "Zeisel_ProperName") []
  gf GZhegalkin_ProperName = mkApp (mkCId "Zhegalkin_ProperName") []
  gf GZinbiel_ProperName = mkApp (mkCId "Zinbiel_ProperName") []
  gf GZorn_ProperName = mkApp (mkCId "Zorn_ProperName") []
  gf GZuckerman_ProperName = mkApp (mkCId "Zuckerman_ProperName") []
  gf GŁojasiewicz_ProperName = mkApp (mkCId "'Łojasiewicz_ProperName'") []
  gf GŁukasiewicz_ProperName = mkApp (mkCId "'Łukasiewicz_ProperName'") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Accor_ProperName" -> GAccor_ProperName 
      Just (i,[]) | i == mkCId "Ackermann_ProperName" -> GAckermann_ProperName 
      Just (i,[]) | i == mkCId "Adams_ProperName" -> GAdams_ProperName 
      Just (i,[]) | i == mkCId "Adinkra_ProperName" -> GAdinkra_ProperName 
      Just (i,[]) | i == mkCId "Ahlfors_ProperName" -> GAhlfors_ProperName 
      Just (i,[]) | i == mkCId "Albert_ProperName" -> GAlbert_ProperName 
      Just (i,[]) | i == mkCId "Alexander_ProperName" -> GAlexander_ProperName 
      Just (i,[]) | i == mkCId "Alexandroff_ProperName" -> GAlexandroff_ProperName 
      Just (i,[]) | i == mkCId "Alexandrov_ProperName" -> GAlexandrov_ProperName 
      Just (i,[]) | i == mkCId "Andrásfai_ProperName" -> GAndrásfai_ProperName 
      Just (i,[]) | i == mkCId "Anosov_ProperName" -> GAnosov_ProperName 
      Just (i,[]) | i == mkCId "Arf_ProperName" -> GArf_ProperName 
      Just (i,[]) | i == mkCId "Aronszajn_ProperName" -> GAronszajn_ProperName 
      Just (i,[]) | i == mkCId "Artin_ProperName" -> GArtin_ProperName 
      Just (i,[]) | i == mkCId "Aschbacher_ProperName" -> GAschbacher_ProperName 
      Just (i,[]) | i == mkCId "Ashtekar_ProperName" -> GAshtekar_ProperName 
      Just (i,[]) | i == mkCId "Atiyah_ProperName" -> GAtiyah_ProperName 
      Just (i,[]) | i == mkCId "Azumaya_ProperName" -> GAzumaya_ProperName 
      Just (i,[]) | i == mkCId "Baer_ProperName" -> GBaer_ProperName 
      Just (i,[]) | i == mkCId "Banach_ProperName" -> GBanach_ProperName 
      Just (i,[]) | i == mkCId "Barnes_ProperName" -> GBarnes_ProperName 
      Just (i,[]) | i == mkCId "Bayes_ProperName" -> GBayes_ProperName 
      Just (i,[]) | i == mkCId "Beltrami_ProperName" -> GBeltrami_ProperName 
      Just (i,[]) | i == mkCId "Bendixson_ProperName" -> GBendixson_ProperName 
      Just (i,[]) | i == mkCId "Benini_ProperName" -> GBenini_ProperName 
      Just (i,[]) | i == mkCId "Benktander_ProperName" -> GBenktander_ProperName 
      Just (i,[]) | i == mkCId "Berge_ProperName" -> GBerge_ProperName 
      Just (i,[]) | i == mkCId "Berkeley_ProperName" -> GBerkeley_ProperName 
      Just (i,[]) | i == mkCId "Bernoulli_ProperName" -> GBernoulli_ProperName 
      Just (i,[]) | i == mkCId "Besov_ProperName" -> GBesov_ProperName 
      Just (i,[]) | i == mkCId "Bhattacharyya_ProperName" -> GBhattacharyya_ProperName 
      Just (i,[]) | i == mkCId "Bianchi_ProperName" -> GBianchi_ProperName 
      Just (i,[]) | i == mkCId "Bidiakis_ProperName" -> GBidiakis_ProperName 
      Just (i,[]) | i == mkCId "Birkhoff_ProperName" -> GBirkhoff_ProperName 
      Just (i,[]) | i == mkCId "'Blanuša_ProperName'" -> GBlanuša_ProperName 
      Just (i,[]) | i == mkCId "Blum_ProperName" -> GBlum_ProperName 
      Just (i,[]) | i == mkCId "Bochner_ProperName" -> GBochner_ProperName 
      Just (i,[]) | i == mkCId "Bockstein_ProperName" -> GBockstein_ProperName 
      Just (i,[]) | i == mkCId "Borel_ProperName" -> GBorel_ProperName 
      Just (i,[]) | i == mkCId "Bott_ProperName" -> GBott_ProperName 
      Just (i,[]) | i == mkCId "Brandt_ProperName" -> GBrandt_ProperName 
      Just (i,[]) | i == mkCId "Brauer_ProperName" -> GBrauer_ProperName 
      Just (i,[]) | i == mkCId "Brauner_ProperName" -> GBrauner_ProperName 
      Just (i,[]) | i == mkCId "Bregman_ProperName" -> GBregman_ProperName 
      Just (i,[]) | i == mkCId "Brinkmann_ProperName" -> GBrinkmann_ProperName 
      Just (i,[]) | i == mkCId "Bruhat_ProperName" -> GBruhat_ProperName 
      Just (i,[]) | i == mkCId "Bruijn_ProperName" -> GBruijn_ProperName 
      Just (i,[]) | i == mkCId "Burnside_ProperName" -> GBurnside_ProperName 
      Just (i,[]) | i == mkCId "Calkin_ProperName" -> GCalkin_ProperName 
      Just (i,[]) | i == mkCId "Camden_ProperName" -> GCamden_ProperName 
      Just (i,[]) | i == mkCId "Cantor_ProperName" -> GCantor_ProperName 
      Just (i,[]) | i == mkCId "Carleson_ProperName" -> GCarleson_ProperName 
      Just (i,[]) | i == mkCId "Carlyle_ProperName" -> GCarlyle_ProperName 
      Just (i,[]) | i == mkCId "Carmichael_ProperName" -> GCarmichael_ProperName 
      Just (i,[]) | i == mkCId "Carnot_ProperName" -> GCarnot_ProperName 
      Just (i,[]) | i == mkCId "Carol_ProperName" -> GCarol_ProperName 
      Just (i,[]) | i == mkCId "Cartan_ProperName" -> GCartan_ProperName 
      Just (i,[]) | i == mkCId "Carter_ProperName" -> GCarter_ProperName 
      Just (i,[]) | i == mkCId "Cartier_ProperName" -> GCartier_ProperName 
      Just (i,[]) | i == mkCId "Cauchy_ProperName" -> GCauchy_ProperName 
      Just (i,[]) | i == mkCId "Cayley_ProperName" -> GCayley_ProperName 
      Just (i,[]) | i == mkCId "Chang_ProperName" -> GChang_ProperName 
      Just (i,[]) | i == mkCId "Chebyshev_ProperName" -> GChebyshev_ProperName 
      Just (i,[]) | i == mkCId "Cheeger_ProperName" -> GCheeger_ProperName 
      Just (i,[]) | i == mkCId "Chevalley_ProperName" -> GChevalley_ProperName 
      Just (i,[]) | i == mkCId "Clebsch_ProperName" -> GClebsch_ProperName 
      Just (i,[]) | i == mkCId "Clifford_ProperName" -> GClifford_ProperName 
      Just (i,[]) | i == mkCId "Coates_ProperName" -> GCoates_ProperName 
      Just (i,[]) | i == mkCId "Cohen_ProperName" -> GCohen_ProperName 
      Just (i,[]) | i == mkCId "Colin_ProperName" -> GColin_ProperName 
      Just (i,[]) | i == mkCId "Colinear_ProperName" -> GColinear_ProperName 
      Just (i,[]) | i == mkCId "Colombeau_ProperName" -> GColombeau_ProperName 
      Just (i,[]) | i == mkCId "Cox_ProperName" -> GCox_ProperName 
      Just (i,[]) | i == mkCId "Coxeter_ProperName" -> GCoxeter_ProperName 
      Just (i,[]) | i == mkCId "Cremona_ProperName" -> GCremona_ProperName 
      Just (i,[]) | i == mkCId "Cullen_ProperName" -> GCullen_ProperName 
      Just (i,[]) | i == mkCId "Cunningham_ProperName" -> GCunningham_ProperName 
      Just (i,[]) | i == mkCId "Dade_ProperName" -> GDade_ProperName 
      Just (i,[]) | i == mkCId "Damm_ProperName" -> GDamm_ProperName 
      Just (i,[]) | i == mkCId "Darboux_ProperName" -> GDarboux_ProperName 
      Just (i,[]) | i == mkCId "De_ProperName" -> GDe_ProperName 
      Just (i,[]) | i == mkCId "Dedekind_ProperName" -> GDedekind_ProperName 
      Just (i,[]) | i == mkCId "Dehn_ProperName" -> GDehn_ProperName 
      Just (i,[]) | i == mkCId "Delambre_ProperName" -> GDelambre_ProperName 
      Just (i,[]) | i == mkCId "Deligne_ProperName" -> GDeligne_ProperName 
      Just (i,[]) | i == mkCId "Delta_ProperName" -> GDelta_ProperName 
      Just (i,[]) | i == mkCId "Descartes_ProperName" -> GDescartes_ProperName 
      Just (i,[]) | i == mkCId "Dieudonné_ProperName" -> GDieudonné_ProperName 
      Just (i,[]) | i == mkCId "Dini_ProperName" -> GDini_ProperName 
      Just (i,[]) | i == mkCId "Dirac_ProperName" -> GDirac_ProperName 
      Just (i,[]) | i == mkCId "Dirichlet_ProperName" -> GDirichlet_ProperName 
      Just (i,[]) | i == mkCId "Dolbeault_ProperName" -> GDolbeault_ProperName 
      Just (i,[]) | i == mkCId "Drinfeld_ProperName" -> GDrinfeld_ProperName 
      Just (i,[]) | i == mkCId "Duflo_ProperName" -> GDuflo_ProperName 
      Just (i,[]) | i == mkCId "Dunkl_ProperName" -> GDunkl_ProperName 
      Just (i,[]) | i == mkCId "Dunn_ProperName" -> GDunn_ProperName 
      Just (i,[]) | i == mkCId "Dyck_ProperName" -> GDyck_ProperName 
      Just (i,[]) | i == mkCId "Dynkin_ProperName" -> GDynkin_ProperName 
      Just (i,[]) | i == mkCId "Dürer_ProperName" -> GDürer_ProperName 
      Just (i,[]) | i == mkCId "Earth_ProperName" -> GEarth_ProperName 
      Just (i,[]) | i == mkCId "Eberlein_ProperName" -> GEberlein_ProperName 
      Just (i,[]) | i == mkCId "'Erdős_ProperName'" -> GErdős_ProperName 
      Just (i,[]) | i == mkCId "Errera_ProperName" -> GErrera_ProperName 
      Just (i,[]) | i == mkCId "Euler_ProperName" -> GEuler_ProperName 
      Just (i,[]) | i == mkCId "Fatou_ProperName" -> GFatou_ProperName 
      Just (i,[]) | i == mkCId "Feigenbaum_ProperName" -> GFeigenbaum_ProperName 
      Just (i,[]) | i == mkCId "Fejér_ProperName" -> GFejér_ProperName 
      Just (i,[]) | i == mkCId "Fermat_ProperName" -> GFermat_ProperName 
      Just (i,[]) | i == mkCId "Feynman_ProperName" -> GFeynman_ProperName 
      Just (i,[]) | i == mkCId "Fibonacci_ProperName" -> GFibonacci_ProperName 
      Just (i,[]) | i == mkCId "Fock_ProperName" -> GFock_ProperName 
      Just (i,[]) | i == mkCId "Folkman_ProperName" -> GFolkman_ProperName 
      Just (i,[]) | i == mkCId "Foster_ProperName" -> GFoster_ProperName 
      Just (i,[]) | i == mkCId "Franklin_ProperName" -> GFranklin_ProperName 
      Just (i,[]) | i == mkCId "Frattini_ProperName" -> GFrattini_ProperName 
      Just (i,[]) | i == mkCId "Fraïssé_ProperName" -> GFraïssé_ProperName 
      Just (i,[]) | i == mkCId "Fredholm_ProperName" -> GFredholm_ProperName 
      Just (i,[]) | i == mkCId "Friedman_ProperName" -> GFriedman_ProperName 
      Just (i,[]) | i == mkCId "Frobenius_ProperName" -> GFrobenius_ProperName 
      Just (i,[]) | i == mkCId "Fréchet_ProperName" -> GFréchet_ProperName 
      Just (i,[]) | i == mkCId "Frölicher_ProperName" -> GFrölicher_ProperName 
      Just (i,[]) | i == mkCId "Galerkin_ProperName" -> GGalerkin_ProperName 
      Just (i,[]) | i == mkCId "Galois_ProperName" -> GGalois_ProperName 
      Just (i,[]) | i == mkCId "Gerstenhaber_ProperName" -> GGerstenhaber_ProperName 
      Just (i,[]) | i == mkCId "Gewirtz_ProperName" -> GGewirtz_ProperName 
      Just (i,[]) | i == mkCId "Gibbs_ProperName" -> GGibbs_ProperName 
      Just (i,[]) | i == mkCId "Gibrat_ProperName" -> GGibrat_ProperName 
      Just (i,[]) | i == mkCId "Giuga_ProperName" -> GGiuga_ProperName 
      Just (i,[]) | i == mkCId "Golomb_ProperName" -> GGolomb_ProperName 
      Just (i,[]) | i == mkCId "Gosset_ProperName" -> GGosset_ProperName 
      Just (i,[]) | i == mkCId "Goursat_ProperName" -> GGoursat_ProperName 
      Just (i,[]) | i == mkCId "Grassmann_ProperName" -> GGrassmann_ProperName 
      Just (i,[]) | i == mkCId "Green_ProperName" -> GGreen_ProperName 
      Just (i,[]) | i == mkCId "Grinberg_ProperName" -> GGrinberg_ProperName 
      Just (i,[]) | i == mkCId "Gromov_ProperName" -> GGromov_ProperName 
      Just (i,[]) | i == mkCId "Grothendieck_ProperName" -> GGrothendieck_ProperName 
      Just (i,[]) | i == mkCId "Grötzsch_ProperName" -> GGrötzsch_ProperName 
      Just (i,[]) | i == mkCId "Hadwiger_ProperName" -> GHadwiger_ProperName 
      Just (i,[]) | i == mkCId "Halin_ProperName" -> GHalin_ProperName 
      Just (i,[]) | i == mkCId "Hall_ProperName" -> GHall_ProperName 
      Just (i,[]) | i == mkCId "Hamel_ProperName" -> GHamel_ProperName 
      Just (i,[]) | i == mkCId "Hanoi_ProperName" -> GHanoi_ProperName 
      Just (i,[]) | i == mkCId "Hardy_ProperName" -> GHardy_ProperName 
      Just (i,[]) | i == mkCId "Harries_ProperName" -> GHarries_ProperName 
      Just (i,[]) | i == mkCId "Harrop_ProperName" -> GHarrop_ProperName 
      Just (i,[]) | i == mkCId "Hart_ProperName" -> GHart_ProperName 
      Just (i,[]) | i == mkCId "Hatzel_ProperName" -> GHatzel_ProperName 
      Just (i,[]) | i == mkCId "Hausdorff_ProperName" -> GHausdorff_ProperName 
      Just (i,[]) | i == mkCId "Hawkes_ProperName" -> GHawkes_ProperName 
      Just (i,[]) | i == mkCId "Heawood_ProperName" -> GHeawood_ProperName 
      Just (i,[]) | i == mkCId "Hecke_ProperName" -> GHecke_ProperName 
      Just (i,[]) | i == mkCId "Heisenberg_ProperName" -> GHeisenberg_ProperName 
      Just (i,[]) | i == mkCId "Hellinger_ProperName" -> GHellinger_ProperName 
      Just (i,[]) | i == mkCId "Henson_ProperName" -> GHenson_ProperName 
      Just (i,[]) | i == mkCId "Herbrand_ProperName" -> GHerbrand_ProperName 
      Just (i,[]) | i == mkCId "Herschel_ProperName" -> GHerschel_ProperName 
      Just (i,[]) | i == mkCId "Heyting_ProperName" -> GHeyting_ProperName 
      Just (i,[]) | i == mkCId "Hilbert_ProperName" -> GHilbert_ProperName 
      Just (i,[]) | i == mkCId "Hill_ProperName" -> GHill_ProperName 
      Just (i,[]) | i == mkCId "Hochschild_ProperName" -> GHochschild_ProperName 
      Just (i,[]) | i == mkCId "Hodge_ProperName" -> GHodge_ProperName 
      Just (i,[]) | i == mkCId "Hoffman_ProperName" -> GHoffman_ProperName 
      Just (i,[]) | i == mkCId "Holt_ProperName" -> GHolt_ProperName 
      Just (i,[]) | i == mkCId "Hosoya_ProperName" -> GHosoya_ProperName 
      Just (i,[]) | i == mkCId "Hurwitz_ProperName" -> GHurwitz_ProperName 
      Just (i,[]) | i == mkCId "Ikeda_ProperName" -> GIkeda_ProperName 
      Just (i,[]) | i == mkCId "Iwasawa_ProperName" -> GIwasawa_ProperName 
      Just (i,[]) | i == mkCId "Jaccard_ProperName" -> GJaccard_ProperName 
      Just (i,[]) | i == mkCId "Jacobi_ProperName" -> GJacobi_ProperName 
      Just (i,[]) | i == mkCId "Jacobson_ProperName" -> GJacobson_ProperName 
      Just (i,[]) | i == mkCId "Jaffard_ProperName" -> GJaffard_ProperName 
      Just (i,[]) | i == mkCId "Johnson_ProperName" -> GJohnson_ProperName 
      Just (i,[]) | i == mkCId "Jordan_ProperName" -> GJordan_ProperName 
      Just (i,[]) | i == mkCId "Julia_ProperName" -> GJulia_ProperName 
      Just (i,[]) | i == mkCId "Jónsson_ProperName" -> GJónsson_ProperName 
      Just (i,[]) | i == mkCId "Kakeya_ProperName" -> GKakeya_ProperName 
      Just (i,[]) | i == mkCId "Kalman_ProperName" -> GKalman_ProperName 
      Just (i,[]) | i == mkCId "Kan_ProperName" -> GKan_ProperName 
      Just (i,[]) | i == mkCId "Kane_ProperName" -> GKane_ProperName 
      Just (i,[]) | i == mkCId "Kaprekar_ProperName" -> GKaprekar_ProperName 
      Just (i,[]) | i == mkCId "Karoubi_ProperName" -> GKaroubi_ProperName 
      Just (i,[]) | i == mkCId "Kasch_ProperName" -> GKasch_ProperName 
      Just (i,[]) | i == mkCId "Keith_ProperName" -> GKeith_ProperName 
      Just (i,[]) | i == mkCId "Kempe_ProperName" -> GKempe_ProperName 
      Just (i,[]) | i == mkCId "Kent_ProperName" -> GKent_ProperName 
      Just (i,[]) | i == mkCId "Khinchin_ProperName" -> GKhinchin_ProperName 
      Just (i,[]) | i == mkCId "Kittell_ProperName" -> GKittell_ProperName 
      Just (i,[]) | i == mkCId "Kleene_ProperName" -> GKleene_ProperName 
      Just (i,[]) | i == mkCId "Klein_ProperName" -> GKlein_ProperName 
      Just (i,[]) | i == mkCId "Kleisli_ProperName" -> GKleisli_ProperName 
      Just (i,[]) | i == mkCId "Kneser_ProperName" -> GKneser_ProperName 
      Just (i,[]) | i == mkCId "Knödel_ProperName" -> GKnödel_ProperName 
      Just (i,[]) | i == mkCId "Kodaira_ProperName" -> GKodaira_ProperName 
      Just (i,[]) | i == mkCId "Kolmogorov_ProperName" -> GKolmogorov_ProperName 
      Just (i,[]) | i == mkCId "Korovkin_ProperName" -> GKorovkin_ProperName 
      Just (i,[]) | i == mkCId "Koszul_ProperName" -> GKoszul_ProperName 
      Just (i,[]) | i == mkCId "Krein_ProperName" -> GKrein_ProperName 
      Just (i,[]) | i == mkCId "Kripke_ProperName" -> GKripke_ProperName 
      Just (i,[]) | i == mkCId "Kronecker_ProperName" -> GKronecker_ProperName 
      Just (i,[]) | i == mkCId "Krylov_ProperName" -> GKrylov_ProperName 
      Just (i,[]) | i == mkCId "Kummer_ProperName" -> GKummer_ProperName 
      Just (i,[]) | i == mkCId "Kynea_ProperName" -> GKynea_ProperName 
      Just (i,[]) | i == mkCId "Künneth_ProperName" -> GKünneth_ProperName 
      Just (i,[]) | i == mkCId "Lagrange_ProperName" -> GLagrange_ProperName 
      Just (i,[]) | i == mkCId "Laue_ProperName" -> GLaue_ProperName 
      Just (i,[]) | i == mkCId "Lawvere_ProperName" -> GLawvere_ProperName 
      Just (i,[]) | i == mkCId "Lebesgue_ProperName" -> GLebesgue_ProperName 
      Just (i,[]) | i == mkCId "Legendre_ProperName" -> GLegendre_ProperName 
      Just (i,[]) | i == mkCId "Leibniz_ProperName" -> GLeibniz_ProperName 
      Just (i,[]) | i == mkCId "Levenshtein_ProperName" -> GLevenshtein_ProperName 
      Just (i,[]) | i == mkCId "Levi_ProperName" -> GLevi_ProperName 
      Just (i,[]) | i == mkCId "Leyland_ProperName" -> GLeyland_ProperName 
      Just (i,[]) | i == mkCId "Lie_ProperName" -> GLie_ProperName 
      Just (i,[]) | i == mkCId "Lindelöf_ProperName" -> GLindelöf_ProperName 
      Just (i,[]) | i == mkCId "Lipschitz_ProperName" -> GLipschitz_ProperName 
      Just (i,[]) | i == mkCId "Lissajous_ProperName" -> GLissajous_ProperName 
      Just (i,[]) | i == mkCId "LiverTox_ProperName" -> GLiverTox_ProperName 
      Just (i,[]) | i == mkCId "Ljubljana_ProperName" -> GLjubljana_ProperName 
      Just (i,[]) | i == mkCId "Loewner_ProperName" -> GLoewner_ProperName 
      Just (i,[]) | i == mkCId "London_ProperName" -> GLondon_ProperName 
      Just (i,[]) | i == mkCId "Lorentz_ProperName" -> GLorentz_ProperName 
      Just (i,[]) | i == mkCId "Loupekine_ProperName" -> GLoupekine_ProperName 
      Just (i,[]) | i == mkCId "Lovász_ProperName" -> GLovász_ProperName 
      Just (i,[]) | i == mkCId "Lucas_ProperName" -> GLucas_ProperName 
      Just (i,[]) | i == mkCId "Lyapunov_ProperName" -> GLyapunov_ProperName 
      Just (i,[]) | i == mkCId "Macdonald_ProperName" -> GMacdonald_ProperName 
      Just (i,[]) | i == mkCId "Mahalanobis_ProperName" -> GMahalanobis_ProperName 
      Just (i,[]) | i == mkCId "Mahler_ProperName" -> GMahler_ProperName 
      Just (i,[]) | i == mkCId "Mandelbrot_ProperName" -> GMandelbrot_ProperName 
      Just (i,[]) | i == mkCId "Manin_ProperName" -> GManin_ProperName 
      Just (i,[]) | i == mkCId "Markov_ProperName" -> GMarkov_ProperName 
      Just (i,[]) | i == mkCId "Mazur_ProperName" -> GMazur_ProperName 
      Just (i,[]) | i == mkCId "McGee_ProperName" -> GMcGee_ProperName 
      Just (i,[]) | i == mkCId "McKay_ProperName" -> GMcKay_ProperName 
      Just (i,[]) | i == mkCId "McLaughlin_ProperName" -> GMcLaughlin_ProperName 
      Just (i,[]) | i == mkCId "Meredith_ProperName" -> GMeredith_ProperName 
      Just (i,[]) | i == mkCId "Meringer_ProperName" -> GMeringer_ProperName 
      Just (i,[]) | i == mkCId "Mersenne_ProperName" -> GMersenne_ProperName 
      Just (i,[]) | i == mkCId "Meyniel_ProperName" -> GMeyniel_ProperName 
      Just (i,[]) | i == mkCId "Miller_ProperName" -> GMiller_ProperName 
      Just (i,[]) | i == mkCId "Milnor_ProperName" -> GMilnor_ProperName 
      Just (i,[]) | i == mkCId "Minkowski_ProperName" -> GMinkowski_ProperName 
      Just (i,[]) | i == mkCId "Montel_ProperName" -> GMontel_ProperName 
      Just (i,[]) | i == mkCId "Moore_ProperName" -> GMoore_ProperName 
      Just (i,[]) | i == mkCId "Morgan_ProperName" -> GMorgan_ProperName 
      Just (i,[]) | i == mkCId "Morse_ProperName" -> GMorse_ProperName 
      Just (i,[]) | i == mkCId "Moser_ProperName" -> GMoser_ProperName 
      Just (i,[]) | i == mkCId "Motzkin_ProperName" -> GMotzkin_ProperName 
      Just (i,[]) | i == mkCId "Mumford_ProperName" -> GMumford_ProperName 
      Just (i,[]) | i == mkCId "Munn_ProperName" -> GMunn_ProperName 
      Just (i,[]) | i == mkCId "Möbius_ProperName" -> GMöbius_ProperName 
      Just (i,[]) | i == mkCId "Münchhausen_ProperName" -> GMünchhausen_ProperName 
      Just (i,[]) | i == mkCId "Nagata_ProperName" -> GNagata_ProperName 
      Just (i,[]) | i == mkCId "Nakajima_ProperName" -> GNakajima_ProperName 
      Just (i,[]) | i == mkCId "Narayana_ProperName" -> GNarayana_ProperName 
      Just (i,[]) | i == mkCId "Neumann_ProperName" -> GNeumann_ProperName 
      Just (i,[]) | i == mkCId "Newton_ProperName" -> GNewton_ProperName 
      Just (i,[]) | i == mkCId "Ockham_ProperName" -> GOckham_ProperName 
      Just (i,[]) | i == mkCId "Paley_ProperName" -> GPaley_ProperName 
      Just (i,[]) | i == mkCId "Pareto_ProperName" -> GPareto_ProperName 
      Just (i,[]) | i == mkCId "Parrondo_ProperName" -> GParrondo_ProperName 
      Just (i,[]) | i == mkCId "Patricia_ProperName" -> GPatricia_ProperName 
      Just (i,[]) | i == mkCId "Peano_ProperName" -> GPeano_ProperName 
      Just (i,[]) | i == mkCId "Pearcey_ProperName" -> GPearcey_ProperName 
      Just (i,[]) | i == mkCId "Penrose_ProperName" -> GPenrose_ProperName 
      Just (i,[]) | i == mkCId "Perkel_ProperName" -> GPerkel_ProperName 
      Just (i,[]) | i == mkCId "Perrin_ProperName" -> GPerrin_ProperName 
      Just (i,[]) | i == mkCId "Petersen_ProperName" -> GPetersen_ProperName 
      Just (i,[]) | i == mkCId "Petri_ProperName" -> GPetri_ProperName 
      Just (i,[]) | i == mkCId "Pettis_ProperName" -> GPettis_ProperName 
      Just (i,[]) | i == mkCId "Picard_ProperName" -> GPicard_ProperName 
      Just (i,[]) | i == mkCId "Pippard_ProperName" -> GPippard_ProperName 
      Just (i,[]) | i == mkCId "Poincaré_ProperName" -> GPoincaré_ProperName 
      Just (i,[]) | i == mkCId "Poisson_ProperName" -> GPoisson_ProperName 
      Just (i,[]) | i == mkCId "Poussin_ProperName" -> GPoussin_ProperName 
      Just (i,[]) | i == mkCId "Ptolemy_ProperName" -> GPtolemy_ProperName 
      Just (i,[]) | i == mkCId "Puig_ProperName" -> GPuig_ProperName 
      Just (i,[]) | i == mkCId "Rado_ProperName" -> GRado_ProperName 
      Just (i,[]) | i == mkCId "Radon_ProperName" -> GRadon_ProperName 
      Just (i,[]) | i == mkCId "Rajchman_ProperName" -> GRajchman_ProperName 
      Just (i,[]) | i == mkCId "Ramanujan_ProperName" -> GRamanujan_ProperName 
      Just (i,[]) | i == mkCId "Rees_ProperName" -> GRees_ProperName 
      Just (i,[]) | i == mkCId "Reeve_ProperName" -> GReeve_ProperName 
      Just (i,[]) | i == mkCId "Reinhardt_ProperName" -> GReinhardt_ProperName 
      Just (i,[]) | i == mkCId "Rham_ProperName" -> GRham_ProperName 
      Just (i,[]) | i == mkCId "Riemann_ProperName" -> GRiemann_ProperName 
      Just (i,[]) | i == mkCId "Riesel_ProperName" -> GRiesel_ProperName 
      Just (i,[]) | i == mkCId "Riesz_ProperName" -> GRiesz_ProperName 
      Just (i,[]) | i == mkCId "Robbins_ProperName" -> GRobbins_ProperName 
      Just (i,[]) | i == mkCId "Robertson_ProperName" -> GRobertson_ProperName 
      Just (i,[]) | i == mkCId "Rosenbrock_ProperName" -> GRosenbrock_ProperName 
      Just (i,[]) | i == mkCId "Royle_ProperName" -> GRoyle_ProperName 
      Just (i,[]) | i == mkCId "Schauder_ProperName" -> GSchauder_ProperName 
      Just (i,[]) | i == mkCId "Schläfli_ProperName" -> GSchläfli_ProperName 
      Just (i,[]) | i == mkCId "Schreier_ProperName" -> GSchreier_ProperName 
      Just (i,[]) | i == mkCId "Schrödinger_ProperName" -> GSchrödinger_ProperName 
      Just (i,[]) | i == mkCId "Schubert_ProperName" -> GSchubert_ProperName 
      Just (i,[]) | i == mkCId "Schur_ProperName" -> GSchur_ProperName 
      Just (i,[]) | i == mkCId "Schwartz_ProperName" -> GSchwartz_ProperName 
      Just (i,[]) | i == mkCId "Schwarz_ProperName" -> GSchwarz_ProperName 
      Just (i,[]) | i == mkCId "Scott_ProperName" -> GScott_ProperName 
      Just (i,[]) | i == mkCId "Selberg_ProperName" -> GSelberg_ProperName 
      Just (i,[]) | i == mkCId "Serre_ProperName" -> GSerre_ProperName 
      Just (i,[]) | i == mkCId "Shannon_ProperName" -> GShannon_ProperName 
      Just (i,[]) | i == mkCId "Shelah_ProperName" -> GShelah_ProperName 
      Just (i,[]) | i == mkCId "Shimura_ProperName" -> GShimura_ProperName 
      Just (i,[]) | i == mkCId "Shrikhande_ProperName" -> GShrikhande_ProperName 
      Just (i,[]) | i == mkCId "Siegel_ProperName" -> GSiegel_ProperName 
      Just (i,[]) | i == mkCId "Sierpinski_ProperName" -> GSierpinski_ProperName 
      Just (i,[]) | i == mkCId "Sitter_ProperName" -> GSitter_ProperName 
      Just (i,[]) | i == mkCId "Slater_ProperName" -> GSlater_ProperName 
      Just (i,[]) | i == mkCId "Smith_ProperName" -> GSmith_ProperName 
      Just (i,[]) | i == mkCId "Sousselier_ProperName" -> GSousselier_ProperName 
      Just (i,[]) | i == mkCId "Spencer_ProperName" -> GSpencer_ProperName 
      Just (i,[]) | i == mkCId "Stark_ProperName" -> GStark_ProperName 
      Just (i,[]) | i == mkCId "Steenrod_ProperName" -> GSteenrod_ProperName 
      Just (i,[]) | i == mkCId "Steinitz_ProperName" -> GSteinitz_ProperName 
      Just (i,[]) | i == mkCId "Stepanoff_ProperName" -> GStepanoff_ProperName 
      Just (i,[]) | i == mkCId "Stiefel_ProperName" -> GStiefel_ProperName 
      Just (i,[]) | i == mkCId "Stirling_ProperName" -> GStirling_ProperName 
      Just (i,[]) | i == mkCId "Stone_ProperName" -> GStone_ProperName 
      Just (i,[]) | i == mkCId "Suslin_ProperName" -> GSuslin_ProperName 
      Just (i,[]) | i == mkCId "Suzuki_ProperName" -> GSuzuki_ProperName 
      Just (i,[]) | i == mkCId "Sylow_ProperName" -> GSylow_ProperName 
      Just (i,[]) | i == mkCId "Sylvester_ProperName" -> GSylvester_ProperName 
      Just (i,[]) | i == mkCId "Szekeres_ProperName" -> GSzekeres_ProperName 
      Just (i,[]) | i == mkCId "Sárközy_ProperName" -> GSárközy_ProperName 
      Just (i,[]) | i == mkCId "Sørensen_ProperName" -> GSørensen_ProperName 
      Just (i,[]) | i == mkCId "Taleb_ProperName" -> GTaleb_ProperName 
      Just (i,[]) | i == mkCId "Tarski_ProperName" -> GTarski_ProperName 
      Just (i,[]) | i == mkCId "Tate_ProperName" -> GTate_ProperName 
      Just (i,[]) | i == mkCId "Taylor_ProperName" -> GTaylor_ProperName 
      Just (i,[]) | i == mkCId "Thabit_ProperName" -> GThabit_ProperName 
      Just (i,[]) | i == mkCId "Thom_ProperName" -> GThom_ProperName 
      Just (i,[]) | i == mkCId "Thomassen_ProperName" -> GThomassen_ProperName 
      Just (i,[]) | i == mkCId "Thompson_ProperName" -> GThompson_ProperName 
      Just (i,[]) | i == mkCId "Thue_ProperName" -> GThue_ProperName 
      Just (i,[]) | i == mkCId "Time_ProperName" -> GTime_ProperName 
      Just (i,[]) | i == mkCId "Trofimov_ProperName" -> GTrofimov_ProperName 
      Just (i,[]) | i == mkCId "Trémaux_ProperName" -> GTrémaux_ProperName 
      Just (i,[]) | i == mkCId "Turán_ProperName" -> GTurán_ProperName 
      Just (i,[]) | i == mkCId "Tutte_ProperName" -> GTutte_ProperName 
      Just (i,[]) | i == mkCId "Tweedie_ProperName" -> GTweedie_ProperName 
      Just (i,[]) | i == mkCId "Tychonoff_ProperName" -> GTychonoff_ProperName 
      Just (i,[]) | i == mkCId "Urysohn_ProperName" -> GUrysohn_ProperName 
      Just (i,[]) | i == mkCId "Vandermonde_ProperName" -> GVandermonde_ProperName 
      Just (i,[]) | i == mkCId "Verdière_ProperName" -> GVerdière_ProperName 
      Just (i,[]) | i == mkCId "Vickrey_ProperName" -> GVickrey_ProperName 
      Just (i,[]) | i == mkCId "Wallman_ProperName" -> GWallman_ProperName 
      Just (i,[]) | i == mkCId "Walther_ProperName" -> GWalther_ProperName 
      Just (i,[]) | i == mkCId "Weibull_ProperName" -> GWeibull_ProperName 
      Just (i,[]) | i == mkCId "Weierstrass_ProperName" -> GWeierstrass_ProperName 
      Just (i,[]) | i == mkCId "Weil_ProperName" -> GWeil_ProperName 
      Just (i,[]) | i == mkCId "Weingarten_ProperName" -> GWeingarten_ProperName 
      Just (i,[]) | i == mkCId "Wells_ProperName" -> GWells_ProperName 
      Just (i,[]) | i == mkCId "Weyl_ProperName" -> GWeyl_ProperName 
      Just (i,[]) | i == mkCId "Whewell_ProperName" -> GWhewell_ProperName 
      Just (i,[]) | i == mkCId "Wieferich_ProperName" -> GWieferich_ProperName 
      Just (i,[]) | i == mkCId "Witt_ProperName" -> GWitt_ProperName 
      Just (i,[]) | i == mkCId "Woodall_ProperName" -> GWoodall_ProperName 
      Just (i,[]) | i == mkCId "Woodin_ProperName" -> GWoodin_ProperName 
      Just (i,[]) | i == mkCId "Yoneda_ProperName" -> GYoneda_ProperName 
      Just (i,[]) | i == mkCId "Zamfirescu_ProperName" -> GZamfirescu_ProperName 
      Just (i,[]) | i == mkCId "Zaslavskii_ProperName" -> GZaslavskii_ProperName 
      Just (i,[]) | i == mkCId "Zassenhaus_ProperName" -> GZassenhaus_ProperName 
      Just (i,[]) | i == mkCId "Zeisel_ProperName" -> GZeisel_ProperName 
      Just (i,[]) | i == mkCId "Zhegalkin_ProperName" -> GZhegalkin_ProperName 
      Just (i,[]) | i == mkCId "Zinbiel_ProperName" -> GZinbiel_ProperName 
      Just (i,[]) | i == mkCId "Zorn_ProperName" -> GZorn_ProperName 
      Just (i,[]) | i == mkCId "Zuckerman_ProperName" -> GZuckerman_ProperName 
      Just (i,[]) | i == mkCId "'Łojasiewicz_ProperName'" -> GŁojasiewicz_ProperName 
      Just (i,[]) | i == mkCId "'Łukasiewicz_ProperName'" -> GŁukasiewicz_ProperName 


      _ -> error ("no ProperName " ++ show t)

instance Gf GRule where
  gf (GNoVarRewriteRule x1 x2) = mkApp (mkCId "NoVarRewriteRule") [gf x1, gf x2]
  gf (GRewriteRule x1 x2 x3) = mkApp (mkCId "RewriteRule") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "NoVarRewriteRule" -> GNoVarRewriteRule (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "RewriteRule" -> GRewriteRule (fg x1) (fg x2) (fg x3)


      _ -> error ("no Rule " ++ show t)

instance Gf GTerm where
  gf (GApp1MacroTerm x1 x2) = mkApp (mkCId "App1MacroTerm") [gf x1, gf x2]
  gf (GApp2MacroTerm x1 x2 x3) = mkApp (mkCId "App2MacroTerm") [gf x1, gf x2, gf x3]
  gf (GApp3MacroTerm x1 x2 x3 x4) = mkApp (mkCId "App3MacroTerm") [gf x1, gf x2, gf x3, gf x4]
  gf (GAppFunctionTerm x1 x2) = mkApp (mkCId "AppFunctionTerm") [gf x1, gf x2]
  gf (GComprehensionTerm x1 x2 x3) = mkApp (mkCId "ComprehensionTerm") [gf x1, gf x2, gf x3]
  gf (GConstTerm x1) = mkApp (mkCId "ConstTerm") [gf x1]
  gf (GEnumSetTerm x1) = mkApp (mkCId "EnumSetTerm") [gf x1]
  gf (GIdentTerm x1) = mkApp (mkCId "IdentTerm") [gf x1]
  gf (GMacroTerm x1) = mkApp (mkCId "MacroTerm") [gf x1]
  gf (GNumberTerm x1) = mkApp (mkCId "NumberTerm") [gf x1]
  gf (GOper2Term x1 x2 x3) = mkApp (mkCId "Oper2Term") [gf x1, gf x2, gf x3]
  gf (GOperTerm x1 x2) = mkApp (mkCId "OperTerm") [gf x1, gf x2]
  gf (GParenthTerm x1) = mkApp (mkCId "ParenthTerm") [gf x1]
  gf (GTextbfTerm x1) = mkApp (mkCId "TextbfTerm") [gf x1]
  gf (Gintegral_Term x1 x2 x3 x4) = mkApp (mkCId "integral_Term") [gf x1, gf x2, gf x3, gf x4]
  gf (Gseries_Term x1 x2 x3) = mkApp (mkCId "series_Term") [gf x1, gf x2, gf x3]
  gf (Gsigma_Term x1 x2 x3 x4) = mkApp (mkCId "sigma_Term") [gf x1, gf x2, gf x3, gf x4]
  gf (Gsum3dots_Term x1 x2 x3) = mkApp (mkCId "sum3dots_Term") [gf x1, gf x2, gf x3]
  gf (Gtimes_Term x1 x2) = mkApp (mkCId "times_Term") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "App1MacroTerm" -> GApp1MacroTerm (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "App2MacroTerm" -> GApp2MacroTerm (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "App3MacroTerm" -> GApp3MacroTerm (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "AppFunctionTerm" -> GAppFunctionTerm (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "ComprehensionTerm" -> GComprehensionTerm (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "ConstTerm" -> GConstTerm (fg x1)
      Just (i,[x1]) | i == mkCId "EnumSetTerm" -> GEnumSetTerm (fg x1)
      Just (i,[x1]) | i == mkCId "IdentTerm" -> GIdentTerm (fg x1)
      Just (i,[x1]) | i == mkCId "MacroTerm" -> GMacroTerm (fg x1)
      Just (i,[x1]) | i == mkCId "NumberTerm" -> GNumberTerm (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "Oper2Term" -> GOper2Term (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "OperTerm" -> GOperTerm (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ParenthTerm" -> GParenthTerm (fg x1)
      Just (i,[x1]) | i == mkCId "TextbfTerm" -> GTextbfTerm (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "integral_Term" -> Gintegral_Term (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "series_Term" -> Gseries_Term (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "sigma_Term" -> Gsigma_Term (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "sum3dots_Term" -> Gsum3dots_Term (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "times_Term" -> Gtimes_Term (fg x1) (fg x2)


      _ -> error ("no Term " ++ show t)

instance Gf GTitle where
  gf (GStringTitle x1) = mkApp (mkCId "StringTitle") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "StringTitle" -> GStringTitle (fg x1)


      _ -> error ("no Title " ++ show t)

instance Gf GUnit where
  gf (GBeginEnvironmentUnit x1 x2) = mkApp (mkCId "BeginEnvironmentUnit") [gf x1, gf x2]
  gf (GBeginProofMethodUnit x1 x2) = mkApp (mkCId "BeginProofMethodUnit") [gf x1, gf x2]
  gf (GCaseGoal x1 x2) = mkApp (mkCId "CaseGoal") [gf x1, gf x2]
  gf GCasesGoal = mkApp (mkCId "CasesGoal") []
  gf (GEndEnvironmentUnit x1) = mkApp (mkCId "EndEnvironmentUnit") [gf x1]
  gf (GEnoughGoal x1) = mkApp (mkCId "EnoughGoal") [gf x1]
  gf (GFirstVerifyGoal x1) = mkApp (mkCId "FirstVerifyGoal") [gf x1]
  gf (GFollowsPropConclusion x1) = mkApp (mkCId "FollowsPropConclusion") [gf x1]
  gf (GHyposAssumption x1) = mkApp (mkCId "HyposAssumption") [gf x1]
  gf (GIdentExpAssumption x1 x2) = mkApp (mkCId "IdentExpAssumption") [gf x1, gf x2]
  gf (GIdentKindAssumption x1 x2) = mkApp (mkCId "IdentKindAssumption") [gf x1, gf x2]
  gf (GImportUnit x1) = mkApp (mkCId "ImportUnit") [gf x1]
  gf GInductionGoal = mkApp (mkCId "InductionGoal") []
  gf (GLabelConclusion x1) = mkApp (mkCId "LabelConclusion") [gf x1]
  gf (GLabelUnit x1) = mkApp (mkCId "LabelUnit") [gf x1]
  gf GObviousConclusion = mkApp (mkCId "ObviousConclusion") []
  gf (GPropAssumption x1 x2) = mkApp (mkCId "PropAssumption") [gf x1, gf x2]
  gf (GPropConclusion x1 x2) = mkApp (mkCId "PropConclusion") [gf x1, gf x2]
  gf (GPropLabelConclusion x1 x2 x3) = mkApp (mkCId "PropLabelConclusion") [gf x1, gf x2, gf x3]
  gf (GSectionUnit x1 x2) = mkApp (mkCId "SectionUnit") [gf x1, gf x2]
  gf (GSinceConclusion x1 x2) = mkApp (mkCId "SinceConclusion") [gf x1, gf x2]
  gf (GSinceGoal x1 x2) = mkApp (mkCId "SinceGoal") [gf x1, gf x2]
  gf (GSubsectionUnit x1 x2) = mkApp (mkCId "SubsectionUnit") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BeginEnvironmentUnit" -> GBeginEnvironmentUnit (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "BeginProofMethodUnit" -> GBeginProofMethodUnit (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "CaseGoal" -> GCaseGoal (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "CasesGoal" -> GCasesGoal 
      Just (i,[x1]) | i == mkCId "EndEnvironmentUnit" -> GEndEnvironmentUnit (fg x1)
      Just (i,[x1]) | i == mkCId "EnoughGoal" -> GEnoughGoal (fg x1)
      Just (i,[x1]) | i == mkCId "FirstVerifyGoal" -> GFirstVerifyGoal (fg x1)
      Just (i,[x1]) | i == mkCId "FollowsPropConclusion" -> GFollowsPropConclusion (fg x1)
      Just (i,[x1]) | i == mkCId "HyposAssumption" -> GHyposAssumption (fg x1)
      Just (i,[x1,x2]) | i == mkCId "IdentExpAssumption" -> GIdentExpAssumption (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "IdentKindAssumption" -> GIdentKindAssumption (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ImportUnit" -> GImportUnit (fg x1)
      Just (i,[]) | i == mkCId "InductionGoal" -> GInductionGoal 
      Just (i,[x1]) | i == mkCId "LabelConclusion" -> GLabelConclusion (fg x1)
      Just (i,[x1]) | i == mkCId "LabelUnit" -> GLabelUnit (fg x1)
      Just (i,[]) | i == mkCId "ObviousConclusion" -> GObviousConclusion 
      Just (i,[x1,x2]) | i == mkCId "PropAssumption" -> GPropAssumption (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PropConclusion" -> GPropConclusion (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "PropLabelConclusion" -> GPropLabelConclusion (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "SectionUnit" -> GSectionUnit (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SinceConclusion" -> GSinceConclusion (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SinceGoal" -> GSinceGoal (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SubsectionUnit" -> GSubsectionUnit (fg x1) (fg x2)


      _ -> error ("no Unit " ++ show t)

instance Gf GVerb where
  gf (GVerbPrepNounVerb x1 x2 x3) = mkApp (mkCId "VerbPrepNounVerb") [gf x1, gf x2, gf x3]
  gf (LexVerb x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "VerbPrepNounVerb" -> GVerbPrepNounVerb (fg x1) (fg x2) (fg x3)

      Just (i,[]) -> LexVerb (showCId i)
      _ -> error ("no Verb " ++ show t)

instance Gf GVerb2 where
  gf (GVerbPrepVerb2 x1 x2) = mkApp (mkCId "VerbPrepVerb2") [gf x1, gf x2]
  gf (LexVerb2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "VerbPrepVerb2" -> GVerbPrepVerb2 (fg x1) (fg x2)

      Just (i,[]) -> LexVerb2 (showCId i)
      _ -> error ("no Verb2 " ++ show t)

instance Gf GVerbC where
  gf (GVerbVerbC x1) = mkApp (mkCId "VerbVerbC") [gf x1]
  gf (LexVerbC x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "VerbVerbC" -> GVerbVerbC (fg x1)

      Just (i,[]) -> LexVerbC (showCId i)
      _ -> error ("no VerbC " ++ show t)

instance Show GCoercion

instance Gf GCoercion where
  gf _ = undefined
  fg _ = undefined



instance Show GTerms

instance Gf GTerms where
  gf _ = undefined
  fg _ = undefined




instance Compos Tree where
  compos r a f t = case t of
    GAdj2Adj x1 x2 -> r GAdj2Adj `a` f x1 `a` f x2
    GAdj3Adj x1 x2 x3 -> r GAdj3Adj `a` f x1 `a` f x2 `a` f x3
    GAdverbAdjAdj x1 x2 -> r GAdverbAdjAdj `a` f x1 `a` f x2
    GAndAdj x1 -> r GAndAdj `a` f x1
    GBothAndAdj x1 x2 -> r GBothAndAdj `a` f x1 `a` f x2
    GEitherOrAdj x1 x2 -> r GEitherOrAdj `a` f x1 `a` f x2
    GOrAdj x1 -> r GOrAdj `a` f x1
    GAdjPrepAdj2 x1 x2 -> r GAdjPrepAdj2 `a` f x1 `a` f x2
    GAdjPrepAdj3 x1 x2 x3 -> r GAdjPrepAdj3 `a` f x1 `a` f x2 `a` f x3
    GAdjAdjC x1 -> r GAdjAdjC `a` f x1
    GAdjAdjE x1 -> r GAdjAdjE `a` f x1
    GBareIdentsArgKind x1 -> r GBareIdentsArgKind `a` f x1
    GDeclarationArgKind x1 -> r GDeclarationArgKind `a` f x1
    GIdentArgKind x1 x2 -> r GIdentArgKind `a` f x1 `a` f x2
    GIdentsArgKind x1 x2 -> r GIdentsArgKind `a` f x1 `a` f x2
    GIndexedDeclarationArgKind x1 -> r GIndexedDeclarationArgKind `a` f x1
    GKindArgKind x1 -> r GKindArgKind `a` f x1
    GElemDeclaration x1 x2 -> r GElemDeclaration `a` f x1 `a` f x2
    GFunctionDeclaration x1 x2 x3 -> r GFunctionDeclaration `a` f x1 `a` f x2 `a` f x3
    GBinaryEquation x1 x2 x3 -> r GBinaryEquation `a` f x1 `a` f x2 `a` f x3
    GChainEquation x1 x2 x3 -> r GChainEquation `a` f x1 `a` f x2 `a` f x3
    GAdj2Example x1 x2 x3 -> r GAdj2Example `a` f x1 `a` f x2 `a` f x3
    GAdj3Example x1 x2 x3 x4 -> r GAdj3Example `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GAdjCExample x1 x2 x3 -> r GAdjCExample `a` f x1 `a` f x2 `a` f x3
    GAdjEExample x1 x2 x3 -> r GAdjEExample `a` f x1 `a` f x2 `a` f x3
    GAdjExample x1 x2 -> r GAdjExample `a` f x1 `a` f x2
    GFam2Example x1 x2 x3 -> r GFam2Example `a` f x1 `a` f x2 `a` f x3
    GFamExample x1 x2 -> r GFamExample `a` f x1 `a` f x2
    GFun2Example x1 x2 x3 -> r GFun2Example `a` f x1 `a` f x2 `a` f x3
    GFunCExample x1 x2 x3 -> r GFunCExample `a` f x1 `a` f x2 `a` f x3
    GFunExample x1 x2 -> r GFunExample `a` f x1 `a` f x2
    GLabelExample x1 -> r GLabelExample `a` f x1
    GNameExample x1 -> r GNameExample `a` f x1
    GNoun1Example x1 x2 -> r GNoun1Example `a` f x1 `a` f x2
    GNoun2Example x1 x2 x3 -> r GNoun2Example `a` f x1 `a` f x2 `a` f x3
    GNounCExample x1 x2 x3 -> r GNounCExample `a` f x1 `a` f x2 `a` f x3
    GNounExample x1 -> r GNounExample `a` f x1
    GVerb2Example x1 x2 x3 -> r GVerb2Example `a` f x1 `a` f x2 `a` f x3
    GVerbCExample x1 x2 x3 -> r GVerbCExample `a` f x1 `a` f x2 `a` f x3
    GVerbExample x1 x2 -> r GVerbExample `a` f x1 `a` f x2
    GAbsExp x1 x2 -> r GAbsExp `a` f x1 `a` f x2
    GAllIdentsKindExp x1 x2 -> r GAllIdentsKindExp `a` f x1 `a` f x2
    GAllKindExp x1 -> r GAllKindExp `a` f x1
    GAndExp x1 -> r GAndExp `a` f x1
    GAnnotateExp x1 x2 -> r GAnnotateExp `a` f x1 `a` f x2
    GAppExp x1 x2 -> r GAppExp `a` f x1 `a` f x2
    GBothAndExp x1 x2 -> r GBothAndExp `a` f x1 `a` f x2
    GCoercionExp x1 x2 -> r GCoercionExp `a` f x1 `a` f x2
    GCoreAbsExp x1 x2 -> r GCoreAbsExp `a` f x1 `a` f x2
    GEitherOrExp x1 x2 -> r GEitherOrExp `a` f x1 `a` f x2
    GEnumSetExp x1 -> r GEnumSetExp `a` f x1
    GEveryIdentKindExp x1 x2 -> r GEveryIdentKindExp `a` f x1 `a` f x2
    GEveryKindExp x1 -> r GEveryKindExp `a` f x1
    GFun2Exp x1 x2 x3 -> r GFun2Exp `a` f x1 `a` f x2 `a` f x3
    GFunCCollExp x1 x2 -> r GFunCCollExp `a` f x1 `a` f x2
    GFunCExp x1 x2 x3 -> r GFunCExp `a` f x1 `a` f x2 `a` f x3
    GFunExp x1 x2 -> r GFunExp `a` f x1 `a` f x2
    GIndefIdentKindExp x1 x2 -> r GIndefIdentKindExp `a` f x1 `a` f x2
    GIndefKindExp x1 -> r GIndefKindExp `a` f x1
    GIndexedTermExp x1 -> r GIndexedTermExp `a` f x1
    GIntegralExp x1 x2 x3 x4 -> r GIntegralExp `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GKindExp x1 -> r GKindExp `a` f x1
    GNameExp x1 -> r GNameExp `a` f x1
    GNoIdentsKindExp x1 x2 -> r GNoIdentsKindExp `a` f x1 `a` f x2
    GNoKindExp x1 -> r GNoKindExp `a` f x1
    GOrExp x1 -> r GOrExp `a` f x1
    GSeriesExp x1 x2 x3 -> r GSeriesExp `a` f x1 `a` f x2 `a` f x3
    GSigmaExp x1 x2 x3 x4 -> r GSigmaExp `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GSomeIdentsKindExp x1 x2 -> r GSomeIdentsKindExp `a` f x1 `a` f x2
    GSomeKindExp x1 -> r GSomeKindExp `a` f x1
    GTermExp x1 -> r GTermExp `a` f x1
    GTypedExp x1 x2 -> r GTypedExp `a` f x1 `a` f x2
    GManyExps x1 -> r GManyExps `a` f x1
    GOneExps x1 -> r GOneExps `a` f x1
    GNounPrepFam x1 x2 -> r GNounPrepFam `a` f x1 `a` f x2
    GNounPrepFam2 x1 x2 x3 -> r GNounPrepFam2 `a` f x1 `a` f x2 `a` f x3
    GStringFilename x1 -> r GStringFilename `a` f x1
    GApp1MacroFormula x1 x2 -> r GApp1MacroFormula `a` f x1 `a` f x2
    GApp2MacroFormula x1 x2 x3 -> r GApp2MacroFormula `a` f x1 `a` f x2 `a` f x3
    GApp3MacroFormula x1 x2 x3 x4 -> r GApp3MacroFormula `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GElemFormula x1 x2 -> r GElemFormula `a` f x1 `a` f x2
    GEquationFormula x1 -> r GEquationFormula `a` f x1
    GMacroFormula x1 -> r GMacroFormula `a` f x1
    Gmodulo_Formula x1 x2 x3 -> r Gmodulo_Formula `a` f x1 `a` f x2 `a` f x3
    GNounPrepFun x1 x2 -> r GNounPrepFun `a` f x1 `a` f x2
    GNounPrepFun2 x1 x2 x3 -> r GNounPrepFun2 `a` f x1 `a` f x2 `a` f x3
    GNounPrepFunC x1 x2 -> r GNounPrepFunC `a` f x1 `a` f x2
    GDerivativeFunction x1 -> r GDerivativeFunction `a` f x1
    GIdentFunction x1 -> r GIdentFunction `a` f x1
    GBareVarHypo x1 -> r GBareVarHypo `a` f x1
    GBareVarsHypo x1 -> r GBareVarsHypo `a` f x1
    GIndexedLetFormulaHypo x1 -> r GIndexedLetFormulaHypo `a` f x1
    GLetDeclarationHypo x1 -> r GLetDeclarationHypo `a` f x1
    GLetFormulaHypo x1 -> r GLetFormulaHypo `a` f x1
    GLocalHypo x1 -> r GLocalHypo `a` f x1
    GPropHypo x1 -> r GPropHypo `a` f x1
    GSupposePropHypo x1 -> r GSupposePropHypo `a` f x1
    GVarHypo x1 x2 -> r GVarHypo `a` f x1 `a` f x2
    GVarsHypo x1 x2 -> r GVarsHypo `a` f x1 `a` f x2
    GStrIdent x1 -> r GStrIdent `a` f x1
    GAxiomExpJmt x1 x2 x3 x4 -> r GAxiomExpJmt `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GAxiomJmt x1 x2 x3 -> r GAxiomJmt `a` f x1 `a` f x2 `a` f x3
    GAxiomKindJmt x1 x2 x3 -> r GAxiomKindJmt `a` f x1 `a` f x2 `a` f x3
    GAxiomPropJmt x1 x2 x3 -> r GAxiomPropJmt `a` f x1 `a` f x2 `a` f x3
    GDefExpJmt x1 x2 x3 x4 x5 -> r GDefExpJmt `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    GDefKindJmt x1 x2 x3 x4 -> r GDefKindJmt `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GDefPropJmt x1 x2 x3 x4 -> r GDefPropJmt `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GDefUntypedExpJmt x1 x2 x3 -> r GDefUntypedExpJmt `a` f x1 `a` f x2 `a` f x3
    GDefinedAdjJmt x1 x2 x3 x4 x5 -> r GDefinedAdjJmt `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    GRewriteJmt x1 -> r GRewriteJmt `a` f x1
    GThmJmt x1 x2 x3 x4 -> r GThmJmt `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GUnitJmt x1 -> r GUnitJmt `a` f x1
    GWeDefineAdjJmt x1 x2 x3 x4 x5 -> r GWeDefineAdjJmt `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    GAdjKind x1 x2 -> r GAdjKind `a` f x1 `a` f x2
    GAnnotateKind x1 x2 -> r GAnnotateKind `a` f x1 `a` f x2
    GAppKind x1 x2 -> r GAppKind `a` f x1 `a` f x2
    GElemKind x1 -> r GElemKind `a` f x1
    GFam2Kind x1 x2 x3 -> r GFam2Kind `a` f x1 `a` f x2 `a` f x3
    GFamKind x1 x2 -> r GFamKind `a` f x1 `a` f x2
    GFunKind x1 x2 -> r GFunKind `a` f x1 `a` f x2
    GIdentKind x1 -> r GIdentKind `a` f x1
    GNounKind x1 -> r GNounKind `a` f x1
    GSuchThatKind x1 x2 x3 -> r GSuchThatKind `a` f x1 `a` f x2 `a` f x3
    GTermKind x1 -> r GTermKind `a` f x1
    GDefNounLabel x1 -> r GDefNounLabel `a` f x1
    GIdentLabel x1 -> r GIdentLabel `a` f x1
    GNounIdentLabel x1 x2 -> r GNounIdentLabel `a` f x1 `a` f x2
    GNounIntLabel x1 x2 -> r GNounIntLabel `a` f x1 `a` f x2
    GNounLabel x1 -> r GNounLabel `a` f x1
    GNounOfNounLabel x1 x2 -> r GNounOfNounLabel `a` f x1 `a` f x2
    GProperNameNounLabel x1 x2 -> r GProperNameNounLabel `a` f x1 `a` f x2
    GcrefLabel x1 -> r GcrefLabel `a` f x1
    GBareLetLocal x1 x2 -> r GBareLetLocal `a` f x1 `a` f x2
    GLetLocal x1 x2 x3 -> r GLetLocal `a` f x1 `a` f x2 `a` f x3
    GStringMacro x1 -> r GStringMacro `a` f x1
    GStringMethod x1 -> r GStringMethod `a` f x1
    GDefNounName x1 -> r GDefNounName `a` f x1
    GProperNameNounName x1 x2 -> r GProperNameNounName `a` f x1 `a` f x2
    GAdjNounNoun x1 x2 -> r GAdjNounNoun `a` f x1 `a` f x2
    GNounNounNoun x1 x2 -> r GNounNounNoun `a` f x1 `a` f x2
    GProperNameNounNoun x1 x2 -> r GProperNameNounNoun `a` f x1 `a` f x2
    GNounNoun1 x1 -> r GNounNoun1 `a` f x1
    GNounPrepNoun2 x1 x2 -> r GNounPrepNoun2 `a` f x1 `a` f x2
    GNounNounC x1 -> r GNounNounC `a` f x1
    GAbsProof x1 x2 -> r GAbsProof `a` f x1 `a` f x2
    GAnnotateProof x1 x2 -> r GAnnotateProof `a` f x1 `a` f x2
    GAppProof x1 x2 -> r GAppProof `a` f x1 `a` f x2
    GNatIndProof x1 x2 x3 x4 x5 x6 -> r GNatIndProof `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    GUnitsProof x1 -> r GUnitsProof `a` f x1
    GandElProof x1 x2 x3 -> r GandElProof `a` f x1 `a` f x2 `a` f x3
    GandErProof x1 x2 x3 -> r GandErProof `a` f x1 `a` f x2 `a` f x3
    GandIProof x1 x2 x3 x4 -> r GandIProof `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GevenSuccProof x1 x2 -> r GevenSuccProof `a` f x1 `a` f x2
    GexistsEProof x1 x2 x3 x4 x5 x6 x7 x8 -> r GexistsEProof `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6 `a` f x7 `a` f x8
    GexistsIProof x1 x2 x3 x4 x5 -> r GexistsIProof `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    GfalseEProof x1 x2 -> r GfalseEProof `a` f x1 `a` f x2
    GforallEProof x1 x2 x3 x4 x5 -> r GforallEProof `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    GforallIProof x1 x2 x3 x4 x5 -> r GforallIProof `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    GhypoProof x1 x2 -> r GhypoProof `a` f x1 `a` f x2
    GifEProof x1 x2 x3 x4 -> r GifEProof `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GifIProof x1 x2 x3 x4 -> r GifIProof `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GoddSuccProof x1 x2 -> r GoddSuccProof `a` f x1 `a` f x2
    GorEProof x1 x2 x3 x4 x5 x6 x7 x8 -> r GorEProof `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6 `a` f x7 `a` f x8
    GorIlProof x1 x2 x3 -> r GorIlProof `a` f x1 `a` f x2 `a` f x3
    GorIrProof x1 x2 x3 -> r GorIrProof `a` f x1 `a` f x2 `a` f x3
    GreflProof x1 x2 -> r GreflProof `a` f x1 `a` f x2
    GAbsProofExp x1 x2 -> r GAbsProofExp `a` f x1 `a` f x2
    GAnnotateProofExp x1 x2 -> r GAnnotateProofExp `a` f x1 `a` f x2
    GAppProofExp x1 x2 -> r GAppProofExp `a` f x1 `a` f x2
    GLabelProofExp x1 -> r GLabelProofExp `a` f x1
    GAdj2Prop x1 x2 x3 -> r GAdj2Prop `a` f x1 `a` f x2 `a` f x3
    GAdj3Prop x1 x2 x3 x4 -> r GAdj3Prop `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GAdjCCollProp x1 x2 -> r GAdjCCollProp `a` f x1 `a` f x2
    GAdjCProp x1 x2 x3 -> r GAdjCProp `a` f x1 `a` f x2 `a` f x3
    GAdjECollProp x1 x2 -> r GAdjECollProp `a` f x1 `a` f x2
    GAdjEProp x1 x2 x3 -> r GAdjEProp `a` f x1 `a` f x2 `a` f x3
    GAdjProp x1 x2 -> r GAdjProp `a` f x1 `a` f x2
    GAllProp x1 x2 -> r GAllProp `a` f x1 `a` f x2
    GAndProp x1 -> r GAndProp `a` f x1
    GAnnotateProp x1 x2 -> r GAnnotateProp `a` f x1 `a` f x2
    GAppProp x1 x2 -> r GAppProp `a` f x1 `a` f x2
    GBothAndProp x1 x2 -> r GBothAndProp `a` f x1 `a` f x2
    GCoreAllProp x1 x2 x3 -> r GCoreAllProp `a` f x1 `a` f x2 `a` f x3
    GCoreAndProp x1 x2 -> r GCoreAndProp `a` f x1 `a` f x2
    GCoreExistProp x1 x2 x3 -> r GCoreExistProp `a` f x1 `a` f x2 `a` f x3
    GCoreIfProp x1 x2 -> r GCoreIfProp `a` f x1 `a` f x2
    GCoreIffProp x1 x2 -> r GCoreIffProp `a` f x1 `a` f x2
    GCoreNotProp x1 -> r GCoreNotProp `a` f x1
    GCoreOrProp x1 x2 -> r GCoreOrProp `a` f x1 `a` f x2
    GDisplayFormulaProp x1 -> r GDisplayFormulaProp `a` f x1
    GEitherOrProp x1 x2 -> r GEitherOrProp `a` f x1 `a` f x2
    GExistNoProp x1 x2 -> r GExistNoProp `a` f x1 `a` f x2
    GExistProp x1 x2 -> r GExistProp `a` f x1 `a` f x2
    GFormulaImpliesProp x1 x2 -> r GFormulaImpliesProp `a` f x1 `a` f x2
    GFormulaProp x1 -> r GFormulaProp `a` f x1
    GIdentProp x1 -> r GIdentProp `a` f x1
    GIfProp x1 x2 -> r GIfProp `a` f x1 `a` f x2
    GIffIffProp x1 x2 -> r GIffIffProp `a` f x1 `a` f x2
    GIffProp x1 x2 -> r GIffProp `a` f x1 `a` f x2
    GIndexedFormulaProp x1 -> r GIndexedFormulaProp `a` f x1
    GKindProp x1 x2 -> r GKindProp `a` f x1 `a` f x2
    GNoArticleExistProp x1 x2 -> r GNoArticleExistProp `a` f x1 `a` f x2
    GNoCommaAllProp x1 x2 -> r GNoCommaAllProp `a` f x1 `a` f x2
    GNoCommaExistProp x1 x2 -> r GNoCommaExistProp `a` f x1 `a` f x2
    GNotAdj2Prop x1 x2 x3 -> r GNotAdj2Prop `a` f x1 `a` f x2 `a` f x3
    GNotAdjCProp x1 x2 -> r GNotAdjCProp `a` f x1 `a` f x2
    GNotAdjEProp x1 x2 -> r GNotAdjEProp `a` f x1 `a` f x2
    GNotAdjProp x1 x2 -> r GNotAdjProp `a` f x1 `a` f x2
    GNotNoun1Prop x1 x2 -> r GNotNoun1Prop `a` f x1 `a` f x2
    GNotNoun2Prop x1 x2 x3 -> r GNotNoun2Prop `a` f x1 `a` f x2 `a` f x3
    GNotNounCProp x1 x2 -> r GNotNounCProp `a` f x1 `a` f x2
    GNotVerb2Prop x1 x2 x3 -> r GNotVerb2Prop `a` f x1 `a` f x2 `a` f x3
    GNotVerbCProp x1 x2 -> r GNotVerbCProp `a` f x1 `a` f x2
    GNotVerbProp x1 x2 -> r GNotVerbProp `a` f x1 `a` f x2
    GNoun1Prop x1 x2 -> r GNoun1Prop `a` f x1 `a` f x2
    GNoun2Prop x1 x2 x3 -> r GNoun2Prop `a` f x1 `a` f x2 `a` f x3
    GNounCProp x1 x2 x3 -> r GNounCProp `a` f x1 `a` f x2 `a` f x3
    GOnlyIfProp x1 x2 -> r GOnlyIfProp `a` f x1 `a` f x2
    GOrProp x1 -> r GOrProp `a` f x1
    GPostQuantProp x1 x2 -> r GPostQuantProp `a` f x1 `a` f x2
    GProofProp x1 -> r GProofProp `a` f x1
    GVerb2Prop x1 x2 x3 -> r GVerb2Prop `a` f x1 `a` f x2 `a` f x3
    GVerbCProp x1 x2 x3 -> r GVerbCProp `a` f x1 `a` f x2 `a` f x3
    GVerbProp x1 x2 -> r GVerbProp `a` f x1 `a` f x2
    GWeHaveProp x1 -> r GWeHaveProp `a` f x1
    GsameParityProp x1 x2 -> r GsameParityProp `a` f x1 `a` f x2
    GNoVarRewriteRule x1 x2 -> r GNoVarRewriteRule `a` f x1 `a` f x2
    GRewriteRule x1 x2 x3 -> r GRewriteRule `a` f x1 `a` f x2 `a` f x3
    GApp1MacroTerm x1 x2 -> r GApp1MacroTerm `a` f x1 `a` f x2
    GApp2MacroTerm x1 x2 x3 -> r GApp2MacroTerm `a` f x1 `a` f x2 `a` f x3
    GApp3MacroTerm x1 x2 x3 x4 -> r GApp3MacroTerm `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GAppFunctionTerm x1 x2 -> r GAppFunctionTerm `a` f x1 `a` f x2
    GComprehensionTerm x1 x2 x3 -> r GComprehensionTerm `a` f x1 `a` f x2 `a` f x3
    GConstTerm x1 -> r GConstTerm `a` f x1
    GEnumSetTerm x1 -> r GEnumSetTerm `a` f x1
    GIdentTerm x1 -> r GIdentTerm `a` f x1
    GMacroTerm x1 -> r GMacroTerm `a` f x1
    GNumberTerm x1 -> r GNumberTerm `a` f x1
    GOper2Term x1 x2 x3 -> r GOper2Term `a` f x1 `a` f x2 `a` f x3
    GOperTerm x1 x2 -> r GOperTerm `a` f x1 `a` f x2
    GParenthTerm x1 -> r GParenthTerm `a` f x1
    GTextbfTerm x1 -> r GTextbfTerm `a` f x1
    Gintegral_Term x1 x2 x3 x4 -> r Gintegral_Term `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Gseries_Term x1 x2 x3 -> r Gseries_Term `a` f x1 `a` f x2 `a` f x3
    Gsigma_Term x1 x2 x3 x4 -> r Gsigma_Term `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Gsum3dots_Term x1 x2 x3 -> r Gsum3dots_Term `a` f x1 `a` f x2 `a` f x3
    Gtimes_Term x1 x2 -> r Gtimes_Term `a` f x1 `a` f x2
    GStringTitle x1 -> r GStringTitle `a` f x1
    GBeginEnvironmentUnit x1 x2 -> r GBeginEnvironmentUnit `a` f x1 `a` f x2
    GBeginProofMethodUnit x1 x2 -> r GBeginProofMethodUnit `a` f x1 `a` f x2
    GCaseGoal x1 x2 -> r GCaseGoal `a` f x1 `a` f x2
    GEndEnvironmentUnit x1 -> r GEndEnvironmentUnit `a` f x1
    GEnoughGoal x1 -> r GEnoughGoal `a` f x1
    GFirstVerifyGoal x1 -> r GFirstVerifyGoal `a` f x1
    GFollowsPropConclusion x1 -> r GFollowsPropConclusion `a` f x1
    GHyposAssumption x1 -> r GHyposAssumption `a` f x1
    GIdentExpAssumption x1 x2 -> r GIdentExpAssumption `a` f x1 `a` f x2
    GIdentKindAssumption x1 x2 -> r GIdentKindAssumption `a` f x1 `a` f x2
    GImportUnit x1 -> r GImportUnit `a` f x1
    GLabelConclusion x1 -> r GLabelConclusion `a` f x1
    GLabelUnit x1 -> r GLabelUnit `a` f x1
    GPropAssumption x1 x2 -> r GPropAssumption `a` f x1 `a` f x2
    GPropConclusion x1 x2 -> r GPropConclusion `a` f x1 `a` f x2
    GPropLabelConclusion x1 x2 x3 -> r GPropLabelConclusion `a` f x1 `a` f x2 `a` f x3
    GSectionUnit x1 x2 -> r GSectionUnit `a` f x1 `a` f x2
    GSinceConclusion x1 x2 -> r GSinceConclusion `a` f x1 `a` f x2
    GSinceGoal x1 x2 -> r GSinceGoal `a` f x1 `a` f x2
    GSubsectionUnit x1 x2 -> r GSubsectionUnit `a` f x1 `a` f x2
    GVerbPrepNounVerb x1 x2 x3 -> r GVerbPrepNounVerb `a` f x1 `a` f x2 `a` f x3
    GVerbPrepVerb2 x1 x2 -> r GVerbPrepVerb2 `a` f x1 `a` f x2
    GVerbVerbC x1 -> r GVerbVerbC `a` f x1
    GListAdj x1 -> r GListAdj `a` foldr (a . a (r (:)) . f) (r []) x1
    GListArgKind x1 -> r GListArgKind `a` foldr (a . a (r (:)) . f) (r []) x1
    GListExp x1 -> r GListExp `a` foldr (a . a (r (:)) . f) (r []) x1
    GListHypo x1 -> r GListHypo `a` foldr (a . a (r (:)) . f) (r []) x1
    GListIdent x1 -> r GListIdent `a` foldr (a . a (r (:)) . f) (r []) x1
    GListProof x1 -> r GListProof `a` foldr (a . a (r (:)) . f) (r []) x1
    GListProp x1 -> r GListProp `a` foldr (a . a (r (:)) . f) (r []) x1
    GListRule x1 -> r GListRule `a` foldr (a . a (r (:)) . f) (r []) x1
    GListTerm x1 -> r GListTerm `a` foldr (a . a (r (:)) . f) (r []) x1
    GListUnit x1 -> r GListUnit `a` foldr (a . a (r (:)) . f) (r []) x1
    _ -> r t

class Compos t where
  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
         -> (forall a. t a -> m (t a)) -> t c -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m
composOpMonoid = composOpFold mempty mappend

composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composOpMPlus = composOpFold mzero mplus

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }
