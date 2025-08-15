--# -path=.:../Core

concrete ForthelEng of Forthel =
  LatexTermsTex,
  ConstantsEng **
  ForthelFunctor - [negPol, pluralNP] with
    (Syntax=SyntaxEng),
    (Symbolic=SymbolicEng),
    (Extend=ExtendEng),
    (Grammar=GrammarEng),
    (Markup=MarkupEng)

** open
  ParadigmsEng,
  (P=ParadigmsEng),
  (M=MakeStructuralEng),
  (R=ResEng),
  (I=IrregEng),
  Prelude
  
in {

lin
  contradictionConstant = mkNP a_Det (mkN "contradiction") ;

oper
-- functor exceptions

  negPol = Extend.UncontractedNeg ;

  pluralNP : NP -> NP = \np -> np ** {a = R.agrP3 R.Pl} ;

-- words etc

  denote_V2 : V2 =
    mkV2 "denote" | mkV2 I.stand_V for_Prep ; --- allow variants ?

  any_Quant = P.mkQuant "any" "any" ;
  each_Det = M.mkDet "each" ;
  such_that_Subj = mkSubj "such that" ;

  iff_Conj = mkConj "iff" | mkConj "if and only if" ;

  equal_A2 : A2 = mkA2 (mkA "equal") to_Prep ;

  assume_VS : VS = mkVS (mkV "assume") ;

  then_Adv : Adv = P.mkAdv "then" ;

  let_Str : Str = "let" ;
}
