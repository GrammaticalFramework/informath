-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelGer of Forthel =
  LatexTermsTex **

  ForthelFunctor - [
    pluralNP, AllTerm,
    IffStatement, NamesAssumption,
    StatementAssumption, PredicateDefinition,
    possessAdv, ApposTermSymb,
    postAdvS

    ]
   with
    (Syntax=SyntaxGer),
    (Symbolic=SymbolicGer),
    (Extend=ExtendGer),
    (Grammar=GrammarGer),
    (Markup=MarkupGer)

** open

  ParadigmsGer,
  (S=SyntaxGer),
  (P=ParadigmsGer),
  (M=MakeStructuralGer),
  (R=ResGer),
  (I=IrregGer),
  Prelude
  
in {

-- functor exceptions
lin
  IffStatement s t = postAdvS s (S.mkAdv iff_Subj t) ;

  AllTerm notion = mkNP alle_Det notion.cn ;

  NamesAssumption names classnoun =
    mkPhr 
      (mkS (mkCl we_NP assume_VS
         (mkS (mkCl (namesNP names) (mkCN classnoun.cn classnoun.adv))))) ;

  PredicateDefinition pred names statement =
    postAdvS (mkS (mkCl (namesNP names) pred)) (S.mkAdv iff_Subj statement) ;

  StatementAssumption stat =
    mkPhr 
      (mkS (mkCl we_NP assume_VS stat)) ;

  ApposTermSymb primc name = {
    np = mkNP the_Det (mkCN (mkCN primc.cn (symb name)) primc.adv) ;
    sym = name
    } ;

oper
  postAdvS : S -> Adv -> S = \s, adv -> lin S {s = \\o => s.s ! o ++ adv.s} ; --- also Sub ?


  pluralNP : NP -> NP = \np -> np ** {a = R.AgPl R.P3} ;

  possessAdv : NP -> Adv = \np -> mkAdv genPrep np ;

-- words etc


  denote_V2 : V2 = mkV2 (mkV "bedeuten") ;

  any_Quant = M.mmkQuant a_Quant (mkA "beliebig") ;
  each_Det = every_Det ; --- = jeder
  alle_Det = M.mkWeakDet "all" plural ;

  such_that_Subj = M.mkSubj "derart dass" ;

  equal_A2 : A2 = mkA2 (mkA "gleich") datPrep ;

  assume_VS : VS = mkVS (prefixV "an" I.nehmen_V) ;

  then_Adv : Adv = P.mkAdv "dann" ;

  let_Str : Str = "sei" ; --- seien ?

  iff_Subj : Subj = M.mkSubj "wenn und genau dann wenn" ;
  
}
