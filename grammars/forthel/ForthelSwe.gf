-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelSwe of Forthel =
  LatexTermsTex **

  ForthelFunctor - [
    pluralNP, 
    IffStatement, NamesAssumption,
    StatementAssumption, PredicateDefinition,
    ApposTermSymb,
    therefore_Adv,
    postAdvS
    ]
   with
    (Syntax=SyntaxSwe),
    (Symbolic=SymbolicSwe),
    (Extend=ExtendSwe),
    (Grammar=GrammarSwe),
    (Markup=MarkupSwe)

** open

  ParadigmsSwe,
  (S=SyntaxSwe),
  (P=ParadigmsSwe),
  (M=MakeStructuralSwe),
  (R=ResSwe),
  (I=IrregSwe),
  Prelude
  
in {

-- functor exceptions
lin
  IffStatement s t = postAdvS s (S.mkAdv iff_Subj t) ;

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
  pluralNP : NP -> NP = \np -> np ** {a = {g = np.a.g ; p = np.a.p ; n = plural}} ;
  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;

  therefore_Adv : Adv = P.mkAdv "då" ;

-- words etc


  denote_V2 : V2 = mkV2 (mkV "betyder") ;

  any_Quant = M.mkQuant "någon" "något" "några" ;
  each_Det = every_Det ; --- = jeder

  such_that_Subj = M.mkSubj "så att" ;

  equal_A2 : A2 = mkA2 (mkA "lika") with_Prep ;

  assume_VS : VS = mkVS (mkV "anta" "antar" "anta" "antog" "antagit" "antagen") ;

  then_Adv : Adv = P.mkAdv "så" ;

  let_Str : Str = "vare" ; --- 

  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\o => s.s ! o ++ adv.s} ;

  iff_Subj : Subj = M.mkSubj "om och endast om" ;
  
}
