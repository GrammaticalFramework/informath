-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelIta of Forthel =
  LatexTermsTex **
  ForthelFunctor - [
    pluralNP, AllTerm, ThereIsNoStatement,
    PredicateSynonym, NamesAssumption, StatementAssumption,
    postAdvS
    ] with
    (Syntax=SyntaxIta),
    (Symbolic=SymbolicIta),
    (Extend=ExtendIta),
    (Grammar=GrammarIta)
---    (Markup=MarkupIta)

** open

  ParadigmsIta,
  (P=ParadigmsIta),
  (M=MakeStructuralIta),
  (R=ResIta),
  (I=IrregIta),
  (X=ExtraIta),
  Prelude
  
in {

-- functor exceptions
lin
  AllTerm notion = mkNP all_Predet (mkNP thePl_Det notion.cn) ;

  ThereIsNoStatement notion = mkS negativePol (Extend.ExistsNP (mkNP no_Quant notion.cn)) ;

  NamesAssumption names classnoun =
    mkPhr 
      (lets_Utt (mkVP assume_VS
         (mkS (mkCl (namesNP names) (mkCN classnoun.cn classnoun.adv))))) ;
      
  StatementAssumption stat =
    mkPhr 
      (lets_Utt (mkVP assume_VS stat)) ;
oper
  pluralNP : NP -> NP = \np -> np ** {a = {g = np.a.g ; n = plural ; p = np.a.p}} ;
  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\m => s.s ! m ++ adv.s} ;
  
-- words etc

  denote_V2 : V2 =
    mkV2 (mkV "denotare") ;

  any_Quant : Quant = a_Quant ; ----
  each_Det = every_Det ; 
  such_that_Subj = M.mkSubjSubj "tale che" ; --- tel/tels/telle/telles

  iff_Conj = M.mkConj [] "se e solo se" singular ;

  equal_A2 : A2 = mkA2 (mkA "uguale") dative ;

  assume_VS : VS = mkVS I.supporre_V ;

  then_Adv : Adv = P.mkAdv "allora" ;

  let_Str : Str = "seja" ; --- pl ?
  
}

