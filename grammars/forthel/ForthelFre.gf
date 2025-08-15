-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelFre of Forthel =
  LatexTermsTex **
  ForthelFunctor - [
    pluralNP, AllTerm, ThereIsNoStatement,
    PredicateSynonym, NamesAssumption, StatementAssumption,
    postAdvS
    ] with
    (Syntax=SyntaxFre),
    (Symbolic=SymbolicFre),
    (Extend=ExtendFre),
    (Grammar=GrammarFre)
---    (Markup=MarkupFre)

** open

  ParadigmsFre,
  (P=ParadigmsFre),
  (M=MakeStructuralFre),
  (R=ResFre),
  (I=IrregFre),
  (X=ExtraFre),
  Prelude
  
in {

-- functor exceptions
lin
  AllTerm notion = mkNP all_Predet (mkNP thePl_Det notion.cn) ;

  ThereIsNoStatement notion = mkS X.PNegNe (Extend.ExistsNP (mkNP no_Quant notion.cn)) ;

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
    mkV2 (mkV "signifier") ;

  any_Quant : Quant =
     let niq = "n'importe quel" in
     M.mkQuant niq (niq + "le") (niq + "s") (niq + "les") ;
  each_Det = every_Det ; 
  such_that_Subj = M.mkSubjSubj "tel que" ; --- tel/tels/telle/telles

  iff_Conj = M.mkConj [] "si et seulement si" singular ;

  equal_A2 : A2 = mkA2 (mkA "égal") dative ;

  assume_VS : VS = mkVS (mkV "supposer") ;

  then_Adv : Adv = P.mkAdv "alors" ;

  let_Str : Str = "soit" ; --- seien ?
  
}
