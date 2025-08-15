-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelSpa of Forthel =
  LatexTermsTex **
  ForthelFunctor - [
    pluralNP, AllTerm, ThereIsNoStatement,
    PredicateSynonym, NamesAssumption, StatementAssumption,
    LetNamesAssumption,
    letSynonym,
    postAdvS
    ] with
    (Syntax=SyntaxSpa),
    (Symbolic=SymbolicSpa),
    (Extend=ExtendSpa),
    (Grammar=GrammarSpa)
---    (Markup=MarkupSpa)

** open

  ParadigmsSpa,
  (P=ParadigmsSpa),
  (M=MakeStructuralSpa),
  (R=ResSpa),
  (I=IrregSpa),
  (X=ExtraSpa),
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

  LetNamesAssumption names classnoun =
    mkPhr
      (lets_Utt (mkVP assume_VS
        (mkS (mkCl (namesNP names) (mkVP (mkCN classnoun.cn classnoun.adv)))))) ;


oper
  letSynonym : NP -> NP -> Text = \dum, dens ->
    mkText (mkS (mkCl dum (mkVP denote_V2 dens))) ;

oper
  pluralNP : NP -> NP = \np -> np ** {a = {g = np.a.g ; n = plural ; p = np.a.p}} ;

  postAdvS : S -> Adv -> S = \s, adv -> s ** {s = \\m => s.s ! m ++ adv.s} ;

-- words etc

  denote_V2 : V2 =
    mkV2 (mkV "denotar") ;

  any_Quant : Quant = a_Quant ; ----
  each_Det = every_Det ; 
  such_that_Subj = M.mkSubjSubj "tal que" ; --- tel/tels/telle/telles

  iff_Conj = M.mkConj [] "si y solo si" singular ;

  equal_A2 : A2 = mkA2 (mkA "igual") dative ;

  assume_VS : VS = mkVS (reflV I.suponer_V) ;

  then_Adv : Adv = P.mkAdv "entonces" ;

  let_Str : Str = "sea" ; --- pl ?
  
}
