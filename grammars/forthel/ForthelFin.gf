-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelFin of Forthel =
  LatexTermsTex **
  ForthelFunctor - [pluralNP, AnyTerm, ThereIsNoStatement, therefore_Adv] with
    (Syntax=SyntaxFin),
    (Symbolic=SymbolicFin),
    (Extend=ExtendFin),
    (Grammar=GrammarFin),
    (Markup=MarkupFin)

** open
  (S=SyntaxFin),
  ParadigmsFin,
  (P=ParadigmsFin),
  (M=MakeStructuralFin),
  (R=ResFin),
  Prelude
  
in {

oper
-- functor exceptions

  pluralNP : NP -> NP = \np -> np ** {a = R.agrP3 R.Pl} ;

lin
  AnyTerm notion = case notion.isPlur of {
    True => S.mkNP S.somePl_Det notion.cn ;
    False => S.mkNP S.someSg_Det notion.cn
    } ;


  ThereIsNoStatement notion =
    mkS negativePol (Extend.ExistsNP (mkNP no_Quant notion.cn)) ;

oper
  therefore_Adv = P.mkAdv "silloin" ;

-- words etc

oper
  denote_V2 : V2 =
    mkV2 "tarkoittaa" partitive ;

  each_Det = every_Det ;
  such_that_Subj = mkSubj "siten että" ;

  iff_Conj = mkConj "jos ja vain jos" ;

  equal_A2 : A2 = mkA2 (mkA (mkN "yhtä" (mkN "suuri" "suuria"))) (mkPrep "kuin" nominative) ;

  assume_VS : VS = mkVS (mkV "olettaa") ;

  then_Adv : Adv = P.mkAdv "niin" ;

  let_Str : Str = "olkoon" ;
}
