concrete NaturalDeductionCze of NaturalDeduction = CategoriesCze **

-- written by Claude, to be reviewed: the Czech of NaturalDeductionEng

open
  SyntaxCze,
  (P=ParadigmsCze),
  (Grammar=GrammarCze),
  (Extend=ExtendCze),
  UtilitiesCze,
  MathCoreCze,
  IdentifiersLatex,
  ProofUnitsCze

in {

lin
  falseEProof C c =
    ccText c (PropConclusion henceHence C) ;

  andIProof A B a b =
    ccText a b (PropConclusion altogetherHence (CoreAndProp A B)) ;

  andElProof A B c =
    ccText c (PropConclusion afortioriHence A) ;
  andErProof A B c =
    ccText c (PropConclusion afortioriHence B) ;

  orIlProof A B a =
    ccText a (PropConclusion afortioriHence (CoreOrProp A B)) ;
  orIrProof A B b =
    ccText b (PropConclusion afortioriHence (CoreOrProp A B)) ;

  orEProof A B C c x d y e =
    ccText c CasesGoal (ccText (CaseGoal A x) d (CaseGoal B y) e)
      (PropConclusion henceHence C) ;

  ifIProof A B h b =
    ccText (PropAssumption A (IdentLabel h)) b (PropConclusion henceHence (CoreIfProp A B)) ;

  ifEProof A B a b =
    ccText a b (PropConclusion henceHence B) ;

  forallIProof A x B z b =
    ccText (IdentKindAssumption A x) b (PropConclusion henceHence (CoreAllProp A x B)) ;

  forallEProof A x B b a =
    ccText b (PropConclusion inParticularHence B) (strText ("kde" ++ x ++ "je" ++ (mkUtt a).s)) ;

  existsIProof A x B a b =
    ccText b (PropConclusion henceHence (CoreExistProp A x B)) ;

  existsEProof A x B C c x y d =
    ccText c (IdentKindAssumption A x) (PropAssumption B (IdentLabel y)) d (PropConclusion weConcludeHence C);

  hypoProof A h = prefixText ("podle" ++ h ++ ",") (mkText (topProp A)) ;

  reflProof A a =
    ccText
      (mkText (mkS (mkCl a (P.mkV2 (P.seV (P.mkV "rovnat")) P.dative) a)))
      (strText "z reflexivity .") ;

  NatIndProof x C d n h e =
    ccText InductionGoal
      (ccText (IdentExpAssumption (TermExp (NumberTerm (lin Int {s = "0"}))) x) d)
      (ccText (IdentKindAssumption natKind x) (PropAssumption C (IdentLabel h)) e)
      (PropConclusion weConcludeHence (CoreAllProp natKind x C)) ;

  evenZeroProof = strText "podle prvního axiomu parity je $ 0 $ sudé ." ;

  evenSuccProof n c = mkText c (prefixText "podle druhého axiomu parity platí, že"
    (mkText (mkS (mkCl (succNP n) (P.mkA "sudý"))))) ;
  oddSuccProof n c = mkText c (prefixText "podle třetího axiomu parity platí, že"
    (mkText (mkS (mkCl (succNP n) (P.mkA "lichý"))))) ;


oper
  natKind : Kind = NounKind (mkCN (P.mkA "přirozený") cislo_N) ;
  succNP : NP -> NP = \x -> mkNP (mkCN (P.mkN "následník" "následníka" P.mascAnimate) (SyntaxCze.mkAdv possess_Prep x)) ;

}
