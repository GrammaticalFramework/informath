concrete NaturalDeductionEng of NaturalDeduction = CategoriesEng **

open
  SyntaxEng,
  (P=ParadigmsEng),
  (Grammar=GrammarEng),
  (Extend=ExtendEng),
  UtilitiesEng,
  MathCoreEng,
  IdentifiersLatex,
  ProofUnitsEng

--- quick and dirty implementation, needs more structure and functorization

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
    ccText c CasesGoal (ccText (CaseGoal A) d (CaseGoal B) e)
      (PropConclusion henceHence C) ;
    
  ifIProof A B h b =
    ccText (PropAssumption A) b (PropConclusion henceHence (CoreIfProp A B)) ;

  ifEProof A B a b =
    ccText a b (PropConclusion henceHence B) ;

  forallIProof A x B z b =
    ccText (IdentKindAssumption A x) b (PropConclusion henceHence (CoreAllProp A x B)) ;

  forallEProof A x B b a =
    ccText b (PropConclusion inParticularHence B) (strText ("where" ++ x ++ "is" ++ (mkUtt a).s)) ;

  existsIProof A x B a b =
    ccText b (PropConclusion henceHence (CoreExistProp A x B)) ;

  existsEProof A x B C c x y d =
    ccText c (IdentKindAssumption A x) (PropAssumption B) d (PropConclusion weConcludeHence C);

  hypoProof A h = prefixText ("by" ++ h ++ ",") (mkText (topProp A)) ;
  
  reflProof A a =
    ccText
      (mkText (mkS (mkCl a (P.mkV2 "equal") a)))
      (strText "by reflexivity .") ;

  NatIndProof x C d n h e =
    ccText InductionGoal
      (ccText (IdentExpAssumption (TermExp (NumberTerm (lin Int {s = "0"}))) x) d)
      (ccText (IdentKindAssumption natKind x) (PropAssumption C) e)
      (PropConclusion weConcludeHence (CoreAllProp natKind x C)) ;

  evenZeroProof = strText "by the first axiom of parity , $ 0 $ is even ." ;
  
  evenSuccProof n c = mkText c (prefixText "by the second axiom of parity ,"
    (mkText (mkS (mkCl (succNP n) (P.mkA "even"))))) ;
  oddSuccProof n c = mkText c (prefixText "by the third axiom of parity ,"
    (mkText (mkS (mkCl (succNP n) (P.mkA "odd"))))) ;
    

oper
  natKind : Kind = NounKind (mkNoun "natural number") ;
  succNP : NP -> NP = \x -> mkNP the_Det (mkCN (P.mkN "successor") (mkAdv possess_Prep x)) ;
  assumeText : Str -> S -> Text = \h, s ->
    mkText (mkText (mkUtt (mkImp (mkVP assume_VS s)))) ((strText ("(" ++ h ++ ")"))) ;


}