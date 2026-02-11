concrete ProofUnitsEng of ProofUnits = CategoriesEng **

open
  SyntaxEng,
  ParadigmsEng,
  (P=ParadigmsEng),
  SymbolicEng,
  UtilitiesEng,
  (Grammar=GrammarEng)

in {

lincat
  [Unit] = Text ;
  Hence = Adv ;

lin
----  UnitsProof units = units ;
  UnitJmt unit = unit ;

  HyposAssumption hypos =
    hypos.text ;
  IdentKindAssumption kind ident =
    mkText (Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP (useKind kind))) ; 
  IdentExpAssumption exp ident =
    mkText (Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP exp)) ; 
  PropAssumption prop =
    prefixText "assume that" (propText prop) ;

  PropConclusion hence prop =
    prefixText hence.s (propText prop) ;
  SinceConclusion A B =
    ccText (strText "since") (commaText (propInText A)) (propText B) ;
  LabelConclusion label =
    strText ("follows by" ++ (mkUtt label.np).s) ;
  PropLabelConclusion hence prop label =
    ccText (strText hence.s) (propText prop) (strText ("by" ++ (mkUtt label.np).s)) ;
  FollowsPropConclusion prop =
    prefixText "it follows that" (propText prop) ;
  ObviousConclusion = strText "it is obvious ." ;
  
  FirstVerifyGoal prop =
    prefixText "first verify that" (mkText (topProp prop)) ;
  EnoughGoal prop = 
    prefixText "it is enough to show that" (propText prop) ;
  SinceGoal prop goal =
    ccText (strText "since") (commaText (propInText prop))
      (prefixText "it is enough to show that" (propText goal)) ;
  InductionGoal =
    strText "we reason by induction :" ;
  CasesGoal =
    strText "we reason by cases :" ;
  CaseGoal A =
    prefixText "\\item assume that" (propText A) ;

  noHence = P.mkAdv "" ;
  henceHence = P.mkAdv "hence" ;
  thusHence = P.mkAdv "thus" ;
  thenHence = P.mkAdv "then" ;
  altogetherHence = P.mkAdv "altogether" ;
  afortioriHence = P.mkAdv "a fortiori" ;
  inParticularHence = P.mkAdv "in particular" ;
  weConcludeHence = P.mkAdv "we conclude that" ;
  
  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;


}