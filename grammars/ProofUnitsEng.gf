concrete ProofUnitsEng of ProofUnits = CategoriesEng **

open
  SyntaxEng,
  ParadigmsEng,
  (P=ParadigmsEng),
  SymbolicEng,
  UtilitiesEng,
  (Grammar=GrammarEng)

in {

lin
  HyposUnit hypos = hypos.text ;
  GoalHyposUnit hypos prop = prefixText ("to prove" ++ (SyntaxEng.mkAdv that_Subj (partProp prop)).s ++ ",") hypos.text ;
  ConclusionUnit prop = mkText (topProp prop) ;

  UnitJmt unit = lin Text {s = ">" ++ unit.s} ; --- not to be used, just to be reached



-- old stuff

lincat
  [Unit] = Text ;
  Hence = Adv ;

lin
----  UnitsProof units = units ;

---  HyposAssumption hypos =
---    hypos.text ;
  IdentKindAssumption kind ident =
    mkText (Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP (useKind kind))) ; 
  IdentExpAssumption exp ident =
    mkText (Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP exp)) ; 
  PropAssumption prop label =
    ccText (prefixText "assume that" (propText prop)) (strText ("(" ++ (mkUtt label.np).s ++ ")")) ;

  PropConclusion hence prop =
    prefixText hence.s (propText prop) ;
  SinceConclusion A B =
    ccText (strText "since") (commaText (propInText A)) (propText B) ;
  LabelConclusion label =
    strText ("follows by" ++ (mkUtt label.np).s) ;
  PropLabelConclusion hence prop label =
    ccText (strText hence.s) (propInText prop) (strText ("by" ++ (mkUtt label.np).s ++ ".")) ;
  LabelPropConclusion label prop =
    ccText (strText ("by" ++ (mkUtt label.np).s ++ ",")) (propText prop) ;
  HencePropConclusion hence prop =
    ccText (strText (hence.s ++ ",")) (propText prop) ;
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
  CaseGoal A h =
    ccText (prefixText "\\item assume that" (propText A)) (strText ("(" ++ h ++ ")")) ;

  noHence = P.mkAdv "" ;
  henceHence = P.mkAdv "hence" ;
  thusHence = P.mkAdv "thus" ;
  thenHence = P.mkAdv "then" ;
  altogetherHence = P.mkAdv "altogether" ;
  afortioriHence = P.mkAdv "a fortiori" ;
  inParticularHence = P.mkAdv "in particular" ;
  inBothCasesHence = P.mkAdv "in both cases" ;
  weConcludeHence = P.mkAdv "we conclude that" ;
  
  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;



  -- case analyses: "we have two cases :", "in the first case , assume that A ."
  lincat CaseNumber = Numeral ;
  lin
  NumberCasesGoal n =
    strText ("we have" ++ (SyntaxEng.mkUtt (SyntaxEng.mkNP (SyntaxEng.mkDet n) case_N)).s ++ ":") ;
  NumberCaseGoal n A =
    prefixText ((SyntaxEng.mkAdv in_Prep (SyntaxEng.mkNP (SyntaxEng.mkDet the_Quant (SyntaxEng.mkOrd n)) case_N)).s ++ "," ++ "assume that") (propText A) ;
  one_CaseNumber = mkNumeral "1" ;
  two_CaseNumber = mkNumeral "2" ;
  three_CaseNumber = mkNumeral "3" ;
  four_CaseNumber = mkNumeral "4" ;
  five_CaseNumber = mkNumeral "5" ;
  six_CaseNumber = mkNumeral "6" ;
  seven_CaseNumber = mkNumeral "7" ;
  eight_CaseNumber = mkNumeral "8" ;
  nine_CaseNumber = mkNumeral "9" ;
}