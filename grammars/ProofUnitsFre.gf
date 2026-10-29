concrete ProofUnitsFre of ProofUnits = CategoriesFre **

open
  SyntaxFre,
  ParadigmsFre,
  (P=ParadigmsFre),
  SymbolicFre,
  UtilitiesFre,
  (Grammar=GrammarFre)

in {

lin
  HyposUnit hypos = hypos.text ;
  -- "que" with the indicative, elided before a vowel: that_Subj would take the subjunctive
  GoalHyposUnit hypos prop = prefixText ("pour démontrer" ++ que_Str ++ (mkUtt (topProp prop)).s ++ ",") hypos.text ;

oper
  que_Str : Str = pre {"que" ; ("qu'" ++ Predef.BIND) / strs {"a" ; "à" ; "e" ; "é" ; "è" ; "i" ; "o" ; "u" ; "h"}} ;

lin
  ConclusionUnit prop = mkText (topProp prop) ;

  UnitJmt unit = lin Text {s = ">" ++ unit.s} ; --- not to be used, just to be reached
  
lincat
  [Unit] = Text ;
  Hence = Adv ;

lin
----  UnitsProof units = units ;

  HyposAssumption hypos =
    hypos.text ;
  IdentKindAssumption kind ident =
    mkText (Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP (useKind kind))) ; 
  IdentExpAssumption exp ident =
    mkText (Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP exp)) ; 
  PropAssumption prop label =
    ccText (prefixText "supposons que" (propText prop)) (strText ("(" ++ (mkUtt label.np).s ++ ")")) ;

  PropConclusion hence prop =
    prefixText hence.s (propText prop) ;
  SinceConclusion A B =
    ccText (strText "comme") (commaText (propInText A)) (propText B) ;
  LabelConclusion label =
    strText ("s'ensuit par" ++ (mkUtt label.np).s) ;
  PropLabelConclusion hence prop label =
    ccText (strText hence.s) (propInText prop) (strText ("par" ++ (mkUtt label.np).s ++ ".")) ;
  LabelPropConclusion label prop =
    ccText (strText ("par" ++ (mkUtt label.np).s ++ ",")) (propText prop) ;
  HencePropConclusion hence prop =
    ccText (strText (hence.s ++ ",")) (propText prop) ;
  FollowsPropConclusion prop =
    prefixText "il s'ensuit que" (propText prop) ;
  ObviousConclusion = strText "il est évident ." ;
    
  FirstVerifyGoal prop =
    prefixText "d'abord vérifier que" (mkText (topProp prop)) ;
  EnoughGoal prop = 
    prefixText "il suffit de montrer que" (propText prop) ;
  SinceGoal prop goal =
    ccText (strText "comme") (commaText (propInText prop))
      (prefixText "il suffit de montrer que" (propText goal)) ;
  InductionGoal =
    strText "par récurrence :" ;
  CasesGoal =
    strText "par cas :" ;
  CaseGoal A h =
      ccText (prefixText "\\item supposons que" (propText A)) (strText ("(" ++ h ++ ")")) ;

  noHence = P.mkAdv "" ;
  henceHence = P.mkAdv "donc" ;
  thusHence = P.mkAdv "ainsi" ;
  thenHence = P.mkAdv "alors" ;
  altogetherHence = P.mkAdv "en somme" ;
  afortioriHence = P.mkAdv "a fortiori" ;
  inParticularHence = P.mkAdv "en particulier" ;
  inBothCasesHence = P.mkAdv "dans les deux cas" ;
  weConcludeHence = P.mkAdv "nous concluons que" ;
 
  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;



  -- case analyses: "we have two cases :", "in the first case , assume that A ."
  lincat CaseNumber = Numeral ;
  lin
  NumberCasesGoal n =
    strText ("nous avons" ++ (SyntaxFre.mkUtt (SyntaxFre.mkNP (SyntaxFre.mkDet n) case_N)).s ++ ":") ;
  NumberCaseGoal n A =
    prefixText ((SyntaxFre.mkAdv in_Prep (SyntaxFre.mkNP (SyntaxFre.mkDet the_Quant (SyntaxFre.mkOrd n)) case_N)).s ++ "," ++ "supposons que") (propText A) ;
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
