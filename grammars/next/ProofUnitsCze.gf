concrete ProofUnitsCze of ProofUnits = CategoriesCze **

-- the functions that proof texts (-proof-text) are made of

open
  SyntaxCze,
  ParadigmsCze,
  (P=ParadigmsCze),
  UtilitiesCze

in {

lin
  HyposUnit hypos = hypos.text ;
  ConclusionUnit prop = mkText (topProp prop) ;

  UnitJmt unit = lin Text {s = ">" ++ unit.s} ; --- not to be used, just to be reached

  -- with -proof-goals: "abychom dokázali, že B, předpokládejme, že A"
  GoalHyposUnit hypos prop = prefixText ("abychom dokázali, že" ++ (mkUtt (topProp prop)).s ++ ",") hypos.text ;

  HyposAssumption hypos = hypos.text ;
  PropAssumption prop label =
    ccText (prefixText "předpokládejme, že" (propText prop)) (strText ("(" ++ (mkUtt label.np).s ++ ")")) ;

  -- "podle" and "z" with the genitive: podle definice sjednocení
  PropConclusion hence prop =
    prefixText hence.s (propText prop) ;
  SinceConclusion A B =
    ccText (strText "protože") (commaText (propInText A)) (propText B) ;
  LabelConclusion label =
    strText ("plyne to" ++ (SyntaxCze.mkAdv z_Prep label.np).s) ;
  PropLabelConclusion hence prop label =
    ccText (strText hence.s) (propInText prop) (strText ((SyntaxCze.mkAdv podle_Prep label.np).s ++ ".")) ;
  LabelPropConclusion label prop =
    ccText (strText ((SyntaxCze.mkAdv podle_Prep label.np).s ++ ",")) (propText prop) ;
  HencePropConclusion hence prop =
    ccText (strText (hence.s ++ ",")) (propText prop) ;
  FollowsPropConclusion prop =
    prefixText "z toho plyne, že" (propText prop) ;
  SinceGoal prop goal =
    ccText (strText "protože") (commaText (propInText prop))
      (prefixText "stačí ukázat, že" (propText goal)) ;
  CaseGoal A h =
    ccText (prefixText "\\item předpokládejme, že" (propText A)) (strText ("(" ++ h ++ ")")) ;

  -- case analyses: "máme dva případy :", "v prvním případě, předpokládejme, že A ."
  NumberCasesGoal n =
    strText ("máme" ++ n.cases ++ ":") ;
  NumberCaseGoal n A =
    prefixText ("v" ++ n.loc ++ "případě, předpokládejme, že") (propText A) ;
  one_CaseNumber = caseNumber "jeden případ" "prvním" ;
  two_CaseNumber = caseNumber "dva případy" "druhém" ;
  three_CaseNumber = caseNumber "tři případy" "třetím" ;
  four_CaseNumber = caseNumber "čtyři případy" "čtvrtém" ;
  five_CaseNumber = caseNumber "pět případů" "pátém" ;
  six_CaseNumber = caseNumber "šest případů" "šestém" ;
  seven_CaseNumber = caseNumber "sedm případů" "sedmém" ;
  eight_CaseNumber = caseNumber "osm případů" "osmém" ;
  nine_CaseNumber = caseNumber "devět případů" "devátém" ;

lincat
  -- the number of cases with its noun ("dva případy", "pět případů"), and the
  -- ordinal in the locative ("v druhém případě")
  CaseNumber = {cases, loc : Str} ;

oper
  caseNumber : Str -> Str -> {cases, loc : Str} = \c, l -> {cases = c ; loc = l} ;
  podle_Prep : SyntaxCze.Prep = mkPrep "podle" genitive ;
  z_Prep : SyntaxCze.Prep = mkPrep "z" genitive ;

lin
  -- the units of tactic proofs (-tactic-proof)
  ObviousConclusion = strText "je to zřejmé ." ;
  FirstVerifyGoal prop =
    prefixText "nejprve ověřme, že" (mkText (topProp prop)) ;
  EnoughGoal prop =
    prefixText "stačí ukázat, že" (propText prop) ;
  InductionGoal =
    strText "postupujeme indukcí :" ;
  CasesGoal =
    strText "rozlišíme případy :" ;

lincat
  [Unit] = Text ;
  Hence = Adv ;

lin
  noHence = P.mkAdv "" ;
  henceHence = P.mkAdv "tedy" ;
  thusHence = P.mkAdv "tudíž" ;
  thenHence = P.mkAdv "pak" ;
  altogetherHence = P.mkAdv "celkem" ;
  afortioriHence = P.mkAdv "tím spíše" ;
  inParticularHence = P.mkAdv "zejména" ;
  inBothCasesHence = P.mkAdv "v obou případech" ;
  weConcludeHence = P.mkAdv "dostáváme, že" ;
  byDefinitionHence = P.mkAdv "z definice" ;
  byRewritingHence = P.mkAdv "přepsáním" ;
  bySimplificationHence = P.mkAdv "zjednodušením" ;
  byComputationHence = P.mkAdv "výpočtem" ;
  byRoutineHence = P.mkAdv "rutinní úvahou" ;

  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;

}
