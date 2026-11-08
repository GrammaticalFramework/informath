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
  weConcludeHence = P.mkAdv "dostáváme, že" ;
  byDefinitionHence = P.mkAdv "z definice" ;
  byRewritingHence = P.mkAdv "přepsáním" ;
  bySimplificationHence = P.mkAdv "zjednodušením" ;
  byComputationHence = P.mkAdv "výpočtem" ;
  byRoutineHence = P.mkAdv "rutinní úvahou" ;

  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;

}
