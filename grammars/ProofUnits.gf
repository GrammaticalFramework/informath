abstract ProofUnits = Categories ** {

-- core functions for proof text linearization

fun
  HyposUnit : [Hypo] -> Unit ;
  GoalHyposUnit : [Hypo] -> Prop -> Unit ;  -- to prove that B , assume that A
  ConclusionUnit : Prop -> Unit ;
  
  UnitJmt : Unit -> Jmt ; --- for top level?



-- old stuff

cat
  [Unit] ;
  Hence ;
  CaseNumber ;  -- the numbers of the cases of a case analysis, one to nine


fun
  --- these are not (yet) interpretable in Dedukti
  UnitsProof : [Unit] -> Proof ;

  HyposAssumption : [Hypo] -> Unit ;
  IdentKindAssumption : Kind -> Ident -> Unit ;
  IdentExpAssumption : Exp -> Ident -> Unit ;
  PropAssumption : Prop -> Label -> Unit ;
  
  PropConclusion : Hence -> Prop -> Unit ;
  SinceConclusion : Prop -> Prop -> Unit ;
  LabelConclusion : Label -> Unit ;
  PropLabelConclusion : Hence -> Prop -> Label -> Unit ;
  LabelPropConclusion : Label -> Prop -> Unit ;  -- by modus ponens, B
  HencePropConclusion : Hence -> Prop -> Unit ;  -- in particular, B
  FollowsPropConclusion : Prop -> Unit ;
  ObviousConclusion : Unit ;
  
  FirstVerifyGoal : Prop -> Unit ;
  EnoughGoal : Prop -> Unit ;
  LabelEnoughGoal : Label -> Prop -> Unit ;  -- by L, it is enough to show that P
  -- a claim verified by the steps that follow, stated forward: P. Indeed:
  HaveIndeedGoal : Prop -> Unit ;
  HaveIndeedLabelGoal : Prop -> Label -> Unit ;  -- P (h). Indeed:
  ConclusionLabelUnit : Prop -> Label -> Unit ;  -- P (h), a fact cited later by its label; the label with its parentheses
  LabelPropLabelConclusion : Label -> Prop -> Label -> Unit ;  -- by L, P (h)
  SinceGoal : Prop -> Prop -> Unit ;
  CasesGoal : Unit ;
  InductionGoal : Unit ;
  CaseGoal : Prop -> Ident -> Unit ;

  -- the case analyses of proof texts: "we have two cases :", and the
  -- assumption of each case, "in the first case , assume that A ."
  NumberCasesGoal : CaseNumber -> Unit ;
  NumberCaseGoal : CaseNumber -> Prop -> Unit ;
  one_CaseNumber, two_CaseNumber, three_CaseNumber, four_CaseNumber, five_CaseNumber,
    six_CaseNumber, seven_CaseNumber, eight_CaseNumber, nine_CaseNumber : CaseNumber ;

  noHence : Hence ;
  henceHence : Hence ;
  thusHence : Hence ;
  thenHence : Hence ;
  altogetherHence : Hence ;
  afortioriHence : Hence ;
  inParticularHence : Hence ;
  inBothCasesHence : Hence ;
  weConcludeHence : Hence ;
  byDefinitionHence : Hence ;  -- rfl
  byRewritingHence : Hence ;  -- rw
  bySimplificationHence : Hence ;  -- simp
  byComputationHence : Hence ;  -- ring, norm_num, omega, linarith
  byRoutineHence : Hence ;  -- grind, aesop, tauto, decide

}