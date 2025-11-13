incomplete concrete MathCoreFunctor of MathCore =
  Categories

 ** open
    Utilities,
    Syntax,
    Grammar,
    Markup,
    Extend,
    Symbolic,
    Prelude

in {


lin
  AxiomJmt label hypos prop =
    labelText label
      (thenText hypos (topProp prop)) ;
  ThmJmt label hypos prop proof =
    labelText label
      (mkText
        (thenText hypos (topProp prop))
        (prefixText proof_Str proof)) ;
  DefPropJmt label hypos prop df =
    labelText label
      (thenText hypos (Grammar.SSubjS (partProp prop) if_Subj (partProp df))) ;
  DefKindJmt label hypos kind df =
    labelText label
      (thenText hypos 
        (mkS (mkCl (mkNP a_Det (useKind kind)) (mkNP a_Det (useKind df))))) ;
  DefExpJmt label hypos exp kind df =
    labelText label
      (thenText hypos (mkS (mkCl exp (definedCN (useKind kind) df)))) ;
  AxiomPropJmt label hypos prop =
    labelText label
      (thenText hypos (mkS (mkCl we_NP can_VV (mkVP say_VS (topProp prop))))) ;
  AxiomKindJmt label hypos kind =
    labelText label
      (thenText hypos 
        (mkS (mkCl (mkNP aPl_Det (useKind kind)) (mkNP a_Det basic_type_CN)))) ;
  AxiomExpJmt label hypos exp kind =
    labelText label
      (thenText hypos (mkS (mkCl exp (useKind kind)))) ;

  DefUntypedExpJmt label exp df =
    labelText label
      (mkText (mkS (mkCl exp (definedAdv df)))) ;

  RewriteJmt rules = prefixText by_cases_Str rules ;
  RewriteRule idents patt exp =
    mkUtt (Grammar.ExtAdvS (Syntax.mkAdv for_Prep idents.np) (mkS (mkCl patt exp))) ;
  NoVarRewriteRule patt exp =
    mkUtt (mkS (mkCl patt exp)) ;

  PropHypo prop = mkUtt (mkImp (mkVP assume_VS (topProp prop))) ; 
  VarsHypo idents kind = Grammar.ImpP3 idents.np (mkVP (useKind kind)) ; 
  BareVarsHypo idents = Grammar.ImpP3 idents.np (mkVP arbitrary_A) ;
  LocalHypo local = Grammar.ImpP3 local.name (mkVP local.value) ; 

  LetLocal ident kind exp =
    let ikind = {cn = mkCN kind.cn (latexNP (mkSymb ident)) ; adv = kind.adv ; isPl = False}
    in {name = mkNP the_Det (useKind ikind) ; value = exp} ; 
  BareLetLocal ident exp = {name = latexNP (mkSymb ident) ; value = exp} ; 

  AppExp exp exps = mkNP exp (Syntax.mkAdv applied_to_Prep exps.np) ;
  AbsExp idents exp =
    mkNP the_Det (mkCN function_N (mkRS (mkRCl which_RP map_V3 idents.np exp))) ;
  KindExp kind = mkNP the_Det (mkCN type_CN (Syntax.mkAdv possess_Prep (mkNP aPl_Det (useKind kind)))) ;
  TermExp term = latexNP (mkSymb term.s) ;
  TypedExp exp kind = mkNP the_Det (mkCN (mkCN kind.cn exp) kind.adv) ;
  EnumSetExp exps = mkNP the_Det (mkCN set_N (Syntax.mkAdv possess_Prep exps.np)) ;

  CoreAndProp A B = complexProp (mkS and_Conj (partProp A) (partProp B)) ;
  CoreOrProp A B = complexProp (mkS or_Conj (partProp A) (partProp B)) ;
  CoreIfProp A B = complexProp (Grammar.ExtAdvS (Syntax.mkAdv if_Subj (partProp A)) (mkS then_Adv (partProp B))) ;
  CoreIffProp A B = complexProp (Grammar.SSubjS (partProp A) iff_Subj (partProp B)) ;
  NotProp prop =
    simpleProp (mkS negPol (mkCl 
          (mkVP (mkNP the_Quant (mkCN case_N (Syntax.mkAdv that_Subj (partProp prop))))))) ;
  CoreAllProp ident kind prop =
    simpleProp (Grammar.ExtAdvS (Syntax.mkAdv for_Prep (allNP (identKindCN ident kind))) (partProp prop)) ;
  CoreExistProp ident kind prop =
    simpleProp (Grammar.SSubjS (mkS (Extend.ExistsNP (mkNP a_Det ((identKindCN ident kind))))) such_that_Subj (partProp prop)) ; 
  IdentProp f = simpleProp (latexS (mkSymb f)) ;
  FalseProp = simpleProp (mkS (mkCl we_NP have_V2 (mkNP a_Det contradiction_N))) ;
  AppProp f exps = simpleProp (mkS (mkCl (latexNP (mkSymb f)) hold_V2 exps.np)) ;

  IdentKind ident = {
    cn = mkCN element_N ;
    adv = Syntax.mkAdv possess_Prep (latexNP (mkSymb ident))
    } ;
  SuchThatKind ident kind prop = {
    cn = mkCN kind.cn (latexNP (mkSymb ident)) ;
    adv = ccAdv kind.adv (Syntax.mkAdv such_that_Subj (partProp prop))
    } ;
  AppKind ident exps = {
    cn = mkCN element_N ;
    adv = Syntax.mkAdv possess_Prep (mkNP (latexNP (mkSymb ident)) (Syntax.mkAdv possess_Prep exps.np))
    } ;
  FunKind argkinds kind = {
    cn = mkCN function_N ;
    adv = ccAdv (Syntax.mkAdv from_Prep argkinds.pl) (Syntax.mkAdv to_Prep (mkNP aPl_Det (useKind kind)))
    } ;

  KindArgKind kind = kind ** {isPl = False} ;
  IdentsArgKind kind idents = {cn = mkCN kind.cn idents.np ; adv = kind.adv ; isPl = idents.isPl} ;

  StrLabel s = {np = symb (mkSymb s.s) ; isEmpty = False} ;
  noLabel = {np = symb (mkSymb "") ; isEmpty = True} ;

  definitionLabel = mkLabel definition_Str ;
  theoremLabel = mkLabel theorem_Str ;
  axiomLabel = mkLabel axiom_Str ;

  AppProof exp proofs =
    mkText proofs
      (mkText (mkUtt (Syntax.mkAdv by_Prep exp))) ;
      
  AbsProof hypos proof =
    mkText hypos.text proof ;

  AppProofExp proofexp exps =
    mkNP proofexp (Syntax.mkAdv applied_to_Prep exps.np) ;

  AbsProofExp hypos proofexp =
    mkNP proofexp <lin Adv (prefixText assuming_Str hypos.text) : Adv> ; ---- quick hack for completeness

  OneExps exp =
    {np = exp ; isPl = False} ;
  ManyExps listexp =
    {np = mkNP and_Conj listexp ; isPl = True} ;


-- using Constants

  AdjProp adj exp = simpleProp (mkS (mkCl exp adj)) ;
  NotAdjProp adj exp = simpleProp (mkS negPol (mkCl exp adj)) ;
  Adj2Prop rel x y = simpleProp (mkS (mkCl x (Grammar.AdvAP rel.ap (Syntax.mkAdv rel.prep y)))) ;
  AdjCProp adj exps = simpleProp (mkS (mkCl exps.np adj)) ;
  NotAdjCProp adj exps = simpleProp (mkS negPol (mkCl exps.np adj)) ;
  AdjEProp adj exps = simpleProp (mkS (mkCl exps.np adj)) ;
  NotAdjEProp adj exps = simpleProp (mkS negPol (mkCl exps.np adj)) ;
  
  NounKind noun = {cn = noun ; adv = lin Adv {s = []}} ;
  NameExp name = name ;
  FunExp f exp = mkNP the_Det (mkCN f.cn (Syntax.mkAdv f.prep exp)) ;
  Fun2Exp f x y = case f.isColl of {
    True => mkNP the_Det (mkCN f.cn (Syntax.mkAdv f.prep1 (mkNP and_Conj x y))) ;
    _ => mkNP the_Det (mkCN (mkCN f.cn (Syntax.mkAdv f.prep1 x)) (Syntax.mkAdv f.prep2 y))
    } ;
  FunCExp f exps = mkNP the_Det (mkCN f.cn (Syntax.mkAdv f.prep exps.np)) ;
  LabelProofExp label = label.np ;
  FamKind fam kind = {cn = fam.cn ; adv = Syntax.mkAdv fam.prep (mkNP aPl_Det (useKind kind))} ;
  Fam2Kind fam kind1 kind2 =
    let
      k1 = mkNP aPl_Det (useKind kind1) ;
      k2 = mkNP aPl_Det (useKind kind2)
    in  
    {cn = fam.cn ; adv = case fam.isCollective of {
      False => ccAdv (Syntax.mkAdv fam.prep1 k1) (Syntax.mkAdv fam.prep2 k2) ;  
      True => Syntax.mkAdv fam.prep1 (mkNP and_Conj k1 k2)  
      }
    } ;
  Noun1Prop noun exp = simpleProp (mkS (mkCl exp noun)) ; 
  VerbProp verb exp = simpleProp (mkS (mkCl exp verb)) ; 
  Verb2Prop verb x y = simpleProp (mkS (mkCl x verb y)) ; 
  Noun2Prop rel x y = simpleProp (mkS (mkCl x (mkCN rel.cn (Syntax.mkAdv rel.prep y)))) ; 
  NotVerbProp verb exp = simpleProp (mkS negPol (mkCl exp verb)) ; 
  NotNoun1Prop noun exp = simpleProp (mkS negPol (mkCl exp noun)) ; 
  NotAdj2Prop adj x y = simpleProp (mkS negPol (mkCl x (Grammar.AdvAP adj.ap (Syntax.mkAdv adj.prep y)))) ;
  NotVerb2Prop verb x y = simpleProp (mkS negPol (mkCl x verb y)) ;
  NotNoun2Prop rel x y = simpleProp (mkS negPol (mkCl x (mkCN rel.cn (Syntax.mkAdv rel.prep y)))) ; 
  Adj3Prop pred x y z =
    simpleProp (mkS (mkCl x (AdvAP (AdvAP pred.ap (Syntax.mkAdv pred.prep1 y)) (Syntax.mkAdv pred.prep2 z)))) ;

-- coercions, to disappear in Core2Informath
-- their purpose is to maintain lossless rendering of Dedukti

  ProofProp prop = prop ** {
    s = mkS (mkCl we_NP can_VV (mkVP prove_VS prop.s)) ;
    } ;
  ElemKind kind = {
    cn = mkCN instance_N ;
    adv = Syntax.mkAdv possess_Prep (mkNP aPl_Det (useKind kind))
    } ;

  CoercionExp coercion exp =
    mkNP
      (mkNP the_Det (mkCN coercion.from exp))
      (Syntax.mkAdv as_Prep (mkNP a_Det coercion.to)) ;

oper

  notionNP : {np : NP ; isPl : Bool} -> {cn : CN ; adv : Adv} -> NP = \idents, kind ->
    let det = case idents.isPl of {
      True => aPl_Det ; 
      False => a_Det
      }
    in mkNP det (mkCN (mkCN kind.cn idents.np) kind.adv) ;

  definedCN : CN -> NP -> CN = \cn, np ->
    mkCN cn (Syntax.mkAdv defined_as_Prep np) ;
    
  definedAdv : NP -> Adv = \df ->
    Syntax.mkAdv defined_as_Prep df ;

  by_Prep : Prep = by8means_Prep ;

  ccAdv : Adv -> Adv -> Adv = \x, y -> lin Adv {s = x.s ++ y.s} ;

  identKindCN : Str -> {cn : CN ; adv : Adv} -> CN = \ident, kind ->
    mkCN (mkCN kind.cn (latexNP (mkSymb ident))) kind.adv ;

  allNP : CN -> NP = \cn ->
    mkNP all_Predet (mkNP aPl_Det cn) ;

-- non-functor
  negPol : Pol = negativePol ;

}