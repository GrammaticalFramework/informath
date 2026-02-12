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
        (prefixTextFullStop proof_Str proof)) ;
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

  RewriteJmt rules =
    prefixText by_cases_Str (embedText begin_itemize_str end_itemize_str rules) ;
  RewriteRule idents patt exp =
    mkUtt (Grammar.ExtAdvS (Syntax.mkAdv for_Prep idents.np) (mkS (mkCl patt exp))) ;
  NoVarRewriteRule patt exp =
    mkUtt (mkS (mkCl patt exp)) ;

  PropHypo prop = mkUtt (mkImp (mkVP assume_VS (topProp prop))) ; 
  VarHypo ident kind = Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP (useKind kind)) ; 
  BareVarHypo ident = Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP arbitrary_A) ;
  LocalHypo local = Grammar.ImpP3 local.name (mkVP local.value) ; 

  LetLocal ident kind exp =
    let ikind = {cn = mkCN kind.cn (latexNP (mkSymb ident)) ; adv = kind.adv ; isPl = False}
    in {name = mkNP the_Det (useKind ikind) ; value = exp} ; 
  BareLetLocal ident exp = {name = latexNP (mkSymb ident) ; value = exp} ; 

  AppExp exp exps = mkNP exp (Syntax.mkAdv applied_to_Prep exps.np) ;
  CoreAbsExp ident exp =
    mkNP the_Det (mkCN function_N (mkRS (mkRCl which_RP map_V3 (latexNP (mkSymb ident)) exp))) ;
  KindExp kind = mkNP the_Det (mkCN type_CN (Syntax.mkAdv possess_Prep (mkNP aPl_Det (useKind kind)))) ;
  TermExp term = latexNP (mkSymb term.s) ;
  TypedExp exp kind = mkNP the_Det (mkCN (mkCN kind.cn exp) kind.adv) ;
  EnumSetExp exps = mkNP the_Det (mkCN set_N (Syntax.mkAdv possess_Prep exps.np)) ;

  CoreAndProp A B = complexProp (mkS and_Conj (partProp A) (partProp B)) ;
  CoreOrProp A B = complexProp (mkS or_Conj (partProp A) (partProp B)) ;
  CoreIfProp A B = complexProp (Grammar.ExtAdvS (Syntax.mkAdv if_Subj (partProp A)) (mkS then_Adv (partProp B))) ;
  CoreIffProp A B = complexProp (Grammar.SSubjS (partProp A) iff_Subj (partProp B)) ;
  CoreNotProp prop =
    simpleProp (mkS negPol (mkCl 
          (mkVP (mkNP the_Quant (mkCN case_N (Syntax.mkAdv that_Subj (partProp prop))))))) ;
  CoreAllProp kind ident prop =
    simpleProp (Grammar.ExtAdvS (Syntax.mkAdv for_Prep (allNP (identKindCN ident kind))) (partProp prop)) ;
  CoreExistProp kind ident prop =
    simpleProp (Grammar.SSubjS (mkS (Extend.ExistsNP (mkNP a_Det ((identKindCN ident kind))))) such_that_Subj (partProp prop)) ; 
  IdentProp f = simpleProp (latexS (mkSymb f)) ;
  FalseProp = simpleProp (mkS (mkCl we_NP have_V2 (mkNP a_Det contradiction_N))) ;
  AppProp f exps = simpleProp (mkS (mkCl (latexNP (mkSymb f)) hold_V2 exps.np)) ;

  IdentKind ident = {
    cn = mkCN element_N ;
    adv = Syntax.mkAdv possess_Prep (latexNP (mkSymb ident))
    } ;
  SuchThatKind kind ident prop = {
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
  IdentArgKind kind ident = {cn = mkCN kind.cn (latexNP (mkSymb ident)) ; adv = kind.adv ; isPl = False} ;

  IdentLabel s = {np = symb (mkSymb s) ; isEmpty = False} ;
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
  Adj2Prop rel x y = simpleProp (mkS (mkCl x (Grammar.AdvAP rel.ap (Syntax.mkAdv rel.prep y)))) ;
  AdjCProp adj x y = simpleProp (mkS (mkCl (mkNP and_Conj x y) adj)) ;
  AdjEProp adj x y = simpleProp (mkS (mkCl (mkNP and_Conj x y) adj)) ;
    
  NounKind noun = {cn = noun ; adv = lin Adv {s = []}} ;
  NameExp name = name ;
  FunExp f exp = mkNP the_Det (mkCN f.cn (Syntax.mkAdv f.prep exp)) ;
  Fun2Exp f x y = case f.isColl of {
    True => mkNP the_Det (mkCN f.cn (Syntax.mkAdv f.prep1 (mkNP and_Conj x y))) ;
    _ => mkNP the_Det (mkCN (mkCN f.cn (Syntax.mkAdv f.prep1 x)) (Syntax.mkAdv f.prep2 y))
    } ;
  FunCExp f x y = mkNP the_Det (mkCN f.cn (Syntax.mkAdv f.prep (mkNP and_Conj x y))) ;
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


}