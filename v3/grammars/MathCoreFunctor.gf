incomplete concrete MathCoreFunctor of MathCore =
  Terms,
  UserConstants

 ** open
    Syntax,
    Grammar,
    Markup,
    Extend,
    Symbolic,
    Prelude

in {

lincat
  Exp = NP ;
  Kind = {cn : CN ; adv : Adv} ;
  Prop = Proposition ;
  Jmt = Text ;
  Exps = {np : NP ; isPl : Bool} ;
  [Prop] = Grammar.ListS ;
  ArgKind = {cn : CN ; adv : Adv} ;
  [ArgKind] = NP ;
  Hypo = Utt ;
  [Hypo] = {text : Text ; isEmpty : Bool} ;
  [Ident] = {np : NP ; isPl : Bool} ;
  Proof = Text ;
  [Proof] = Text ;
  Rule = Utt ;
  [Rule] = Text ;
  Coercion = {from, to : CN} ;  -- the <from> <Exp> as <to>

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

  RewriteJmt rules = prefixText by_cases_Str rules ;
  RewriteRule idents patt exp =
    mkUtt (Grammar.ExtAdvS (Syntax.mkAdv for_Prep idents.np) (mkS (mkCl patt exp))) ;
  NoVarRewriteRule patt exp =
    mkUtt (mkS (mkCl patt exp)) ;

  PropHypo prop = mkUtt (mkImp (mkVP assume_VS (topProp prop))) ; 
  VarsHypo idents kind = Grammar.ImpP3 idents.np (mkVP (useKind kind)) ; 
  BareVarsHypo idents = Grammar.ImpP3 idents.np (mkVP arbitrary_A) ;

  AppExp exp exps = mkNP exp (Syntax.mkAdv applied_to_Prep exps.np) ;
  AbsExp idents exp =
    mkNP the_Det (mkCN function_N (mkRS (mkRCl which_RP map_V3 idents.np exp))) ; 
  TermExp term = latexNP (mkSymb term.s) ;
  TypedExp exp kind = mkNP the_Det (mkCN (mkCN kind.cn exp) kind.adv) ;

  AndProp props = complexProp (mkS and_Conj props) ;
  OrProp props = complexProp (mkS or_Conj props) ;
  IfProp A B = complexProp (Grammar.ExtAdvS (Syntax.mkAdv if_Subj (partProp A)) (mkS then_Adv (partProp B))) ;
  IffProp A B = complexProp (Grammar.SSubjS (partProp A) iff_Subj (partProp B)) ;
  NotProp prop =
    simpleProp (mkS negPol (mkCl 
          (mkVP (mkNP the_Quant (mkCN case_N (Syntax.mkAdv that_Subj (partProp prop))))))) ;
  AllProp argkinds prop =
    simpleProp (Grammar.ExtAdvS (Syntax.mkAdv for_Prep (mkNP all_Predet argkinds)) (partProp prop)) ;
  ExistProp argkinds prop =
    simpleProp (Grammar.SSubjS (mkS (Extend.ExistsNP argkinds)) such_that_Subj (partProp prop)) ; ---- TODO: sg/pl correctly
  IdentProp f = simpleProp (latexS (mkSymb f)) ;
  FalseProp = simpleProp (mkS (mkCl we_NP have_V2 (mkNP a_Det contradiction_N))) ;
  AppProp f exps = simpleProp (mkS (mkCl (latexNP (mkSymb f)) hold_V2 exps.np)) ;

  TermKind term = {
    cn = mkCN element_N ;
    adv = Syntax.mkAdv possess_Prep (latexNP (mkSymb term.s))
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
    adv = ccAdv (Syntax.mkAdv from_Prep argkinds) (Syntax.mkAdv to_Prep (mkNP aPl_Det (useKind kind)))
    } ;

  KindArgKind kind = kind ;
  IdentsArgKind kind idents = {cn = mkCN kind.cn idents.np ; adv = kind.adv} ;

  StrLabel s = {np = symb (mkSymb s.s) ; isEmpty = False} ;
  noLabel = {np = symb (mkSymb "") ; isEmpty = True} ;

  definitionLabel = mkLabel definition_Str ;
  theoremLabel = mkLabel theorem_Str ;
  axiomLabel = mkLabel axiom_Str ;

  AppProof proofs exp =
    mkText proofs
      (mkText (mkUtt (Syntax.mkAdv by_Prep exp))) ;
      
  AbsProof hypos proof =
    mkText hypos.text proof ;

  BaseIdent ident =
    {np = latexNP (mkSymb ident) ; isPl = False} ;
  ConsIdent ident idents =
    {np = mkNP and_Conj (latexNP (mkSymb ident)) idents.np ; isPl = True} ;

  OneExps exp =
    {np = exp ; isPl = False} ;
  AddExps exp exps =
    {np = mkNP and_Conj exp exps.np ; isPl = True} ;

  BaseArgKind kind =
    mkNP aPl_Det (useKind kind) ;
  ConsArgKind kind kinds =
    mkNP and_Conj (mkNP aPl_Det (useKind kind)) kinds ;

  BaseHypo = {text = emptyText ; isEmpty = True} ;
  ConsHypo hypo hypos = {text = mkText hypo hypos.text ; isEmpty = False} ;
  
  BaseProp a b = mkListS (partProp a) (partProp b) ;
  ConsProp a bs = mkListS (partProp a) bs ;

  BaseProof = emptyText ;
  ConsProof proof proofs = mkText proof proofs ;

  BaseRule rule = prefixText item_Label (mkText rule) ;
  ConsRule rule rules = mkText (prefixText "\\item" (mkText rule)) rules ;

-- using Constants

  AdjProp adj exp = simpleProp (mkS (mkCl exp adj)) ;
  NotAdjProp adj exp = simpleProp (mkS negPol (mkCl exp adj)) ;
  RelAdj rel exp = Grammar.AdvAP rel.ap (Syntax.mkAdv rel.prep exp) ;
  ComparAdj compar exp = Grammar.AdvAP compar.rel.ap (Syntax.mkAdv compar.rel.prep exp) ;
  NounKind noun = {cn = noun ; adv = lin Adv {s = []}} ;
  SetKind set = {cn = set.cn ; adv = lin Adv {s = []}} ;
  NameExp name = name ;
  FunListExp f exps = mkNP the_Det (mkCN f.cn (Syntax.mkAdv f.prep exps.np)) ;
  LabelExp label = label.np ;
  ConstExp const = const.np ;
  OperListExp op exps = mkNP the_Det (mkCN op.f.cn (Syntax.mkAdv op.f.prep exps.np)) ;

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
  prefixText : Str -> Text -> Text = \s, t -> lin Text {s = s ++ t.s} ;

  labelText : LabelT -> Text -> Text = \label, text ->
    let period = if_then_Str label.isEmpty "" "." in
    lin Text {s = (mkUtt label.np).s ++ period ++ text.s} ;

  thenText : {text : Text ; isEmpty : Bool} -> S -> Text = \hypos, s ->
    case hypos.isEmpty of {
      True => mkText hypos.text (mkText (mkUtt s)) ;
      False => mkText hypos.text (mkText (mkUtt (mkS thenText_Adv s)))
	     | mkText hypos.text (mkText (mkUtt s))    ---- variants !!??
      } ;

  Proposition : Type = {s : S ; isComplex : Bool} ;

  simpleProp : S -> Proposition = \s -> {s = s ; isComplex = False} ;
  complexProp : S -> Proposition = \s -> {s = s ; isComplex = True} ;

  topProp : Proposition -> S = \prop -> prop.s ;
  partProp : Proposition -> S = \prop -> case prop.isComplex of {
    True => parenthS prop.s ;
    False => prop.s
    } ;

  notionNP : {np : NP ; isPl : Bool} -> {cn : CN ; adv : Adv} -> NP = \idents, kind ->
    let det = case idents.isPl of {
      True => aPl_Det ; 
      False => a_Det
      }
    in mkNP det (mkCN (mkCN kind.cn idents.np) kind.adv) ;

  definedCN : CN -> NP -> CN = \cn, np ->
    mkCN cn (Syntax.mkAdv defined_as_Prep np) ;

  useKind : {cn : CN ; adv : Adv} -> CN = \kind -> mkCN kind.cn kind.adv ;

  latexNP : Symb -> NP = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;
  latexS : Symb -> S = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  parenthS : S -> S = \s -> Markup.MarkupS (lin Mark {begin = "(" ; end = ")"}) s ;

  by_Prep : Prep = by8means_Prep ;

  ccAdv : Adv -> Adv -> Adv = \x, y -> lin Adv {s = x.s ++ y.s} ;

  item_Label : Str = "\\item" ;

-- non-functor
  negPol : Pol = negativePol ;

}