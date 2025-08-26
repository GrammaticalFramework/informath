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
  Kind = {cn : CN ; adv : Adv} ;
  Prop = Proposition ;
  Jmt = Text ;
  Exps = {np : NP ; isPl : Bool} ;
  [Prop] = Grammar.ListS ;
  ArgKind = {cn : CN ; adv : Adv ; isPl : Bool} ;  -- isPl = idents.isPl
  [ArgKind] = {sg, neg, pl : NP} ;  -- there exists an A / there exists no A / for all As
  Hypo = Utt ;
  [Hypo] = {text : Text ; isEmpty : Bool} ;
  [Ident] = {np : NP ; isPl : Bool} ;
  Proof = Text ;
  [Proof] = Text ;
  ProofExp = NP ;
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
  LetHypo ident kind exp =
    let ikind = {cn = mkCN kind.cn (latexNP (mkSymb ident)) ; adv = kind.adv ; isPl = False}
    in Grammar.ImpP3 (mkNP the_Det (useKind ikind)) (mkVP exp) ; 
  BareLetHypo ident exp = Grammar.ImpP3 (latexNP (mkSymb ident)) (mkVP exp) ; 

  AppExp exp exps = mkNP exp (Syntax.mkAdv applied_to_Prep exps.np) ;
  AbsExp idents exp =
    mkNP the_Det (mkCN function_N (mkRS (mkRCl which_RP map_V3 idents.np exp))) ;
  KindExp kind = mkNP the_Det (mkCN type_CN (Syntax.mkAdv possess_Prep (mkNP aPl_Det (useKind kind)))) ;
  TermExp term = latexNP (mkSymb term.s) ;
  TypedExp exp kind = mkNP the_Det (mkCN (mkCN kind.cn exp) kind.adv) ;
  EnumSetExp exps = mkNP the_Det (mkCN set_N (Syntax.mkAdv possess_Prep exps.np)) ;

  AndProp props = complexProp (mkS and_Conj props) ;
  OrProp props = complexProp (mkS or_Conj props) ;
  IfProp A B = complexProp (Grammar.ExtAdvS (Syntax.mkAdv if_Subj (partProp A)) (mkS then_Adv (partProp B))) ;
  IffProp A B = complexProp (Grammar.SSubjS (partProp A) iff_Subj (partProp B)) ;
  NotProp prop =
    simpleProp (mkS negPol (mkCl 
          (mkVP (mkNP the_Quant (mkCN case_N (Syntax.mkAdv that_Subj (partProp prop))))))) ;
  AllProp argkinds prop =
    simpleProp (Grammar.ExtAdvS (Syntax.mkAdv for_Prep (mkNP all_Predet argkinds.pl)) (partProp prop)) ;
  ExistProp argkinds prop =
    simpleProp (Grammar.SSubjS (mkS (Extend.ExistsNP argkinds.sg)) such_that_Subj (partProp prop)) ; 
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
    adv = ccAdv (Syntax.mkAdv from_Prep argkinds.sg) (Syntax.mkAdv to_Prep (mkNP aPl_Det (useKind kind)))
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

  BaseIdent ident =
    {np = latexNP (mkSymb ident) ; isPl = False} ;
  ConsIdent ident idents = {
    np = case idents.isPl of {
      False => mkNP and_Conj (latexNP (mkSymb ident)) idents.np ;
      True => mkNP commaConj (latexNP (mkSymb ident)) idents.np 
      } ;
    isPl = True
    } ;

  OneExps exp =
    {np = exp ; isPl = False} ;
  AddExps exp exps =
    {np = mkNP and_Conj exp exps.np ; isPl = True} ;

  BaseArgKind kind = {
    sg = case kind.isPl of {
      True => mkNP aPl_Det (useKind kind) ;
      False => mkNP aSg_Det (useKind kind)
      } ;
    neg = case kind.isPl of {
      True => mkNP (mkDet no_Quant pluralNum) (useKind kind) ;
      False => mkNP no_Quant (useKind kind)
      } ;
    pl = mkNP aPl_Det (useKind kind)
    } ;
  ConsArgKind kind kinds = {
    sg = case kind.isPl of {
      True => mkNP and_Conj (mkNP aPl_Det (useKind kind)) kinds.sg ;
      False => mkNP and_Conj (mkNP aSg_Det (useKind kind)) kinds.sg
      } ;
    neg = case kind.isPl of {
      True => mkNP and_Conj (mkNP (mkDet no_Quant pluralNum) (useKind kind)) kinds.neg ;
      False => mkNP and_Conj (mkNP no_Quant (useKind kind)) kinds.neg
      } ;
    pl = mkNP and_Conj (mkNP aPl_Det (useKind kind)) kinds.pl 
    } ;
    
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
  ReladjProp rel x y = simpleProp (mkS (mkCl x (Grammar.AdvAP rel.ap (Syntax.mkAdv rel.prep y)))) ;
  ComparAdj compar exp = Grammar.AdvAP compar.rel.ap (Syntax.mkAdv compar.rel.prep exp) ;
  NounKind noun = {cn = noun ; adv = lin Adv {s = []}} ;
  SetKind set = {cn = set.cn ; adv = lin Adv {s = []}} ;
  NameExp name = name ;
  FunListExp f exps = mkNP the_Det (mkCN f.cn (Syntax.mkAdv f.prep exps.np)) ;
  LabelProofExp label = label.np ;
  ConstExp const = const.np ;
  OperListExp op exps = mkNP the_Det (mkCN op.f.cn (Syntax.mkAdv op.f.prep exps.np)) ;
  FamKind fam kind = {cn = fam.cn ; adv = Syntax.mkAdv fam.prep1 (mkNP aPl_Det (useKind kind))} ;
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
  VerbProp verb exp = simpleProp (mkS (mkCl exp verb)) ; 
  RelverbProp verb x y = simpleProp (mkS (mkCl x verb y)) ; 
  RelnounProp rel x y = simpleProp (mkS (mkCl x (mkVP (mkCN rel y)))) ; 
  NotVerbProp verb exp = simpleProp (mkS negPol (mkCl exp verb)) ; 
  NotRelverbProp verb x y = simpleProp (mkS negPol (mkCl x verb y)) ; 
  NotRelnounProp rel x y = simpleProp (mkS negPol (mkCl x (mkVP (mkCN rel y)))) ; 
  ComparnounProp rel x y = simpleProp (mkS (mkCl x (mkVP ((mkCN rel.cn (Syntax.mkAdv rel.prep y)))))) ;
  Pred3Prop pred x y z =
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
    
  definedAdv : NP -> Adv = \df ->
    Syntax.mkAdv defined_as_Prep df ;

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