incomplete concrete CategoriesFunctor of Categories =
  IdentifiersLatex ** 
  open
    Syntax,
    Prelude,
    Markup,
    Symbolic

in {

lincat
-- syntax
  Exp = NP ;
  Kind = {cn : CN ; adv : Adv} ;
  Prop = Proposition ;
  Jmt = Text ;
  Exps = {np : NP ; isPl : Bool} ;
  [Prop] = ListS ;
  ArgKind = {cn : CN ; adv : Adv ; isPl : Bool} ;  -- isPl = idents.isPl
  [ArgKind] = {sg, neg, pl : NP} ;  -- there exists an A / there exists no A / for all As
  Hypo = Utt ;
  [Hypo] = {text : Text ; isEmpty : Bool} ;
  Proof = Text ;
  [Proof] = Text ;
  ProofExp = NP ;
  Rule = Utt ;
  [Rule] = Text ;
  Coercion = {from, to : CN} ;  -- the <from> <Exp> as <to>
  [Ident] = {np : NP ; isPl : Bool} ;

-- lexicon, verbal
  Noun = CN ;
  Fam = FamilyT ; 
  Adj = AP ;
  Verb = VP ;
  Reladj = RelationT ;
  Relverb = V2 ;
  Relnoun = N2 ;
  Name = NP ;
  Fun = FunctionT ;
  Label = LabelT ;

-- lexicon, symbolic
  Set = SetT ;
  Const = ConstantT ;
  Oper = OperatorT ;
  Compar = ComparisonT ;

-- type synonyms
oper
  Proposition : Type = {s : S ; isComplex : Bool} ;
  
  RelationT : Type = {ap : AP ; prep : Prep} ;
  FunctionT : Type = {cn : CN ; prep : Prep} ;
  ConstantT : Type = {np : NP ; c : Str} ;
  OperatorT : Type = {f : FunctionT} ; 
  ComparisonT : Type = {rel : RelationT ; op :  Str} ;
  SetT : Type = {cn : CN ; c : Str} ;
  FamilyT : Type = {cn : CN ; prep1, prep2 : Prep ; isCollective : Bool} ;
  LabelT = {np : NP ; isEmpty : Bool} ;
  ComparnounT = {cn : CN ; prep : Prep ; op : Str} ;
  Pred3T = {ap : AP ; prep1, prep2 : Prep} ;

lin
-- Base and Const for lists
  BaseIdent ident =
    {np = latexNP (mkSymb ident) ; isPl = False} ;
  ConsIdent ident idents = {
    np = case idents.isPl of {
      False => mkNP and_Conj (latexNP (mkSymb ident)) idents.np ;
      True => mkNP commaConj (latexNP (mkSymb ident)) idents.np 
      } ;
    isPl = True
    } ;

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

-- utilities
oper
  latexNP : Symb -> NP = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  commaConj : Conj = mkConj "," ;

  useKind : {cn : CN ; adv : Adv} -> CN = \kind -> mkCN kind.cn kind.adv ;

  latexS : Symb -> S = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  simpleProp : S -> Proposition = \s -> {s = s ; isComplex = False} ;
  complexProp : S -> Proposition = \s -> {s = s ; isComplex = True} ;

  topProp : Proposition -> S = \prop -> prop.s ;
  partProp : Proposition -> S = \prop -> case prop.isComplex of {
    True => parenthS prop.s ;
    False => prop.s
    } ;
    
  parenthS : S -> S = \s -> Markup.MarkupS (lin Mark {begin = "(" ; end = ")"}) s ;

  prefixText : Str -> Text -> Text = \s, t -> lin Text {s = s ++ t.s} ;

  item_Label : Str = "\\item" ;


}
