incomplete concrete CategoriesFunctor of Categories =
  IdentifiersLatex ** 
  open
    Utilities,
    Syntax,
    Prelude,
    Markup,
    Symbolic

in {

lincat
-- syntax
  Exp = NP ;
  Kind = {cn : CN ; adv : Syntax.Adv} ;
  Prop = Proposition ;
  Jmt = Text ;
  Exps = {np : NP ; isPl : Bool} ;
  [Exp] = Syntax.ListNP ;
  [Prop] = ListS ;
  ArgKind = {cn : CN ; adv : Syntax.Adv ; isPl : Bool} ;  -- isPl = idents.isPl
  [ArgKind] = {sg, neg, pl : ArgKindNP} ;  -- there exists an A / there exists no A / for all As
  Hypo = Utt ;
  [Hypo] = {text : Text ; isEmpty : Bool} ;
  Local = {name, value : NP} ;
  Proof = Text ;
  [Proof] = Text ;
  ProofExp = NP ;
  Rule = Utt ;
  [Rule] = Text ;
  Coercion = {from, to : CN} ;  -- the <from> <Exp> as <to>
  [Ident] = {np : NP ; isPl : Bool} ;
  Unit = Text ;
  Clause = Utt ;
  [Clause] = Text ;

-- lexicon, verbal
  Noun = CN ;
  Fam = FamT ;
  Fam2 = Fam2T ;
  Noun1 = CN ;
  Adj = AdjT ;
  Adj2 = Adj2T ;
  AdjE = AdjT ;
  AdjC = AdjT ;
  Adj3 = Adj3T ;
  Verb = VerbT ;
  Verb2 = Verb2T ;
  VerbC = VerbT ;
  Noun2 = {cn : CN ; prep : Syntax.Prep} ;
  Noun3 = {cn : CN ; prep1 : Syntax.Prep ; prep2 : Syntax.Prep} ;
  NounC = CN ;
  Name = NP ;
  Fun = FunT ;
  Fun2 = Fun2T ;
  FunC = FunT ;
  Label = LabelT ;
  Dep = {cn : CN ; prep : Syntax.Prep} ;
  Dep2 =  {cn : CN ; prep1, prep2 : Syntax.Prep} ;
  DepC =  {cn : CN ; prep : Syntax.Prep} ;

  Adv = Syntax.Adv ;
  Adv2 = Syntax.Prep ;
  AdvC = Syntax.Adv ;
  
  Binder = CN ;
  Binder1 = CN ;
  Binder2 = CN ;

  Prep = Syntax.Prep ;
  ProperName = PN ;
  Adverb = Syntax.Adv ;
  
linref
  Kind = \kind -> (mkUtt (useKind kind)).s ;
  Verb = \verb -> (mkUtt verb).s ;
  
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

  BaseExp a b = mkListNP a b ;
  ConsExp a bs = mkListNP a bs ;

  BaseArgKind kind = {
    sg = case kind.isPl of {
      True => toArgKindNP (mkNP aPl_Det (useKind kind)) ;
      False => toArgKindNP (mkNP aSg_Det (useKind kind))
      } ;
    neg = case kind.isPl of {
      True => toArgKindNP (mkNP (mkDet no_Quant pluralNum) (useKind kind)) ;
      False => toArgKindNP (mkNP no_Quant (useKind kind))
      } ;
    pl = toArgKindNP (mkNP aPl_Det (useKind kind))
    } ;
  ConsArgKind kind kinds = {
    sg = case kind.isPl of {
      True => toArgKindNP (mkNP and_Conj (mkNP aPl_Det (useKind kind)) (fromArgKindNP kinds.sg)) ;
      False => toArgKindNP (mkNP and_Conj (mkNP aSg_Det (useKind kind)) (fromArgKindNP kinds.sg))
      } ;
    neg = case kind.isPl of {
      True => toArgKindNP (mkNP and_Conj (mkNP (mkDet no_Quant pluralNum) (useKind kind)) (fromArgKindNP kinds.neg)) ;
      False => toArgKindNP (mkNP and_Conj (mkNP no_Quant (useKind kind)) (fromArgKindNP kinds.neg))
      } ;
    pl = toArgKindNP (mkNP and_Conj (mkNP aPl_Det (useKind kind)) (fromArgKindNP kinds.pl)) 
    } ;
    
  BaseHypo = {text = emptyText ; isEmpty = True} ;
  ConsHypo hypo hypos = {text = mkText hypo hypos.text ; isEmpty = False} ;
  
  BaseProp a b = mkListS (partProp a) (partProp b) ;
  ConsProp a bs = mkListS (partProp a) bs ;

  BaseProof = emptyText ;
  ConsProof proof proofs = mkText proof proofs ;

  BaseRule rule = prefixText item_str (mkText rule) ;
  ConsRule rule rules = mkText (prefixText item_str (mkText rule)) rules ;
  
  BaseClause clause = prefixText item_str (mkText clause) ;
  ConsClause clause clauses = mkText (prefixText item_str (mkText clause)) clauses ;


}
