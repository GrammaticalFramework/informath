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
  Kind = {cn : CN ; adv : Adv} ;
  Prop = Proposition ;
  Jmt = Text ;
  Exps = {np : NP ; isPl : Bool} ;
  [Exp] = Syntax.ListNP ;
  [Prop] = ListS ;
  ArgKind = {cn : CN ; adv : Adv ; isPl : Bool} ;  -- isPl = idents.isPl
  [ArgKind] = {sg, neg, pl : NP} ;  -- there exists an A / there exists no A / for all As
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
  Adj3 = Adj3T ;
  Unit = Text ;

-- lexicon, verbal
  Noun = CN ;
  Fam = FamT ;
  Fam2 = Fam2T ;
  Noun1 = CN ;
  Adj = AdjT ;
  Adj2 = Adj2T ;
  AdjE = AdjT ;
  AdjC = AdjT ;
  Verb = VerbT ;
  Verb2 = Verb2T ;
  Noun2 = {cn : CN ; prep : Prep} ;
  Name = NP ;
  Fun = FunT ;
  Fun2 = Fun2T ;
  FunC = FunT ;
  Label = LabelT ;

  Prep = Syntax.Prep ;
  
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

  BaseRule rule = prefixText item_str (mkText rule) ;
  ConsRule rule rules = mkText (prefixText item_str (mkText rule)) rules ;


}
