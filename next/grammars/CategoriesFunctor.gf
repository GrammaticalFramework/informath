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
  Adj3 = Pred3T ;

-- lexicon, verbal
  Noun = CN ;
  Fam = FamilyT ;
  Fam2 = FamilyT ;
  Noun1 = CN ;
  Adj = AP ;
  Adj2 = RelationT ;
  AdjC = AP ;
  Verb = VP ;
  Verb2 = V2 ;
  Noun2 = {cn : CN ; prep : Prep} ;
  Name = NP ;
  Fun = FunctionT ;
  Fun2 = Function2T ;
  FunC = FunctionT ;
  Label = LabelT ;

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


}
