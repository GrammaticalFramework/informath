-- the categories of Informath as symbolic LaTeX: expressions, kinds and
-- propositions are formulas with precedences as in TermsLatex

concrete CategoriesLatex of Categories =
  IdentifiersLatex **
  open
    Formal,
    Prelude
  in {

lincat
-- syntax
  Exp = TermPrec ;
  Kind = TermPrec ;
  Prop = TermPrec ;
  Jmt = Str ;
  Exps = Str ;
  [Exp] = Str ;
  [Prop] = Str ;
  ArgKind = {q : Str ; d : TermPrec} ;  -- q = as a quantifier prefix, d = as a domain
  [ArgKind] = {q : Str ; d : Str} ;
  Hypo = Str ;
  [Hypo] = {s : Str ; isEmpty : Bool} ;
  Local = Str ;
  Proof = TermPrec ;
  [Proof] = {s : Str ; isEmpty : Bool} ;
  ProofExp = TermPrec ;
  Rule = Str ;
  [Rule] = Str ;
  Coercion = Str ;
  [Ident] = Str ;
  Unit = Str ;

-- lexicon, verbal: not used in symbolic LaTeX
  Noun, Fam, Fam2, Noun1, Noun2, Noun3, NounC = Str ;
  Adj, Adj2, Adj3, AdjE, AdjC = Str ;
  Verb, Verb2, VerbC = Str ;
  Name, Fun, Fun2, FunC = Str ;
  Dep, Dep2, DepC = Str ;
  Adv, Adv2, AdvC = Str ;
  Binder, Binder1, Binder2 = Str ;
  Adverb, Prep, ProperName = Str ;

  Label = {s : Str ; isIdent : Bool} ; -- isIdent: the judgement gives the keyword

lin
  BaseIdent x = x ;
  ConsIdent x xs = x ++ "," ++ xs ;

  BaseExp x y = top x ++ "," ++ top y ;
  ConsExp x xs = top x ++ "," ++ xs ;

  BaseProp a b = top a ++ "," ++ top b ;
  ConsProp a as = top a ++ "," ++ as ;

  BaseArgKind a = {q = a.q ; d = usePrec 1 a.d} ;
  ConsArgKind a as = {q = a.q ++ as.q ; d = usePrec 1 a.d ++ "\\rightarrow" ++ as.d} ;

  BaseHypo = {s = [] ; isEmpty = True} ;
  ConsHypo h hs = {
    s = h ++ case hs.isEmpty of {True => [] ; False => "," ++ hs.s} ;
    isEmpty = False
    } ;

  BaseProof = {s = [] ; isEmpty = True} ;
  ConsProof p ps = {
    s = top p ++ case ps.isEmpty of {True => [] ; False => "," ++ ps.s} ;
    isEmpty = False
    } ;

  BaseRule r = r ;
  ConsRule r rs = r ++ "\\qquad" ++ rs ;

}
