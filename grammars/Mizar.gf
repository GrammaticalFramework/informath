abstract Mizar = Categories, Terms
** {
  cat
    Predicate;
    DefCases;
    DefCase;
    [DefCase] {1};

  fun
    -- non_Adverb : Adverb ;
    Noun3Prop : Noun3 -> Exp -> Exp -> Exp -> Prop ;
    NotNoun3Prop : Noun3 -> Exp -> Exp -> Exp -> Prop ;

    PredicateKind : [ArgKind] -> Kind ; -- hmm peut-être un peu trop large...
    BareSuchThatKind : Kind -> Prop -> Kind ;
    DefSuchThatJmt : Label -> [Hypo] -> Exp -> Kind -> Prop -> Jmt ;
    ApposTermExp : Noun -> Term -> Exp ;
    ApposIdentNoun : Noun -> Ident -> Noun ;
    VerbCCollProp : VerbC -> [Exp] -> Prop;
    AdjNoun2Noun2 : Adj -> Noun2 -> Noun2 ;
    AdjNoun3Noun3 : Adj -> Noun3 -> Noun3 ;
    AdjNounCNounC : Adj -> NounC -> NounC ;
    Noun2ExpNoun : Noun2 -> Exp -> Noun ;
    Noun3ExpsNoun : Noun3 -> Exp -> Exp -> Noun ;
    NounPrepsNoun3 : Noun -> Prep -> Prep -> Noun3 ;
    NounCCollProp : NounC -> [Exp] -> Prop ;
    FunIdentFun : Fun -> Ident -> Fun ;
    DepNoun : Dep -> Exp -> Noun ;
    Dep2Noun : Dep2 -> Exp -> Exp -> Noun ;
    DepCNoun : DepC -> Exp -> Exp -> Noun ;

    DefByCasesJmt : Label -> [Hypo] -> DefCases -> Jmt ;
    PropsDefCase : Prop -> Prop -> DefCase ;
    NoOtherwiseDefCases : [DefCase] -> DefCases ;
    OtherwiseDefCases : [DefCase] -> Prop -> DefCases ;
    otherwise_Adv : Adv ;

    TupleTerm : [Term] -> Term ;
    IdentPredicate : Ident -> Predicate;
    AppPredicateFormula : Predicate -> [Term] -> Formula;
    FraenkelComprehensionTerm : Term -> [ArgKind] -> Prop -> Term ;
    FraenkelSimpleComprehensionTerm : Term -> [ArgKind] -> Term ;
    TextualTerm : Exp -> Term ;

    notsubseteq_Compar : Compar;
    inverse_Oper : Oper ;
    apply_Oper2 : Oper2 ;
    apply_inverse_Oper2 : Oper2 ;
    placeholderAdj_Adj2 : Adj2 ;

}