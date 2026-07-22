incomplete concrete MizarFunctor of Mizar = Categories, TermsLatex
** 
open
  Syntax,
  Symbolic,
  Grammar,
  Extend,
  Utilities,
  Formal,
  Prelude
in {
  lincat
  Predicate = Str;
  DefCases = Text;
  DefCase = Utt;
  [DefCase] = Text;

  lin
  Noun3Prop noun x y z = simpleProp (mkS (mkCl x (Noun3ExpsNoun noun y z))) ;
  NotNoun3Prop noun x y z = simpleProp (mkS negPol (mkCl x (Noun3ExpsNoun noun y z))) ;

  PredicateKind argkinds = {
    cn = mkCN predicate_N ;
    adv = Syntax.mkAdv over_Prep argkinds.pl
    } ;

  BareSuchThatKind kind prop = {
    cn = kind.cn ;
    adv = ccAdv kind.adv (Syntax.mkAdv such_that_Subj (partProp prop))
    } ;
    
  DefSuchThatJmt label hypos exp kind cond =
    labelText label
      (thenText hypos 
        (mkS (mkCl exp (mkNP a_Det (useKind (BareSuchThatKind kind cond)))))) ;

  ApposTermExp noun term =
    mkNP (mkCN noun (latexNP (mkSymb term.s))) ;

  ApposIdentNoun noun ident = mkCN noun (latexNP (mkSymb ident)) ;

  VerbCCollProp verb exps = simpleProp (mkS (mkCl (mkNP and_Conj exps) verb)) ;

  AdjNoun2Noun2 adj noun = {cn = mkCN adj noun.cn ; prep= noun.prep} ;

  AdjNounCNounC adj noun = mkCN adj noun ;

  AdjNoun3Noun3 adj noun = {cn = mkCN adj noun.cn ; prep1 = noun.prep1 ; prep2 = noun.prep2} ;

  Noun2ExpNoun rel y = mkCN rel.cn (Syntax.mkAdv rel.prep y) ;

  Noun3ExpsNoun rel y z = mkCN (mkCN rel.cn (Syntax.mkAdv rel.prep1 y)) (Syntax.mkAdv rel.prep2 z) ;

  NounPrepsNoun3 noun prep1 prep2 = {cn = noun ; prep1 = prep1 ; prep2 = prep2} ;

  NounCCollProp noun exps = simpleProp (mkS (mkCl (mkNP and_Conj exps) noun)) ;

  FunIdentFun func ident = {cn = ApposIdentNoun func.cn ident ; prep = func.prep} ;

  DepNoun dep x = mkCN dep.cn (Syntax.mkAdv dep.prep x) ;
  Dep2Noun dep x y = mkCN dep.cn (ccAdv (Syntax.mkAdv dep.prep1 x) (Syntax.mkAdv dep.prep2 y)) ;
  DepCNoun dep x y = mkCN dep.cn (Syntax.mkAdv dep.prep (mkNP and_Conj x y)) ;

  BaseDefCase defCase = prefixText item_str (mkText defCase) ;
  ConsDefCase defCase defCases = mkText (prefixText item_str (mkText defCase)) defCase ;

  DefByCasesJmt label hypos byCases =
    labelText label
      (thenTextfromText hypos (prefixText by_cases_Str byCases)) ;
  PropsDefCase guard prop = mkUtt (Grammar.SSubjS (partProp prop) if_Subj (partProp guard)) ;
  NoOtherwiseDefCases listCases = embedText begin_itemize_str end_itemize_str listCases ;
  OtherwiseDefCases listCases otherwise = embedText begin_itemize_str end_itemize_str (mkText listCases (mkText (mkS otherwise_Adv (topProp otherwise)))) ;

  
  TupleTerm ts = constant ("\\langle" ++ ts.s ++ "\\rangle") ** {isNumber = False} ;
  IdentPredicate id = id ;
  AppPredicateFormula P xs = constant (P ++ "(" ++ xs.s ++ ")");
  FraenkelComprehensionTerm t argkinds compr =
    tconstant ("\\{" ++ top t ++ "\\text{" ++
    (mkUtt (Syntax.mkAdv for_Prep (Syntax.mkNP argkinds.pl (Syntax.mkAdv such_that_Subj (partProp compr))))).s
     ++ "}" ++ "\\}") ;
  FraenkelSimpleComprehensionTerm t argkinds =
    tconstant ("\\{" ++ top t ++ "\\text{" ++
    (mkUtt (Syntax.mkAdv for_Prep argkinds.pl)).s
     ++ "}" ++ "\\}") ;
  TextualTerm t = tconstant ("\\text{" ++ (mkUtt t).s ++ "}");
  
  notsubseteq_Compar = "\\nsubseteq";
  inverse_Oper = mkOper "" "^{ -1 }" <2 : Prec> ;
  apply_Oper2 = mkOper2 "" "(" ")" <3 : Prec> <4 : Prec> <2 : Prec> ;
  apply_inverse_Oper2 = mkOper2 "" "^{ -1 }(" ")" <3 : Prec> <4 : Prec> <2 : Prec> ;

  oper
    thenTextfromText : {text : Text ; isEmpty : Bool} -> Text -> Text = \hypos, t ->
      case hypos.isEmpty of {
        True => mkText hypos.text t ;
        False => mkText hypos.text (mkText (mkUtt thenText_Adv) t)
        | mkText hypos.text t    ---- variants !!??
        } ;
}