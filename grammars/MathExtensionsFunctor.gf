incomplete concrete MathExtensionsFunctor of MathExtensions =
  Categories,
  Terms
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
  [Adj] = Syntax.ListAP ;

lin
  TermKind term = {
    cn = mkCN element_N ;
    adv = Syntax.mkAdv possess_Prep (latexNP (mkSymb term.s))
    } ;

  FormulaProp formula = simpleProp (latexS (mkSymb formula.s)) ;
  
  DisplayFormulaProp formula = simpleProp (displayLatexS (mkSymb formula.s)) ;

  FormulaImpliesProp a b = simpleProp (mkS (mkCl (latexNP (mkSymb a.s)) imply_V2 (latexNP (mkSymb b.s)))) ;

  IdentsArgKind kind idents = {cn = mkCN kind.cn idents.np ; adv = kind.adv ; isPl = idents.isPl} ;
  VarsHypo idents kind = Grammar.ImpP3 idents.np (mkVP (useKind kind)) ; 
  BareVarsHypo idents = Grammar.ImpP3 idents.np (mkVP arbitrary_A) ;
  AbsExp idents exp =
    mkNP the_Det (mkCN function_N (mkRS (mkRCl which_RP map_V3 idents.np exp))) ;


  AndProp props = simpleProp (mkS and_Conj props) ;
  OrProp props = simpleProp (mkS or_Conj props) ;
  IfProp A B = simpleProp (Grammar.ExtAdvS (Syntax.mkAdv if_Subj (partProp A)) (mkS then_Adv (partProp B))) ;
  IffProp A B = simpleProp (Grammar.SSubjS (partProp A) iff_Subj (partProp B)) ;
  AllProp argkinds prop =
    simpleProp (Grammar.ExtAdvS (Syntax.mkAdv for_Prep (mkNP all_Predet argkinds.pl)) (partProp prop)) ;
  ExistProp argkinds prop =
    simpleProp (Grammar.SSubjS (mkS (Extend.ExistsNP argkinds.sg)) such_that_Subj (partProp prop)) ; 

  NotAdjProp adj exp = simpleProp (mkS negPol (mkCl exp adj)) ;
  NotAdj2Prop adj x y = simpleProp (mkS negPol (mkCl x (Grammar.AdvAP adj.ap (Syntax.mkAdv adj.prep y)))) ;
  NotAdjCProp adj exps = simpleProp (mkS negPol (mkCl exps.np adj)) ;
  NotAdjEProp adj exps = simpleProp (mkS negPol (mkCl exps.np adj)) ;
  NotNoun1Prop noun exp = simpleProp (mkS negPol (mkCl exp noun)) ; 
  NotNoun2Prop rel x y = simpleProp (mkS negPol (mkCl x (mkCN rel.cn (Syntax.mkAdv rel.prep y)))) ; 
  NotVerbProp verb exp = simpleProp (mkS negPol (mkCl exp verb)) ; 
  NotVerb2Prop verb x y = simpleProp (mkS negPol (mkCl x verb y)) ;

  AndAdj adjs = mkAP and_Conj adjs | mkAP both7and_DConj adjs ;
  OrAdj adjs = mkAP or_Conj adjs  | mkAP either7or_DConj adjs ;

  AndExp exps =  mkNP and_Conj exps | mkNP both7and_DConj exps ;
  OrExp exps = mkNP or_Conj exps | mkNP either7or_DConj exps ;

  BothAndProp x y = simpleProp (mkS both7and_DConj x.s y.s) ;
  EitherOrProp x y = simpleProp (mkS either7or_DConj x.s y.s) ;
  
  BothAndAdj x y = mkAP both7and_DConj x y ;
  EitherOrAdj x y = mkAP either7or_DConj x y ;
  
  BothAndExp x y = mkNP both7and_DConj x y ;
  EitherOrExp x y = mkNP either7or_DConj x y ;

  OnlyIfProp A B = simpleProp (Grammar.SSubjS (partProp A) only_if_Subj (partProp B)) ;

  ExistNoProp argkinds prop = simpleProp (Grammar.SSubjS (mkS (Extend.ExistsNP argkinds.neg)) such_that_Subj (partProp prop)) ; 

  Adj2Adj rel y = Grammar.AdvAP rel.ap (Syntax.mkAdv rel.prep y) ;
  Adj3Adj pred y z =
    AdvAP (AdvAP pred.ap (Syntax.mkAdv pred.prep1 y)) (Syntax.mkAdv pred.prep2 z) ;

  BaseAdj a b = mkListAP a b ;
  ConsAdj a bs = mkListAP a bs ;

  IndexedTermExp i = symb (mkSymb ("\\INDEXEDTERM{" ++ i.s ++ "}")) ;
  IndexedFormulaProp i = simpleProp (symb (mkSymb ("\\INDEXEDTERM{" ++ i.s ++ "}"))) ;
  IndexedLetFormulaHypo i = lin Utt {s = let_Str ! False ++ "\\INDEXEDTERM{" ++ i.s ++ "}"} ;

-- Pathak's and more

  LetFormulaHypo formula = lin Utt {s = let_Str ! False ++ "$" ++ top formula ++ "$"} ;

  PostQuantProp prop exp =
    simpleProp (postAdvS prop.s (Syntax.mkAdv for_Prep exp)) ; -- ambiguous: there is no complexProp in Informath

  LetDeclarationHypo decl = lin Utt {s = let_Str ! decl.isPl ++ "$" ++ decl.s ++ "$"} ;

  DefinedAdjJmt label hypos exp adj prop =
    labelText (label)
      (thenText hypos (
        mkS (mkCl exp (mkVP (mkVP (passiveVP define_V2)
          <lin Adv (mkUtt (mkVP adj)) : Adv>) (Syntax.mkAdv if_Subj prop.s))))) ; 
  WeDefineAdjJmt label hypos exp adj prop =
    labelText (label)
      (thenText hypos (
        mkS (mkCl we_NP (mkVP (mkVP (mkVP define_V2 exp)
          <lin Adv (mkUtt (mkVP adj)) : Adv>) (Syntax.mkAdv if_Subj prop.s))))) ; 

  AdjKind adj kind = kind ** {cn = mkCN adj kind.cn} ;
  KindProp exp kind = simpleProp (mkS (mkCl exp (useKind kind))) ;

  AllKindExp kind = mkNP all_Predet (mkNP aPl_Det (useKind kind)) ;
  AllIdentsKindExp idents kind = mkNP all_Predet (mkNP aPl_Det (mkCN (mkCN kind.cn idents.np) kind.adv)) ;

  EveryKindExp kind =
    mkNP every_Det (mkCN kind.cn kind.adv) | mkNP all_Predet (mkNP aPl_Det (mkCN kind.cn kind.adv)) ;
  EveryIdentKindExp ident kind = mkNP every_Det (mkCN (mkCN kind.cn (latexNP (mkSymb ident))) kind.adv) ;

  SomeKindExp kind = mkNP someSg_Det (mkCN kind.cn kind.adv) ;
  SomeIdentsKindExp idents kind = case idents.isPl of {
    False => mkNP someSg_Det (mkCN (mkCN kind.cn idents.np) kind.adv) ;
    True  => mkNP somePl_Det (mkCN (mkCN kind.cn idents.np) kind.adv)
    } ;
  IndefKindExp kind = mkNP a_Det (mkCN kind.cn kind.adv) ;
  IndefIdentKindExp ident kind = mkNP a_Det (mkCN (mkCN kind.cn (latexNP (mkSymb ident))) kind.adv) ;
  
  NoIdentsKindExp idents kind = case idents.isPl of {
    False => mkNP no_Quant (mkCN (mkCN kind.cn idents.np) kind.adv) ;
    True  => mkNP (mkDet no_Quant pluralNum) (mkCN (mkCN kind.cn idents.np) kind.adv)
    } ;
  NoKindExp kind = mkNP no_Quant (mkCN kind.cn kind.adv) ;

  AdjCCollProp adj exps = simpleProp (mkS (mkCl exps.np adj)) ;
  AdjECollProp adj exps = simpleProp (mkS (mkCl exps.np adj)) ;
  FunCCollExp f exps = mkNP the_Det (mkCN f.cn (Syntax.mkAdv f.prep exps.np)) ;
  
}