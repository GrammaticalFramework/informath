-- based on http://nevidal.org/download/forthel.pdf

incomplete concrete ForthelFunctor of Forthel =
  LatexTerms, Constants **

open
  Syntax,
  Symbolic,
  Extend,
  Grammar,
---  Markup,
  Prelude

in {

lincat
  Toplevel = Text ;
  Section = Text ;
  Header = Text ;
  Assumption = Phr ;
  Assumptions = Text ;
  Synonym = Text ;
  Definition = S ;
  
  Statement = S ;
  Constant = NP ;

  Predicates = Extend.VPS ;
  Predicate = VP ;
  Adjective = AP ;  --- these two are one cat in 1.3.1 but separated in 1.3.4
  Verb = VP ;

  Terms = NP ;
  Term = NP ;
  Notions = NP ;

  Notion = {cn : CN ; isPlur : Bool} ;
  ClassNoun = {cn : CN ; adv : Adv} ;
  PrimClass = {cn : CN ; adv : Adv} ; -- element (x) , of X
  DefiniteNoun = CN ;

  SymbTerm = Symb ;
  VarName = Symb ;
  [VarName] = {s : Str ; isPlur : Bool} ;

lin
-- importing from ForthelTerms

  IndexedTerm n = mkSymb (mathEnvStr (macroApp "INDEXEDTERM" (n.s))) ;

  LatexFormulaSymbTerm formula = mkSymb (mathEnvStr formula.s) ;
  LatexExpSymbTerm exp = mkSymb (mathEnvStr exp.s) ;
  LatexVarName v = mkSymb (mathEnvStr v) ;


-- 1.3.2
  PrimClassNoun pc = pc ;
  
  ClassNounNotion pc = {
    cn = mkCN pc.cn pc.adv ;
    isPlur = False
    } ;
    
  ClassNounNamesNotion pc xs = {
    cn = mkCN (mkCN pc.cn (<symb (mkSymb xs.s) : NP>)) pc.adv ;
    isPlur = xs.isPlur
    } ;
    
---  NameSymbTerm n = n ;

----  StringName s = mkSymb s.s ; ---- replaced by VariableName
  
  BaseVarName x = {s = x.s ; isPlur = False} ;
  ConsVarName x xs = {s = x.s ++ "," ++ xs.s ; isPlur = True} ;

  AdjClassNoun noun adjective =
    noun ** {cn = mkCN adjective noun.cn} ;
  RelClassNoun noun predicates =
    noun ** {cn = Extend.SubjunctRelCN noun.cn (Extend.RelVPS which_RP predicates)} ; --- place of rel?
  StatClassNoun noun statement =
    noun ** {adv = concatAdv noun.adv (Syntax.mkAdv such_that_Subj statement)} ;

-- 1.3.3
  EveryTerm notion = mkNP every_Det notion.cn ;  --- overgenerates "every set A, B"
  
  EachTerm notion = mkNP each_Det notion.cn ;  --- overgenerates "each set A, B"
  
  AllTerm notion = mkNP all_Predet (mkNP aPl_Det notion.cn) ;

  SomeTerm notion = case notion.isPlur of {
    True => mkNP somePl_Det notion.cn ;
    False => mkNP someSg_Det notion.cn
    } ;

  AnyTerm notion = case notion.isPlur of {
    True => mkNP any_Quant pluralNum notion.cn ;
    False => mkNP any_Quant notion.cn
    } ;
    
  NoTerm notion = case notion.isPlur of {
    True => mkNP no_Quant pluralNum notion.cn ;
    False => mkNP no_Quant notion.cn
    } ;

  SymbolicTerm t = symb t ;
  
  DefiniteSgNounTerm n = mkNP the_Det n ;
  DefinitePlNounTerm n = mkNP thePl_Det n ;

  DoesPredicate verb = verb ;
  IsPredicate adjective = mkVP adjective ;
  IsaPredicate notion = mkVP notion.cn ;
  HasPredicate notion = mkVP have_V2 (mkNP a_Det notion.cn) ;
  HasNoPredicate notion = mkVP have_V2 (mkNP no_Quant notion.cn) ;

-- 1.3.5
  SimpleStatement terms predicates = Extend.PredVPS terms predicates ;
  WeHaveSymbStatement sym = mkS (mkCl we_NP have_V2 <symb sym : NP>) ;
  WeHaveConstStatement const = mkS (mkCl we_NP have_V2 const) ;
  LatexFormulaStatement formula = <symb (mkSymb (mathEnvStr formula.s)) : S> ;

  ThereIsStatement notions = mkS (Extend.ExistsNP notions) ;
  ThereIsNoStatement notion = mkS (Extend.ExistsNP (mkNP no_Quant notion.cn)) ;

  PosOnePredicates pred =
    Extend.MkVPS (mkTemp presentTense simultaneousAnt) positivePol pred ;
  NegOnePredicates pred = 
    Extend.MkVPS (mkTemp presentTense simultaneousAnt) negPol pred ;
  PosAddPredicates pred preds =
    Extend.ConjVPS and_Conj
      (Extend.BaseVPS
        (Extend.MkVPS (mkTemp presentTense simultaneousAnt) positivePol pred) preds) ;
  NegAddPredicates pred preds =
    Extend.ConjVPS and_Conj
      (Extend.BaseVPS
        (Extend.MkVPS (mkTemp presentTense simultaneousAnt) negPol pred) preds) ;

  OneTerms term = term ;
  AddTerms term terms = mkNP and_Conj term terms ; --- commas in spec

  OneNotions notion =
    mkNP a_Det notion.cn ;
  AddNotions notion notions =
    mkNP and_Conj (mkNP a_Det notion.cn) notions ; --- commas in spec

---- symbolic statements TODO

  ForStatement qns s = Grammar.ExtAdvS (Syntax.mkAdv for_Prep qns) s ;
  PostForStatement qns s = postAdvS s (Syntax.mkAdv for_Prep qns) ;

--- simplicied from spec, which uses many levels
--- to resolve ambiguities: that can be misleading to an innocent
--- reader (and the use of primaryStatement seems to compromise this)

  AndStatement s t = mkS and_Conj s t ;
  OrStatement s t = mkS or_Conj s t ;
  IfStatement s t = Grammar.ExtAdvS (mkAdv if_Subj s) (mkS then_Adv t) ;
  IffStatement s t = mkS iff_Conj s t ;

-- 1.3.6

  NotionDefinition definiendum definiens =
    mkS (mkCl (mkNP a_Det definiendum.cn) definiens.cn) ;
  FunctionDefinition definiendum definiens =
    mkS (mkCl (mkNP the_Det definiendum) definiens) ;
  FunctionIsEqualDefinition definiendum definiens =
    mkS (mkCl (mkNP the_Det definiendum) (mkAP equal_A2 definiens)) ;
  PredicateDefinition pred names statement =
    mkS iff_Conj (mkS (mkCl (namesNP names) pred)) statement ;  --><


  NotionSynonym head target =
    letSynonym (mkNP a_Det head.cn) (mkNP a_Det target.cn) ;
  FunctionSynonym head target =
    letSynonym (symb head) target ;
  PredicateSynonym pred names target = 
    letSynonym (s2np (mkS (mkCl (namesNP names) pred))) (s2np target) ; --><
  

-- 1.5.1
  NamesAssumption names classnoun =
    mkPhr
      (mkUtt (mkImp (Extend.ComplBareVS assume_VS
         (mkS (mkCl (namesNP names) (mkCN classnoun.cn classnoun.adv)))))) ;
      
  LetNamesAssumption names classnoun =
    mkPhr
      (mkUtt (ImpP3 (namesNP names) (mkVP (mkCN classnoun.cn classnoun.adv)))) ;

  StatementAssumption stat =
    mkPhr
      (mkUtt (mkImp (Extend.ComplBareVS assume_VS stat))) ;

  LatexFormulaAssumption formula =
    lin Phr {s = let_Str ++ mathEnvStr (formula.s)} ;

  OneAssumptions assumption = mkText assumption fullStopPunct ;
  AddAssumptions assumption assumptions = mkText (lin Text {s = assumption.s}) (mkText (lin Text {s = "and"}) assumptions) ;

  SectionToplevel header section = mkText header section ;

  EmptySection = emptyText ;
  AssumptionsSection assumptions section = mkText assumptions section ;
  StatementSection statement section = mkText (mkUtt statement) section ;
  ThenStatementSection statement section = mkText (mkUtt (mkS therefore_Adv statement)) section ;
  DefinitionSection definition section = mkText (mkUtt definition) section ;

  noHeader = emptyText ;
  stringHeader s = lin Text {s = s.s} ;

-- using Constants

  AdjAdjective adj = adj ;
  NameTerm name = name ;
  FunAppTerm f terms = mkNP the_Det (mkCN f.cn (Syntax.mkAdv f.prep terms)) ;

  NounClassNoun noun = {cn = noun ; adv = lin Adv {s = []}} ;
  LabelHeader label = mkText (mkUtt label) ;


-- Kohlhase

lincat
  TermSymb = {np : NP ; sym : Symb} ;

lin
  MkTermSymb np sym = {np = np ; sym = sym} ;
  ApposTermSymb primc name = {
    np = mkNP (mkCN (mkCN primc.cn (symb name)) primc.adv) ;
    sym = name
    } ;

oper
  possessAdv : NP -> Adv = \np -> mkAdv possess_Prep np ;

--------------------------

oper
--- these work for many languages but can be overridden
  emptyAdv : Adv = lin Adv {s = ""} ;
  concatAdv : Adv -> Adv -> Adv = \a, b -> lin Adv {s = a.s ++ b.s} ;

  postAdvS : S -> Adv -> S = \s, adv -> lin S {s = s.s ++ adv.s} ;

  negPol = negativePol ;

  namesNP : [VarName] -> NP = \xs -> case xs.isPlur of {
    True => pluralNP (symb xs.s) ;
    False => symb xs.s
    } ;

  pluralNP : NP -> NP = \np -> np ;  --- to be overridden for languages

  letSynonym : NP -> NP -> Text = \dum, dens ->
    mkText (Grammar.ImpP3 dum (mkVP denote_V2 dens)) ;

  s2np : S -> NP = \s -> symb (mkSymb (mkUtt s).s) ; --- hack; Forthel is not quite grammatical here

---  parenthS : S -> S = \s -> Markup.MarkupS (lin Mark {begin = "(" ; end = ")"}) s ;

  mkPrimClass = overload {
    mkPrimClass : N -> PrimClass
      = \n -> lin PrimClass {cn = mkCN n ; adv = emptyAdv} ;
    mkPrimClass : CN -> PrimClass
      = \n -> lin PrimClass {cn = n ; adv = emptyAdv}
    } ;

  therefore_Adv : Adv = then_Adv ;

}
