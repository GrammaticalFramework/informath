-- simplified and extended from http://nevidal.org/download/forthel.pdf

abstract Forthel = LatexTerms, Constants ** {

flags startcat = Toplevel ;

cat
  Toplevel ;   -- Text, one or more sections with headers
  Section ;    -- Text, one or more assumptions, definition, statements
  Header ;     -- Text, header of Section
  Assumption ; -- Text, declaration of variables or assumption of a statement
  Assumptions ;-- Text, assumptions joined with and
  Synonym ;    -- Text, syntax definition of new Notion, Function
  Definition ; -- S,    meaning definition of DefiniteNoun, Notion, PredicateHead
  
  Statement ;  -- S,    logical proposition, either verbal or symbolic
  Constant ;   -- NP,   constant statement such as "contradiction"

  Predicates ; -- VPS,  positive or negated Predicate, or an and-conjunction of them
  Predicate ;  -- VP,   one-place predicate, with verb, adjective, or noun (is a)
  Adjective ;  -- AP,   atomic adjectival phrase
  Verb ;       -- VP,   atomic verb phrase

  Terms ;      -- NP,   either one Term or an and-conjunction
  Term ;       -- NP,   noun phrase, either quantified or definite or symbolic
  Notions ;    -- NP,   Notion with indefinite article, one or many with and 

  Notion ;       -- (CN, Num),  ClassNoun possibly with a list of names
  ClassNoun ;    -- (CN, Adv),  type expression, possibly "of" stg (e.g. element of A)
  PrimClass ;    -- (CN, Adv),  basic type expression
  DefiniteNoun ; -- CN,         individual-valued function name, e.g. order

  SymbTerm ;     -- Symb,  either Exp or Formula
  VarName ;      -- Symb,  variable
  [VarName] {1} ;-- (Str, Num)  list of variables

fun
-- importing from LatexTerms

  IndexedTerm : Int -> SymbTerm ; -- \INDEXEDTERM{ 8 }  to use in preprocessing
  
  LatexFormulaSymbTerm : Formula -> SymbTerm ;
  LatexExpSymbTerm : Exp -> SymbTerm ; 
  LatexVarName : Var -> VarName ;


-- 1.3.2
  PrimClassNoun : PrimClass -> ClassNoun ;
  ClassNounNotion : ClassNoun -> Notion ;
  ClassNounNamesNotion : ClassNoun -> [VarName] -> Notion ;

  AdjClassNoun : ClassNoun -> Adjective -> ClassNoun ; -- both left and right
  RelClassNoun : ClassNoun -> Predicates -> ClassNoun ; -- some of right
  StatClassNoun : ClassNoun -> Statement -> ClassNoun ;

-- 1.3.3
  EveryTerm : Notion -> Term ;  --- can only take Sg --><
  EachTerm  : Notion -> Term ;  --><

  AllTerm   : Notion -> Term ;  -- can take both Sg and Pl   --><
  AnyTerm   : Notion -> Term ;    --><
  SomeTerm  : Notion -> Term ;    --><
  NoTerm    : Notion -> Term ;    --><

  SymbolicTerm : SymbTerm -> Term ; --><

  DefiniteSgNounTerm : DefiniteNoun -> Term ;  --><
  DefinitePlNounTerm : DefiniteNoun -> Term ;  --><

-- 1.3.4
-- reading nonterminals doesPredicate etc as funs, not cats

  DoesPredicate : Verb -> Predicate ;
  IsPredicate : Adjective -> Predicate ;
  IsaPredicate : Notion -> Predicate ; --- not notion in doc 
  HasPredicate : Notion -> Predicate ; --- hence we may overgenerate
  HasNoPredicate : Notion -> Predicate ;


-- 1.3.5
--- intervening categories as functions again

  SimpleStatement : Terms -> Predicates -> Statement ;

  ThereIsStatement : Notions -> Statement ;
  ThereIsNoStatement : Notion -> Statement ;
  WeHaveSymbStatement : SymbTerm -> Statement ;
  WeHaveConstStatement : Constant -> Statement ;
  LatexFormulaStatement : Formula -> Statement ;

  PosOnePredicates : Predicate -> Predicates ;
  NegOnePredicates : Predicate -> Predicates ;
  PosAddPredicates : Predicate -> Predicates -> Predicates ;
  NegAddPredicates : Predicate -> Predicates -> Predicates ;

  OneTerms : Term -> Terms ;
  AddTerms : Term -> Terms -> Terms ;

  OneNotions : Notion -> Notions ;
  AddNotions : Notion -> Notions -> Notions ;

---- symbolic statements TODO

  ForStatement : Terms -> Statement -> Statement ; -->< terms before
  PostForStatement : Terms -> Statement -> Statement ; -- terms after

--- simplicied from spec, which uses many levels
--- to resolve ambiguities: that can be misleading to an innocent
--- reader (and the use of primaryStatement seems to compromise this)

  AndStatement : Statement -> Statement -> Statement ;
  OrStatement : Statement -> Statement -> Statement ;
  IfStatement : Statement -> Statement -> Statement ;
  IffStatement : Statement -> Statement -> Statement ;

-- 1.3.6

  NotionDefinition :  Notion -> Notion -> Definition ;
  FunctionDefinition : DefiniteNoun -> Term -> Definition ; --><
  FunctionIsEqualDefinition : DefiniteNoun -> Term -> Definition ;
  PredicateDefinition : Predicate -> [VarName] ->  Statement -> Definition ;  --><


--- spec p. 15: "Note that synonym declarations are not statements but instructions to the
--- parser, just like token groups (see Section 1.3.1). Roughly, synonyms relate
--- to defined symbols as preprocessor macros relate to subroutines
--- in a programming language."

  NotionSynonym : Notion -> PrimClass -> Synonym ;  --- in spec: notionPattern classNoun
  FunctionSynonym : SymbTerm -> Term -> Synonym ; -- in spec: functionPattern --><
  PredicateSynonym : Predicate -> [VarName] -> Statement -> Synonym ;  -->


-- 1.5.1
  NamesAssumption : [VarName] -> ClassNoun -> Assumption ;
  LetNamesAssumption : [VarName] -> ClassNoun -> Assumption ;
  StatementAssumption : Statement -> Assumption ;
  LatexFormulaAssumption : Formula -> Assumption ; 

  SectionToplevel : Header -> Section -> Toplevel ;

  OneAssumptions : Assumption -> Assumptions ;
  AddAssumptions : Assumption -> Assumptions -> Assumptions ;
  
  EmptySection : Section ;
  AssumptionsSection : Assumptions -> Section -> Section ;
  StatementSection : Statement -> Section -> Section ;
  ThenStatementSection : Statement -> Section -> Section ;
  DefinitionSection : Definition -> Section -> Section ;

  noHeader : Header ;
  stringHeader : String -> Header ;

  contradictionConstant : Constant ;

-- using Constants
  AdjAdjective : Adj -> Adjective ;
  NameTerm : Name -> Term ;
  FunAppTerm : Fun -> Terms -> Term ;
  
  NounClassNoun : Noun -> ClassNoun ;
  LabelHeader : Label -> Header ;

-- for Michael Kohlhase's example

cat
  TermSymb ;
  
fun  
  MkTermSymb : Term -> SymbTerm -> TermSymb ;
  ApposTermSymb : PrimClass -> VarName -> TermSymb ;


}
