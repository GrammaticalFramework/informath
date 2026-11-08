# A BNF grammar of Informath's English

Made by `scripts/InformathBNF.hs` from `share/InformathEng.pgf` (do not edit by hand:
run `stack runghc scripts/InformathBNF.hs > doc/informath-bnf.md`).

This is a simplified grammar. Each rule is made from the English linearization of
one function of Informath's abstract syntax (named in the comment after it), with
its arguments shown by their categories. Each rule shows one form, with the default
values of the parameters (number, case, mood, ...); an argument that has several
parts in different places (a discontinuous constituent) is shown where it first
occurs, and an argument that may be empty may be missing. `[C]` is a list of `C`;
in a conjunction (`[Exp] "and" Exp`), its last element comes after the
conjunction word. `(...)?` is optional, `(...)*` repeated, and `""` empty.
Strings in quotes are words or symbols; `Ident`, `Int` and `String` are
identifiers and literals. The categories are given top-down, from the judgements
(`Jmt`) to the words; for a lexical category, a few of its words are shown.

Categories: [Jmt](#jmt), [Label](#label), [[Hypo]](#hypo-list-of-hypo), [Exp](#exp), [Kind](#kind), [Prop](#prop), [[Exp]](#exp-list-of-exp), [DefVerb](#defverb), [AdjC](#adjc), [Adj](#adj), [DefCases](#defcases), [Example](#example), [Ident](#ident), [[ArgKind]](#argkind-list-of-argkind), [[Rule]](#rule-list-of-rule), [[Clause]](#clause-list-of-clause), [Proof](#proof), [Unit](#unit), [Noun](#noun), [Int](#int), [ProperName](#propername), [Hypo](#hypo), [[Ident]](#ident-list-of-ident), [Exps](#exps), [Term](#term), [Binder1](#binder1), [Binder2](#binder2), [Binder](#binder), [Fun2](#fun2), [Fun3](#fun3), [FunC](#func), [Fun](#fun), [Name](#name), [Quant](#quant), [ArgKind](#argkind), [Dep2](#dep2), [DepC](#depc), [Dep](#dep), [Fam2](#fam2), [Fam](#fam), [Adj2](#adj2), [PropEnum](#propenum), [Pred](#pred), [Adj3](#adj3), [AdjC3](#adjc3), [AdjE](#adje), [Adv2](#adv2), [AdvC](#advc), [Adv](#adv), [[Prop]](#prop-list-of-prop), [Formula](#formula), [NumDet](#numdet), [Condition](#condition), [NegNoun2](#negnoun2), [Noun1](#noun1), [Noun2](#noun2), [Noun3](#noun3), [NounC](#nounc), [Verb2](#verb2), [VerbC](#verbc), [Verb](#verb), [Prep](#prep), [Adverb](#adverb), [[Adj]](#adj-list-of-adj), [[DefCase]](#defcase-list-of-defcase), [Argument](#argument), [BoundVariable](#boundvariable), [KindArgument](#kindargument), [String](#string), [Rule](#rule), [Clause](#clause), [[Proof]](#proof-list-of-proof), [ProofExp](#proofexp), [Environment](#environment), [Method](#method), [Hence](#hence), [CaseNumber](#casenumber), [Title](#title), [Declaration](#declaration), [Local](#local), [Macro](#macro), [Function](#function), [[Term]](#term-list-of-term), [Const](#const), [Oper2](#oper2), [Oper](#oper), [Predicate](#predicate), [Equation](#equation), [Card](#card), [DefCase](#defcase), [Compar](#compar)

## Jmt

```
Jmt ::= Label "." ([Hypo] "then")? Exp "is" "a" Kind "."   -- AxiomExpJmt
      | Label "." ([Hypo] "then")? Prop "."   -- AxiomJmt
      | Label "." ([Hypo] "then")? "the" Kind "form" "a" Kind "."   -- AxiomKindJmt
      | Label "." ([Hypo] "then")? "we" "can" "say" "that" Prop "."   -- AxiomPropJmt
      | Label "." ([Hypo] "then")? [Exp] "and" Exp "are" DefVerb "to" "be" AdjC "if" Prop "."   -- DefAdjCJmt
      | Label "." ([Hypo] "then")? Exp "is" DefVerb "to" "be" Adj "if" Prop "."   -- DefAdjJmt
      | Label "." ([Hypo] "then")? "a" Kind "is" DefVerb "to" "be" "any" Kind "."   -- DefAnyKindJmt
      | Label "." ([Hypo] "then")? "." "by" "cases:" DefCases   -- DefByCasesJmt
      | Label "." ([Hypo] "then")? Exp "is" "a" Kind "defined" "as" Exp "."   -- DefExpJmt
      | Label "." ([Hypo] "then")? "a" Kind "is" "a" Kind "."   -- DefKindJmt
      | Label "." ([Hypo] "then")? Exp "is" DefVerb "to" "be" "a" Kind "if" Prop "."   -- DefNounJmt
      | Label "." ([Hypo] "then")? Prop "," "if" Prop "."   -- DefPropJmt
      | Label "." ([Hypo] "then")? Exp "is" "a" Kind "such" "that" Prop "."   -- DefSuchThatJmt
      | Label "." Exp "is" "defined" "as" Exp "."   -- DefUntypedExpJmt
      | Label "." ([Hypo] "then")? Exp "is" "defined" "to" "be" Adj "if" Prop "."   -- DefinedAdjJmt
      | Label "." ([Hypo] "then")? "we" "denote" "by" Exp Exp "."   -- DenoteJmt
      | Example   -- ExampleJmt
      | Label "." "a" Kind "is" "a" Kind "$" Ident "$" "with" [ArgKind] "."   -- ExtensionDataJmt
      | Label "." ([Hypo] "then")? "a" Kind "is" "a" Kind "$" Ident "$" "with" [ArgKind] "such" "that" Prop "."   -- ExtensionHyposJmt
      | Label "." "a" Kind "is" "a" Kind "$" Ident "$" "with" [ArgKind] "such" "that" Prop "."   -- ExtensionJmt
      | Label "." "a" Kind "is" "a" Kind "$" Ident "$" "such" "that" Prop "."   -- ExtensionPropJmt
      | Label "." ([Hypo] "then")? Exp "is" "denoted" "by" Exp "."   -- IsDenotedJmt
      | Label "." ([Hypo] "then")? Exp "is" DefVerb "to" "be" Exp "."   -- NamingJmt
      | Label "." ([Hypo] "then")? Kind "are" DefVerb "to" "be" Kind "."   -- NamingKindJmt
      | "by" "cases:" "\\begin{itemize}" [Rule] "\\end{itemize}"   -- RewriteJmt
      | Label "." "a" Kind "is" "an" "algebraic" "structure" "," "where" "\\begin{itemize}" [Clause] "\\end{itemize}"   -- StructureJmt
      | Label "." ([Hypo] "then")? Prop "." "proof" "." Proof   -- ThmJmt
      | ">" Unit   -- UnitJmt
      | Label "." ([Hypo] "then")? "we" "define" Exp "to" "be" Adj "if" Prop "."   -- WeDefineAdjJmt
      | Label "." ([Hypo] "then")? "we" "define" Exp "to" "be" Exp "."   -- WeDefineExpJmt
      | Label "." ([Hypo] "then")? "we" "say" "that" Prop "if" Prop "."   -- WeSayJmt
```

## Label

```
Label ::= "the" Noun   -- DefNounLabel
        | Ident   -- IdentLabel
        | Noun Ident   -- NounIdentLabel
        | Noun Int   -- NounIntLabel
        | Noun   -- NounLabel
        | "the" Noun "of" Noun   -- NounOfNounLabel
        | ProperName Noun   -- ProperNameNounLabel
        | "\\cref" "{" Ident "}"   -- crefLabel
        | "axiom" | "the" "definition" "of" "difference" | "the" "definition" "of" "inclusion" | "the" "definition" "of" "set" "equality" | "the" "definition" "of" "union" | ...   -- 10 words
```

## [Hypo] (list of Hypo)

```
[Hypo] ::= Hypo "." [Hypo]   -- ConsHypo
         | ""
```

## Exp

```
Exp ::= "the" "function" "that" "maps" [Ident] "to" Exp   -- AbsExp
      | [Exp] "and" Exp   -- AndExp
      | Exp "applied" "to" Exps   -- AppExp
      | Noun "$" Term "$"   -- ApposTermExp
      | "the" Binder1 "of" Exp "where" "$" Ident "$" "is" "a" Kind   -- Binder1Exp
      | "the" Binder2 "of" Exp "where" "$" Ident "$" "ranges" "from" Exp "to" Exp   -- Binder2Exp
      | "the" Binder Exp "of" "$" Ident "$"   -- BinderExp
      | "both" Exp "and" Exp   -- BothAndExp
      | Exp "," "called" Exp   -- CalledExp
      | Exp "," "denoted" "by" Exp   -- DenotedExp
      | "either" Exp "or" Exp   -- EitherOrExp
      | Exp "," "endowed" "with" Exp ","   -- EndowedExp
      | "the" "set" "of" Exps   -- EnumSetExp
      | "the" Fun2 Exp "and" Exp   -- Fun2Exp
      | "the" Fun3 Exp Exp Exp   -- Fun3Exp
      | "the" FunC [Exp] "and" Exp   -- FunCCollExp
      | "the" FunC Exp "and" Exp   -- FunCExp
      | "the" Fun Exp   -- FunExp
      | "\\INDEXEDTERM" "{" Int "}"   -- IndexedTermExp
      | "the" "set" "of" Kind   -- KindExp
      | Name   -- NameExp
      | Exp "," "namely" Exp   -- NamelyExp
      | "the" "number" "of" Kind   -- NumberOfExp
      | [Exp] "or" Exp   -- OrExp
      | "the" Kind   -- PluralKindExp
      | "the" "proposition" "that" Prop   -- PropExp
      | Quant   -- QuantExp
      | "the" "set" "of" "all" Kind   -- SetOfAllExp
      | "$" Term "$"   -- TermExp
      | "the" "unique" ArgKind "such" "that" Prop   -- TheUniqueExp
      | "the" Kind Exp   -- TypedExp
```

## Kind

```
Kind ::= Adj Kind   -- AdjKind
       | Kind "such" "that" Prop   -- BareSuchThatKind
       | Kind Prop "such" "that" Prop   -- DeclSuchThatKind
       | Dep2 Exp Exp   -- Dep2Kind
       | DepC Exp "and" Exp   -- DepCKind
       | Dep Exp   -- DepKind
       | "instance" "of" Kind   -- ElemKind
       | "element" "of" Exp   -- ExpKind
       | Fam2 Kind Kind   -- Fam2Kind
       | Fam Kind   -- FamKind
       | "function" "from" [ArgKind] "to" Kind   -- FunKind
       | Noun   -- NounKind
       | Kind "of" "the" "form" Exp "," "where" Prop   -- OfTheFormKind
       | Kind Adj2 Exp   -- PostAdj2Kind
       | "predicate" "over" [ArgKind]   -- PredicateKind
       | Kind "with" "the" "following" "properties" ":" PropEnum   -- PropertiesKind
       | Kind "that" Pred   -- RelKind
       | Kind "$" Ident "$" "such" "that" Prop   -- SuchThatKind
       | Kind "(" "or" "a" Kind ")"   -- SynonymKind
       | Kind "$" Ident "$" "with" "the" "property" "that" Prop   -- WithPropertyKind
```

## Prop

```
Prop ::= Exp "is" Adj2 Exp   -- Adj2Prop
       | Exp "is" Adj3 Exp Exp   -- Adj3Prop
       | Exp "and" Exp "are" AdjC3 Exp   -- AdjC3Prop
       | [Exp] "and" Exp "are" AdjC   -- AdjCCollProp
       | Exp "and" Exp "are" AdjC   -- AdjCProp
       | [Exp] "and" Exp "are" AdjE   -- AdjECollProp
       | Exp "and" Exp "are" AdjE   -- AdjEProp
       | Exp "is" Adj   -- AdjProp
       | Exp "is" Adv2 Exp   -- Adv2Prop
       | [Exp] "and" Exp "are" AdvC   -- AdvCCollProp
       | Exp "and" Exp "are" AdvC   -- AdvCProp
       | Exp "is" Adv   -- AdvProp
       | "for" "all" [ArgKind] "," Prop   -- AllProp
       | [Prop] "and" Prop   -- AndProp
       | "$" Ident "$" "holds" "for" Exps   -- AppProp
       | "for" "all" [Ident] "," Prop   -- BareAllProp
       | "there" "exists" [Ident] "," "such" "that" Prop   -- BareExistProp
       | "both" Prop "and" Prop   -- BothAndProp
       | "for" "all" Kind "$" Ident "$" "," Prop   -- CoreAllProp
       | Prop "and" Prop   -- CoreAndProp
       | "there" "exists" "a" Kind "$" Ident "$" "such" "that" Prop   -- CoreExistProp
       | "if" Prop "," "then" Prop   -- CoreIfProp, IfProp
       | Prop "," "if" "and" "only" "if" Prop   -- CoreIffProp, IffProp
       | "it" "is" "not" "the" "case" "that" Prop   -- CoreNotProp
       | Prop "or" Prop   -- CoreOrProp
       | "$$" Formula "$$"   -- DisplayFormulaProp
       | "either" Prop "or" Prop   -- EitherOrProp
       | "the" "following" "conditions" "are" "equivalent" ":" PropEnum   -- EquivalentProp
       | "there" "is" "a" Kind   -- ExistKindProp
       | "there" "exists" [ArgKind] "," "such" "that" Prop   -- ExistNoProp, ExistProp
       | "there" "exists" "a" "unique" ArgKind "such" "that" Prop   -- ExistUniqueProp
       | "$" Formula "$" "implies" "$" Formula "$"   -- FormulaImpliesProp
       | "$" Formula "$"   -- FormulaProp
       | "given" [ArgKind] "," Prop   -- GivenProp
       | Exp "has" NumDet Noun   -- HaveProp
       | "$" Ident "$"   -- IdentProp
       | "if" Exp Pred "," "then" "so" "is" Exp   -- IfSoIsProp
       | Prop "iff" Prop   -- IffIffProp
       | "\\INDEXEDTERM" "{" Int "}"   -- IndexedFormulaProp
       | Prop "," "if" Prop   -- InverseIfProp
       | Exp "is" "a" Kind   -- KindProp
       | "for" Exp "to" Pred "," "it" "is" Condition "that" Prop   -- NecSuffProp
       | Exp "is" "not" "a" NegNoun2 Exp   -- NegNoun2Prop
       | "there" "exists" ArgKind "such" "that" Prop   -- NoArticleExistProp
       | "for" "all" [ArgKind] Prop   -- NoCommaAllProp
       | "there" "exists" [ArgKind] "such" "that" Prop   -- NoCommaExistProp
       | Exp "is" "not" Adj2 Exp   -- NotAdj2Prop
       | [Exp] "and" Exp "are" "not" AdjC   -- NotAdjCProp
       | [Exp] "and" Exp "are" "not" AdjE   -- NotAdjEProp
       | Exp "is" "not" Adj   -- NotAdjProp
       | Exp "is" "not" Adv2 Exp   -- NotAdv2Prop
       | [Exp] "and" Exp "are" "not" AdvC   -- NotAdvCProp
       | Exp "is" "not" Adv   -- NotAdvProp
       | Exp "is" "not" "a" Kind   -- NotKindProp
       | Exp "is" "not" "a" Noun1   -- NotNoun1Prop
       | Exp "is" "not" "a" Noun2 Exp   -- NotNoun2Prop
       | Exp "is" "not" "a" Noun3 Exp Exp   -- NotNoun3Prop
       | [Exp] "and" Exp "are" "not" NounC   -- NotNounCProp
       | Exp "does" "not" Verb2 Exp   -- NotVerb2Prop
       | [Exp] "and" Exp "do" "not" VerbC   -- NotVerbCProp
       | Exp "does" "not" Verb   -- NotVerbProp
       | Exp "is" "a" Noun1   -- Noun1Prop
       | Exp "is" "a" Noun2 Exp   -- Noun2Prop
       | Exp "is" "a" Noun3 Exp Exp   -- Noun3Prop
       | [Exp] "and" Exp "are" NounC   -- NounCCollProp
       | Exp "and" Exp "are" NounC   -- NounCProp
       | Prop "," "only" "if" Prop   -- OnlyIfProp
       | [Prop] "or" Prop   -- OrProp
       | Prop "for" Quant   -- PostQuantProp
       | "we" "can" "prove" "that" Prop   -- ProofProp
       | Prop "(" "resp" "." Prop ")"   -- RespProp
       | Exp Verb2 Exp   -- Verb2Prop
       | [Exp] "and" Exp VerbC   -- VerbCCollProp
       | Exp "and" Exp VerbC   -- VerbCProp
       | Exp Verb   -- VerbProp
       | "we" "have" Prop   -- WeHaveProp
       | Prop "whenever" Prop   -- WheneverProp
       | "we" "have" "a" "contradiction"
```

## [Exp] (list of Exp)

```
[Exp] ::= Exp ("," Exp)*
```

## DefVerb

```
DefVerb ::= "by" "definition" | ...   -- 4 words
```

## AdjC

```
AdjC ::= Adj   -- AdjAdjC
       | "distinct" | "disjoint" | "isomorphic" | "orthogonal" | "perpendicular" | ...   -- 6 words
```

## Adj

```
Adj ::= Adj2 Exp   -- Adj2Adj
      | Adj3 Exp Exp   -- Adj3Adj
      | Adj Prep Noun   -- AdjPrepNounAdj
      | Adverb Adj   -- AdverbAdjAdj
      | [Adj] "and" Adj   -- AndAdj
      | "both" Adj "and" Adj   -- BothAndAdj
      | "either" Adj "or" Adj   -- EitherOrAdj
      | "not" Adj   -- NotAdjAdj
      | [Adj] "or" Adj   -- OrAdj
      | Adj "(" "resp" "." Adj ")"   -- RespAdj
      | Adj "(" "or" Adj ")"   -- SynonymAdj
      | "even" | "prime" | "finite" | "continuous" | "injective" | ...   -- 1371 words
```

## DefCases

```
DefCases ::= "\\begin{itemize}" [DefCase] "\\end{itemize}"   -- NoOtherwiseDefCases
           | "\\begin{itemize}" [DefCase] "otherwise" Prop "." "\\end{itemize}"   -- OtherwiseDefCases
```

## Example

```
Example ::= Argument "is" Adj2 Argument   -- Adj2Example
          | Argument "is" Adj3 Argument Argument   -- Adj3Example
          | Argument "and" Argument "are" AdjC3 Argument   -- AdjC3Example
          | Argument "and" Argument "are" AdjC   -- AdjCExample
          | Argument "and" Argument "are" AdjE "EQUIVALENCE"   -- AdjEExample
          | Argument "is" Adj   -- AdjExample
          | Argument "is" Adv2 Argument   -- Adv2Example
          | Argument "and" Argument "are" AdvC   -- AdvCExample
          | Argument "is" Adv   -- AdvExample
          | "the" Binder1 "of" Argument "where" "$" BoundVariable "$" "is" "a" KindArgument   -- Binder1Example
          | "the" Binder2 "of" Argument "where" "$" BoundVariable "$" "ranges" "from" Argument "to" Argument   -- Binder2Example
          | "the" Binder Argument "of" "$" BoundVariable "$"   -- BinderExample
          | Dep2 Argument Argument   -- Dep2Example
          | DepC Argument "and" Argument   -- DepCExample
          | Dep Argument   -- DepExample
          | Fam2 KindArgument KindArgument   -- Fam2Example
          | Fam KindArgument   -- FamExample
          | "the" Fun2 Argument "and" Argument   -- Fun2Example
          | "the" Fun3 Argument Argument Argument   -- Fun3Example
          | "the" FunC Argument "and" Argument   -- FunCExample
          | "the" Fun Argument   -- FunExample
          | Label "."   -- LabelExample
          | Name   -- NameExample
          | Argument "is" "not" "a" NegNoun2 Argument   -- NegNoun2Example
          | Argument "is" "a" Noun1   -- Noun1Example
          | Argument "is" "a" Noun2 Argument   -- Noun2Example
          | Argument "is" "a" Noun3 Argument Argument   -- Noun3Example
          | Argument "and" Argument "are" NounC   -- NounCExample
          | Noun   -- NounExample
          | Argument Verb2 Argument   -- Verb2Example
          | Argument "and" Argument VerbC   -- VerbCExample
          | Argument Verb   -- VerbExample
```

## Ident

```
Ident ::= String   -- StrIdent
```

## [ArgKind] (list of ArgKind)

```
[ArgKind] ::= ArgKind   -- BaseArgKind
            | ArgKind "and" [ArgKind]   -- ConsArgKind
```

## [Rule] (list of Rule)

```
[Rule] ::= "\\item" Rule "."   -- BaseRule
         | "\\item" Rule "." [Rule]   -- ConsRule
```

## [Clause] (list of Clause)

```
[Clause] ::= "\\item" Clause "."   -- BaseClause
           | "\\item" Clause "." [Clause]   -- ConsClause
```

## Proof

```
Proof ::= [Hypo] Proof   -- AbsProof
        | [Proof] "by" ProofExp "."   -- AppProof
        | "we" "reason" "by" "induction" ":" "let" "$" Ident "$" "be" "$" "0" "$" "." Proof "let" "$" "$" "be" "a" "natural" "number" "." "assume" "that" Prop "." "(" Ident ")" Proof "we" "conclude" "that" "for" "all" "natural" "numbers" "$" "$" "," "."   -- NatIndProof
        | Proof "a" "fortiori" Prop "."   -- andElProof, andErProof
        | Proof Proof "altogether" Prop "and" Prop "."   -- andIProof
        | Proof "by" "the" "second" "axiom" "of" "parity" "," "the" "successor" "of" Exp "is" "even" "."   -- evenSuccProof
        | Proof "let" "$" Ident "$" "be" "a" Kind "." "assume" "that" Prop "." "(" Ident ")" Proof "we" "conclude" "that" Prop "."   -- existsEProof
        | Proof "hence" "there" "exists" "a" Kind "$" Ident "$" "such" "that" Prop "."   -- existsIProof
        | Proof "hence" Prop "."   -- falseEProof
        | Proof "in" "particular" Prop "." "where" Ident "is" Exp   -- forallEProof
        | "let" "$" Ident "$" "be" "a" Kind "." Proof "hence" "for" "all" "$" "$" "," Prop "."   -- forallIProof
        | "by" Ident "," Prop "."   -- hypoProof
        | Proof Proof "hence" Prop "."   -- ifEProof
        | "assume" "that" Prop "." "(" Ident ")" Proof "hence" "if" "," "then" Prop "."   -- ifIProof
        | Proof "by" "the" "third" "axiom" "of" "parity" "," "the" "successor" "of" Exp "is" "odd" "."   -- oddSuccProof
        | Proof "we" "reason" "by" "cases" ":" "\\item" "assume" "that" Prop "." "(" Ident ")" Proof "\\item" "assume" "that" Prop "." "(" Ident ")" Proof "hence" Prop "."   -- orEProof
        | Proof "a" "fortiori" Prop "or" Prop "."   -- orIlProof, orIrProof
        | Exp "equals" "." "by" "reflexivity" "."   -- reflProof
        | "by" "the" "first" "axiom" "of" "parity" "," "$" "0" "$" "is" "even" "."
```

## Unit

```
Unit ::= "\\begin" "{" Environment "}" "\\label" "{" Label "}"   -- BeginEnvironmentUnit
       | "\\begin" "{" "proof" "}" "\\label" "{" Label "}" "(" Method ")"   -- BeginProofMethodUnit
       | "\\item" "assume" "that" Prop "." "(" Ident ")"   -- CaseGoal
       | Prop "."   -- ConclusionUnit
       | "\\end" "{" Environment "}"   -- EndEnvironmentUnit
       | "it" "is" "enough" "to" "show" "that" Prop "."   -- EnoughGoal
       | "first" "verify" "that" Prop "."   -- FirstVerifyGoal
       | "it" "follows" "that" Prop "."   -- FollowsPropConclusion
       | "to" "prove" "that" Prop "," [Hypo]   -- GoalHyposUnit
       | Hence "," Prop "."   -- HencePropConclusion
       | [Hypo]   -- HyposUnit
       | "let" "$" Ident "$" "be" Exp "."   -- IdentExpAssumption
       | "let" "$" Ident "$" "be" "a" Kind "."   -- IdentKindAssumption
       | "\\import" "{" String "}"   -- ImportUnit
       | "follows" "by" Label   -- LabelConclusion
       | "by" Label "," Prop "."   -- LabelPropConclusion
       | "\\label" "{" Label "}"   -- LabelUnit
       | "in" "the" CaseNumber "case" "," "assume" "that" Prop "."   -- NumberCaseGoal
       | "we" "have" CaseNumber "case" ":"   -- NumberCasesGoal
       | "assume" "that" Prop "." "(" Label ")"   -- PropAssumption
       | Hence Prop "."   -- PropConclusion
       | Hence Prop "by" Label "."   -- PropLabelConclusion
       | "\\section" "{" Title "}" "\\label" "{" Label "}"   -- SectionUnit
       | "since" Prop "," Prop "."   -- SinceConclusion
       | "since" Prop "," "it" "is" "enough" "to" "show" "that" Prop "."   -- SinceGoal
       | "\\subsection" "{" Title "}" "\\label" "{" Label "}"   -- SubsectionUnit
       | "we" "reason" "by" "cases" ":" | "we" "reason" "by" "induction" ":" | "it" "is" "obvious" "."
```

## Noun

```
Noun ::= Adj Noun   -- AdjNounNoun
       | Noun "$" Ident "$"   -- ApposIdentNoun
       | Dep2 Exp Exp   -- Dep2Noun
       | DepC Exp "and" Exp   -- DepCNoun
       | Dep Exp   -- DepNoun
       | Noun2 Exp   -- Noun2ExpNoun
       | Noun3 Exp Exp   -- Noun3ExpsNoun
       | Noun Noun   -- NounNounNoun
       | Noun Prep Noun   -- NounPrepNounNoun
       | ProperName Noun   -- ProperNameNounNoun
       | "set" | "number" | "group" | "function" | "sequence" | ...   -- 2171 words
```

## Int

```
Int ::= "0" | "1" | "2" | ...   -- integer literals
```

## ProperName

```
ProperName ::= "Accor" | "Cremona" | "Hawkes" | "Lucas" | "Schur" | ...   -- 372 words
```

## Hypo

```
Hypo ::= "let" [Ident] "be" "a" Adj Kind   -- AdjKindHypo
       | "let" "$" Ident "$" "be" "arbitrary"   -- BareVarHypo
       | "let" [Ident] "be" "arbitrary"   -- BareVarsHypo
       | "let" "\\INDEXEDTERM" "{" Int "}"   -- IndexedLetFormulaHypo
       | "let" "\\INDEXEDTERM" "{" Int "}" "such" "that" Prop   -- IndexedLetFormulaSuchThatHypo
       | "let" "$" Declaration "$"   -- LetDeclarationHypo
       | "let" "$" Formula "$"   -- LetFormulaHypo
       | "let" "$" Formula "$" "such" "that" Prop   -- LetFormulaSuchThatHypo
       | "let" Local "be"   -- LocalHypo
       | "assume" "that" Prop   -- PropHypo
       | "assume" "that" Prop "(" Ident ")"   -- PropVarHypo
       | "let" [Ident] "be" "a" Kind "such" "that" Prop   -- SuchThatHypo
       | "suppose" Prop   -- SupposePropHypo
       | "let" "$" Ident "$" "be" "a" Kind   -- VarHypo
       | "let" [Ident] "be" "a" Kind   -- VarsHypo
```

## [Ident] (list of Ident)

```
[Ident] ::= "$" Ident "$"   -- BaseIdent
          | "$" Ident "$" "and" [Ident]   -- ConsIdent
```

## Exps

```
Exps ::= [Exp] "and" Exp   -- ManyExps
       | Exp   -- OneExps
```

## Term

```
Term ::= "\\lambda" Ident "." Term   -- AbsTerm
       | Macro "{" Term "}"   -- App1MacroTerm
       | Macro "{" Term "}" "{" Term "}"   -- App2MacroTerm
       | Macro "{" Term "}" "{" Term "}" "{" Term "}"   -- App3MacroTerm
       | Macro "{" Term "}" "{" Term "}" "{" Term "}" "{" Term "}"   -- App4MacroTerm
       | Macro "{" Term "}" "{" Term "}" "{" Term "}" "{" Term "}" "{" Term "}"   -- App5MacroTerm
       | Function "(" [Term] ")"   -- AppFunctionTerm
       | Term "[" Term "/" Ident "]"   -- BetaRedexTerm
       | "\\{" Ident "\\in" Term "\\mid" Formula "\\}"   -- ComprehensionTerm
       | "\\{" Ident "\\in" Term "\\mid" "\\text{" Prop "}" "\\}"   -- ComprehensionTextTerm
       | Const   -- ConstTerm
       | "\\{" [Term] "\\}"   -- EnumSetTerm
       | "\\{" Term "\\text{" "for" [ArgKind] "such" "that" Prop "}" "\\}"   -- FraenkelComprehensionTerm
       | "\\{" Term "\\text{" "for" [ArgKind] "}" "\\}"   -- FraenkelSimpleComprehensionTerm
       | Term "\\rightarrow" Term   -- FunTypeTerm
       | Ident   -- IdentTerm
       | Macro   -- MacroTerm
       | Int   -- NumberTerm
       | Oper2 Term Term   -- Oper2Term
       | Oper Term   -- OperTerm
       | "(" Term ")"   -- ParenthTerm
       | "\\textbf" "{" Term "}"   -- TextbfTerm
       | "\\text{" Exp "}"   -- TextualTerm
       | "\\langle" [Term] "\\rangle"   -- TupleTerm
       | Term "+" Term "+" "\\cdots" "+" Term   -- sum3dots_Term
       | Term "\\times" Term   -- times_Term
```

## Binder1

```
Binder1 ::= Noun   -- NounBinder1
```

## Binder2

```
Binder2 ::= Noun   -- NounBinder2
```

## Binder

```
Binder ::= Noun   -- NounBinder
```

## Fun2

```
Fun2 ::= Noun   -- NounPrepFun2
       | "angle" | "binomial" "coefficient" | "set" "of" "combinations" | "difference" | "logarithm" | ...   -- 10 words
```

## Fun3

```
Fun3 ::= Noun   -- NounPrepsFun3
```

## FunC

```
FunC ::= Noun   -- NounPrepFunC
       | "cartesian" "product" | "dot" "product" | "intersection" | "sum" | "product" | ...   -- 9 words
```

## Fun

```
Fun ::= Fun "$" Ident "$"   -- FunIdentFun
      | Noun   -- NounPrepFun
      | "absolute" "value" | "degree" | "square" | "area" | ...   -- 23 words
```

## Name

```
Name ::= "the" Noun   -- DefNounName
       | ProperName Noun   -- ProperNameNounName
       | "the" "empty" "set" | "the" "number" "\\(\\pi\\)" | "three" | "two" | "the" "universal" "set"
```

## Quant

```
Quant ::= "all" Kind [Ident]   -- AllIdentsKindQuant
        | "all" Kind   -- AllKindQuant
        | "every" Kind "$" Ident "$"   -- EveryIdentKindQuant
        | "every" Kind   -- EveryKindQuant
        | "a" Kind "$" Ident "$"   -- IndefIdentKindQuant
        | "a" Kind   -- IndefKindQuant
        | "no" Kind [Ident]   -- NoIdentsKindQuant
        | "no" Kind   -- NoKindQuant
        | NumDet Kind   -- NumDetKindQuant
        | "some" Kind [Ident]   -- SomeIdentsKindQuant
        | "some" Kind   -- SomeKindQuant
```

## ArgKind

```
ArgKind ::= [Ident]   -- BareIdentsArgKind
          | Kind Prop   -- DeclArgKind
          | "$" Declaration "$"   -- DeclarationArgKind
          | Kind "$" Ident "$"   -- IdentArgKind
          | Kind [Ident]   -- IdentsArgKind
          | "\\INDEXEDTERM{" Int "}"   -- IndexedDeclarationArgKind
          | Kind   -- KindArgKind
```

## Dep2

```
Dep2 ::= Noun   -- NounPrepDep2
```

## DepC

```
DepC ::= Noun   -- NounPrepDepC
       | "isomorphism"
```

## Dep

```
Dep ::= Noun   -- NounPrepDep
      | "family" "of" "subset"
```

## Fam2

```
Fam2 ::= Noun   -- NounPrepFam2
       | "function"
```

## Fam

```
Fam ::= Noun   -- NounPrepFam
      | "list" | "set"
```

## Adj2

```
Adj2 ::= Adj Prep   -- AdjPrepAdj2
       | "equal" "to" | "less" "than" | "divisible" "by" | ...   -- 23 words
```

## PropEnum

```
PropEnum ::= "a" ")" Prop ";" "b" ")" Prop   -- BasePropEnum
           | "a" ")" Prop ";" PropEnum   -- ConsPropEnum
```

## Pred

```
Pred ::= "is" Adj2 Exp   -- Adj2Pred
       | "is" Adj   -- AdjPred
       | "has" NumDet Noun   -- HavePred
       | "is" "a" Kind   -- KindPred
       | "is" "a" Noun2 Exp   -- Noun2Pred
       | Verb2 Exp   -- Verb2Pred
       | Verb   -- VerbPred
```

## Adj3

```
Adj3 ::= Adj Prep Prep   -- AdjPrepAdj3
       | "congruent" "to" "modulo"
```

## AdjC3

```
AdjC3 ::= Adj Prep   -- AdjPrepAdjC3
```

## AdjE

```
AdjE ::= Adj   -- AdjAdjE
       | "equal"
```

## Adv2

```
Adv2 ::= Prep   -- PrepAdv2
```

## AdvC

```
AdvC ::= Adv   -- AdvAdvC
```

## Adv

```
Adv ::= Prep Noun   -- PrepNounAdv
      | "otherwise"
```

## [Prop] (list of Prop)

```
[Prop] ::= Prop ("," Prop)*
```

## Formula

```
Formula ::= Formula "\\wedge" Formula   -- AndFormula
          | Macro "{" Term "}"   -- App1MacroFormula
          | Macro "{" Term "}" "{" Term "}"   -- App2MacroFormula
          | Macro "{" Term "}" "{" Term "}" "{" Term "}"   -- App3MacroFormula
          | Macro "{" Term "}" "{" Term "}" "{" Term "}" "{" Term "}"   -- App4MacroFormula
          | Macro "{" Term "}" "{" Term "}" "{" Term "}" "{" Term "}" "{" Term "}"   -- App5MacroFormula
          | Predicate "(" [Term] ")"   -- AppPredicateFormula
          | [Term] "\\in" Term   -- ElemFormula
          | Equation   -- EquationFormula
          | Formula "\\Rightarrow" Formula   -- IfFormula
          | Formula "\\Leftrightarrow" Formula   -- IffFormula
          | Macro   -- MacroFormula
          | "\\neg" Formula   -- NotFormula
          | Formula "\\vee" Formula   -- OrFormula
```

## NumDet

```
NumDet ::= "at" "least" Card   -- AtLeastNumDet
         | "at" "most" Card   -- AtMostNumDet
         | "exactly" Card   -- ExactlyNumDet
         | "a" | "no"
```

## Condition

```
Condition ::= "necessary" | "necessary" "and" "sufficient" | "sufficient"
```

## NegNoun2

```
NegNoun2 ::= Noun2   -- NotNoun2
```

## Noun1

```
Noun1 ::= Noun   -- NounNoun1
```

## Noun2

```
Noun2 ::= Adj Noun2   -- AdjNoun2Noun2
        | Noun   -- NounPrepNoun2
        | "divisor" | "element" | "non-element" | "proper" "subset" | "proper" "superset" | ...   -- 9 words
```

## Noun3

```
Noun3 ::= Adj Noun3   -- AdjNoun3Noun3
        | Noun   -- NounPrepsNoun3
```

## NounC

```
NounC ::= Adj NounC   -- AdjNounCNounC
        | Noun   -- NounNounC
```

## Verb2

```
Verb2 ::= Verb Prep   -- VerbPrepVerb2
        | Verb   -- VerbVerb2
        | "divide" | "contain" | "meet" | ...   -- 6 words
```

## VerbC

```
VerbC ::= Verb   -- VerbVerbC
```

## Verb

```
Verb ::= Verb "the" Noun   -- VerbDefNounVerb
       | Verb "a" Noun   -- VerbNounVerb
       | Verb Noun   -- VerbPluralNounVerb
       | Verb Prep "the" Noun   -- VerbPrepDefNounVerb
       | Verb Prep "a" Noun   -- VerbPrepNounVerb
       | Verb Prep Noun   -- VerbPrepPluralNounVerb
       | "converge" | "vanish" | "exist" | "abandon" | "complexvalue" | ...   -- 438 words
```

## Prep

```
Prep ::= "of" | "on" | "in" | "by" | "from" | ...   -- 34 words
```

## Adverb

```
Adverb ::= "absolutely" | "doubly" | "fully" | "non" | "separably" | ...   -- 56 words
```

## [Adj] (list of Adj)

```
[Adj] ::= Adj ("," Adj)*
```

## [DefCase] (list of DefCase)

```
[DefCase] ::= "\\item" DefCase "."   -- BaseDefCase
            | "\\item" DefCase "." [DefCase]   -- ConsDefCase
```

## Argument

```
Argument ::= "#" Int   -- IntArgument
```

## BoundVariable

```
BoundVariable ::= "i" | "x"
```

## KindArgument

```
```

## String

```
String ::= ...   -- string literals: identifiers, macro names, titles
```

## Rule

```
Rule ::= Exp "is" Exp   -- NoVarRewriteRule
       | "for" [Ident] "," Exp "is" Exp   -- RewriteRule
```

## Clause

```
Clause ::= Prop   -- PropClause
         | "$" Ident "$" "is" "a" Kind   -- VarClause
```

## [Proof] (list of Proof)

```
[Proof] ::= Proof [Proof]   -- ConsProof
          | ""
```

## ProofExp

```
ProofExp ::= ProofExp "assuming:" [Hypo]   -- AbsProofExp
           | ProofExp "applied" "to" Exps   -- AppProofExp
           | Label   -- LabelProofExp
```

## Environment

```
Environment ::= "abbreviation" | "byCase" | "definition" | "primrec" | "signature" | ...   -- 17 words
```

## Method

```
Method ::= String   -- StringMethod
```

## Hence

```
Hence ::= "a" "fortiori" | "altogether" | "in" "both" "cases" | "in" "particular" | "thus" | ...   -- 8 words
```

## CaseNumber

```
CaseNumber ::= "eight" | "five" | "nine" | "seven" | "three" | ...   -- 9 words
```

## Title

```
Title ::= String   -- StringTitle
```

## Declaration

```
Declaration ::= [Term] "\\in" Term   -- ElemDeclaration
              | Ident ":" Term "\\rightarrow" Term   -- FunctionDeclaration
```

## Local

```
Local ::= "$" Ident "$" Exp   -- BareLetLocal
        | "the" Kind "$" Ident "$" Exp   -- LetLocal
```

## Macro

```
Macro ::= String   -- StringMacro
```

## Function

```
Function ::= Function "'"   -- DerivativeFunction
           | Ident   -- IdentFunction
```

## [Term] (list of Term)

```
[Term] ::= Term   -- BaseTerm
         | Term "," [Term]   -- ConsTerm
```

## Const

```
Const ::= "C" | "\\emptyset" | "N" | "\\pi" | "R" | ...   -- 8 words
```

## Oper2

```
Oper2 ::= "\\binom{" | "\\frac{" | "\\left(\\frac{" | "\\log_{" | ...   -- 5 words
```

## Oper

```
Oper ::= "|" | "\\arcsin" | "{" | "-" | "\\sin" | ...   -- 13 words
```

## Predicate

```
Predicate ::= Ident   -- IdentPredicate
```

## Equation

```
Equation ::= Term Compar Term   -- BinaryEquation
           | Term Compar Equation   -- ChainEquation
```

## Card

```
Card ::= Exp   -- ExpCard
       | "one" | "three" | "two"
```

## DefCase

```
DefCase ::= Prop "," "if" Prop   -- PropsDefCase
```

## Compar

```
Compar ::= "=" | "\\leq" | "\\notmeets" | "\\notin" | "\\subset" | ...   -- 17 words
```

