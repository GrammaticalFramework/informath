# Informath: Informalization and Autoformalization of Formal Mathematics

(c) Aarne Ranta 2025-2026

[Code repository](https://github.com/GrammaticalFramework/informath)

[Documents in github.io](https://grammaticalframework.github.io/informath/)

## NEWS

13 January 2026: divided this document to a basic part (this README file) and a new [Informath under the Hood document](./doc/informath-under-the-hood.md)).

13 January 2026: enable reading standard input; see `RunInformath -help`.

12 January 2026: a very rudimentary proof-of-concept [Dedukti implementation](./test/natural_deduction.dk) and [GF grammar](./grammars/NaturalDeduction.gf) for natural deduction proofs. You can test this with `make natural_deduction`.

19 December 2025: paper [Multilingual Autoformalization via Fine-tuning Large Language Models with Symbolically Generated Data](https://epub.jku.at/doi/10.35011/risc-proceedings-scml.1) appeared. Its focus is on the use of Informath in training data generation.

24 November 2025: The "-next" version is now default and "-previous" must be invoked with a flag. The previous version will be deprecated very soon, as all its functionality is available in the default version.

## Documentation

[Informath Under the Hood](https://grammaticalframework.github.io/informath/doc/informath-under-the-hood.html)

[Video from MCLP conference at Institut Pascal, Paris Saclay, September 2025](https://www.youtube.com/watch?v=9puGzYqta7Y&list=PLaT9F1eDUuN0FJAONMXxdGJrGGg2_x9Wb&index=4)

[Updated slides shown in Saclay, Prague, and some other places in 2025](./doc/dedukti-gf-2025.pdf)

[InformathAPI haddock-generated documentation](https://grammaticalframework.github.io/informath/doc/InformathAPI.html)

[Symbolic Informalization: Fluent, Productive, Multilingual](https://aitp-conference.org/2025/abstract/AITP_2025_paper_4.pdf) (by A. Ranta, AITP-2025, extended abstract)

[Multilingual Autoformalization via Fine-tuning Large Language Models with Symbolically Generated Data](https://epub.jku.at/doi/10.35011/risc-proceedings-scml.1), by Pei Huang, Nicholas Smallbone and Aarne Ranta, SCML Vol. 1, 2025.


## The Informath project

The Informath project addresses the problem of translating between formal and informal languages for mathematics. It aims to translate between multiple formal and informal languages in all directions: 

- formal to informal (**informalization**)
- informal to formal (**autoformalization**)
- informal to informal (translation, via formal)
- formal to formal (works in special cases)

The formal languages included are Agda, Rocq (formerly Coq), Dedukti, and Lean. The informal languages are English, French, German, and Swedish. 

Here is an example statement involving all of the currently available languages. The Dedukti statement has been used as the source of all the other formats. Also any of the natural languages could be used as the source:
```
Dedukti: prop110 : (a : Elem Int) -> (c : Elem Int) ->
  Proof (and (odd a) (odd c)) ->
  Proof (forall Int (b => even (plus (times a b) (times b c)))).

Agda: postulate prop110 : (a : Int) -> (c : Int) ->
  and (odd a) (odd c) ->
  all Int (\ b -> even (plus (times a b) (times b c)))

Rocq: Axiom prop110 : forall a : Int, forall c : Int,
  (odd a /\ odd c -> forall b : Int, even (a * b + b * c)) .

Lean: axiom prop110 (a c : Int) (x : odd a ∧ odd c) :
  ∀ b : Int, even (a * b + b * c)
```

- English: Prop110. Let $a$ and $c$ be integers. Assume that both $a$ and $c$ are odd. Then $a b + b c$ is even for all integers $b$.

- French: Prop110. Soient $a$ et $c$ des entiers. Supposons qu'et $a$ et $c$ sont impairs. Alors $a b + b c$ est pair pour tous les entiers $b$.

- German: Prop110. Seien $a$ und $c$ ganze Zahlen. Nimm an, dass sowohl $a$ als auch $c$ ungerade ist. Dann ist $a b + b c$ gerade für jede ganze Zahl $b$.

- Swedish: Prop110. Låt $a$ och $c$ vara heltal. Anta att både $a$ och $c$ är udda. Då är $a b + b c$ jämnt för alla heltal $b$.


More formalisms and informal languages will be added later. Also the scope of language structures is at the moment theorem statements and definitions; proofs are included for the sake of completeness, but will require more work to enable more natural verbalizations.


## Using Informath

The software included in this repository supports the translation of text and code files in batch mode. For a quick start, you can just do
```
  $ make
```
to build the executable `RunInformath` and all its dependencies. After that, you can do
```
  $ make demo
```
which illustrates different functionalities: translating between Dedukti and natural languages, as well as from Dedukti to Agda, Rocq, and Lean. 

Building the system requires the following software:

- [GF](https://www.grammaticalframework.org/) >= 3.12 (both as executable and as the PGF library)
- [GF-RGL](https://github.com/GrammaticalFramework/gf-rgl) (the Resource Grammar Library, to be compiled from its GitHub source)
- [BNFC](https://bnfc.digitalgrammars.com/) >= 2.9 (executable)
- [GHC](https://www.haskell.org/ghcup/) >= 9.6 (executable, with some common libraries)
- [alex](https://www.haskell.org/alex/) (executable, tested with 3.5.4)
- [happy](https://www.haskell.org/happy/) (executable)


## Some test datasets

The following datasets can be processed with `RunInformath <filename>` to generate text or code eveb without additional options; see `RunInformath -help` to see what can be done with various options.

- [test/exx.dk](./test/exx.dk) is a set of simple arithmetic statements.

- [test/gf-lean.data](./test/gflean-data.txt) is a set of arithmetic statements in natural language, extracted from the textbook [*Mathematical Proofs: A Transition to Advanced Mathematics*](https://pdfcoffee.com/mathematical-proofs-3rd-edition-chartrand-pdf-free.html) by Chartrand et al, used in [Pathak's GFLean project](https://arxiv.org/abs/2404.01234). Some statements in this set are not yet parsed or interpreted correctly.

- [test/naproche-zf-set.tex](./test/naproche-zf-set.tex) is a set of de Lon's [Naproche-ZF](https://adelon.net/naproche-zf) statements. Try `make naproche` to directly display a LaTeX document. Use `make lang=Fre naproche` to generate French (and similarly for Ger, Swe). Some statements are not yet parsed or interpreted correctly.

- [test/sets.dk](./test/sets.dk) contains set algebra statements from a [Wikipedia article](https://en.wikipedia.org/wiki/Algebra_of_sets). Try `make sets` to directly display a LaTeX document. Use `make lang=Fre sets` to generate French (and similarly for Ger, Swe).

- [test/sigma.dk](./test/sigma.dk) contains some examples of variable-binding constructs (sums, integrals). Try `make sigma` to directly display a LaTeX document.

- [test/top100.dk](./test/top100.dk) contains a selection of [Wiedijk's "100 theorems"](https://www.cs.ru.nl/~freek/100/). Try `make top100` to directly display a LaTeX document. Use `make lang=Fre top100` to generate French (and similarly for Ger, Swe).
  
- [datasets/smad.tar.bz2](./datasets/smad.tar.bz2) contains the synthetic data used in the [autoformalization experiment of Huang et al.](https://epub.jku.at/doi/10.35011/risc-proceedings-scml.1)

- [test/natural.tex](./test/natural.tex) contains the manually written top100-statements used for evaluating autoformalization in Huang et al. 


## Possible input and output formats formats

Use `RunInformath -help` to see the actually available file types and extensions. You can also use `RunInformath` on standard input, for instance,
```
$ echo "c : Proof (forall Num (n => if (even n) (not (odd n))))." | RunInformath
C. If $n$ is even, then $n$ is not odd for all numbers $n$.

$ echo "every number is even or odd." | RunInformath -formalize           
noLabel : Proof (forall Num (_h0 => or (even _h0) (odd _h0))) .
```
The option `-loop` allows you to translate between individual Dedukti and natural language judgements:
```
$ RunInformath -loop
> prop1 : Proof (forall Nat (n => if (even n) (not (odd n)))).
Prop1. If $n$ is even, then $n$ is not odd for all natural numbers $n$.
> ? Every number is even or odd.
noLabel : Proof (forall Num (_h0 => or (even _h0) (odd _h0))) .
> 
```
Input prefixed with `?` is treated as natural language, all other input as Dedukti. 
You can change the source and target languages with the `-from-lang` and `-to-lang` flags. 
You can quit the loop with Ctrl-C.


## Using your own data

You can in principle generate from any Dedukti (`.dk`) file, at least if it is well typed in Dedukti (which is not always necessary). However, the result will be quite bad unless you provide a symbol table with a `.dkgf` file, converting Dedukti identifiers to GF functions; see below about the structure of this file. 

There is a default symbol table, [baseconstants.dkgf](src/baseconstants.dkgf), which works for the examples listed above. But for other Dedukti files, it can give strange results or even processing errors because of name clashes between that file and the default symbol table. The first aid to this is to use the empty symbol table, by passing it to the flag `-constants`. An example is the conversion of a Matita dump:
```
$ RunInformath -constants=test/empty.dkgf test/mini-matita.dk
```
Notice that if you call `RunInformath` from another directory than the top directory of Informath (where this README resides), you need to pass a link to [informath/src/base_constants.dkgf](./informath/src/base_constants.dkgf) with the `-constants` flag, unless you use some other `.dkgf` file.

Thus the mapping between Dedukti and GF is defined in .dkgf files, by default in [baseconstants.dkgf](src/baseconstants.dkgf), which assigns GF functions to the constants in [BaseConstants.dk](src/BaseConstant.dk). The syntax of .dkgf files recognizes three kinds of lines;
- `<DeduktiIdent> <GFFunction>+`: different GF functions usable for expressing the Dedukti concept; the first one is consireded primary and the other ones are optional synonyms
- `#CONV <formalism> <DeduktiIdent> <FormalismIdent>`: conversion of Dedukti identifier to another formalism (e.g. its standard library function) 
- `#DROP <DeduktiIdent> <int>`: drop a number of initial arguments from the Dedukti function application

The coverage of Informath can be extended by writing a .dkgf file that maps Dedukti identifiers to GF functions. If those GF functions are already available, nothing else is needed than the inclusion of the flag `-constants=<file>.dkgf+` where `base_constants.dkgf`can be one of the files. How to define new GF functions is covered in the [under the hood document](./doc/informath-under-the-hood.md).

## Generating synthetic data

For those who are interested just in the generation of synthetic data, the following commands (after building Informath with `make`) can do it: assuming that you have a `.dk` file available, build a `.jsonl` file with all conversions of each Dedukti judgement:
```
$ RunInformath -parallel-data <file>.dk > <file>.jsonl
```
After that, select the desired formal and informal languages to generate a new `.jsonl` data with just those pairs:
```
$ python3 ./scripts/jsonltest.py <file.jsonl> <formal> <informal>
```
The currently available values of `<formal>` and `<informal>` are the keys in `<file>.jsonl` - for example, `agda` and `InformathEng`, respectively.

An example is [datasets/smad.tar.bz2](./datasets/smad.tar.bz2), which contains the synthetic data used in the [autoformalization experiment of Huang et al.](https://epub.jku.at/doi/10.35011/risc-proceedings-scml.1). It was generated with an earlier version of Informath in Spring 2025. But the Dedukti statements contained in it can be used for generating data with later versions.

## The files in this repository

The [src](./src/) directory contains
- Haskell and other sources
- subdirectory in [typetheory](./src/typetheory/) with generated parser and printer for the proof systems [Dedukti](https://deducteam.github.io/), Agda](https://wiki.portal.chalmers.se/agda/pmwiki.php), [Rocq](https://rocq-prover.org/), and [Lean](https://lean-lang.org/) 
- a translator from MathCore to Dedukti and vice-versa
- translations between MathCore and Informath
- file [BaseConstants.dk](./src/BaseConstants.dk) of logical and numeric operations assumed in most of the data examples, and correspoonding files for Agda, Rocq, and Lean
- file [baseconstants.dkgf](./src/baseconstants.dkgf), a symbol table for converting Dedukti constants in BaseConstants.dk to GF abstract syntax functions

The [test](./test/) directory contains
- some test data as `.dk`, `.tex`, and `.txt` files (see above)

The [grammars](./grammars) directory contains

- [MathCore](./grammars/MathCore.gf), the abstract syntax of a minimal CNL for mathematics
- [MathCoreEng](./grammars/MathCoreEng.gf), Fre, Ger, Swe - concrete syntaxes of MathCore 
- [MathExtensions(./grammars/MathExtensions.gf), an extension of MathCore with alternative expressions, and corresponding concrete syntaxes
- [VerbalConstants](./grammars/VerbalConstants.gf), lexicon of natural language mathematical concepts
- [SymbolicConstants](./grammars/SymbolicConstants.gf), lexicon of symbolic concepts in LaTeX.
- [Terms](./grammars/Terms.gf), grammar of formal notations, with a single concrete syntax [TermsLatex](./grammars/TermsLatex.gf)
- [UserExtensions](./grammars/UserExtensions.gf), user-definable extension modules, such as Naproche and NaturalDeduction
- [Utilities](./grammars/Utilities.gf), auxiliary functions and type synonyms used in other modules, also usable in user extensions
- [Informath](./grammars/Informath.gf), the top module that puts everything together

In addition to the above grammars, which are used in the actual runtime, there are directories that can be used as libraries for implementing new constants:
- [grammars/mathterms](./grammars/mathterms/), multilingual mathematics lexicon extracted from Wikidata
- [grammars/extraction](./grammars/extraction/), auxiliary grammars used for the extraction task and also imported in the lexicon modules
  
The [scripts](./test/) directory contains 
- Python scripts for various related tasks


## The structure of Informath

The structure of Informath is shown in the following picture:

![Informath](./doc/informath-dedukti-core.png)

The diagram has four kinds of arrowheads. Solid ones mean that the operation is a total function, giving exactly one result for every input (triangular arrowheads) or possibly many (diamond). Hollow arrowheads mean partial functions which can likewise give at most one result (triangular) or many results (diamond):

 - Conversions from Dedukti to Agda, Rocq, and Lean are partial, because Dedukti is more permissive than these formalisms.
 - Conversion from MathCore to Dedukti may fail because MathCore is more permissive than Dedukti; this is because we delegate dependent type checking to Dedukti.
 - Conversion from MathCore to Informath is one-to-many, and always results in at least one value, the MathCore expression itself.
 - Conversions from English and other natural languages to Informath may fail, because the input is not covered by the grammar. They can also give many results, because the grammar accepts ambiguity; the idea is that ambiguity is ultimately checked on semantic grounds in Dedukti.

Conversions between MathCore and Informath, and extending the Informath language itself, are the most open-ended parts of the project and hence the main research focus. 

Conversions from Dedukti to Agda, Coq, and Lean and back are mostly engineering (although tricky in some cases) that has to a large extent been done for the kind of code needed in Informath. Conversions from these type theories to Dedukti rely on already existing third-party tools. Those tools are not always up to date with the latest versions of the systems, but they have their own development teams that have goals independent of Informath.

## Processing in type theory

### Type checking in Dedukti

The type checking is based on the file [BaseConstants.dk](./src/BaseConstants.dk), which is meant to be extended as the project grows. This file type checks in Dedukti with the command
```
  $ dk check BaseConstants.dk
```
The example file [test/exx.dk](./src/test/exx.dk) assumes this file. As shown in `make demo`, it must at the moment be appended to the base file to type check:
```
$ cat BaseConstants.dk test/exx.dk >bexx.dk
$ dk check bexx.dk
```
Since this is cumbersome, we will need to implement something more automatic in the future. We also plan to use Dedukti for type selecting among ambiguous parse results by type checking, and Lambdapi (a syntactically richer version of Dedukti with implicit arguments) to restore implicit arguments.


### Generating other type theories

Each of Agda, Rocq, and Lean will be described below. A common feature to all of them are the conversion rules of constants stored in [BaseConstants.dk](./src/BaseConstants.dk), with the format as in
```
#CONV agda forall all
#CONV rocq forall All
#CONV lean forall All
```
The purpose of these conversions is to
- avoid clashes of the target systems' reserved words
- map Dedukti to standard libraries of these systems
- comply to the identifier syntax of each system

The last purpose might be better served by a generic conversion, but that remains to be done.

### Generating and type checking Agda

There a simple generation of Agda from Dedukti. At the moment, it is only reliable for generating Agda "postulates". The usage is
```
$ RunInformath -to-formalism=agda <file>
```
where the file can be either a .dk or a text file.
As shown by `make demo`, this process can produce valid Agda code:
```
$ RunInformath -to-formalixm=agda test/exx.dk >exx.agda
$ agda --prop exx.agda
```
The base file [BaseConstants.agda](./src/BaseConstants.agda) is accessed by an `open import` statement.

### Generating and type checking Rocq

Generation from Dedukti is similar to Agda, but type checking requires at the moment concatenation with [BaseConstants.v](BaseConstants.v):
```
$ RunInformath -to-formalism=rocq test/exx.dk >exx.v
$ cat BaseConstants.v exx.v >bexx.v
$ coqc bexx.lean
```
This should be made less cumbersome in the future.

### Generating and type checking Lean

Just like in Rocq, type checking requires at the moment concatenation with [BaseConstants.lean](BaseConstants.lean):
```
$ RunInformath -to-formalism=lean test/exx.dk >exx.lean
$ cat BaseConstants.lean exx.lean >bexx.lean
$ lean bexx.lean
```
This should be made less cumbersome in the future.

# ToDo

- improve conversions from Dedukti to other proof systems in particular to guarantee type correctness
- extend the MathCore-Informath conversion
- investigate the possibility of a declarative, user-defined extension of MathCore-Informath conversion
- improve the concrete syntaxes of different languages by functor exceptions
- add concrete syntaxes to yet other natural languages
- extend the Informath language, in particular, with
     - proofs, in addition to theorems and definitions to proofs (complete in theory, but very rudimentary now)
     - wider coverage of BaseConstants.dk and the multilingual lexicon of math terms
