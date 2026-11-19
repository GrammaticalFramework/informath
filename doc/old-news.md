## Informath old news (see also Git history)

9 September 2026: an updated version of the Informath paper in arXiv: [Symbolic Informalization: Fluent, Productive, Multilingual](https://arxiv.org/abs/2606.16893).

24 July 2026: New binary release, entitled [Informath-0.4](https://github.com/GrammaticalFramework/informath/releases/tag/informath-0.4).

22 July 2026: also verbal constants can now be defined with `#` variables, which enables permutations, as well as drops without the `#DROP` directive. See [the alternative symbol table](test/profiletest.dkgf) for examples.

10 July 2026: an experiment on porting the Informath grammar to new languages (Finnish and Czech) by help from Claude code. The process is documented in the [vibe directory](./doc/vibe/). NOTICE: these grammars are not yet an "official" part of Informath but need checking for known and still unknown bugs.

22 June 2026: macros in symbol tables now directly possible in entries of form `$...$`, e.g. `$#1 \mid #2$`, from which a macro is generated automatically; see test/profiletest.dkgf

15 June 2026: a lengthy paper on Informath in arXiv: [Symbolic Informalization: Fluent, Productive, Multilingual](https://arxiv.org/abs/2606.16893). At the moment, this is the most up-to-date description on some features of Informath.

9 June 2026: moved experimental and/or deprecated code to a separate repository, called [informath-experiments](https://github.com/aarneranta/informath-experiments).

6 May 2026: New binary release, entitled [Informath-0.3](https://github.com/GrammaticalFramework/informath/releases/tag/informath-0.3).

15 April 2026: Generalized production of synonyms, regulated by new flags `-synonyms=<int>` and `-symbolics=<int>`. Value 1 is reasonable for both of them: it means that just one verbal and one symbolic synonym is taken from the symbol table. Otherwise, the number of variations can now grow *very* large. Even without the `-variations` flag, this can make generation slow. In many demos accessible by `Makefile`, these flag values are used, but they can be overridden.

4 March 2026: Binary release, entitled [Informath-0.2](https://github.com/GrammaticalFramework/informath/releases/tag/informath-0.2).

22 February 2026: a very first attempt at a binary release, starting with MacOS. No need to install Haskell or GF. See below on "Using ready-made binaries".

19 February 2026: symbol tables are not generalized to contain full GF trees whose types are lexical categories. Such trees can also be parsed from quoted strings. In order to mark the boundaries between functions (which are no more single tokens), vertical bars `|` are now needed in symbol tables.See last chapter, "Symbol tables", for details.

19 February 2026: fixed a logical flaw in synonym treatment. Synonyms were generated from primary GF functions instead of the original Dedukti identifiers. But this is reliable only if the primary GF functions are unique - which cannot be guaranteed.

18 February 2026: support for adapting Informath to new data without changing the grammar. This will also enable a binary-only release of Informath, which does not require Haskell or GF. A description is found below in the "Symbol tables" section.

18 February 2026: deprecated `-previous`; it can be restored by checking out the GitHub version before the deprecation commit, but there should be no need.

16 February 2026: a much larger lexicon in [WikidataWords.gf](grammars/WikidataWords.gf). About 2000 words in English, less in other languages. Mostly extracted from WikiData. Also other lexica extracted from Naproche and Rijke's HoTT book via Universal Dependency parsing. This is a preparation for a version of Informath that can be adapted to new data without editing the grammar.

13 January 2026: divided this document to a basic part (this README file) and a new [Informath under the Hood document](./doc/informath-under-the-hood.md)).

13 January 2026: enable reading standard input; see `RunInformath -help`.

12 January 2026: a very rudimentary proof-of-concept [Dedukti implementation](./test/natural_deduction.dk) and [GF grammar](./grammars/NaturalDeduction.gf) for natural deduction proofs. You can test this with `make natural_deduction`.

19 December 2025: paper [Multilingual Autoformalization via Fine-tuning Large Language Models with Symbolically Generated Data](https://epub.jku.at/doi/10.35011/risc-proceedings-scml.1) appeared. Its focus is on the use of Informath in training data generation.

24 November 2025: The "-next" version is now default and "-previous" must be invoked with a flag. The previous version will be deprecated very soon, as all its functionality is available in the default version.
