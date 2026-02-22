## Informath old news (see also Git history)

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
