# Felix integration

Informath uses Felix as a library to parse Felix source into its surface AST.
[Felix2Informath](../src/Felix2Informath.hs) translates a supported fragment of
that AST into Informath trees for English generation.

This integration is experimental. The Stack project uses the current Felix
checkout at `../felix`; Informath does not promise compatibility with a
particular Felix release or corpus revision.

## Running

`NAPROCHE_LIB` selects the Felix library used while parsing:

```sh
make english_grammar
stack build
felix_library=$(cd ../felix/library && pwd)
INFORMATH_ROOT="$PWD" NAPROCHE_LIB="$felix_library" \
  stack exec -- RunInformath -from-felix "$felix_library/set.tex"
```

Generated statements are written to standard output in source order; the
diagnostic translation summary is written to standard error. Its format is not
a stable interface.

## Translation boundary

The integration produces `PresentationJmt` trees:

- `FormalPresentationJmt` wraps an ordinary `Jmt`, currently a Felix axiom.
- `ClaimPresentationJmt` represents a proofless Felix claim for generation.

Informath's ordinary parser still requests the `Jmt` start category, and its
formal and Dedukti APIs accept `Jmt`, not `PresentationJmt`. A presentation
claim therefore cannot enter those typed conversion paths. `PresentationJmt`
is generation-only by application policy; an explicit PGF caller can still
select it as a parse category.

The supported fragment covers the blocks, assumptions, propositions, and set
expressions needed by the current `set.tex` demo. Adjective definitions and
abbreviations update the lexical environment. Other definition and
abbreviation forms are counted and ignored, and proofs are counted and
omitted. Unsupported top-level blocks and unsupported syntax in emitted axioms
or claims fail with block context. Selected unknown operations remain visible
as symbolic applications.

## Tests

The default target is hermetic and does not read an external Felix corpus or
inherit `NAPROCHE_LIB`:

```sh
make felix_test
```

The opt-in demo target runs the current selected `set.tex` through the complete
CLI pipeline without pinning its contents or output:

```sh
NAPROCHE_LIB=../felix/library make felix_set_test
```
