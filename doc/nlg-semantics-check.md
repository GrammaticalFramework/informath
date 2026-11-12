# The semantics of NLG: the empirical check and what it found

The specification of [nlg-and-semantics.md](nlg-and-semantics.md): if NLG
produces a tree t' from a MathCore tree t, the semantics of t' includes t.
`RunInformath -check-nlg-semantics <file>.dk` tests it (`src/NLGCheck.hs`).

## The check

For each judgement of the file and each of its MathCore trees t (the
pipeline's own, from at most four annotations), every NLG variant t' is
made, as with `-more-variants`, and attributed to the first NLG stage that
makes it: the passes up to `flatten`, `aggregate`, `insitu`, `varless`,
`collectivize`, `negated`/`implied`/`scoped`, the default output
(`letDeclared`), `bareQuantifiers`, `variations` and the `#NLG` rules (at most
200 each). Its semantics is computed (an error in it counts as no trees) and
the variant is

- **exact** if t is among the semantics of t';
- **equivalent** if it is so after both are normalized: coercions removed;
  `AllProp`, `ExistProp` unfolded as `addParenth` does; ∧ and ∨ as flat lists;
  antecedents uncurried; ∀ distributed over ∧; ¬∃ as ∀¬; the quantifiers and
  antecedents of a theorem's conclusion as hypotheses; hypotheses one
  variable or one proposition at a time, unlabelled, sorted; the variables
  of hypotheses renamed by their order in the conclusion, then by name, and
  bound ones by binder; an unnamed argument (`_h0`) as unnamed; `AdjKind` as
  the `SuchThatKind` the semantics makes of it, `PluralKindExp` as `KindExp`,
  nested collective functions folded to the left;
- **lost** otherwise.

The report counts the verdicts by stage, groups the lost variants by the
smallest subtrees where t and the semantics of t' differ, and shows examples.

## The corpora, before and after

| corpus | trees | variants | lost, first run | lost now |
|---|---:|---:|---:|---:|
| `test/exx.dk` | 55 | 359 | 7 | **0** |
| `test/top100.dk` | 98 | 653 | 0 | **0** |
| `test/sets.dk` | 124 | 615 | 28 | **0** |
| Mathlib sample (8 modules, 1,521 statements) | 5,109 | 87,537 | 47,698 | **45** |

The Mathlib sample is the statements of `Order.Defs.Unbundled`,
`Algebra.Group.Basic`, `Data.Set.Basic`, `GroupTheory.Coset.Basic`,
`Data.Nat.Prime.Defs`, `Topology.Basic`, `Algebra.Group.Hom.Defs` and
`Data.Finset.Card`, read with mathlib-informath's symbol tables and `#HIDE`
lines. Most of the first run's lost variants were findings of
nlg-and-semantics.md; some were gaps in the check's own normalization,
fixed first.

The Mathlib sample by stage, now:

| stage | variants | exact | equivalent | lost |
|---|---:|---:|---:|---:|
| up to flatten (passes 1-5) | 5,109 | 113 | 4,996 | 0 |
| aggregate | 3,881 | 30 | 3,851 | 0 |
| varless | 130 | 0 | 130 | 0 |
| collectivize | 20 | 0 | 20 | 0 |
| negated, implied, scoped | 98 | 4 | 94 | 0 |
| letDeclared (the default output) | 3,764 | 72 | 3,692 | 0 |
| variations | 74,535 | 750 | 73,740 | 45 |
| **all** | 87,537 | 969 | 86,523 | 45 |

## What was changed

**F7, "let G be a monoid"** (44,177 lost variants of the Mathlib sample). NLG
read a set variable with the assumption that it is a monoid as a variable of
the kind "monoid", and the semantics kept it so: G an element of the kind,
`Elem monoid`, instead of a set with the assumption `Proof (Monoid G)`. Now
NLG gives such a variable the kind `ClassKind monoid_Noun`, a new function of
`MathExtensions` that reads exactly as `NounKind` ("let G be a monoid", "for
every group G"), and the semantics gives it back as a set and the
assumption, in hypotheses (`classHypo`), in the argument kinds of
quantifiers, and in the quantifiers that `PostQuantProp` and Cooper storage
make (`classKinds`, the last pass of the semantics). As `ClassKind` records
what NLG did and is no reading of a text, the parser leaves out the trees
with it (`ParseInformath.hasNLGOnly`), and so does the parsing of symbol table
entries (`BuildConstantTable.parseExample`): without that, every noun kind
of a text or of a table wording got a second, spurious reading.

**F5, symbolic function types** (1,866). "$f \in A \rightarrow B$" and "let f be
a function from A to B" for `Elem A -> Elem B`: the semantics now makes a
function kind of `ExpKind (TermExp (FunTypeTerm a b))` again, also when a
`LetDeclarationHypo` makes it.

**F12, "A implies B" between propositions** (188). The formulas that
`implied` makes of propositions (`NotFormula`, `AndFormula`, `OrFormula`,
`IfFormula`, `IffFormula`, a propositional variable as `MacroFormula`, `\bot`)
are propositions again in the semantics (`formulaProp`).

**F13, collective functions** (9). "the sum of a, b and c" (`FunCCollExp`) has
a semantics, the function folded to the left.

The regression suite: 51 pass, 2 known failures; `insitu-dedukti` changed,
12 readings with an undefined `FunCCollExp` merged with the others.

## What is left

1. **Table wordings with an indefinite** (45 lost variants, all
   "for all monoids with an inverse operation G", `DivInvMonoid`). Where the
   wording of a constant contains "an X", the semantics reads the "an"
   compositionally in some positions, as "there exists an inverse operation
   such that G is a monoid with it", rather than as the name of the
   constant. A question about such wordings: keep them, or word them without
   a quantifier.
2. **Equivalent, not exact** (decided 12 November: the specification is
   taken modulo the normalization, and `ClassKind` is kept). 86,500 of the Mathlib variants are equivalent
   only modulo the normalization (F1-F4, F6, F8-F11, F14, F17 of
   nlg-and-semantics.md). Either the specification is taken modulo this
   normalization, or the semantics is made to undo them exactly (fold ∧ to
   the right as Dedukti does, not curry, restore coercions where
   `Dedukti2MathCore` puts them, keep the order of hypotheses). The first is
   simpler, and the normalization is then part of the specification, to be
   proved sound (each step preserves meaning).
3. **Not exercised by these corpora:** the `bareQuantifiers` stage (no
   `#IMPLICIT` types here), `#NLG` rules (none in these tables), `sum3dots`
   (F13), and the Godement variants (`-godement`, F16). They need corpora
   that have them: Edukera for implicit types, Godement for the rest.
4. **Not checked by the normalization:** coercions. The check removes them
   on both sides, so F3 (coercions restored only in some places) is not
   tested; the back-translation to Dedukti would test it.
