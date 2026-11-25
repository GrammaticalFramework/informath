# Lean files read by Informath

`lean-export.py` translates the declarations of a Lean 4 file to Informath's
Dedukti dialect, as `../rocq/rocq-export.py` does for Rocq and
`../agda/agda-export.py` for Agda. It works for any Lean file: unlike
mathlib-informath's exporter, it needs neither Mathlib nor a Lake project
of its own, only Lean.

    python3 $INFORMATH/scripts/lean/lean-export.py File.lean -out=<dir>
    # -> <dir>/File.dk, File.json, File-hide.dkgf, File-deps.dk, File-proofs.dk,
    #    File-tactics.json   (default <dir>: out)
    python3 $INFORMATH/scripts/lean/lean-tactic-units.py File <dir>
    # -> <dir>/File-tactic-units.json: the tactic proofs as proof units
    RunInformath -tactic-proof <dir>/File-tactic-units.json   # their proof texts

or, reading the result in English at once (see `RunInformath -help`):

    RunInformath -from-formalism=lean File.lean
    RunInformath -from-formalism=lean -export-dir=<dir> File.lean     # keeping the export
    RunInformath -from-formalism=lean File.lean -to-formalism=agda    # to Agda, via Dedukti
    RunInformath -from-formalism=lean File.lean -proof-text           # the proof texts of its theorems,
                                                                      # by terms and by tactics
    RunInformath -from-formalism=lean File.lean -to-html-doc -to-lang=Eng,Fre >File.html

In a project of many files (`godement-index/lean/`), `-project=<dir>`
names the directory of the project's `config.py`: the file's structures are
then exported as the Rocq exporter exports Records, as Informath structures
(`Group : Structure.`, the constructor `mkGroup` into `Str Group`, the
fields by their own names, `groupLaw`), with the constructor's arguments
named by `FIELD_VARIABLES`; and `../rocq/rocq-index.py`, with `FORMALISM =
'lean'` in `config.py`, reads all the files into the project's `index.json`.
A constant named as one of Informath's base constants gets an underscore
(`In` is `In_`).

## How it works

Lean elaborates the file: `Export.lean` is a Lean program, run with
`lean --run Export.lean File.lean <dir> <mapping>`, which processes the file
with Lean's own front end and translates the declarations it makes from the
environment, where every implicit argument is filled in. Nothing is compiled
or installed: `lean` (the version of `elan`'s default toolchain, or of the
project's `lean-toolchain`) runs it directly, in a couple of seconds for a
file that imports Lean's own libraries only. A file in a Lake project (a
`lakefile.lean` or `lakefile.toml` in its directory or above) is elaborated
with `lake env`, so that its imports, Mathlib say, are found; they must be
built, and the project's Lean must be one that `Export.lean` compiles with
(it is written for Lean 4.36).

## The translation

The dialect is the one of the other exports and of mathlib-informath:

* a binder `(x : α)` with `α : Type` is `(x : Elem α)`, one with `α : Prop`
  is `(h : Proof α)`, a type `(α : Type u)` is `(α : Set)`, and an instance
  `[C α]` is a hypothesis `(_ : Proof (C α))`; universe levels are dropped;
* in propositions, ∀ is `forall`, → `if`, ∃ `exists`, `=` is `Eq` on numbers
  and `Eqt` elsewhere, numerals are Informath's numerals, and the constants of
  `mapping.tsv` are Informath's base constants (`And` is `and`, `HAdd.hAdd` is
  `plus`, `LE.le` is `Leq`, `Nat.succ` is `succ`, ...), applied to their
  explicit arguments; a project adds its own with `-mapping=<file.tsv>`;
* any other constant keeps its Lean name, with `.` written `__` (`List.length`
  is `List__length`), and all its arguments but the instances; the implicit
  ones are listed in `File-hide.dkgf` (`#HIDE`), which hides them in the
  readings, as the source does;
* a theorem or an axiom is its statement; a definition by pattern matching or
  recursion is rewrite rules, one for each of its equation lemmas (Lean's own
  statement of each case); another definition gets its definiens;
* an inductive type is a `Set` (or a predicate) with its constructors, a
  structure also with its fields as functions (`Point__x`).

A theorem whose source gives its proof as a term (not `by` and tactics) gets
its proof term in `File-proofs.dk`, as mathlib-informath's exporter writes
them: a λ that proves an implication or a ∀ is its introduction rule (`ifI`,
`forallI`), a hypothesis applied is its elimination (`ifE`, `forallE`), and
Lean's logical rules (`And.intro`, `Or.elim`, `Exists.elim`, ...) are those of
Informath's `share/nd.dk` (the third column `all` of `mapping.tsv`), which
`RunInformath -proof-text` reads the proof with.

A theorem proved `by` tactics gets its steps in `File-tactics.json`, as
mathlib-informath's exporter writes them for Mathlib (`--tactics`): Lean
elaborates the file with info trees, and each tactic written directly in a
tactic sequence (of `by`, of a `·` case, of a nested `have ... := by`, of
the alternatives of `cases ... with` and `induction ... with`) is a step,
with its line, text, the theorem it proves, the main goal before it and the
goals after it, their hypotheses and targets in the same Dedukti.
`lean-tactic-units.py` (mathlib-informath's `tactic-units.py`, with the
additions of `../rocq/rocq-tactic-units.py`) compares the goals of each step
and makes it proof units, which `RunInformath -tactic-proof` reads: a new
hypothesis is assumed (`intro`, the case of `cases`, `by_cases`) or let be
(`obtain`, `rcases`), a new fact is stated (`have`, `simp at h`), a closed
goal is concluded ("by computation" for `omega`, "by rewriting" for `rw`,
...), a changed goal is what is enough to show (`apply`, `unfold`,
`constructor`), a split is "we reason by cases" or "by induction". A step
cites what its tactic applies: the theorems and axioms of the file by their
names ("By `double_eq`, ..."), the hypotheses made by earlier steps by their
labels ("By (h), ..."). `-from-formalism=lean -proof-text` and `-to-html-doc`
do all this, and `tests/Tactics.lean` (`make from_formalism_tests`) is a test.

Each declaration is preceded by `(; //LEAN:<name> ;)`. `File.json` lists them
with their Lean names, Dedukti names, kinds, docstrings, Lean's statements,
lines and source texts; `File-deps.dk` declares the constants that the file uses but
neither defines nor maps, with their types translated in the same way.

## Limits

* The Dedukti is for reading, not for checking: universes and instances are
  left out, and the patterns of the rules are what Lean's equation lemmas
  have.
* Tactic proofs are shortened and cited as Rocq's are
  (`../proof_units.py -lean`; `-proof-all-steps` gives every step); a step
  combined by `<;>` is one step; the steps that a tactic makes itself (`simp`, `omega`) are not seen,
  only what they leave; a step whose goals Export.lean cannot translate is
  "obvious". A hypothesis of the statement is not cited by its label, which
  the proof text does not show; lemmas of the libraries are cited only in the
  tactic column.
* Instances, the functions of derived instances (`deriving Repr`), recursors
  and other generated declarations are left out; mutual and nested inductive
  types are declared as the others, classes as structures.
* mathlib-informath's exporter knows more of Mathlib (given instances as
  named structures, classes as structure definitions, coercions); for Mathlib,
  it is the one to use.
