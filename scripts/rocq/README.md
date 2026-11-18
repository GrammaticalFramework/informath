# Rocq (Coq) files read by Informath

These scripts translate the statements, definitions and tactic proofs of a
Rocq file to Informath's Dedukti dialect and read them with Informath: in
English, as formulas, and as proof texts. They do for Rocq what
mathlib-informath's `Export.lean` does for Lean. They were written for Jan
von Plato's articles on Gödel's independence proofs (the repository
[kg-heritage](https://github.com/aarneranta/kg-heritage), projects `kgah` and
`kgb`), and are kept here for any Rocq project.

## Use

A project is a directory with

* `config.py`: the Rocq modules and the symbol table, and optionally the
  project's tables (see `project.py`);
* the symbol table (`.dkgf`), with the wordings and notations of the project;
* `coq/`: the Rocq files, compiled with Rocq 9.3 (`rocq compile`); a
  `_CoqProject` there gives the load paths (its `-Q` and `-R` lines);
* `out/`: where the export goes.

In the project's directory (or with `ROCQ_INFORMATH_PROJECT` set to it):

    python3 $INFORMATH/scripts/rocq/rocq-export.py coq/M.v   # -> out/M.dk, .json, -tactics.json
    python3 $INFORMATH/scripts/rocq/rocq-index.py            # -> index.json

A single file can also be exported without a project (without `config.py`,
the settings have their defaults); the file is compiled first if its `.vo` is
missing or older:

    python3 $INFORMATH/scripts/rocq/rocq-export.py M.v -out=<dir>
    RunInformath -from-formalism=rocq M.v          # read at once (see RunInformath -help)

`index.json` has, for each declaration, its Rocq source and comment, its
Dedukti, its reading in English and as a formula (a definition by cases one
reading per case), and for a theorem the proof text of its tactic proof, all
steps and shortened (`../proof_shortening.py`). Pages and articles are made
from it by the project's own scripts; `../symbolic_proofs.py` writes proof
texts with formulas for articles, and `../proof_folds.py` shows them in HTML.

| | |
|---|---|
| `project.py` | the project: its directory and `config.py`, and the settings it may give: binder constants, arguments the readings leave out, record fields, section variables, sets defined as predicates, macros, side conditions |
| `rocqterm.py` | a parser of the terms Rocq prints with `Set Printing All`, and their translation to Informath's Dedukti |
| `rocq-export.py` | the export of a Rocq file: its declarations (`Check` and `Print` in Rocq), and its tactic proofs replayed with `Show` before and after each tactic |
| `rocq-tactic-units.py` | the steps of the tactic proofs as proof units for `RunInformath -tactic-proof` (mathlib-informath's `tactic-units.py` with Rocq's tactics) |
| `rocq-index.py` | the readings, formulas and proof texts → `index.json` |

## The translation

Rocq prints each declaration fully elaborated (`Set Printing All`: every
implicit argument, arrows as `forall (_ : A), B`), and `rocqterm.py`
translates it with the types of the constants and of the variables in scope,
which tell a proposition (`Proof A`) from a set (`Elem A`): a Rocq `forall`
over a proposition is an implication, over a set a quantifier. The standard
library's basic notions are mapped to Informath's base constants (`nat` to
`Nat`, `le` to `Leq`, `eq` to `Eq` or `Eqt`, the logical connectives,
lists, options, pairs, `S (S O)` to `2`). A `Fixpoint`, and a definition by
cases on an argument, becomes rewrite rules, one for each case, which
Informath reads one at a time. A `Record` is an Informath structure (as in
`test/algebra.dk`): `R : Structure.`, its constructor with the fields as
arguments into `Str R`, and the fields as selectors; a variable of type `R`
is `Str R` ("let G be a group"), and the symbol table should `#EXCLUDE R`
and word both `R` and the constructor. A file that requires other modules of
its directory (`Require Import M.`, transitively) is exported with their
constants known, but not exported again. A theorem ending in `Admitted.`
(with or without a proof begun) is exported with `"admitted": true` in its
JSON. Constants with a binder notation are
eta-expanded, so that their function arguments are a bound variable and a
body. A definition in a `Section` takes the section's variables as its
first arguments, as it does outside the section.

The tactic proofs are replayed in Rocq with the goals printed before and
after each tactic; the goals are translated in the same way, and
`rocq-tactic-units.py` makes proof units of them: assumptions (`intro`),
derived facts (`destruct`, `assert ... by`, `... in H`), claims (`assert`
with a proof of its own), what is enough to show, case splits and
induction, each with the tactic it comes from, and indented by the bullets
of the script.

Exported: `Lemma`, `Theorem`, `Corollary`, `Proposition`, `Fact`, `Remark`,
`Example`, `Definition`, `Instance`, `Fixpoint`, `Record`/`Structure`,
`Inductive`/`CoInductive`, `Axiom`, `Parameter`, `Conjecture`, also with
attributes and `Local`/`Global`/`Program` before them. Reported but not
exported: `Variable(s)`, `Hypothesis`, `Context`, `Let`, `Class`, `Module`,
mutual fixpoints and theorems, `CoFixpoint`, `Canonical`, `Coercion`.
Proofs are `Proof. ... Qed.` (or `Defined.`) with standard tactics and
bullets; ssreflect-style proofs are not classified well. Of the standard
library only the basic notions above are mapped: `Z`, `R`, `Q`, `Ensembles`
and the type classes are read under their own names unless the symbol table
words them.

## Compared with CoqInE

[CoqInE](https://github.com/Deducteam/CoqInE) also translates Rocq to
Dedukti, but for another purpose: CoqInE is for checking, these scripts are
for reading, as lean2dk and mathlib-informath's exporter are for Lean.

| | CoqInE | rocq-export.py |
|---|---|---|
| what it is | an OCaml plugin in Rocq | a Python script that calls Rocq |
| input | compiled `.vo` modules: the kernel terms, the standard library and other dependencies included | the `.v` file, through what Rocq prints with `Set Printing All` |
| output | a faithful encoding of Rocq's theory (sorts, universes, cumulativity, inductive types, `match`, `fix`), meant to type-check in Dedukti | Informath's reading dialect (`Set`, `Elem`, `Prop`, `Proof`, `forall`, `Leq`, ...), not meant to type-check |
| names and notions | Rocq's own, fully qualified | mapped to what a mathematician reads: `nat` is ℕ, `le` is ≤, technical arguments dropped |
| proofs | the proof terms (a `lia` step is a large arithmetic certificate) | the tactic script replayed step by step, giving the lines of a proof text |
| coverage | the whole calculus: modules, mutual inductives, universe polymorphism (in progress) | the parts of Rocq listed above |

CoqInE's output could replace the parser of `rocqterm.py` for statements,
but it would still need a translation to Informath's dialect, as its terms
are at the same kernel level, and it does not help with proof texts, which
come from the tactic steps. What it gives that these scripts do not is an
independent check of the Dedukti. CoqInE must be compiled with the OCaml of
the Rocq whose `.vo` files it reads.
