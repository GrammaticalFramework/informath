# Agda files read by Informath

`agda-export` translates the definitions of Agda modules to Informath's
Dedukti dialect, as `../rocq/rocq-export.py` does for Rocq, and
`../rocq/rocq-index.py` reads them in English and as formulas. It was
written for Rijke's Agda formalization of his *Introduction to Homotopy Type
Theory* (the HoTT-Intro library), in the repository hott-llm (`version-2`).

It has two parts:

* `Main.hs`: an Agda backend (Agda 2.8.0 as a library) that writes the
  definitions of each module, as Agda's type checker has them, as JSON
  (`out/agda/<M>.json`): the type, and the clauses (telescope, patterns,
  body), constructors or fields, with every implicit argument and the binder
  names. Build it once with `cabal build agda-export` in this directory; the
  first build compiles the Agda library (about 11 minutes with GHC 9.6.7).
* `agda-export.py` and `agdaterm.py`: run the backend on the project's
  modules and translate the JSON to Dedukti.

## Use

A project is a directory with `config.py` (see `../rocq/project.py`):

    AGDA_ROOT = '~/claude/hott-agda/Agda'         # the Agda library (its .agda-lib)
    MODULES = ['book.05-identity-types', ...]      # the modules to export
    AGDA_TABLE = 'hott.dkgf'                       # the symbol table, #k = explicit arguments
    TABLE = 'out/table.dkgf'                       # made from it, #k = all arguments

In the project's directory:

    python3 $INFORMATH/scripts/agda/agda-export.py   # -> out/<M>.dk, .json, -hide.dkgf, out/table.dkgf
    python3 $INFORMATH/scripts/rocq/rocq-index.py    # -> index.json

A single file can also be exported without a project: its module's root is
its directory less the qualifiers of its `module` line:

    python3 $INFORMATH/scripts/agda/agda-export.py M.agda -out=<dir>
    RunInformath -from-formalism=agda M.agda       # read at once (see RunInformath -help)

## The translation

The encoding is shallow: a type is a `Set`, its elements are `Elem A`, a
universe `UU l` is `Set` itself, and universe levels are dropped. A function
type is a Dedukti arrow in a binder, and in the position of a type
`Pi A (x => B)` or `Arr A B`. A definition by pattern matching is rewrite
rules, one for each clause; one clause of variables is a definiens; a data
type is declared with its constructors. Agda leaves the patterns of leading
implicit arguments out of some clauses (`pr1 (pair a _) = a`); they are
restored from the clause's telescope.

Agda's implicit arguments are kept in the Dedukti, and the readings leave
them out, as the source does. The symbol table is written with `#k` counting
the explicit arguments only (`concat : $#1 \cdot #2$`), and `agda-export.py`
renumbers it to count all arguments (`$#5 \cdot #6$`), as Informath's
argument profiles do; a constant without an entry gets one made of its
Agda name and its explicit arguments, `\mathsf{left\text{-}unit\text{-}law\text{-}add}_{\mathbb{N}}(#1)`.
`#{k}` refers to the k'th argument counting all of them, implicit ones too,
for wordings that mention an implicit argument: `center : "the center of
contraction of #{1}"`, where #{1} is the type $A$ of `center : {A : UU l} →
is-contr A → A`. A constant marked `#VERBAL` whose entry has no symbolic form
gets the generated one added, for use inside formulas.
(Informath's `#HIDE` would hide them in terms, but not in the head of a
declaration, which Informath builds from the declaration's hypotheses.)
Anonymous binders (`_`) are named by their types: `p`, `q` for
identifications, `n`, `m` for natural numbers, `f`, `g` for functions. The
names come from a table that a project extends in its `config.py`, keyed by
the short Agda name of the type (or `function`, `type`, `''` for the rest),
each a string of one-letter names or a list:

    VARIABLE_NAMES = {'ℕ': 'nmkl', 'ℤ': 'klmn', 'Id': 'pqrs'}

The same table names the parts of the cases in the proof texts (`inl
(succ n)`), where a part that is split further, and so does not appear in
the cases, gets a neutral name (`x`).

The comments of the source that name its numbered items (`-- Definition
5.1.1`) and sections (`-- Section 5.2 ...`) are kept in `out/<M>.json`, so
that a project can put each definition under the item of its text.

## Limits

* The Dedukti is for reading, not for checking: a constructor's
  applications leave out its parameters, as Agda's internal terms do, and
  levels are dropped.
* Records, copatterns, `with`-functions and pattern-matching lambdas are
  exported only as declarations or not shown; cubical features are not
  handled.
* Proof texts are made for the theorems (with `PROPOSITIONS = ['Id']` in
  `config.py`, an identification is a proof) defined by one clause of
  variables or by pattern matching (`agdaproof.py`): each split becomes the
  data type's generated induction rule, or its case analysis where no
  recursive call needs an induction hypothesis (`out/eliminators.dk`:
  `ind_D_P`, `cases_D_P`), with a motive; recursive calls become induction
  hypotheses; type definitions such as `ℤ = coprod ℕ (coprod unit ℕ)` are
  unfolded. An argument that depends on the split one, or that a recursive
  call changes, is generalized over in the motive ("for all y, if Eq(x, y)
  then x = y"), introduced in each case and eliminated after; the induction
  hypothesis is then the generalized statement, applied to the call's own
  arguments. A case that no clause covers is impossible: an argument's type
  computes to the empty type there (Eq-ℕ 0 (succ y)), which is eliminated.
  The proof terms are in `out/<M>-proofs.dk`, read by `rocq-index.py`. A
  definition whose recursive calls are not induction hypotheses, or whose
  split argument has indices that are not variables, keeps its rewrite rules
  (`AGDAPROOF_DEBUG=1` says why). In HTML,
  `proof_folds.inductions` collapses an induction with a generalized motive
  to one line, "by path induction on $p$", which ⊕ unfolds.
