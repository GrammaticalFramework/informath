# Autoformalizing a mathematical text in Rocq, read back by Informath

This directory is a project that formalizes a mathematical text (a book, its
chapters or sections) in Rocq, from the text itself. The Rocq is exported to
Dedukti by Informath's `scripts/rocq/rocq-export.py` and read back in English
(and other languages) by Informath, statements and tactic proofs alike. The
template is distilled from the formalization of Godement's *Algebra* in
`godement-index/rocq/`, which is worth reading for hard cases.

## The project

| file | what it is |
|---|---|
| `text/chN.txt` | the source, one sentence per line: `sent_id<TAB>sentence`, stable ids (from a CoNLL-U parse, or numbered by a script) |
| `coq/ChapterN.v` | the formalization of unit N (a chapter or a section), each file `Require Export`ing the one before |
| `dkgf/ChapterN.dkgf` | Informath's symbol table of unit N: how its constants are said in words and in notation |
| `config.py` | the project for `scripts/rocq` (`templates/config.py`): modules, table, tag, binder constants |
| `build.sh` | compile, export, read (`templates/build.sh`) → `index.json` |
| `names.py`, `pending.py` | duplicate names; what is still to be proved (`templates/`) |
| `NOTES.md` | the encoding decisions, the state, the open issues |

Always `export INFORMATH_ROOT=<the Informath checkout>`, and use only that
checkout; the build uses the newest RunInformath under its `.stack-work`.

## Conventions of the formalization

- **Tags.** A declaration that formalizes a sentence has
  `(* //TAG:<sent_id> <the sentence> *)` right before it, outside proofs,
  one tag per declaration (TAG is `config.py`'s `TAG`, e.g. `GOD1TEXT`).
  A footnote mark "(*)" is written "( * )"; "f*)" closes a Rocq comment.
  A sentence that says nothing formalizable is not tagged.
- **Objects.** Choose the foundation of the text and keep to it: for
  Godement, one type `Obj` of sets with membership `In`, his `=` Rocq's `=`,
  the sets he constructs as parameters characterized by axioms.
- **Structures** (group, ring, module over a ring, ...) are Records, whose
  fields are the set, the laws and the axioms in the text's order. The
  exporter makes them Informath structures ("let G be a group"); a project
  whose records are mere sets of objects says `RECORDS_AS_STRUCTURES = False`
  in `config.py`. Field names are global: make them unique (`groupLaw`).
  Never name anything `unit` or `tt`.
- **Proofs** by tactics, following the text's proof where it gives one, with
  the elementary tactics only, which Informath turns into proof lines:
  `intros apply exact split left right destruct exists rewrite unfold assert
  exfalso reflexivity symmetry pose proof`, bullets and braces. No
  automation (`auto`, `tauto`, `firstorder`, `congruence`, `;` chains): it
  leaves nothing to read. `lia` only for side goals about `nat`. Classical
  logic by `NNPP` and `classic`.
- **Statements not proved** are Axioms, marked: `(* AXIOM: ... *)` for what
  the text itself assumes (with its justification), `(* PENDING: what it
  needs *)` for what is not proved yet. After each unit, go through the
  PENDING ones again (the pending-pass skill).
- **Edge cases first.** Check every axiom at n = 0, the empty set, the zero
  ring before adding it: a false axiom makes everything provable. A
  statement of the text false at an edge case is proved in a corrected form,
  with a comment saying so; a necessary extra hypothesis gets a comment too.
- **Names** are unique across all units (`python3 names.py`); give the
  helpers of a unit a suffix (`...31`, `...32p`) when agents work in parallel.

## Symbol tables

- Test every new wording with a one-line table before adding it (the
  test-wording skill): one unparsable entry empties every reading of a build.
- Function-like nouns: "the N of #1", "the N of #1 and #2", "the N of #1 to
  #2", "the N of #1 at #2", "the N of #1 with respect to #2"; predicates:
  "#1 is A", "#1 is an N of #2", "#1 splits over #2". Failing: adverb +
  adjective, "at most", hyphens, unknown words, comparatives, plural subjects,
  four or more shown arguments.
- Directives: `#IMPLICIT Elem Obj` (the type of the text's objects: its
  variables are not introduced, its functions are "a function", "a
  predicate", "a family indexed by N"); `#NEGATION Eqt Neqt` (x ≠ y);
  `#EXCLUDE` (a structure kind); `#MACRO`.
- Another language: a table `dkgf-<lang>/ChapterN.dkgf` of the entries whose
  words differ from the translation of the English, compiled with the
  English one so that the other constants keep their English GF trees (see
  `godement-index/rocq/build-symboltables-fre.py`, `fre/`).

## Working with Claude

- **Commits.** Claude commits finished work, here and in Informath, with
  the `Co-Authored-By` trailer; the user pushes.
- **Parallel work.** One agent per unit (the formalize-chapter skill), or
  per pending pass, each in its own copy of the project (a scratch
  directory), with a distinctive name suffix. A merge (the merge-chapter
  skill) copies the unit back, sets its `Require Export`, runs `names.py`,
  `build.sh` and the page build, and commits. When several copies are
  merged, copy each one's own units only: a copy also holds stale versions
  of the others.
- **After each merge**, report: the counts (declarations, readings, proof
  texts, pending), what is PENDING and why, which statements were
  corrected, the problems with readings.
- **Faithfulness.** The type checker does not check that a statement says
  what the text says. Audit it (the faithfulness-audit skill): every tagged
  declaration against its sentence, then fix what the audit lists.
- **Memory.** Keep a running log of decisions and open issues in the
  project's memory file and in `NOTES.md`.
