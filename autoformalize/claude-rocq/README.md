# Autoformalization in Rocq with Claude Code

A template for formalizing a mathematical text in Rocq with Claude Code,
from the text itself, with the result read back by Informath: statements in
English (and other languages) and as formulas, tactic proofs as proof texts,
each declaration tied to the sentence it formalizes. It is the Rocq
counterpart of the Dedukti template one directory up (`../CLAUDE.md`), and
packages the workflow of the formalization of Godement's *Algebra*
(`godement-index/rocq/`: 37 sections, 5,363 declarations, 4,483 tactic
proofs, made in about a day with Claude Code and some 50 agents; see its
`doc/godement-rocq.pdf` and `COMPARISON.md`).

| | |
|---|---|
| `CLAUDE.md` | the instructions Claude follows: the project, the conventions, how to work |
| `.claude/skills/formalize-chapter/` | one unit of the text, by an agent in a copy of the project |
| `.claude/skills/pending-pass/` | proving what was left PENDING, once what it needs is there |
| `.claude/skills/merge-chapter/` | merging an agent's work, rebuilding, committing |
| `.claude/skills/test-wording/` | testing a symbol table entry before adding it |
| `.claude/skills/faithfulness-audit/` | checking each statement against its sentence |
| `templates/config.py`, `build.sh` | the project for Informath's `scripts/rocq` |
| `templates/names.py`, `pending.py` | duplicate names; what is still to be proved |
| `templates/dump.py` | what the faithfulness audit compares, per unit |

## Starting a project

    mkdir myproject && cd myproject
    cp $INFORMATH_ROOT/autoformalize/claude-rocq/CLAUDE.md .
    cp -r $INFORMATH_ROOT/autoformalize/claude-rocq/.claude .
    cp $INFORMATH_ROOT/autoformalize/claude-rocq/templates/* .
    mkdir coq dkgf text

Put the source's sentences in `text/` (a CoNLL-U parse with `# sent_id`
and `# text` lines, named in `config.py`, and per unit `text/chN.txt` with
`sent_id<TAB>sentence` lines for the agents), set `TAG` in `config.py`,
and ask Claude to formalize the first unit. Requirements: Rocq 9 (`rocq`),
Python 3, and Informath built (`stack build` in the checkout that
`INFORMATH_ROOT` names; `build.sh` uses the newest RunInformath there).

Index pages, a French version and articles are built by the project's own
scripts; `godement-index/version-1/build-index.py -rocq` and
`godement-index/rocq/build-pages-fre.py` are examples.
