# Autoformalization experiments with Informath

A template for formalizing a mathematical text written in LaTeX with Claude
Code: the text's definitions and results are formalized in Dedukti, checked
with `dk check`, given Informath symbol table entries that say each constant
in words and in notation as close to the text as possible, and presented as
an HTML index of the text's items, one page per chapter or section, with the
formalization and Informath's English and formula readings of each, the
proofs in English, and, if there is a CoNLL-U parse of the text, the
dependency trees of its sentences.

It packages the workflow of two earlier experiments, Godement's *Algebra*
(`godement-index`) and Rijke's *Introduction to Homotopy Type Theory*
(`hott-llm`).

`claude-rocq/` is the same for Rocq: the text formalized in Rocq, with
tactic proofs, exported to Dedukti and read back by Informath (see its
README).

## What is here

| | |
|---|---|
| `CLAUDE.md` | the instructions Claude follows: deliverables, the loop, conventions |
| `.claude/skills/symbol-table/` | how to write and check symbol table entries |
| `.claude/skills/grammar-extension/` | how to add missing words to Informath's grammar |
| `experiment.json` | the settings of an experiment: its source file, name and title, and optionally the Informath checkout and the CoNLL-U parse to use |
| `base/nd.dk` | natural deduction rules for Informath's base, used in proofs |
| `scripts/` | the pipeline (below) |
| `example/` | a small complete experiment: `divisibility.tex` and its formalization |

## Requirements

* Informath, built: `make` and `stack install` in the informath checkout, so
  that `RunInformath` is on the PATH, and `INFORMATH_ROOT` pointing to the
  checkout; or `"informath": "<checkout>"` in `experiment.json`, and `stack
  build` there, which the scripts then use without anything installed.
* Dedukti (`dk`, e.g. `opam install dedukti`) and Python 3.
* Optionally, a CoNLL-U parse of the text (`<name>.conllu`, or `"conllu"` in
  `experiment.json`), e.g. by UDPipe, for the dependency trees.
* Claude Code.

## Starting an experiment

    cp -r $INFORMATH_ROOT/autoformalize ~/experiments/mytext
    cd ~/experiments/mytext
    cp /path/to/mytext.tex .          # and the files it \inputs
    # edit experiment.json: "source": "mytext.tex", "name": "mytext", "title": ...
    scripts/init.sh                   # or: scripts/init.sh own (see CLAUDE.md)
    python3 scripts/extract-items.py
    claude

and then, for instance: *"Formalize section 1 of mytext.tex and show me the
results."* Claude works unit by unit, and builds `mytext-index.html`, its
pages in `pages/`, and the proofs in `proofs/` as it goes.

## The pipeline

Run in the experiment's directory (in `example/`, as `../scripts/...`):

    scripts/init.sh                         # base.dk, base.dkgf (once)
    python3 scripts/extract-items.py        # items.json: the numbered items of the source
    scripts/check.sh                        # dk check of base.dk, sec01.dk, ...
    python3 scripts/check-table.py          # the symbol tables against the formalization
    python3 scripts/build-informal.py       # English readings -> secNN-informal.tsv
    python3 scripts/build-informal.py -symbolic    # formulas -> secNN-symbolic.tsv
    python3 scripts/build-informal.py -closest     # the readings closest to the source,
                                                   # replacing the plain ones in secNN-informal.tsv
    scripts/build-proofs.sh                 # proofs/secNN.tex: the proofs in English
    python3 scripts/build-index.py          # <name>-index.html, pages/secNN.html

## The example

    cd example
    ../scripts/init.sh && python3 ../scripts/extract-items.py
    ../scripts/check.sh && python3 ../scripts/check-table.py
    python3 ../scripts/build-informal.py && python3 ../scripts/build-informal.py -symbolic
    python3 ../scripts/build-informal.py -closest
    ../scripts/build-proofs.sh && python3 ../scripts/build-index.py
    open divisibility-index.html

`divisibility.tex` has two sections, five numbered items and a remark;
`sec01.dk` and `sec02.dk` formalize the items with one proof (Theorem 1.2,
checked by Dedukti and shown in English in `pages/sec01.html`) and stated results,
and `sec01.dkgf` and `sec02.dkgf` word the two new notions.
