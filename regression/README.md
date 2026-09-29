# Regression tests for Informath

`make demo` in the root shows the most important behaviour of Informath, and
until now it was checked by eyeballing its output. This directory turns that,
and the other demos of the Makefile, into tests that are run and compared
automatically:

    python3 regression/run.py              # all 49 cases, about 15 seconds
    make -C regression demo                # only what `make demo` runs

The result is a line per case and a report in `results/latest/report.md`, with
the diffs of whatever changed. The exit code is 0 when nothing changed.

## What is tested

The cases are listed in [cases.py](cases.py), each a shell command in the style
of the Makefile, in four tiers:

| tier | what | from |
|---|---|---|
| `demo` | English, Agda, Rocq and Lean from `exx.dk`; the English parsed back; the Chartrand et al. examples parsed; the LaTeX documents of `sets.dk` and `top100.dk` | `make demo` |
| `multi` | French, Swedish, German | `make multidemo`, `make fulldemo` |
| `dev` | the other demos: symbol tables, natural deduction, proof texts, Naproche, sums and integrals, HoTT, Matita, the Godement syntax, symbolic LaTeX, Godement-style variants, and the parse failures of the test corpora | `make devtest` and newer targets |
| `check` | the generated code checked by its own checker: Dedukti (`dk check`), Agda, Rocq (`coqc`), Lean; and every reading parsed back from English type-checked in Dedukti | `make top100check`, `make typechecks` |

Each case is checked in three ways.

1. **Its output is compared with the expected output** stored in
   `expected/<case>.out`. Any difference is shown as a diff, and the case is
   CHANGED until the difference is accepted.
2. **Checks that must hold:**
   - the command ends normally, within its time limit;
   - a LaTeX document compiles with pdflatex, with no more errors than stored;
   - no new *failure lines*. For the cases marked `fewer`, each output line is
     a failure (an unparsed sentence, an ill-typed reading), so a new line is a
     regression and a vanished line an improvement;
   - no more undefined constants (`UNDEFINED`, `UNRESOLVED`) than stored.
3. **Timing:** a case that takes more than twice its stored time is marked SLOW.
   Slowness has been a real problem, e.g. the English readback of big Godement
   statements.

So a case is PASS, CHANGED (review the diff), NEW (no expected output yet),
FAIL (a check failed), XFAIL (a known failure, still failing), XPASS (a known
failure that no longer fails) or SKIP (e.g. German, which is only in the grammar
of `make full_grammar`).

## The workflow

After a change to the grammar or the Haskell code:

    make english_grammar multi_grammar; stack build     # as usual
    python3 regression/run.py

- **Everything PASS:** nothing visible changed.
- **CHANGED:** read the diffs in the report. If the change is intended (a
  better wording, a new reading), accept it and commit the new expected outputs
  with the change:

      python3 regression/run.py --accept exx-eng sets-latex
      git add regression/expected

  `--accept` without case names accepts every case that was run.
- **FAIL:** something broke. The actual output, stderr and diff of each case
  are in `results/latest/<case>/`.

The runner tests the `RunInformath` of this checkout's `stack build`, with
`INFORMATH_ROOT` set to this checkout. So neither a binary on the PATH built
from another checkout, nor an `INFORMATH_ROOT` pointing elsewhere, can confuse
it. `--bin` tests another binary. It also warns when a grammar or Haskell file
is newer than the built grammar or binary.

## Adding a case

Add a `dict(...)` to `cases.py`, with a new name, a tier and the command, and run

    python3 regression/run.py --accept <name>

Look at `expected/<name>.out` before committing it, since it becomes the
standard. For outputs that list failures (e.g. `-failures`), mark the case
`fewer=True`. For a LaTeX document, mark it `latex=True`. For a known bug, give
`xfail='reason'`.

## What the first run found

The expected outputs are the behaviour of commit `7a0a687`, with its known
problems recorded as the baseline, so that they cannot get worse unnoticed.
These are the problems:

1. **Dedukti parsed back from English did not type-check** (fixed after the
   first run). Hypotheses came out as `n : Nat ->` where Dedukti needs
   `n : Elem Nat ->`. `addCoercions` in `src/Informath2MathCore.hs` toggled
   the `Elem` coercion instead of adding it once, and the parser applies the
   semantics twice (`processLatexLine` gives the core tree to `gjmt2dedukti`,
   which runs `ext2core` again). With the fix, 12 of the 15 statements of the
   `exx.dk` round trip have a well-typed reading, against 7 before, and 12 of the
   19 readings of the Chartrand et al. examples type-check, against none. The
   rest were spurious readings (see 2) and `prop120`, `prop130`, whose
   "$a \times b$" was read only as the cartesian product.
   That was fixed next: the formulas in `$...$`, parsed apart from the text,
   kept only their first parse, which for `\times` was `cartesian`. Now all
   their readings are kept (at most 50 combinations per sentence), and a
   grammar symbol like `\times` is no longer accepted as a user macro. Every
   statement of the `exx.dk` round trip now has its correct reading; the one
   that `dk-roundtrip` still lists for `prop140` is only ill-typed because
   `sameParity` is defined in `test/exx.dk` and not in the base constants.
2. **Spurious readings:** "$n + 1$" is parsed both as `plus` and as
   `vectorPlus`, whose notation is also `+` (`share/baseconstants.dkgf`), and
   "$=$" also as `equalset`. The type check rules them out (`dk-roundtrip`,
   `dk-gflean`), but `make demo` shows them all. Since all readings of the
   formulas are kept, they multiply: `prop100` has 24 readings, of which one
   type-checks, and the Naproche round trip (`naproche-interpret`) grew from 55
   to 130 pages. Filtering the parser's readings by type-checking them would
   remove the spurious ones.
3. **`make fermat` stops** with *conflicting profile information in "Fermat's
   theorem ."* (XFAIL `fermat`).
4. **`make bind` stops**: `test/bind.dk` does not parse, *syntax error at line
   1, column 36* (XFAIL `bind`).
5. **Some LaTeX documents have errors** that pdflatex's batch mode hides:
   `natural-deduction-rules` (77 errors), `natural-deduction` (2, *Lonely
   \item*), `naproche-translate` (141, *Undefined control sequence*),
   `godement-syntax` (16, *Double subscript*, from fresh variables like
   `_x_0`). The PDFs are produced, but they are wrong where the errors are.
6. **`make typechecks` calls `rocq compile`**, which is not installed here;
   `coqc` is, and the regression test uses it.
7. `insitu-dedukti` has 25 `UNRESOLVED` constants in its Dedukti. This may be
   intended, but it is now counted.

## Suggestions

- **Hook it into the root Makefile**, e.g.

      regression:
      	python3 regression/run.py
      regression-demo:
      	python3 regression/run.py --tier demo

  and make `devtest` call it, since it is faster than `make demo` (no PDF
  viewer) and checks more.
- **Run it before each commit.** It takes 15 seconds. It could be a git
  pre-commit hook, or a GitHub Actions workflow on each push. For CI the
  binary and grammars have to be built first, and `dk`, `agda`, `coqc` and
  `lean` installed for the `check` tier, which can be left out with
  `--tier demo,multi,dev`.
- **Commit `expected/` with the code change that changes it.** Then a review of
  a commit shows both the code and its effect on the output.
- **More cases worth adding:**
  - the Godement formalization: a sample of `godement-index/*.dk` with its
    symbol tables, since it is the largest real use (heavy statements belong in
    a `slow` tier);
  - `-to-lang=Fre` and `Swe` for `sets.dk` and `top100.dk`, not only `exx.dk`;
  - Agda, Rocq and Lean type-checking of `sets.dk` and `top100.dk`;
  - the experimental grammars of `make next_grammar` (Finnish, Czech, Polish),
    as a tier of their own, where changes are expected.
- **Unit-level tests** would complement these end-to-end ones, e.g. GF treebank
  files (`gf -treebank`) that pin the linearization of chosen trees, or
  Haskell tests (`stack test`) for functions like `godementVariants`,
  `kindPred` and the macro naming.
