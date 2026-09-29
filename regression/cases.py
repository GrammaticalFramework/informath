"""The regression test cases of Informath.

Each case is a shell command, run in the root of the repository, whose standard
output is compared with a stored expected output (expected/<name>.out), and on
which some checks are made. In the command:

    {RUN}   the RunInformath binary under test
    {WORK}  a fresh directory for intermediate files of this case
    {ROOT}  the root of the repository (also the current directory)

Fields:
    name     unique name, also the name of the expected output file
    tier     demo   what `make demo` runs: the most important tests
             multi  the other languages (`make multidemo`, `make fulldemo`)
             dev    the other demos (`make devtest` and newer features)
             check  external checkers: Dedukti, Agda, Rocq, Lean
    cmd      the shell command
    what     one line on what the case tests; `make` names the Makefile target
    golden   compare stdout with the expected output (default True); False for
             cases whose output is not stable or not informative (the checkers)
    latex    the output is a LaTeX document, which must compile with pdflatex
    fewer    the number of output lines should not grow: each line is a
             failure (unparsed sentence, undefined constant); a new line is a
             regression, a vanished line an improvement
    timeout  seconds (default 300)
    skip     a regular expression: if the standard error matches it, the case is
             skipped (e.g. a language missing from the grammar)
    xfail    a known failure, with the reason: reported as XFAIL while it fails,
             and as XPASS when it no longer does, so that the xfail can be removed

LaTeX errors and undefined constants are counted, and the counts must not grow
beyond those stored with the expected outputs; so documents that already have
LaTeX errors are tested for not getting worse.
"""

CASES = [

# ---------------------------------------------------------------------------
# make demo

dict(name='exx-eng', tier='demo', make='demo',
     what='arithmetic statements from Dedukti to English',
     cmd='{RUN} -to-lang=Eng test/exx.dk'),

dict(name='exx-roundtrip', tier='demo', make='demo',
     what='the English of exx.dk parsed back to Dedukti',
     cmd='{RUN} -to-lang=Eng test/exx.dk >{WORK}/exx.txt && '
         '{RUN} -from-lang=Eng {WORK}/exx.txt | grep -v UN'),

dict(name='exx-roundtrip-failures', tier='demo', make='demo',
     what='the lines of the English of exx.dk that do not parse back',
     cmd='{RUN} -to-lang=Eng test/exx.dk >{WORK}/exx.txt && '
         '{RUN} -failures {WORK}/exx.txt',
     fewer=True),

dict(name='gflean-parse', tier='demo', make='demo',
     what='examples from Chartrand et al. parsed to Dedukti',
     cmd='{RUN} -from-lang=Eng test/gflean-data.txt | grep -v UN'),

dict(name='gflean-failures', tier='demo', make='demo',
     what='the examples from Chartrand et al. that do not parse',
     cmd='{RUN} -failures test/gflean-data.txt', fewer=True),

dict(name='exx-agda', tier='demo', make='demo',
     what='arithmetic statements from Dedukti to Agda',
     cmd='{RUN} -to-formalism=agda test/exx.dk'),

dict(name='exx-rocq', tier='demo', make='demo',
     what='arithmetic statements from Dedukti to Rocq',
     cmd='{RUN} -to-formalism=rocq test/exx.dk'),

dict(name='exx-lean', tier='demo', make='demo',
     what='arithmetic statements from Dedukti to Lean',
     cmd='{RUN} -to-formalism=lean test/exx.dk'),

dict(name='sets-latex', tier='demo', make='demo',
     what='set theory statements as a LaTeX document, with variations',
     cmd='{RUN} -to-latex-doc -variations -sampling=10 test/sets.dk', latex=True),

dict(name='top100-latex', tier='demo', make='demo',
     what='a sample of the 100 theorems as a LaTeX document',
     cmd='{RUN} -to-latex-doc -variations -to-lang=Eng -synonyms=1 -symbolics=1 '
         '-sampling=20 test/top100.dk', latex=True),

# ---------------------------------------------------------------------------
# make multidemo, make fulldemo

dict(name='exx-fre', tier='multi', make='multidemo',
     what='arithmetic statements from Dedukti to French',
     cmd='{RUN} -to-lang=Fre test/exx.dk'),

dict(name='exx-swe', tier='multi', make='multidemo',
     what='arithmetic statements from Dedukti to Swedish',
     cmd='{RUN} -to-lang=Swe test/exx.dk'),

dict(name='exx-ger', tier='multi', make='fulldemo',
     what='arithmetic statements from Dedukti to German',
     cmd='{RUN} -to-lang=Ger test/exx.dk',
     skip='not a valid language: Ger'),   # only in the grammar of `make full_grammar`

# ---------------------------------------------------------------------------
# make devtest, and features added later

dict(name='baseconstants-latex', tier='dev', make='baseconstants',
     what='the base constants as a LaTeX document',
     cmd='{RUN} -to-latex-doc -variations share/baseconstants.dk', latex=True),

dict(name='sets-single', tier='dev', make='sets',
     what='set theory statements, best readings only',
     cmd='{RUN} -to-latex-doc -variations -to-lang=Eng -synonyms=1 -symbolics=1 test/sets.dk',
     latex=True),

dict(name='sigma-latex', tier='dev', make='sigma',
     what='sums and integrals',
     cmd='{RUN} -variations -to-latex-doc test/sigma.dk', latex=True),

dict(name='embedded-sigma', tier='dev', make='embedded_sigma',
     what='Dedukti embedded in LaTeX',
     cmd='{RUN} -variations -nbest=3 test/sigma.dktex'),

dict(name='maps-latex', tier='dev', make='maps',
     what='maps theory with its symbol table',
     cmd='{RUN} -to-latex-doc -to-lang=Eng -add-symboltables=test/maps.dkgf test/maps.dk',
     latex=True),

dict(name='topo-latex', tier='dev', make='topo',
     what='topology with its symbol table',
     cmd='{RUN} -to-latex-doc -to-lang=Eng -add-symboltables=test/topo.dkgf test/topo.dk',
     latex=True),

dict(name='hott-latex', tier='dev', make='hott_demo',
     what='homotopy type theory with its own symbol table',
     cmd='{RUN} -variations -nbest=10 -to-latex-doc -symboltables=test/hott_demo.dkgf '
         'test/hott_demo.dk', latex=True),

dict(name='symboltable-check', tier='dev', make='symboltest',
     what='consistency of an example-based symbol table',
     cmd='{RUN} -base=test/symboltest.dk test/symboltest.dkgf'),

dict(name='symboltest-latex', tier='dev', make='symboltest',
     what='statements worded by an example-based symbol table',
     cmd='{RUN} -add-symboltables=test/symboltest.dkgf -variations -to-latex-doc test/symboltest.dk',
     latex=True),

dict(name='natural-deduction', tier='dev', make='natural_deduction',
     what='natural deduction proofs',
     cmd='{RUN} -to-latex-doc -symboltables=test/natural_deduction.dkgf '
         'test/natural_deduction_proofs.dk', latex=True),

dict(name='natural-deduction-rules', tier='dev', make='natural_deduction_rules',
     what='natural deduction rules',
     cmd='{RUN} -to-latex-doc -symboltables=test/natural_deduction.dkgf test/natural_deduction.dk',
     latex=True),

dict(name='proof-text', tier='dev', make='prooftextdemo',
     what='proofs as numbered lines of English',
     cmd='{RUN} -proof-text -base=test/natdedrules.dk -add-symboltables=test/natdrop.dkgf '
         'test/natdedproofs.dk', latex=True),

dict(name='mathcore-examples', tier='dev', make='mathcore_examples',
     what='MathCore text only',
     cmd='{RUN} -add-symboltables=test/natural_deduction.dkgf -mathcore test/mathcore_examples.dk'),

dict(name='mathextensions-examples', tier='dev', make='mathextensions_examples',
     what='parsing the MathExtensions examples to Dedukti',
     cmd='{RUN} -add-symboltables=test/natural_deduction.dkgf test/mathextensions_examples.dk'),

dict(name='naproche-translate', tier='dev', make='naproche',
     what='a Naproche document parsed and regenerated without Dedukti',
     cmd='{RUN} -translate -to-latex-doc -variations -synonyms=1 -symbolics=1 -to-lang=Eng '
         'test/naproche-zf-set.tex', latex=True),

dict(name='naproche-failures', tier='dev', make='naproche',
     what='the lines of the Naproche document that do not parse',
     cmd='{RUN} -failures test/naproche-zf-set.tex', fewer=True),

dict(name='naproche-interpret', tier='dev', make='interpret_naproche',
     what='a Naproche document through Dedukti and back',
     cmd='{RUN} test/naproche-zf-set.tex | grep -v "UN" | grep ":" >{WORK}/napzf.dk && '
         '{RUN} -to-latex-doc -variations -synonyms=1 -symbolics=1 -nbest=100 -to-lang=Eng '
         '{WORK}/napzf.dk', latex=True, timeout=600),

dict(name='semtest-failures', tier='dev', make=None,
     what='the sentences of semtest.tex that do not parse',
     cmd='{RUN} -failures test/semtest.tex', fewer=True),

dict(name='natural-failures', tier='dev', make=None,
     what='the sentences of natural.tex that do not parse',
     cmd='{RUN} -failures test/natural.tex', fewer=True),

dict(name='insitu-dedukti', tier='dev', make=None,
     what='in situ quantifiers parsed to Dedukti',
     cmd='{RUN} test/insitu.tex'),

dict(name='godement-syntax', tier='dev', make='godement_syntax',
     what="Godement's constructions parsed, with their MathCore readings",
     cmd='{RUN} -translate-core -to-latex-doc -add-symboltables=test/godement_syntax.dkgf '
         'test/godement_syntax.tex', latex=True),

dict(name='top100-symbolic-latex', tier='dev', make='top100latex',
     what='the 100 theorems in standard logical notation',
     cmd='{RUN} -to-latex-doc -to-symbolic-latex test/top100.dk', latex=True),

dict(name='top100-force-symbolic', tier='dev', make='top100symbolic',
     what='the 100 theorems, maximally symbolic',
     cmd='{RUN} -to-latex-doc -to-lang=Eng -force-symbolic test/top100.dk', latex=True),

dict(name='top100-godement-trees', tier='dev', make=None,
     what='Godement-style variants with their GF trees',
     cmd='{RUN} -trees -nbest=50 -godement test/top100.dk'),

dict(name='mini-matita', tier='dev', make='matita',
     what='a Matita library with an empty symbol table',
     cmd='{RUN} -symboltables=test/empty.dkgf test/mini-matita.dk'),

dict(name='fermat', tier='dev', make='fermat',
     what="Fermat's theorem with its symbol table",
     cmd='{RUN} -add-symboltables=test/fermat.dkgf -variations test/fermat.dk',
     xfail='RunInformath stops: conflicting profile information in "Fermat\'s theorem ."'),

dict(name='cartesian', tier='dev', make='cartesian',
     what='cartesian products with their symbol table',
     cmd='{RUN} -add-symboltables=test/cartesian.dkgf -variations test/cartesian.dk'),

dict(name='bind', tier='dev', make='bind',
     what='binders with their symbol table',
     cmd='{RUN} -add-symboltables=test/bind.dkgf -variations test/bind.dk',
     xfail='test/bind.dk does not parse: syntax error at line 1, column 36'),

# ---------------------------------------------------------------------------
# external checkers: the golden output is not compared, only the exit code

dict(name='dk-top100', tier='check', make='top100check', golden=False,
     what='the 100 theorems type-check in Dedukti',
     cmd='cat share/baseconstants.dk test/top100.dk >{WORK}/texx.dk && dk check {WORK}/texx.dk'),

dict(name='dk-sets', tier='check', make='sets', golden=False,
     what='the set theory statements type-check in Dedukti',
     cmd='cat share/baseconstants.dk test/sets.dk >{WORK}/sexx.dk && dk check {WORK}/sexx.dk'),

dict(name='dk-symboltest', tier='check', make='symboltest', golden=False,
     what='the symbol table test type-checks in Dedukti',
     cmd='dk check test/symboltest.dk'),

dict(name='dk-roundtrip', tier='check', make=None, fewer=True,
     what='the readings parsed back from the English of exx.dk that do not type-check',
     cmd='{RUN} -to-lang=Eng test/exx.dk >{WORK}/exx.txt && '
         '{RUN} -from-lang=Eng {WORK}/exx.txt | grep -v UN | grep ":" >{WORK}/back.dk && '
         'regression/tools/dkcheck-each.sh share/baseconstants.dk {WORK}/back.dk {WORK}'),

dict(name='dk-gflean', tier='check', make=None, fewer=True,
     what='the readings of the Chartrand et al. examples that do not type-check',
     cmd='{RUN} -from-lang=Eng test/gflean-data.txt | grep -v UN | grep ":" >{WORK}/back.dk && '
         'regression/tools/dkcheck-each.sh share/baseconstants.dk {WORK}/back.dk {WORK}'),

dict(name='agda-exx', tier='check', make='typechecks', golden=False,
     what='the Agda of exx.dk type-checks',
     cmd='printf "open import BaseConstants\\n\\n" >{WORK}/exx.agda && '
         '{RUN} -to-formalism=agda test/exx.dk >>{WORK}/exx.agda && '
         'cp -p share/BaseConstants.agda {WORK}/ && cd {WORK} && agda --prop exx.agda'),

dict(name='rocq-exx', tier='check', make='typechecks', golden=False,
     what='the Rocq of exx.dk type-checks',
     cmd='{RUN} -to-formalism=rocq test/exx.dk >{WORK}/exx.v && '
         'cat share/BaseConstants.v {WORK}/exx.v >{WORK}/bexx.v && cd {WORK} && coqc bexx.v'),

dict(name='lean-exx', tier='check', make='typechecks', golden=False,
     what='the Lean of exx.dk type-checks',
     cmd='{RUN} -to-formalism=lean test/exx.dk >{WORK}/exx.lean && '
         'cat share/BaseConstants.lean {WORK}/exx.lean >{WORK}/bexx.lean && lean {WORK}/bexx.lean'),

]
