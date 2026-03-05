RUN  := RunInformath
OPEN := open  # pdf viewer command

# for binary_packages
ARCH := macos-arm
VERSION := 0.2

GF_FILES := $(wildcard grammars/*.gf)

lang=Eng

.PHONY: all usual Dedukti Agda Lean Rocq demo devdemo RunInformath

all: Dedukti Agda Rocq Lean english_grammar full_grammar RunInformath rootlink

english_grammar: share/InformathEng.pgf

RunInformath:
	stack install

devel: english_grammar RunInformath rootlink

rootlink:
	export INFORMATH_ROOT=$(CURDIR)

multi_grammar:
	cd grammars ; gf --make --probs=Informath.probs InformathEng.gf InformathSwe.gf InformathFre.gf ; mv Informath.pgf ../share/InformathFull.pgf

full_grammar:
	cd grammars ; gf --make --probs=Informath.probs InformathEng.gf InformathSwe.gf InformathFre.gf InformathGer.gf ; mv Informath.pgf ../share/InformathFull.pgf

share/InformathEng.pgf: $(GF_FILES)
	cd grammars ; gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Noun1,Noun2,NounC,Fam,Fam2,Adj,Adj2,Adj3,AdjC,AdjE,Fun,Fun2,FunC,Verb,Verb2,VerbC,Label,Compar,Const,Oper,Oper2,Environment,Prep,Dep,Dep2,DepC --probs=Informath.probs InformathEng.gf ; mv Informath.pgf ../share/InformathEng.pgf ; mv Informath.hs ../src


Dedukti:
	cd src/typetheory ; bnfc -m -p Dedukti --haskell-gadt Dedukti.bnf ; make

Agda:
	cd src/typetheory ; bnfc -m -p Agda --haskell-gadt Agda.bnf ; make

Lean:
	cd src/typetheory ; bnfc -m -p Lean --haskell-gadt Lean.bnf ; make

Rocq:
	cd src/typetheory ; bnfc -m -p Rocq --haskell-gadt Rocq.bnf ; make

clean:
	cd src/typetheory && \
	for dir in Agda Rocq Dedukti Lean; do \
		rm -rf "$$dir"/*; \
	done

cleangrammars:
	cd grammars && rm *.gfo *.pgf *.hs

demo:
	echo "## The first user demo, only requiring Informath and Latex"
	echo "## converting some simple arithmetic statements to English"
	$(RUN) -to-lang=Eng test/exx.dk
	echo "## parsing generated English with conversions back to Dedukti"
	$(RUN) -to-lang=Eng test/exx.dk >out/exx.txt
	$(RUN) -from-lang=Eng out/exx.txt
	echo "## parsing examples from Chartrand et al. with conversions to Dedukti"
	$(RUN) -from-lang=Eng test/gflean-data.txt | grep -v UN
	cat share/BaseConstants.dk test/exx.dk >out/bexx.dk
	echo "## converting some simple arithmetic statements to Agda"
	$(RUN) -to-formalism=agda test/exx.dk
	echo "## converting some simple arithmetic statements to Rocq"
	$(RUN) -to-formalism=rocq test/exx.dk
	echo "## converting some simple arithmetic statements to Lean"
	$(RUN) -to-formalism=lean test/exx.dk
	echo "# converting some set theory statements to LaTeX"
	$(RUN) -to-latex-doc -variations test/sets.dk >out/sets.tex
	echo "consider pdflatex out/sets.tex"
	echo "## creating and displaying a LaTeX document from a sample of 100 theorems"
	$(RUN) -to-latex-doc -variations -to-lang=$(lang) test/top100.dk >out/top100.tex
	cd out ; pdflatex top100.tex ; $(OPEN) top100.pdf

multidemo:
	make demo
	echo "## converting some simple arithmetic statements to French"
	$(RUN) -to-lang=Fre test/exx.dk
	echo "## converting some simple arithmetic statements to Swedish"
	$(RUN) -to-lang=Swe test/exx.dk

fulldemo:
	make multidemo
	echo "## converting some simple arithmetic statements to German"
	$(RUN) -to-lang=Ger test/exx.dk


devtest:
#	make fulldemo
	make top100check
	make typechecks
	make sets
	make sigma
	make naproche
	make interpret_naproche
#	make natural_deduction
	make symboltest


typechecks:
	echo "## converting some simple arithmetic statements to Agda"
	echo "open import BaseConstants\n\n" >out/exx.agda
	$(RUN) -to-formalism=agda test/exx.dk >>out/exx.agda
	cp -p share/baseconstants.agda out/
	echo "## checking the generated file in Agda"
	cd out ; agda --prop exx.agda
	echo "## converting some simple arithmetic statements to Rocq"
	$(RUN) -to-formalism=rocq test/exx.dk >out/exx.v
	cat share/baseconstants.v out/exx.v >out/bexx.v
	echo "## checking the generated file in Rocq"
	coqc out/bexx.v
	echo "## converting some simple arithmetic statements to Lean"
	$(RUN) -to-formalism=lean test/exx.dk >out/exx.lean
	echo "## checking the generated file in Lean"
	cat share/baseconstants.lean out/exx.lean >out/bexx.lean
	lean out/bexx.lean


top100:
	echo "## creating and displaying a LaTeX document from a sample of 100 theorems"
	$(RUN) -to-latex-doc -variations -to-lang=$(lang) test/top100.dk >out/top100$(lang).tex
	cd out ; pdflatex top100$(lang).tex ; $(OPEN) top100$(lang).pdf

top100check:
	echo "## type-checking the theorems in Dedukti"
	cat share/BaseConstants.dk test/top100.dk >out/texx.dk
	dk check out/texx.dk

top100single:
	echo "## generating only the best-ranked verbalizations of 100 theorems"
	$(RUN) -to-latex-doc -to-lang=$(lang) test/top100.dk >out/top100.tex
	cd out ; pdflatex top100.tex ; $(OPEN) top100.pdf
	cat share/BaseConstants.dk test/top100.dk >out/texx.dk
	dk check out/texx.dk

sets:
	echo "# checking some set theory statements and generating LaTeX"
	cat share/BaseConstants.dk test/sets.dk >out/sexx.dk
	dk check out/sexx.dk
	$(RUN) -variations -to-latex-doc -to-lang=$(lang) test/sets.dk >out/sets.tex
	cd out ; pdflatex sets.tex ; $(OPEN) sets.pdf

sigma:
	echo "# generating some expressions with sums and integrals"
	$(RUN) -variations -to-latex-doc test/sigma.dk >out/sigma.tex
	cd out ; pdflatex sigma.tex ; $(OPEN) sigma.pdf

symboltest:
	echo "# testing an example-based symbol table"
	dk check test/symboltest.dk
	RunInformath -base=test/symboltest.dk test/symboltest.dkgf
	RunInformath -add-symboltables=test/symboltest.dkgf -variations -to-latex-doc test/symboltest.dk

natural_deduction:
	$(RUN) -to-latex-doc -symboltables=test/natural_deduction.dkgf test/natural_deduction_proofs.dk >out/nd.tex
	cd out ; pdflatex nd.tex ; $(OPEN) nd.pdf

natural_deduction_rules:
	echo "## generating some natural deduction proofs"
	$(RUN) -to-latex-doc -symboltables=test/natural_deduction.dkgf test/natural_deduction.dk >out/ndr.tex
	cd out ; pdflatex ndr.tex ; $(OPEN) ndr.pdf

proof_units:
	echo "proof units have no Dedukti formalization so far"
#	$(RUN) -add-symboltables=test/proof_units.dkgf -to-lang=$(lang) test/proof_units.dk

mathcore_examples:
	$(RUN) -add-symboltables=test/natural_deduction.dkgf -mathcore test/mathcore_examples.dk

mathextensions_examples:
	$(RUN) -add-symboltables=test/natural_deduction.dkgf test/mathextensions_examples.dk

naproche:
	echo "## parsing and regenerating a Naproche document without going through Dedukti"
	$(RUN) -translate -to-latex-doc -variations -to-lang=$(lang) test/naproche-zf-set.tex >out/napzf.tex
	cd out ; pdflatex napzf.tex ; $(OPEN) napzf.pdf

interpret_naproche:
	echo "## parsing and regenerating a Naproche document going through Dedukti"
	$(RUN) test/naproche-zf-set.tex | grep -v "UN"  | grep ":" >tmp/napzf.dk
	$(RUN) -to-latex-doc -variations -nbest=100 -to-lang=$(lang) tmp/napzf.dk >out/inapzf.tex
	cd out ; pdflatex inapzf.tex ; $(OPEN) inapzf.pdf

baseconstants:
	cat share/BaseConstants.dk >tmp/baseconstants.dk

	$(RUN) -to-latex-doc -variations tmp/baseconstants.dk >out/baseconstants.tex
	cd out ; pdflatex baseconstants.tex ; $(OPEN) baseconstants.pdf

parallel:
	tail -150 share/BaseConstants.dk >tmp/parallel.dk
	cat test/exx.dk >>tmp/parallel.dk
	cat test/sets.dk >>tmp/parallel.dk
	cat test/top100.dk >>tmp/parallel.dk
	$(RUN) -parallel-data -variations -no-ranking tmp/parallel.dk >tmp/parallel-informath.jsonl

parallel-def:
	tail -150 share/BaseConstants.dk >tmp/parallel.dk
	cat test/exx.dk >>tmp/parallel.dk
	cat test/sets.dk >>tmp/parallel.dk
	$(RUN) -parallel-data  -variations -no-ranking -no-unlex -dedukti-tokens tmp/parallel.dk >tmp/parallel-def-train.jsonl

matita:
	$(RUN) -symboltables=test/empty.dkgf test/mini-matita.dk

gflean:
	echo "## parsing examples from Chartrand et al. with conversions to Dedukti"
	$(RUN) test/gflean-data.txt

fermat:
	$(RUN) -add-symboltables=test/fermat.dkgf -variations test/fermat.dk

cartesian:
	RunInformath -add-symboltables=test/cartesian.dkgf -variations  test/cartesian.dk

bind:
	RunInformath -add-symboltables=test/bind.dkgf -variations test/bind.dk


binary_packages:
	cp -p `which RunInformath` tmp/
	cp -p share/InformathEng.pgf share/InformathFull.pgf tmp/
	cd tmp ; strip RunInformath ; tar cvfz RunInformath-$(VERSION)-$(ARCH).tgz RunInformath ; tar cvfz Informath-grammars-$(VERSION).tgz InformathEng.pgf InformathFull.pgf
	ls -l tmp/*.tgz


