RUN  := RunInformath
OPEN := open  # pdf viewer command

# Some colors to improve the readability
lightgreen='\e[1;32m'
neutral='\e[0;m'

# for binary_packages
ARCH := macos-arm
VERSION := 0.3

GF_FILES := $(wildcard grammars/*.gf)

lang=Eng
synonyms=1
symbolics=1
sampling=20

.PHONY: all usual Dedukti Agda Lean Rocq demo devdemo RunInformath

all: Dedukti Agda Rocq Lean english_grammar full_grammar RunInformath rootlink

english_grammar: share/InformathEng.pgf

my_grammar:
	cd grammars ; gf --make --probs=Informath.probs InformathEng.gf InformathFre.gf next/InformathCze.gf ; mv Informath.pgf ../share/InformathFull.pgf


RunInformath:
	stack install

devel: english_grammar RunInformath rootlink

rootlink:
	export INFORMATH_ROOT=$(CURDIR)

multi_grammar:
	cd grammars ; gf --make --probs=Informath.probs InformathEng.gf InformathSwe.gf InformathFre.gf ; mv Informath.pgf ../share/InformathFull.pgf

full_grammar:
	cd grammars ; gf --make --probs=Informath.probs InformathEng.gf InformathSwe.gf InformathFre.gf InformathGer.gf ; mv Informath.pgf ../share/InformathFull.pgf

next_grammar:
	cd grammars ; gf --make --probs=Informath.probs InformathEng.gf next/InformathFin.gf next/InformathCze.gf next/InformathPol.gf ; mv Informath.pgf ../share/InformathFull.pgf

share/InformathEng.pgf: $(GF_FILES)
	cd grammars ; gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Noun1,Noun2,Noun3,NounC,Fam,Fam2,Adj,Adj2,Adj3,AdjC,AdjE,Fun,Fun2,FunC,Verb,Verb2,VerbC,Label,Compar,Const,Oper,Oper2,Environment,Prep,Dep,Dep2,DepC --probs=Informath.probs InformathEng.gf ; mv Informath.pgf ../share/InformathEng.pgf ; mv Informath.hs ../src


Dedukti:
	cd src/typetheory ; bnfc -m -p Dedukti --haskell-gadt Dedukti.bnf ; make

Agda:
	cd src/typetheory ; bnfc -m -p Agda --haskell-gadt Agda.bnf ; make

Lean:
	cd src/typetheory ; bnfc -m -p Lean --haskell-gadt Lean.bnf ; make

Rocq:
	cd src/typetheory ; bnfc -m -p Rocq --haskell-gadt Rocq.bnf ; make


clean-typetheory:
	cd src/typetheory && \
	for dir in Agda Rocq Dedukti Lean; do \
		rm -rf "$$dir"/*; \
	done
	rm src/typetheory/Makefile
	rm src/typetheory/Makefile.bak

clean-grammars:
	rm grammars/*.gfo
	rm grammars/extraction/*.gfo
	rm src/Informath.hs
	rm share/*.pgf

clean-full:
	make clean-typetheory
	make clean-grammars
	rm stack.yaml.lock
	rm -rf .stack-work


demo:
	echo "${lightgreen}## The first user demo, only requiring Informath and Latex${neutral}"
	echo "${lightgreen}## converting some simple arithmetic statements to English${neutral}"
	$(RUN) -to-lang=Eng test/exx.dk
	echo "${lightgreen}## parsing generated English with conversions back to Dedukti${neutral}"
	$(RUN) -to-lang=Eng test/exx.dk >out/exx.txt
	$(RUN) -from-lang=Eng out/exx.txt | grep -v UN
	echo "${lightgreen}## parsing examples from Chartrand et al. with conversions to Dedukti${neutral}"
	$(RUN) -from-lang=Eng test/gflean-data.txt | grep -v UN
	cat share/BaseConstants.dk test/exx.dk >out/bexx.dk
	echo "${lightgreen}## converting some simple arithmetic statements to Agda${neutral}"
	$(RUN) -to-formalism=agda test/exx.dk
	echo "${lightgreen}## converting some simple arithmetic statements to Rocq${neutral}"
	$(RUN) -to-formalism=rocq test/exx.dk
	echo "${lightgreen}## converting some simple arithmetic statements to Lean${neutral}"
	$(RUN) -to-formalism=lean test/exx.dk
	echo "${lightgreen}# converting some set theory statements to LaTeX${neutral}"
	$(RUN) -to-latex-doc -variations -sampling=10 test/sets.dk >out/sets.tex
	echo "${lightgreen}consider pdflatex out/sets.tex${neutral}"
	echo "${lightgreen}## creating and displaying a LaTeX document from a sample of 100 theorems${neutral}"
	$(RUN) -to-latex-doc -variations -to-lang=$(lang) -synonyms=$(synonyms) -symbolics=$(symbolics) -sampling=20 test/top100.dk >out/top100.tex
	cd out ; pdflatex top100.tex ; $(OPEN) top100.pdf

multidemo:
	make demo
	echo "${lightgreen}## converting some simple arithmetic statements to French${neutral}"
	$(RUN) -to-lang=Fre test/exx.dk
	echo "${lightgreen}## converting some simple arithmetic statements to Swedish${neutral}"
	$(RUN) -to-lang=Swe test/exx.dk

fulldemo:
	make multidemo
	echo "${lightgreen}## converting some simple arithmetic statements to German${neutral}"
	$(RUN) -to-lang=Ger test/exx.dk


devtest:
	make multidemo
	make baseconstants
	make top100check
	make typechecks
	make sets
	make sigma
	make naproche
	make interpret_naproche
	make natural_deduction
	make symboltest


typechecks:
	echo "${lightgreen}## converting some simple arithmetic statements to Agda${neutral}"
	echo "open import BaseConstants\n\n" >out/exx.agda
	$(RUN) -to-formalism=agda test/exx.dk >>out/exx.agda
	cp -p share/baseconstants.agda out/
	echo "${lightgreen}## checking the generated file in Agda${neutral}"
	cd out ; agda --prop exx.agda
	echo "${lightgreen}## converting some simple arithmetic statements to Rocq${neutral}"
	$(RUN) -to-formalism=rocq test/exx.dk >out/exx.v
	cat share/baseconstants.v out/exx.v >out/bexx.v
	echo "${lightgreen}## checking the generated file in Rocq${neutral}"
	rocq out/bexx.v
	echo "${lightgreen}## converting some simple arithmetic statements to Lean${neutral}"
	$(RUN) -to-formalism=lean test/exx.dk >out/exx.lean
	echo "${lightgreen}## checking the generated file in Lean${neutral}"
	cat share/baseconstants.lean out/exx.lean >out/bexx.lean
	lean out/bexx.lean


top100:
	echo "${lightgreen}## creating and displaying a LaTeX document from a sample of 100 theorems${neutral}"
	$(RUN) -to-latex-doc -variations -to-lang=$(lang) -sampling=$(sampling) -synonyms=$(synonyms)  -symbolics=$(symbolics) test/top100.dk >out/top100$(lang).tex
	cd out ; pdflatex top100$(lang).tex ; $(OPEN) top100$(lang).pdf

top100verbal:
	echo "${lightgreen}## creating and displaying a LaTeX document from a sample of 100 theorems with a parsed symboltable${neutral}"
	$(RUN) -to-latex-doc -variations -to-lang=$(lang) -synonyms=$(synonyms) -symboltables=test/verbalconstants.dkgf -symbolics=$(symbolics) test/top100.dk >out/top100$(lang).tex
	cd out ; pdflatex top100$(lang).tex ; $(OPEN) top100$(lang).pdf

top100profile:
	echo "${lightgreen}## creating and displaying a LaTeX document from a sample of 100 theorems with a parsed symboltable with profiles${neutral}"
	$(RUN) -to-latex-doc -variations -to-lang=$(lang) -synonyms=$(synonyms) -symboltables=test/profileconstants.dkgf -symbolics=$(symbolics) -sampling=$(sampling) test/top100.dk >out/top100$(lang).tex
	cd out ; pdflatex top100$(lang).tex ; $(OPEN) top100$(lang).pdf

top100check:
	echo "${lightgreen}## type-checking the theorems in Dedukti${neutral}"
	cat share/BaseConstants.dk test/top100.dk >out/texx.dk
	dk check out/texx.dk

top100single:
	echo "${lightgreen}## generating only the best-ranked verbalizations of 100 theorems${neutral}"
	$(RUN) -to-latex-doc -to-lang=$(lang) test/top100.dk >out/top100.tex
	cd out ; pdflatex top100.tex ; $(OPEN) top100.pdf
	cat share/BaseConstants.dk test/top100.dk >out/texx.dk
	dk check out/texx.dk

sets:
	echo "${lightgreen}# checking some set theory statements and generating LaTeX${neutral}"
	cat share/BaseConstants.dk test/sets.dk >out/sexx.dk
	dk check out/sexx.dk
	$(RUN) -variations -to-latex-doc -to-lang=$(lang) -synonyms=$(synonyms)  -symbolics=$(symbolics) test/sets.dk >out/sets.tex
	cd out ; pdflatex sets.tex ; $(OPEN) sets.pdf

maps:
	echo "${lightgreen}# checking some maps theory statements and generating LaTeX${neutral}"
	cat share/BaseConstants.dk test/maps.dk >out/mapsx.dk
	dk check out/mapsx.dk
	$(RUN) -to-latex-doc -to-lang=$(lang) -add-symboltables=test/maps.dkgf test/maps.dk >out/maps.tex
	cd out ; pdflatex maps.tex ; $(OPEN) maps.pdf

topo:
	echo "${lightgreen}# checking some topology statements and generating LaTeX${neutral}"
	dk check test/topo.dk
	$(RUN) -to-latex-doc -to-lang=$(lang) -add-symboltables=test/topo.dkgf test/topo.dk >out/topo.tex
	cd out ; pdflatex topo.tex ; $(OPEN) topo.pdf

sigma:
	echo "${lightgreen}# generating some expressions with sums and integrals${neutral}"
	$(RUN) -variations -to-latex-doc test/sigma.dk >out/sigma.tex
	cd out ; pdflatex sigma.tex ; $(OPEN) sigma.pdf

embedded_sigma:
	echo "${lightgreen}# generating some expressions with sums and integrals in embedded tex${neutral}"
	$(RUN) -variations -nbest=3 test/sigma.dktex >out/emsigma.tex
	cd out ; pdflatex emsigma.tex ; $(OPEN) emsigma.pdf

hott_demo:
	echo "${lightgreen}# generating Homotopy Type Theory statements${neutral}"
	$(RUN) -variations -nbest=10 -to-latex-doc -symboltables=test/hott_demo.dkgf test/hott_demo.dk >out/hott_demo.tex
	cd out ; pdflatex hott_demo.tex ; $(OPEN) hott_demo.pdf

symboltest:
	echo "${lightgreen}# testing an example-based symbol table${neutral}"
	dk check test/symboltest.dk
	RunInformath -base=test/symboltest.dk test/symboltest.dkgf
	RunInformath -add-symboltables=test/symboltest.dkgf -variations -to-latex-doc test/symboltest.dk

natural_deduction:
	$(RUN) -to-latex-doc -symboltables=test/natural_deduction.dkgf test/natural_deduction_proofs.dk >out/nd.tex
	cd out ; pdflatex nd.tex ; $(OPEN) nd.pdf

natural_deduction_rules:
	echo "${lightgreen}## generating some natural deduction proofs${neutral}"
	$(RUN) -to-latex-doc -symboltables=test/natural_deduction.dkgf test/natural_deduction.dk >out/ndr.tex
	cd out ; pdflatex ndr.tex ; $(OPEN) ndr.pdf

proof_units:
	echo "${lightgreen}proof units have no Dedukti formalization so far${neutral}"
#	$(RUN) -add-symboltables=test/proof_units.dkgf -to-lang=$(lang) test/proof_units.dk

mathcore_examples:
	$(RUN) -add-symboltables=test/natural_deduction.dkgf -mathcore test/mathcore_examples.dk

mathextensions_examples:
	$(RUN) -add-symboltables=test/natural_deduction.dkgf test/mathextensions_examples.dk

naproche:
	echo "${lightgreen}## parsing and regenerating a Naproche document without going through Dedukti${neutral}"
	$(RUN) -translate -to-latex-doc -variations -synonyms=$(synonyms)  -symbolics=$(symbolics) -to-lang=$(lang) test/naproche-zf-set.tex >out/napzf.tex
	cd out ; pdflatex napzf.tex ; $(OPEN) napzf.pdf

interpret_naproche:
	echo "${lightgreen}## parsing and regenerating a Naproche document going through Dedukti${neutral}"
	$(RUN) test/naproche-zf-set.tex | grep -v "UN"  | grep ":" >tmp/napzf.dk
	$(RUN) -to-latex-doc -variations -synonyms=$(synonyms)  -symbolics=$(symbolics) -nbest=100 -to-lang=$(lang) tmp/napzf.dk >out/inapzf.tex
	cd out ; pdflatex inapzf.tex ; $(OPEN) inapzf.pdf

baseconstants:
	$(RUN) -to-latex-doc -variations share/baseconstants.dk >out/baseconstants.tex
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
	echo "${lightgreen}## parsing examples from Chartrand et al. with conversions to Dedukti${neutral}"
	$(RUN) test/gflean-data.txt

fermat:
	$(RUN) -add-symboltables=test/fermat.dkgf -variations test/fermat.dk

cartesian:
	$(RUN) -add-symboltables=test/cartesian.dkgf -variations  test/cartesian.dk

bind:
	$(RUN) -add-symboltables=test/bind.dkgf -variations test/bind.dk

prooftextdemo:
	$(RUN) -proof-text -base=test/natdedrules.dk -add-symboltables=test/natdrop.dkgf test/natdedproofs.dk >out/prooftextdemo.tex
	cd out ; pdflatex prooftextdemo.tex ; $(OPEN) prooftextdemo.pdf

binary_packages:
	cp -p `which RunInformath` tmp/
	cp -p share/InformathEng.pgf share/InformathFull.pgf tmp/
	cd tmp ; strip RunInformath ; tar cvfz RunInformath-$(VERSION)-$(ARCH).tgz RunInformath ; tar cvfz Informath-grammars-$(VERSION).tgz InformathEng.pgf InformathFull.pgf
	ls -l tmp/*.tgz
