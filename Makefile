RUN  := stack exec RunInformath --
PREVIOUSRUN := RunInformath -previous
OPEN := open  # pdf viewer command
GF_FILES := $(wildcard grammars/*.gf)

lang=Eng

.PHONY: all usual Dedukti Agda Lean Rocq demo devdemo RunInformath

all: grammars/Informath.pgf previous/grammars/PreviousInformath.pgf Dedukti Agda Rocq Lean RunInformath grammar

grammar: grammars/Informath.pgf

RunInformath:
	stack install

devel: grammar RunInformath

grammars/Informath.pgf: $(GF_FILES)
	cd grammars ; gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Noun1,Noun2,Fam,Fam2,Adj,Adj2,Adj3,AdjC,AdjE,Fun,Fun2,FunC,Verb,Verb2,Label,Compar,Const,Oper,Oper2 --probs=Informath.probs InformathEng.gf InformathSwe.gf InformathFre.gf InformathGer.gf

# if you want to use the -previous option, also do this
previous/grammars/PreviousInformath.pgf: $(GF_FILES)
	cd previous/grammars ; gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Fam,Adj,Rel,Fun,Label,Const,Oper,Compar,Set,Coercion,Relverb,Relnoun,Reladj,Comparnoun,Verb,Pred3 -name=PreviousInformath --probs=Informath.probs InformathEng.gf # InformathFre.gf InformathSwe.gf InformathGer.gf

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
	echo "## The main user demo, only requiring Informath and Latex"
	echo "## converting some simple arithmetic statements to English"
	$(RUN) -to-lang=Eng test/exx.dk
	echo "## converting some simple arithmetic statements to French"
	$(RUN) -to-lang=Fre test/exx.dk
	echo "## converting some simple arithmetic statements to German"
	$(RUN) -to-lang=Ger test/exx.dk
	echo "## converting some simple arithmetic statements to Swedish"
	$(RUN) -to-lang=Swe test/exx.dk
	echo "## parsing generated English with conversions back to Dedukti"
	$(RUN) -to-lang=Eng test/exx.dk >out/exx.txt
	$(RUN) -from-lang=Eng out/exx.txt
	echo "## parsing some natural English with conversions back to Dedukti"
	$(RUN) -from-lang=Eng test/gflean-data.txt
	cat src/BaseConstants.dk test/exx.dk >out/bexx.dk
	echo "## converting some simple arithmetic statements to Agda"
	$(RUN) -to-formalism=agda test/exx.dk
	echo "## converting some simple arithmetic statements to Rocq"
	$(RUN) -to-formalism=rocq test/exx.dk
	echo "## converting some simple arithmetic statements to Lean"
	$(RUN) -to-formalism=lean test/exx.dk
	echo "# converting some set theory statements to LaTeX"
	$(RUN) -to-latex-file -variations test/sets.dk >out/sets.tex
	echo "consider pdflatex out/sets.tex"
	echo "## creating and displaying a LaTeX document from a sample of 100 theorems"
	$(RUN) -to-latex-doc -variations -to-lang=$(lang) test/top100.dk >out/top100.tex
	cd out ; pdflatex top100.tex ; $(OPEN) top100.pdf


devdemo:
	echo "## The development demo, needs Dedukti, Agda, Lean, Rocq"
	echo "## checking some simple arithmetic statements in Dedukti"
	dk check out/bexx.dk
	echo "## converting some simple arithmetic statements to English"
	$(RUN) -to-lang=Eng test/exx.dk
	echo "## converting some simple arithmetic statements to French"
	$(RUN) -to-lang=Fre test/exx.dk
	echo "## converting some simple arithmetic statements to German"
	$(RUN) -to-lang=Ger test/exx.dk
	echo "## converting some simple arithmetic statements to Swedish"
	$(RUN) -to-lang=Swe test/exx.dk
	echo "## parsing generated English with conversions back to Dedukti"
	$(RUN) -to-lang=Eng test/exx.dk >out/exx.txt
	$(RUN) -from-lang=Eng out/exx.txt
	echo "## parsing some natural English with conversions back to Dedukti"
	$(RUN) -from-lang=Eng test/gflean-data.txt
	cat src/BaseConstants.dk test/exx.dk >out/bexx.dk
	echo "## converting some simple arithmetic statements to Agda"
	echo "open import BaseConstants\n\n" >out/exx.agda
	$(RUN) -to-formalism=agda test/exx.dk >>out/exx.agda
	cp -p src/BaseConstants.agda out/
	echo "## checking the generated file in Agda"
	cd out ; agda --prop exx.agda
	echo "## converting some simple arithmetic statements to Rocq"
	$(RUN) -to-formalism=rocq test/exx.dk >out/exx.v
	cat src/BaseConstants.v out/exx.v >out/bexx.v
	echo "## checking the generated file in Rocq"
	coqc out/bexx.v
	echo "## converting some simple arithmetic statements to Lean"
	$(RUN) -to-formalism=lean test/exx.dk >out/exx.lean
	echo "## checking the generated file in Lean"
	cat src/BaseConstants.lean out/exx.lean >out/bexx.lean
	lean out/bexx.lean
	echo "# checking some set theory statements and generating LaTeX"
	cat src/BaseConstants.dk test/sets.dk >out/sexx.dk
	dk check out/sexx.dk
	$(RUN) -to-latex-file -variations test/sets.dk >out/sets.tex
	echo "consider pdflatex out/sets.tex"
	echo "## checking a selection of 100 theorems in Dedukti"
	cat src/BaseConstants.dk test/top100.dk >out/texx.dk
	dk check out/texx.dk
	echo "## parsing and regenerating a Naproche document"
	make naproche
	echo "## generating some natural deduction proofs"
	make natural_deduction
	echo "## creating and displaying a LaTeX document from a sample of 100 theorems"
	make top100


top100:
	$(RUN) -to-latex-doc -variations -to-lang=$(lang) test/top100.dk >out/top100.tex
	cd out ; pdflatex top100.tex ; $(OPEN) top100.pdf
	cat src/BaseConstants.dk test/top100.dk >out/texx.dk
	dk check out/texx.dk

top100single:
	$(RUN) -to-latex-doc -to-lang=$(lang) test/top100.dk >out/top100.tex
	cd out ; pdflatex top100.tex ; $(OPEN) top100.pdf
	cat src/BaseConstants.dk test/top100.dk >out/texx.dk
	dk check out/texx.dk

sets:
	$(RUN) -variations -to-latex-doc -to-lang=$(lang) test/sets.dk >out/sets.tex
	cd out ; pdflatex sets.tex ; $(OPEN) sets.pdf

sigma:
	$(RUN) -variations -to-latex-doc test/sigma.dk >out/sigma.tex
	cd out ; pdflatex sigma.tex ; $(OPEN) sigma.pdf

natural_deduction:
	$(RUN) -to-latex-doc -constants=test/natural_deduction.dkgf test/natural_deduction_proofs.dk >out/nd.tex
	cd out ; pdflatex nd.tex ; $(OPEN) nd.pdf

natural_deduction_rules:
	$(RUN) -to-latex-doc -constants=test/natural_deduction.dkgf test/natural_deduction.dk >out/ndr.tex
	cd out ; pdflatex ndr.tex ; $(OPEN) ndr.pdf

proof_units:
	$(RUN) -constants=src/baseconstants.dkgf,test/proof_units.dkgf -to-lang=$(lang) test/proof_units.dk

mathcore_examples:
	$(RUN) -constants=src/baseconstants.dkgf,test/natural_deduction.dkgf -mathcore test/mathcore_examples.dk

mathextensions_examples:
	$(RUN) -constants=src/baseconstants.dkgf,test/natural_deduction.dkgf test/mathextensions_examples.dk

naproche:
	$(RUN) -translate -to-latex-doc -variations -to-lang=$(lang) test/naproche-zf-set.tex >out/napzf.tex
	cd out ; pdflatex napzf.tex ; $(OPEN) napzf.pdf

interpret_naproche:
	$(RUN) test/naproche-zf-set.tex | grep -v "UN"  | grep ":" >tmp/napzf.dk
	$(RUN) -to-latex-doc -variations -to-lang=$(lang) tmp/napzf.dk >out/napzf.tex
	cd out ; pdflatex -batchmode napzf.tex ; $(OPEN) napzf.pdf

baseconstants:
	cat src/BaseConstants.dk >tmp/baseconstants.dk

	$(RUN) -to-latex-file -variations tmp/baseconstants.dk >out/baseconstants.tex
	cd out ; pdflatex baseconstants.tex ; $(OPEN) baseconstants.pdf

parallel:
	tail -150 src/BaseConstants.dk >tmp/parallel.dk
	cat test/exx.dk >>tmp/parallel.dk
	cat test/sets.dk >>tmp/parallel.dk
	cat test/top100.dk >>tmp/parallel.dk
	$(RUN) -parallel-data -variations -no-ranking tmp/parallel.dk >tmp/parallel-informath.jsonl

parallel-def:
	tail -150 src/BaseConstants.dk >tmp/parallel.dk
	cat test/exx.dk >>tmp/parallel.dk
	cat test/sets.dk >>tmp/parallel.dk
	$(RUN) -parallel-data  -variations -no-ranking -no-unlex -dedukti-tokens tmp/parallel.dk >tmp/parallel-def-train.jsonl

matita:
	$(RUN) -constants=test/empty.dkgf test/mini-matita.dk

gflean:
	$(RUN) test/gflean-data.txt

previousdemo:
	$(PREVIOUSRUN) -lang=Eng test/exx.dk
	$(PREVIOUSRUN) -lang=Fre test/exx.dk
	$(PREVIOUSRUN) -lang=Ger test/exx.dk
	$(PREVIOUSRUN) -lang=Swe test/exx.dk
	$(PREVIOUSRUN) -lang=Eng test/exx.dk >out/exx.txt
	$(PREVIOUSRUN) -lang=Eng out/exx.txt
	$(PREVIOUSRUN) -lang=Eng test/gflean-data.txt
	cat src/BaseConstants.dk test/exx.dk >bexx.dk
	dk check bexx.dk
	$(PREVIOUSRUN) -to-agda test/exx.dk >src/exx.agda
	cd src ; agda --prop exx.agda
	$(PREVIOUSRUN) -to-coq test/exx.dk >exx.v
	cat src/BaseConstants.v exx.v >bexx.v
	coqc bexx.v
	$(PREVIOUSRUN) -to-lean test/exx.dk >exx.lean
	cat src/BaseConstants.lean exx.lean >bexx.lean
	lean bexx.lean
	cat src/BaseConstants.dk test/top100.dk >out/texx.dk
	dk check out/texx.dk
	cat src/BaseConstants.dk test/sets.dk >out/sexx.dk
	dk check out/sexx.dk
	$(PREVIOUSRUN) -to-latex-file -variations test/top100.dk >out/top100.tex
	echo "consider pdflatex out/top100.tex"
	$(PREVIOUSRUN) -to-latex-file -variations test/sets.dk >out/sets.tex
	echo "consider pdflatex out/sets.tex"
