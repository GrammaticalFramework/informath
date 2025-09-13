RUN  := stack exec RunInformath --
OPEN := open  # pdf viewer command
GF_FILES := $(wildcard grammars/*.gf)

all: grammars/Informath.pgf Dedukti Agda Rocq Lean RunInformath

.PHONY: all Dedukti Agda Lean Rocq demo RunInformath

grammars/Informath.pgf: $(GF_FILES)
	cd grammars ; gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Fam,Adj,Rel,Fun,Label,Const,Oper,Compar,Set,Coercion,Relverb,Relnoun,Reladj,Comparnoun,Verb,Pred3 --probs=Informath.probs InformathEng.gf InformathFre.gf InformathSwe.gf InformathGer.gf

Dedukti:
	cd src/typetheory ; bnfc -m -p Dedukti --haskell-gadt Dedukti.bnf ; make

Agda:
	cd src/typetheory ; bnfc -m -p Agda --haskell-gadt Agda.bnf ; make

Lean:
	cd src/typetheory ; bnfc -m -p Lean --haskell-gadt Lean.bnf ; make

Rocq:
	cd src/typetheory ; bnfc -m -p Rocq --haskell-gadt Rocq.bnf ; make
RunInformath:
	stack install

clean:
	cd src/typetheory && \
	for dir in Agda Rocq Dedukti Lean; do \
		rm -rf "$$dir"/*; \
	done

cleangrammars:
	cd grammars && rm *.gfo *.pgf *.hs

demo:
	$(RUN) -lang=Eng test/exx.dk
	$(RUN) -lang=Fre test/exx.dk
	$(RUN) -lang=Ger test/exx.dk
	$(RUN) -lang=Swe test/exx.dk
	$(RUN) -lang=Eng test/exx.dk >exx.txt
	$(RUN) -lang=Eng exx.txt
	$(RUN) -lang=Eng test/gflean-data.txt
	cat src/BaseConstants.dk test/exx.dk >bexx.dk
	dk check bexx.dk
	$(RUN) -to-agda test/exx.dk >src/exx.agda
	cd src ; agda --prop exx.agda
	$(RUN) -to-coq test/exx.dk >exx.v
	cat src/BaseConstants.v exx.v >bexx.v
	coqc bexx.v
	$(RUN) -to-lean test/exx.dk >exx.lean
	cat src/BaseConstants.lean exx.lean >bexx.lean
	lean bexx.lean
	cat src/BaseConstants.dk test/top100.dk >out/texx.dk
	dk check out/texx.dk
	cat src/BaseConstants.dk test/sets.dk >out/sexx.dk
	dk check out/sexx.dk
	$(RUN) -to-latex-file -variations test/top100.dk >out/top100.tex
	echo "consider pdflatex out/top100.tex"
	$(RUN) -to-latex-file -variations test/sets.dk >out/sets.tex
	echo "consider pdflatex out/sets.tex"

top100:
	$(RUN) -to-latex-file -variations -ranking -lang=$(lang) test/top100.dk >out/top100.tex
	cd out ; pdflatex top100.tex ; $(OPEN) top100.pdf
	cat src/BaseConstants.dk test/top100.dk >out/texx.dk
	dk check out/texx.dk

top100single:
	$(RUN) -to-latex-file -lang=$(lang) test/top100.dk >out/top100.tex
	cd out ; pdflatex top100.tex ; $(OPEN) top100.pdf
	cat src/BaseConstants.dk test/top100.dk >out/texx.dk
	dk check out/texx.dk

sigma:
	$(RUN) -variations -to-latex-file -ranking test/sigma.dk >out/sigma.tex
	cd out ; pdflatex sigma.tex ; $(OPEN) sigma.pdf

olymp:
	$(RUN) -variations -to-latex-file -ranking test/olymp.dk >out/olymp.tex
	cd out ; pdflatex olymp.tex ; $(OPEN) olymp.pdf

naproche:
	$(RUN) test/naproche-zf-set.tex | grep -v "UN"  | grep ":" >tmp/napzf.dk
	$(RUN) -to-latex-file -variations -lang=$(lang) tmp/napzf.dk >out/napzf.tex
	cd out ; pdflatex napzf.tex ; $(OPEN) napzf.pdf

baseconstants:
#	tail -150 src/BaseConstants.dk >tmp/baseconstants.dk
	cat src/BaseConstants.dk >tmp/baseconstants.dk

	$(RUN) -to-latex-file -variations tmp/baseconstants.dk >out/baseconstants.tex
	cd out ; pdflatex baseconstants.tex ; $(OPEN) baseconstants.pdf

parallel:
	tail -150 src/BaseConstants.dk >tmp/parallel.dk
	cat test/exx.dk >>tmp/parallel.dk
	cat test/sets.dk >>tmp/parallel.dk
	cat test/top100.dk >>tmp/parallel.dk
	$(RUN) -parallel tmp/parallel.dk >tmp/parallel-informath.jsonl

parallel-def:
	tail -150 src/BaseConstants.dk >tmp/parallel.dk
	cat test/exx.dk >>tmp/parallel.dk
	cat test/sets.dk >>tmp/parallel.dk
	$(RUN) -parallel -no-unlex -dedukti-tokens tmp/parallel.dk >tmp/parallel-def-train.jsonl



matita:
	$(RUN) test/mini-matita.dk

gflean:
	$(RUN) test/gflean-data.txt
