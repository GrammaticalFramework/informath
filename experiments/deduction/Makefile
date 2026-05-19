all: natural

natural:
	runghc Deduction.hs >de.tex
	pdflatex de.tex

compile:
	ghc --make -package containers Proofs.hs
#	ghc --make -package containers -package array Proofs.hs

check:
	cat natdedrules.dk natdedproofs.dk | dk check --stdin=x

test: compile
	./Proofs natdedrules.dk natdedproofs.dk natdrop.txt >ex.tex
	pdflatex ex.tex
