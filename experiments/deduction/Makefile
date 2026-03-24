all: compile check test

compile:
	ghc --make -package containers Proofs.hs

check:
	cat natdedrules.dk natdedproofs.dk | dk check --stdin=x

test:
	./Proofs natdedrules.dk natdedproofs.dk >ex.tex
	pdflatex ex.tex
