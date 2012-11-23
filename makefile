
#.PHONY: #none

all: numerical/main doc-ger

numerical/main: numerical/main.hs
	cd numerical ;\
	ghc --make main.hs -o main

numericaltest: numerical/main
	cd numerical ;\
	./main > out.csv 2> out.info ;\
	python show.py out

RungeKutta/RungeKutta: RungeKutta/RungeKutta.hs
	cd RungeKutta ;\
	ghc --make RungeKutta.hs -o RungeKutta ;\
	haddock -h -odoc RungeKutta.hs

fftw/fourier: fftw/fourier.hs fftw/fourierplot.m
	cd fftw                          ;\
	ghc --make fourier.hs -o fourier ;\
	echo "running ./fourier ..."     ;\
	./fourier > in.csv 2> out.csv    ;\
	octave octaveplot.m

doc-ger: doc-ger/Dokumentation.pdf doc-ger/Herleitung_standalone.pdf

%.pdf: %.tex
	rubber --inplace -pdf $*.tex

