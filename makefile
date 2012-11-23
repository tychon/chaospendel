
.PHONY: numerical

all: numerical doc-ger

numerical:
	make -C numerical

fftw/fourier: fftw/fourier.hs fftw/fourierplot.m
	cd fftw                          ;\
	ghc --make fourier.hs -o fourier ;\
	echo "running ./fourier ..."     ;\
	./fourier > in.csv 2> out.csv    ;\
	octave octaveplot.m

doc-ger: doc-ger/Dokumentation.pdf doc-ger/Herleitung_standalone.pdf

%.pdf: %.tex
	rubber --inplace -pdf $*.tex

