
.PHONY: all numerical arduino_read

all: numerical doc-ger arduino_read

numerical:
	make -C numerical

arduino_read:
	make -C arduino_read

fftw/fourier: fftw/fourier.hs fftw/fourierplot.m
	cd fftw                          ;\
	ghc --make fourier.hs -o fourier ;\
	echo "running ./fourier ..."     ;\
	./fourier > in.csv 2> out.csv    ;\
	octave octaveplot.m

