
all: main fourier

main: main.hs RungeKutta.hs
	ghc --make main -o main

fourier: fourier.c
	gcc -g -std=gnu99 -Wall -Werror -o fourier fourier.c -lfftw3 -lm

# Build the RungeKutta Haskell module
RungeKutta: RungeKutta.hs 
	ghc --make RungeKutta.hs

### These following targets are phony and
### are built always. Therefore they are
### not included in the "all" target

# run all the programms and scripts
test: main fourier show.py
	./main > out.csv 2> out.info
	./fourier --inputfile out.csv --samples `cat out.csv | wc -l` --window 1000 --column 1 --projectfile out.info --outputfile out.pgm 2> fftwin.csv
	python show.py out

# generate haskell doc
documentation:
	haddock -h -odoc main.hs RungeKutta.hs
