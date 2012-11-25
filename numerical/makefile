
main: main.hs RungeKutta.hs
	ghc --make main -o main

fourier: fourier.c
	gcc -g -std=c99 -o fourier fourier.c -lfftw3 -lm

RungeKutta: RungeKutta.hs 
	ghc --make RungeKutta.hs

test: main show.py
	./main > out.csv 2> out.info
	./fourier --inputfile out.csv --samples `cat out.csv | wc -l` --outputfile out.pgm --window 512 --column 1
	python show.py out

documentation:
	haddock -h -odoc main.hs RungeKutta.hs
