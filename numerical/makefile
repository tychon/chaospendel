
FWINDOW=512
FPS=default

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
	time ./main > out.csv 2> out.info
	echo "\n"
	time ./fourier --inputfile out.csv --samples `cat out.csv | wc -l` --window $(FWINDOW) --column 1 --projectfile out.info --outputfile out.pend1.pgm
	echo "\n"
	time ./fourier --inputfile out.csv --samples `cat out.csv | wc -l` --window $(FWINDOW) --column 3 --projectfile out.info --outputfile out.pend2.pgm
	echo "\n"
	python show.py -fps $(FPS) out

# generate haskell doc
documentation:
	haddock -h -odoc main.hs RungeKutta.hs
