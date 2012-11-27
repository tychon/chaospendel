
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
run: main fourier show.py out.info out.csv out.pend1.pgm out.pend2.pgm
	python show.py -fps $(FPS) out
# this target runs the simulation and fourier transforms
# it's only used by the "run" target
out.csv out.info: ./main
	./main > out.csv 2> out.info
out.pend1.pgm out.pend2.pgm: ./fourier out.csv
	time ./fourier --inputfile out.csv --samples `cat out.csv | wc -l` --window $(FWINDOW) --column 0 --projectfile out.info --outputfile out.pend1.pgm
	echo "\n"
	time ./fourier --inputfile out.csv --samples `cat out.csv | wc -l` --window $(FWINDOW) --column 1 --projectfile out.info --outputfile out.pend2.pgm
	echo "\n"
	

# generate haskell doc
documentation:
	haddock -h -odoc main.hs RungeKutta.hs
