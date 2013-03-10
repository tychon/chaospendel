
FWINDOW=512
FPS=default
SKIPT=0

GHC_OPTS=-O2 -funfolding-use-threshold=16 -optc-O3 -fexcess-precision -optc-ffast-math

all: main.x fourier.x pendulum.x

main.x: main.hs RungeKutta.hs
	ghc $(GHC_OPTS) --make main -o main.x

fourier.x: fourier.c
	gcc -g -std=gnu99 -Wall -Werror -o fourier.x fourier.c -lfftw3 -lm

# Build the RungeKutta Haskell module
RungeKutta: RungeKutta.hs 
	ghc $(GHC_OPTS) --make RungeKutta.hs

### These following targets are possibly phony and
### are maybe built always. Therefore they are
### not included in the "all" target

# run all the programms and scripts
run: main.x fourier.x show.py out.info out.csv out.pend1.pgm out.pend2.pgm
	python show.py -fps $(FPS) -time $(SKIPT) out

# this target runs the simulation and fourier transforms
# it's only used by the "run" target
out.csv out.info: ./main.x stats.awk
	./main.x > out.csv 2> out.info
	awk -F "," -f stats.awk out.csv >> out.info # energy stats

out.pend1.pgm out.pend2.pgm: ./fourier.x out.csv
	./fourier.x --inputfile out.csv --samples `cat out.csv | wc -l` --window $(FWINDOW) --column 0 --projectfile out.info --outputfile out.pend1.pgm --keyprefix fourier1
	echo "\n"
	./fourier.x --inputfile out.csv --samples `cat out.csv | wc -l` --window $(FWINDOW) --column 1 --projectfile out.info --outputfile out.pend2.pgm --keyprefix fourier2
	echo "\n"	

feigenbaum:
	./feigenbaum.sh

# generate haskell doc
documentation:
	haddock -h -odoc main.hs RungeKutta.hs

pendulum.x: pendulum.c
	gcc -mfpmath=387 -malign-double -march=native -mtune=native -g -Werror -Wall -O3 -fwhole-program -flto -ffast-math -std=gnu99 -o pendulum.x pendulum.c -lm -lowfat
