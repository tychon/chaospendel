
FWINDOW=512
FPS=default
SKIPT=0

all: main.x fourier.x pendulum.x

main.x: main.hs RungeKutta.hs
	ghc --make main -o main.x

fourier.x: fourier.c
	gcc -g -std=gnu99 -Wall -Werror -o fourier.x fourier.c -lfftw3 -lm

# Build the RungeKutta Haskell module
RungeKutta: RungeKutta.hs 
	ghc --make RungeKutta.hs

### These following targets are phony and
### are built always. Therefore they are
### not included in the "all" target

# run all the programms and scripts
run: main.x fourier.x show.py out.info out.csv out.pend1.pgm out.pend2.pgm
	python show.py -fps $(FPS) -time $(SKIPT) out

# this target runs the simulation and fourier transforms
# it's only used by the "run" target
out.csv out.info: ./main.x
	./main.x > out.csv 2> out.info
	awk -F "," -f stats.awk out.csv >> out.info

out.pend1.pgm out.pend2.pgm: ./fourier.x out.csv
	time ./fourier.x --inputfile out.csv --samples `cat out.csv | wc -l` --window $(FWINDOW) --column 0 --projectfile out.info --outputfile out.pend1.pgm
	echo "\n"
	time ./fourier.x --inputfile out.csv --samples `cat out.csv | wc -l` --window $(FWINDOW) --column 1 --projectfile out.info --outputfile out.pend2.pgm
	echo "\n"	

# generate haskell doc
documentation:
	haddock -h -odoc main.hs RungeKutta.hs

pendulum.x: pendulum.c
	gcc -mfpmath=387 -malign-double -march=native -mtune=native -g -Werror -Wall -O3 -fwhole-program -flto -ffast-math -std=gnu99 -o pendulum.x pendulum.c -lm -lowfat
