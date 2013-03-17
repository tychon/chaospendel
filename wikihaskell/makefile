
all: main.x

main.x: main.hs
	ghc --make main.hs -o main.x

run:
	./main.x > out.csv
	python show.py

clean:
	rm -f *.x *.csv *.hi *.o
