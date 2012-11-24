
main: main.hs RungeKutta.hs
	ghc --make main -o main

RungeKutta: RungeKutta.hs 
	ghc --make RungeKutta.hs

test: main show.py
	./main > out.csv 2> out.info
	python show.py out

documentation:
	haddock -h -odoc main.hs RungeKutta.hs
