
main: main.hs RungeKutta.hs
	ghc --make main -o main

RungeKutta: RungeKutta.hs 
	ghc --make RungeKutta.hs

documentation:
	haddock -h -odoc main.hs RungeKutta.hs
