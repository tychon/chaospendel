
#.PHONY: #none

all: numerical/main doc-ger

numerical/main: numerical/main.hs
	cd numerical ;\
	ghc --make main.hs -o main

doc-ger: doc-ger/Dokumentation.pdf doc-ger/Herleitung_standalone.pdf

%.pdf: %.tex
	rubber --inplace -pdf $*.tex

