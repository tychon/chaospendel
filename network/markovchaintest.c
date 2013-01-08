
#include <stdlib.h>
#include "markov_chain.h"

int main(void) {
  markovchainmatrix *m = allocateMarkovChain(6);
  
  markovchain_addsample(m, 0, 1);
  markovchain_addsample(m, 0, 1);
  markovchain_addsample(m, 0, 2);
  markovchain_addsample(m, 1, 2);
  markovchain_addsample(m, 2, 3);
  markovchain_addsample(m, 4, 5);
  markovchain_addsample(m, 4, 5);
  markovchain_addsample(m, 4, 5);
  markovchain_addsample(m, 4, 5);
  
  markovchain_printToFile(m, "markovtestgraph.dot");
}

