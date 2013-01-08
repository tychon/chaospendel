
#include <stdlib.h>
#include "memory_wrappers.h"

#include "markov_chain.h"

markovchainmatrix *allocateMarkovChain(int statenum) {
  markovchainmatrix *matrix = assert_malloc(sizeof(markovchainmatrix));
  
  matrix->statenum = statenum;
  
  matrix->relations = assert_calloc(statenum, sizeof(double));
  for (int i = 0; i < statenum; i++)
    matrix->relations[i] = assert_calloc(statenum, sizeof(double));
  
  matrix->samplesperrelations = assert_calloc(statenum, sizeof(int));
  for (int i = 0; i < statenum; i++)
    matrix->samplesperrelations[i] = assert_calloc(statenum, sizeof(int));
  
  return matrix;
}

