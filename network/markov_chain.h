
#ifndef _MARKOV_CHAIN_H
#define _MARKOV_CHAIN_H

typedef struct markovchainmatrix {
  int statenum;
  double **relations;
  int **samplesperrelations;
} markovchainmatrix;

markovchainmatrix *allocateMarkovChain(int statenum);

void markovchain_addsample(markovchainmatrix *matrix
                         , int stateindex_from
                         , int stateindex_dest);

int markovchain_getMostProbableNextState(markovchainmatrix *matrix
                                       , int stateindex_from);

#endif // _MARKOV_CHAIN_H

