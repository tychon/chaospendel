
#ifndef _MARKOV_CHAIN_H
#define _MARKOV_CHAIN_H

typedef struct markovchainmatrix {
  int statenum;
  int **relations;
  int *samplesperstate;
} markovchainmatrix;

markovchainmatrix *allocateMarkovChain(int statenum);

void markovchain_addsample(markovchainmatrix *matrix
                         , int stateindex_from
                         , int stateindex_dest);

double markovchain_getprob(markovchainmatrix *matrix
                       , int stateindex_from
                       , int stateindex_dest);

int markovchain_getMostProbableNextState(markovchainmatrix *matrix
                                       , int stateindex_from);

int markovchain_getSamplesAt(markovchainmatrix *matrix
                           , int stateindex_from);

void markovchain_printToFile(markovchainmatrix *matrix, char *filepath);

#endif // _MARKOV_CHAIN_H

