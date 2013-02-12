
#ifndef _MARKOV_CHAIN_H
#define _MARKOV_CHAIN_H

typedef struct markovchainmatrix {
  int statenum;
  /// Holds the number of transitions recorded for every state.
  /// Size: statenum*sizeof(int)
  /// *Size: statenum*sizeof(int)
  /// use: relations[statindex1][stateindex2] = number
  int **relations;
  /// Holds the number of transitions done from every state.
  /// Size: statenum * sizeof(int)
  /// use: samplesperstate[stateindex] = number
  int *samplesperstate;
} markovchainmatrix;

/**
 * Allocate all the memory for one markov chain.
 */
markovchainmatrix *allocateMarkovChain(int statenum);

void markovchain_addsample(markovchainmatrix *matrix
                         , int stateindex_from
                         , int stateindex_dest);

/**
 * @returns the probability for the transition from one given state to
 * another one.
 */
double markovchain_getprob(markovchainmatrix *matrix
                       , int stateindex_from
                       , int stateindex_dest);

/**
 * @returns the index of the most probable next state.
 */
int markovchain_getMostProbableNextState(markovchainmatrix *matrix
                                       , int stateindex_from);

/**
 * @returns the number of samples collected for one given state.
 */
int markovchain_getSamplesAt(markovchainmatrix *matrix
                           , int stateindex_from);

void markovchain_printDOTLanguageToFile(markovchainmatrix *matrix, char *filepath);
void markovchain_writeDataFile(markovchainmatrix *matrix, char *filepath);
void markovchain_readDataFile(char *filepath, markovchainmatrix *matrix);

#endif // _MARKOV_CHAIN_H

