
#include <stdlib.h>
#include <stdio.h>
#include "common.h"

#include "markov_chain.h"

markovchainmatrix *allocateMarkovChain(int statenum) {
  markovchainmatrix *matrix = assert_malloc(sizeof(markovchainmatrix));
  
  matrix->statenum = statenum;
  
  matrix->relations = assert_malloc(statenum * sizeof(int*));
  for (int i = 0; i < statenum; i++)
    matrix->relations[i] = assert_calloc(statenum, sizeof(int));
  
  matrix->samplesperstate = assert_calloc(statenum, sizeof(int));
  
  return matrix;
}

void markovchain_addsample(markovchainmatrix *matrix
                         , int stateindex_from
                         , int stateindex_dest) {
  matrix->relations[stateindex_from][stateindex_dest] ++;
  matrix->samplesperstate[stateindex_from] ++;
}

double markovchain_getprob(markovchainmatrix *matrix
                       , int stateindex_from
                       , int stateindex_dest) {
  return (double)matrix->relations[stateindex_from][stateindex_dest]
       / (double)matrix->samplesperstate[stateindex_from];
}

int markovchain_getMostProbableNextState(markovchainmatrix *matrix
                                       , int stateindex_from) {
  int bestcount = 0;
  int bestindex = -1;
  for (int i = 0; i < matrix->statenum; i++) {
    if (matrix->relations[stateindex_from][i] > bestcount) {
      bestcount = matrix->relations[stateindex_from][i];
      bestindex = i;
    }
  }
  
  return bestindex;
}

int markovchain_getSamplesAt(markovchainmatrix *matrix
                           , int stateindex_from) {
  return matrix->samplesperstate[stateindex_from];
};

void markovchain_printDOTLanguageToFile(markovchainmatrix *matrix, char *filepath) {
  FILE *f = fopen(filepath , "w+");
  if (! f) {
    perror("opening dot file");
    exit(1);
  }
  
  // print opening
  fputs("digraph markovrelations {\n", f);
  // print all nodes
  for (int i = 0; i < matrix->statenum; i++)
    fprintf(f, " Q%d [label=\"Q%d\\n%d\"];\n", i, i, matrix->samplesperstate[i]);
  // print all relations
  for (int from = 0; from < matrix->statenum; from++) {
    for (int dest = 0; dest < matrix->statenum; dest++) {
      if (matrix->relations[from][dest] > 0) {
        fprintf(f, " Q%d -> Q%d [color = black, label=\"%lf\"];\n", from, dest, markovchain_getprob(matrix, from, dest));
      }
    }
  }
  // print closing
  fputs("}\n", f);
}


void markovchain_writeDataFile(markovchainmatrix *matrix, char *filepath) {
  fprintf(stderr, "expected file size: %ld MB\n", (matrix->statenum*matrix->statenum+matrix->statenum)*sizeof(int) / 1000000);
  FILE *f = fopen(filepath, "wb+");
  for (int i = 0; i < matrix->statenum; i++) {
    fprintf(stderr, ESCAPE_CLEARLINE"step 1: (%2.1lf%%) writing line %d  ", (double)i / (double)matrix->statenum * 100.0, i+1);
    if (fwrite(matrix->relations[i], sizeof(int), matrix->statenum, f) <= 0) {
      perror("saving markov chain: writing binary data");
      exit(1);
    }
  }
  fprintf(stderr, "\nstep 2: writing additional line\n");
  if (fwrite(matrix->samplesperstate, sizeof(int), matrix->statenum, f) <= 0) {
    perror("saving markov chain: writing binary data");
    exit(1);
  }
  fflush(f);
  fclose(f);
  fprintf(stderr, "finished writing markov data file.\n");
}
//int markovchain_readDataFile(char *filepath, markovchainmatrix *matrix) {
//}


