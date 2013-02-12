#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include "common.h"

#include "markov_chain.h"

markovchainmatrix *allocateMarkovChain(int statenum) {
  markovchainmatrix *matrix = assert_malloc(sizeof(markovchainmatrix));
  
  matrix->statenum = statenum;
  
  matrix->relations = assert_calloc(statenum, sizeof(int*));
  
  matrix->samplesperstate = assert_calloc(statenum, sizeof(int));
  
  return matrix;
}

void markovchain_addsample(markovchainmatrix *matrix
                         , int stateindex_from
                         , int stateindex_dest) {
  if (matrix->relations[stateindex_from] == NULL) {
    matrix->relations[stateindex_from] = assert_calloc(matrix->statenum, sizeof(int));
  }
  matrix->relations[stateindex_from][stateindex_dest] ++;
  matrix->samplesperstate[stateindex_from] ++;
}

double markovchain_getprob(markovchainmatrix *matrix
                       , int stateindex_from
                       , int stateindex_dest) {
  if (matrix->relations[stateindex_from] == NULL) return 0;
  return (double)matrix->relations[stateindex_from][stateindex_dest]
       / (double)matrix->samplesperstate[stateindex_from];
}

int markovchain_getMostProbableNextState(markovchainmatrix *matrix
                                       , int stateindex_from) {
  int bestcount = 0;
  int bestindex = -1;
  for (int i = 0; i < matrix->statenum; i++) {
    if (matrix->relations[stateindex_from] != NULL
        && matrix->relations[stateindex_from][i] > bestcount) {
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
      if (matrix->relations[from] != NULL && matrix->relations[from][dest] > 0) {
        fprintf(f, " Q%d -> Q%d [color = black, label=\"%lf\"];\n", from, dest, markovchain_getprob(matrix, from, dest));
      }
    }
  }
  // print closing
  fputs("}\n", f);
}


void markovchain_writeDataFile(markovchainmatrix *matrix, char *filepath) {
  fprintf(stderr, "expected file size: %d MB\n", (int)((matrix->statenum*matrix->statenum+matrix->statenum)*sizeof(int) / 1000000));
  FILE *f = fopen(filepath, "wb+");
  if (! f) {
    perror("opening file");
    exit(1);
  }
  int *emptydata = calloc(matrix->statenum, sizeof(int));
  for (int i = 0; i < matrix->statenum; i++) {
    fprintf(stderr, ESCAPE_CLEARLINE"  step 1: (%2.1lf%%) writing line %d  ", (double)i / (double)matrix->statenum * 100.0, i+1);
    int *data = (matrix->relations[i] == NULL) ? emptydata : matrix->relations[i];
    if (fwrite(data, sizeof(int), matrix->statenum, f) <= 0) {
      perror("saving markov chain: writing binary data");
      exit(1);
    }
  }
  free(emptydata);
  fprintf(stderr, "\n  step 2: writing additional line\n");
  if (fwrite(matrix->samplesperstate, sizeof(int), matrix->statenum, f) <= 0) {
    perror("saving markov chain: writing binary data");
    exit(1);
  }
  fflush(f);
  fclose(f);
  fprintf(stderr, "finished writing markov data file.\n");
}

static bool check_allnulls(void *data, size_t len) {
  char *c = data, *e = data+len;
  do {
    if (*c != '\0') return false;
  } while (++c != e);
  return true;
}

void markovchain_readDataFile(char *filepath, markovchainmatrix *matrix) {
  FILE *f = fopen(filepath, "rb");
  if (! f) {
    perror("opening file");
    exit(1);
  }
  for (int i = 0; i < matrix->statenum; i++) {
    fprintf(stderr, ESCAPE_CLEARLINE"  step 1: (%2.1lf%%) reading line %d  ", (double)i / (double)matrix->statenum * 100.0, i+1);
    if (matrix->relations[i] == NULL) {
      matrix->relations[i] = assert_calloc(matrix->statenum, sizeof(int));
    }
    if (fread(matrix->relations[i], sizeof(int), matrix->statenum, f) <= 0) {
      perror("reading markov chain");
      exit(1);
    }
    if (check_allnulls(matrix->relations[i], sizeof(int)*matrix->statenum)) {
      free(matrix->relations[i]);
      matrix->relations[i] = NULL;
    }
  }
  fprintf(stderr, "\n  step 2: reading additional line\n");
  if (fread(matrix->samplesperstate, sizeof(int), matrix->statenum, f) <= 0) {
    perror("reading markov chain");
    exit(1);
  }
  fclose(f);
  fprintf(stderr, "finished reading markov data file.\n");
}



