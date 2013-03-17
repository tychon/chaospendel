
/**
 * Read a CSV file and do a fourier transform over one column.
 * It can run fftw3 multiple times with shifted window.
 * The result is a CSV with one transform in one row and a new row
 * for every shift.
 * One row in the result has the size window/2-1
 */

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <fftw3.h>

#include "common.h"

static int column = 0;
static FILE *inpf
          , *outf;

// returns 0 if the correct amount of data was parsed
static bool parseCSVLine(FILE *f, int columns, double *res) {
  char buf[1000];
  if (fgets(buf, 1000, f) == NULL) {
    return -2; // real error
  }
  
  char *strtok_buf = buf;
  char *p = buf;
  int resindex = 0;
  while ((p = strtok(strtok_buf, ",")) != NULL) {
    strtok_buf = NULL;
    if (resindex == columns) return 1; // no more space in res
    char *endptr;
    res[resindex++] = strtod(p, &endptr);
    if (endptr == p) {
      fprintf(stderr, "WARNING: unparseable number!\n");
    } else if (*endptr != 0) {
      fprintf(stderr, "WARNING: can't parse the whole number!\n");
    }
  }
  if (resindex == columns) {
    return true; // everything is fine
  } else {
    return false; // didn't parse at least `columns` columns
  }
}

static void printCSVLine(FILE *f, int columns, double *data) {
  for (int i = 0; i < columns-1; i++) {
    fprintf(f, "%f,", data[i]);
  }
  fprintf(f, "%f", data[columns-1]);
  fputc('\n', f);
}

double *run(int window, bool single, bool nodc) {
  int freqn = window/2 + 1; // array length of result
  
  // initalizing fftw
  double *fftwin = fftw_malloc(sizeof(double) * window); // input for fftw3
  fftw_complex *fftwout = fftw_malloc(sizeof(fftw_complex) * freqn);
  fftw_plan plan_forward = fftw_plan_dft_r2c_1d(window, fftwin, fftwout, FFTW_ESTIMATE);
  
  int csvline = 0; // line in csv file
  double numbers[column+1]; // rows of csv file go here
  
  // initial filling of input array
  for (int i = 0; i < window; i++) {
    if (csvline ++, ! parseCSVLine(inpf, column+1, numbers) < 0) {
      fprintf(stderr, "ERROR: Bad CSV file! line %d\n", csvline);
      exit(1);
    }
    fftwin[i] = numbers[column];
  }
  
  // run fftw and load new data
  double *result = malloc(sizeof(double) * freqn);
  double tmp1, tmp2;
  for (;;) {
    fftw_execute (plan_forward); // yeay!
    // post process and copy results
    if (nodc) result[0] = 0.0; // delete DC
    for (int i = 1; i < freqn; i++) {
      tmp1 = fftwout[i][0]/window;
      tmp2 = fftwout[i][1]/window;
      result[i] = sqrt(tmp1*tmp1+tmp2*tmp2);
    }
    printCSVLine(outf, freqn, result);
    
    if (feof(inpf) || single) {
      return result;
    }
    
    // load and push in new numbers
    // shift every number one field left
    //memmove(fftwin, fftwin+sizeof(double), (window-1) * sizeof(double));
    for (int i = 0; i < window-1; i++) fftwin[i] = fftwin[i+1];
    // read next csv line
    if (csvline ++, ! parseCSVLine(inpf, column+1, numbers) < 0) {
      fprintf(stderr, "ERROR: Bad CSV file! line %d\n", csvline);
      exit(1);
    }
    fftwin[window-1] = numbers[column];
  }
}

int main(int argc, char** argv) {
  // parse command line arguments
  char *inputfilepath = NULL
     , *outputfilepath = NULL;
  int window = 512;
  bool single = false
     , cleardc = false;
  
  for (int i = 1; i < argc; i++) {
    if      (argcmpassint("--window|-w", argc, argv, &i, &window));
    else if (argcmpassint("--column|-c", argc, argv, &i, &column));
    else if (argcmpass("--inputfile|-i", argc, argv, &i, &inputfilepath));
    else if (argcmpass("--outputfile|-o", argc, argv, &i, &outputfilepath));
    else if (! strcmp("--single", argv[i])) single = true;
    else if (! strcmp("--cleardc", argv[i])) cleardc = true;
    else {
      fprintf(stderr, "Unknown argument: %s\n", argv[i]);
      exit(1);
    }
  }
  
  if (window <= 0) {
    fprintf(stderr, "ERROR: Window too small: %d\n", window);
    exit(1);
  }
  
  if (! inputfilepath || ! outputfilepath) {
    fprintf(stderr, "Give me an input and an output file!\n");
    exit(1);
  }
  
  // open input file
  inpf = fopen(inputfilepath, "r");
  outf = fopen(outputfilepath, "w+");
  
  //// RUN ////
  run(window, single, cleardc);
  
  fclose(inpf);
  fclose(outf);
  
  return 0;
}


