
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <fftw3.h>

// You know what this is:
#define PI  3.141592653589793238462643383279502884197169
// TAU is 2*PI
#define TAU 6.283185307179586476925286766559005768394338

// returns whether the correct amount of data was parsed
int parseCSVLine(FILE *f, double *res, int columns) {
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
    return 0; // everything fine
  } else {
    return -1; // didn't parse exactly "columns" rows
  }
}

int main(int argc, char** argv) {
  // parse command line arguments
  // no default values!
  int samples = -1;
  char* inputfilepath = NULL;
  char* outputfilepath = NULL;
  // with default values
  int window = 512;
  int column = 0;
  
  for (int i = 1; i < argc; i++) {
    if (strcmp("--window", argv[i]) == 0) {
      i ++;
      window = atoi(argv[i]);
    }
    else if (strcmp("--inputfile", argv[i]) == 0) {
      i ++;
      inputfilepath = argv[i];
    }
    else if (strcmp("--column", argv[i]) == 0) {
      i ++;
      column = atoi(argv[i]);
    }
    else if (strcmp("--samples", argv[i]) == 0) {
      i ++;
      samples = atoi(argv[i]);
    }
    else if (strcmp("--outputfile", argv[i]) == 0) {
      i ++;
      outputfilepath = argv[i];
    }
    else {
      printf("Argument ignored: %s\n", argv[i]);
    }
  }
  
  printf("input file: %s\n", inputfilepath);
  printf("output file: %s\n", outputfilepath);
  printf("number of samples: %d\n", samples);
  printf("window: %d\n", window);
  
  if (samples <= window) {
    printf("you should give me more samples: samples > window.\n");
    return -1;
  }
  
  
  // open input file
  FILE *f = fopen(inputfilepath, "r");
  
  // initalizing fftw
  double *fftwin = fftw_malloc(sizeof(double) * window);
  int freqn = window/2 + 1; // array length of result
  fftw_complex *fftwout = fftw_malloc(sizeof(fftw_complex) * freqn);
  fftw_plan plan_forward = fftw_plan_dft_r2c_1d(window, fftwin, fftwout, FFTW_ESTIMATE);
  
  // initial filling of input array
  double numbers[column+1];
  int samplecount = 0;
  for (int i = 0; i < window; i++) {
    if (parseCSVLine(f, numbers, column+1) < 0) {
      fprintf(stderr, "parseCSVLine returned an error!\n");
      return 1;
    }
    fftwin[i] = numbers[column];
    samplecount ++;
    if (feof(f)) return 1; // mhhh, this should NOT happen
  }
  
  int ressamples = (samples-window+1);
  double *result = malloc(sizeof(double) * freqn * ressamples);
  int resindex = 0;
  double maxval = 0; // maximum value of doubles in result array
  // run fft
  while (1) {
    fftw_execute (plan_forward); // yeay!
    // copy and post process results
    for (int i = 0; i < freqn; i++) {
      result[resindex*freqn+i] =   (fftwout[i][0]/window)*(fftwout[i][0]/window)
                                 + (fftwout[i][1]/window)*(fftwout[i][1]/window);
      if (result[resindex*freqn+i] > maxval)
        maxval = result[resindex*freqn+i];
    }
    resindex ++;
    // push in new numbers
    if (feof(f)) {
      printf("There are less samples than you said.\n");
      break;
    }
    if (samplecount == samples) break;
    // aaargghh: shift every number one field left (but: O(n))
    for (int i = 0; i < window-1; i++) fftwin[i] = fftwin[i+1];
    // read next csv line
    if (parseCSVLine(f, numbers, column+1) < 0) {
      fprintf(stderr, "parseCSVLine returned an error!\n");
      return 1;
    }
    samplecount ++;
    fftwin[window-1] = numbers[column];
  }
  fclose(f);
  
  printf("I read %d samples and have %d fourier transform results.\n", samplecount, resindex);
  
  // write pgm file
  f = fopen(outputfilepath, "w");
  if (! f) {
    printf("Could not write to file: %s\n", outputfilepath);
    return -1;
  }
  fprintf(f, "P2\n%d %d\n256\n", resindex, freqn);
  for (int i = 0; i < resindex; i++) {
    for (int freq = freqn-1; freq >= 0; freq --) {
      fprintf(f, "%d ", (int)floor(result[i*freqn+freq] * (255/maxval)));
    }
    fprintf(f, "\n");
  }
  fclose(f);
  
  return 0;
}


