
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <fftw3.h>

#include "common.h"
#include "uds_client.h"
#include "protocol.h"

/*
// You know what this is:
#define PI  3.141592653589793238462643383279502884197169
// TAU is 2*PI
#define TAU 6.283185307179586476925286766559005768394338
*/

int window = 512;
int column = 0;

int instartpos; // start of data in buffer
int inbuflen; // length of data in buffer
double *inputbuffer; // ringbuffer holding inputdata for fourier transform

double inputbufferpushvalue(double val) {
  if (inbuflen < window) {
    inputbuffer[(instartpos+inbuflen)%window] = val;
    inbuflen ++;
    return 0;
  } else {
    double ret = inputbuffer[instartpos];
    inputbuffer[(instartpos+inbuflen)%window] = val;
    instartpos ++;
    if (instartpos == window)
      instartpos = 0;
    return ret;
  }
}

int main(int argc, char** argv) {
  char *inputsocketpath = NULL;
  char *outputsocketpath = NULL;
  // with default values
  
  for (int i = 1; i < argc; i++) {
    if (argcmpassint("--window|-w", argc, argv, &i, &window)) ;
    else if (argcmpassint("--column|-c", argc, argv, &i, &column)) ;
    else if (argcmpass("--inputsocket|-i", argc, argv, &i, &inputsocketpath)) ;
    else if (argcmpass("--outputsocket|-o", argc, argv, &i, &outputsocketpath)) ;
    else fprintf(stderr, "warning: Unknown argument ignored: \"%s\"\n", argv[i]);
  }
  
  if (! inputsocketpath || ! outputsocketpath) {
    printf("usage: %s [--window|-w INT] [--column|-c INT] --inputsocket|-i PATH --outputsocket|-o PATH\n", argv[0]);
    exit(0);
  }
  if (window <= 0) {
    fprintf(stderr, "error: window must be longer than zero!\n");
    exit(1);
  }
  
  // initalizing
  inputbuffer = fftw_malloc(sizeof(double) * window);
  instartpos = inbuflen = 0;
  
  fprintf(stderr, "opening connection to server on \"%s\"\n", inputsocketpath);
  udsclientsocket *udscs = uds_create_client(inputsocketpath);
  
  unsigned char readingbuffer[GLOBALSEQPACKETSIZE];
  int readingbufferlength;
  struct packet8byte *packet = allocate8bytePacket(column+1);
  
  fprintf(stderr, "start reading data ...\n");
  while ( (readingbufferlength = uds_read(udscs, readingbuffer, GLOBALSEQPACKETSIZE)) > 0) {
    if (parse8bytePacket(readingbuffer, readingbufferlength, packet, 1, column+1) != column+1) {
      fprintf(stderr, "Received invalid packet.\n");
      continue;
    }
    
    inputbufferpushvalue(packet->values[column]);
    if (inbuflen < window) {
      putchar('.');
    } else { // buffer is full
      printf("%lld\n", packet->timestamp);
      //TODO
    }
    
    fflush(stdout);
  }
  
  uds_close_client(udscs);
  
  /*
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
    result[resindex*freqn] = 0.0; // delete DC
    for (int i = 1; i < freqn; i++) {
      result[resindex*freqn+i] = log1p((fftwout[i][0]/window)*(fftwout[i][0]/window)+(fftwout[i][1]/window)*(fftwout[i][1]/window));
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
  printf("max freq magnitude: %f\n", maxval);
  
  // append some data to .info project file
  if (projectfilepath) {
    f = fopen(projectfilepath, "a+");
    fprintf(f, "fourier_window=%d\n", window);
    //fprintf(f, "fourier_rows=%d\n", resindex);
    //fprintf(f, "fourier_freqn=%d\n", freqn);
    fprintf(f, "fourier_pgm_scaling=%f\n", 256/maxval);
    fclose(f);
  }
  
  // write pgm file
  f = fopen(outputfilepath, "w");
  if (! f) {
    printf("Could not write to file: %s\n", outputfilepath);
    return -1;
  }
  fprintf(f, "P2\n%d %d\n256\n", freqn, resindex);
  for (int res = 0; res < resindex; res++) {
    for (int freq = 0; freq < freqn; freq ++) {
      // ~70% of user time are spent in this fprintf() call - if the program becomes too slow,
      // optimize that
      fprintf(f, "%d ", (int)floor(result[res*freqn+freq] * (256/maxval)));
    }
    fprintf(f, "\n");
  }
  fclose(f);
  */
}


