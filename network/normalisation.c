
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "common.h"
#include "uds_client.h"
#include "protocol.h"
#include "projectreader.h"

#define ESCAPE_CLEARLINE "\x1B[80D\x1B[K"

int main(int argc, char *argv[]) {
  char *socketpath = NULL;
  char *pendulumdatapath = NULL;
  int samplenum = 1000;
  
  for (int i = 1; i < argc; i++) {
    if (argcmpassint("--samplenum|-n", argc, argv, &i, &samplenum)) ;
    else if (argcmpass("--pendulumdata|-p", argc, argv, &i, &pendulumdatapath)) ;
    else if (argcmpass("--inputsocket|-i", argc, argv, &i, &socketpath)) ;
    else fprintf(stderr, "warning: Unknown Argument ignored: \"%s\"\n", argv[i]);
  }
  
  if (!socketpath || !pendulumdatapath) {
    printf("usage: %s [--samplenum INT] --pendulumdata|-p PATH --inputsocket|-i PATH\n", argv[0]);
    exit(1);
  }
  
  projectdata *pd = assert_malloc(sizeof(projectdata));
  
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  readPendulumData(pd, pendulumdatapath);
  
  fprintf(stderr, "opening connection to server on \"%s\" ...\n", socketpath);
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  int samplecount = 0; // counts number of received valid packets
  unsigned char buffer[GLOBALSEQPACKETSIZE]; // data from socket goes here
  int length; // valid data in 'buffer'
  // this contains the parsed data out of 'buffer'
  struct packet2byte *parsedinput = allocate2bytePacket(pd->solnum);
  
  double *average = assert_calloc(pd->solnum, sizeof(double)); // the mean value E(X)
  double *average_powered = assert_calloc(pd->solnum, sizeof(double)); // this is E(X^2)
  
  fprintf(stderr, "Waiting for %d samples ...\n", samplenum);
  while ( (length = uds_read(udscs, buffer, GLOBALSEQPACKETSIZE)) > 0) {
    if (parse2bytePacket(buffer, length, parsedinput, 1, pd->solnum) != pd->solnum) {
      fprintf(stderr, ESCAPE_CLEARLINE"Received invalid packet.\n");
      continue;
    }
    
    fprintf(stderr, ESCAPE_CLEARLINE"%3.0f%% ", (double)samplecount / (double)samplenum * 100.0);
    fflush(stderr);
    
    for (int i = 0; i < pd->solnum; i++) {
      average[i] += (double)parsedinput->values[i] / (double)samplenum;
      average_powered[i] += (double)parsedinput->values[i] * (double)parsedinput->values[i] / (double)samplenum;
    }
    
    samplecount ++;
    if (samplecount == samplenum) {
      fprintf(stderr, "\n");
      break;
    }
  }
  if (samplecount != samplenum) {
    fprintf(stderr, ESCAPE_CLEARLINE"\nERROR: collected %d samples instead of %d samples.\n", samplecount, samplenum);
    exit(1);
  }
  
  fprintf(stderr, "closing unix domain socket ...\n");
  uds_close_client(udscs);
  
  
  fprintf(stderr, "writing data ...\n\n");
  
  double variance, stddeviation;
  for (int i = 0; i < pd->solnum; i++) {
    variance = average_powered[i] - average[i] * average[i];
    stddeviation = sqrt(variance);
    
    printf("arithmetic_mean%d = %f\n", i, average[i]);
    printf("variance%d = %lf\n", i, variance);
    printf("standard_deviation%d = %f\n", i, stddeviation);
  }
  printf("\n");
  
  fflush(stdout);
  // end!
}

