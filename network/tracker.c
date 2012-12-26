
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "projectreader.h"
#include "uds_client.h"
#include "protocol.h"

int main(int argc, char *argv[]) {
  char *socketpath = NULL;
  char *pendulumdatapath = NULL;
  char *normalisationdatapath = NULL;
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--pendulum|-p", argc, argv, &i, &pendulumdatapath)) ;
    else if (argcmpass("--normalisation|-n", argc, argv, &i, &normalisationdatapath)) ;
    else if (argcmpass("--inputsocket|-i", argc, argv, &i, &socketpath)) ;
    else {
      fprintf(stderr, "Unknown argument ignored: \"%s\"\n", argv[i]);
    }
  }
  
  if (! socketpath || ! pendulumdatapath || ! normalisationdatapath) {
    printf("usage: %s --pendulum|-p PATH --normalisation|-n PATH --inputsocket|-i PATH\n", argv[0]);
    exit(1);
  }
  
  projectdata *pd = assert_malloc(sizeof(projectdata));
  
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  readPendulumData(pd, pendulumdatapath);
  
  fprintf(stderr, "reading normalisation data from \"%s\" ...\n", normalisationdatapath);
  readNormalisationData(pd, normalisationdatapath);
  
  printf("opening connection to server on \"%s\"\n", socketpath);
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  double *lastnormvalue = assert_calloc(pd->solnum, sizeof(double));
  double *derivative1 = assert_malloc(sizeof(double) * pd->solnum);
  double *derivative2 = assert_malloc(sizeof(double) * pd->solnum);
  double normval, d1, d2;
  
  unsigned char buffer[GLOBALSEQPACKETSIZE];
  int bufferlength;
  struct packet2byte *parsedinput = allocate2bytePacket(pd->solnum);
  while ( (bufferlength = uds_read(udscs, buffer, GLOBALSEQPACKETSIZE)) > 0) {
    if (parse2bytePacket(buffer, bufferlength, parsedinput, 1, pd->solnum) != pd->solnum) {
      fprintf(stderr, "Received invalid packet.\n");
      continue;
    }
    
    for (int i = 0; i < pd->solnum; i++) {
      // normalize input value
      normval = (double)parsedinput->values[i];
      normval -= pd->sols[i][IDX_MEAN];
      normval /= pd->sols[i][IDX_STD_DEVIATION];
      // first derivative
      d1 = normval - lastnormvalue[i];
      // second derivative
      d2 = derivative1[i] - d1;
      // store the values
      derivative1[i] = d1;
      derivative2[i] = d2;
      lastnormvalue[i] = normval;
    }
    
    //TODO write output
  }
  
  fprintf(stderr, "end of data\n");
  uds_close_client(udscs);
}

