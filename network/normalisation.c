
#include <stdlib.h>
#include <stdio.h>

#include "common.h"
#include "uds_client.h"
#include "protocol.h"
#include "projectreader.h"

#define BUFFERSIZE 1024

int main(int argc, char *argv[]) {
  char *socketpath = NULL;
  char *pendulumdatapath = NULL;
  char *outputdatafilepath = NULL;
  int samplenum = 1000;
  
  for (int i = 1; i < argc; i++) {
    if (ARGCMP("--samplenum", i)) {
      i ++;
      samplenum = atoi(argv[i]);
    }
    else if (ARGCMP("--pendulumdata", i) || ARGCMP("-p", i)) {
      i++;
      pendulumdatapath = argv[i];
    }
    else if (ARGCMP("--datafile", i) || ARGCMP("-o", i)) {
      i ++;
      outputdatafilepath = argv[i];
    }
    else if (ARGCMP("--inputsocket", i) || ARGCMP("-i", i)) {
      i ++;
      socketpath = argv[i];
    }
  }
  
  if (!socketpath || !pendulumdatapath) {
    printf("usage: %s [--samplenum INT] --pendulumdata|-p PATH --inputsocket|-i PATH [--datafile|-o OUTPUTFILE]\n", argv[0]);
    exit(1);
  }
  
  projectdata *pd = assert_malloc(sizeof(projectdata));
  
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  readPendulumData(pd, pendulumdatapath);
  
  fprintf(stderr, "opening connection to server on \"%s\" ...\n", socketpath);
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  unsigned char buffer[BUFFERSIZE];
  int length;
  struct packet2byte *parsedinput = assert_malloc(sizeof(struct packet2byte));
  
  fprintf(stderr, "Waiting for %d samples ...\n", samplenum);
  while ( (length = uds_read(udscs, buffer, BUFFERSIZE)) > 0) {
    printf("%lld\n", parsedinput->timestamp);
    // TODO
  }
  
  if (outputdatafilepath) {
    fprintf(stderr, "writing data to file \"%s\" ...\n", outputdatafilepath);
    //TODO
  }
  
  fprintf(stderr, "end of data\n");
  uds_close_client(udscs);
}

