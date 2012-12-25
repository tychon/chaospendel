
#define _POSIX_SOURCE

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "projectreader.h"
#include "uds_client.h"
#include "protocol.h"

int argcmpTestOptions(const char *arg, char *options) {
  char *token = "x", *saveptr = "y";
  while ( (token = strtok_r(options, "|", &saveptr)) ) {
    options = NULL;
    if (strcmp(token, arg) == 0) return 1;
  }
  return 0;
}
int argcmpass(char *options, const int argc, char *argv[], int *argindex, char **dest) {
  if (argcmpTestOptions(argv[*argindex], options)) {
    if (*argindex >= argc-1) {
      fprintf(stderr, "Invalid argument, expected value after option \"%s\"\n", options);
      exit(1);
    }
    (*argindex) ++;
    *dest = argv[*argindex];
    
    return 1;
  }
  else
    return 0;
}

int main(int argc, char *argv[]) {
  char *socketpath = NULL;
  char *pendulumdatapath = NULL;
  char *normalisationdatapath = NULL;
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--pendulum", argc, argv, &i, &pendulumdatapath)) ;
    else if (argcmpass("--normalisation", argc, argv, &i, &normalisationdatapath)) ;
    else if (argcmpass("--inputsocket|-i", argc, argv, &i, &socketpath)) ;
    else {
      fprintf(stderr, "Argument ignored: \"%s\"\n", argv[i]);
    }
  }
  
  if (! socketpath || ! pendulumdatapath || ! normalisationdatapath) {
    printf("usage: %s SOCKETPATHNAME\n", argv[0]); //TODO you know
    exit(1);
  }
  
  projectdata *pd = assert_malloc(sizeof(projectdata));
  
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  readPendulumData(pd, pendulumdatapath);
  
  fprintf(stderr, "reading normalisation data from \"%s\" ...\n", normalisationdatapath);
  readNormalisationData(pd, normalisationdatapath);
  
  printf("opening connection to server on \"%s\"\n", socketpath);
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  // TODO
  
  fprintf(stderr, "end of data\n");
  uds_close_client(udscs);
}

