
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
  
  for (int i = 1; i < argc; i++) {
    if (ARGCMP("--pendulum", i)) {
      i ++;
      pendulumdatapath = argv[i];
    }
    else {
      socketpath = argv[i];
    }
  }
  
  if (! socketpath) { //TODO paths to data files
    printf("usage: %s SOCKETPATHNAME\n", argv[0]);
    exit(1);
  }
  
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  projectdata *pd = assert_malloc(sizeof(projectdata));
  readPendulumData(pd, pendulumdatapath);
  
  printf("opening connection to server on \"%s\"\n", socketpath);
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  // TODO
  
  fprintf(stderr, "end of data\n");
  uds_close_client(udscs);
}

