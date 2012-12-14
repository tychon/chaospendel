
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <stdlib.h>

#include "memory_wrappers.h"
#include "uds_client.h"
#include "protocol.h"

int main(int argc, char *argv[]) {
  char *socketpath = NULL;
  
  for (int i = 1; i < argc; i++) {
    socketpath = argv[i];
  }
  if (! socketpath) {
    printf("usage: %s SOCKETPATHNAME\n", argv[0]);
    exit(1);
  }
  
  printf("opening connection to server on \"%s\"\n", socketpath);
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  // TODO
  
  printf("end of data\n");
  uds_close_client(udscs);
}

