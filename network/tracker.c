
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <stdlib.h>

#include "uds_client.h"

int main(int argc, char *argv[]) {
  char *socketpath = NULL;
  
  for (int i = 0; i < argc; i++) {
    socketpath = argv[i];
  }
  if (! socketpath) {
    printf("usage: %s SOCKETPATHNAME", argv[0]);
    exit(1);
  }
  
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  //TODO
  
  uds_close_client(udscs);
}

