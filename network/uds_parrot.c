
#include <stdlib.h>
#include <stdio.h>

#include "uds_client.h"

int main(int argc, char *argv[]) {
  char *socketpath = argv[1];
  fprintf(stderr, "opening socket \"%s\"\n", socketpath);
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  char buffer[1024];
  int retv;
  for (;;) {
    retv = uds_read(udscs, buffer, 1024);
    if (retv < 0) {
      perror("reading data");
      exit(1);
    }
    if (retv == 0) {
      break;
    }
    fwrite(buffer, sizeof(char), retv, stdout);
    fflush(stdout);
  }
  
  uds_close_client(udscs);
}

