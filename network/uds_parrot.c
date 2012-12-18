/**
 * This application enables you to directly print binary data from
 * unix domain sockets to the standard output. Info and error messages are
 * printed out to the standard error output.
 */

#include <stdlib.h>
#include <stdio.h>

#include "common.h" // GLOBALSEQPACKETSIZE is defined here
#include "uds_client.h"

int main(int argc, char *argv[]) {
  char *socketpath = argv[1];
  fprintf(stderr, "opening socket \"%s\"\n", socketpath);
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  char buffer[GLOBALSEQPACKETSIZE];
  int retv;
  for (;;) {
    retv = uds_read(udscs, buffer, GLOBALSEQPACKETSIZE);
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

