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
  while ( (retv = uds_read(udscs, buffer, GLOBALSEQPACKETSIZE)) > 0) {
    fwrite(buffer, sizeof(char), retv, stdout);
    fflush(stdout);
  }
  
  uds_close_client(udscs);
}

