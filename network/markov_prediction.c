
#include <stdlib.h>
#include <stdio.h>
#include "common.h"
#include "uds_client.h"
#include "protocol.h"
#include "markov_chain.h"

int main(int argc, char *argv[]) {
  char *inputsocketpath = NULL;
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--inputsocket|-i", argc, argv, &i, &inputsocketpath) );
    else fprintf(stderr, "warning: Ignoring unknown argument \"%s\"\n", argv[i]);
  }
  
  if (! inputsocketpath) {
    printf("usage %s --inputsocket|-i PATH\n", argv[0]);
    exit(1);
  }
  
  fprintf(stderr, "opening connection to server on \"%s\"\n", inputsocketpath);
  udsclientsocket *udscs = uds_create_client(inputsocketpath);
  
  unsigned char buffer[GLOBALSEQPACKETSIZE];
  int bufferlength;
  struct packet4byte *packet = allocate4bytePacket(1);
  
  fprintf(stderr, "start reading data ...\n");
  while ( (bufferlength = uds_read(udscs, buffer, GLOBALSEQPACKETSIZE)) > 0) {
    // parse input data
    if (parse4bytePacket(buffer, bufferlength, packet, 1, 1) != 1) {
      fprintf(stderr, "Received invalid packet.\n");
      continue;
    }
    
    if (packet->values[0] > 0) {
      printf("%lld\t%d\n", packet->timestamp, packet->values[0]);
      fflush(stdout);
    }
  }
}

