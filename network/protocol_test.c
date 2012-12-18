
#define _BSD_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "common.h"
#include "uds_server.h"
#include "protocol.h"

int main(int argc, char *argv[]) {
  char *socketpath = argv[1];
  unlink(socketpath);
  udsserversocket *udsss = uds_create_server(socketpath);
  uds_start_server(udsss);
  
  unsigned char buffer[GLOBALSEQPACKETSIZE];
  int retv;
  unsigned short values[10];
  for (;;) {
    for (int i = 0; i < 10; i++) values[i] ++;
    retv = format2bytePacket(buffer, GLOBALSEQPACKETSIZE, getUnixMillis(), values, 10);
    if (retv < 0) {
      printf("error: buffer overflow\n");
      exit(1);
    }
    uds_send_toall(udsss, buffer, retv);
    usleep(100*1000);
  }
  
  uds_stop_server(udsss);
}

