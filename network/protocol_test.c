
#define _BSD_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "uds_server.h"
#include "protocol.h"

int main(int argc, char *argv[]) {
  char *socketpath = argv[1];
  unlink(socketpath);
  udsserversocket *udsss = uds_create_server(socketpath);
  uds_start_server(udsss);
  
  char buffer[1024];
  int retv;
  unsigned short values[10];
  for (;;) {
    for (int i = 0; i < 10; i++) values[i] ++;
    retv = formatHalfbyte2Packet(buffer, 1024, getUnixMillis(), values, 10);
    if (retv < 0) {
      printf("error: buffer overflow\n");
      exit(1);
    }
    uds_write_toall(udsss, buffer, retv);
    usleep(100*1000);
  }
  
  uds_stop_server(udsss);
}

