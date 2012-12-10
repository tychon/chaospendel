
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <stdlib.h>

#include "memory_wrappers.h"
#include "uds_client.h"
#include "time_join.h"

int main(int argc, char *argv[]) {
  char *socketpath = NULL;
  
  for (int i = 0; i < argc; i++) {
    socketpath = argv[i];
  }
  if (! socketpath) {
    printf("usage: %s SOCKETPATHNAME", argv[0]);
    exit(1);
  }
  
  printf("opening connection to server on \"%s\"\n", socketpath);
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  char buffer[1024];
  int lastindex = 0;
  int retval;
  standardline *parsed = assert_malloc(sizeof(standardline));
  for (;;) {
    retval = uds_read(udscs, buffer+lastindex, 1024-lastindex);
    if (retval == 0) break;
    if (retval > 0) {
      printf("%d\t%.*s\n", retval, retval, buffer);
      retval = parseStandardLine(parsed, NULL, NULL, buffer, retval, 0);
      if (retval >= 0) {
        printf("parsed timestamp %lld\n", parsed->timestamp);
      } else {
        printf("parser error: %d\n", retval);
      }
    }
  }
  
  printf("end of data\n");
  uds_close_client(udscs);
}

