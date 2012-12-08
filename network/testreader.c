
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
  int sock;
  struct sockaddr_un server;
  
  char *sockpath = NULL;
  int showcharacters = 0;
  for (int i = 0; i < argc; i++) {
    if (strcmp("-c", argv[i]) == 0) {
      showcharacters = 1;
    } else if (strcmp("-b", argv[i]) == 0) {
      showcharacters = 2;
    } else sockpath = argv[i];
  }
  if (! sockpath) {
    printf("usage: %s [-c | -b] <pathname>", argv[0]);
    exit(1);
  }
  
  sock = socket(AF_UNIX, SOCK_STREAM, 0);
  if (sock < 0) {
    perror("opening stream socket");
    exit(1);
  }
  
  server.sun_family = AF_UNIX;
  strcpy(server.sun_path, argv[1]);
  
  
  if (connect(sock, (struct sockaddr *) &server, sizeof(struct sockaddr_un)) < 0) {
    close(sock);
    perror("connecting stream socket");
    exit(1);
  }
  
  int rval;
  char c;
  for (;;) {
    rval = read(sock, &c, 1);
    if (rval < 0) {
      perror("reading data");
      exit(1);
    }
    if (rval == 0) {
      printf("end of data\n");
      exit(0);
    }
    if (showcharacters == 0) printf("%d ", (int)c);
    else if (showcharacters == 1) printf("%c", c);
    else if (showcharacters == 2) printf("%d (%c) ", (int)c, c);
    fflush(stdout);
  }
  
  close(sock);
}

