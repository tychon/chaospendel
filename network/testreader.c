
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <stdlib.h>

main(int argc, char *argv[]) {
  int sock;
  struct sockaddr_un server;
  
  if (argc < 2) {
    printf("usage: %s <pathname>", argv[0]);
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
    printf("%d (%c) ", (int)c, c);
  }
  
  close(sock);
}

