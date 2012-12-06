#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>

int main(int argc, char *argv[]) {
  int deleteOldSocket = 0;
  char *socketname = NULL;
  char *csvname = NULL;
  for (int i = 1; i < argc; i++) {
    if (strcmp("-f", argv[i]) == 0) deleteOldSocket = 1;
    else if (strcmp("-in", argv[i]) == 0) {
      i ++;
      csvname = argv[i];
    }
    else if (strcmp("--socket", argv[i]) == 0) {
      i ++;
      socketname = argv[i];
    }
    else socketname = argv[i];
  }
  
  if (! socketname || ! csvname) {
    fprintf(stderr, "usage [-f] -in FILENAME --socket SOCKETNAME\n");
    exit(1);
  }
  
  printf("opening socket with path \"%s\"\n", socketname);
  
  int sock, msgsock, rval;
  struct sockaddr_un server;
  char buf[1024];
  sock = socket(AF_UNIX, SOCK_STREAM, 0);
  if (sock < 0) {
    perror("opening stream socket");
    exit(1);
  }
  server.sun_family = AF_UNIX;
  strcpy(server.sun_path, socketname);
  if (deleteOldSocket) unlink(server.sun_path);
  if (bind(sock, (struct sockaddr *) &server, sizeof(struct sockaddr_un))) {
    perror("binding stream socket");
    exit(1);
  }
  printf("Socket has name %s\n", server.sun_path);
  listen(sock, 5);
  for (;;) {
    msgsock = accept(sock, 0, 0);
    if (msgsock == -1) perror("accept");
    else do {
        bzero(buf, sizeof(buf));
        if ((rval = read(msgsock, buf, 1024)) < 0)
          perror("reading stream message");
        else if (rval == 0)
          printf("Ending connection\n");
        else
          printf("-->%s\n", buf);
      } while (rval > 0);
    close(msgsock);
  }
  
  close(sock);
  unlink(socketname);
}

