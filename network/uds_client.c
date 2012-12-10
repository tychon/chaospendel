
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "memory_wrappers.h"

#include "uds_client.h"

udsclientsocket *uds_create_client(char *socketpath) {
  udsclientsocket *udscs = assert_malloc(sizeof(udsclientsocket));
  
  udscs->socketfd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (udscs->socketfd == -1) {
    perror("uds client, opening stream socket");
    exit(1);
  }
  udscs->server.sun_family = AF_UNIX;
  strcpy(udscs->server.sun_path, socketpath);
  if ( connect(udscs->socketfd,
               (struct sockaddr *) &(udscs->server),
               sizeof(struct sockaddr_un)            ) < 0) {
    close(udscs->socketfd);
    perror("uds client, connecting stream socket");
    exit(1);
  }
  
  return udscs;
}

void uds_close_client(udsclientsocket *udscs) {
  close(udscs->socketfd);
  free(udscs);
}

int uds_read(udsclientsocket *udscs, void *buffer, size_t nbytes) {
  int retval = read(udscs->socketfd, buffer, nbytes);
  if (retval < 0) {
    perror("uds client, read");
    exit(1);
  } else return retval;
}

