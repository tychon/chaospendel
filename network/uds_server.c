

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#include "memory_wrappers.h"
#include "uds_server.h"

void *uds_run(void *ptr) {
  udsserversocket *udsss = (udsserversocket*)ptr;
  listen(udsss->socketfd, 5);
  for (;;) {
    int msgsock = accept(udsss->socketfd, 0, 0);
    if (msgsock == -1) {
      perror("accept");
      exit(1);
    }
    if (udsss->connection_count == MAXCONNECTIONS-1) {
      fprintf(stderr, "connection refused because maximum connection counf of %d was reached", MAXCONNECTIONS);
      continue;
    }
    pthread_mutex_lock( &(udsss->mutex) );
    //TODO
    pthread_mutex_unlock( &(udsss->mutex) );
  }
  return udsss;
}

udsserversocket *uds_create(char *socketpath, int deleteSocketAfterUse) {
  udsserversocket* udsss = assert_malloc(sizeof(udsserversocket));
  udsss->deleteSocketAfterUse = deleteSocketAfterUse;
  udsss->connection_count = 0;
  // initialize mutex with standard attributes
  if (pthread_mutex_init(&(udsss->mutex), NULL)) {
    perror("initializing thread synchronisation");
    exit(1);
  }
  
  udsss->socketfd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (udsss->socketfd < 0) {
    perror("opening stream socket");
    exit(1);
  }
  
  udsss->server.sun_family = AF_UNIX;
  strcpy(udsss->server.sun_path, socketpath);
  if (bind(udsss->socketfd, (struct sockaddr *) &(udsss->server), sizeof(struct sockaddr_un))) {
    perror("binding stream socket");
    exit(1);
  }
  
  return udsss;
}

/**
 * Returns zero, if the listening thread was started successfully.
 * @see pthread_create
 */
int uds_start(udsserversocket *server) {
  return pthread_create(&(server->thread), NULL, uds_run, (void*)server);
}


