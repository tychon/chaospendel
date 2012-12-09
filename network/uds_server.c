
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>

#include "memory_wrappers.h"
#include "uds_server.h"

////////
// local

void *uds_run(void *ptr) {
  udsserversocket *udsss = (udsserversocket*)ptr;
  listen(udsss->socketfd, 5);
  for (;;) {
    int msgsock = accept(udsss->socketfd, 0, 0);
    if (msgsock == -1) {
      perror("accepting for server socket");
      return NULL;
    }
    pthread_mutex_lock( &(udsss->mutex) );
    if (udsss->connection_count == MAXCONNECTIONS-1) {
      fprintf(stderr, "connection refused because maximum connection counf of %d was reached", MAXCONNECTIONS);
    } else {
      udsss->messagesocketsfds[udsss->connection_count] = msgsock;
      udsss->connection_count ++;
    }
    pthread_mutex_unlock( &(udsss->mutex) );
  }
}

/////////////////////
// declared in header

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

void uds_start(udsserversocket *udsss) {
  if (pthread_create(&(udsss->thread), NULL, uds_run, (void*)udsss)) {
    perror("creating unix domain socket server thread");
    exit(1);
  }
}

void uds_stop(udsserversocket *udsss) {
  if (close(udsss->socketfd)) {
    perror("closing server socket");
    exit(1);
  }
}

void uds_dprintf_all(udsserversocket *udsss, const char *format, ...) {
  va_list argp;
  va_start(argp, format);
  pthread_mutex_lock( &(udsss->mutex) );
  for (int i = 0; i < udsss->connection_count; i++) {
    int retv = vdprintf(udsss->messagesocketsfds[i], format, argp);
    if (retv < 0) {
      // this socket was closed
      perror("message socket was closed");
      // remove socket
      for (int x = i; x < udsss->connection_count-1; i++) // shift following sockets
        udsss->messagesocketsfds[x] = udsss->messagesocketsfds[x+1];
      i --;
      udsss->connection_count --;
    }
  }
  va_end(argp);
  pthread_mutex_unlock( &(udsss->mutex) );
}

void uds_write_all(udsserversocket *udsss, const void *buffer, size_t nbytes) {
  pthread_mutex_lock( &(udsss->mutex) );
  for (int i = 0; i < udsss->connection_count; i++) {
    int retv = write(udsss->messagesocketsfds[i], buffer, nbytes);
    if (retv < 0) {
      // this socket was closed
      perror("message socket was closed");
      // remove socket
      for (int x = i; x < udsss->connection_count-1; i++) // shift following sockets
        udsss->messagesocketsfds[x] = udsss->messagesocketsfds[x+1];
      i --;
      udsss->connection_count --;
    }
  }
  pthread_mutex_unlock( &(udsss->mutex) );
}


