
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/socket.h>
#include <signal.h>
#include <errno.h>
#include <assert.h>

#include "memory_wrappers.h"
#include "uds_server.h"

////////
// local

static void connection_read_cb(struct ev_loop *loop, ev_io *w, int revents) {
  udsserversocket *udsss = w->data;
  pthread_mutex_lock( &(udsss->mutex) );

  char data[257];
  ssize_t n = read(w->fd, data, 256);
  if (n == 0 || n == -1) {
    // this socket was closed
    if (n == 0) errno = 0;
    perror("message socket was closed");
    // remove socket
    int i;
    for (i=0; i<udsss->connection_count; i++) {
      if (udsss->messagesocketsfds[i] == w->fd) break;
    }
    assert(i != udsss->connection_count);
    for (int x = i; x < udsss->connection_count-1; i++) // shift following sockets
      udsss->messagesocketsfds[x] = udsss->messagesocketsfds[x+1];
    udsss->connection_count --;
    pthread_mutex_unlock( &(udsss->mutex) );
    return;
  }

  pthread_mutex_unlock( &(udsss->mutex) );

  data[n] = 0;
  if (udsss->handle_incoming != NULL) {
    udsss->handle_incoming(udsss, data, n);
  }
}

static void udsss_master_ready_cb(struct ev_loop *loop, ev_io *w, int revents) {
  udsserversocket *udsss = w->data;
  pthread_mutex_lock( &(udsss->mutex) );

  if (udsss->connection_count == MAXCONNECTIONS-1) {
    fprintf(stderr, "connection refused: maximum connection count of %d was reached\n", MAXCONNECTIONS);
    goto out;
  }

  int msgsock = accept(udsss->socketfd, 0, 0);
  if (msgsock == -1) {
    perror("accepting for server socket");
    abort();
  }
  
  ev_io *w_ = malloc(sizeof(*w_));
  ev_io_init(w_, connection_read_cb, msgsock, EV_READ);
  udsss->messagesocketsfds[udsss->connection_count] = msgsock;
  udsss->connection_count ++;
  fprintf(stderr, "new connection accepted\n");

out:
  pthread_mutex_unlock( &(udsss->mutex) );
}

void *uds_run(void *ptr) {
  udsserversocket *udsss = (udsserversocket*)ptr;
  udsss->loop = ev_loop_new(0);
  listen(udsss->socketfd, 5);
  ev_io udsss_watcher;
  ev_io_init(&udsss_watcher, udsss_master_ready_cb, udsss->socketfd, EV_READ);
  ev_io_start(udsss->loop, &udsss_watcher);
  udsss_watcher.data = udsss;
  ev_run(udsss->loop, 0);
  return NULL;
}

/////////////////////
// declared in header

udsserversocket *uds_create_server(char *socketpath) {
  // don't die just because you can't talk to someone anymore. it's not
  // that bad.
  signal(SIGPIPE, SIG_IGN);

  udsserversocket* udsss = assert_malloc(sizeof(udsserversocket));
  udsss->connection_count = 0;
  // initialize mutex with standard attributes
  if (pthread_mutex_init(&(udsss->mutex), NULL)) {
    perror("initializing thread synchronisation");
    exit(1);
  }
  
  udsss->socketfd = socket(AF_UNIX, SOCK_SEQPACKET, 0);
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

void uds_start_server(udsserversocket *udsss) {
  if (pthread_create(&(udsss->thread), NULL, uds_run, (void*)udsss)) {
    perror("creating unix domain socket server thread");
    exit(1);
  }
}

void uds_stop_server(udsserversocket *udsss) {
  if (close(udsss->socketfd)) {
    perror("closing server socket");
    exit(1);
  }
  pthread_cancel(udsss->thread);
  //pthread_join(udsss->thread, NULL);
  free(udsss);
}

void uds_send_toall(udsserversocket *udsss, const void *buffer, size_t nbytes) {
  pthread_mutex_lock( &(udsss->mutex) );
  for (int i = 0; i < udsss->connection_count; i++) {
    int retv = send(udsss->messagesocketsfds[i], buffer, nbytes, MSG_EOR);
    if (retv < 0) {
      // this socket was closed
      perror("trying to send message, but socket was closed");
      // no, we don't clean up here - we let the network thread take care of that
    }
  }
  pthread_mutex_unlock( &(udsss->mutex) );
}


