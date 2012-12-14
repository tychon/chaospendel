
#ifndef _UDS_SERVER_H
#define _UDS_SERVER_H

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <pthread.h>

#if !defined(__GLIBC__) && _POSIX_C_SOURCE < 200809
#error "dprintf may not exist, or may be wrong"
#endif

#ifndef MAXCONNECTIONS
  #define MAXCONNECTIONS 16
#endif

struct udsserversocket {
  char *socketpath;
  struct sockaddr_un server;
  int socketfd;
  int connection_count;
  int messagesocketsfds[MAXCONNECTIONS];
  pthread_t thread;
  pthread_mutex_t mutex;
};
typedef struct udsserversocket udsserversocket;

udsserversocket *uds_create_server(char *socketpath);
void uds_start_server(udsserversocket*);
void uds_stop_server(udsserversocket*);

void uds_dprintf_toall(udsserversocket*, const char *format, ...);
void uds_write_toall(udsserversocket*, const void *buffer, size_t nbytes);

#endif // _UDS_SERVER_H

