
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
  int deleteSocketAfterUse;
  struct sockaddr_un server;
  int socketfd;
  int connection_count;
  int messagesocketsfds[MAXCONNECTIONS];
  pthread_t thread;
  pthread_mutex_t mutex;
};
typedef struct udsserversocket udsserversocket;

udsserversocket *uds_create(char *socketpath, int deleteSocketAfterUse);
void uds_start(udsserversocket*);
void uds_stop(udsserversocket*);

void uds_dprintf_all(udsserversocket*, const char *format, ...);
void uds_write_all(udsserversocket*, const void *buffer, size_t nbytes);

#endif // _UDS_SERVER_H

