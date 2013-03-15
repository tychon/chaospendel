
#ifndef _UDS_CLIENT_H
#define _UDS_CLIENT_H

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

struct udsclientsocket {
  int socketfd;
  struct sockaddr_un server;
  char *socketpath;
};
typedef struct udsclientsocket udsclientsocket;

udsclientsocket *uds_create_client(char *socketpath);
void uds_close_client(udsclientsocket*);

int uds_read(udsclientsocket*, void *buffer, size_t nbytes);
void uds_empty_pipe(udsclientsocket *udscs);
int uds_write(udsclientsocket *udscs, void *buffer, size_t nbytes);

#endif // _UDS_CLIENT_H

