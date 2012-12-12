
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CMP(str, index) (strcmp(str, argv[index]) == 0)
#define FORMATHEX 1
#define FORMATDEC 2
#define FORMATBIN 3
#define FORMATASC 4

#define BYTETOBINARYPATTERN "%d%d%d%d%d%d%d%d"
#define BYTETOBINARY(byte)  \
  (byte & 0x80 ? 1 : 0), \
  (byte & 0x40 ? 1 : 0), \
  (byte & 0x20 ? 1 : 0), \
  (byte & 0x10 ? 1 : 0), \
  (byte & 0x08 ? 1 : 0), \
  (byte & 0x04 ? 1 : 0), \
  (byte & 0x02 ? 1 : 0), \
  (byte & 0x01 ? 1 : 0) 

#define PRINTABLE(ch) (ch >= 32 && ch <= 126)

int main(int argc, char *argv[]) {
  int sock;
  struct sockaddr_un server;
  
  char *sockpath = NULL;
  int format = FORMATHEX;
  for (int i = 1; i < argc; i++) {
    if (strcmp("--format", argv[i]) == 0 || strcmp("-f", argv[i]) == 0) {
      i++;
      if (CMP("bin", i) || CMP("binary", i)) format = FORMATBIN;
      else if (CMP("dec", i) || CMP("decimal", i)) format = FORMATDEC;
      else if (CMP("hex", i) || CMP("hexadecimal", i)) format = FORMATHEX;
      else if (CMP("asc", i) || CMP("ascii", i)) format = FORMATASC;
    }
    else if (! sockpath) sockpath = argv[i];
    else fprintf(stderr, "Argument ignored: %s\n", argv[i]);
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
    switch (format) {
      case FORMATBIN: {
        if (PRINTABLE(c)) printf(BYTETOBINARYPATTERN"\t%c\n", BYTETOBINARY(c), c);
        else printf(BYTETOBINARYPATTERN"\t%x\n", BYTETOBINARY(c), (int)c);
        break;
      }
      case FORMATDEC: printf("%d ", (int)c); break;
      case FORMATHEX: printf("%x ", (int)c); break;
      case FORMATASC: printf("%c", c); break;
    }
    fflush(stdout);
  }
  
  close(sock);
}

