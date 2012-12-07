
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>

#define BUFFERSIZES 1024

int main(int argc, char *argv[]) {
  int deleteOldSocket = 0;
  int deleteSocketAfterUse = 0;
  char *socketpath = NULL;
  char *csvpath = NULL;
  int samplerate = -1; // default value: send as fast as we can
  
  for (int i = 1; i < argc; i++) {
    if (strcmp("-f", argv[i]) == 0) deleteOldSocket = 1;
    else if (strcmp("-in", argv[i]) == 0) {
      i ++;
      csvpath = argv[i];
    }
    else if (strcmp("--socket", argv[i]) == 0) {
      i ++;
      socketpath = argv[i];
    }
    else if (strcmp("-fps", argv[i]) == 0) {
      i ++;
      samplerate = atoi(argv[i]);
    }
    else if (strcmp("-d", argv[i]) == 0) deleteOldSocket = 1;
    else fprintf(stderr, "Ignoring unknown argument: \"%s\"", argv[i]);
  }
  
  if (! socketpath || ! csvpath) {
    fprintf(stderr, "usage [-f] [-d] -in CSVFILEPATH --socket SOCKETPATH\n");
    exit(1);
  }
  
  printf("opening socket with path \"%s\"\n", socketpath);
  
  int sock, msgsock;
  struct sockaddr_un server;
  sock = socket(AF_UNIX, SOCK_STREAM, 0);
  if (sock < 0) {
    perror("opening stream socket");
    exit(1);
  }
  server.sun_family = AF_UNIX;
  strcpy(server.sun_path, socketpath);
  if (deleteOldSocket) unlink(server.sun_path);
  if (bind(sock, (struct sockaddr *) &server, sizeof(struct sockaddr_un))) {
    perror("binding stream socket");
    exit(1);
  }
  
  
  listen(sock, 5);
  printf("Waiting for incoming connection ...\n");
  msgsock = accept(sock, 0, 0);
  if (msgsock == -1) {
    perror("accept");
    exit(1);
  }
  
  printf("Connection established.\n");
  
  printf("Opening csv file \"%s\" ...", csvpath);
  FILE *csvf = fopen(csvpath, "r");
  if (! csvf) {
    perror("opening csv file");
    goto CLOSING_ERR;
  }
  
  if (samplerate < 0) printf("Sending samples as fast as we can ...\n");
  else printf("Sendings a rate of %d samples per second ...\n", samplerate);
  
  int outputpos = 0;
  int outreadpos = 0;
  int lineLength = -1; // indicates if there is a full line in outbuffer
  char readbuffer[BUFFERSIZES]; // this buffer the read method writes to
  char outbuffer[BUFFERSIZES]; // this is a buffer with the formatted output
  for (;;) {
    if (lineLength >= 0) {
      // send data
      if (write(sock, outbuffer[outputpos], sizeof(char)*lineLength) < 0) { // TODO this may be wrong
        perror("sending data");
        goto CLOSING_ERR;
      }
    } else {
      // read new data
      //TODO
    }
  }
  
  /*
  int rval;
  int outputpos = 0;
  int outreadpos = 0;
  char readbuffer[BUFFERSIZES]; // this buffer the read method writes to
  char outbuffer[BUFFERSIZES]; // this is a ringbuffer with the formatted output
  while ( (rval = fread(readbuffer, sizeof(char), BUFFERSIZES, csvf)) > 0) {
    for (int i = 0; i < rval; i++) {
      readbuffer[i]
    }
    //TODO implement tuning of samplerate
  }
  if (rval < 0) {
    perror("reading from csv");
    goto CLOSING_ERR;
  }*/
  
  printf("closing file ...\n");
  fclose(csvf);
  
  printf("closing sockets ...\n");
  close(msgsock);
  close(sock);
  if (deleteSocketAfterUse) {
    printf("deleting socket file ...\n");
    unlink(socketpath);
  }
  exit(0);
  
CLOSING_ERR:
  printf("closing sockets ...\n");
  close(msgsock);
  close(sock);
  if (deleteSocketAfterUse) {
    printf("deleting socket file ...\n");
    unlink(socketpath);
  }
  exit(1);
}

