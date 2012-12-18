
#define _XOPEN_SOURCE 500 // for usleep in unistd
#define _GNU_SOURCE
#include <features.h>
#include <sys/timeb.h>
#include <time.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "common.h"
#include "uds_server.h"
#include "protocol.h"

#define BUFFERSIZES 2048

int main(int argc, char *argv[]) {
  int deleteOldSocket = 0;
  int deleteSocketAfterUse = 0;
  int quiet = 0;
  int printtimestamp = 1;
  char *socketpath = NULL;
  char *csvpath = NULL;
  int samplerate = -1; // default value: send as fast as we can
  
  for (int i = 1; i < argc; i++) {
    if (ARGCMP("-i", i) || ARGCMP("--inputfile", i)) {
      i ++;
      csvpath = argv[i];
    }
    else if (ARGCMP("-o", i) || ARGCMP("--outputsocket", i)) {
      i ++;
      socketpath = argv[i];
    }
    else if (ARGCMP("-r", i) || ARGCMP("--samplerate", i)) {
      i ++;
      samplerate = atoi(argv[i]);
    }
    else if (ARGCMP("-f", i)) deleteOldSocket = 1;
    else if (ARGCMP("-d", i)) deleteSocketAfterUse = 1;
    else if (ARGCMP("-q", i) || ARGCMP("--quiet", i)) quiet = 1;
    else if (ARGCMP("--notimestamp", i)) printtimestamp = 0;
    else fprintf(stderr, "Ignoring unknown argument: \"%s\"\n", argv[i]);
  }
  
  if (! socketpath || ! csvpath) {
    fprintf(stderr, "usage [-f] [-d] [--samplerate SAMPLERATE] --inputfile CSVFILEPATH --outputsocket SOCKETPATH\n");
    exit(1);
  }
  
  if (deleteOldSocket) unlink(socketpath);
  if ( ! quiet) printf("opening socket with path \"%s\"\n", socketpath);
  udsserversocket *udsss = uds_create_server(socketpath);
  uds_start_server(udsss);
  
  if ( ! quiet) printf("Opening csv file \"%s\" ...\n", csvpath);
  FILE *csvf = fopen(csvpath, "r");
  if (! csvf) {
    perror("opening csv file");
    exit(1);
  }
  
  if (samplerate <= 0 && ! quiet) printf("Sending samples as fast as we can ...\n");
  else if ( ! quiet) printf("Sending at a rate of %d samples per second ...\n", samplerate);
  int sleepmillis = (int) (1.0/ ((double)samplerate) * 1000.0);
  
  int retv, shift, inbufferlength = 0;
  unsigned short nextNumber = -1; // indicates if there is a full line in outbuffer and its length
  char *endptr;
  char inbuffer[BUFFERSIZES]; // this buffer the read method writes to
  unsigned char outbuffer[BUFFERSIZES]; // this buffer the protocol formatter writes to
  long long millisecs;
  // enless loop til end of csv file
  for (;;) {
    //// parse data
    nextNumber = (unsigned short)strtol(inbuffer, &endptr, 0);
    if (endptr == inbuffer || (endptr == inbuffer+inbufferlength)) { // no valid digits found
      // read data
      if ( ! quiet) { printf("<\n"); fflush(stdout); }
      retv = fread(inbuffer+inbufferlength, sizeof(char), BUFFERSIZES-inbufferlength, csvf);
      if (retv < 0) {
        perror("reading from csv file");
        exit(1);
      } else if (retv == 0) {
        if ( ! quiet) printf("\nend of data.\n");
        break;
      }
      inbufferlength += retv;
      
      continue;
    } // else
    if (endptr == NULL) {
      inbufferlength = 0; // whole input is valid
      inbuffer[0] = '_'; // put some invalid character in it
    }
    else {
      // shift unread data to beginning of inbuffer
      shift = endptr - inbuffer;
      for (int i = shift; i < BUFFERSIZES; i ++)
        inbuffer[i-shift] = inbuffer[i];
      inbufferlength -= shift;
      inbuffer[inbufferlength] = '_';
    }
    
    //// write data
    if ( ! quiet) { printf(">"); fflush(stdout); }
    if (samplerate > 0) usleep(sleepmillis*1000);
    // format dataset
    millisecs = 0;
    if (printtimestamp) millisecs = getUnixMillis();
    retv = formatHalfbyte2Packet(outbuffer, BUFFERSIZES
                               , millisecs
                               , &nextNumber, 1);
    if (retv <= 0) {
      fprintf(stderr, "error while formatting packet, code: (%d)\n", retv);
      break;
    }
    // send data
    uds_write_toall(udsss, outbuffer, retv);
  }
  
  printf("closing file ...\n");
  fclose(csvf);
  
  printf("closing sockets ...\n");
  uds_stop_server(udsss);
  if (deleteSocketAfterUse) {
    printf("deleting socket file ...\n");
    unlink(socketpath);
  }
  exit(0);
}

