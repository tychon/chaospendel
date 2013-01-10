
#define _XOPEN_SOURCE 700 // for 'usleep' in unistd and 'getline' in stdio
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
#include "projectreader.h"

#define BUFFERSIZES 1024

/**
 * @returns the number of long long integers read.
 *  or -1 if the end of the file was reached (last dataset gets lost)
 */
int readCSVLine(FILE *f, long long *dest) {
  static char *buffer;
  if (! buffer) buffer = assert_malloc(BUFFERSIZES);
  size_t size = BUFFERSIZES;
  if (getline(&buffer, &size, f) < 0) {
    if (! feof(f)) {
      perror("reading from csv file");
      exit(1);
    } else return -1;
  }
  
  int destpos = 0;
  char *tmp = buffer;
  char *endptr;
  for (;;) {
    long long num = strtoll(tmp, &endptr, 10);
    if (endptr == tmp) {
      // no more valid digits found
      return destpos;
    }
    if (*endptr == '\0') {
      // end of line
      return destpos;
    }
    dest[destpos++] = num;
    (*endptr) = ' ';
    tmp = endptr;
  }
}

int main(int argc, char *argv[]) {
  int deleteOldSocket = 0;
  int deleteSocketAfterUse = 0;
  int quiet = 0;
  char *socketpath = "socket_arduino";
  char *pendulumdatapath = "data_pendulum";
  char *csvpath = "data_values_lastreplay.csv";
  int readtimestamp = 1; // if a timestamp is in csv file present
  int samplerate = -1; // default value: send as fast as we can
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--inputfile|-i", argc, argv, &i, &csvpath)) ;
    else if (argcmpass("--pendulum|-p", argc, argv, &i, &pendulumdatapath)) ;
    else if (argcmpass("--outputsocket|-o", argc, argv, &i, &socketpath)) ;
    else if (argcmpassint("--samplerate|-r", argc, argv, &i, &samplerate)) ;
    else if (ARGCMP("-f", i)) deleteOldSocket = 1;
    else if (ARGCMP("-d", i)) deleteSocketAfterUse = 1;
    else if (ARGCMP("-q", i) || ARGCMP("--quiet", i)) quiet = 1;
    else if (ARGCMP("--notimestamp", i) || ARGCMP("-nt", i)) readtimestamp = 0;
    else fprintf(stderr, "Ignoring unknown argument: \"%s\"\n", argv[i]);
  }
  
  if (! socketpath || ! csvpath || ! pendulumdatapath) {
    fprintf(stderr, "usage: %s [-f] [-d] [--quiet|-q] [--notimestamp|-nt] [--samplerate|-r SAMPLERATE] --inputfile|-i CSVFILEPATH --pendulum|-p PENDULUMDATAPATH --outputsocket|-o SOCKETPATH\n", argv[0]);
    exit(1);
  }
  
  projectdata *pd = assert_malloc(sizeof(projectdata));
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  readPendulumData(pd, pendulumdatapath);
  
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
  int sleepmicros = (int) (1.0/ ((double)samplerate) * 1000.0 * 1000.0);
  
  long long *parsedLLs = assert_malloc(sizeof(long long int) * pd->solnum);
  uint16_t *parsedShorts = assert_malloc(sizeof(uint16_t) * pd->solnum);
  int numcount, retv;
  unsigned char outbuffer[BUFFERSIZES]; // this buffer the protocol formatter writes to
  long long micros;
  
  // endless loop til end of csv file
  for (;;) {
    // parse data
    numcount = readCSVLine(csvf, parsedLLs);
    if (numcount == -1) {
      if ( ! quiet) printf(" end of data\n");
      break;
    }
    if (readtimestamp && numcount-1 != pd->solnum) {
      fprintf(stderr, "skipping invalid csv line, not exactly %d data values + 1 timestamp value found.\n", pd->solnum);
      continue;
    } else if (!readtimestamp && numcount != pd->solnum) {
      fprintf(stderr, "skipping invalid csv line, not exactly %d values found.\n", pd->solnum);
      continue;
    }
    
    if ( ! quiet) { putchar('>'); fflush(stdout); }
    // sleep
    if (samplerate > 0) usleep(sleepmicros);
    
    // extract timestamp (if it should exist)
    int lli = 0; // index iterating through parsedLLs
    if (readtimestamp) {
      micros = parsedLLs[lli++];
    } else micros = getMicroseconds();
    for (int i = 0; i < pd->solnum; i++) 
      parsedShorts[i] = parsedLLs[lli++];
    
    // format dataset
    retv = format2bytePacket(outbuffer, BUFFERSIZES
                           , micros
                           , parsedShorts, pd->solnum);
    if (retv <= 0) {
      fprintf(stderr, "error while formatting packet, code: (%d)\n", retv);
      break;
    }
    
    // send data
    uds_send_toall(udsss, outbuffer, retv);
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

