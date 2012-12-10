
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

#include "uds_server.h"

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
    else if (strcmp("--rate", argv[i]) == 0) {
      i ++;
      samplerate = atoi(argv[i]);
    }
    else if (strcmp("-d", argv[i]) == 0) deleteSocketAfterUse = 1;
    else fprintf(stderr, "Ignoring unknown argument: \"%s\"", argv[i]);
  }
  
  if (! socketpath || ! csvpath) {
    fprintf(stderr, "usage [-f] [-d] [--rate SAMPLERATE] -in CSVFILEPATH --socket SOCKETPATH\n");
    exit(1);
  }
  
  if (deleteOldSocket) unlink(socketpath);
  printf("opening socket with path \"%s\"\n", socketpath);
  udsserversocket *udsss = uds_create_server(socketpath);
  uds_start_server(udsss);
  
  printf("Opening csv file \"%s\" ...\n", csvpath);
  FILE *csvf = fopen(csvpath, "r");
  if (! csvf) {
    perror("opening csv file");
    exit(1);
  }
  
  if (samplerate <= 0) printf("Sending samples as fast as we can ...\n");
  else printf("Sendings a rate of %d samples per second ...\n", samplerate);
  int sleepmillis = (int) (1.0/ ((double)samplerate) * 1000.0);
  
  int bufferReadPos = 0;
  int bufferWritePos = 0;
  int lineLength = -1; // indicates if there is a full line in outbuffer and its length
  char buffer[BUFFERSIZES]; // this buffer the read method writes to
  struct timeb tmb; // for the timestamp
  struct tm timetm;
  long long millisecs;
  // enless loop til end of csv file
  for (;;) {
    if (lineLength >= 0) {
      printf(">"); fflush(stdout);
      if (samplerate > 0) usleep(sleepmillis*1000);
      // get time step
      ftime(&tmb);
      timetm = *localtime(& tmb.time);
      millisecs =                              (long long)tmb.millitm
                  +                       1000*(long long)timetm.tm_sec
                  +                    60*1000*(long long)timetm.tm_min
                  +                 60*60*1000*(long long)timetm.tm_hour
                  +              24*60*60*1000*(long long)timetm.tm_yday
                  +(long long)365*24*60*60*1000*(long long)timetm.tm_year;
      // send data
      uds_dprintf_all(udsss, "[%ld]", millisecs);
      uds_write_all(udsss, buffer+bufferReadPos, lineLength);
      bufferReadPos += lineLength;
    } else {
      //// read new data
      // shift data to beginning:
      for (int i = bufferReadPos; i < BUFFERSIZES; i++) buffer[i-bufferReadPos] = buffer[i];
      bufferWritePos -= bufferReadPos;
      bufferReadPos = 0;
      // read data from file
      int rval = fread(buffer+bufferReadPos, sizeof(char), BUFFERSIZES-bufferWritePos, csvf);
      printf("<\n");// fflush(stdin);
      if (rval < 0) {
        perror("reading from csv file");
      } else if (rval == 0) {
        printf("\nend of data.\n");
        break;
      }
      bufferWritePos += rval;
      assert(bufferWritePos <= BUFFERSIZES);
    }
    
    // skipt trailing newlines
    while (buffer[bufferReadPos] == '\n' && bufferReadPos < bufferWritePos) bufferReadPos ++;
    // find next lineLength
    lineLength = -1;
    for (int i = bufferReadPos; i < bufferWritePos; i++) {
      if (buffer[i] == '\n') {
        lineLength = i - bufferReadPos;
        break;
      }
    }
    if (lineLength == 0) {
      fprintf(stderr, "empty line\n");
      goto END;
    }
  }
  
END:
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

