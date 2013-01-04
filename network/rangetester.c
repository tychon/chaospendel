
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <endian.h>

#include "common.h"
#include "projectreader.h"
#include "uds_server.h"
#include "uds_client.h"
#include "protocol.h"
#include "x11draw.h"

#define BARHEIGHT 200
#define BARWIDTH 30

int main(int argc, char *argv[]) {
  char *socketpath = NULL;
  char *pendulumdatapath = NULL;
  char *normalisationdatapath = NULL;
  int maxframerate = 80;
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--pendulum|-p", argc, argv, &i, &pendulumdatapath)) ;
    else if (argcmpass("--normalisation|-n", argc, argv, &i, &normalisationdatapath)) ;
    else if (argcmpass("--inputsocket|-i", argc, argv, &i, &socketpath)) ;
    else if (argcmpassint("--maxframerate|-f", argc, argv, &i, &maxframerate)) ;
    else fprintf(stderr, "warning: Unknown argument ignored: \"%s\"\n", argv[i]);
  }
  
  if (! socketpath || ! pendulumdatapath || ! normalisationdatapath) {
    printf("usage: %s [--maxframerate|-f INT] --pendulum|-p PATH --normalisation|-n PATH --inputsocket|-i PATH\n", argv[0]);
    exit(1);
  }
  
  projectdata *pd = assert_malloc(sizeof(projectdata));
  
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  readPendulumData(pd, pendulumdatapath);
  
  fprintf(stderr, "reading normalisation data from \"%s\" ...\n", normalisationdatapath);
  readNormalisationData(pd, normalisationdatapath);
  
  fprintf(stderr, "opening connection to server on \"%s\"\n", socketpath);
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  // the x11 thing
  shmsurface *surface = createSHMSurface(100, 100, BARWIDTH*pd->solnum, BARHEIGHT);
  
  // precompute some values
  const double scale = 1; //TODO
  
  // precompute ranges of noise
  double *noiseabs = assert_malloc(sizeof(double) * pd->solnum);
  double noiseminabs, noisemaxabs;
  for (int i = 0; i < pd->solnum; i++) {
    noiseminabs = fabs(pd->sols[i][IDX_NOISEMIN]);
    noisemaxabs = fabs(pd->sols[i][IDX_NOISEMAX]);
    noiseabs[i] = noiseminabs >= noisemaxabs ? noiseminabs : noisemaxabs;
  }
  
  double *fallingmax = malloc(sizeof(double) * pd->solnum);
  double *fallingmin = malloc(sizeof(double) * pd->solnum);
  
  // some temporary variables used in loop
  double normval;
  
  // This is the number of milliseconds to sleep before flushing
  // the SHM surface again.
  const double minframewait = 1000 / (double)maxframerate;
  // 'millis' is for saving current time,
  // 'lastframemillis' is for saving the time of the last frame flushed
  int millis, lastframemillis = getUnixMillis();
  
  unsigned char buffer[GLOBALSEQPACKETSIZE];
  int bufferlength;
  struct packet2byte *packet = allocate2bytePacket(pd->solnum);
  
  fprintf(stderr, "start reading data ...\n");
  while ( (bufferlength = uds_read(udscs, buffer, GLOBALSEQPACKETSIZE)) > 0) {
    if (parse2bytePacket(buffer, bufferlength, packet, 1, pd->solnum) != pd->solnum) {
      fprintf(stderr, "Received invalid packet.\n");
      continue;
    }
    
    for (int i = 0; i < pd->solnum; i++) {
      // normalize input value
      normval = (double)packet->values[i];
      normval /= pd->sols[i][IDX_COILS];
    }
    
    millis = getUnixMillis();
    if (millis-lastframemillis > minframewait) {
      // draw pendulum
      
      drawBresenhamLine(surface, 0, 0, BARWIDTH, BARHEIGHT-1, 0xff00ff00);
      
      // show the drawing
      flushSHMSurface(surface);
      lastframemillis = millis;
    }
  }
  
  fprintf(stderr, "end of data\n");
  uds_close_client(udscs);
}

