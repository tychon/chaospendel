
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <limits.h>

#include "common.h"
#include "projectreader.h"
#include "uds_server.h"
#include "uds_client.h"
#include "protocol.h"
#include "x11draw.h"

#define BARHEIGHT 1024
#define BARWIDTH 30

int main(int argc, char *argv[]) {
  char *socketpath = "socket_arduino";
  char *pendulumdatapath = "data_pendulum";
  char *normalisationdatapath = "data_normalisation";
  int maxframerate = 80;
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--pendulum|-p", argc, argv, &i, &pendulumdatapath)) ;
    else if (argcmpass("--normalisation|-n", argc, argv, &i, &normalisationdatapath)) ;
    else if (argcmpass("--inputsocket|-i", argc, argv, &i, &socketpath)) ;
    else if (argcmpassint("--maxframerate|-f", argc, argv, &i, &maxframerate)) ;
    else fprintf(stderr, "warning: Unknown argument ignored: \"%s\"\n", argv[i]);
  }
  
  if (! socketpath || ! pendulumdatapath || ! normalisationdatapath) {
    printf("usage: %s [--maxframerate|-f INT] [--pendulum|-p PATH] [--normalisation|-n PATH] [--inputsocket|-i PATH]\n", argv[0]);
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
  shmsurface *surface = createSHMSurface(100, 10, BARWIDTH*pd->solnum, BARHEIGHT);
  
  // precompute some values
  const double scale = ((double)BARHEIGHT-1.0) / (pd->inputrangemax - pd->inputrangemin);
  
  // precompute positions of mean and noise
  double *meanypos = assert_malloc(sizeof(double) * pd->solnum);
  double *noiseypos = assert_malloc(sizeof(double) * pd->solnum);
  double *noiseyheight = assert_malloc(sizeof(double) * pd->solnum);
  for (int i = 0; i < pd->solnum; i++) {
    meanypos[i] = BARHEIGHT - (pd->sols[i][IDX_MEAN] - pd->inputrangemin) * scale - 1;
    noiseypos[i] = BARHEIGHT - (pd->sols[i][IDX_NOISEMAX] - pd->inputrangemin) * scale - 1;
    noiseyheight[i] = (pd->sols[i][IDX_NOISEMAX] - pd->sols[i][IDX_NOISEMIN] - pd->inputrangemin) * scale;
  }
  
  double *fallingmin = malloc(sizeof(double) * pd->solnum);
  for (int i = 0; i < pd->solnum; i++) fallingmin[i] = pd->inputrangemax;
  double *fallingmax = malloc(sizeof(double) * pd->solnum);
  for (int i = 0; i < pd->solnum; i++) fallingmax[i] = pd->inputrangemin;
  
  // some temporary variable used in loop
  double value, y, yheight;
  
  // This is the number of milliseconds to sleep before flushing
  // the SHM surface again.
  const double minframewait = 1000000 / (double)maxframerate;
  // 'millis' is for saving current time,
  // 'lastframemillis' is for saving the time of the last frame flushed
  int micros, lastframemicros = getMicroseconds();
  
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
      value = (double) packet->values[i];
      if (value > fallingmax[i]) fallingmax[i] = value;
      if (value < fallingmin[i]) fallingmin[i] = value;
    }
    
    micros = getMicroseconds();
    if (micros-lastframemicros > minframewait) {
      shmsurface_fill(surface, COLOR_BLACK);
      
      // draw bars
      for (int i = 0; i < pd->solnum; i++) {
        value = (double) packet->values[i];
        
        // draw blue rect for noise range
        fillRect(surface, BARWIDTH*i, noiseypos[i], BARWIDTH-1, noiseyheight[i], COLOR_BLUE);
        
        // draw rect for current position
        y = BARHEIGHT - ((value - pd->inputrangemin) * scale);
        yheight = y - meanypos[i];
        yheight -= yheight > 0 ? 1 : -1;
        fillRect(surface, BARWIDTH*i, meanypos[i], BARWIDTH-1, yheight, COLOR_GREEN);
        
        // draw minimum
        y = BARHEIGHT - (fallingmin[i] - pd->inputrangemin) * scale - 1;
        drawBresenhamLine(surface, BARWIDTH*i, y, BARWIDTH*(i+1)-1, y, COLOR_RED);
        fallingmin[i] += 1/scale;
        
        // draw maximum
        y = BARHEIGHT - (fallingmax[i] - pd->inputrangemin) * scale - 1;
        drawBresenhamLine(surface, BARWIDTH*i, y, BARWIDTH*(i+1)-1, y, COLOR_RED);
        fallingmax[i] -= 1/scale;
        
        // draw grey separator
        drawBresenhamLine(surface, BARWIDTH*(i+1)-1, 0, BARWIDTH*(i+1)-1, BARHEIGHT-1, 0xff505050);
      }
      
      // show the drawing
      flushSHMSurface(surface);
      lastframemicros = micros;
    }
  }
  
  fprintf(stderr, "end of data\n");
  uds_close_client(udscs);
}

