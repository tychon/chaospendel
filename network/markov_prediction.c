
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "x11draw.h"
#include "common.h"
#include "projectreader.h"
#include "uds_client.h"
#include "protocol.h"
#include "markov_chain.h"

void toPendulumCartesian(projectdata *pd, shmsurface *sf
                       , double scale
                       , int solindex
                       , double *x, double *y) {
  *x = sin(pd->sols[solindex][IDX_ANGLE]) * pd->sols[solindex][IDX_RADIUS] * scale;
  *y = cos(pd->sols[solindex][IDX_ANGLE]) * pd->sols[solindex][IDX_RADIUS] * scale;
  *x += sf->width/2;
  *y += sf->height/2;
}

void drawPendulum(shmsurface *sf, projectdata *pd
                , int tracklength, int *track) {
  // clear surface
  shmsurface_fill(sf, 0xff000000);
  
  // precompute scaling
  double maxpendlength = (double)(pd->l1 + (pd->l2a > pd->l2b ? pd->l2a : pd->l2b));
  // this scale is in pixels per meter
  double scale = (double)(sf->width < sf->height ? sf->width : sf->height) / 2.0 * (4.0/5.0) / maxpendlength;
  
  // draw center (main axis of pendulum)
  drawDot(sf, sf->width/2, sf->height/2, 0xffff0000);
  
  // draw fixed size blue circles
  double xpos, ypos;
  for (int i = 0; i < pd->solnum; i++) {
    toPendulumCartesian(pd, sf, scale, i, &xpos, &ypos);
    drawCircle(sf, xpos, ypos, 2, 0xff0000ff);
  }
  
  // draw last positions
  int color;
  double lastx, lasty;
  for (int i = tracklength-1; i >= 0; i--) {
    if (track[i] < 0) break;
    
    toPendulumCartesian(pd, sf, scale, track[i], &xpos, &ypos);
    color = 0xff000000;
    color |= (255 - 250/tracklength * i) << 8;
    fillCircle(sf, xpos, ypos, 5, color);
    if (i < tracklength-1)
      drawBresenhamLine(sf, lastx, lasty, xpos, ypos, color);
    
    lastx = xpos;
    lasty = ypos;
  }
}

int main(int argc, char *argv[]) {
  char *inputsocketpath = NULL;
  char *pendulumdatapath = NULL;
  int maxframerate = 80;
  int tracklength = 4;
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--pendulum|-p", argc, argv, &i, &pendulumdatapath) );
    else if (argcmpass("--inputsocket|-i", argc, argv, &i, &inputsocketpath) );
    else if (argcmpassint("--maxframerate|-fps", argc, argv, &i, &maxframerate) );
    else if (argcmpassint("--tracklength|-t", argc, argv, &i, &tracklength) );
    else fprintf(stderr, "warning: Ignoring unknown argument \"%s\"\n", argv[i]);
  }
  
  if (! inputsocketpath || ! pendulumdatapath) {
    printf("usage %s [--tracklength|-t INT] [--maxframerate|-fps INT] --pendulum|-p PATH --inputsocket|-i PATH\n", argv[0]);
    exit(1);
  }
  
  projectdata *pd = assert_malloc(sizeof(projectdata));
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  readPendulumData(pd, pendulumdatapath);
  
  shmsurface *surface = createSHMSurface(100, 100, 500, 500);
  
  int *track = assert_malloc(tracklength * sizeof(int));
  for (int i = 0; i < tracklength; i++) track[i] = -1;
  
  unsigned char buffer[GLOBALSEQPACKETSIZE];
  int bufferlength;
  struct packet4byte *packet = allocate4bytePacket(1);
  
  fprintf(stderr, "opening connection to server on \"%s\"\n", inputsocketpath);
  udsclientsocket *udscs = uds_create_client(inputsocketpath);
  
  // This is the number of milliseconds to sleep before flushing
  // the SHM surface again.
  const double minframewait = 1000000 / (double)maxframerate;
  // 'millis' is for saving current time,
  // 'lastframemillis' is for saving the time of the last frame flushed
  int micros, lastframemicros = getMicroseconds();
  int index;
  
  fprintf(stderr, "start reading data ...\n");
  while ( (bufferlength = uds_read(udscs, buffer, GLOBALSEQPACKETSIZE)) > 0) {
    // parse input data
    if (parse4bytePacket(buffer, bufferlength, packet, 1, 1) != 1) {
      fprintf(stderr, "Received invalid packet.\n");
      continue;
    }
    
    index = packet->values[0] - 1;
    if (index >= 0 && index != track[0]) {
      for (int i = tracklength-1; i > 0; i--) track[i] = track[i-1];
      track[0] = index;
      // print to console
      printf("%lld", packet->timestamp);
      for (int i = tracklength-1; i >= 0; i--) {
        printf("\t-> %d", track[i]);
      }
      printf("\n");
      fflush(stdout);
    }
    
    micros = getMicroseconds();
    if (micros-lastframemicros > minframewait) {
      drawPendulum(surface, pd
                 , tracklength, track);
      flushSHMSurface(surface);
      lastframemicros = micros;
    }
  }
  
  fprintf(stderr, "\nend of data\n");
  uds_close_client(udscs);
}

