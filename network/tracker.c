
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
#include "integral.h"

#define NOISEFACTOR 1.01
#define INTEGRESETSAMPLES 60

void toCartesian(double radius, double angle, double *x, double *y) {
  *x = cos(angle) * radius;
  *y = sin(angle) * radius;
}
void toPendulumCartesian(double radius, double angle, double *x, double *y) {
  *x = sin(angle) * radius;
  *y = cos(angle) * radius;
}

void drawPendulum(shmsurface *sf, projectdata *pd
                , double *normval, double normrangeabs
                , integral **integ, double integrange) {
  // clear surface
  shmsurface_fill(sf, 0xff000000);
  
  // precompute scaling
  double maxpendlength = (double)(pd->l1 + (pd->l2a > pd->l2b ? pd->l2a : pd->l2b));
  // this scale is in pixels per meter
  double scale = (double)(sf->width < sf->height ? sf->width : sf->height) / 2.0 * (4.0/5.0) / maxpendlength;
  
  int color;
  
  // draw center (main axis of pendulum)
  drawDot(sf, sf->width/2, sf->height/2, 0xffff0000);
  
  // draw norm value and integral for every solenoid
  double val, xpos, ypos, radius;
  for (int i = 0; i < pd->solnum; i++) {
    toPendulumCartesian(pd->sols[i][IDX_RADIUS] * scale, pd->sols[i][IDX_ANGLE], &xpos, &ypos);
    xpos += sf->width/2;
    ypos += sf->height/2;
    
    // make blue circle
    drawCircle(sf, xpos, ypos, 2, 0xff0000ff);
    
    // make colored circle
    
    // this stuff is currently not used
    color = 0xff000000;
    val = normval[i] / normrangeabs * M_E; // fit value into range of 0 to e
    if (val < 0) {
      color |= be32toh(htobe32((int)lround(log1p(-val)*255.0)) >> 8);
    } else if (val > 0) {
      color |= be32toh(htobe32((int)lround(log1p(val))*255.0) >> 16);
    }
    
    radius = integral_getsum(integ[i]) / integrange * 50.0;
    if (radius > 50) radius = 50;
    fillCircle(sf, xpos, ypos, radius, 0xff00ff00);
  }
}

double normalizeValue(double inputvalue, double *soldata) {
  inputvalue -= soldata[IDX_MEAN];
  
  inputvalue *= 1000;
  inputvalue /= soldata[IDX_COILS];
  
  inputvalue *= 1000;
  if (soldata[IDX_PARALLEL_RESISTANCE] > 0)
    inputvalue *= soldata[IDX_SELF_RESISTANCE] / soldata[IDX_PARALLEL_RESISTANCE];
  
  return inputvalue;
}

int main(int argc, char *argv[]) {
  char *socketpath = NULL, *outsockpath = NULL;
  char *pendulumdatapath = NULL;
  char *normalisationdatapath = NULL;
  int printtempdata = 0, showoverflows = 0;
  int showx11gui = 0;
  int maxframerate = 80;
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--pendulum|-p", argc, argv, &i, &pendulumdatapath)) ;
    else if (argcmpass("--normalisation|-n", argc, argv, &i, &normalisationdatapath)) ;
    else if (argcmpass("--inputsocket|-i", argc, argv, &i, &socketpath)) ;
    else if (argcmpass("--outputsocket|-o", argc, argv, &i, &outsockpath)) ;
    else if (argcmpassint("--maxframerate|-f", argc, argv, &i, &maxframerate)) ;
    else if (ARGCMP("--printtempdata", i)) printtempdata = 1;
    else if (ARGCMP("--showoverflows", i)) showoverflows = 1;
    else if (ARGCMP("--showx11gui", i)) showx11gui = 1;
    else fprintf(stderr, "warning: Unknown argument ignored: \"%s\"\n", argv[i]);
  }
  
  if (! socketpath || ! pendulumdatapath || ! normalisationdatapath || ! outsockpath) {
    printf("usage: %s [--showoverflows] [--printtempdata] [--showx11gui] [--maxframerate|-f INT] --pendulum|-p PATH --normalisation|-n PATH --inputsocket|-i PATH --outputsocket|-o PATH\n", argv[0]);
    exit(1);
  }
  
  projectdata *pd = assert_malloc(sizeof(projectdata));
  
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  readPendulumData(pd, pendulumdatapath);
  
  fprintf(stderr, "reading normalisation data from \"%s\" ...\n", normalisationdatapath);
  readNormalisationData(pd, normalisationdatapath);
  
  fprintf(stderr, "opening connection to server on \"%s\"\n", socketpath);
  udsclientsocket *udscs = uds_create_client(socketpath);
  
  //fprintf(stderr, "opening server socket with path \"%s\"\n", outsockpath);
  //udsserversocket *udsss = uds_create_server(outsockpath);
  //uds_start_server(udsss);
  
  // x11 things
  shmsurface *surface = NULL;
  if (showx11gui) {
    surface = createSHMSurface(100, 100, 500, 500);
  }
  
  // precompute ranges of noise
  double *noiseabs = assert_malloc(sizeof(double) * pd->solnum);
  double noiseminabs, noisemaxabs;
  for (int i = 0; i < pd->solnum; i++) {
    noiseminabs = fabs(pd->sols[i][IDX_NOISEMIN]);
    noisemaxabs = fabs(pd->sols[i][IDX_NOISEMAX]);
    noiseabs[i] = noiseminabs >= noisemaxabs ? noiseminabs : noisemaxabs;
    noiseabs[i] *= NOISEFACTOR;
    noiseabs[i] = normalizeValue(noiseabs[i], pd->sols[i]);
  }
  
  // here are some values stored that are recomputed for every dataset
  double *lastnormvalue = assert_calloc(pd->solnum, sizeof(double));
  double *derivative = assert_malloc(sizeof(double) * pd->solnum);
  integral **integrals = assert_malloc(sizeof(integral*) * pd->solnum);
  for (int i = 0; i < pd->solnum; i++)
    integrals[i] = integral_allocate(noiseabs[i], INTEGRESETSAMPLES);
  
  // some temporary variables used in loop
  double normval, integ, d1;
  
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
    
    // look for invalid data before calculation
    int invalid = 0;
    for (int i = 0; i < pd->solnum; i++) {
      normval = (double)packet->values[i];
      if (normval <= 0 || normval >= 1023) invalid = 1;
    }
    if (invalid) {
      if (showoverflows) {
        fputc('!',stderr);
        fflush(stderr);
      }
      continue;
    }
    
    for (int i = 0; i < pd->solnum; i++) {
      // normalize input value
      normval = normalizeValue((double)packet->values[i], pd->sols[i]);
      
      // calc integral
      integ = integral_push(integrals[i], normval);
      
      // first derivative
      d1 = normval - lastnormvalue[i];
      
      // store the values
      derivative[i] = d1;
      lastnormvalue[i] = normval;
    }
    
    if (showx11gui) {
      millis = getUnixMillis();
      if (millis-lastframemillis > minframewait) {
        //TODO remove hard coded ranges
        drawPendulum(surface, pd
                   , lastnormvalue, 60000
                   , integrals, 500000);
        flushSHMSurface(surface);
        lastframemillis = millis;
      }
    }
    
    if (printtempdata) {
      // print results of calculations in csv format to stdout
      // normalised values:
      for (int i = 0; i < pd->solnum; i++) {
        if (i == 0) printf("%lf", lastnormvalue[i]);
        else printf(",%lf", lastnormvalue[i]);
      }
      // integrals:
      for (int i = 0; i < pd->solnum; i++)
        printf(",%lf", integral_getsum(integrals[i]));
      // derivatives of normalised values:
      for (int i = 0; i < pd->solnum; i++)
        printf(",%lf", derivative[i]);
      
      fputc('\n', stdout);
      fflush(stdout);
    }
    
    // TODO find pendulum
    // TODO calculate angles
    
    //TODO write output
    /*
    // format dataset
    bufferlength = format2bytePacket(buffer, BUFFERSIZES
                                   , packet->timestamp
                                   , [DATA], 2);
    if (bufferlength <= 0) {
      fprintf(stderr, "error while formatting packet, code: (%d)\n", bufferlength);
      break;
    } else
      uds_send_toall(udsss, buffer, bufferlength);
    */
  }
  
  fprintf(stderr, "end of data\n");
  uds_close_client(udscs);
  //uds_stop_server(udsss);
  unlink(outsockpath);
}

