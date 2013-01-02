
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

#define INTEGWINDOWSIZE  10000
#define INTEGTHRESHOLD  0.0004
#define INTEGRESETSAMPLES 100

#define DEGTORAD(deg) (deg / 360.0 * 2*M_PI)

void toCartesian(double radius, double angle, double *x, double *y) {
  *x = cos(angle) * radius;
  *y = sin(angle) * radius;
}
void toPendulumCartesian(double radius, double angle, double *x, double *y) {
  *x = sin(angle) * radius;
  *y = cos(angle) * radius;
}
//void toPolar(double x, double y, double *radius, double *angle)

void drawPendulum(shmsurface *sf, projectdata *pd, double absrangemax, double *normval, int intindex1, int intindex2, double intratio) {
  shmsurface_fill(sf, 0xff000000);
  
  double maxpendlength = (double)(pd->l1 + (pd->l2a > pd->l2b ? pd->l2a : pd->l2b));
  // this scale is in pixels per meter
  double scale = (double)(sf->width < sf->height ? sf->width : sf->height) / 2.0 * (4.0/5.0) / maxpendlength;
  
  drawDot(sf, sf->width/2, sf->height/2, 0xffff0000);
  //drawBresenhamLine(sf, xres * scale + sf->width/2, 0, xres * scale + sf->width/2, sf->height-1, 0xff0000ff);
  //drawBresenhamLine(sf, 0, yres * scale + sf->height/2, sf->width-1, yres * scale + sf->height/2, 0xff0000ff);
  
  double val;
  int xpos, ypos, color;
  for (int i = 0; i < pd->solnum; i++) {
    color = 0xff000000;
    val = (double)(normval[i]) / absrangemax * M_E;
    if (val < 0) {
      color |= htobe32((int)lround(log1p(-val)*255.0)) >> 8;
    } else if (val > 0) {
      color |= htobe32((int)lround(log1p(val))*255.0) >> 16;
    }
    
    xpos = sin(pd->sols[i][IDX_ANGLE]) * scale * (double)pd->sols[i][IDX_RADIUS] + sf->width/2;
    ypos = cos(pd->sols[i][IDX_ANGLE]) * scale * (double)pd->sols[i][IDX_RADIUS] + sf->height/2;
    drawCircle(sf, xpos, ypos, 6, 0xff0000ff);
    fillCircle(sf, xpos, ypos, 5, color);
  }
  
  if (intratio > 0) {
    double ax, ay, bx, by;
    toPendulumCartesian(pd->sols[intindex1][IDX_RADIUS]*scale, pd->sols[intindex1][IDX_ANGLE], &ax, &ay);
    ax += sf->width/2;
    ay += sf->height/2;
    toPendulumCartesian(pd->sols[intindex2][IDX_RADIUS]*scale, pd->sols[intindex2][IDX_ANGLE], &bx, &by);
    bx += sf->width/2;
    by += sf->height/2;
    //fprintf(stderr, "%d, %d\n", intindex1, intindex2);
    //fprintf(stderr, "%lf, %lf, %lf, %lf\n", ax, ay, bx, by);
    drawHyperbola(sf, ax, ay, bx, by, intratio, 0xffffffff);
  }
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
  
  double *lastnormvalue = assert_calloc(pd->solnum, sizeof(double));
  double *derivative = assert_malloc(sizeof(double) * pd->solnum);
  
  integral **integrals = assert_malloc(sizeof(integral*) * pd->solnum);
  for (int i = 0; i < pd->solnum; i++)
    integrals[i] = integral_allocate(INTEGWINDOWSIZE, INTEGTHRESHOLD, INTEGRESETSAMPLES);
  
  // some temporary variables used in loop
  double normval, integ, d1;
  
  // saves values and indices of two maximum integral values
  double absval1, absval2;
  int superindex1, superindex2;
  
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
    
    absval1 = absval2 = -1;
    for (int i = 0; i < pd->solnum; i++) {
      // normalize input value
      normval = (double)packet->values[i];
      normval -= pd->sols[i][IDX_MEAN];
      normval /= pd->sols[i][IDX_COILS];
      
      // calc integral
      integ = integral_push(integrals[i], normval);
      
      // first derivative
      d1 = normval - lastnormvalue[i];
      
      // store the values
      derivative[i] = d1;
      lastnormvalue[i] = normval;
      
      // comment?
      if (integ > 0) {
        if (fabs(integ) > absval1) {
          absval2 = absval1;
          superindex2 = superindex1;
          absval1 = fabs(integ);
          superindex1 = i;
        } else if (fabs(integ) > absval2) {
          absval2 = fabs(integ);
          superindex2 = i;
        }
      }
    }
    
    // calculate position of pendulum
    double ratio = absval1 / absval2; // TODO use squareroots?
    /*
    double x1, y1, x2, y2;
    toCartesian(pd->sols[superindex1][IDX_RADIUS], pd->sols[superindex1][IDX_ANGLE], &x1, &y1);
    toCartesian(pd->sols[superindex2][IDX_RADIUS], pd->sols[superindex2][IDX_ANGLE], &x2, &y2);
    double xres = x1 + (x2-x1) * ratio;
    double yres = y1 + (y2-y1) * ratio;
    */
    
    if (absval2 < 0.005) {
      ratio = -1.0;
    }
    
    if (showx11gui) {
      millis = getUnixMillis();
      if (millis-lastframemillis > minframewait) {
        drawPendulum(surface, pd, 0.1, lastnormvalue, superindex1, superindex2, 1/ratio); //TODO this is hardcoded :-(
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

