
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "x11draw.h"
#include "common.h"
#include "projectreader.h"
#include "uds_client.h"
#include "protocol.h"
#include "markov_chain.h"

double getPolarDistance(projectdata *pd, int solindex1, int solindex2) {
  double r1 = pd->sols[solindex1][IDX_RADIUS] * pd->sols[solindex1][IDX_RADIUS];
  double r2 = pd->sols[solindex2][IDX_RADIUS] * pd->sols[solindex2][IDX_RADIUS];
  double phi1 = pd->sols[solindex1][IDX_RADIUS] * pd->sols[solindex1][IDX_ANGLE];
  double phi2 = pd->sols[solindex2][IDX_RADIUS] * pd->sols[solindex2][IDX_ANGLE];
  
  return sqrt(r1*r1 + r2*r2 - 2*r1*r2*cos(phi1-phi2));
}

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
                , int tracklength, int *track
                , int nextindex, double nextprob) {
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
  if (nextindex >= 0) {
    toPendulumCartesian(pd, sf, scale, nextindex, &xpos, &ypos);
    double radius = nextprob * 20;
    fillCircle(sf, xpos, ypos, radius, COLOR_BLUE);
    drawBresenhamLine(sf, lastx, lasty, xpos, ypos, COLOR_BLUE);
  }
}

/**
 * This function sorts the given 'velocity' into one of the ranges, given int
 * the projectdata and returns the ranges index.
 * There are a number of projectdata.markovvelrangenum ranges and the maximum
 * velocity is projectdata.markovvelmax. Every velocity above this maximum is
 * assigned to the last range and a error message is given on stderr.
 */
int encodeVelocityRangeIndex(projectdata *pd, double velocity) {
  for (int i = 1; i <= pd->markovvelrangenum; i++) {
    if (velocity < pd->markovvelmax/pd->markovvelrangenum*i) return i-1;
  }
  fprintf(stderr, "!WARNING!: velocity out of range!\n");
  return pd->markovvelrangenum-1;
}

/**
 * Encodes the indices of the solenoids and the velocity unambiguously into one
 * integer. First first solenoid index is in the most insignificant place of the
 * encoded integer, the velocity at the most significant end.
 * @param range The range of the indices from zero (included) to range (excluded).
 * @param indicesnum The length of the integer array in 'indices'
 * @param indices The array of indices in the given range
 * @param velocityrange the range of the velocity
 */
int encodeIndex(int range, int indicesnum, int *indices, int velocityrange) {
  int enc = 0;
  int basemult = 1;
  for (int i = 0; i < indicesnum; i++) {
    enc += indices[i] * basemult;
    basemult *= range;
  }
  enc += velocityrange * basemult;
  return enc;
}

void decodeIndex(long encoded, int range, int indicesnum, int *indices, int *velocity) {
  //rintf("decoding:\n");
  double basemult = pow(range, indicesnum);
  *velocity = (int) floor((double)encoded / basemult);
  //printf("velocity: %d = floor(%ld / %lf)\n", *velocity, encoded, basemult);
  
  long largerindexpart = (*velocity) * basemult;
  for (int i = indicesnum-1; i >= 0; i--) {
    basemult /= range;
    indices[i] = (int) floor(((double)encoded - largerindexpart) / basemult);
    //printf("dec (%d) %d = floor( (%ld - %ld) / %lf)\n", i, indices[i], encoded, largerindexpart, basemult);
    largerindexpart += indices[i] * basemult;
  }
}

int main(int argc, char *argv[]) {
  char *inputsocketpath = "socket_integrals";
  char *pendulumdatapath = "data_pendulum";
  char *markovinputpath = "data_markovchain";
  char *markovoutputpath = NULL;
  int maxframerate = 80;
  char *manipulatorsocketpath = NULL;
  int minimizeenergy = 0;
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--pendulum|-p", argc, argv, &i, &pendulumdatapath) );
    else if (argcmpass("--inputsocket|-i", argc, argv, &i, &inputsocketpath) );
    else if (argcmpass("--markovinput|-mi", argc, argv, &i, &markovinputpath) );
    else if (argcmpass("--markovoutput|-mo", argc, argv, &i, &markovoutputpath) );
    else if (argcmpass("--manip|-m", argc, argv, &i, &manipulatorsocketpath) );
    else if (argcmpassint("--maxframerate|-fps", argc, argv, &i, &maxframerate) );
    else if (ARGCMP("--minen", i)) minimizeenergy = 1;
    else fprintf(stderr, "warning: Ignoring unknown argument \"%s\"\n", argv[i]);
  }
  
  if (! inputsocketpath || ! pendulumdatapath) {
    printf("usage %s [--maxframerate|-fps INT] [--markovinput|-mi PATH] [--markovoutput|-mo PATH] [--pendulum|-p PATH] [--inputsocket|-i PATH] [--minen --manip|-m PATH]\n", argv[0]);
    exit(1);
  }
  
  projectdata *pd = assert_malloc(sizeof(projectdata));
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  readPendulumData(pd, pendulumdatapath);
  
  shmsurface *surface = createSHMSurface(100, 100, 500, 500);
  
  int *track = assert_malloc(pd->markovtracklength * sizeof(int));
  for (int i = 0; i < pd->markovtracklength; i++) track[i] = -1;
  long long *tracktimes = assert_malloc(pd->markovtracklength * sizeof(long long));
  
  unsigned char buffer[GLOBALSEQPACKETSIZE];
  int bufferlength;
  struct packet4byte *packet = allocate4bytePacket(1);
  
  // This is the number of milliseconds to sleep before flushing
  // the SHM surface again.
  const double minframewait = 1000000 / (double)maxframerate;
  // 'millis' is for saving current time,
  // 'lastframemillis' is for saving the time of the last frame flushed
  int micros, lastframemicros = getMicroseconds();
  
  int realtracklength = 0;
  long long timediff;
  long laststateindex = -1, stateindex = -1; // they do not need to be longs
  int index, velocityrangeindex, nextstateindex = -1;
  double dist, velocity;
  
  markovchainmatrix *mcm = allocateMarkovChain(pow(pd->solnum, pd->markovtracklength) * pd->markovvelrangenum);
  if (markovinputpath) {
    fprintf(stderr, "reading markov chain data from \"%s\"...\n", markovinputpath);
    fflush(stderr);
    markovchain_readDataFile(markovinputpath, mcm);
  }
  int *nexttrack = assert_malloc(pd->markovtracklength * sizeof(int));
  int nextvelocityrange, nextsolindex = -1;
  double probability;
  int predictionfitcount = 0, predictionfailcount = 0;
  
  udsclientsocket *manipulatorsocket = NULL;  
  if (manipulatorsocketpath) {
    fprintf(stderr, "opening connection to manipulation server on \"%s\"\n", manipulatorsocketpath);
    manipulatorsocket = uds_create_client(manipulatorsocketpath);
  }
  
  fprintf(stderr, "opening connection to server on \"%s\"\n", inputsocketpath);
  udsclientsocket *udscs = uds_create_client(inputsocketpath);
  
  fprintf(stderr, "start reading data ...\n");
  while ( (bufferlength = uds_read(udscs, buffer, GLOBALSEQPACKETSIZE)) > 0) {
    // parse input data
    if (parse4bytePacket(buffer, bufferlength, packet, 1, 1) != 1) {
      fprintf(stderr, "Received invalid packet.\n");
      continue;
    }
    
    // this is the index of the solenoid with the strongest current magnetic field
    index = packet->values[0] - 1; 
    
    if (index >= 0 && index != track[0]) {
      // the solenoid with the stronges field has changed
      
      // add it to track
      for (int i = pd->markovtracklength-1; i > 0; i--) {
        track[i] = track[i-1];
        tracktimes[i] = tracktimes[i-1];
      }
      track[0] = index;
      tracktimes[0] = packet->timestamp;
      if (realtracklength < pd->markovtracklength) realtracklength ++;
      
      if (realtracklength == pd->markovtracklength) {
        // the track is filled, hence we can make reasonable statements
        
        // calculate velocity
        dist = 0;
        timediff = 0;
        for (int i = 1; i < pd->markovtracklength && track[i] >= 0; i ++) {
          timediff = tracktimes[0] - tracktimes[i];
          dist += getPolarDistance(pd, track[i-1], track[i]);
        }
        
        if (timediff > 0) velocity = dist / (double)((long double)timediff / 1000000.0);
        else {
          fprintf(stderr, "Whow, thats tooo fast!\n");
          exit(1);
        }
        velocityrangeindex = encodeVelocityRangeIndex(pd, velocity);
        
        // make some output
        printf("%lld  d=%lf  v=%lf (%d)", packet->timestamp, dist, velocity, velocityrangeindex);
        if (nextsolindex >= 0) {
          if (index == nextsolindex) {
            predictionfitcount ++;
            printf("\tok! ");
          } else if (nextstateindex >= 0) {
            predictionfailcount ++;
            printf("\tfail");
          }
        } else printf("\t    ");
        
        // encode the index
        stateindex = encodeIndex(pd->solnum, pd->markovtracklength, track, velocityrangeindex);
        
        // add encoded index to markov chain
        if (laststateindex >= 0)
          markovchain_addsample(mcm, laststateindex, stateindex);
        laststateindex = stateindex;
        
        // make prediction
        nextstateindex = markovchain_getMostProbableNextState(mcm, stateindex);
        if (nextstateindex >= 0) {
          // there is a prediction for the current state
          // calculate the probability of the best prediction:
          probability = markovchain_getprob(mcm, stateindex, nextstateindex);
          // get the first solenoid of the predicted track:
          decodeIndex(nextstateindex, pd->solnum, pd->markovtracklength, nexttrack, &nextvelocityrange);
          nextsolindex = nexttrack[0];
          // print some output
          printf(" nextsol=%d\tnextvrange=%d prob=%2.1lf%%, at %d samples", nextsolindex, nextvelocityrange, probability, markovchain_getSamplesAt(mcm, stateindex));
          printf("\tfit: %2.2lf%%", (double)predictionfitcount / (double)(predictionfitcount + predictionfailcount) * 100.0);
          
          int cmd;
          if (minimizeenergy) {
            if (index == 12) {
              printf(" on ");
              cmd = (1 << 1) | 1;
            } else {
              printf(" off");
              cmd = (1 << 1) | 0;
            }
            uds_write(manipulatorsocket, &cmd, 1);
            if (index == 13) {
              printf(" on ");
              cmd = (2 << 1) | 1;
            } else {
              printf(" off");
              cmd = (2 << 1) | 0;
            }
            uds_write(manipulatorsocket, &cmd, 1);
          }
        } else {
          // aw, no prediction possible, this state has never occurred before.
          nextsolindex = -1;
        }
        
        printf("\n");
        fflush(stdout);
      }
    }
    
    // draw image if necessary
    micros = getMicroseconds();
    if (micros-lastframemicros > minframewait) {
      drawPendulum(surface, pd
                 , pd->markovtracklength, track, nextsolindex, probability);
      flushSHMSurface(surface);
      lastframemicros = micros;
    }
  }
  
  fprintf(stderr, "\nend of data\n");
  if (markovoutputpath) {
    fprintf(stderr, "writing markov chain data to \"%s\"...\n", markovoutputpath);
    markovchain_writeDataFile(mcm, markovoutputpath);
  }
  if (manipulatorsocketpath) {
    uds_close_client(manipulatorsocket);
  }
  uds_close_client(udscs);
  
}

