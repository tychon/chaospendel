
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "common.h"
#include "projectreader.h"
#include "uds_server.h"
#include "uds_client.h"
#include "protocol.h"

int main(int argc, char *argv[]) {
  char *socketpath = NULL, *outsockpath = NULL;
  char *pendulumdatapath = NULL;
  char *normalisationdatapath = NULL;
  int printtempdata = 0;
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--pendulum|-p", argc, argv, &i, &pendulumdatapath)) ;
    else if (argcmpass("--normalisation|-n", argc, argv, &i, &normalisationdatapath)) ;
    else if (argcmpass("--inputsocket|-i", argc, argv, &i, &socketpath)) ;
    else if (argcmpass("--outputsocket|-o", argc, argv, &i, &outsockpath)) ;
    else if (ARGCMP("--printtemp", i)) printtempdata = 1;
    else {
      fprintf(stderr, "Unknown argument ignored: \"%s\"\n", argv[i]);
    }
  }
  
  if (! socketpath || ! pendulumdatapath || ! normalisationdatapath || ! outsockpath) {
    printf("usage: %s --pendulum|-p PATH --normalisation|-n PATH --inputsocket|-i PATH --outputsocket|-o PATH\n", argv[0]);
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
  
  double *lastnormvalue = assert_calloc(pd->solnum, sizeof(double));
  double *derivative1 = assert_malloc(sizeof(double) * pd->solnum);
  double *derivative2 = assert_malloc(sizeof(double) * pd->solnum);
  double normval, d1, d2;
  
  unsigned char buffer[GLOBALSEQPACKETSIZE];
  int bufferlength;
  struct packet2byte *packet = allocate2bytePacket(pd->solnum);
  
  fprintf(stderr, "start reading data ...\n");
  while ( (bufferlength = uds_read(udscs, buffer, GLOBALSEQPACKETSIZE)) > 0) {
    if (parse2bytePacket(buffer, bufferlength, packet, 1, pd->solnum) != pd->solnum) {
      fprintf(stderr, "Received invalid packet.\n");
      continue;
    }
    
    int inversion = 0;
    for (int i = 0; i < pd->solnum; i++) {
      // normalize input value
      normval = (double)packet->values[i];
      normval -= pd->sols[i][IDX_MEAN];
      normval /= pd->sols[i][IDX_STD_DEVIATION];
      normval /= pd->sols[i][IDX_COILS];
      // first derivative
      d1 = normval - lastnormvalue[i];
      // second derivative
      d2 = derivative1[i] - d1;
      
      // find inversion
      if ((derivative1[i] > 0.006) && derivative1[i] >= d2) inversion = i+1;
      
      // store the values
      derivative1[i] = d1;
      derivative2[i] = d2;
      lastnormvalue[i] = normval;
    }
    
    if (printtempdata) {
      for (int i = 0; i < pd->solnum; i++) {
        if (i == 0) printf("%f", lastnormvalue[i]);
        else printf(",%f", lastnormvalue[i]);
      }
      for (int i = 0; i < pd->solnum; i++)
        printf(",%f", derivative1[i]);
      //for (int i = 0; i < pd->solnum; i++)
      //  printf(",%f", derivative2[i]);
      printf(",%d\n", inversion);
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

