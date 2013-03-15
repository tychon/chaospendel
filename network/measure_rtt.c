
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#include "common.h"
#include "memory_wrappers.h"
#include "uds_client.h"
#include "protocol.h"

static int nvalues = -1;
static int input_index = -1;

static udsclientsocket *udscs = NULL;

int getval() {
  unsigned char buffer[1024];
  int retv;
  if ((retv = uds_read(udscs, buffer, 1024)) == 0) {
    fprintf(stderr, "end of data\n");
    exit(1);
  }
  static struct packet2byte *parsed = NULL;
  if (parsed == NULL) parsed = allocate2bytePacket(nvalues);
  int retv2;
  if ( (retv2 = parse2bytePacket(buffer, retv
                         , parsed, 0/*timestamped*/
                         , nvalues)) < 0) {
    fprintf(stderr, "parsing failed!\n");
    exit(1);
  }
  return parsed->values[input_index];
}

long long ts_diff(struct timespec start, struct timespec end) {
  return 1000000000*((long long)end.tv_sec-start.tv_sec) + (end.tv_nsec-start.tv_nsec);
}

int main(int argc, char *argv[]) {
  setvbuf(stderr, NULL, _IONBF, 0);

  char *sockpath = NULL;
  int outpin = -1;
  int calibration_rounds = 5*500; /* ~5s for calibration */
  int rtt_rounds = 60; /* ~60s measurements by default */
  int cooldown_us = 500000;
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--inputsocket|-i", argc, argv, &i, &sockpath)) ;
    else if (argcmpassint("--nvalues|-v", argc, argv, &i, &nvalues)) ;
    else if (argcmpassint("--input-index|-I", argc, argv, &i, &input_index)) ;
    else if (argcmpassint("--outpin|-o", argc, argv, &i, &outpin)) ;
    else if (argcmpassint("--calibration-rounds|-c", argc, argv, &i, &calibration_rounds)) ;
    else if (argcmpassint("--rtt-rounds|-r", argc, argv, &i, &rtt_rounds)) ;
    else if (argcmpassint("--cooldown-us|-C", argc, argv, &i, &cooldown_us)) ;
    else fprintf(stderr, "warning: Unknown argument ignored: \"%s\"\n", argv[i]);
  }
  if (!sockpath || input_index < 0 || nvalues <= 0 || outpin < 0) {
    fprintf(stderr, "usage: %s --input-index|-I INPUTINDEX --inputsocket|-i SOCKETPATH --nvalues|-v NVALUES --outpin|-o OUTPIN [--cooldown-us|-C COOLDOWN_TIME]\n", argv[0]);
    exit(1);
  }
  
  fprintf(stderr, "opening socket on \"%s\"\n", sockpath);
  udscs = uds_create_client(sockpath);
  
  fprintf(stderr, "calibrating, please wait...\n");
  int rangemin = 10000, rangemax = 0, rangemid, rangebreadth;
  int steps_done = 0;
  char statebuf[] = "\r[                    ] ";
  fputs(statebuf, stderr);
  for (int i=0; i<calibration_rounds; i++) {
    int val = getval();
    if (val < rangemin) rangemin = val;
    else if (val > rangemax) rangemax = val;
    int steps_done_ = 20*i/calibration_rounds;
    if (steps_done_ != steps_done) {
      steps_done = steps_done_;
      statebuf[steps_done+1] = '#';
      fputs(statebuf, stderr);
    }
  }
  rangebreadth = rangemax - rangemin;
  rangemid = rangemin + rangebreadth;
  rangemin = rangemid - rangebreadth*2;
  rangemax = rangemid + rangebreadth*2;
  
  fprintf(stderr, " done.\n");
  
  fprintf(stderr, "performing %d rtt measurements, please wait...\n", rtt_rounds);
  for (int i=0; i<rtt_rounds;) {
    fprintf(stderr, "%d of %d done, measuring...                               \r", i, rtt_rounds);
    uds_empty_pipe(udscs);
    // start measuring
    struct timespec start_time, end_time;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_time);
    // send the ping
    uint8_t cmd;
    cmd = (outpin<<1)|1;
    uds_write(udscs, &cmd, 1);
    
    // wait 10000 frames (~5s) for a pong
    for (int j=0; j<10000; j++) {
      int val = getval();
      if (val < rangemin || val > rangemax) {
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_time);
        long long rtt = ts_diff(start_time, end_time);
        fprintf(stdout, "%lld\n", rtt);
        i++;
        fprintf(stderr, "%d of %d done, cooldown                                \r", i, rtt_rounds);
        goto prepare_nextround;
      } else {
        fprintf(stderr, "%d of %d done, current value: %d                       \r", i, rtt_rounds, val);
      }
    }
    fprintf(stderr, "%d of %d done, cooldown, last attempt was a failure        \r", i, rtt_rounds);

prepare_nextround:
    cmd = (outpin<<1)|1;
    uds_write(udscs, &cmd, 1);
    usleep(cooldown_us);
  }
  
  uds_close_client(udscs);
}

