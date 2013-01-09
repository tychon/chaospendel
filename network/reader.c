
#include <termios.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <assert.h>

#include "common.h"
#include "projectreader.h"
#include "uds_server.h"
#include "protocol.h"

#define SAMPLERATEBUFFERSIZE 100
#define SAMPLERATEREFRESHWAIT 100000 // every tenth second

int main(int argc, char *argv[]) {
  char *serialdevicepath = "/dev/ttyACM0";
  char *outputsocketpath = "socket_arduino";
  char *pendulumdatapath = "data_pendulum";
  char *replaypath = "data_values_lastreplay.csv";
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--serialdevice|-i", argc, argv, &i, &serialdevicepath)) ;
    else if (argcmpass("--outputsocketpath|-o", argc, argv, &i, &outputsocketpath)) ;
    else if (argcmpass("--pendulum|-p", argc, argv, &i, &pendulumdatapath)) ;
    else if (argcmpass("--savereplay|-r", argc, argv, &i, &replaypath)) ;
    else if (ARGCMP("--noreplay", i)) replaypath = NULL;
    else fprintf(stderr, "warning: Ignored unknown argument \"%s\"\n", argv[i]);
  }
  
  fprintf(stderr, "Reading pendulum data ...\n");
  projectdata *pd = assert_malloc(sizeof(projectdata));
  readPendulumData(pd, pendulumdatapath);
  
  fprintf(stderr, "Connecting to serial device ...\n");
  int serial_fd = open(serialdevicepath, O_RDWR | O_NOCTTY);
  struct termios config;
  if(tcgetattr(serial_fd, &config) < 0) exit(1);
  config.c_iflag &= ~(IGNBRK | BRKINT | ICRNL | INLCR | PARMRK | INPCK | ISTRIP | IXON);
  config.c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN | ISIG);
  config.c_cflag &= ~(CSIZE | PARENB);
  config.c_cflag |= CS8;
  config.c_cc[VMIN]  = 1;
  config.c_cc[VTIME] = 0;
  if(cfsetispeed(&config, B500000) < 0 || cfsetospeed(&config, B500000) < 0) exit(1);
  if(tcsetattr(serial_fd, TCSAFLUSH, &config) < 0) exit(1);
  FILE *serial = fdopen(serial_fd, "r");
  
  setbuf(stdin, NULL);
  fcntl(0/*stdin*/, F_SETFL, O_NONBLOCK);
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);
  setbuf(serial, NULL);
  
  int adc_was_blocking = -1;
  int wantport = 0;
  
  unsigned char outbuffer[GLOBALSEQPACKETSIZE];
  uint16_t values[pd->solnum];

  long long lastprintmicros = getMicroseconds();  
  long long *sampletimes = assert_calloc(SAMPLERATEBUFFERSIZE, sizeof(long long));
  int sampletimespos = 0;
  long long micros, firstmicros, samplerate;
  
  fprintf(stderr, "Starting unix domain server on \"%s\" ...\n", outputsocketpath);
  unlink(outputsocketpath);
  udsserversocket *udsserver = uds_create_server(outputsocketpath);
  uds_start_server(udsserver);
  
  FILE *replayf = NULL;
  if (replaypath) {
    fprintf(stderr, "Opening replay file \"%s\" ...\n", replaypath);
    if (! (replayf = fopen(replaypath, "w+"))) {
      perror("opening replay file");
      exit(1);
    }
  }

  while (1) {
    {
      char c;
      while (read(0, &c, 1) == 1) {
        if (c != '+' && c != '-') continue;
        uint8_t val = (c == '+') ? 42 : 41;
        if (write(serial_fd, &val, 1) != 1) {
          assert(0);
        }
      }
    }
    char b;
    while (((b=fgetc(serial))&0xe0) != 0xa0) {
      // ignore bad head
      fprintf(stderr, "bad byte 0x%x - waiting for valid head\n", b);
    };
    
    int adc_blocked = (b&0x10) ? true : false;
    int port = (b&0x0f);
    int hpart = fgetc(serial);
    if (hpart&0x80) { ungetc(hpart, serial); goto badval; }
    int lpart = fgetc(serial);
    if (lpart&0x80) { ungetc(lpart, serial); goto badval; }
    // bits 9,10,11,12 are duplicated - make sure they're the same
    if ((hpart&0x0f) != (lpart>>3)) goto badval;
    int val = (hpart<<3)|lpart;
    if (port != wantport) {
      fprintf(stderr, "want port %i, but got data about port %i\n", wantport, port);
      continue;
    }
    
    values[port] = val;
    
    wantport++;
    if (wantport == pd->solnum) {
      // collected one data set
      wantport = 0;
      
      micros = getMicroseconds();
      
      // send it over the socket
      int size = format2bytePacket(outbuffer, GLOBALSEQPACKETSIZE, micros, values, pd->solnum);
      uds_send_toall(udsserver, outbuffer, size);
      
      // write to replay
      if (replaypath) {
        fprintf(replayf, "%lld", micros);
        for (int i = 0; i < pd->solnum; i++)
          fprintf(replayf, ", %d", values[i]);
        fputc('\n', replayf);
      }
      
      firstmicros = sampletimes[sampletimespos];
      sampletimes[sampletimespos] = micros;
      sampletimespos ++;
      if (sampletimespos == SAMPLERATEBUFFERSIZE) sampletimespos = 0;
      samplerate = (long long)((long double)SAMPLERATEBUFFERSIZE / (long double)(micros - firstmicros) * 1000000);
      if (micros - lastprintmicros > SAMPLERATEREFRESHWAIT) {
        printf(ESCAPE_CLEARLINE"samplerate: %lld", samplerate);
        lastprintmicros = micros;
      }
    }
    
    if (adc_blocked != adc_was_blocking) {
      adc_was_blocking = adc_blocked;
      if (adc_blocked) {
        fprintf(stderr, "ADC is the slow part\n");
      } else {
        fprintf(stderr, "ADC isn't the slow part\n");
      }
    }

    continue;

badval:
    fprintf(stderr, "invalid data!\n");
  }
  
  if (replaypath) {
    fprintf(stderr, "\nClosing replay file ...\n");
    fclose(replayf);
  }
  
  fprintf(stderr, "\nStopping unix domain server ...\n");
  uds_stop_server(udsserver);
  unlink(outputsocketpath);
  
  return 42;
}
