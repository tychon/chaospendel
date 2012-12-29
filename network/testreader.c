
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "memory_wrappers.h"
#include "uds_client.h"
#include "protocol.h"

#define FORMATHEX 1
#define FORMATDEC 2
#define FORMATBIN 3
#define FORMATASC 4
#define FORMATHALFBYTE2 5
#define FORMATHALFBYTE4 6
#define FORMATHALFBYTE8 7

#define BYTETOBINARYPATTERN "%d%d%d%d%d%d%d%d"
#define BYTETOBINARY(byte)  \
  (byte & 0x80 ? 1 : 0), \
  (byte & 0x40 ? 1 : 0), \
  (byte & 0x20 ? 1 : 0), \
  (byte & 0x10 ? 1 : 0), \
  (byte & 0x08 ? 1 : 0), \
  (byte & 0x04 ? 1 : 0), \
  (byte & 0x02 ? 1 : 0), \
  (byte & 0x01 ? 1 : 0) 

#define PRINTABLE(ch) (ch >= 32 && ch <= 126)

int main(int argc, char *argv[]) {
  char *sockpath = NULL;
  int format = FORMATHEX;
  int timestamped = 0;
  int nvalues = -1;
  for (int i = 1; i < argc; i++) {
    if (ARGCMP("--format", i) || ARGCMP("-f", i)) {
      i ++;
      if (ARGCMP("bin", i) || ARGCMP("binary", i)) format = FORMATBIN;
      else if (ARGCMP("dec", i) || ARGCMP("decimal", i)) format = FORMATDEC;
      else if (ARGCMP("hex", i) || ARGCMP("hexadecimal", i)) format = FORMATHEX;
      else if (ARGCMP("asc", i) || ARGCMP("ascii", i)) format = FORMATASC;
      else if (ARGCMP("half2", i)) format = FORMATHALFBYTE2;
      else if (ARGCMP("half4", i)) format = FORMATHALFBYTE4;
      else if (ARGCMP("half8", i)) format = FORMATHALFBYTE8;
    }
    else if (ARGCMP("--timestamped", i) || ARGCMP("-t", i)) {
      if (format >= FORMATHALFBYTE2 && format <=  FORMATHALFBYTE8) timestamped = 1;
      else {
        fprintf(stderr, "The timestamp option only goes with the halfX format.\n");
        exit(1);
      }
    }
    else if (argcmpassint("--nvalues|-v", argc, argv, &i, &nvalues)) ;
    else if (argcmpass("--inputsocket|-i", argc, argv, &i, &sockpath)) ;
    else fprintf(stderr, "warning: Unknown argument ignored: \"%s\"\n", argv[i]);
  }
  if (! sockpath) {
    fprintf(stderr, "usage: %s [--format|-f bin|binary|dec|decimal|hex|hexadecimal|asc|ascii|half2|half4|half8] [--timestampted|-t] [--nvalues|-v INT] --inputsocket|-i SOCKETPATH\n", argv[0]);
    exit(1);
  }
  
  fprintf(stderr, "opening socket on \"%s\"\n", sockpath);
  udsclientsocket *udscs = uds_create_client(sockpath);
  
  int retv;
  unsigned char buffer[1024];
  unsigned char c;
  
  if (format <= FORMATASC) {
    for (;;) {
      if ( (retv = uds_read(udscs, buffer, 1024)) == 0) {
        printf("end of data\n");
        break;
      }
      for (int i = 0; i < retv; i++) {
        c = buffer[i];
        switch (format) {
          case FORMATBIN: {
            if (PRINTABLE(c)) printf(BYTETOBINARYPATTERN"\t%c\n", BYTETOBINARY(c), c);
            else printf(BYTETOBINARYPATTERN"\t%02x\n", BYTETOBINARY(c), c);
            break;
          }
          case FORMATDEC: printf("%d ", c); break;
          case FORMATHEX: printf("%02x ", c); break;
          case FORMATASC: printf("%c", c); break;
        }
      }
      printf("\n");
      fflush(stdout);
    }
  } else {
    // initialize struct for parsing
    void *parsed = NULL;
    switch (format) {
      case FORMATHALFBYTE2: {
        parsed = allocate2bytePacket(nvalues);
      } break;
      // TODO other halfbyte formats
    }
    
    // start reading
    int retv2;
    for (;;) {
      if ((retv = uds_read(udscs, buffer, 1024)) == 0) {
        printf("end of data\n");
        break;
      }
      switch (format) {
        case FORMATHALFBYTE2: {
          if ( (retv2 = parse2bytePacket(buffer, retv
                                 , (struct packet2byte*)parsed, timestamped
                                 , nvalues)) < 0) {
            fprintf(stderr, "parsing failed with code (%d), buffer: ", retv2);
            for (int i = 0; i < retv; i++) fprintf(stderr, " %02x", buffer[i]);
            fprintf(stderr, "\n");
          } else {
            printf("[%lld]", ((struct packet2byte*)parsed)->timestamp);
            for (int i = 0; i < nvalues; i++) 
              printf(" %d", ((struct packet2byte*)parsed)->values[i]);
            printf("\n");
          }
        } break;
        // TODO other halfbyte formats
      }
    }
  }
  
  fflush(stdout);
  uds_close_client(udscs);
}

