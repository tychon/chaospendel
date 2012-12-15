
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory_wrappers.h"
#include "uds_client.h"
#include "protocol.h"

#define CMP(str, index) (strcmp(str, argv[index]) == 0)

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
    if (CMP("--format", i) || CMP("-f", i)) {
      i ++;
      if (CMP("bin", i) || CMP("binary", i)) format = FORMATBIN;
      else if (CMP("dec", i) || CMP("decimal", i)) format = FORMATDEC;
      else if (CMP("hex", i) || CMP("hexadecimal", i)) format = FORMATHEX;
      else if (CMP("asc", i) || CMP("ascii", i)) format = FORMATASC;
      else if (CMP("half2", i)) format = FORMATHALFBYTE2;
      else if (CMP("half4", i)) format = FORMATHALFBYTE4;
      else if (CMP("half8", i)) format = FORMATHALFBYTE8;
    }
    else if (CMP("--timestamped", i) || CMP("-t", i)) {
      if (format >= FORMATHALFBYTE2 && format <=  FORMATHALFBYTE8) timestamped = 1;
      else {
        fprintf(stderr, "The timestamp option only goes with the halfX format.\n");
        exit(1);
      }
    }
    else if (CMP("--nvalues", i) || CMP("-v", i)) {
      i ++;
      nvalues = atoi(argv[i]);
    }
    else if (! sockpath) sockpath = argv[i];
    else fprintf(stderr, "Argument ignored: %s\n", argv[i]);
  }
  if (! sockpath) {
    fprintf(stderr, "usage: %s [-c | -b] <pathname>", argv[0]);
    exit(1);
  }
  
  fprintf(stderr, "opening socket on \"%s\"\n", sockpath);
  udsclientsocket *udscs = uds_create_client(sockpath);
  
  if (format <= FORMATASC) {
    char c;
    for (;;) {
      if (uds_read(udscs, &c, 1) == 0) {
        printf("end of data\n");
        break;
      }
      switch (format) {
        case FORMATBIN: {
          if (PRINTABLE(c)) printf(BYTETOBINARYPATTERN"\t%c\n", BYTETOBINARY(c), c);
          else printf(BYTETOBINARYPATTERN"\t%02x\n", BYTETOBINARY(c), (unsigned char)c);
          break;
        }
        case FORMATDEC: printf("%d ", (int)c); break;
        case FORMATHEX: printf("%02x ", (int)c); break;
        case FORMATASC: printf("%c", c); break;
      }
      fflush(stdout);
    }
  } else {
    // initialize struct for parsing
    void *parsed = NULL;
    switch (format) {
      case FORMATHALFBYTE2: {
        parsed = assert_malloc(sizeof(struct halfbyte2));
        ((struct halfbyte2*)parsed)->values = assert_malloc(sizeof(uint16_t)*10);
      } break;
      // TODO other halfbyte formats
    }
    
    // start reading
    unsigned char buffer[1024], *startptr, *endptr;
    int retv = 0, retv2;
    for (;;) {
      if ((retv = uds_read(udscs, buffer, 1024)) == 0) {
        printf("end of data\n");
        break;
      }
      switch (format) {
        case FORMATHALFBYTE2: {
          if ( (retv2 = parseHalfbyte2Packet(buffer, retv
                                 , (struct halfbyte2*)parsed, timestamped, nvalues
                                 , &startptr, &endptr                )) < 0) {
            fprintf(stderr, "parsing failed with (%d), buffer: ", retv2);
            for (int i = 0; i < retv; i++) fprintf(stderr, " %02x", buffer[i]);
            fprintf(stderr, "\n");
          }
          printf("[%lld]", ((struct halfbyte2*)parsed)->timestamp);
          for (int i = 0; i < nvalues; i++) 
            printf(" %d", ((struct halfbyte2*)parsed)->values[i]);
          printf("\n");
        } break;
        // TODO other halfbyte formats
      }
    }
  }
  
  fflush(stdout);
  uds_close_client(udscs);
}

