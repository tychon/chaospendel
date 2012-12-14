
#define _BSD_SOURCE
#include <stdlib.h>
#include <stdint.h>
#include <sys/timeb.h>
#include <time.h>
#include <string.h>
#include <endian.h>

#include "protocol.h"

////////
// local

// Writes 3 bytes of data
int format2Bytes(char *buffer, uint16_t data) {
  data = htobe16(data);
  
  // get these: 1111110000000000 and put 10 in front of them
  buffer[0] = (data           >> 10) | 0x2;
  // get these: 0000001111110000 and put 01 in front of them
  buffer[1] = ((data & 0x3f0) >> 4 ) | 0x1;
  // get these: 0000000000001111 and put 01 in front of them
  buffer[2] = ( data & 0xf         ) | 0x1;
  
  return 3;
}

// Writes 11 Bytes of data
int format8Bytes(char *buffer, uint64_t data) {
  data = htobe64(data);
  
  // get these: 
  // 1111110000000000000000000000000000000000000000000000000000000000
  buffer[0] = (data >> 82) | 0x2;
  // 0000001111110000000000000000000000000000000000000000000000000000
  buffer[10] = (data & 0xf) | 0x1;
  
  return 11;
}


/////////////////////
// declared in header

long long getUnixMillis() {
  static struct timeb tmb;
  ftime(&tmb);
  static struct tm timetm;
  timetm = *localtime(& tmb.time);
  
  return                              (long long)tmb.millitm
        +                        1000*(long long)timetm.tm_sec
        +                     60*1000*(long long)timetm.tm_min
        +                  60*60*1000*(long long)timetm.tm_hour
        +               24*60*60*1000*(long long)timetm.tm_yday
        +(long long)365*24*60*60*1000*(long long)timetm.tm_year;
}
                  
int formatHalfbyte2Packet(char *buffer
                        , int bufferlength
                        , long long timestamp
                        , short *values
                        , int nvalues) {
  //int bpos = 0;
  if (timestamp) {
    //TODO
  }
  return 0;
}

