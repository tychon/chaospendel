
#define _BSD_SOURCE
#include <stdlib.h>
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
  
  // 11111100 00000000
  buffer[0] = (data >> 10       ) | 0x40;
  // 00000011 11110000
  buffer[1] = (data >>  4 & 0x3f) | 0x80;
  // 00000000 00001111
  buffer[2] = (data       & 0xf ) | 0x80;
  
  return 3;
}

// Reads 3 bytes of data
uint16_t parse2Bytes(char *buffer) {
  uint16_t result = 0;
  result  = (buffer[0] & 0x3f) << 10;
  result |= (buffer[1] & 0x3f) <<  4;
  result |= (buffer[2] &  0xf);
  return be16toh(result);
}

// Writes 11 bytes of data
int format8Bytes(char *buffer, uint64_t data) {
  data = htobe64(data);
  
  // 11111100 00000000 00000000 00000000 00000000 00000000 00000000 00000000
  buffer[0]  =  data >> (7*8+2)         | 0x40;
  // 00000011 11110000 00000000 00000000 00000000 00000000 00000000 00000000
  buffer[1]  = (data >> (6*8+4) & 0x3f) | 0x80;
  // 00000000 00001111 11000000 00000000 00000000 00000000 00000000 00000000
  buffer[2]  = (data >> (5*8+6) & 0x3f) | 0x80;
  // 00000000 00000000 00111111 00000000 00000000 00000000 00000000 00000000
  buffer[3]  = (data >> (5*8  ) & 0x3f) | 0x80;
  // 00000000 00000000 00000000 11111100 00000000 00000000 00000000 00000000
  buffer[4]  = (data >> (4*8+2) & 0x3f) | 0x80;
  // 00000000 00000000 00000000 00000011 11110000 00000000 00000000 00000000
  buffer[5]  = (data >> (3*8+4) & 0x3f) | 0x80;
  // 00000000 00000000 00000000 00000000 00001111 11000000 00000000 00000000
  buffer[6]  = (data >> (2*8+6) & 0x3f) | 0x80;
  // 00000000 00000000 00000000 00000000 00000000 00111111 00000000 00000000
  buffer[7]  = (data >> (2*8  ) & 0x3f) | 0x80;
  // 00000000 00000000 00000000 00000000 00000000 00000000 11111100 00000000
  buffer[8]  = (data >> (1*8+2) & 0x3f) | 0x80;
  // 00000000 00000000 00000000 00000000 00000000 00000000 00000011 11110000
  buffer[9]  = (data >>      4  & 0x3f) | 0x80;
  // 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00001111
  buffer[10] = (data            &  0xf) | 0x80;
  
  return 11;
}

// Reads 11 bytes of data
uint64_t parse8Bytes(char *buffer) {
  uint64_t result = 0;
  for (int i = 0; i < 10; i ++)
    result |= ((uint64_t)buffer[i] & 0x3f) << (6*(9-i)+4);
  result |= ((uint64_t)buffer[10] & 0xf);
  return be64toh(result);
}


/////////////////////
// declared in header

// I'm not so convinced, that this is efficient
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
                        , uint16_t *values
                        , int nvalues) {
  int bpos = 0;
  
  if (timestamp) {
    // write 11 bytes
    if (bufferlength < 11) return -1;
    format8Bytes(buffer, timestamp);
    bpos += 11;
  }
  
  if (bufferlength-bpos < nvalues*3 || (!timestamp && !nvalues)) return -1;
  for (int i = 0; i < nvalues; i++) {
    bpos += format2Bytes(buffer+bpos, values[i]);
  }
  
  buffer[0] &= 0x3f; // meta data of first byte
  buffer[bpos-1] |= 0xc0; // meta data of last byte
  
  return bpos;
}

/**
 * @returns The length of the valid data packet.
 */
int parseHalfbyte2Packet(char *buffer
                       , int bufferlength
                       , struct halfbyte2 *result
                       , int timestamp
                       , int nvalues
                       , char **startptr
                       , char **endptr) {
  // find start of dataset
  /*
  for (*startptr = buffer; *startptr < buffer+bufferlength; *startptr ++)
    if ( ! (**startptr & 0xc0))
      break;
  if (*startptr >= buffer+bufferlength) return -1;
  */
  *startptr = NULL;
  for (int i = 0; i < bufferlength; i++) {
    if ( ! (buffer[i] & 0xc0)) {
      *startptr = buffer+i;
      break;
    }
  }
  if (! startptr) return -1;
  
  int bpos = *startptr - buffer;
  
  // parse timestamp (maybe)
  if (timestamp) {
    if (bufferlength-bpos < 11) return -2;
    // TODO validate meta data
    result->timestamp = parse8Bytes(buffer+bpos);
    bpos += 11;
  } else result->timestamp = 0;
  
  if (bufferlength-bpos < nvalues*3) return -3;
  for (int i = 0; i < nvalues; i++) {
    // TODO validate metadata
    result->values[i] = parse2Bytes(buffer+bpos);
    bpos += 3;
  }
  
  *endptr = buffer+bpos;
  return bpos - (*startptr - buffer);
}






