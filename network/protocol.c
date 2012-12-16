
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
int format2Bytes(unsigned char *buffer, uint16_t data) {
  data = htobe16(data);
  
  // 11111100 00000000
  buffer[0] = (data >> 10       ) | 0x40;
  // 00000011 11110000
  buffer[1] = (data >>  4 & 0x3f) | 0x80;
  // 00000000 00001111
  buffer[2] = (data       & 0xf ) | 0x80;
  
  return 3;
}
// Looks for 3 bytes of data
int validate2Bytes(unsigned char *buffer) {
  if (! ((buffer[0] & 0xc0) == 0x40  ||  (buffer[0] & 0xc0) ==  0x0)) return 0;
  if (! ((buffer[1] & 0xc0) == 0x80)) return 1;
  if (! ((buffer[2] & 0xc0) == 0x80  ||  (buffer[2] & 0xc0) == 0xc0)) return 2;
  return -1;
}
// Reads 3 bytes of data
uint16_t parse2Bytes(unsigned char *buffer) {
  uint16_t result = 0;
  result  = (buffer[0] & 0x3f) << 10;
  result |= (buffer[1] & 0x3f) <<  4;
  result |= (buffer[2] &  0xf);
  return be16toh(result);
}

// Writes 11 bytes of data
int format8Bytes(unsigned char *buffer, uint64_t data) {
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
// Looks for 11 bytes of data
int validate8Bytes(unsigned char *buffer) {
  // first byte
  if (! ((buffer[0] & 0xc0) == 0x40  ||  (buffer[0] & 0xc0) ==  0x0)) return 0;
  // 9 middle bytes
  for (int i = 1; i < 10; i++)
    if (! (buffer[i] & 0xc0) == 0x80) return i;
  // last byte
  if (! ((buffer[10] & 0xc0) == 0x80  ||  (buffer[10] & 0xc0) == 0xc0)) return 10;
  // everything is ok
  return -1;
}
// Reads 11 bytes of data
uint64_t parse8Bytes(unsigned char *buffer) {
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

int formatHalfbyte2Packet(unsigned char *buffer
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
 * @returns The length >= 0 of the valid data packet
 *          (*endptr points to first byte after dataset)
 *  or:
 *   -1 if there was no beginning of a dataset found
 *   -2 if the buffer is too small to contain a timestamp
 *      (only with the timestamp option)
 *   -3 if the timestamp contains invalid bits
 *      (*endptr points to the first invalid byte found)
 *   -4 if the buffer is too small to contain all the values
 *   -5 if there was invalid data in the dataset
 *      (*endptr points to the first invalid byte found)
 *   -6 if the last byte of the dataset is invalid
 *      (*endptr points to the first byte after the invalid byte)
 */
int parseHalfbyte2Packet(unsigned char *buffer
                       , int bufferlength
                       , struct halfbyte2 *result
                       , int timestamp
                       , int nvalues
                       , unsigned char **startptr
                       , unsigned char **endptr) {
  // find start of dataset
  *startptr = NULL;
  for (int i = 0; i < bufferlength; i++) {
    if ( ! (buffer[i] & 0xc0)) {
      *startptr = buffer+i;
      break;
    }
  }
  if (! startptr) return -1;
  
  int bpos = *startptr - buffer; // is on first valid bit
  int retv; // return value of validator functions
  
  // parse timestamp (maybe)
  if (timestamp) {
    if (bufferlength-bpos < 11) return -2;
    if ( (retv = validate8Bytes(buffer)) >= 0) {
      *endptr = buffer+bpos+retv;
      return -3;
    }
    result->timestamp = parse8Bytes(buffer+bpos);
    bpos += 11;
  } else result->timestamp = 0;
  
  // parse uint16_ts
  if (bufferlength-bpos < nvalues*3) return -4;
  for (int i = 0; i < nvalues; i++) {
    if ( (retv = validate2Bytes(buffer+bpos)) >= 0) {
      *endptr = buffer+bpos+retv;
      return -5;
    }
    result->values[i] = parse2Bytes(buffer+bpos);
    bpos += 3;
  }
  
  *endptr = buffer+bpos;
  
  // validate last byte
  if ((buffer[bpos-1] & 0xc0) != 0xc0) {
    return -6;
  }
  
  return *endptr - *startptr;
}

