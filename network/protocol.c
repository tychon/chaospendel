
#define _BSD_SOURCE
#include <stdlib.h>
#include <sys/timeb.h>
#include <time.h>
#include <string.h>
#include <endian.h>

#include "protocol.h"

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

int format2bytePacket(unsigned char *buffer
                    , int bufferlength
                    , uint64_t timestamp
                    , uint16_t *values
                    , int nvalues) {
  int bpos = 0;
  
  if (timestamp) {
    // write 8 byte long timestamp
    if (bufferlength < 8) return -1;
    uint64_t be = htobe64(timestamp);
    memcpy(buffer, &be, 8);
    bpos += 8;
  }
  
  if (   bufferlength-bpos < nvalues*2
      || (!timestamp && !nvalues))
    return -1;
  
  uint16_t be;
  for (int i = 0; i < nvalues; i++) {
    be = htobe16(values[i]);
    memcpy(buffer+bpos, &be, 2);
    bpos += 2;
  }
  
  return bpos;
}

/**
 * @returns the number of uint16_ts read by this function (>= 0) or an error code (< 0):
 *   -1: buffer too short for timestamp
 *   -2: buffer too short for values
 */
int parse2bytePacket(unsigned char *buffer
                   , int bufferlength
                   , struct packet2byte *result
                   , int timestamp  /// boolean value indcating 8 byte long timestamp
                   , int nvalues    /// number of 2 byte long values in buffer
                     ) {
  int bpos = 0;
  if (timestamp) {
    // parse timestamp
    if (bufferlength < 8) return -1;
    memcpy(&(result->timestamp), buffer, 8);
    result->timestamp = be64toh(result->timestamp);
    bpos += 8;
  } else result->timestamp = 0;
  
  // parse uint16_ts
  if (bufferlength-bpos < nvalues*2) return -2;
  int i;
  for (i = 0; i < nvalues; i++) {
    memcpy(result->values + i, buffer+bpos, 2);
    result->values[i] = be16toh(result->values[i]);
    bpos += 2;
  }
  
  return i;
}

