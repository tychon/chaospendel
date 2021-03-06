
#define _BSD_SOURCE
#include <stdlib.h>
#include <sys/timeb.h>
#include <time.h>
#include <string.h>
#include <endian.h>

#include "common.h"
#include "protocol.h"

struct packet2byte *allocate2bytePacket(int valnum) {
  struct packet2byte *p2b = assert_malloc(sizeof(struct packet2byte));
  p2b->values = assert_malloc(sizeof(uint16_t) * valnum);
  return p2b;
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

struct packet4byte *allocate4bytePacket(int valnum) {
  struct packet4byte *p4b = assert_malloc(sizeof(struct packet4byte));
  p4b->values = assert_malloc(sizeof(uint32_t) * valnum);
  return p4b;
}

int format4bytePacket(unsigned char *buffer
                    , int bufferlength
                    , uint64_t timestamp
                    , uint32_t *values
                    , int nvalues) {
  int bpos = 0;
  
  if (timestamp) {
    // write 8 byte long timestamp
    if (bufferlength < 8) return -1;
    uint64_t be = htobe64(timestamp);
    memcpy(buffer, &be, 8);
    bpos += 8;
  }
  
  if (   bufferlength-bpos < nvalues*4
      || (!timestamp && !nvalues))
    return -1;
  
  uint32_t be;
  for (int i = 0; i < nvalues; i++) {
    be = htobe32(values[i]);
    memcpy(buffer+bpos, &be, 4);
    bpos += 4;
  }
  
  return bpos;
}

/**
 * @returns the number of uint32_ts read by this function (>= 0) or an error code (< 0):
 *   -1: buffer too short for timestamp
 *   -2: buffer too short for values
 */
int parse4bytePacket(unsigned char *buffer
                   , int bufferlength
                   , struct packet4byte *result
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
  
  // parse uint32_ts
  if (bufferlength-bpos < nvalues*4) return -2;
  int i;
  for (i = 0; i < nvalues; i++) {
    memcpy(result->values + i, buffer+bpos, 4);
    result->values[i] = be32toh(result->values[i]);
    bpos += 4;
  }
  
  return i;
}

struct packet8byte *allocate8bytePacket(int valnum) {
  struct packet8byte *p8b = assert_malloc(sizeof(struct packet8byte));
  p8b->values = assert_malloc(sizeof(uint64_t) * valnum);
  return p8b;
}

int format8bytePacket(unsigned char *buffer
                    , int bufferlength
                    , uint64_t timestamp
                    , uint64_t *values
                    , int nvalues) {
  int bpos = 0;
  
  if (timestamp) {
    // write 8 byte long timestamp
    if (bufferlength < 8) return -1;
    uint64_t be = htobe64(timestamp);
    memcpy(buffer, &be, 8);
    bpos += 8;
  }
  
  if (   bufferlength-bpos < nvalues*8
      || (!timestamp && !nvalues))
    return -1;
  
  uint64_t be;
  for (int i = 0; i < nvalues; i++) {
    be = htobe64(values[i]);
    memcpy(buffer+bpos, &be, 8);
    bpos += 8;
  }
  
  return bpos;
}

/**
 * @returns the number of uint64_ts read by this function (>= 0) or an error code (< 0):
 *   -1: buffer too short for timestamp
 *   -2: buffer too short for values
 */
int parse8bytePacket(unsigned char *buffer
                   , int bufferlength
                   , struct packet8byte *result
                   , int timestamp  /// boolean value indcating 8 byte long timestamp
                   , int nvalues    /// number of 8 byte long values in buffer
                     ) {
  int bpos = 0;
  if (timestamp) {
    // parse timestamp
    if (bufferlength < 8) return -1;
    memcpy(&(result->timestamp), buffer, 8);
    result->timestamp = be64toh(result->timestamp);
    bpos += 8;
  } else result->timestamp = 0;
  
  // parse uint64_ts
  if (bufferlength-bpos < nvalues*8) return -2;
  int i;
  for (i = 0; i < nvalues; i++) {
    memcpy(result->values + i, buffer+bpos, 8);
    result->values[i] = be64toh(result->values[i]);
    bpos += 8;
  }
  
  return i;
}

