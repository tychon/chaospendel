
#ifndef _PROTOCOL_H
#define _PROTOCOL_H

#include <stdint.h>
#include "uds_server.h"

struct packet2byte {
  long long timestamp;
  uint16_t *values;
};
struct packet4byte {
  long long timestamp;
  uint32_t *values;
};
struct packet8byte {
  long long timestamp;
  uint64_t *values;
};

long long getUnixMillis();

int format2bytePacket(unsigned char *buffer, int bufferlength
                    , uint64_t timestamp
                    , uint16_t *values, int nvalues);
int parse2bytePacket(unsigned char *buffer, int bufferlength
                   , struct packet2byte *result, int timestamp, int nvalues);

#endif // _PROTOCOL_H

