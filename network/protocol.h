
#ifndef _PROTOCOL_H
#define _PROTOCOL_H

#include <stdint.h>
#include "uds_server.h"

struct halfbyte2 {
  long long timestamp;
  uint16_t *values;
};
struct halfbyte4 {
  long long timestamp;
  uint32_t *values;
};
struct halfbyte8 {
  long long timestamp;
  uint64_t *values;
};

long long getUnixMillis();

int formatHalfbyte2Packet(char *buffer, int bufferlength
                        , long long timestamp
                        , uint16_t *values, int nvalues);
int parseHalfbyte2Packet(char *buffer, int bufferlength
                       , struct halfbyte2 *result, int timestamp, int nvalues
                       , char **startptr, char **endptr);

#endif // _PROTOCOL_H

