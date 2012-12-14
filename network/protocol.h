
#ifndef _PROTOCOL_H
#define _PROTOCOL_H

#include <stdint.h>
#include "uds_server.h"

struct halfbyte2 {
  long long timestamp;
  short values;
};
struct halfbyte4 {
  long long timestamp;
  int *values;
};
struct halfbyte8 {
  long long timestamp;
  long long *values;
};

long long getUnixMillis();

int formatHalfbyte2Packet(char *buffer, int bufferlength
                        , long long timestamp
                        , uint16_t *values, int nvalues);

int parseHalfbyte2Packet(struct halfbyte2 *result, char **startptr, char **endptr, char *buffer, int bufferlength, int nvalues);

#endif // _PROTOCOL_H

