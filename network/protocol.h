
#ifndef _PROTOCOL_H
#define _PROTOCOL_H

#include "uds_server.h"

#define BINFORMAT_TIMESTAMP 1
#define BINFORMAT_BINARY    2
#define BINFORMAT_ASCII     4
#define BINFORMAT_TYPE_SHORT   8
#define BINFORMAT_TYPE_INT    16
#define BINFORMAT_TYPE_DOUBLE 32

#define ASCFORMAT_TIMESTAMP 'T'
#define ASCFORMAT_BINARY    'B'
#define ASCFORMAT_ASCII     'A'
#define ASCFORMAT_TYPE_SHORT  'S'
#define ASCFORMAT_TYPE_INT    'I'
#define ASCFORMAT_TYPE_DOUBLE 'D'

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

int sendHalfbyte2Packet(udsserversocket*, long long timestamp, short *values, int nvalues);

int parseHalfbyte2Packet(struct halfbyte2 *result, char **startptr, char **endptr, char *buffer, int bufferlength, int nvalues);

#endif // _PROTOCOL_H

