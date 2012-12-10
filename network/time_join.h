
#ifndef _TIME_JOIN_H
#define _TIME_JOIN_H

struct standardline {
  long long timestamp;
  char modifier;
  int *values;
};
typedef struct standardline standardline;

int parseStandardLine(standardline *result, char **startptr, char **endptr, char *buffer, int bufferlength, int nvalues);

#endif // _TIME_JOIN_H

