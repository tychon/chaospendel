
#ifndef _COMMON_H
#define _COMMON_H

#include "memory_wrappers.h"

#define GLOBALSEQPACKETSIZE 1024

#define ESCAPE_CLEARLINE "\x1B[80D\x1B[K"

// some timing functions
long long getMicroseconds();

// some functions for parsing options
#define ARGCMP(str, index) (strcmp(str, argv[index]) == 0)

int argcmpass(char *options
            , const int argc, char *argv[], int *argindex
            , char **dest);
int argcmpassint(char *options
               , const int argc, char *argv[], int *argindex
               , int *dest);

#endif // _COMMON_H

