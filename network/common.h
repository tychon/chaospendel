
#ifndef _COMMON_H
#define _COMMON_H

#include "memory_wrappers.h"

#define GLOBALSEQPACKETSIZE 1024

#define ARGCMP(str, index) (strcmp(str, argv[index]) == 0)

int argcmpass(char *options
            , const int argc, char *argv[], int *argindex
            , char **dest);
int argcmpassint(char *options
               , const int argc, char *argv[], int *argindex
               , long *dest);

#endif // _COMMON_H

