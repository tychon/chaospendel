
#ifndef _COMMON_C
#define _COMMON_C

#define _POSIX_SOURCE // for 'strtok_r' in stdio

#include <stdio.h>
#include <string.h>

#include "common.h"

// local
int argcmpTestOptions(const char *arg, char *options) {
  char *token = "x", *saveptr = "y";
  while ( (token = strtok_r(options, "|", &saveptr)) ) {
    options = NULL;
    if (strcmp(token, arg) == 0) return 1;
  }
  return 0;
}

// declared in header
int argcmpass(char *options, const int argc, char *argv[], int *argindex, char **dest) {
  char *opts = assert_malloc(strlen(options));
  strcpy(opts, options);
  if (argcmpTestOptions(argv[*argindex], opts)) {
    if (*argindex >= argc-1) {
      fprintf(stderr, "Invalid argument, expected value after option \"%s\"\n", options);
      exit(1);
    }
    (*argindex) ++;
    *dest = argv[*argindex];
    
    return 1;
  }
  else
    return 0;
}

#endif // _COMMON_C

