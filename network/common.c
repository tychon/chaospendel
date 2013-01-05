
#ifndef _COMMON_C
#define _COMMON_C

#define _POSIX_SOURCE // for 'strtok_r' in stdio

#include <stdlib.h>
#include <limits.h> // for LONG_MIN und LONG_MAX
#include <stdio.h>
#include <string.h>
#include <sys/time.h>

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

/////////////////////
// declared in header

long long getMicroseconds() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (long long)tv.tv_sec * 1000000 + tv.tv_usec;
}


int argcmpass(char *options, const int argc, char *argv[], int *argindex, char **dest) {
  char *opts = assert_malloc(strlen(options));
  strcpy(opts, options);
  if (argcmpTestOptions(argv[*argindex], opts)) {
    if (*argindex >= argc-1) {
      fprintf(stderr, "error: Invalid argument, expected value after option \"%s\"\n", options);
      exit(1);
    }
    (*argindex) ++;
    *dest = argv[*argindex];
    
    return 1;
  }
  else
    return 0;
}
int argcmpassint(char *options, const int argc, char *argv[], int *argindex, int *dest) {
  char *opt = argv[*argindex];
  char *opts = assert_malloc(strlen(options));
  strcpy(opts, options);
  if (argcmpTestOptions(opt, opts)) {
    if (*argindex >= argc-1) {
      fprintf(stderr, "error: Invalid argument, expected value after option \"%s\"\n", options);
      exit(1);
    }
    
    (*argindex) ++;
    
    char *endptr, *value = argv[*argindex];
    long parsed = strtol(value, &endptr, 0);
    if (parsed < INT_MIN) {
      fprintf(stderr, "error: Value of option \"%s\" is too small.\n", opt);
      exit(1);
    } else if (parsed == LONG_MAX) {
      fprintf(stderr, "error: Value of option \"%s\" is too big.\n", opt);
      exit(1);
    } else if (value == endptr) {
      fprintf(stderr, "error: Invalid number in option \"%s\".\n", opt);
      exit(1);
    } else
      *dest = (int)parsed;
    
    return 1;
  }
  else
    return 0;
}

#endif // _COMMON_C

