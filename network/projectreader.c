
#define _ISOC99_SOURCE // for 'isblank' in ctype
#define _POSIX_C_SOURCE 200809L // for 'getline' in stdio

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "projectreader.h"

#define BUFFERSIZE 1024
#define ISVALID(c) isalnum(c)

/**
 * The length of the line without comments or -1 if not valid characters
 * were found before a '#' character.
 */
int findContent(char *line, int linelength) {
  int contfound = 0;
  for (int i = 0; i < linelength; i++) {
    if (ISVALID(line[i])) contfound = 1;
    else if (line[i] == '#') {
      if (contfound) return i;
      else return -1;
    }
  }
  
  if (contfound) return linelength;
  else return -1;
}

/**
 * @returns length of valid string in 'buffer' or errorcode:
 * -1: end of line reached and no '=' was found
 * -2: output buffer too small
 */
int readToken(char *line, int linelength, char **endptr
             , char *buffer, int bufferlength
             , char delimiter) {
  int bpos = 0;
  for (*endptr = line; *endptr < line+linelength; *endptr ++) {
    char c = **endptr;
    
    if (isblank(c)) {
      // skip
    } else if (isalnum(c)) {
      // character in token found
      buffer[bpos++] = c;
      if (bpos == bufferlength-1) return -2;
    } else if (c == delimiter) {
      // end of token reached
      buffer[bpos] = '\0'; // write terminating nullbyte
      *endptr ++; // set endptr to first char after end of token
      return bpos;
    }
  }
  
  return -1; // no terminating delimiter found
}

projectdata *readPendulumData(projectdata *dest, char *filepath) {
  FILE *f = fopen(filepath, "r");
  if ( ! f) {
    perror("opening file");
    exit(1);
  }
  
  char **line
     , **endptr
     , buffer[BUFFERSIZE];
  int linelength, keylength;
  *line = NULL;
  while ( (linelength = getline(line, NULL, f)) >= 0) {
    if ( (linelength = findContent(*line, linelength)) < 0)
      continue;
    
    // read key
    keylength = readToken(*line, linelength, endptr, buffer, BUFFERSIZE, '=');
    printf("key: %.*s\n", keylength, buffer);
    //TODO
    
    free(line);
    *line = NULL;
  }
  
  return dest;
}

