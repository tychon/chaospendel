
#define _ISOC99_SOURCE // for 'isblank' in ctype
#define _POSIX_C_SOURCE 200809L // for 'getline' in stdio

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "common.h"
#include "projectreader.h"

#define BUFFERSIZE 1024
#define ISVALID(c) (isalnum(c) || c == '.')

#define DEBUG

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
  for (*endptr = line; *endptr < line+linelength; (*endptr) ++) {
    char c = **endptr;
    
    if (isblank(c)) {
      // skip
    } else if (ISVALID(c)) {
      // character in token found
      buffer[bpos++] = c;
      if (bpos == bufferlength-1) return -2;
    } else if (c == delimiter) {
      // end of token reached
      buffer[bpos] = '\0'; // write terminating nullbyte
      (*endptr) ++; // set endptr to first char after end of token
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
  
  char *line
     , *endptr
     , keybuffer[BUFFERSIZE]
     , valbuffer[BUFFERSIZE];
  int linelength, keylength, vallength;
  size_t dummy = sizeof(line); // required for getline
  line = NULL;
  int linenum = 0;
  while ( (linelength = getline(&line, &dummy, f)) >= 0) {
    linenum ++;
    if ( (linelength = findContent(line, linelength)) < 0) {
      continue;
    }
    
    // read key
    keylength = readToken(line, linelength, &endptr, keybuffer, BUFFERSIZE, '=');
    if (keylength == -1) {
      fprintf(stderr, "error: no '=' found in line %d\n", linenum);
      exit(1);
    }
    if (keylength == -2) {
      fprintf(stderr, "error: buffer overflow in line %d\n", linenum);
      exit(1);
    }
    #ifdef DEBUG
      printf("key   (length %d): %s\n", keylength, keybuffer);
    #endif
    
    // read value
    vallength = readToken(endptr, line+linelength-endptr, &endptr, valbuffer, BUFFERSIZE, '\n');
    if (vallength == -1) {
      fprintf(stderr, "error: no newline found in line %d\n", linenum);
      exit(1);
    }
    if (vallength == -2) {
      fprintf(stderr, "error: buffer overflow in line %d\n", linenum);
      exit(1);
    }
    #ifdef DEBUG
      printf("value (length %d): %s\n", vallength, valbuffer);
    #endif
    
    //TODO
    #define CMPREAD(str, to) (strcmp(str, keybuffer) == 0) { \
          to = strtod(valbuffer, &endptr); \
          if (endptr == valbuffer) { \
            fprintf(stderr, "error: Invalid value in line %d\n", linenum); \
            exit(1);                 \
          } \
        }
    if CMPREAD("l1a", dest->l1a)
    else if CMPREAD("l1b", dest->l1b)
    else if CMPREAD("l1", dest->l1)
    else if CMPREAD("l2a", dest->l2a)
    else if CMPREAD("l2b", dest->l2b)
    else if CMPREAD("l1m", dest->l1m)
    else if CMPREAD("l2m", dest->l2m)
    #undef CMPREAD
    
    free(line);
    line = NULL;
  }
  if ( ! feof(f) ) {
    perror("reading file");
    exit(1);
  }
  
  return dest;
}

