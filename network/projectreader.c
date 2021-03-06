
#define _ISOC99_SOURCE // for 'isblank' in ctype
#define _POSIX_C_SOURCE 200809L // for 'getline' in stdio
#define _BSD_SOURCE // for 'M_PI' in math

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

#include "common.h"

#include "projectreader.h"

// buffer length for parsing keys and values
#define BUFFERSIZE 1024

// conditional for valid characters in keys and values
#define ISVALID(c) (isalnum(c) || c == '.' || c == '-' || c == '_')

// You pass these defines to 'readData' as mode argument.
#define PENDULUM_DATA 1
#define CALIBRATION_DATA 2
#define NORMALISATION_DATA 3

// #define DEBUG

////////////////////////////////////////////////////////////////////////////////
// local functions

/**
 * @returns the length of the line without comments or -1 if not valid
 * characters (by ISVALID macro) were found before a '#' character.
 */
int findContent(char *line, int linelength) {
  int contfound = 0;
  for (int i = 0; i < linelength; i++) {
    if (ISVALID(line[i])) contfound = 1;
    else if (line[i] == '#') {
      line[i] = '\n';
      if (contfound) return i+1;
      else return -1;
    }
  }
  
  if (contfound) return linelength;
  else return -1;
}

/**
 * @returns size of valid string in 'buffer' or errorcode:
 *   -1: end of line reached and no '=' was found
 *   -2: output buffer too small
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

/**
 * Parse key-value-pairs in file given by 'filepath' and store them in 'dest'.
 * The 'mode' gives wich pairs are to be found in the file, see the 3 defines
 * PENDULUM_DATA, CALIBRATION_DATA and NORMALISATION_DATA.
 * @returns 'dest'
 */
projectdata *readData(projectdata *dest, const char *filepath, const int mode) {
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
      // line without any valid character for a value
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
    
    // compare keys and fill destination structure
    
    // this define compares a string to the keybuffer, then parses the
    // double in the valuebuffer, if the comparison succeeded and saves the
    // double in 'to'.
    #define CMPREAD(str, to) (strcmp(str, keybuffer) == 0) { \
          to = strtod(valbuffer, &endptr); \
          if (endptr == valbuffer) { \
            fprintf(stderr, "error: Invalid value in line %d\n", linenum); \
            exit(1);                 \
          } \
        }
    #define SCANREAD(format, solindexptr, destvar)           \
        if (sscanf(keybuffer, format, solindexptr) == 1) {      \
          if (*solindexptr < 0 || *solindexptr >= dest->solnum) { \
            fprintf(stderr, "error: solenoid index not in range in line %d\n", linenum); \
            exit(1); \
          }          \
          destvar = strtod(valbuffer, &endptr); \
          if (endptr == valbuffer) {            \
            fprintf(stderr, "error: Invalid value in line %d\n", linenum); \
            exit(1); \
          }          \
        }
    
    if (mode == PENDULUM_DATA) {
      // parse lengths of pendulum in doubles
      if CMPREAD("l1a", dest->l1a)
      else if CMPREAD("l1b", dest->l1b)
      else if CMPREAD("l1", dest->l1)
      else if CMPREAD("l2a", dest->l2a)
      else if CMPREAD("l2b", dest->l2b)
      else if CMPREAD("l1m", dest->l1m)
      else if CMPREAD("l2m", dest->l2m)
      else if CMPREAD("inputrangeminimum", dest->inputrangemin)
      else if CMPREAD("inputrangemaximum", dest->inputrangemax)
      else if CMPREAD("noisefactor", dest->noisefactor)
      else if CMPREAD("integralresetsamples", dest->integralresetsamples)
      else if CMPREAD("integralmaximum", dest->integralmax)
      else if CMPREAD("integralthreshold", dest->integralthreshold)
      else if CMPREAD("markovtracklength", dest->markovtracklength)
      else if CMPREAD("markovvelocityrangenum", dest->markovvelrangenum)
      else if CMPREAD("markovvelocitymax", dest->markovvelmax)
      // parse number of solenoids as integer
      else if (strcmp("solnum", keybuffer) == 0) {
        dest->solnum = atoi(valbuffer);
        if (dest < 0) {
          fprintf(stderr, "error: expected number of solenoids to be a natural number in line %d\n", linenum);
          exit(1);
        }
        dest->sols = assert_malloc(sizeof(double) * dest->solnum);
        for (int i = 0; i < dest->solnum; i++)
          dest->sols[i] = assert_calloc(PROJECTDATASOLS_LENGTH, sizeof(double));
      }
      // try to read solenoid radiuses and angles
      else {
        int solindex;
        SCANREAD("solradius%d", &solindex, dest->sols[solindex][IDX_RADIUS])
        else SCANREAD("solangle%d", &solindex, dest->sols[solindex][IDX_ANGLE]) // this is converted to radians later
        else SCANREAD("solcoils%d", &solindex, dest->sols[solindex][IDX_COILS])
        else SCANREAD("solselfresistance%d", &solindex, dest->sols[solindex][IDX_SELF_RESISTANCE])
        else SCANREAD("solparallelresistance%d", &solindex, dest->sols[solindex][IDX_PARALLEL_RESISTANCE])
      }
    }
    else if (mode == CALIBRATION_DATA) {
      if CMPREAD("k1", dest->k1)
      else if CMPREAD("k2", dest->k2)
      else if CMPREAD("k3", dest->k3)
      else if CMPREAD("k4", dest->k4)
      else if CMPREAD("k5", dest->k5)
    }
    else if (mode == NORMALISATION_DATA) {
      int solindex;
      SCANREAD("arithmeticmean%d", &solindex, dest->sols[solindex][IDX_MEAN])
      else SCANREAD("standarddeviation%d", &solindex, dest->sols[solindex][IDX_STD_DEVIATION])
      else SCANREAD("noiseminimum%d", &solindex, dest->sols[solindex][IDX_NOISEMIN])
      else SCANREAD("noisemaximum%d", &solindex, dest->sols[solindex][IDX_NOISEMAX])
    }
    
    #undef CMPREAD
    
    free(line);
    line = NULL;
  }
  if ( ! feof(f) ) {
    perror("reading file");
    exit(1);
  }
  
  // convert angles from degree to radians
  if (mode == PENDULUM_DATA) {
    for (int i = 0; i < dest->solnum; i++) {
      dest->sols[i][IDX_ANGLE] = dest->sols[i][IDX_ANGLE] / 360.0 * 2 * M_PI;
    }
  }
  
  return dest;
}

////////////////////////////////////////////////////////////////////////////////
// declared in header
////////////////////////////////////////////////////////////////////////////////

/**
 * Parses the keys l1a, l1b, l1, l2a, l2b, l1m, l2m and the number of solenoids
 * in solnum. Additionaly it parses some values for the 'sols' array in
 * 'projectdata'.
 * NOTE: the solnum key must always occur before the radiuses and angles!
 * 2. NOTE: A new array is initialized for 'sols' in 'dest', every time the
 *   key-value-pair 'solnum' was parsed. Old content is not freed!
 */
projectdata *readPendulumData(projectdata *dest, const char *filepath) {
  return readData(dest, filepath, PENDULUM_DATA);
}
projectdata *readCalibrationData(projectdata *dest, const char *filepath) {
  return readData(dest, filepath, CALIBRATION_DATA);
}
projectdata *readNormalisationData(projectdata *dest, const char *filepath) {
  return readData(dest, filepath, NORMALISATION_DATA);
}
