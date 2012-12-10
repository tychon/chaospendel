
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "time_join.h"

#define MILLIS_TIMESTAMP_MAX_DIGITS 20 // somewhere in some thousands of years
#define VALUES_MAX_DIGITS 5 // arduino specific

/**
 * After the successfull execution:
 * startptr points to the opening '[' of the timestamp.
 * If the whole buffer was matched, endptr is NULL. Otherwise it points to the
 * first character after the last digit matched for a value.
 * 
 * NOTE: empty values (e.g. "...TIMESTAMP]256,,374,234,...") default to zero!
 * 
 * @returns positive integer with number of values read or error code:
 * -1: not even the beginning of a standardline was found (*endptr: unspecified)
 * -2: invalid character in timestamp (*endptr: index of invalid character)
 * -3: timestamp not closed (*endptr: unspecified)
 * -4: invalid character in value (*endptr: index of invalid character)
 */
int parseStandardLine(standardline *result
                , char **startptr
                , char **endptr
                , char *buffer
                , int bufferlength
                , int nvalues) {
  // find beginning of next line
  if (startptr != NULL) *startptr = NULL;
  int stampstart = -1; // index of opening '['
  for (int i = 0; i < bufferlength; i++) {
    if (buffer[i] == '[') {
      stampstart = i;
      break;
    }
  }
  if (stampstart < 0)
    // ERROR: beginning of line not found
    return -1;
  if (startptr != NULL) *startptr = buffer+stampstart;
  
  // parse timestamp
  char tdigits[MILLIS_TIMESTAMP_MAX_DIGITS+1];
  int digitcounter = 0; // index of next digit in 'digits'
  int stampend = -1; // index of closing ']'
  for (int i = stampstart+1; i < bufferlength; i++) {
    if (buffer[i] == ']') {
      // end of buffer
      stampend = i;
      break;
    }
    else if (buffer[i] >= 48 && buffer[i] <= 57) {
      // valid digit read
      tdigits[digitcounter] = buffer[i];
      digitcounter ++;
      if (digitcounter == MILLIS_TIMESTAMP_MAX_DIGITS) {
        fprintf(stderr, "maximum number of timestamp digits reached.\n");
        exit(1);
      }
    }
    else {
      // ERROR: invalid character read
      if (endptr != NULL) *endptr = buffer+i;
      return -2;
    }
  }
  if (stampend == -1) {
    // ERROR: time stamp not closed
    return -3;
  }
  tdigits[digitcounter] = '\0';
  result->timestamp = strtoll(tdigits, NULL, 0);
  
  // parse values
  char vdigits[VALUES_MAX_DIGITS+1];
  digitcounter = 0;
  int valcount = 0;
  int i;
  for (i = stampend+1; i < bufferlength && valcount < nvalues; i++) {
    if (endptr != NULL) *endptr = buffer+i;
    
    if (buffer[i] >= 48 && buffer[i] <= 57) {
      // valid digit found
      vdigits[digitcounter] = 0;
      digitcounter ++;
      if (digitcounter == VALUES_MAX_DIGITS) {
        fprintf(stderr, "maximum number of digits in value reached.\n");
        exit(1);
      }
    }
    else if (buffer[i] == ',' || buffer[i] == '[') {
      // end of valid value
      if (digitcounter == 0) {
        // empty value defaults to zero
        result->values[valcount] = 0;
      } else {
        // parse digits
        vdigits[digitcounter] = '\0';
        result->values[valcount] = strtol(vdigits, NULL, 0);
        digitcounter = 0;
        valcount ++;
      }
    } else {
      // invalid digit read
      return -4;
    }
    
    if (buffer[i] == '[') {
      return valcount;
    }
  }
  if (endptr != NULL && i == bufferlength)
    // reached end of buffer (it is not ensured, that all values are read!
    *endptr = NULL;
  
  return valcount;
}

