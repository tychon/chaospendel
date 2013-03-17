
#include "ncsv.h"

#include <stdlib.h>
#include <string.h>

// returns 0 if the correct amount of data was parsed
bool parseNCSVLine(FILE *f, int columns, double *res) {
  char buf[1000];
  if (fgets(buf, 1000, f) == NULL) {
    return -2; // real error
  }
  
  char *strtok_buf = buf;
  char *p = buf;
  int resindex = 0;
  while ((p = strtok(strtok_buf, ",")) != NULL) {
    strtok_buf = NULL;
    if (resindex == columns) return 1; // no more space in res
    char *endptr;
    res[resindex++] = strtod(p, &endptr);
    if (endptr == p) {
      fprintf(stderr, "WARNING: unparseable number!\n");
    } else if (*endptr != 0) {
      fprintf(stderr, "WARNING: can't parse the whole number!\n");
    }
  }
  if (resindex == columns) {
    return true; // everything is fine
  } else {
    return false; // didn't parse at least `columns` columns
  }
}

void printNCSVLine(FILE *f, int columns, double *data) {
  for (int i = 0; i < columns-1; i++) {
    fprintf(f, "%f,", data[i]);
  }
  fprintf(f, "%f", data[columns-1]);
  fputc('\n', f);
}

