
#include "ncsv.h"

#include <stdlib.h>
#include <string.h>

// returns 0 if the correct amount of data was parsed
int parseNCSVLine(FILE *f, int columns, double *res) {
  char buf[10000];
  if (fgets(buf, sizeof(buf), f) == NULL) {
    return -1; // probably EOF
  }
  
  char *strtok_buf = buf;
  char *p = buf;
  int resindex = 0;
  while ((p = strtok(strtok_buf, ",")) != NULL) {
    strtok_buf = NULL;
    if (resindex == columns) return -3; // no more space in res
    char *endptr;
    res[resindex++] = strtod(p, &endptr);
    if (endptr == p) {
      fprintf(stderr, "WARNING: unparseable number!\n");
      exit(1);
    } else if (*endptr != 0 && *endptr != 10 && *endptr != 13) {
      fprintf(stderr, "WARNING: can't parse the whole number! hex code %x\n", (int)*endptr);
      exit(1);
    }
  }
  if (resindex == columns) {
    return 0; // everything is fine
  } else {
    return -2; // didn't parse at least `columns` columns
  }
}

void printNCSVLine(FILE *f, int columns, double *data) {
  for (int i = 0; i < columns-1; i++) {
    fprintf(f, "%f,", data[i]);
  }
  fprintf(f, "%f", data[columns-1]);
  fputc('\n', f);
}

