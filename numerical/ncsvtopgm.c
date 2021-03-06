
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "common.h"

typedef enum {
  ALL, ROWS, COLUMNS
} autoscale;

int main(int argc, char *argv[]) {
  char *inputpath = NULL
     , *outputpath = NULL;
  int columns = 1
    , pgmmaxval = 255;
  bool mirror = false
     , maximum = false
     , logarithmic = false
     , invert = false
     , binary = false;
  autoscale ascale = ALL;
  
  for (int i = 1; i < argc; i++) {
    if      (argcmpass("--inputpath|-i", argc, argv, &i, &inputpath));
    else if (argcmpass("--outputpath|-o", argc, argv, &i, &outputpath));
    else if (argcmpassint("--columns|-c", argc, argv, &i, &columns));
    else if (! strcmp("--mirror", argv[i])) mirror = true;
    else if (! strcmp("--maximum", argv[i])) maximum = true;
    else if (! strcmp("--logarithmic", argv[i])) logarithmic = true;
    else if (! strcmp("--scalerows", argv[i])) ascale = ROWS;
    else if (! strcmp("--scalecols", argv[i])) ascale = COLUMNS;
    else if (! strcmp("--invert", argv[i])) invert = true;
    else if (! strcmp("--binary", argv[i])) binary = true;
    else {
      fprintf(stderr, "Unknown argument: %s\n", argv[i]);
      exit(1);
    }
  }
  
  if (binary) pgmmaxval = 255;
  
  //// load csv file ////
  int allocated_rows = 100;
  double **data = assert_malloc(allocated_rows * sizeof(double*));
  int rows = 0; // count rows
  
  FILE *inpf = inputpath ? fopen(inputpath, "r") : stdin;
  if (! inpf) {
    perror("Could not open input file");
    exit(1);
  }
  double numbers[columns];
  for (;;) {
    int retv;
    if ( (retv = parseNCSVLine(inpf, columns, numbers))) {
      if (retv == -1) break; // end of file
      fprintf(stderr, "ERROR: Bad CSV file! err %d line %d\n", retv, rows+1);
      exit(1);
    }
    
    if (rows == allocated_rows) {
      allocated_rows += 1000;
      data = assert_realloc(data, allocated_rows * sizeof(double*));
    }
    
    data[rows] = assert_malloc(columns * sizeof(double));
    memcpy(data[rows], numbers, columns * sizeof(double));
    rows++;
  }
  
  if (inputpath) fclose(inpf);
  
  //// mirror and scale ////
  
  if (ascale == ROWS) {
    for (int row = 0; row < rows; row ++) {
      double maxval = 0;
      for (int col = 0; col < columns; col ++) {
        if (data[row][col] < 0) {
          fprintf(stderr, "Invalid number smaller zero in row %d col %d: %f\n", row+1, col+1, data[row][col]);
          exit(1);
        }
        if (data[row][col] > maxval) maxval = data[row][col];
      }
      double scale = maxval == 0.0 ? 1.0 : pgmmaxval / maxval;
      fprintf(stderr, "scale%d=%f\n", row, scale);
      for (int col = 0; col < columns; col ++) {
        data[row][col] = data[row][col] * scale;
      }
    }
  }
  else if (ascale == COLUMNS) {
    //TODO scale columns
  }
  else {
    double maxval = 0;
    for (int row = 0; row < rows; row ++) {
      for (int col = 0; col < columns; col ++) {
        if (data[row][col] < 0) {
          fprintf(stderr, "Invalid number smaller zero in row %d col %d: %f\n", row+1, col+1, data[row][col]);
          exit(1);
        }
        if (data[row][col] > maxval) maxval = data[row][col];
      }
    }
    double scale = maxval == 0.0 ? 1.0 : pgmmaxval / maxval;
    fprintf(stderr, "scale=%f\n", scale);
    for (int row = 0; row < rows; row ++) {
      for (int col = 0; col < columns; col ++) {
        data[row][col] = data[row][col] * scale;
      }
    }
  }
  
  if (maximum) {
    for (int row = 0; row < rows; row ++) {
      double val1 = data[row][0]
           , val2 = data[row][1];
      for (int col = 2; col < columns; col ++) {
        if (val1 < val2 && val2 > data[row][col]) {
          val1 = val2;
          val2 = data[row][col];
          data[row][col-2] = 0.0;
        } else {
          val1 = val2;
          val2 = data[row][col];
          data[row][col-1] = 0.0;
        }
      }
      if (val2 > data[row][columns-1])
        data[row][columns-1] = 0.0;
    }
  }
  
  if (logarithmic) {
    int scale = pgmmaxval / log1p(pgmmaxval);
    for (int row = 0; row < rows; row ++) {
      for (int col = 2; col < columns; col ++) {
        data[row][col] = log1p(data[row][col]) * scale;
      }
    }
  }
  
  if (mirror) {
    //TODO mirror
  }
  
  if (invert) {
    for (int row = 0; row < rows; row ++) {
      for (int col = 0; col < columns; col ++) {
        data[row][col] = pgmmaxval - data[row][col];
      }
    }
  }
  
  //// output ////
  FILE *outf = outputpath ? fopen(outputpath, "w+") : stdout;
  if (! outf) {
    perror("Could not open output file");
    exit(1);
  }
  
  if (binary) fprintf(outf, "P5\n%d %d\n%d\n", columns, rows, pgmmaxval);
  else fprintf(outf, "P2\n%d %d\n%d\n", columns, rows, pgmmaxval);
  
  for (int row = 0; row < rows; row ++) {
    for (int col = 0; col < columns-1; col ++) {
      if (binary) fwrite(&data[row][col], 1, sizeof(unsigned char), outf);
      else fprintf(outf, "%d ", (int)data[row][col]);
    }
    if (binary) fwrite(&data[row][columns-1], 1, sizeof(unsigned char), outf);
    else fprintf(outf, "%d\n", (int)data[row][columns-1]);
  }
  
  if (outputpath) fclose(outf);
}

