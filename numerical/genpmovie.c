#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>

static char *tmpl_insert(char *tmpl, int n, double val) {
  char *s;
  asprintf(&s, tmpl, val, n);
  return s;
}

int main(int argc, char *argv[]) {
  if (argc != 5) {
    fputs("invocation: ./genpmovie.x <cmd with %f and %d> <min> <step> <num>", stderr);
    exit(1);
  }
  char *cmd_tmpl = argv[1];
  double min = strtod(argv[2], NULL);
  double step = strtod(argv[3], NULL);
  int num = atoi(argv[4]);
  
  for (int i=0; i<num; i++) {
    char *cmd = tmpl_insert(cmd_tmpl, i, min+i*step);
    printf(">>> %s\n", cmd);
    system(cmd);
    free(cmd);
  }
}
