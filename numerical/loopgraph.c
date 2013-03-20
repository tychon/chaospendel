#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

// x is phi2, y is phi1

int main(int argc, char **argv) {
  if (argc != 11) {
    fprintf(stderr, "wrong invocation: want width, height, minphi1, maxphi1, minphi2, maxphi2, binoutpath, csvoutpath, starti, nofi!\n");
    exit(1);
  }
  int width = atoi(argv[1]);
  int height = atoi(argv[2]);
  double minphi1 = strtod(argv[3], NULL);
  double maxphi1 = strtod(argv[4], NULL);
  double minphi2 = strtod(argv[5], NULL);
  double maxphi2 = strtod(argv[6], NULL);
  char *outpath = argv[7];
  char *csvoutpath = argv[8];
  int starti = atoi(argv[9]);
  int nofi = atoi(argv[10]);
  
  int outfd = open(outpath, O_RDWR|O_CREAT|O_TRUNC, 0666);
  if (outfd == -1) {
    perror("can't open binout file");
    return 1;
  }
  
  FILE *csvout = fopen(csvoutpath, "w");
  if (csvout == NULL) {
    perror("can't open csvout file");
    return 1;
  }
  
  char *header = NULL;
  asprintf(&header, "P6\n%d %d\n255\n", width, height);
  if (header == NULL) exit(1);
  if (write(outfd, header, strlen(header)) != strlen(header)) exit(1);
  int datasize = 3*width*height;
  if (ftruncate(outfd, strlen(header)+datasize)) exit(1);
  
  time_t t0 = time(NULL);
  
  unsigned char *data_ = mmap(NULL, strlen(header)+datasize, PROT_READ|PROT_WRITE, MAP_SHARED, outfd, 0);
  if (data_ == MAP_FAILED) exit(1);
  unsigned char *data = data_ + strlen(header);
  
  setvbuf(stdout, NULL, _IONBF, 0);
  setvbuf(csvout, NULL, _IONBF, 0); /* TODO FIXME WHAT THE FUCK IS UP HERE? This line fixes an output bug... is stdio broken? */
  
  for (int y=starti/width; y<height; y++) {
    /* y=0 means phi1=pi/2, y=max means phi1=0 */
    double phi1 = maxphi1 - (maxphi1-minphi1)*(y/(double)height);
    for (int x=(y==starti/width)?(starti%width):0; x<width; x++) {
      int i = y*width+x;
      if (i == starti+nofi) goto end;
      printf("\rstep %d (%d of %d)", i, i-starti, nofi);
      if (i != starti) {
        time_t t = time(NULL)-t0;
        printf(", ~%d minutes total          ", (int)(t*nofi/(i-starti)/60));
      }
      
      double phi2 = minphi2 + (maxphi2-minphi2)*(x/(double)width);
      
      unsigned char *pixel = data + 3*i;
      pixel[0] = 0;
      
      int pipefds[2];
      if (pipe(pipefds)) exit(1);
      int forkres = fork();
      if (forkres == -1) exit(1);
      if (forkres == 0) {
        dup2(pipefds[1], 1);
        close(0);
        char *cmd = NULL;
        asprintf(&cmd, "./fast_pendulum.x --phi1 %f --phi2 %f", phi1, phi2);
        system(cmd);
        exit(0);
      }
      close(pipefds[1]);
      if (read(pipefds[0], pixel+1, 1) != 1) exit(1);
      if (read(pipefds[0], pixel+2, 1) != 1) exit(1);
      fprintf(csvout, "%f,%f,%hhu,%hhu\n", phi1, phi2, *(unsigned char*)(pixel+1), *(unsigned char*)(pixel+2));
      close(pipefds[0]);
      waitpid(forkres, NULL, 0);
    }
  }
end:
  printf("\rdone!                                   \n");
}