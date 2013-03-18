#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
  // parse arguments
  if (argc < 3) {
    fprintf(stderr, "give me at least one input and one output file!\n");
    exit(1);
  }
  char *outpath = argv[1];
  char **inpaths = argv+2;
  int inpaths_len = argc-2;
  
  // map infiles
  char **infiles = alloca(sizeof(char*)*inpaths_len);
  off_t file_size = 0;
  for (int i=0; i<inpaths_len; i++) {
    char *path = inpaths[i];
    struct stat st;
    int fd = open(path, O_RDONLY);
    if (fd == -1) {
      fprintf(stderr, "can't open input file \"%s\": %s\n", path, strerror(errno));
      exit(1);
    }
    if (fstat(fd, &st)) {
      fprintf(stderr, "can't stat input fd for file \"%s\": %s\n", path, strerror(errno));
      exit(1);
    }
    if (i==0) {
      file_size = st.st_size;
    } else {
      if (file_size != st.st_size) {
        fprintf(stderr, "size mismatch between input files \"%s\" and \"%s\", aborting!\n", inpaths[0], path);
        exit(1);
      }
    }
    infiles[i] = mmap(NULL, file_size, PROT_READ, MAP_SHARED, fd, 0);
    if (infiles[i] == MAP_FAILED) {
      fprintf(stderr, "can't mmap \"%s\": %s\n", path, strerror(errno));
      exit(1);
    }
    close(fd);
  }
  
  // map outfile
  int out_fd = open(outpath, O_RDWR|O_CREAT|O_EXCL, 0777);
  if (out_fd == -1) {
    perror("can't open outfile");
    exit(1);
  }
  if (ftruncate(out_fd, file_size)) {
    perror("can't allocate disk space for outfile");
    exit(1);
  }
  char *outfile = mmap(NULL, file_size, PROT_READ|PROT_WRITE, MAP_SHARED, out_fd, 0);
  if (outfile == MAP_FAILED) {
    perror("can't mmap outfile");
    exit(1);
  }
  close(out_fd);
  
  // merge data
  for (off_t i=0; i<file_size; i++) {
    char *outc = outfile+i;
    for (int j=0; j<inpaths_len; j++) {
      char inc = infiles[j][i];
      if (*outc != 0 && inc != 0 && *outc != inc) {
        fprintf(stderr, "input file integrity is bad, this won't work\n");
        exit(1);
      }
      if (inc != 0) *outc = inc;
    }
  }
  
  fprintf(stderr, "looks good ☺\n");
}