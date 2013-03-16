
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <signal.h>
#include <stdbool.h>
#include <sys/time.h>
#include <sys/inotify.h>
#include <errno.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <linux/limits.h>

#include "common.h"
#include "projectreader.h"
#include "uds_server.h"
#include "uds_client.h"
#include "protocol.h"
#include "x11draw.h"
#include "integral.h"
#include "../numerical/pendulum.h"

void toPendulumCartesian(double radius, double angle, double *x, double *y) {
  *x = sin(angle) * radius;
  *y = cos(angle) * radius;
}
void pstateToCartesianEnd(pstate *s, projectdata *pd, double *second_x, double *second_y) {
  double first_x, first_y;
  toPendulumCartesian(pd->l1, s->phi1, &first_x, &first_y);
  double second_x_rel, second_y_rel;
  toPendulumCartesian(pd->l2b, s->phi2, &second_x_rel, &second_y_rel);
  *second_x = first_x+second_x_rel;
  *second_y = first_y+second_y_rel;
}

void drawPendulum(shmsurface *sf, projectdata *pd, pstate *states, int states_len, int whitebg) {
  int green_color = COLOR_GREEN;
  int red_color = COLOR_RED;
  // clear surface
  if (whitebg) {
    shmsurface_fill(sf, COLOR_WHITE);
    green_color = 0xff009600;
    red_color = 0xff960000;
  } else shmsurface_fill(sf, COLOR_BLACK);
  
  // precompute scaling
  double maxpendlength = (double)(pd->l1 + (pd->l2a > pd->l2b ? pd->l2a : pd->l2b));
  // this scale is in pixels per meter
  double scale = (double)(sf->width < sf->height ? sf->width : sf->height) / 2.0 * (4.0/5.0) / maxpendlength;
  
  // draw center (main axis of pendulum)
  drawDot(sf, sf->width/2, sf->height/2, red_color);
  
  // draw data
  int last_xpos, last_ypos, xpos, ypos;
  double xpos_, ypos_;
  for (int i = 0; i < states_len; i++) {
    pstate *s = &states[i];
    pstateToCartesianEnd(s, pd, &xpos_, &ypos_);
    xpos = (int)(xpos_ * scale);
    ypos = (int)(ypos_ * scale);
    xpos += sf->width/2;
    ypos += sf->height/2;
    
    if (i != 0) {
      // the first point does not have a previous one
      // to which we could draw a line, so ignore it
      //fprintf(stderr, "(%d|%d)->(%d|%d)\n", last_xpos, last_ypos, xpos, ypos);
      drawBresenhamLine(sf, last_xpos, last_ypos, xpos, ypos, green_color);
    }
    
    last_xpos = xpos;
    last_ypos = ypos;
  }
}

void run(char *datafpath, shmsurface *sf, projectdata *pd, int whitebg) {
  struct stat st;
  if (stat(datafpath, &st)) {
    fprintf(stderr, "can't stat \"%s\": %s\n", datafpath, strerror(errno));
    exit(1);
  }
  // file truncated? whatever.
  int states_len = st.st_size / sizeof(pstate);
  int fd = open(datafpath, O_RDONLY);
  if (fd == -1) {
    fprintf(stderr, "can't open \"%s\": %s\n", datafpath, strerror(errno));
    exit(1);
  }
  void *states = mmap(NULL, states_len*sizeof(pstate), PROT_READ, MAP_PRIVATE, fd, 0);
  if (states == MAP_FAILED) {
    fprintf(stderr, "can't mmap \"%s\n: %s\n", datafpath, strerror(errno));
    exit(1);
  }
  close(fd);
  fprintf(stderr, "drawing pendulum with %d states on %s...\n", states_len, whitebg?"white":"black");
  drawPendulum(sf, pd, states, states_len, whitebg);
  munmap(states, states_len*sizeof(pstate));
  flushSHMSurface(sf);
}

int main(int argc, char *argv[]) {
  char *pendulumdatapath = "data_pendulum";
  char *datafilepath = "../numerical/out.bin";
  int whitebg = 0;
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--pendulum|-p", argc, argv, &i, &pendulumdatapath)) ;
    else if (argcmpass("--datafilepath|-d", argc, argv, &i, &datafilepath)) ;
    else if (ARGCMP("--whitebg", i)) {
      whitebg = 1;
    }
    else fprintf(stderr, "warning: Unknown argument ignored: \"%s\"\n", argv[i]);
  }
  
  projectdata *pd = assert_malloc(sizeof(projectdata));
  
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  readPendulumData(pd, pendulumdatapath);
  
  // x11 things
  shmsurface *surface = createSHMSurface(100, 100, 500, 500);
  
  int in_fd = inotify_init();
  if (in_fd == -1) {
    fprintf(stderr, "can't create inotify fd: %s\n", strerror(errno));
    exit(1);
  }
  int in_watch = inotify_add_watch(in_fd, datafilepath, IN_CLOSE_WRITE);
  if (in_watch == -1) {
    fprintf(stderr, "can't create inotify watch: %s\n", strerror(errno));
    exit(1);
  }
  
  run(datafilepath, surface, pd, whitebg);
  
  while (1) {
    size_t evt_size = sizeof(struct inotify_event)+NAME_MAX+1;
    struct inotify_event *evt = alloca(evt_size);
    int readres = read(in_fd, evt, evt_size);
    if (readres == 0) {
      fprintf(stderr, "inotify EOF\n");
      exit(1);
    }
    if (readres == -1) {
      fprintf(stderr, "inotify read error: %s\n", strerror(errno));
      exit(1);
    }
    // we ignore the actual inotify event – there's only one thing it could
    // plausibly be, and if we're wrong, we just run one more iteration
    run(datafilepath, surface, pd, whitebg);
  }
}

