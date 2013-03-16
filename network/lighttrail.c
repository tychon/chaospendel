
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
#include <assert.h>

#include "common.h"
#include "projectreader.h"
#include "uds_server.h"
#include "uds_client.h"
#include "protocol.h"
#include "x11draw.h"
#include "integral.h"
#include "../numerical/pendulum.h"

static projectdata *pd;
static shmsurface *sf;
static char *datafilepath = "../numerical/out.bin";

void toPendulumCartesian(double radius, double angle, double *x, double *y) {
  *x = sin(angle) * radius;
  *y = cos(angle) * radius;
}
void pstateToCartesianEnd(pstate *s, double *second_x, double *second_y) {
  double first_x, first_y;
  toPendulumCartesian(pd->l1, s->phi1, &first_x, &first_y);
  double second_x_rel, second_y_rel;
  toPendulumCartesian(pd->l2b, s->phi2, &second_x_rel, &second_y_rel);
  *second_x = first_x+second_x_rel;
  *second_y = first_y+second_y_rel;
}

static int whitebg = 0;

void drawPendulum(pstate *states, int states_len) {
  int left_loopings = 0, right_loopings = 0;
  
  int green_color = COLOR_GREEN;
  int red_color = COLOR_RED;
  int blue_color = COLOR_BLUE;
  // clear surface
  if (whitebg) {
    shmsurface_fill(sf, COLOR_WHITE);
    green_color = 0xff009600;
    red_color = 0xff960000;
    blue_color = 0xff000096;
  } else shmsurface_fill(sf, COLOR_BLACK);
  
  // precompute scaling
  double maxpendlength = (double)(pd->l1 + (pd->l2a > pd->l2b ? pd->l2a : pd->l2b));
  // this scale is in pixels per meter
  double scale = (double)(sf->width < sf->height ? sf->width : sf->height) / 2.0 * (4.0/5.0) / maxpendlength;
  
  // draw center (main axis of pendulum)
  drawDot(sf, sf->width/2, sf->height/2, red_color);
  
  // draw data
  int last_xpos, last_ypos, xpos, ypos;
  int last_section, cur_section;
  double xpos_, ypos_;
  for (int i = 0; i < states_len; i++) {
    pstate *s = &states[i];
    
    pstateToCartesianEnd(s, &xpos_, &ypos_);
    xpos = (int)(xpos_ * scale);
    ypos = (int)(ypos_ * scale);
    xpos += sf->width/2;
    ypos += sf->height/2;
    
    cur_section = s->phi2/(M_PI/2);
    cur_section = ((cur_section%4)+4)%4;
    assert(cur_section >= 0 && cur_section < 4);
    
    if (i != 0) {
      // the first point does not have a previous one
      // to which we could draw a line, so ignore it
      //fprintf(stderr, "(%d|%d)->(%d|%d)\n", last_xpos, last_ypos, xpos, ypos);
      int color = green_color;
      if (last_section == 1/*upper left*/ && cur_section == 2/*upper right*/) { color = red_color; right_loopings++; }
      if (last_section == 2/*upper right*/ && cur_section == 1/*upper left*/) { color = blue_color; left_loopings++; }
      drawBresenhamLine(sf, last_xpos, last_ypos, xpos, ypos, color);
    }
    
    last_xpos = xpos;
    last_ypos = ypos;
    last_section = cur_section;
  }
  
  fprintf(stderr, "pendulum rendered. %d loopings to the left, %d loopings to the right.\n", left_loopings, right_loopings);
}

void run() {
  struct stat st;
  if (stat(datafilepath, &st)) {
    fprintf(stderr, "can't stat \"%s\": %s\n", datafilepath, strerror(errno));
    exit(1);
  }
  // file truncated? whatever.
  int states_len = st.st_size / sizeof(pstate);
  int fd = open(datafilepath, O_RDONLY);
  if (fd == -1) {
    fprintf(stderr, "can't open \"%s\": %s\n", datafilepath, strerror(errno));
    exit(1);
  }
  void *states = mmap(NULL, states_len*sizeof(pstate), PROT_READ, MAP_PRIVATE, fd, 0);
  if (states == MAP_FAILED) {
    fprintf(stderr, "can't mmap \"%s\n: %s\n", datafilepath, strerror(errno));
    exit(1);
  }
  close(fd);
  fprintf(stderr, "drawing pendulum with %d states on %s...\n", states_len, whitebg?"white":"black");
  drawPendulum(states, states_len);
  munmap(states, states_len*sizeof(pstate));
  flushSHMSurface(sf);
}

int main(int argc, char *argv[]) {
  char *pendulumdatapath = "data_pendulum";
  
  for (int i = 1; i < argc; i++) {
    if (argcmpass("--pendulum|-p", argc, argv, &i, &pendulumdatapath)) ;
    else if (argcmpass("--datafilepath|-d", argc, argv, &i, &datafilepath)) ;
    else if (ARGCMP("--whitebg", i)) {
      whitebg = 1;
    }
    else fprintf(stderr, "warning: Unknown argument ignored: \"%s\"\n", argv[i]);
  }
  
  pd = assert_malloc(sizeof(projectdata));
  
  fprintf(stderr, "reading pendulum data from \"%s\" ...\n", pendulumdatapath);
  readPendulumData(pd, pendulumdatapath);
  
  // x11 things
  sf = createSHMSurface(100, 100, 500, 500);
  
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
  
  run();
  
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
    run();
  }
}

