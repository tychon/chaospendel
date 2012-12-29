
#include <unistd.h>

#include "x11draw.h"

int main(int argc, char *argv[]) {
  shmsurface *surface = createSHMSurface(20, 20, 200, 200);
  sleep(1);
  
  shmsurface_fill(surface, 0xff808080);
  flushSHMSurface(surface);
  sleep(1);
  
  drawBresenhamLine(surface, 10, 10, 75, 125, 0xff00ffff);
  flushSHMSurface(surface);
  sleep(1);
  
  drawRect(surface, 5, 5, 100, 150, 0xffff0000);
  flushSHMSurface(surface);
  sleep(1);
  
  for (int i = 0; i < 100; i++) {
    shmsurface_memshift(surface, -10);
    flushSHMSurface(surface);
    usleep(1000*10);
  }
  sleep(1);
  
  shmsurface_fill(surface, 0xff000000);
  flushSHMSurface(surface);
  sleep(1);
  
  fillRect(surface, 50, 50, 100, 100, 0xff00ff00);
  flushSHMSurface(surface);
  sleep(1);
}

