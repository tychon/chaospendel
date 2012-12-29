
#include <unistd.h>

#include "x11draw.h"

int main(int argc, char *argv[]) {
  shmsurface *surface = createSHMSurface(20, 20, 200, 200);
  
  drawBresenhamLine(surface, 10, 10, 100, 75, 0xff0000ff);
  flushSHMSurface(surface);
  
  sleep(1);
  
  drawBresenhamLine(surface, 10, 10, 75, 125, 0xff00ffff);
  flushSHMSurface(surface);
  
  sleep(3);
}

