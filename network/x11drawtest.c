
#include <unistd.h>

#include "x11draw.h"

int main(int argc, char *argv[]) {
  shmsurface *surface = createSHMSurface(20, 20, 200, 200);
  sleep(1);
  
  drawCircle(surface, 80, 100, 5, 0xffffffff);
  drawCircle(surface, 120, 100, 5, 0xffffffff);
  drawHyperbola(surface, 100, 100, 1.0, 20.0, 1, 0xff00ff00);
  drawHyperbola(surface, 100, 100, -5.0, 20.0, 1, 0xff00ffff);
  drawHyperbola(surface, 100, 100, 10.0, 20.0, 1, 0xffffff00);
  drawHyperbola(surface, 100, 100, 15.0, 20.0, 1, 0xffffffff);
  drawHyperbola(surface, 100, 100, 18.0, 20.0, 1, 0xffff00ff);
  flushSHMSurface(surface);
  sleep(10);
  
  
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
  
  shmsurface_fill(surface, 0xff000000);
  drawCircle(surface, 100, 75, 50, 0xff00ffff);
  flushSHMSurface(surface);
  sleep(2);
  
  shmsurface_fill(surface, 0xff000000);
  fillCircle(surface, 100, 75, 50, 0xffff0000);
  flushSHMSurface(surface);
  sleep(2);
}

