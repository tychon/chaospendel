
#include <unistd.h>
#include <math.h>

#include "x11draw.h"

int main(int argc, char *argv[]) {
  shmsurface *surface = createSHMSurface(20, 20, 200, 200);
  sleep(1);
  
  const double dist = 50;
  for (double angle = 0; angle < 2*M_PI; angle += 2*M_PI/100) {
    shmsurface_fill(surface, 0xff000000);
    drawCircle(surface, 100, 100, 5, 0xffffffff);
    drawCircle(surface, (int)(cos(angle) * dist)+100, (int)(sin(angle) * dist)+100, 5, 0xffffffff);
    drawHyperbola(surface
                 , 100, 100
                 , (int)(cos(angle) * dist)+100, (int)(sin(angle) * dist)+100
                 , 3
                 , 0xffffffff);
    flushSHMSurface(surface);
    usleep(1000*100);
  }
  sleep(1);
  
  shmsurface_fill(surface, 0xff000000);
  drawCircle(surface, 100, 100, 5, 0xffffffff);
  drawCircle(surface, 100+dist, 100, 5, 0xffffffff);
  for (double ratio = 1.1; ratio < 10.0; ratio += 0.5) {
    drawHyperbola(surface
                 , 100, 100
                 , dist+100, 100
                 , ratio
                 , 0xffffffff);
    drawHyperbola(surface
                 , 100, 100
                 , dist+100, 100
                 , 1/ratio
                 , 0xffffffff);
    flushSHMSurface(surface);
    usleep(1000*500);
  }
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
  
  shmsurface_fill(surface, 0xff000000);
  drawCircle(surface, 100, 75, 50, 0xff00ffff);
  flushSHMSurface(surface);
  sleep(2);
  
  shmsurface_fill(surface, 0xff000000);
  fillCircle(surface, 100, 75, 50, 0xffff0000);
  flushSHMSurface(surface);
  sleep(2);
}

