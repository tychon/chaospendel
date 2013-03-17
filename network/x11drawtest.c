
#include <unistd.h>
#include <math.h>

#include "x11draw.h"

int main(int argc, char *argv[]) {
  generic_surface *surface = createSHMSurface(20, 20, 600, 600);
  sleep(1);
  
  const double dist = 50;
  for (double angle = 0; angle < 2*M_PI; angle += 2*M_PI/100) {
    generic_surface_fill(surface, 0xff000000);
    drawCircle(surface, surface->width/2, surface->height/2, 5, 0xffffffff);
    drawCircle(surface, (int)(cos(angle) * dist)+surface->width/2, (int)(sin(angle) * dist)+surface->height/2, 5, 0xffffffff);
    drawHyperbola(surface
                 , surface->width/2, surface->height/2
                 , cos(angle)*dist+surface->width/2, sin(angle)*dist+surface->height/2
                 , 100
                 , 0xffffffff);
    flushSHMSurface(surface);
    usleep(1000*60);
  }
  sleep(1);
  
  generic_surface_fill(surface, 0xff000000);
  drawCircle(surface, surface->width/2, surface->height/2, 5, 0xffffffff);
  drawCircle(surface, surface->width/2+dist, surface->height/2, 5, 0xffffffff);
  for (double ratio = 1.1; ratio < 100.0; ratio += 0.5) {
    drawHyperbola(surface
                 , surface->width/2, surface->height/2
                 , dist+surface->width/2, surface->height/2+20
                 , ratio
                 , 0xffffffff);
    drawHyperbola(surface
                 , surface->width/2, surface->height/2
                 , dist+surface->width/2, surface->height/2+20
                 , 1/ratio
                 , 0xffffffff);
    flushSHMSurface(surface);
    usleep(1000*100);
  }
  sleep(1);
  
  
  generic_surface_fill(surface, 0xff808080);
  flushSHMSurface(surface);
  sleep(1);
  
  drawBresenhamLine(surface, 10, 10, 75, 125, 0xff00ffff);
  flushSHMSurface(surface);
  sleep(1);
  
  drawRect(surface, 5, 5, 100, 150, 0xffff0000);
  flushSHMSurface(surface);
  sleep(1);
  
  for (int i = 0; i < 100; i++) {
    generic_surface_memshift(surface, -10);
    flushSHMSurface(surface);
    usleep(1000*10);
  }
  sleep(1);
  
  generic_surface_fill(surface, 0xff000000);
  flushSHMSurface(surface);
  sleep(1);
  
  fillRect(surface, 50, 50, 100, 100, 0xff00ff00);
  flushSHMSurface(surface);
  sleep(1);
  
  generic_surface_fill(surface, 0xff000000);
  drawCircle(surface, 100, 75, 50, 0xff00ffff);
  flushSHMSurface(surface);
  sleep(1);
  
  generic_surface_fill(surface, 0xff000000);
  fillCircle(surface, 100, 75, 50, 0xffff0000);
  flushSHMSurface(surface);
  sleep(1);
}

