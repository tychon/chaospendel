
#ifndef _X11DRAW_H
#define _X11DRAW_H

#include <X11/Xlib.h>

struct shmsurface {
  int width, height;
  Display *display;
  Window window;
  GC graphics_context;
  XImage *image;
};
typedef struct shmsurface shmsurface;

shmsurface *createSHMSurface(int xpos, int ypos, int width, int height);
void flushSHMSurface(shmsurface *surface);

void shmsurface_memshift(shmsurface *surface, int xshift);
void shmsurface_fill(shmsurface *surface, int color);

void drawDot(shmsurface *surface, int x, int y, int color);
void drawBresenhamLine(shmsurface *surface, int x0, int y0, int x1, int y1, int color);
void drawRect(shmsurface *surface, int xpos, int ypos, int width, int height, int color);
void fillRect(shmsurface *surface, int xpos, int ypos, int width, int height, int color);
void drawCircle(shmsurface *surface, int xpos, int ypos, int radius, int color);
void fillCircle(shmsurface *surface, int xpos, int ypos, int radius, int color);

int drawHyperbola(shmsurface *surface
                 , double ax, double ay
                 , double fx, double fy
                 , double ratio
                 , int color);

#endif // _X11DRAW_H

