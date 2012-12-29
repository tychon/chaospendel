
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

void shmsurfshift(shmsurface *surface, int xshift);

int drawBresenhamLine(shmsurface *surface, int x0, int y0, int x1, int y1, int color);

#endif // _X11DRAW_H

