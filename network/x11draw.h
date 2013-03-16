
#ifndef _X11DRAW_H
#define _X11DRAW_H

#include <X11/Xlib.h>

#define COLOR_BLACK ((int)0xff000000)
#define COLOR_WHITE ((int)0xffffffff)
#define COLOR_RED ((int)0xffff0000)
#define COLOR_GREEN ((int)0xff00ff00)
#define COLOR_BLUE ((int)0xff0000ff)
#define COLOR_CYAN ((int)0xff00ffff)
#define COLOR_MAGENTA ((int)0xff00ff)
#define COLOR_YELlOW ((int)0xffffff00)

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

void dump_ppm(int fd, shmsurface *s);

#endif // _X11DRAW_H

