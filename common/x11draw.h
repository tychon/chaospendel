
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

struct generic_surface {
  int width, height;
  int bytes_per_line;
  Display *display;
  Window window;
  GC graphics_context;
  XImage *image;
  char *data;
};
typedef struct generic_surface generic_surface;

generic_surface *createSHMSurface(int xpos, int ypos, int width, int height);
void flushSHMSurface(generic_surface *surface);

void generic_surface_memshift(generic_surface *surface, int xshift);
void generic_surface_fill(generic_surface *surface, int color);

void drawDot(generic_surface *surface, int x, int y, int color);
void drawBresenhamLine(generic_surface *surface, int x0, int y0, int x1, int y1, int color);
void drawRect(generic_surface *surface, int xpos, int ypos, int width, int height, int color);
void fillRect(generic_surface *surface, int xpos, int ypos, int width, int height, int color);
void drawCircle(generic_surface *surface, int xpos, int ypos, int radius, int color);
void fillCircle(generic_surface *surface, int xpos, int ypos, int radius, int color);

int drawHyperbola(generic_surface *surface
                 , double ax, double ay
                 , double fx, double fy
                 , double ratio
                 , int color);

void dump_ppm(int fd, generic_surface *s);

#endif // _X11DRAW_H

