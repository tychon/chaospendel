
#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h> // for 'memmove'
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>
#include <math.h>

#include "x11draw.h"

/**
 * Initialize all the X11 stuff.
 * The function puts some infos about the window on stderr.
 */
shmsurface *createSHMSurface(int xpos, int ypos, int width, int height) {
  Display *dpy = XOpenDisplay(NULL);
  if (dpy == NULL) {
    fputs("Can't open display.\n", stderr);
    exit(1);
  }
  
  XVisualInfo vinfo;
  if (!XMatchVisualInfo(dpy, XDefaultScreen(dpy), 32, TrueColor, &vinfo)) {
    fputs("No visual with wished configurations found.\n", stderr);
    exit(1);
  }
  fprintf(stderr, "Matched visual 0x%lx class %d (%s) depth %d\n",
         vinfo.visualid,
         vinfo.class,
         vinfo.class == TrueColor ? "TrueColor" : "unknown",
         vinfo.depth);
  XSync(dpy, True);
  Visual *visual = vinfo.visual;
  int depth = vinfo.depth;
  XSetWindowAttributes attrs;
  attrs.colormap = XCreateColormap(dpy, XDefaultRootWindow(dpy), visual, AllocNone);
  attrs.background_pixel = 0;
  attrs.border_pixel = 0;
  attrs.override_redirect = 1;
  
  int blackColor = BlackPixel(dpy, DefaultScreen(dpy));
  Window window = XCreateWindow(dpy, DefaultRootWindow(dpy), xpos, ypos, width, height, 0,
                           depth, InputOutput, visual, CWOverrideRedirect | CWBackPixel | CWColormap | CWBorderPixel, &attrs);
  XSelectInput(dpy, window, StructureNotifyMask);
  XMapWindow(dpy, window);
  GC gc = XCreateGC(dpy, window, 0, NULL);
  XSetForeground(dpy, gc, blackColor);
  for(;;) {
    XEvent e;
    XNextEvent(dpy, &e);
    if (e.type == MapNotify)
      break;
  }
  
  XShmSegmentInfo *shminfo = malloc(sizeof(XShmSegmentInfo));
  XImage *img = XShmCreateImage(dpy, visual, depth, ZPixmap, NULL, shminfo, width, height);
  if (! img) {
    fprintf(stderr, "error: could not create shared memory image\n");
    exit(1);
  }
  shminfo->shmid = shmget(IPC_PRIVATE, img->bytes_per_line*img->height, IPC_CREAT|0777);
  shminfo->shmaddr = img->data = shmat(shminfo->shmid, 0, 0);
  shminfo->readOnly = False;
  XShmAttach(dpy, shminfo);
  XFlush(dpy);
  
  shmsurface *surf = malloc(sizeof(shmsurface));
  surf->width = width;
  surf->height = height;
  surf->display = dpy;
  memcpy(&surf->window, &window, sizeof(Window));
  memcpy(&surf->graphics_context, &gc, sizeof(GC));
  surf->image = img;
  
  return surf;
}

/**
 * Flush, flush, flush! the image.
 * -> make it visible on the screen.
 */
void flushSHMSurface(shmsurface *surface) {
  XShmPutImage(surface->display
             , surface->window
             , surface->graphics_context
             , surface->image
             , 0, 0, 0, 0
             , surface->width
             , surface->height
             , False);
  XFlush(surface->display);
}

/**
 * Shift all the data in the image. A positive xshift makes the data go to the
 * right, a negative xshift makes the data go to the left.
 * Be aware: If the xshift is negative, pixels disappearing on the left, appear
 * one row higher on the right. And if the xshift is positive, pixels
 * disappearing on the right appear one row lower on the left!
 */
void shmsurface_memshift(shmsurface *surface, int xshift) {
  if (xshift < 0) { // move to the left
    memmove(surface->image->data
          , surface->image->data - xshift*sizeof(int)
          , surface->image->bytes_per_line * surface->image->height + xshift*sizeof(int));
  } else if (xshift > 0) { // move to the right
    memmove(surface->image->data + xshift*sizeof(int)
          , surface->image->data
          , surface->image->bytes_per_line * surface->image->height - xshift*sizeof(int));
  }
}
/**
 * Set all the pixels in the surface to one and only one 'color'.
 */
void shmsurface_fill(shmsurface *surface, int color) {
  memset(surface->image->data
       , color
       , surface->image->bytes_per_line * surface->image->height);
}

#define SWAPINTS(a, b) { int swapints_temp = a; a = b; b = swapints_temp; }
#define SWAPDOUBLES(a, b) { double swapd_temp = a; a = b; b = swapd_temp; }
#define STDPLOT(surface, x, y, color) { *(((int*)surface->image->data)+(x)+(y)*surface->width) = color; }

/**
 * Draw a simple dot, to put it in other words: color one pixel.
 */
void drawDot(shmsurface *surface, int x, int y, int color) {
  STDPLOT(surface, x, y, color)
}

/**
 * Draw a line from point (x0, y0) to (x1, y1).
 * 
 * This is an implementation of Bresenham's line algorithm.
 * code found on the english Wikipedia
 */
void drawBresenhamLine(shmsurface *surface, int x0, int y0, int x1, int y1, int color) {
  int steep = abs(y1-y0) > abs(x1-x0);
  if (steep) {
    SWAPINTS(x0, y0)
    SWAPINTS(x1, y1)
  }
  if (x0 > x1) {
    SWAPINTS(x0, x1)
    SWAPINTS(y0, y1)
  }
  
  int deltax = x1 - x0;
  int deltay = abs(y1-y0);
  int error = deltax / 2;
  int ystep = y0 < y1 ? 1 : -1;
  int y = y0;
  
  for (int x = x0; x <= x1; x++) {
    if (steep) STDPLOT(surface, y, x, color)
    else STDPLOT(surface, x, y, color)
    error -= deltay;
    if (error < 0) {
      y += ystep;
      error += deltax;
    }
  }
}

// how about Xiaolin Wu's line algorithm?
// You could draw anti-aliased lines!

/**
 * Draw the borders of the given rect.
 */
void drawRect(shmsurface *surface, int xpos, int ypos, int width, int height, int color) {
  int tmp = ypos + height - 1;
  for (int x = xpos; x < xpos + width; x++) {
    STDPLOT(surface, x, ypos, color)
    STDPLOT(surface, x, tmp, color)
  }
  tmp = xpos+width-1;
  for (int y = ypos; y < ypos + height; y++) {
    STDPLOT(surface, xpos, y, color)
    STDPLOT(surface, tmp, y, color)
  }
}

/**
 * Fills a given area with the given color.
 * For filling the whole surface use the more efficient
 * function 'shmsurface_fill'.
 */
void fillRect(shmsurface *surface, int xpos, int ypos, int width, int height, int color) {
  for (int x = xpos; x < xpos+width; x++) {
    for (int y = ypos; y < ypos+height; y++) {
      STDPLOT(surface, x, y, color)
    }
  }
}

/**
 * Draws the borders of a circle with its center at (xpos, ypos).
 * This is an implementation of the midpoint circle algorithm.
 * code fount on the english Wikipedia
 */
void drawCircle(shmsurface *surface, int xpos, int ypos, int radius, int color) {
  int f = 1 - radius;
  int ddF_x = 1;
  int ddF_y = -2 * radius;
  int x = 0;
  int y = radius;
  
  STDPLOT(surface, xpos, ypos+radius, color)
  STDPLOT(surface, xpos, ypos-radius, color)
  STDPLOT(surface, xpos+radius, ypos, color)
  STDPLOT(surface, xpos-radius, ypos, color)
  
  while (x < y) {
    if (f >= 0) {
      y --;
      ddF_y += 2;
      f += ddF_y;
    }
    
    x ++;
    ddF_x += 2;
    f += ddF_x;
    
    STDPLOT(surface, xpos+x, ypos+y, color)
    STDPLOT(surface, xpos-x, ypos+y, color)
    STDPLOT(surface, xpos+x, ypos-y, color)
    STDPLOT(surface, xpos-x, ypos-y, color)
    
    STDPLOT(surface, xpos+y, ypos+x, color)
    STDPLOT(surface, xpos-y, ypos+x, color)
    STDPLOT(surface, xpos+y, ypos-x, color)
    STDPLOT(surface, xpos-y, ypos-x, color)
  }
}

/**
 * Fills all pixels within a given radius around (xpos, ypos) with the
 * given color.
 * This is a slightly modified version of the midpoint circle algorithm.
 */
void fillCircle(shmsurface *surface, int xpos, int ypos, int radius, int color) {
  int f = 1 - radius;
  int ddF_x = 1;
  int ddF_y = -2 * radius;
  int x = 0;
  int y = radius;
  
  int tmp;
  for (tmp = xpos-radius; tmp <= xpos+radius; tmp ++) STDPLOT(surface, tmp, ypos, color)
  
  while (x < y) {
    if (f >= 0) {
      y --;
      ddF_y += 2;
      f += ddF_y;
    }
    
    x ++;
    ddF_x += 2;
    f += ddF_x;
    
    for (tmp = xpos-x; tmp <= xpos+x; tmp ++) STDPLOT(surface, tmp, ypos+y, color)
    for (tmp = xpos-x; tmp <= xpos+x; tmp ++) STDPLOT(surface, tmp, ypos-y, color)
    
    for (tmp = xpos-y; tmp <= xpos+y; tmp ++) STDPLOT(surface, tmp, ypos+x, color)
    for (tmp = xpos-y; tmp <= xpos+y; tmp ++) STDPLOT(surface, tmp, ypos-x, color)
  }
}

/**
 * This is veery inefficient and not even accurate.
 */
int drawHyperbola(shmsurface *surface
                 , double ax, double ay
                 , double fx, double fy
                 , double ratio
                 , int color) {
  if (ratio <= 0 || ratio == 1) return -1;
  if (ratio < 1) {
    SWAPDOUBLES(ax, fx)
    SWAPDOUBLES(ay, fy)
    ratio = 1 / ratio;
  }
  
  double xdiff = fx - ax;
  double ydiff = fy - ay;
  double midx = ax + xdiff/2;
  double midy = ay + ydiff/2;
  double angle = atan2(ydiff, xdiff);
  double dist = sqrt(xdiff * xdiff + ydiff * ydiff);
  double e = dist / 2.0; // focal distance
  double a = dist * ratio / (1.0 + ratio) - e; // angular point
  double b = sqrt(e * e - a * a);
  
  double x, y, angleadd;
  double xreal, xreal2, yreal, yreal2;
  double lastx1, lasty1, lastx2, lasty2;
  
  x = a;
  for (;;) {
    y = b * sqrt(x*x - a*a) / a;
    
    angleadd = atan2(y, x);
    xreal = cos(angle + angleadd) * x + midx;
    xreal2 = cos(angle - angleadd) * x + midx;
    yreal = sin(angle + angleadd) * y + midy;
    yreal2 = sin(angle - angleadd) * y + midy;
    
    if (xreal < 0 || xreal >= surface->width
        || xreal2 < 0 || xreal2 >= surface->width
        || yreal < 0 || yreal >= surface->height
        || yreal2 < 0 || yreal2 >= surface->height) break;
        
    printf("%lf, %lf, %lf, %lf, %lf; %lf %lf\n", e, a, angle, b, y, xreal, yreal);
    
    if (x == a) {
      STDPLOT(surface, (int)lround(xreal), (int)lround(yreal), color);
      STDPLOT(surface, (int)lround(xreal2), (int)lround(yreal2), color);
    } else {
      drawBresenhamLine(surface, lastx1, lasty1, (int)lround(xreal), (int)lround(yreal), color);
      drawBresenhamLine(surface, lastx2, lasty2, (int)lround(xreal2), (int)lround(yreal2), color);
    }
    lastx1 = xreal;
    lasty1 = yreal;
    lastx2 = xreal2;
    lasty2 = yreal2;
    
    x += 0.1;
  }
  
  return 0;
}

#undef SWAPDOUBLES
#undef SWAPINTS
#undef STDPLOT

