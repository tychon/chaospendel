
#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h> // for 'memmove'
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>
#include <math.h>
#include <assert.h>

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
  fprintf(stderr, "img->data is at %p\n", img->data);
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

#define CHECKRANGE(surface, x, y) { \
  assert((x) >= 0); \
  assert((y) >= 0); \
  assert((x) < surface->width); \
  assert((y) < surface->height); \
}

#define ISRANGE(x, y) (                                                     \
  (x) >= 0 && (y) >= 0 && (x) < (surface)->width && (y) < (surface)->height \
)

#define SWAPINTS(a, b) { int swapints_temp = a; a = b; b = swapints_temp; }
#define SWAPDOUBLES(a, b) { double swapd_temp = a; a = b; b = swapd_temp; }
#define STDPLOT(surface, x, y, color) { \
  CHECKRANGE(surface, x, y) \
  *(((int*)surface->image->data)+(x)+(y)*surface->width) = color; \
}

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
  CHECKRANGE(surface, x0, y0)
  CHECKRANGE(surface, x1, y1)

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
  CHECKRANGE(surface, xpos, ypos)
  CHECKRANGE(surface, xpos+width, ypos+height)

  if (width < 0) {
    xpos += width;
    width *= -1;
  }
  if (height < 0) {
    ypos += height;
    height *= -1;
  }
  
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
  CHECKRANGE(surface, xpos, ypos)
  CHECKRANGE(surface, xpos+width, ypos+height)

  if (width < 0) {
    xpos += width;
    width *= -1;
  }
  if (height < 0) {
    ypos += height;
    height *= -1;
  }
  
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
  CHECKRANGE(surface, xpos-radius, ypos-radius)
  CHECKRANGE(surface, xpos+radius, ypos+radius)

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
  CHECKRANGE(surface, xpos-radius, ypos-radius)
  CHECKRANGE(surface, xpos+radius, ypos+radius)

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

int drawHyperbola(shmsurface *surface
                 , double ax, double ay
                 , double fx, double fy
                 , double f/*ratio*/
                 , int color) {
  CHECKRANGE(surface, ax, ay)
  CHECKRANGE(surface, fx, fy)

  if (f <= 0 || f == 1) return -1;

  double xdiff = fx - ax;
  double ydiff = fy - ay;
  double dist = sqrt(xdiff * xdiff + ydiff * ydiff);
  double e = dist / 2.0; // focal distance
  double a = dist * f / (1.0 + f) - e; // angular point
  
  int lxreal_l = -1, lxreal_r = -1
    , lyreal_lp = -1, lyreal_ln = -1
    , lyreal_rp = -1, lyreal_rn = -1
  ;
  
  for (double x=0; a-x>=0 || a+x<surface->width; x++) {
    double xreal_l = a-x;
    double xreal_r = a+x;
    double xadq_l = (xreal_l-fx)*(xreal_l-fx);
    double xbdq_l = (xreal_l-ax)*(xreal_l-ax);
    double xadq_r = (xreal_r-fx)*(xreal_r-fx);
    double xbdq_r = (xreal_r-ax)*(xreal_r-ax);
    double yreal_lp = (sqrt(-xadq_l*f*f*f*f+xadq_l*f*f+xbdq_l*f*f-xbdq_l+f*f*fy*fy-2*f*f*fy*ay+f*f*ay*ay)+f*f*fy-ay)/(f*f-1);
    double yreal_ln = (-sqrt(-xadq_l*f*f*f*f+xadq_l*f*f+xbdq_l*f*f-xbdq_l+f*f*fy*fy-2*f*f*fy*ay+f*f*ay*ay)+f*f*fy-ay)/(f*f-1);
    double yreal_rp = (sqrt(-xadq_r*f*f*f*f+xadq_r*f*f+xbdq_r*f*f-xbdq_r+f*f*fy*fy-2*f*f*fy*ay+f*f*ay*ay)+f*f*fy-ay)/(f*f-1);
    double yreal_rn = (-sqrt(-xadq_r*f*f*f*f+xadq_r*f*f+xbdq_r*f*f-xbdq_r+f*f*fy*fy-2*f*f*fy*ay+f*f*ay*ay)+f*f*fy-ay)/(f*f-1);

    // these are rounded values for bounds checks and drawing
    int rxreal_l = lround(xreal_l)
      , rxreal_r = lround(xreal_r)
      , ryreal_lp = isfinite(yreal_lp) ? lround(yreal_lp) : -1
      , ryreal_ln = isfinite(yreal_ln) ? lround(yreal_ln) : -1
      , ryreal_rp = isfinite(yreal_rp) ? lround(yreal_rp) : -1
      , ryreal_rn = isfinite(yreal_rn) ? lround(yreal_rn) : -1
    ;
    
    if (x != 0) {
      if (ISRANGE(lxreal_l, 0) && ISRANGE(xreal_l, 0)) {
        if (ISRANGE(0, lyreal_lp) && ISRANGE(0, ryreal_lp)) {
          drawBresenhamLine(surface, lxreal_l, lyreal_lp, rxreal_l, ryreal_lp, color);
        }
        if (ISRANGE(0, lyreal_ln) && ISRANGE(0, ryreal_ln)) {
          drawBresenhamLine(surface, lxreal_l, lyreal_ln, rxreal_l, ryreal_ln, color);
        }
      }
      if (ISRANGE(lxreal_r, 0) && ISRANGE(rxreal_r, 0)) {
        if (ISRANGE(0, lyreal_rp) && ISRANGE(0, ryreal_rp)) {
          drawBresenhamLine(surface, lxreal_r, lyreal_rp, rxreal_r, ryreal_rp, color);
        }
        if (ISRANGE(0, lyreal_rn) && ISRANGE(0, ryreal_rn)) {
          drawBresenhamLine(surface, lxreal_r, lyreal_rn, rxreal_r, ryreal_rn, color);
        }
      }
    }
    
    lxreal_l  = lround(xreal_l);
    lxreal_r  = lround(xreal_r);
    lyreal_lp = isfinite(yreal_lp) ? lround(yreal_lp) : -1;
    lyreal_ln = isfinite(yreal_ln) ? lround(yreal_ln) : -1;
    lyreal_rp = isfinite(yreal_rp) ? lround(yreal_rp) : -1;
    lyreal_rn = isfinite(yreal_rn) ? lround(yreal_rn) : -1;
  }
  
  return 0;
}

#undef SWAPDOUBLES
#undef SWAPINTS
#undef STDPLOT

