// Transform a loopgraph from [phi1;phi2] coordinates to
// [x;y] coordinates.

#include <math.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/mman.h>

#define l1 0.285
#define l2b TODO

/* -----  HELPERS ----- */
double xatof(const char *nptr) {
  if (*nptr == '\0') {
    fputs("an empty string is not a valid number!\n", stderr);
    exit(1);
  }
  char *end;
  double val = strtod(nptr, &end);
  if (*end != '\0') {
    fprintf(stderr, "%s: \"%s\"\n",
      (nptr==end)?"unreadable number":"can't parse whole number",
      nptr
    );
    exit(1);
  }
  return val;
}

int xatoi(const char *nptr) {
  if (*nptr == '\0') {
    fputs("an empty string is not a valid number!\n", stderr);
    exit(1);
  }
  char *end;
  int val = (int)strtol(nptr, &end, 10);
  if (*end != '\0') {
    fprintf(stderr, "%s: \"%s\"\n",
      (nptr==end)?"unreadable number":"can't parse whole number",
      nptr
    );
    exit(1);
  }
  return val;
}

/* Like fgets, but exits on error and strips newlines. */
void grabline(char *buf, int buflen, FILE *f, char *errstr) {
  if (!fgets(buf, buflen, f))
    perror(errstr?errstr:"can't read expected line"), exit(1);
  size_t datalen = strlen(buf);
  if (buf[datalen-1] == '\n') buf[datalen-1] = '\0';
  return buf;
}

/* Like grabline, but skips comment lines */
void grabline_nocomment(char *buf, int buflen, FILE *f, char *errstr) {
start:
  grabline(buf, buflen, f, errstr);
  if (buf[0] == '#') goto start;
}

#define nmax(a,b) ( ((a)>(b)) ? (a) : (b) )
#define nmin(a,b) ( ((a)<(b)) ? (a) : (b) )


/* -----  COORDINATE TRANSFORMATION STUFF  ----- */

struct angle_pair { double phi1,phi2; };
struct polar { double phi,r; };
struct cartesian { double x,y; };
struct frange { double min, max; }; /* floating-point range */

// Convert polar coordinates to cartesian coordinates.
struct cartesian pol_to_car(struct polar in) {
  return (struct cartesian) {
    .x = in.r * cos(in.phi),
    .y = in.r * sin(in.phi)
  };
}

struct cartesian car_add(struct cartesian a, struct cartesian b) {
  return (struct cartesian) {
    .x = a.x+b.x,
    .y = a.y+b.y
  };
}

struct cartesian angles_to_cartesian(struct angle_pair in) {
  return car_add(
    pol_to_car((struct polar) {
      .phi = in.phi1,
      .r = l1
    }),
    pol_to_car((struct polar) {
      .phi = in.phi2,
      .r = l2
    })
  );
}

/* transforms a value into a position in an image, value is double
 * so we can calculate weight without loss of precision */
double scale(double d, int maxn, struct frange r) {
  d -= r.min;
  d /= r.max-r.min;
  d *= maxn;
  return d;
}

/* transforms a position in an image to the actual value */
double unscale(int n, int maxn, struct frange r) {
  double d = n; /* [0..maxn] */
  d /= maxn;    /* [0..1] */
  d *= r.max-r.min;
  d += r.min;
  return d;
}

double dist(double x1, double y1, double x2, double y2) {
  double dx = x1-x2;
  double dy = y1-y2;
  return sqrt(dx*dx+dy*dy);
}


/* -----  PROGRAM LOGIC  ----- */
struct outpixel { double numerator, denominator; };

struct frange phi1_range, phi2_range;
int width_in, height_in;
char *data_in;

struct frange x_range, y_range;
int width_out, height_out;
struct outpixel *pixels_out;

int main(int argc, char **argv) {
  /* -- parse args -- */
  if (argc != 13) fputs(stderr, "bad invocation\n"), exit(1);
  
  char *in_name = argv[1];
  phi1_range.min = xatof(argv[2]);
  phi1_range.max = xatof(argv[3]);
  phi2_range.min = xatof(argv[4]);
  phi2_range.max = xatof(argv[5]);
  
  char *out_name = argv[6];
  x_range.min = xatof(argv[7]);
  x_range.max = xatof(argv[8]);
  y_range.min = xatof(argv[9]);
  y_range.max = xatof(argv[10]);
  width_out = xatoi(argv[11]);
  height_out = atoi(argv[12]);
  
  
  /* -- read and parse infile -- */
  FILE *f_in = fopen(in_name, "r");
  if (!f_in) perror("can't open infile"), exit(1);
  char lbuf[256];
  grabline(lbuf, sizeof(lbuf), f_in, "can't read magic");
  if (strcmp(lbuf, "P6")) fputs("bad magic", stderr), exit(1);
  grabline_nocomment(lbuf, sizeof(lbuf), f_in, "can't read dimensions");
  if (sscanf(lbuf, "%d %d", &width_in, &height_in) != 2)
    fprintf(stderr, "can't parse dimensions: \"%s\"\n", lbuf), exit(1);
  grabline_nocomment(lbuf, sizeof(lbuf), f_in, "can't read depth");
  if (strcmp(lbuf, "255"))
    fprintf(stderr, "depth is %s, not 255\n", lbuf), exit(1);
  int infile_size = width_in * height_in * 3;
  long in_pos = ftell(f_in);
  /* map the whole file... */
  data_in = mmap(NULL, in_pos+infile_size, PROT_READ, MAP_PRIVATE, getfd(f_in), 0);
  if (data_in == MAP_FAILED) perror("can't mmap infile"), exit(1);
  /* then bump the pointer up to the image data */
  data_in += in_pos;
  
  
  /* -- calculate outdata -- */
  pixels_out = assert_calloc(infile_size, sizeof(struct outpixel));
  for (int color = 1; color < 3; color++) {
    for (int src_x=0; src_x<width_in; src_x++) {
      double phi2 = unscale(src_x, width_in, phi2_range);
      for (int src_y=0; src_y<height_in; src_y++) {
        double phi1 = unscale(src_y, height_in, phi1_range);
        double value = data[(src_y*width_in+src_x)*3+color] / (double)255;
        
        // we add pi/2 to compensate that our polar coordinate system has
        // 0 pointing to the bottom while the normal one has 0 pointing to
        // the right
        struct cartesian out_coords = angles_to_cartesian(
          (struct angle_pair){.phi1=phi1+M_PI/2,.phi2=phi2+M_PI/2}
        );
        
        // out_coords is in meters now, but we want pixels: scale it!
        out_coords.x = scale(out_coords.x, width_out, x_range);
        out_coords.y = scale(out_coords.y, height_out, y_range);
        
        // flip y because stupid humans want y to increase from bottom to
        // top. stupid humans. define math stuff before looking at how
        // to implement it in computers. nobody cares about simplicity
        // and performance anymore ;(
        out_coords.y = (height_out-1) - out_coords.y;
        
        // ok, out_coords is the projected position in the out-image now
        
        int xout_r = (int)round(out_coords.x);
        int yout_r = (int)round(out_coords.y);
        for (int x=nmax(0,xout_r-10); x<nmin(xout_r+10,width_out); x++) {
          for (int y=nmax(0,yout_r-10); y<nmin(yout_r+10,height_out); y++) {
            // add 0.0001 to prevent DIV/0 errors
            double weight = 1/(0.0001+dist(out_coords.x, out_coords.y, x, y));
            struct outpixel *px = pixels_out+(y*width+x)*3;
            px->numerator += value * weight;
            px->denominator += weight;
          }
        }
      }
    }
  }
  
  
  /* -- write outdata -- */
  FILE *f_out = fopen(out_name, "wx");
  if (f_out == NULL) perror("can't open outfile"), exit(1);
  if (fprintf(f_out, "P6\n%d %d\n255\n", width_out, height_out) < 0)
    perror("can't write outheaders"), exit(1);
  for (struct outpixel *px = pixels_out; px < pixels_out+infile_size; px++) {
    unsigned char px_val = (unsigned char)round(255*px->numerator/px->denominator);
    if (fwrite(&px_val, 1, 1, f_out) != 1)
      perror("fwrite failed"), exit(1);
  }
  if (fclose(f_out)) perror("can't close f_out"), exit(1);
}