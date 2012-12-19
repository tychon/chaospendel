// t steht für kinetische Energie
// v steht für potenzielle Energie


#include <math.h>
#include <fmt.h>
#include <stdio.h>
#include <unistd.h>

// Naturkonstante:
// Durchschnittliche Ortskraft in Mitteleuropa.
static double g = -9.81;

// Andere Konstanten
static double time = 60.0;
static double timestep = 0.001;
static double optFps = 60;


// Startbedingungen
/*
static const double l1 = 4;
static const double l2 = 3;
static const double m1 = 2;
static const double m2 = 1;
*/
#define l1 4
#define l2 3
#define m1 2
#define m2 1

static const double phi1_0 = M_PI*1.5;
static const double phi2_0 = M_PI;
static const double p1_0 = 0;
static const double p2_0 = 0;

static const double k1 = (1/3) * (l1*l1) * m1;
static const double k2 = (1/2) * l1      * m1;
static const double k3 = m2;
static const double k4 = (1/3) * l2*(l2) * m2;
static const double k5 = (1/2) * l2      * m2;


// Statistiken
static double t1min = HUGE_VAL;
static double v1min = HUGE_VAL;
static double t2min = HUGE_VAL;
static double v2min = HUGE_VAL;
static double tmin = HUGE_VAL;
static double vmin = HUGE_VAL;
static double emin = HUGE_VAL;
static double t1max = -HUGE_VAL;
static double v1max = -HUGE_VAL;
static double t2max = -HUGE_VAL;
static double v2max = -HUGE_VAL;
static double tmax = -HUGE_VAL;
static double vmax = -HUGE_VAL;
static double emax = -HUGE_VAL;


// Pendel-Zustand
typedef struct {
  double phi1;
  double phi2;
  double p1;
  double p2;
  double kin1;
  double pot1;
  double kin2;
  double pot2;
  double kin;
  double pot;
  double e;
} pstate;


// Differentialgleichungen
static double fphi1(const pstate s) {
  double phidiff = cos(s.phi1-s.phi2);
  return (k4*s.p1-k5*l1*phidiff*s.p2) / (k1*k4+l1*(k3*k4-(k5*k5)*l1*(phidiff*phidiff)));
}
static double fphi2(const pstate s) {
  double phidiff = cos(s.phi1-s.phi2);
  return (k1*s.p2+l1*(k3*s.p2-k5*s.p1*phidiff)) / (k1*k4+l1*(k3*k4-(k5*k5)*l1*(phidiff*phidiff)));
}
static double fp1(const pstate s) {
  return -l1*fphi1(s)*fphi2(s)*k5*sin(s.phi1-s.phi2) - g*k2*sin(s.phi1) - g*l1*k3*sin(s.phi1);
}
static double fp2(const pstate s) {
  return l1*fphi1(s)*fphi2(s)*k5*sin(s.phi1-s.phi2) - g*k5*sin(s.phi2);
}


// Energien
static double t1(double phi1_) { return (1/2) * (phi1_*phi1_) * k1; }
static double v1(double phi1) { return (-g) * k2 * cos(phi1); }
static double t2(double phi1, double phi2, double phi1_, double phi2_) {
  return (1/2)*l1*(phi1_*phi1_)*k3+(1/2)*(phi2_*phi2_)*k4+l1*phi1_*phi2_*k5*cos(phi1-phi2);
}
static double v2(double phi1, double phi2) {
  return (-g) * l1 * k3 * cos(phi1) - g * k5 * cos(phi2);
}


static char outbuf[10001];
static int outbuf_pos = 0;
static void print_state_as_csv(pstate s) {
  if (outbuf_pos + 177 >= 10000) {
    *(outbuf+outbuf_pos) = '\0';
    write(1, outbuf, outbuf_pos+1);
    outbuf_pos = 0;
  }
  char *p = outbuf + outbuf_pos;
  // 11 times 15+1 bytes max plus one newline => 177 bytes max
  p+=fmt_double(p,s.phi1,15,10); *(p++) = ',';
  p+=fmt_double(p,s.phi2,15,10); *(p++) = ',';
  p+=fmt_double(p,s.p1  ,15,10); *(p++) = ',';
  p+=fmt_double(p,s.p2  ,15,10); *(p++) = ',';
  p+=fmt_double(p,s.kin1,15,10); *(p++) = ',';
  p+=fmt_double(p,s.pot1,15,10); *(p++) = ',';
  p+=fmt_double(p,s.kin2,15,10); *(p++) = ',';
  p+=fmt_double(p,s.pot2,15,10); *(p++) = ',';
  p+=fmt_double(p,s.kin ,15,10); *(p++) = ',';
  p+=fmt_double(p,s.pot ,15,10); *(p++) = ',';
  p+=fmt_double(p,s.e   ,15,10); *(p++) = ',';
  *(p++) = '\n';
  outbuf_pos = p - outbuf;
}
static void flush_outbuf() {
  if (outbuf_pos > 0) {
    *(outbuf+outbuf_pos) = '\0';
    write(1, outbuf, outbuf_pos+1);
    outbuf_pos = 0;
  }
}

/*
(phi1':phi2':p1':p2':_) = multiRungeKuttaStep [fphi1, fphi2, fp1, fp2] state timeStep
*/

static pstate calc_s_(pstate s) {
  pstate s_;
  s_.phi1 = fphi1(s);
  s_.phi2 = fphi2(s);
  s_.p1   = fp1(s);
  s_.p2   = fp2(s);
  return s_;
}

static pstate step(pstate s0, double h) {
  // see http://de.wikipedia.org/wiki/Klassisches_Runge-Kutta-Verfahren
  // sxd is the first derivation of sx

  pstate s0d = calc_s_(s0);

  pstate sa;
  sa.phi1 = s0.phi1 + h/2 * s0d.phi1;
  sa.phi2 = s0.phi2 + h/2 * s0d.phi2;
  sa.p1 =   s0.p1   + h/2 * s0d.p1;
  sa.p2 =   s0.p2   + h/2 * s0d.p2;
  pstate sad = calc_s_(sa);

  pstate sb;
  sb.phi1 = s0.phi1 + h/2 * sad.phi1;
  sb.phi2 = s0.phi2 + h/2 * sad.phi2;
  sb.p1 =   s0.p1   + h/2 * sad.p1;
  sb.p2 =   s0.p2   + h/2 * sad.p2;
  pstate sbd = calc_s_(sb);

  pstate sc;
  sc.phi1 = s0.phi1 + h   * sbd.phi1;
  sc.phi2 = s0.phi2 + h   * sbd.phi2;
  sc.p1 =   s0.p1   + h   * sbd.p1;
  sc.p2 =   s0.p2   + h   * sbd.p2;
  pstate scd = calc_s_(sc);

  pstate s1;
  s1.phi1 = s0.phi1 + h/6 * (s0d.phi1 + 2*(sad.phi1+sbd.phi1) + scd.phi1);
  s1.phi2 = s0.phi2 + h/6 * (s0d.phi2 + 2*(sad.phi2+sbd.phi2) + scd.phi2);
  s1.p1 = s0.p1 + h/6 * (s0d.p1 + 2*(sad.p1+sbd.p1) + scd.p1);
  s1.p2 = s0.p2 + h/6 * (s0d.p2 + 2*(sad.p2+sbd.p2) + scd.p2);
  return s1;
}

// Parameters:
//  - `s` is the state of the pendulum in the beginning
//  - `time` is reverse time (as in, it progresses from positive towards zero)
//  - `output_each_nth` specifies how many of the values we generate should
//    appear in our output - 1 means "print each line", 2 means "print every second
//    line" and so on
static void run(pstate s, double timestep, double time, int output_each_nth) {
  int output_i = output_each_nth;
  while (time > 0) {
    s = step(s, timestep);
    time -= timestep;

    output_i--;
    if (output_i == 0) {
      output_i = output_each_nth;

      s.kin1 = t1(fphi1(s));
      s.pot1 = v1(s.phi1);
      s.kin2 = t2(s.phi1, s.phi2, fphi1(s), fphi2(s));
      s.pot2 = v2(s.phi1, s.phi2);
      s.kin = s.kin1 + s.kin2;
      s.pot = s.pot1 + s.pot2;
      s.e = s.kin + s.pot;

      print_state_as_csv(s);
    }
  }
  flush_outbuf();
}

int main(void) {
  fprintf(stderr, "time          = %f\n", time);
  fprintf(stderr, "integral_step = %f\n", timestep);
  fprintf(stderr, "opt_fps       = %f\n", optFps);
  fprintf(stderr, "time_step     = %f\n", 1/optFps);
  fprintf(stderr, "frames_loss   = %f\n", 1-optFps*timestep);
  fprintf(stderr, "l1=%f", l1);
  fprintf(stderr, "l2=%f", l2);
  pstate state0 = {.phi1=phi1_0, .phi2 = phi2_0, .p1 = p1_0, .p2 = p2_0 };
  run(state0, timestep, time, (int)ceil((1/timestep)/optFps));
  //putStr . formatCSV $ outres
  fprintf(stderr, "t1min=%f\n", t1min);
  fprintf(stderr, "v1min=%f\n", v1min);
  fprintf(stderr, "t2min=%f\n", t2min);
  fprintf(stderr, "v2min=%f\n", v2min);
  fprintf(stderr, "tmin=%f\n", tmin);
  fprintf(stderr, "vmin=%f\n", vmin);
  fprintf(stderr, "emin=%f\n", emin);
  fprintf(stderr, "t1max=%f\n", t1max);
  fprintf(stderr, "v1max=%f\n", v1max);
  fprintf(stderr, "t2max=%f\n", t2max);
  fprintf(stderr, "v2max=%f\n", v2max);
  fprintf(stderr, "tmax=%f\n", tmax);
  fprintf(stderr, "vmax=%f\n", vmax);
  fprintf(stderr, "emax=%f\n", emax);
  return 0;
}