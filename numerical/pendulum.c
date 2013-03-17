
#include <math.h>
#include <fmt.h>
#include <stdio.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "pendulum.h"

// Average gravity in Germany
static const double g = 9.81;

/////// Other constants with default vals ////////
// Total time of simulation
static double time = 60.0;
// Time per step in Runge-Kutta
static double simtimestep = 0.0001;
// Time per sample in output
static double outtimestep = 1. / 80.;
// Output as binary, otherwise ASCII CSV
static bool binout = false;

// Startbedingungen
#define l1 4.0
#define l2 3.0
#define m1 2.0
#define m2 1.0

static double phi1_0 = M_PI / 4.0 + M_PI;
static double phi2_0 = M_PI / 2.0;
static double p1_0 = 0;
static double p2_0 = 0;

static double k1 = (1.0/3) * (l1*l1) * m1;
static double k2 = (1.0/2) * l1      * m1;
static double k3 = m2;
static double k4 = (1.0/3) * l2*(l2) * m2;
static double k5 = (1.0/2) * l2      * m2;

//////// Variables about minimum and maximum vals ///////
// These are updated during `run`
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
static int loopings_left = 0;
static int loopings_right = 0;

//////// Equations of motion and energy ////////
// Differential equations
static double fphi1(const pstate s) {
  double phidiff = cos(s.phi1-s.phi2);
  return (k4*s.p1-k5*l1*phidiff*s.p2) / (k1*k4+l1*l1*(k3*k4-k5*k5*phidiff*phidiff));
}
static double fphi2(const pstate s) {
  double phidiff = cos(s.phi1-s.phi2);
  return (k1*s.p2+l1*(k3*l1*s.p2-k5*s.p1*phidiff)) / (k1*k4+l1*l1*(k3*k4-k5*k5*phidiff*phidiff));
}
static double fp1(const pstate s) {
  return -l1*fphi1(s)*fphi2(s)*k5*sin(s.phi1-s.phi2) - g*k2*sin(s.phi1) - g*l1*k3*sin(s.phi1);
}
static double fp2(const pstate s) {
  return l1*fphi1(s)*fphi2(s)*k5*sin(s.phi1-s.phi2) - g*k5*sin(s.phi2);
}

// Energies
static double t1(double phi1d) { return (1.0/2) * (phi1d*phi1d) * k1; }
static double v1(double phi1) { return (-g) * k2 * cos(phi1); }
static double t2(double phi1, double phi2, double phi1d, double phi2d) {
  return (1.0/2)*l1*l1*(phi1d*phi1d)*k3+(1.0/2)*(phi2d*phi2d)*k4+l1*phi1d*phi2d*k5*cos(phi1-phi2);
}
static double v2(double phi1, double phi2) {
  return (-g) * l1 * k3 * cos(phi1) - g * k5 * cos(phi2);
}


static void print_state(pstate s) {
  if (binout) {
    write(1, &s, sizeof(pstate));
  } else {
    printf("%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f\n"
        , s.phi1
        , s.phi2
        , s.p1
        , s.p2
        , s.kin1
        , s.pot1
        , s.kin2
        , s.pot2
        , s.kin
        , s.pot
        , s.e);
  }
}

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
  double phi2, phi2new;
  while (time > 0) {
    phi2 = floor((fabs(s.phi2)+M_PI) / (2 * M_PI));
    
    s = step(s, timestep);
    
    phi2new = floor((fabs(s.phi2)+M_PI) / (2 * M_PI));
    if (phi2new > phi2) loopings_left ++;
    else if (phi2new < phi2) loopings_right ++;
    
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

      #define UPDATEIF(var, cond, newval) if (newval cond var) var = newval;

      UPDATEIF(t1min, <, s.kin1)
      UPDATEIF(v1min, <, s.pot1)
      UPDATEIF(t2min, <, s.kin2)
      UPDATEIF(v2min, <, s.pot2)
      UPDATEIF(tmin, <, s.kin)
      UPDATEIF(vmin, <, s.pot)
      UPDATEIF(emin, <, s.e)

      UPDATEIF(t1max, >, s.kin1)
      UPDATEIF(v1max, >, s.pot1)
      UPDATEIF(t2max, >, s.kin2)
      UPDATEIF(v2max, >, s.pot2)
      UPDATEIF(tmax, >, s.kin)
      UPDATEIF(vmax, >, s.pot)
      UPDATEIF(emax, >, s.e)

      print_state(s);
    }
  }
}

int main(int argc, char *argv[]) {
  for (int i = 1; i < argc; i++) {
    if (!strcmp("--binout", argv[i])) binout = true;
    if (!strcmp("--time", argv[i])) { i++; time = strtod(argv[i], NULL); }
    
    // start conditions
    if (!strcmp("--phi1", argv[i])) { i++; phi1_0 = strtod(argv[i], NULL); }
    if (!strcmp("--phi2", argv[i])) { i++; phi2_0 = strtod(argv[i], NULL); }
    if (!strcmp("--p1", argv[i])) { i++; p1_0 = strtod(argv[i], NULL); }
    if (!strcmp("--p2", argv[i])) { i++; p2_0 = strtod(argv[i], NULL); }
    
    // constant pendulum properties (unless someone moves the magnet...)
    if (!strcmp("--k1", argv[i])) { i++; k1 = strtod(argv[i], NULL); }
    if (!strcmp("--k2", argv[i])) { i++; k2 = strtod(argv[i], NULL); }
    if (!strcmp("--k3", argv[i])) { i++; k3 = strtod(argv[i], NULL); }
    if (!strcmp("--k4", argv[i])) { i++; k4 = strtod(argv[i], NULL); }
    if (!strcmp("--k5", argv[i])) { i++; k5 = strtod(argv[i], NULL); }
  }
  
  fprintf(stderr, "time          = %f\n", time);
  fprintf(stderr, "integral_step = %f\n", simtimestep);
  fprintf(stderr, "opt_fps       = %f\n", 1/outtimestep);
  fprintf(stderr, "time_step     = %f\n", outtimestep);
  fprintf(stderr, "frames_loss   = %f\n", 1-1/outtimestep*simtimestep);
  fprintf(stderr, "l1=%f\n", l1);
  fprintf(stderr, "l2=%f\n", l2);
  pstate state0 = {.phi1=phi1_0, .phi2 = phi2_0, .p1 = p1_0, .p2 = p2_0 };
  run(state0, simtimestep, time, (int)ceil((1/simtimestep)*outtimestep));
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
  fprintf(stderr, "loopings_left=%d\n", loopings_left);
  fprintf(stderr, "loopings_right=%d\n", loopings_right);
  return 0;
}
