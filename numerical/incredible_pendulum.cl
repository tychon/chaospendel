#pragma OPENCL EXTENSION cl_khr_byte_addressable_store : enable

#define ITERATIONS 600000
#define timestep 0.0001
#define g 9.81
#define l1 0.285
#define k1 0.0284
#define k2 0.118
#define k3 0.448
#define k4 0.00441
#define k5 0.0254
#define FLOAT_PI 3.14159265358979323846264338327950288419716939938


// Data structure for current values during sim
typedef struct {
  float phi1;
  float phi2;
  float p1;
  float p2;
} pstate;


//////// Equations of motion and energy ////////
// Differential equations
inline float fphi1(const pstate s) {
  float phidiff = cos(s.phi1-s.phi2);
  return (k4*s.p1-k5*l1*phidiff*s.p2) / (k1*k4+l1*l1*(k3*k4-k5*k5*phidiff*phidiff));
}
inline float fphi2(const pstate s) {
  float phidiff = cos(s.phi1-s.phi2);
  return (k1*s.p2+l1*(k3*l1*s.p2-k5*s.p1*phidiff)) / (k1*k4+l1*l1*(k3*k4-k5*k5*phidiff*phidiff));
}
inline float fp1(const pstate s, float newphi1, float newphi2) {
  return -l1*newphi1*newphi2*k5*sin(s.phi1-s.phi2) - g*k2*sin(s.phi1) - g*l1*k3*sin(s.phi1);
}
inline float fp2(const pstate s, float newphi1, float newphi2) {
  return l1*newphi1*newphi2*k5*sin(s.phi1-s.phi2) - g*k5*sin(s.phi2);
}

pstate calc_s_(pstate s) {
  pstate s_;
  s_.phi1 = fphi1(s);
  s_.phi2 = fphi2(s);
  s_.p1   = fp1(s, s_.phi1, s_.phi2);
  s_.p2   = fp2(s, s_.phi1, s_.phi2);
  return s_;
}

inline pstate mystep(pstate s0, float h) {
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

__kernel void simulate_pendulum(__global float *in, __global unsigned char *out_) {
  // grab our stuff from the environment
  size_t tid = get_global_id(0);
  float phi1_0 = in[2*tid+0];
  float phi2_0 = in[2*tid+1];
  __global unsigned char *out = out_+3*tid;
  unsigned char loopings_left=0, loopings_right=0;
  
  pstate s = {.phi1=phi1_0, .phi2 = phi2_0, .p1 = 0, .p2 = 0 };
  float phi2 = floor((s.phi2+FLOAT_PI) / (2 * FLOAT_PI));
  float phi2new;
  int i;
  for (i=0; i<ITERATIONS; i++) {
    s = mystep(s, timestep);
    
    phi2new = floor((s.phi2+FLOAT_PI) / (2 * FLOAT_PI));
    if (phi2new > phi2) loopings_left ++;
    else if (phi2new < phi2) loopings_right ++;
    phi2 = phi2new;
  }
  
  out[0] = 0;
  out[1] = loopings_left;
  out[2] = loopings_right;
}
