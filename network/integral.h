
#ifndef _INTEGRAL_H
#define _INTEGRAL_H

#include "ringbuffer.h"

typedef struct {
  double max_reset_threshold;
  /// Number of samples to collect in the absolute range of max_reset_threshold
  /// before resetting the integral
  int min_reset_samples;
  
  double sum; /// This is the actual integral
  /// Number of continuous samples below threshold
  int reset_samples;
} integral;

integral *integral_allocate(double reset_threshold, size_t reset_samples);
double integral_push(integral *integ, double val);
double integral_getsum(integral *integ);

#endif // _INTEGRAL_H

