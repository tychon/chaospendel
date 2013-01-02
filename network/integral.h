
#ifndef _INTEGRAL_H
#define _INTEGRAL_H

#include "ringbuffer.h"

typedef struct {
  double max_reset_threshold;
  /// Number of samples to collect in the absolute range of max_reset_threshold
  /// before resetting the integral
  int min_reset_samples;
  
  double sum; /// This is the actual integral
  ringbuffer *buffer;
  /// Number of continuous samples below threshold
  int reset_samples;
} integral;

integral *integral_allocate(size_t windowsize, double reset_threshold, size_t reset_samples);

#endif // _INTEGRAL_H

