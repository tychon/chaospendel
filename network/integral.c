
#include <math.h>
#include "memory_wrappers.h"

#include "integral.h"

integral *integral_allocate(size_t windowsize, double reset_threshold, size_t reset_samples) {
  integral *integ = assert_malloc(sizeof(integral));
  integ->max_reset_threshold = reset_threshold;
  integ->min_reset_samples = reset_samples;
  integ->sum = 0.0;
  integ->buffer = ringbuffer_allocate(windowsize, sizeof(double));
  integ->reset_samples = 0;
  return integ;
}

double integral_push(integral *integ, double val) {
  if (ringbuffer_push(integ->buffer, &val)) {
    // ringbuffer is full and integral was not resetted
    // what should we do here?
  }
  integ->sum += val;
  if (fabs(val) <= integ->max_reset_threshold) {
    integ->reset_samples ++;
    if (integ->reset_samples == integ->min_reset_samples) {
      // reset integral
      ringbuffer_clear(integ->buffer);
      integ->sum = 0;
      integ->reset_samples = 0;
    }
  } else {
    // reset reset counter :-)
    integ->reset_samples = 0;
  }
  
  return integ->sum;
}

double integral_getsum(integral *integ) {
  return integ->sum;
}

