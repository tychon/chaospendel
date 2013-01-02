
#include "memory_wrappers.h"

#include "integral.h"

integral *integral_allocate(size_t windowsize, double reset_threshold, size_t reset_samples) {
  integral *integ = assert_malloc(sizeof(integral));
  integ->max_reset_threshold = reset_threshold;
  integ->min_reset_samples = reset_samples;
  integ->sum = 0.0;
  integ->buffer = ringbuffer_allocate(windowsize, sizeof(double));
  integ->reset_samples = 1;
  return integ;
}



