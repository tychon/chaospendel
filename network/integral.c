
#include <math.h>
#include "memory_wrappers.h"

#include "integral.h"

integral *integral_allocate(double reset_threshold, size_t reset_samples) {
  integral *integ = assert_malloc(sizeof(integral));
  integ->max_reset_threshold = reset_threshold;
  integ->min_reset_samples = reset_samples;
  integ->sum = 0.0;
  integ->reset_samples = 0;
  return integ;
}

double integral_push(integral *integ, double val) {
  integ->sum += val;
  if (fabs(val) <= integ->max_reset_threshold) {
    integ->reset_samples ++;
    if (integ->reset_samples >= integ->min_reset_samples) {
      // reset integral
      integ->sum = 0;
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

