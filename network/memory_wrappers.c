
#include <stdlib.h>
#include <assert.h>

#include "memory_wrappers.h"

void *assert_calloc(size_t nmemb, size_t size) {
  void *result = calloc(nmemb, size);
  assert(result != NULL || nmemb == 0 || size == 0);
  return result;
}

void *assert_malloc(size_t size) {
  void *result = malloc(size);
  assert(result != NULL || size == 0);
  return result;
}

void *assert_realloc(void *ptr, size_t size) {
  void *result = realloc(ptr, size);
  assert(result != NULL || size == 0);
  return result;
}

