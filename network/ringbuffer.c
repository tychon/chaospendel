
#define _GNU_SOURCE
#include <stdlib.h>
#include <string.h>

#include "memory_wrappers.h"

#include "ringbuffer.h"

ringbuffer *ringbuffer_allocate(size_t nmemb, ssize_t membsize) {
  ringbuffer *rb = assert_malloc(sizeof(ringbuffer));
  rb->buflength = nmemb * membsize;
  rb->membsize = membsize;
  rb->firstvalid = 0;
  rb->validsize = 0;
  rb->buffer = assert_malloc(nmemb * membsize);
  return rb;
}

int ringbuffer_push(ringbuffer *rb, void *val) {
  int nextpos = (rb->firstvalid + rb->validsize) % rb->buflength;
  memcpy(rb->buffer + nextpos, val, rb->membsize);
  if (rb->validsize == rb->buflength) {
    rb->firstvalid += rb->membsize;
    if (rb->firstvalid == rb->buflength)
      rb->firstvalid = 0;
  } else {
    rb->validsize += rb->membsize;
  }
  
  return rb->validsize == rb->buflength;
}

void *ringbuffer_getval(ringbuffer *rb, int index) {
  if (index < 0 || index * rb->membsize >= rb->buflength) return NULL;
  return rb->buffer + (rb->firstvalid + index * rb->membsize) % rb->buflength;
}

int ringbuffer_getlength(ringbuffer *rb) {
  return rb->validsize / rb->membsize;
}

void ringbuffer_clear(ringbuffer *rb) {
  rb->firstvalid = 0;
  rb->validsize = 0;
}

