
#ifndef _RINGBUFFER_H
#define _RINGBUFFER_H

typedef struct {
  size_t buflength;
  ssize_t membsize;
  off_t firstvalid;
  size_t validsize;
  void *buffer;
} ringbuffer;

ringbuffer *ringbuffer_allocate(size_t nmemb, ssize_t membsize);
int ringbuffer_push(ringbuffer *rb, void *val);
void *ringbuffer_getval(ringbuffer *rb, int index);
void ringbuffer_clear(ringbuffer *rb);

#endif // _RINGBUFFER_H

