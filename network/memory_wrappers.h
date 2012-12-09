
/**
 * These functions are to be used instead of normal calloc(), malloc() and
 * realloc() everywhere in the code. When the application is out of memory,
 * they don't return NULL or so - they crash. We don't want inconsistent
 * state because of an OOM, right? And we don't want to assert() after each
 * call either.
 *
 * NOTE: When you call one of them with size=0 or nmemb=0, they'll
 *       return NULL without crashing.
 */

#ifndef _MEMORY_WRAPPERS_H
#define _MEMORY_WRAPPERS_H

#include <stdlib.h>

void *assert_calloc(size_t nmemb, size_t size);
void *assert_malloc(size_t size);
void *assert_realloc(void *ptr, size_t size);

#endif // MEMORY_WRAPPERS_H

