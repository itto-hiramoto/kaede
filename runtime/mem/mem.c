#include "mem.h"
#include <gc.h>

void *kaede_mem_alloc(size_t size) { return GC_malloc(size); }

void *kaede_mem_realloc(void *ptr, size_t new_size) {
  return GC_realloc(ptr, new_size);
}
