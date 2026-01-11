#pragma once
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

void *kaede_mem_alloc(size_t size);
void *kaede_mem_realloc(void *ptr, size_t new_size);

#ifdef __cplusplus
}
#endif
