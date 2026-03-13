#pragma once
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

int32_t kaede_http_ws_accept(const char *key, size_t key_len, char *out,
                             size_t out_len);

#ifdef __cplusplus
}
#endif
