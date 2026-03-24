#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Runtime stdio accessors
 *
 * NOTE:
 * - These expose libc FILE* intentionally.
 * - They are runtime-internal APIs.
 * - stdlib / user code must NOT depend on FILE* directly.
 */

/* Standard input */
FILE *kaede_rt_stdin(void);

/* Standard output */
FILE *kaede_rt_stdout(void);

/* Standard error */
FILE *kaede_rt_stderr(void);

/* fd / buffer / length — thin wrapper around POSIX write(2) */
int32_t kaede_io_write(int32_t fd, const void *buf, size_t len);

#ifdef __cplusplus
}
#endif
