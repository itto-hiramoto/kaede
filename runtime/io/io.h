#pragma once

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

#ifdef __cplusplus
}
#endif
