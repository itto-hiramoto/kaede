#include "io.h"
#include <stdio.h>

/*
 * These simply forward libc's standard streams.
 * Keeping them behind runtime boundary allows
 * future replacement (fd-based, WASM, Windows, etc).
 */

FILE *kaede_rt_stdin(void) { return stdin; }

FILE *kaede_rt_stdout(void) { return stdout; }

FILE *kaede_rt_stderr(void) { return stderr; }
