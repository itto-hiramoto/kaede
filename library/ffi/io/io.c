#include "io.h"
#include <stdio.h>
#include <unistd.h>

/*
 * These simply forward libc's standard streams.
 * Keeping them behind runtime boundary allows
 * future replacement (fd-based, WASM, Windows, etc).
 */

FILE *kaede_rt_stdin(void) { return stdin; }

FILE *kaede_rt_stdout(void) { return stdout; }

FILE *kaede_rt_stderr(void) { return stderr; }

int32_t kaede_io_write(int32_t fd, const unsigned char *buf, size_t len) {
    return (int32_t)write((int)fd, buf, len);
}
