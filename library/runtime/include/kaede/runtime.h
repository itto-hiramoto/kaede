#ifndef KAEDE_RUNTIME_H
#define KAEDE_RUNTIME_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef void (*TaskFn)(void *arg);

typedef enum KaedeIoWaitResult {
    KAEDE_IO_WAIT_FAILED = 0,
    KAEDE_IO_WAIT_READY = 1,
    KAEDE_IO_WAIT_CLOSED = 2,
} KaedeIoWaitResult;

void kaede_spawn_with_arg(TaskFn fn, void *arg, size_t arg_size);

static inline void kaede_spawn(void (*fn)(void)) {
    kaede_spawn_with_arg((TaskFn)fn, NULL, 0);
}

void kaede_yield(void);

void kaede_runtime_init(void);
void kaede_spawn_main(TaskFn fn, void *arg, size_t arg_size);
int kaede_runtime_run(void);
void kaede_runtime_shutdown(void);

void kaede_runtime_set_exit_code(int code);
KaedeIoWaitResult kaede_io_wait_readable(int fd);
KaedeIoWaitResult kaede_io_wait_writable(int fd);
void kaede_io_forget_fd(int fd);

#endif // KAEDE_RUNTIME_H
