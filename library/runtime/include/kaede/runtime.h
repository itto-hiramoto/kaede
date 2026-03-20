#ifndef KAEDE_RUNTIME_H
#define KAEDE_RUNTIME_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef void (*TaskFn)(void *arg);

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
bool kaede_io_wait_readable(int fd);
bool kaede_io_wait_writable(int fd);
void kaede_io_forget_fd(int fd);

#endif // KAEDE_RUNTIME_H
