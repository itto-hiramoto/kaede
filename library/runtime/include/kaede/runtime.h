#ifndef KAEDE_RUNTIME_H
#define KAEDE_RUNTIME_H

#include <stddef.h>

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

#endif // KAEDE_RUNTIME_H
