#ifndef KAEDE_WORKER_H
#define KAEDE_WORKER_H

#include <kaede/runtime.h>
#include <stdbool.h>

bool worker_init(void);
void worker_deinit(void);
void *worker_loop(void *arg);
bool worker_spawn(TaskFn fn, void *arg, size_t arg_size, bool is_main);
void worker_yield(void);
void worker_reset_main_state(void);
int worker_wait_for_main(void);

#endif // KAEDE_WORKER_H
