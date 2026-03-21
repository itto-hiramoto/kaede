#ifndef KAEDE_WORKER_H
#define KAEDE_WORKER_H

#include <kaede/runtime.h>
#include <stdbool.h>
#include <stdint.h>

bool worker_init(void);
void worker_deinit(void);
void worker_request_shutdown(void);
void *worker_loop(void *arg);
bool worker_spawn(TaskFn fn, void *arg, size_t arg_size, bool is_main);
KaedeIoWaitResult worker_park_current_on_io(int fd, uint32_t events);
void worker_forget_fd(int fd);
void worker_yield(void);
void worker_reset_main_state(void);
int worker_wait_for_main(void);

#endif // KAEDE_WORKER_H
