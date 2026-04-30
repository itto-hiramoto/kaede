#ifndef KAEDE_WORKER_H
#define KAEDE_WORKER_H

#include <kaede/runtime.h>
#include <kaede/task.h>
#include <stdbool.h>
#include <stdint.h>

// The task currently executing on this OS thread, or NULL when the worker
// is in scheduler/system code.
//
// Why _Thread_local:
//   1. Each worker pthread runs at most one green-thread task at a time, so
//      "current task" is intrinsically per-OS-thread state — no shared
//      writes, no locking needed.
//   2. The SIGSEGV handler reads this from inside a signal context to
//      decide whether the fault is a guard-page hit, and a plain
//      _Thread_local load is one of the few async-signal-safe ways to
//      reach per-thread state (mutexes, malloc, and most pthread APIs are
//      not signal-safe).
//
// This is the single source of truth for the runtime's "current task".
// Do not introduce a parallel per-worker mirror.
extern _Thread_local struct Task *kaede_current_task;

bool worker_init(void);
void worker_deinit(void);
void worker_request_shutdown(void);
void *worker_loop(void *arg);
bool worker_spawn(TaskFn fn, void *arg, size_t arg_size, bool is_main);
KaedeIoWaitResult worker_park_current_on_io(int fd, uint32_t events);
void worker_forget_fd(int fd);
void worker_scheduler_lock(void);
void worker_scheduler_unlock(void);
bool worker_shutdown_requested_locked(void);
struct Task *worker_current_task(void);
bool worker_park_current_on_channel_locked(void *obj, uint32_t wait_kind,
                                           void *value_slot);
bool worker_wake_task_locked(struct Task *task, bool success);
void worker_yield(void);
void worker_reset_main_state(void);
int worker_wait_for_main(void);

#endif // KAEDE_WORKER_H
