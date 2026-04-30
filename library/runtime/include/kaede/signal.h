#ifndef KAEDE_SIGNAL_H
#define KAEDE_SIGNAL_H

struct Task;

// Per-thread pointer to the green-thread task currently executing on this
// worker, or NULL if the worker is in scheduler/system code. Mirrors
// `worker.current_task` in worker.c, but lives at file scope so the SIGSEGV
// handler can read it via async-signal-safe TLS load.
extern _Thread_local struct Task *kaede_current_task;

// Install a process-wide SIGSEGV/SIGBUS handler that turns green-thread
// stack overflows into a clear diagnostic on stderr before chaining to the
// default disposition. Call once during runtime startup, before any worker
// thread is created.
void kaede_install_crash_handler(void);

// Install a per-thread alternate signal stack so the crash handler can run
// even after the green-thread stack has been exhausted. Call from each
// worker thread before it starts executing tasks.
void kaede_install_worker_altstack(void);

#endif // KAEDE_SIGNAL_H
