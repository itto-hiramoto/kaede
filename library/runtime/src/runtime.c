#include <gc/gc.h>
#include <kaede/runtime.h>
#include <kaede/task.h>
#include <kaede/worker.h>
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int runtime_threads = 0;

void kaede_runtime_init(void) {
    worker_reset_main_state();
    if (signal(SIGPIPE, SIG_IGN) == SIG_ERR) {
        fprintf(stderr, "Failed to ignore SIGPIPE: %s\n", strerror(errno));
        abort();
    }
    if (!worker_init()) {
        fprintf(stderr, "Failed to initialize worker/runtime\n");
        abort();
    }
    GC_allow_register_threads();
}

void kaede_spawn_main(TaskFn fn, void *arg, size_t arg_size) {
    if (!worker_spawn(fn, arg, arg_size, true)) {
        fprintf(stderr, "Failed to spawn main task\n");
        abort();
    }
}

int kaede_runtime_run(void) {
    runtime_threads = (int)sysconf(_SC_NPROCESSORS_ONLN);

    if (runtime_threads <= 0) {
        fprintf(stderr, "Failed to get number of threads: %s\n",
                strerror(errno));
        return 1;
    }

    pthread_t *threads = calloc(runtime_threads, sizeof(pthread_t));
    if (!threads) {
        fprintf(stderr, "Failed to allocate worker threads\n");
        return 1;
    }

    for (int worker_id = 0; worker_id < runtime_threads; ++worker_id) {
        if (pthread_create(&threads[worker_id], NULL, worker_loop,
                           (void *)(intptr_t)worker_id) != 0) {
            fprintf(stderr, "Failed to create worker thread\n");
            free(threads);
            return 1;
        }
        pthread_detach(threads[worker_id]);
    }

    free(threads);
    return worker_wait_for_main();
}

void kaede_runtime_shutdown(void) {
    (void)runtime_threads;
}

bool kaede_io_wait_readable(int fd) {
    return worker_park_current_on_io(fd, KAEDE_IO_EVENT_READ);
}

bool kaede_io_wait_writable(int fd) {
    return worker_park_current_on_io(fd, KAEDE_IO_EVENT_WRITE);
}

void kaede_io_forget_fd(int fd) {
    worker_forget_fd(fd);
}
