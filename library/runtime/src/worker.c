#include <kaede/poller.h>
#include <kaede/task.h>
#include <kaede/worker.h>
#include <gc/gc.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INITIAL_RUN_QUEUE_CAPACITY 1024
#define INITIAL_IO_WAIT_CAPACITY 64
#define MAX_POLLER_EVENTS 64

static struct TaskQueue runnable_tasks;
static pthread_mutex_t scheduler_mutex;
static pthread_cond_t scheduler_cond = PTHREAD_COND_INITIALIZER;
static pthread_once_t runtime_init_once = PTHREAD_ONCE_INIT;
static bool runtime_init_ok = false;
static bool shutdown_requested = false;
static bool poller_waiter_active = false;
static pthread_mutex_t main_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t main_cond = PTHREAD_COND_INITIALIZER;
static bool main_finished = false;
static bool scheduler_main_finished = false;
static bool main_spawned = false;
static int main_exit_code = 0;

struct Worker {
    struct Task *current_task;
    struct Context context;
    void *gc_thread_handle;
    struct GC_stack_base gc_stack_base;
};

struct IoWaitEntry {
    int fd;
    struct Task *read_task;
    struct Task *write_task;
    bool in_use;
};

struct IoWaitTable {
    struct IoWaitEntry *entries;
    size_t capacity;
    size_t count;
};

static struct IoWaitTable io_waits;

_Thread_local struct Worker worker;

static void fail_runtime(const char *message) {
    fprintf(stderr, "%s\n", message);
    abort();
}

static void cleanup_task(struct Task *task) {
    if (!task) {
        return;
    }

    task->state = TASK_FINISHED;
    task_cleanup(task);
    free(task);
}

static bool update_poller_interest_locked(int fd, uint32_t old_events,
                                          uint32_t new_events);

static uint32_t io_wait_entry_events(const struct IoWaitEntry *entry) {
    uint32_t events = KAEDE_IO_EVENT_NONE;
    if (entry->read_task) {
        events |= KAEDE_IO_EVENT_READ;
    }
    if (entry->write_task) {
        events |= KAEDE_IO_EVENT_WRITE;
    }
    return events;
}

static void io_wait_entry_reset(struct IoWaitEntry *entry) {
    entry->fd = -1;
    entry->read_task = NULL;
    entry->write_task = NULL;
    entry->in_use = false;
}

static bool io_wait_table_init(struct IoWaitTable *table, size_t capacity) {
    table->entries = calloc(capacity, sizeof(struct IoWaitEntry));
    if (!table->entries) {
        table->capacity = 0;
        table->count = 0;
        return false;
    }

    table->capacity = capacity;
    table->count = 0;
    for (size_t i = 0; i < capacity; ++i) {
        io_wait_entry_reset(&table->entries[i]);
    }
    return true;
}

static void io_wait_table_deinit(struct IoWaitTable *table) {
    if (!table) {
        return;
    }

    free(table->entries);
    table->entries = NULL;
    table->capacity = 0;
    table->count = 0;
}

static bool io_wait_table_grow(struct IoWaitTable *table) {
    const size_t new_capacity = table->capacity ? table->capacity * 2 : 1;
    struct IoWaitEntry *entries =
        calloc(new_capacity, sizeof(struct IoWaitEntry));
    if (!entries) {
        return false;
    }

    for (size_t i = 0; i < new_capacity; ++i) {
        io_wait_entry_reset(&entries[i]);
    }

    size_t dst = 0;
    for (size_t i = 0; i < table->capacity; ++i) {
        if (!table->entries[i].in_use) {
            continue;
        }
        entries[dst++] = table->entries[i];
    }

    free(table->entries);
    table->entries = entries;
    table->capacity = new_capacity;
    table->count = dst;
    return true;
}

static struct IoWaitEntry *io_wait_table_find(struct IoWaitTable *table, int fd) {
    for (size_t i = 0; i < table->capacity; ++i) {
        if (table->entries[i].in_use && table->entries[i].fd == fd) {
            return &table->entries[i];
        }
    }
    return NULL;
}

static struct IoWaitEntry *io_wait_table_get_or_insert(struct IoWaitTable *table,
                                                       int fd) {
    struct IoWaitEntry *entry = io_wait_table_find(table, fd);
    if (entry) {
        return entry;
    }

    if (table->count == table->capacity && !io_wait_table_grow(table)) {
        return NULL;
    }

    for (size_t i = 0; i < table->capacity; ++i) {
        if (table->entries[i].in_use) {
            continue;
        }

        entry = &table->entries[i];
        entry->fd = fd;
        entry->read_task = NULL;
        entry->write_task = NULL;
        entry->in_use = true;
        table->count++;
        return entry;
    }

    return NULL;
}

static void io_wait_table_remove_entry(struct IoWaitTable *table,
                                       struct IoWaitEntry *entry) {
    if (!entry || !entry->in_use) {
        return;
    }

    io_wait_entry_reset(entry);
    if (table->count > 0) {
        table->count--;
    }
}

static void io_wait_entry_clear_task(struct IoWaitEntry *entry,
                                     struct Task *task) {
    if (entry->read_task == task) {
        entry->read_task = NULL;
    }
    if (entry->write_task == task) {
        entry->write_task = NULL;
    }
}

static void remove_task_from_wait_table_locked(struct Task *task) {
    if (!task || task->waiting_fd < 0) {
        return;
    }

    struct IoWaitEntry *entry = io_wait_table_find(&io_waits, task->waiting_fd);
    if (!entry) {
        task->waiting_fd = -1;
        task->waiting_events = KAEDE_IO_EVENT_NONE;
        return;
    }

    const uint32_t old_events = io_wait_entry_events(entry);
    io_wait_entry_clear_task(entry, task);
    const uint32_t new_events = io_wait_entry_events(entry);
    if (!update_poller_interest_locked(task->waiting_fd, old_events, new_events)) {
        fail_runtime("Failed to update poller interest while removing task");
    }
    if (new_events == KAEDE_IO_EVENT_NONE) {
        io_wait_table_remove_entry(&io_waits, entry);
    }

    task->waiting_fd = -1;
    task->waiting_events = KAEDE_IO_EVENT_NONE;
}

// Caller must hold scheduler_mutex while reconciling wait-table state with the
// poller registration for this fd.
static bool update_poller_interest_locked(int fd, uint32_t old_events,
                                          uint32_t new_events) {
    if (old_events == new_events) {
        return true;
    }
    if (new_events == KAEDE_IO_EVENT_NONE) {
        return kaede_poller_del(fd);
    }
    if (old_events == KAEDE_IO_EVENT_NONE) {
        return kaede_poller_add(fd, new_events);
    }
    return kaede_poller_mod(fd, new_events);
}

// Caller must hold scheduler_mutex before moving a task back onto the runnable
// queue.
static bool enqueue_runnable_task_locked(struct Task *task) {
    task->state = TASK_RUNNABLE;
    task->waiting_fd = -1;
    task->waiting_events = KAEDE_IO_EVENT_NONE;
    return task_queue_push(&runnable_tasks, task);
}

// Caller must hold scheduler_mutex while waking tasks and updating the shared
// wait-table / poller state for the fd.
static size_t wake_waiting_tasks_locked(int fd, uint32_t ready_events) {
    struct IoWaitEntry *entry = io_wait_table_find(&io_waits, fd);
    if (!entry) {
        return 0;
    }

    struct Task *tasks_to_wake[2] = {NULL, NULL};
    size_t wake_count = 0;
    const uint32_t old_events = io_wait_entry_events(entry);

    if ((ready_events & KAEDE_IO_EVENT_READ) != 0 && entry->read_task) {
        struct Task *task = entry->read_task;
        io_wait_entry_clear_task(entry, task);
        tasks_to_wake[wake_count++] = task;
    }

    if ((ready_events & KAEDE_IO_EVENT_WRITE) != 0 && entry->write_task) {
        struct Task *task = entry->write_task;
        io_wait_entry_clear_task(entry, task);
        if (wake_count == 0 || tasks_to_wake[0] != task) {
            tasks_to_wake[wake_count++] = task;
        }
    }

    const uint32_t new_events = io_wait_entry_events(entry);
    if (!update_poller_interest_locked(fd, old_events, new_events)) {
        fail_runtime("Failed to update poller interest while waking task");
    }
    if (new_events == KAEDE_IO_EVENT_NONE) {
        io_wait_table_remove_entry(&io_waits, entry);
    }

    for (size_t i = 0; i < wake_count; ++i) {
        if (!enqueue_runnable_task_locked(tasks_to_wake[i])) {
            fail_runtime("Failed to enqueue runnable task");
        }
    }

    if (wake_count > 0) {
        pthread_cond_broadcast(&scheduler_cond);
    }

    return wake_count;
}

void worker_forget_fd(int fd) {
    if (fd < 0) {
        return;
    }

    pthread_mutex_lock(&scheduler_mutex);
    const bool should_wake_poller =
        wake_waiting_tasks_locked(fd, KAEDE_IO_EVENT_READ | KAEDE_IO_EVENT_WRITE) > 0 &&
        poller_waiter_active;
    pthread_mutex_unlock(&scheduler_mutex);
    if (should_wake_poller) {
        (void)kaede_poller_wake();
    }
}

static void runtime_init_impl(void) {
    if (pthread_mutex_init(&scheduler_mutex, NULL) != 0) {
        runtime_init_ok = false;
        return;
    }

    if (!task_queue_init(&runnable_tasks, INITIAL_RUN_QUEUE_CAPACITY)) {
        pthread_mutex_destroy(&scheduler_mutex);
        runtime_init_ok = false;
        return;
    }

    if (!io_wait_table_init(&io_waits, INITIAL_IO_WAIT_CAPACITY)) {
        task_queue_deinit(&runnable_tasks);
        pthread_mutex_destroy(&scheduler_mutex);
        runtime_init_ok = false;
        return;
    }

    if (!kaede_poller_init()) {
        io_wait_table_deinit(&io_waits);
        task_queue_deinit(&runnable_tasks);
        pthread_mutex_destroy(&scheduler_mutex);
        runtime_init_ok = false;
        return;
    }

    runtime_init_ok = true;
}

static void task_finished(void) {
    if (!worker.current_task) {
        return;
    }

    worker.current_task->state = TASK_FINISHED;
    if (worker.current_task->is_main) {
        pthread_mutex_lock(&main_mutex);
        main_finished = true;
        pthread_cond_broadcast(&main_cond);
        pthread_mutex_unlock(&main_mutex);

        pthread_mutex_lock(&scheduler_mutex);
        scheduler_main_finished = true;
        pthread_cond_broadcast(&scheduler_cond);
        pthread_mutex_unlock(&scheduler_mutex);
        (void)kaede_poller_wake();
    }

    context_switch(&worker.current_task->context, &worker.context);
}

void task_entrypoint(void) {
    TaskFn fn = get_task_body();
    void *arg = get_task_arg();
    fn(arg);
    task_finished();
}

bool worker_init(void) {
    pthread_once(&runtime_init_once, runtime_init_impl);
    return runtime_init_ok;
}

void worker_request_shutdown(void) {
    if (!runtime_init_ok) {
        return;
    }

    pthread_mutex_lock(&scheduler_mutex);
    shutdown_requested = true;
    pthread_cond_broadcast(&scheduler_cond);
    pthread_mutex_unlock(&scheduler_mutex);
    (void)kaede_poller_wake();
}

void worker_deinit(void) {
    if (!runtime_init_ok) {
        return;
    }

    pthread_mutex_lock(&scheduler_mutex);

    struct Task *task = NULL;
    while (task_queue_pop(&runnable_tasks, &task)) {
        cleanup_task(task);
    }

    for (size_t i = 0; i < io_waits.capacity; ++i) {
        struct IoWaitEntry *entry = &io_waits.entries[i];
        if (!entry->in_use) {
            continue;
        }

        struct Task *read_task = entry->read_task;
        struct Task *write_task = entry->write_task;
        io_wait_table_remove_entry(&io_waits, entry);
        cleanup_task(read_task);
        if (write_task != read_task) {
            cleanup_task(write_task);
        }
    }

    pthread_mutex_unlock(&scheduler_mutex);

    kaede_poller_deinit();
    io_wait_table_deinit(&io_waits);
    task_queue_deinit(&runnable_tasks);
    pthread_cond_destroy(&scheduler_cond);
    pthread_mutex_destroy(&scheduler_mutex);
    runtime_init_ok = false;
}

static void worker_loop_impl(int worker_id) {
    struct GC_stack_base sb;
    if (GC_get_stack_base(&sb) == GC_SUCCESS && !GC_thread_is_registered()) {
        int reg_result = GC_register_my_thread(&sb);
        if (reg_result != GC_SUCCESS && reg_result != GC_DUPLICATE) {
            fail_runtime("Failed to register GC thread");
        }
    }
    worker.gc_thread_handle = GC_get_my_stackbottom(&worker.gc_stack_base);

    for (;;) {
        struct Task *task = NULL;

        pthread_mutex_lock(&scheduler_mutex);
        for (;;) {
            if (shutdown_requested || scheduler_main_finished) {
                pthread_mutex_unlock(&scheduler_mutex);
                return;
            }

            if (task_queue_pop(&runnable_tasks, &task)) {
                break;
            }

            if (!poller_waiter_active) {
                struct KaedePollEvent events[MAX_POLLER_EVENTS];

                poller_waiter_active = true;
                pthread_mutex_unlock(&scheduler_mutex);

                const int ready = kaede_poller_wait(events, MAX_POLLER_EVENTS, -1);

                pthread_mutex_lock(&scheduler_mutex);
                poller_waiter_active = false;
                pthread_cond_broadcast(&scheduler_cond);

                if (shutdown_requested || scheduler_main_finished) {
                    pthread_mutex_unlock(&scheduler_mutex);
                    return;
                }

                if (ready < 0) {
                    pthread_mutex_unlock(&scheduler_mutex);
                    fail_runtime("Poller wait failed");
                }

                for (int i = 0; i < ready; ++i) {
                    (void)wake_waiting_tasks_locked(events[i].fd, events[i].events);
                }
                continue;
            }

            pthread_cond_wait(&scheduler_cond, &scheduler_mutex);
        }
        pthread_mutex_unlock(&scheduler_mutex);

        worker.current_task = task;

        if (worker.gc_thread_handle) {
            struct GC_stack_base task_sb;
            task_sb.mem_base = (void *)((uint8_t *)task->stack + STACK_SIZE);
            GC_set_stackbottom(worker.gc_thread_handle, &task_sb);
        }

        context_switch(&worker.context, &task->context);

        if (worker.gc_thread_handle) {
            GC_set_stackbottom(worker.gc_thread_handle, &worker.gc_stack_base);
        }

        worker.current_task = NULL;

        pthread_mutex_lock(&scheduler_mutex);
        const bool should_shutdown = shutdown_requested;
        if (should_shutdown && task->state == TASK_WAITING_IO) {
            remove_task_from_wait_table_locked(task);
        }
        pthread_mutex_unlock(&scheduler_mutex);

        if (should_shutdown) {
            cleanup_task(task);
            continue;
        }

        switch (task->state) {
        case TASK_RUNNABLE:
            pthread_mutex_lock(&scheduler_mutex);
            if (!task_queue_push(&runnable_tasks, task)) {
                pthread_mutex_unlock(&scheduler_mutex);
                fail_runtime("Failed to requeue task");
            }
            pthread_cond_signal(&scheduler_cond);
            pthread_mutex_unlock(&scheduler_mutex);
            break;
        case TASK_WAITING_IO:
            break;
        case TASK_FINISHED:
            task_cleanup(task);
            free(task);
            break;
        default:
            fail_runtime("Unknown task state");
        }
    }

    (void)worker_id;
}

void *worker_loop(void *arg) {
    const int worker_id = (int)(intptr_t)arg;
    worker_loop_impl(worker_id);
    return NULL;
}

bool worker_park_current_on_io(int fd, uint32_t events) {
    if (!worker.current_task || events == KAEDE_IO_EVENT_NONE) {
        return false;
    }

    pthread_mutex_lock(&scheduler_mutex);

    if (shutdown_requested) {
        pthread_mutex_unlock(&scheduler_mutex);
        return false;
    }

    struct IoWaitEntry *entry = io_wait_table_get_or_insert(&io_waits, fd);
    if (!entry) {
        pthread_mutex_unlock(&scheduler_mutex);
        return false;
    }

    if (((events & KAEDE_IO_EVENT_READ) != 0 && entry->read_task) ||
        ((events & KAEDE_IO_EVENT_WRITE) != 0 && entry->write_task)) {
        pthread_mutex_unlock(&scheduler_mutex);
        return false;
    }

    const uint32_t old_events = io_wait_entry_events(entry);
    const uint32_t new_events = old_events | events;
    if (!update_poller_interest_locked(fd, old_events, new_events)) {
        if (old_events == KAEDE_IO_EVENT_NONE &&
            io_wait_entry_events(entry) == KAEDE_IO_EVENT_NONE) {
            io_wait_table_remove_entry(&io_waits, entry);
        }
        pthread_mutex_unlock(&scheduler_mutex);
        return false;
    }

    if ((events & KAEDE_IO_EVENT_READ) != 0) {
        entry->read_task = worker.current_task;
    }
    if ((events & KAEDE_IO_EVENT_WRITE) != 0) {
        entry->write_task = worker.current_task;
    }

    worker.current_task->state = TASK_WAITING_IO;
    worker.current_task->waiting_fd = fd;
    worker.current_task->waiting_events = events;

    pthread_mutex_unlock(&scheduler_mutex);

    if (worker.gc_thread_handle) {
        GC_set_stackbottom(worker.gc_thread_handle, &worker.gc_stack_base);
    }
    context_switch(&worker.current_task->context, &worker.context);
    return true;
}

void worker_yield(void) {
    if (worker.gc_thread_handle) {
        GC_set_stackbottom(worker.gc_thread_handle, &worker.gc_stack_base);
    }
    context_switch(&worker.current_task->context, &worker.context);
}

bool worker_spawn(TaskFn fn, void *arg, size_t arg_size, bool is_main) {
    if (!worker_init()) {
        fprintf(stderr, "Failed to initialize worker/runtime\n");
        return false;
    }

    struct Task *task = calloc(1, sizeof(struct Task));
    if (!task) {
        fprintf(stderr, "Failed to allocate task\n");
        return false;
    }

    uint8_t *stack = create_stack();
    if (!stack) {
        fprintf(stderr, "Failed to create stack for task\n");
        free(task);
        return false;
    }

    uint64_t stack_top = (uint64_t)stack + STACK_SIZE;
    void *arg_on_stack = NULL;

    if (arg_size > 0) {
        stack_top -= arg_size;
        stack_top &= ~0x0F;
        arg_on_stack = (void *)stack_top;
        memcpy(arg_on_stack, arg, arg_size);
    }

    task->fn = fn;
    task->arg = arg_on_stack;
    task->arg_size = arg_size;
    task->stack = stack;
    task->finished = false;
    task->state = TASK_RUNNABLE;
    task->waiting_fd = -1;
    task->waiting_events = KAEDE_IO_EVENT_NONE;
    task->is_main = is_main;
    create_context(&task->context, fn, arg_on_stack, stack_top);
    task_register_stack_roots(task);

    if (is_main) {
        pthread_mutex_lock(&main_mutex);
        if (main_spawned) {
            pthread_mutex_unlock(&main_mutex);
            fprintf(stderr, "Main task already spawned\n");
            task_cleanup(task);
            free(task);
            return false;
        }
        main_spawned = true;
        pthread_mutex_unlock(&main_mutex);
    }

    pthread_mutex_lock(&scheduler_mutex);
    if (shutdown_requested) {
        pthread_mutex_unlock(&scheduler_mutex);
        if (is_main) {
            pthread_mutex_lock(&main_mutex);
            main_spawned = false;
            pthread_mutex_unlock(&main_mutex);
        }
        task_cleanup(task);
        free(task);
        return false;
    }
    const bool should_wake_poller = poller_waiter_active;
    const bool ok = task_queue_push(&runnable_tasks, task);
    if (ok) {
        pthread_cond_signal(&scheduler_cond);
    }
    pthread_mutex_unlock(&scheduler_mutex);
    if (!ok) {
        fprintf(stderr, "Failed to push task to queue\n");
        if (is_main) {
            pthread_mutex_lock(&main_mutex);
            main_spawned = false;
            pthread_mutex_unlock(&main_mutex);
        }
        task_cleanup(task);
        free(task);
        return false;
    }

    if (should_wake_poller) {
        (void)kaede_poller_wake();
    }
    return true;
}

void worker_reset_main_state(void) {
    pthread_mutex_lock(&main_mutex);
    main_finished = false;
    main_spawned = false;
    main_exit_code = 0;
    pthread_mutex_unlock(&main_mutex);

    if (runtime_init_ok) {
        pthread_mutex_lock(&scheduler_mutex);
        shutdown_requested = false;
        scheduler_main_finished = false;
        poller_waiter_active = false;
        pthread_mutex_unlock(&scheduler_mutex);
    } else {
        shutdown_requested = false;
        scheduler_main_finished = false;
        poller_waiter_active = false;
    }
}

int worker_wait_for_main(void) {
    pthread_mutex_lock(&main_mutex);
    while (!main_finished) {
        pthread_cond_wait(&main_cond, &main_mutex);
    }
    int exit_code = main_exit_code;
    pthread_mutex_unlock(&main_mutex);
    return exit_code;
}

void kaede_runtime_set_exit_code(int code) {
    pthread_mutex_lock(&main_mutex);
    main_exit_code = code;
    pthread_mutex_unlock(&main_mutex);
}

void kaede_spawn_with_arg(TaskFn fn, void *arg, size_t arg_size) {
    if (!worker_spawn(fn, arg, arg_size, false)) {
        fprintf(stderr, "Failed to spawn task\n");
        abort();
    }
}

void kaede_yield(void) {
    worker_yield();
}
