#include <kaede/task.h>
#include <kaede/worker.h>
#include <gc/gc.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct TaskQueue tasks;
static pthread_mutex_t tasks_mutex;
static pthread_once_t runtime_init_once = PTHREAD_ONCE_INIT;
static bool runtime_init_ok = false;
static pthread_mutex_t main_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t main_cond = PTHREAD_COND_INITIALIZER;
static bool main_finished = false;
static bool main_spawned = false;
static int main_exit_code = 0;

struct Worker {
    struct Task *current_task;
    struct Context context;
    void *gc_thread_handle;
    struct GC_stack_base gc_stack_base;
};

_Thread_local struct Worker worker;

static void runtime_init_impl(void) {
    if (pthread_mutex_init(&tasks_mutex, NULL) != 0) {
        runtime_init_ok = false;
        return;
    }

    if (!task_queue_init(&tasks, 1024)) {
        pthread_mutex_destroy(&tasks_mutex);
        runtime_init_ok = false;
        return;
    }

    runtime_init_ok = true;
}

static void task_finished(void) {
    if (!worker.current_task) {
        return;
    }

    worker.current_task->finished = true;
    if (worker.current_task->is_main) {
        pthread_mutex_lock(&main_mutex);
        main_finished = true;
        pthread_cond_broadcast(&main_cond);
        pthread_mutex_unlock(&main_mutex);
    }

    // Switch back to scheduler
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

void worker_deinit(void) {
    if (!runtime_init_ok) {
        return;
    }
    pthread_mutex_lock(&tasks_mutex);
    task_queue_deinit(&tasks);
    pthread_mutex_unlock(&tasks_mutex);
    pthread_mutex_destroy(&tasks_mutex);
}

static void worker_loop_impl(int worker_id) {
    struct GC_stack_base sb;
    if (GC_get_stack_base(&sb) == GC_SUCCESS && !GC_thread_is_registered()) {
        int reg_result = GC_register_my_thread(&sb);
        if (reg_result != GC_SUCCESS && reg_result != GC_DUPLICATE) {
            fprintf(stderr, "Failed to register GC thread\n");
            abort();
        }
    }
    worker.gc_thread_handle = GC_get_my_stackbottom(&worker.gc_stack_base);

    for (;;) {
        struct Task task;
        pthread_mutex_lock(&tasks_mutex);
        const bool ok = task_queue_pop(&tasks, &task);
        pthread_mutex_unlock(&tasks_mutex);
        if (!ok) {
            break;
        }

        // Store pointer to local task variable
        // This is safe because context_switch returns to the same stack frame
        worker.current_task = &task;

        if (worker.gc_thread_handle) {
            struct GC_stack_base task_sb;
            task_sb.mem_base = (void *)((uint8_t *)task.stack + STACK_SIZE);
            GC_set_stackbottom(worker.gc_thread_handle, &task_sb);
        }

        context_switch(&worker.context, &task.context);

        if (worker.gc_thread_handle) {
            GC_set_stackbottom(worker.gc_thread_handle, &worker.gc_stack_base);
        }

        // After context switch returns, task is still valid in this stack frame
        if (!task.finished) {
            // Push the task back to the queue
            pthread_mutex_lock(&tasks_mutex);
            (void)task_queue_push(&tasks, &task);
            pthread_mutex_unlock(&tasks_mutex);
        } else {
            task_cleanup(&task);
        }
    }

    printf("Worker %d exiting\n", worker_id);
}

void *worker_loop(void *arg) {
    const int worker_id = (int)(intptr_t)arg;
    worker_loop_impl(worker_id);
    return NULL;
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

    uint8_t *stack = create_stack();
    if (!stack) {
        fprintf(stderr, "Failed to create stack for task\n");
        return false;
    }

    // Stack top is just below the guard page (stack grows downward)
    // Guard page starts at stack + STACK_SIZE
    uint64_t stack_top = (uint64_t)stack + STACK_SIZE;
    void *arg_on_stack = NULL;

    if (arg_size > 0) {
        stack_top -= arg_size;
        stack_top &= ~0x0F;
        arg_on_stack = (void *)stack_top;
        memcpy(arg_on_stack, arg, arg_size);
    }

    struct Task task;
    task.fn = fn;
    task.arg = arg_on_stack;
    task.arg_size = arg_size;
    task.stack = stack;
    task.is_main = is_main;
    create_context(&task.context, fn, arg_on_stack, stack_top);
    task.finished = false;

    if (is_main) {
        pthread_mutex_lock(&main_mutex);
        if (main_spawned) {
            pthread_mutex_unlock(&main_mutex);
            fprintf(stderr, "Main task already spawned\n");
            task_cleanup(&task);
            return false;
        }
        main_spawned = true;
        pthread_mutex_unlock(&main_mutex);
    }

    pthread_mutex_lock(&tasks_mutex);
    const bool ok = task_queue_push(&tasks, &task);
    pthread_mutex_unlock(&tasks_mutex);
    if (!ok) {
        fprintf(stderr, "Failed to push task to queue\n");
        task_cleanup(&task);
        return false;
    }

    return true;
}

void worker_reset_main_state(void) {
    pthread_mutex_lock(&main_mutex);
    main_finished = false;
    main_spawned = false;
    main_exit_code = 0;
    pthread_mutex_unlock(&main_mutex);
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
