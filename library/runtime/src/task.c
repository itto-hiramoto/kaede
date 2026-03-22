#include <errno.h>
#include <gc/gc.h>
#include <kaede/task.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

static size_t get_page_size(void) {
    long page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0) {
        return 4096;
    }
    return (size_t)page_size;
}

// Round up `value` to the next multiple of `alignment` (power-of-two expected).
static size_t align_up(size_t value, size_t alignment) {
    return (value + alignment - 1) & ~(alignment - 1);
}

static size_t get_usable_stack_size(void) {
    return align_up(STACK_SIZE, get_page_size());
}

static size_t get_stack_mapping_size(void) {
    return get_page_size() + get_usable_stack_size();
}

static void get_stack_root_range(uint8_t *stack, void **root_base,
                                 void **root_limit) {
    const size_t usable_size = get_usable_stack_size();
    *root_base = (void *)stack;
    *root_limit = (void *)(stack + usable_size);
}

static inline size_t next_index(const struct TaskQueue *queue, size_t index) {
    return (index + 1) % queue->capacity;
}

static bool task_queue_grow(struct TaskQueue *queue) {
    size_t new_capacity = queue->capacity ? queue->capacity * 2 : 1;
    struct Task **new_tasks = calloc(new_capacity, sizeof(struct Task *));
    if (!new_tasks) {
        return false;
    }

    // Copy existing tasks in order into the new buffer
    for (size_t i = 0; i < queue->count; ++i) {
        size_t idx = (queue->head + i) % queue->capacity;
        new_tasks[i] = queue->tasks[idx];
    }

    free(queue->tasks);
    queue->tasks = new_tasks;
    queue->capacity = new_capacity;
    queue->head = 0;
    queue->tail = queue->count;
    return true;
}

bool task_queue_init(struct TaskQueue *queue, size_t capacity) {
    if (!queue || capacity == 0) {
        return false;
    }

    queue->tasks = calloc(capacity, sizeof(struct Task *));
    if (!queue->tasks) {
        queue->head = 0;
        queue->tail = 0;
        queue->capacity = 0;
        queue->count = 0;
        return false;
    }

    queue->capacity = capacity;
    queue->head = 0;
    queue->tail = 0;
    queue->count = 0;
    return true;
}

void task_queue_deinit(struct TaskQueue *queue) {
    if (!queue) {
        return;
    }

    free(queue->tasks);
    queue->tasks = NULL;
    queue->head = 0;
    queue->tail = 0;
    queue->capacity = 0;
    queue->count = 0;
}

bool task_queue_is_empty(const struct TaskQueue *queue) {
    if (!queue || queue->capacity == 0) {
        return true;
    }

    return queue->count == 0;
}

bool task_queue_is_full(const struct TaskQueue *queue) {
    if (!queue || queue->capacity == 0) {
        return false;
    }

    return queue->count == queue->capacity;
}

bool task_queue_push(struct TaskQueue *queue, struct Task *task) {
    if (!queue || !task || queue->capacity == 0) {
        return false;
    }

    if (task->scheduler.queued) {
        return true;
    }

    if (task_queue_is_full(queue)) {
        if (!task_queue_grow(queue)) {
            return false;
        }
    }

    queue->tasks[queue->tail] = task;
    queue->tail = next_index(queue, queue->tail);
    queue->count++;
    task->scheduler.queued = true;
    return true;
}

bool task_queue_pop(struct TaskQueue *queue, struct Task **task) {
    if (!queue || !task || task_queue_is_empty(queue)) {
        return false;
    }

    *task = queue->tasks[queue->head];
    queue->head = next_index(queue, queue->head);
    queue->count--;
    (*task)->scheduler.queued = false;
    return true;
}

uint8_t *create_stack(void) {
    const size_t page_size = get_page_size();
    const size_t mapping_size = get_stack_mapping_size();

    void *mapping = mmap(NULL, mapping_size, PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mapping == MAP_FAILED) {
        fprintf(stderr, "Failed to allocate stack mapping: %s\n",
                strerror(errno));
        return NULL;
    }

    // Protect the low end of the downward-growing stack.
    if (mprotect(mapping, page_size, PROT_NONE) != 0) {
        fprintf(stderr, "Failed to set stack guard page: %s\n",
                strerror(errno));
        (void)munmap(mapping, mapping_size);
        return NULL;
    }

    return (uint8_t *)mapping + page_size;
}

void destroy_stack(uint8_t *stack) {
    if (!stack) {
        return;
    }

    const size_t page_size = get_page_size();
    const size_t mapping_size = get_stack_mapping_size();

    // `stack` points past the guard page; recover the original mmap base.
    void *mapping = (void *)(stack - page_size);
    if (munmap(mapping, mapping_size) != 0) {
        fprintf(stderr, "Failed to release stack mapping: %s\n",
                strerror(errno));
    }
}

void task_register_stack_roots(struct Task *task) {
    if (!task || !task->stack || task->roots.registered) {
        return;
    }

    void *root_base = NULL;
    void *root_limit = NULL;
    get_stack_root_range(task->stack, &root_base, &root_limit);

    GC_add_roots(root_base, root_limit);
    task->roots.base = root_base;
    task->roots.limit = root_limit;
    task->roots.registered = true;
}

void task_unregister_stack_roots(struct Task *task) {
    if (!task || !task->roots.registered) {
        return;
    }

    GC_remove_roots(task->roots.base, task->roots.limit);
    task->roots.base = NULL;
    task->roots.limit = NULL;
    task->roots.registered = false;
}

void task_cleanup(struct Task *task) {
    if (!task) {
        return;
    }
    task_unregister_stack_roots(task);
    destroy_stack(task->stack);
    task->stack = NULL;
    task->arg = NULL;
    task->arg_size = 0;
    task->scheduler.state = TASK_RUNNABLE;
    task->scheduler.queued = false;
    task->scheduler.is_main = false;
    task->io_wait.fd = -1;
    task->io_wait.events = KAEDE_IO_EVENT_NONE;
    task->io_wait.wake_success = false;
    task->channel_wait.obj = NULL;
    task->channel_wait.kind = KAEDE_TASK_WAIT_NONE;
    task->channel_wait.value_slot = NULL;
    task->channel_wait.wake_success = false;
    task->channel_wait.next = NULL;
}
