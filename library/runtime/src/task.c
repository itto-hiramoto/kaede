#include <kaede/task.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static inline size_t next_index(const struct TaskQueue *queue, size_t index) {
    return (index + 1) % queue->capacity;
}

static bool task_queue_grow(struct TaskQueue *queue) {
    size_t new_capacity = queue->capacity ? queue->capacity * 2 : 1;
    struct Task *new_tasks = calloc(new_capacity, sizeof(struct Task));
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

    queue->tasks = calloc(capacity, sizeof(struct Task));
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

bool task_queue_push(struct TaskQueue *queue, const struct Task *task) {
    if (!queue || !task || queue->capacity == 0) {
        return false;
    }

    if (task_queue_is_full(queue)) {
        if (!task_queue_grow(queue)) {
            return false;
        }
    }

    queue->tasks[queue->tail] = *task;
    queue->tail = next_index(queue, queue->tail);
    queue->count++;
    return true;
}

bool task_queue_pop(struct TaskQueue *queue, struct Task *task) {
    if (!queue || !task || task_queue_is_empty(queue)) {
        return false;
    }

    *task = queue->tasks[queue->head];
    queue->head = next_index(queue, queue->head);
    queue->count--;
    return true;
}

uint8_t *create_stack(void) {
    uint8_t *stack = malloc(STACK_SIZE);
    if (!stack) {
        fprintf(stderr, "Failed to allocate stack: %s\n", strerror(errno));
        return NULL;
    }
    return stack;
}

void destroy_stack(uint8_t *stack) {
    if (!stack) {
        return;
    }
    free(stack);
}

void task_cleanup(struct Task *task) {
    if (!task) {
        return;
    }
    destroy_stack(task->stack);
    task->stack = NULL;
    task->arg = NULL;
    task->arg_size = 0;
}
