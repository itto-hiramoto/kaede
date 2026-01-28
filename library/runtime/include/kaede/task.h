#ifndef KAEDE_TASK_H
#define KAEDE_TASK_H

#if defined(__x86_64__) || defined(_M_X64)
#include <kaede/arch/x86_64.h>
#elif defined(__aarch64__) || defined(_M_ARM64)
#include <kaede/arch/aarch64.h>
#else
#error "Unsupported architecture for kaede runtime"
#endif
#include <kaede/runtime.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define STACK_SIZE (64 * 1024) // 64KB

struct Task {
    struct Context context;
    TaskFn fn;
    void *arg;
    size_t arg_size;
    uint8_t *stack;
    bool finished;
    bool is_main;
};

struct TaskQueue {
    struct Task *tasks;
    size_t capacity;
    size_t head;
    size_t tail;
    size_t count;
};

bool task_queue_init(struct TaskQueue *queue, size_t capacity);
void task_queue_deinit(struct TaskQueue *queue);
bool task_queue_is_empty(const struct TaskQueue *queue);
bool task_queue_is_full(const struct TaskQueue *queue);
bool task_queue_push(struct TaskQueue *queue, const struct Task *task);
bool task_queue_pop(struct TaskQueue *queue, struct Task *task);

uint8_t *create_stack(void);
void destroy_stack(uint8_t *stack);
void task_cleanup(struct Task *task);

#endif // KAEDE_TASK_H
