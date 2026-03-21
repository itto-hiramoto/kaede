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

// Task stacks are fixed-size for now and the whole region is registered as a GC
// root set, so keep this large enough for ordinary call depth but not so large
// that thousands of parked tasks become expensive to scan.
#define STACK_SIZE (256 * 1024) // 256KB

enum TaskState {
    TASK_RUNNABLE,
    TASK_WAITING_IO,
    TASK_FINISHED,
};

enum KaedeIoEvent {
    KAEDE_IO_EVENT_NONE = 0,
    KAEDE_IO_EVENT_READ = 1u << 0,
    KAEDE_IO_EVENT_WRITE = 1u << 1,
};

struct Task {
    struct Context context;
    TaskFn fn;
    void *arg;
    size_t arg_size;
    uint8_t *stack;
    void *stack_root_base;
    void *stack_root_limit;
    bool roots_registered;
    bool finished;
    enum TaskState state;
    int waiting_fd;
    uint32_t waiting_events;
    bool is_main;
};

struct TaskQueue {
    struct Task **tasks;
    size_t capacity;
    size_t head;
    size_t tail;
    size_t count;
};

bool task_queue_init(struct TaskQueue *queue, size_t capacity);
void task_queue_deinit(struct TaskQueue *queue);
bool task_queue_is_empty(const struct TaskQueue *queue);
bool task_queue_is_full(const struct TaskQueue *queue);
bool task_queue_push(struct TaskQueue *queue, struct Task *task);
bool task_queue_pop(struct TaskQueue *queue, struct Task **task);

uint8_t *create_stack(void);
void destroy_stack(uint8_t *stack);
void task_register_stack_roots(struct Task *task);
void task_unregister_stack_roots(struct Task *task);
void task_cleanup(struct Task *task);

#endif // KAEDE_TASK_H
