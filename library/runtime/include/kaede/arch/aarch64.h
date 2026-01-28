#ifndef KAEDE_ARCH_AARCH64_H
#define KAEDE_ARCH_AARCH64_H

#include <stdint.h>

struct Context {
    uint64_t sp;
    uint64_t lr;
    uint64_t fp;
    uint64_t x19;
    uint64_t x20;
    uint64_t x21;
    uint64_t x22;
    uint64_t x23;
    uint64_t x24;
    uint64_t x25;
    uint64_t x26;
    uint64_t x27;
    uint64_t x28;
    uint64_t d8;
    uint64_t d9;
    uint64_t d10;
    uint64_t d11;
    uint64_t d12;
    uint64_t d13;
    uint64_t d14;
    uint64_t d15;
};

void create_context(struct Context *context, void (*fn)(void *), void *arg,
                    uint64_t stack_top);

void (*get_task_body(void))(void *);
void *get_task_arg(void);

void context_switch(struct Context *old, struct Context *new);

#endif // KAEDE_ARCH_AARCH64_H
