#include <kaede/arch/aarch64.h>
#include <string.h>

extern void task_entrypoint(void);

__attribute__((naked)) void context_switch(struct Context *old
                                           __attribute__((unused)),
                                           struct Context *new
                                           __attribute__((unused))) {
    __asm__ volatile(
        "mov x9, sp\n\t"
        "str x9,  [x0, #0x00]\n\t"
        "str x30, [x0, #0x08]\n\t"
        "str x29, [x0, #0x10]\n\t"
        "str x19, [x0, #0x18]\n\t"
        "str x20, [x0, #0x20]\n\t"
        "str x21, [x0, #0x28]\n\t"
        "str x22, [x0, #0x30]\n\t"
        "str x23, [x0, #0x38]\n\t"
        "str x24, [x0, #0x40]\n\t"
        "str x25, [x0, #0x48]\n\t"
        "str x26, [x0, #0x50]\n\t"
        "str x27, [x0, #0x58]\n\t"
        "str x28, [x0, #0x60]\n\t"
        "str d8,  [x0, #0x68]\n\t"
        "str d9,  [x0, #0x70]\n\t"
        "str d10, [x0, #0x78]\n\t"
        "str d11, [x0, #0x80]\n\t"
        "str d12, [x0, #0x88]\n\t"
        "str d13, [x0, #0x90]\n\t"
        "str d14, [x0, #0x98]\n\t"
        "str d15, [x0, #0xA0]\n\t"

        "ldr x9,  [x1, #0x00]\n\t"
        "mov sp, x9\n\t"
        "ldr x30, [x1, #0x08]\n\t"
        "ldr x29, [x1, #0x10]\n\t"
        "ldr x19, [x1, #0x18]\n\t"
        "ldr x20, [x1, #0x20]\n\t"
        "ldr x21, [x1, #0x28]\n\t"
        "ldr x22, [x1, #0x30]\n\t"
        "ldr x23, [x1, #0x38]\n\t"
        "ldr x24, [x1, #0x40]\n\t"
        "ldr x25, [x1, #0x48]\n\t"
        "ldr x26, [x1, #0x50]\n\t"
        "ldr x27, [x1, #0x58]\n\t"
        "ldr x28, [x1, #0x60]\n\t"
        "ldr d8,  [x1, #0x68]\n\t"
        "ldr d9,  [x1, #0x70]\n\t"
        "ldr d10, [x1, #0x78]\n\t"
        "ldr d11, [x1, #0x80]\n\t"
        "ldr d12, [x1, #0x88]\n\t"
        "ldr d13, [x1, #0x90]\n\t"
        "ldr d14, [x1, #0x98]\n\t"
        "ldr d15, [x1, #0xA0]\n\t"
        "ret\n\t");
}

void (*get_task_body(void))(void *) {
    uint64_t fn;
    __asm__ volatile("mov %0, x19\n\t" : "=r"(fn));
    return (void (*)(void *))fn;
}

void *get_task_arg(void) {
    uint64_t arg;
    __asm__ volatile("mov %0, x20\n\t" : "=r"(arg));
    return (void *)arg;
}

void create_context(struct Context *context, void (*task_body)(void *), void *arg,
                    uint64_t stack_top) {
    memset(context, 0, sizeof(*context));
    uint64_t sp = stack_top & ~0x0FULL;
    context->sp = sp;
    context->lr = (uint64_t)task_entrypoint;
    context->x19 = (uint64_t)task_body;
    context->x20 = (uint64_t)arg;
}
