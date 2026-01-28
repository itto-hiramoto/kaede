#include <kaede/arch/x86_64.h>
#include <stdio.h>

extern void task_entrypoint(void);

// Mark the function as naked so the compiler does not emit a prologue/epilogue.
// We do all stack/register management manually; a regular prologue would
// corrupt the saved stack pointer and cause the subsequent `ret` to jump to an
// invalid address.
__attribute__((naked)) void context_switch(struct Context *old
                                           __attribute__((unused)),
                                           struct Context *new
                                           __attribute__((unused))) {
    __asm__ volatile(
        // Save callee-saved registers to old (arguments are already in rdi/rsi)
        "movq %rsp, 0(%rdi)\n\t"
        "movq %rbp, 8(%rdi)\n\t"
        "movq %rbx, 16(%rdi)\n\t"
        "movq %r12, 24(%rdi)\n\t"
        "movq %r13, 32(%rdi)\n\t"
        "movq %r14, 40(%rdi)\n\t"
        "movq %r15, 48(%rdi)\n\t"

        // Restore callee-saved registers from new
        "movq 0(%rsi), %rsp\n\t"
        "movq 8(%rsi), %rbp\n\t"
        "movq 16(%rsi), %rbx\n\t"
        "movq 24(%rsi), %r12\n\t"
        "movq 32(%rsi), %r13\n\t"
        "movq 40(%rsi), %r14\n\t"
        "movq 48(%rsi), %r15\n\t"

        // Continue execution at the return address on the new stack
        "ret\n\t");
}

void (*get_task_body(void))(void *) {
    uint64_t fn;
    __asm__ volatile("movq %%r15, %0\n\t" : "=r"(fn));
    return (void (*)(void *))fn;
}

void *get_task_arg(void) {
    uint64_t arg;
    __asm__ volatile("movq %%r14, %0\n\t" : "=r"(arg));
    return (void *)arg;
}

void create_context(struct Context *context, void (*task_body)(void *), void *arg,
                    uint64_t stack_top) {
    // Set RSP to a safe position below the guard page
    // Reserve space for return address (8 bytes) and ensure 16-byte alignment
    // Use a larger offset to ensure we're well below the guard page boundary
    uint64_t rsp = stack_top - 128;
    // Ensure 16-byte alignment (x86-64 ABI requirement)
    rsp = rsp & ~0x0F;

    // Store the function pointer as the return address on the stack
    *((void **)rsp) = task_entrypoint;

    context->rsp = rsp;
    context->rbp = 0;
    context->rbx = 0;
    context->r12 = 0;
    context->r13 = 0;
    context->r14 = (uint64_t)arg;
    context->r15 = (uint64_t)task_body;
}
