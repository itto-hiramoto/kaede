#include <kaede/signal.h>
#include <kaede/task.h>

#include <errno.h>
#include <signal.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

_Thread_local struct Task *kaede_current_task = NULL;

// Cached at handler-install time so the handler does not need to call
// sysconf() (which is technically async-signal-safe but better avoided).
// Set once on the main thread before any worker exists, then read-only.
static volatile sig_atomic_t cached_page_size = 4096;

static void safe_write(const void *buf, size_t len) {
    const char *p = (const char *)buf;
    while (len > 0) {
        ssize_t n = write(STDERR_FILENO, p, len);
        if (n < 0) {
            if (errno == EINTR) {
                continue;
            }
            return;
        }
        p += n;
        len -= (size_t)n;
    }
}

static void write_str(const char *s) {
    size_t len = 0;
    while (s[len] != '\0') {
        len++;
    }
    safe_write(s, len);
}

static void write_uint(uintmax_t v) {
    char buf[32];
    size_t i = sizeof(buf);
    if (v == 0) {
        buf[--i] = '0';
    } else {
        while (v > 0 && i > 0) {
            buf[--i] = (char)('0' + (v % 10));
            v /= 10;
        }
    }
    safe_write(buf + i, sizeof(buf) - i);
}

static void write_hex_ptr(uintptr_t v) {
    static const char digits[] = "0123456789abcdef";
    const int n = (int)(sizeof(uintptr_t) * 2);
    char buf[2 + sizeof(uintptr_t) * 2];
    buf[0] = '0';
    buf[1] = 'x';
    for (int i = 0; i < n; ++i) {
        buf[2 + n - 1 - i] = digits[v & 0xf];
        v >>= 4;
    }
    safe_write(buf, 2 + (size_t)n);
}

static void crash_handler(int sig, siginfo_t *info, void *uctx) {
    (void)uctx;

    void *fault_addr = info ? info->si_addr : NULL;
    struct Task *task = kaede_current_task;

    if (task != NULL && task->stack != NULL && fault_addr != NULL) {
        const uintptr_t addr = (uintptr_t)fault_addr;
        const uintptr_t stack_base = (uintptr_t)task->stack;
        const uintptr_t page_size = (uintptr_t)cached_page_size;
        const uintptr_t guard_lo = stack_base - page_size;
        const uintptr_t guard_hi = stack_base;

        if (addr >= guard_lo && addr < guard_hi) {
            write_str("\nkaede runtime: green thread stack overflow\n");
            write_str("  task: ");
            write_str(task->scheduler.is_main ? "main\n" : "spawned\n");
            write_str("  configured stack size: ");
            write_uint((uintmax_t)STACK_SIZE);
            write_str(" bytes\n");
            write_str("  faulting address: ");
            write_hex_ptr(addr);
            write_str("\n  stack base: ");
            write_hex_ptr(stack_base);
            write_str("\n");
        }
    }

    // Reset disposition and re-raise so the kernel takes its default action
    // (terminate, plus a core dump pointing at the original faulting frame
    // when core dumps are enabled).
    struct sigaction dfl;
    memset(&dfl, 0, sizeof(dfl));
    dfl.sa_handler = SIG_DFL;
    sigemptyset(&dfl.sa_mask);
    (void)sigaction(sig, &dfl, NULL);
    (void)raise(sig);
}

void kaede_install_crash_handler(void) {
    long ps = sysconf(_SC_PAGESIZE);
    if (ps > 0) {
        cached_page_size = (sig_atomic_t)ps;
    }

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_sigaction = crash_handler;
    sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
    sigemptyset(&sa.sa_mask);

    (void)sigaction(SIGSEGV, &sa, NULL);
    (void)sigaction(SIGBUS, &sa, NULL);
}

// Per-thread alternate stack storage. Backed by TLS so the pthread runtime
// reclaims it when the worker thread exits — no manual cleanup needed.
// 32 KiB is comfortably above MINSIGSTKSZ on every supported platform and
// plenty for our handler, which only writes a short diagnostic and
// re-raises the original signal.
#define KAEDE_ALTSTACK_SIZE (32u * 1024u)
static _Thread_local char kaede_altstack_storage[KAEDE_ALTSTACK_SIZE];

void kaede_install_worker_altstack(void) {
    stack_t ss;
    ss.ss_sp = kaede_altstack_storage;
    ss.ss_size = sizeof(kaede_altstack_storage);
    ss.ss_flags = 0;
    (void)sigaltstack(&ss, NULL);
}
