#include <kaede/poller.h>

#include <errno.h>

bool kaede_poller_init(void) {
    errno = ENOSYS;
    return false;
}

void kaede_poller_deinit(void) {}

bool kaede_poller_add(int fd, uint32_t events) {
    (void)fd;
    (void)events;
    errno = ENOSYS;
    return false;
}

bool kaede_poller_mod(int fd, uint32_t events) {
    (void)fd;
    (void)events;
    errno = ENOSYS;
    return false;
}

bool kaede_poller_del(int fd) {
    (void)fd;
    errno = ENOSYS;
    return false;
}

int kaede_poller_wait(struct KaedePollEvent *events, int max_events, int timeout_ms) {
    (void)events;
    (void)max_events;
    (void)timeout_ms;
    errno = ENOSYS;
    return -1;
}

bool kaede_poller_wake(void) {
    errno = ENOSYS;
    return false;
}
