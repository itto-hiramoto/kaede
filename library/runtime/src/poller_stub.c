#include <kaede/poller.h>

#include <errno.h>

bool kaede_poller_init(void) {
    errno = ENOSYS;
    return false;
}

void kaede_poller_deinit(void) {}

bool kaede_poller_set(int fd, uint32_t old_events, uint32_t new_events) {
    (void)fd;
    (void)old_events;
    (void)new_events;
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
