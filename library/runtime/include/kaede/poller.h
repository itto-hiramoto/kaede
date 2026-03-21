#ifndef KAEDE_POLLER_H
#define KAEDE_POLLER_H

#include <stdbool.h>
#include <stdint.h>

struct KaedePollEvent {
    int fd;
    uint32_t events;
};

bool kaede_poller_init(void);
void kaede_poller_deinit(void);
bool kaede_poller_set(int fd, uint32_t old_events, uint32_t new_events);
int kaede_poller_wait(struct KaedePollEvent *events, int max_events, int timeout_ms);
bool kaede_poller_wake(void);

#endif // KAEDE_POLLER_H
