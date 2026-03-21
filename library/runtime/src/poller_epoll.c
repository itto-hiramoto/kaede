#include <kaede/poller.h>
#include <kaede/task.h>

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <unistd.h>

static int poller_fd = -1;
static int wake_fd = -1;

static uint32_t to_epoll_events(uint32_t events) {
    uint32_t epoll_events = 0;
    if ((events & KAEDE_IO_EVENT_READ) != 0) {
        epoll_events |= EPOLLIN | EPOLLRDHUP | EPOLLPRI;
    }
    if ((events & KAEDE_IO_EVENT_WRITE) != 0) {
        epoll_events |= EPOLLOUT;
    }
    return epoll_events;
}

static uint32_t from_epoll_events(uint32_t epoll_events) {
    uint32_t events = KAEDE_IO_EVENT_NONE;
    if ((epoll_events & (EPOLLIN | EPOLLPRI | EPOLLRDHUP)) != 0) {
        events |= KAEDE_IO_EVENT_READ;
    }
    if ((epoll_events & EPOLLOUT) != 0) {
        events |= KAEDE_IO_EVENT_WRITE;
    }
    if ((epoll_events & (EPOLLERR | EPOLLHUP)) != 0) {
        events |= KAEDE_IO_EVENT_READ | KAEDE_IO_EVENT_WRITE;
    }
    return events;
}

static bool poller_ctl(int op, int fd, uint32_t events) {
    struct epoll_event event;
    memset(&event, 0, sizeof(event));
    event.events = to_epoll_events(events);
    event.data.fd = fd;

    if (epoll_ctl(poller_fd, op, fd, &event) == 0) {
        return true;
    }

    fprintf(stderr, "epoll_ctl failed: %s\n", strerror(errno));
    return false;
}

static void drain_wake_fd(void) {
    eventfd_t value;
    // eventfd becomes readable while its internal counter is non-zero.
    // Reading drains the pending wakeup count back to zero, so epoll will not
    // keep reporting wake_fd as readable forever.
    if (eventfd_read(wake_fd, &value) != 0 && errno != EAGAIN) {
        fprintf(stderr, "Failed to drain poller wake fd: %s\n", strerror(errno));
    }
}

bool kaede_poller_init(void) {
    if (poller_fd >= 0) {
        return true;
    }

    poller_fd = epoll_create1(EPOLL_CLOEXEC);
    if (poller_fd < 0) {
        fprintf(stderr, "Failed to create epoll instance: %s\n", strerror(errno));
        return false;
    }

    wake_fd = eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK);
    if (wake_fd < 0) {
        fprintf(stderr, "Failed to create eventfd: %s\n", strerror(errno));
        close(poller_fd);
        poller_fd = -1;
        return false;
    }

    // Register the internal wake fd as a read event. kaede_poller_wake()
    // writes to the eventfd, and drain_wake_fd() consumes that counter so the
    // poller can go back to sleep after handling the wakeup.
    if (!poller_ctl(EPOLL_CTL_ADD, wake_fd, KAEDE_IO_EVENT_READ)) {
        close(wake_fd);
        close(poller_fd);
        wake_fd = -1;
        poller_fd = -1;
        return false;
    }

    return true;
}

void kaede_poller_deinit(void) {
    if (wake_fd >= 0) {
        close(wake_fd);
        wake_fd = -1;
    }
    if (poller_fd >= 0) {
        close(poller_fd);
        poller_fd = -1;
    }
}

bool kaede_poller_set(int fd, uint32_t old_events, uint32_t new_events) {
    if (old_events == new_events) {
        return true;
    }

    if (new_events == KAEDE_IO_EVENT_NONE) {
        if (epoll_ctl(poller_fd, EPOLL_CTL_DEL, fd, NULL) == 0 || errno == ENOENT) {
            return true;
        }

        fprintf(stderr, "epoll_ctl delete failed: %s\n", strerror(errno));
        return false;
    }

    if (old_events == KAEDE_IO_EVENT_NONE) {
        return poller_ctl(EPOLL_CTL_ADD, fd, new_events);
    }

    return poller_ctl(EPOLL_CTL_MOD, fd, new_events);
}

int kaede_poller_wait(struct KaedePollEvent *events, int max_events, int timeout_ms) {
    if (!events || max_events <= 0) {
        errno = EINVAL;
        return -1;
    }

    struct epoll_event ready_events[max_events];
    int ready;
    for (;;) {
        ready = epoll_wait(poller_fd, ready_events, max_events, timeout_ms);
        if (ready >= 0) {
            break;
        }
        // stop-the-world GC and other signal-based coordination can interrupt
        // epoll_wait with EINTR. That is not a real poller failure, so retry.
        if (errno != EINTR) {
            return -1;
        }
    }

    int count = 0;
    for (int i = 0; i < ready; ++i) {
        if (ready_events[i].data.fd == wake_fd) {
            drain_wake_fd();
            continue;
        }

        const uint32_t translated = from_epoll_events(ready_events[i].events);
        if (translated == KAEDE_IO_EVENT_NONE) {
            continue;
        }

        events[count].fd = ready_events[i].data.fd;
        events[count].events = translated;
        count++;
    }

    return count;
}

bool kaede_poller_wake(void) {
    eventfd_t value = 1;
    if (eventfd_write(wake_fd, value) == 0) {
        return true;
    }

    if (errno == EAGAIN) {
        return true;
    }

    fprintf(stderr, "Failed to wake poller: %s\n", strerror(errno));
    return false;
}
