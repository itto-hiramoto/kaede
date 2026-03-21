#include <kaede/poller.h>
#include <kaede/task.h>

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/event.h>
#include <unistd.h>

static int poller_fd = -1;
static int wake_read_fd = -1;
static int wake_write_fd = -1;

static void close_if_open(int *fd) {
    if (*fd >= 0) {
        close(*fd);
        *fd = -1;
    }
}

static bool set_fd_flag(int fd, int get_cmd, int set_cmd, int flag) {
    for (;;) {
        int value = fcntl(fd, get_cmd, 0);
        if (value >= 0) {
            if ((value & flag) != 0) {
                return true;
            }

            for (;;) {
                if (fcntl(fd, set_cmd, value | flag) == 0) {
                    return true;
                }
                if (errno != EINTR) {
                    return false;
                }
            }
        }

        if (errno != EINTR) {
            return false;
        }
    }
}

static bool prepare_fd(int fd) {
    return set_fd_flag(fd, F_GETFD, F_SETFD, FD_CLOEXEC) &&
           set_fd_flag(fd, F_GETFL, F_SETFL, O_NONBLOCK);
}

static bool create_wake_pipe(void) {
    int pipe_fds[2] = {-1, -1};
    if (pipe(pipe_fds) != 0) {
        return false;
    }

    if (!prepare_fd(pipe_fds[0]) || !prepare_fd(pipe_fds[1])) {
        const int saved_errno = errno;
        close(pipe_fds[0]);
        close(pipe_fds[1]);
        errno = saved_errno;
        return false;
    }

    wake_read_fd = pipe_fds[0];
    wake_write_fd = pipe_fds[1];
    return true;
}

static bool apply_filter_change(int fd, int16_t filter, bool enable) {
    struct kevent change;
    EV_SET(&change, (uintptr_t)fd, filter,
           enable ? (uint16_t)(EV_ADD | EV_ENABLE | EV_CLEAR) : EV_DELETE, 0, 0,
           NULL);

    int rc;
    for (;;) {
        rc = kevent(poller_fd, &change, 1, NULL, 0, NULL);
        if (rc == 0) {
            return true;
        }
        if (errno != EINTR) {
            break;
        }
    }

    if (!enable && errno == ENOENT) {
        return true;
    }

    fprintf(stderr, "kevent change failed: %s\n", strerror(errno));
    return false;
}

static bool sync_filter(int fd, uint32_t old_events, uint32_t new_events,
                        uint32_t event_bit, int16_t filter) {
    const bool had_event = (old_events & event_bit) != 0;
    const bool needs_event = (new_events & event_bit) != 0;

    if (had_event == needs_event) {
        return true;
    }

    return apply_filter_change(fd, filter, needs_event);
}

static bool reverse_filter(int fd, uint32_t old_events, uint32_t new_events,
                           uint32_t event_bit, int16_t filter) {
    const bool had_event = (old_events & event_bit) != 0;
    const bool needs_event = (new_events & event_bit) != 0;

    if (had_event == needs_event) {
        return true;
    }

    return apply_filter_change(fd, filter, had_event);
}

static void drain_wake_fd(void) {
    uint8_t buffer[256];
    for (;;) {
        ssize_t read_bytes = read(wake_read_fd, buffer, sizeof(buffer));
        if (read_bytes > 0) {
            continue;
        }
        if (read_bytes == 0) {
            return;
        }
        if (errno == EINTR) {
            continue;
        }
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return;
        }

        fprintf(stderr, "Failed to drain poller wake pipe: %s\n", strerror(errno));
        return;
    }
}

static uint32_t translate_ready_event(const struct kevent *event) {
    uint32_t ready = KAEDE_IO_EVENT_NONE;
    if (event->filter == EVFILT_READ) {
        ready |= KAEDE_IO_EVENT_READ;
    }
    if (event->filter == EVFILT_WRITE) {
        ready |= KAEDE_IO_EVENT_WRITE;
    }
    if ((event->flags & (EV_EOF | EV_ERROR)) != 0) {
        ready |= KAEDE_IO_EVENT_READ | KAEDE_IO_EVENT_WRITE;
    }
    return ready;
}

static int merge_ready_event(struct KaedePollEvent *events, int count, int fd,
                             uint32_t ready) {
    for (int i = 0; i < count; ++i) {
        if (events[i].fd == fd) {
            events[i].events |= ready;
            return count;
        }
    }

    events[count].fd = fd;
    events[count].events = ready;
    return count + 1;
}

bool kaede_poller_init(void) {
    if (poller_fd >= 0) {
        return true;
    }

    poller_fd = kqueue();
    if (poller_fd < 0) {
        fprintf(stderr, "Failed to create kqueue instance: %s\n", strerror(errno));
        return false;
    }

    if (!create_wake_pipe()) {
        fprintf(stderr, "Failed to create poller wake pipe: %s\n", strerror(errno));
        close_if_open(&poller_fd);
        return false;
    }

    if (!apply_filter_change(wake_read_fd, EVFILT_READ, true)) {
        close_if_open(&wake_write_fd);
        close_if_open(&wake_read_fd);
        close_if_open(&poller_fd);
        return false;
    }

    return true;
}

void kaede_poller_deinit(void) {
    close_if_open(&wake_write_fd);
    close_if_open(&wake_read_fd);
    close_if_open(&poller_fd);
}

bool kaede_poller_set(int fd, uint32_t old_events, uint32_t new_events) {
    if (old_events == new_events) {
        return true;
    }

    if (!sync_filter(fd, old_events, new_events, KAEDE_IO_EVENT_READ,
                     EVFILT_READ)) {
        return false;
    }

    if (sync_filter(fd, old_events, new_events, KAEDE_IO_EVENT_WRITE,
                    EVFILT_WRITE)) {
        return true;
    }

    (void)reverse_filter(fd, old_events, new_events, KAEDE_IO_EVENT_READ,
                         EVFILT_READ);
    return false;
}

int kaede_poller_wait(struct KaedePollEvent *events, int max_events,
                      int timeout_ms) {
    if (!events || max_events <= 0) {
        errno = EINVAL;
        return -1;
    }

    struct kevent ready_events[max_events];
    struct timespec timeout;
    struct timespec *timeout_ptr = NULL;

    if (timeout_ms >= 0) {
        timeout.tv_sec = timeout_ms / 1000;
        timeout.tv_nsec = (long)(timeout_ms % 1000) * 1000000L;
        timeout_ptr = &timeout;
    }

    int ready;
    for (;;) {
        ready = kevent(poller_fd, NULL, 0, ready_events, max_events, timeout_ptr);
        if (ready >= 0) {
            break;
        }
        if (errno != EINTR) {
            return -1;
        }
    }

    int count = 0;
    for (int i = 0; i < ready; ++i) {
        if ((int)ready_events[i].ident == wake_read_fd &&
            ready_events[i].filter == EVFILT_READ) {
            drain_wake_fd();
            continue;
        }

        const uint32_t translated = translate_ready_event(&ready_events[i]);
        if (translated == KAEDE_IO_EVENT_NONE) {
            continue;
        }

        count = merge_ready_event(events, count, (int)ready_events[i].ident,
                                  translated);
    }

    return count;
}

bool kaede_poller_wake(void) {
    const uint8_t byte = 1;

    for (;;) {
        if (write(wake_write_fd, &byte, sizeof(byte)) == (ssize_t)sizeof(byte)) {
            return true;
        }
        if (errno == EINTR) {
            continue;
        }
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return true;
        }

        fprintf(stderr, "Failed to wake poller: %s\n", strerror(errno));
        return false;
    }
}
