#include "sys.h"

#include <kaede/runtime.h>

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

// Socket I/O has to be non-blocking so EAGAIN can park the current task
// without blocking the whole worker thread in the kernel.
static int set_nonblocking(int fd) {
  for (;;) {
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags >= 0) {
      if ((flags & O_NONBLOCK) != 0) {
        return 0;
      }

      for (;;) {
        if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == 0) {
          return 0;
        }
        if (errno != EINTR) {
          return -1;
        }
      }
    }

    if (errno != EINTR) {
      return -1;
    }
  }
}

static sys_fd_t set_nonblocking_or_close(sys_fd_t fd) {
  if (fd < 0) {
    return (sys_fd_t)-1;
  }

  if (set_nonblocking((int)fd) == 0) {
    return fd;
  }

  const int saved_errno = errno;
  (void)close((int)fd);
  errno = saved_errno;
  return (sys_fd_t)-1;
}

sys_fd_t kaede_sys_socket_tcp4(void) {
  return set_nonblocking_or_close(
      (sys_fd_t)socket(AF_INET, SOCK_STREAM, 0));
}

int kaede_sys_set_reuseaddr(sys_fd_t fd) {
  int yes = 1;
  return setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &yes, (socklen_t)sizeof(yes));
}

int kaede_sys_bind_ipv4(sys_fd_t fd, const char *ip_or_null, uint16_t port) {
  struct sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));

  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);

  if (ip_or_null == NULL) {
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
  } else {
    // inet_pton returns 1 on success, 0 invalid, -1 error
    int ok = inet_pton(AF_INET, ip_or_null, &addr.sin_addr);
    if (ok != 1) {
      errno = EINVAL;
      return -1;
    }
  }

  return bind(fd, (struct sockaddr *)&addr, (socklen_t)sizeof(addr));
}

int kaede_sys_listen(sys_fd_t fd, int backlog) { return listen(fd, backlog); }

sys_fd_t kaede_sys_accept(sys_fd_t listen_fd) {
  for (;;) {
    int c = accept((int)listen_fd, NULL, NULL);
    if (c >= 0)
      return set_nonblocking_or_close((sys_fd_t)c);
    if (errno == EINTR)
      continue;
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      if (!kaede_io_wait_readable((int)listen_fd))
        return (sys_fd_t)-1;
      continue;
    }
    return (sys_fd_t)-1;
  }
}

sys_fd_t kaede_sys_open_read(const char *path, size_t path_len) {
  char *owned_path = (char *)malloc(path_len + 1);
  if (owned_path == NULL)
    return (sys_fd_t)-1;

  memcpy(owned_path, path, path_len);
  owned_path[path_len] = '\0';

  for (;;) {
    int fd = open(owned_path, O_RDONLY);
    if (fd >= 0) {
      free(owned_path);
      return (sys_fd_t)fd;
    }
    if (errno == EINTR)
      continue;
    free(owned_path);
    return (sys_fd_t)-1;
  }
}

long kaede_sys_read(sys_fd_t fd, void *buf, size_t len) {
  for (;;) {
    ssize_t n = read((int)fd, buf, len);
    if (n >= 0)
      return (long)n;
    if (errno == EINTR)
      continue;
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      if (!kaede_io_wait_readable((int)fd))
        return -1;
      continue;
    }
    return -1;
  }
}

long kaede_sys_write(sys_fd_t fd, const void *buf, size_t len) {
  for (;;) {
    ssize_t n = write((int)fd, buf, len);
    if (n >= 0)
      return (long)n;
    if (errno == EINTR)
      continue;
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      if (!kaede_io_wait_writable((int)fd))
        return -1;
      continue;
    }
    return -1;
  }
}

int kaede_sys_is_regular_file(sys_fd_t fd) {
  struct stat st;
  if (fstat((int)fd, &st) < 0)
    return -1;

  return S_ISREG(st.st_mode) ? 1 : 0;
}

int kaede_sys_close(sys_fd_t fd) {
  // Wake tasks parked on this fd so they do not sleep forever after close.
  kaede_io_forget_fd((int)fd);
  return close((int)fd);
}

int kaede_sys_sleep_ms(uint64_t ms) {
  struct timespec req;
  req.tv_sec = (time_t)(ms / 1000);
  req.tv_nsec = (long)((ms % 1000) * 1000000ULL);

  for (;;) {
    if (nanosleep(&req, &req) == 0)
      return 0;
    if (errno == EINTR)
      continue;
    return -1;
  }
}

int kaede_sys_errno(void) { return errno; }

int kaede_strcmp(const char *s1, size_t len1, const char *s2, size_t len2) {
  if (len1 != len2)
    return 1;

  return memcmp(s1, s2, len1);
}

int kaede_sys_somaxconn(void) {
#ifdef SOMAXCONN
  return SOMAXCONN;
#else
  return 128;
#endif
}
