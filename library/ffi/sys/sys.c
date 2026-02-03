#include "sys.h"

#include <errno.h>
#include <string.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

sys_fd_t kaede_sys_socket_tcp4(void) {
  return (sys_fd_t)socket(AF_INET, SOCK_STREAM, 0);
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
      return (sys_fd_t)c;
    if (errno == EINTR)
      continue;
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
    return -1;
  }
}

int kaede_sys_close(sys_fd_t fd) { return close((int)fd); }

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
