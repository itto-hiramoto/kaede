#pragma once
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef int32_t sys_fd_t;

/* --- TCP server primitives (blocking) --- */
sys_fd_t kaede_sys_socket_tcp4(void);     // AF_INET / SOCK_STREAM
int kaede_sys_set_reuseaddr(sys_fd_t fd); // SO_REUSEADDR
int kaede_sys_bind_ipv4(sys_fd_t fd, const char *ip_or_null,
                        uint16_t port); // ip==NULL => 0.0.0.0
int kaede_sys_listen(sys_fd_t fd, int backlog);
sys_fd_t kaede_sys_accept(sys_fd_t listen_fd); // returns client fd, -1 on error
int kaede_sys_somaxconn(void); // best-effort SOMAXCONN, falls back to 128

/* --- I/O (EINTR hidden) --- */
long kaede_sys_read(sys_fd_t fd, void *buf,
                    size_t len); // returns n>=0, -1 on error
long kaede_sys_write(sys_fd_t fd, const void *buf,
                     size_t len); // returns n>=0, -1 on error
int kaede_sys_close(sys_fd_t fd);

/* --- Error introspection (optional but handy) --- */
int kaede_sys_errno(void);

int kaede_strcmp(const char *s1, size_t len1, const char *s2, size_t len2);

#ifdef __cplusplus
}
#endif
