#pragma once
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef int32_t sys_fd_t;

/* --- TCP server primitives --- */
sys_fd_t kaede_sys_socket_tcp4(void);     // AF_INET / SOCK_STREAM
int kaede_sys_set_reuseaddr(sys_fd_t fd); // SO_REUSEADDR
int kaede_sys_bind_ipv4(sys_fd_t fd, const unsigned char *ip_or_null,
                        uint16_t port); // ip==NULL => 0.0.0.0
int kaede_sys_listen(sys_fd_t fd, int backlog);
sys_fd_t kaede_sys_accept(sys_fd_t listen_fd); // returns client fd, -1 on error
int kaede_sys_somaxconn(void); // best-effort SOMAXCONN, falls back to 128

/* --- I/O (EINTR hidden, socket I/O parks on EAGAIN) --- */
sys_fd_t kaede_sys_open_read(const unsigned char *path, size_t path_len);
long kaede_sys_read(sys_fd_t fd, unsigned char *buf,
                    size_t len); // returns n>=0, -1 on error
long kaede_sys_write(sys_fd_t fd, const unsigned char *buf,
                     size_t len); // returns n>=0, -1 on error
int kaede_sys_is_regular_file(sys_fd_t fd);
int kaede_sys_close(sys_fd_t fd);
int kaede_sys_sleep_ms(uint64_t ms);

/* --- Error introspection (optional but handy) --- */
int kaede_sys_errno(void);

int kaede_strcmp(const unsigned char *s1, size_t len1, const unsigned char *s2, size_t len2);
uint32_t kaede_utf8_char_at(const unsigned char *s, uint64_t len, uint64_t index);
uint64_t kaede_utf8_char_count(const unsigned char *s, uint64_t len);
uint64_t kaede_utf8_byte_index_of_char(const unsigned char *s, uint64_t len, uint64_t index);

#ifdef __cplusplus
}
#endif
