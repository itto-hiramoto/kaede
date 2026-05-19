#include "sys.h"

#include <kaede/runtime.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#ifndef O_DIRECTORY
#define O_DIRECTORY 0
#endif

// Kaede paths cross the FFI boundary as pointer + length slices. POSIX file
// APIs require NUL-terminated strings, so every path passed to open/rename/etc.
// must first be copied into an owned C string.
static unsigned char *path_slice_to_c_string(const unsigned char *path,
                                             size_t path_len) {
  if (path == NULL) {
    errno = EINVAL;
    return NULL;
  }

  unsigned char *owned_path = (unsigned char *)malloc(path_len + 1);
  if (owned_path == NULL) {
    return NULL;
  }

  memcpy(owned_path, path, path_len);
  owned_path[path_len] = '\0';
  return owned_path;
}

// Translate the structured std.fs OpenOptions fields into POSIX open flags.
// Keeping this mapping here keeps raw OS bit flags out of the Kaede API.
static int build_open_flags(int read, int write, int create, int create_new,
                            int truncate, int append) {
  int flags;

  if (read && write) {
    flags = O_RDWR;
  } else if (write) {
    flags = O_WRONLY;
  } else {
    flags = O_RDONLY;
  }

  if (create) {
    flags |= O_CREAT;
  }
  if (create_new) {
    flags |= O_CREAT | O_EXCL;
  }
  if (truncate) {
    flags |= O_TRUNC;
  }
  if (append) {
    flags |= O_APPEND;
  }

  return flags;
}

// write(2) may write fewer bytes than requested, or return EINTR before
// writing anything. Keep writing until the full temp file body is on disk
// before the atomic rename step.
static ssize_t write_all_blocking(int fd, const unsigned char *buf,
                                  size_t len) {
  size_t off = 0;

  while (off < len) {
    ssize_t n = write(fd, buf + off, len - off);
    if (n > 0) {
      off += (size_t)n;
      continue;
    }
    if (n == 0) {
      errno = EIO;
      return -1;
    }
    if (errno == EINTR) {
      continue;
    }
    return -1;
  }

  return (ssize_t)off;
}

// Error paths often close a temporary fd after another syscall has already set
// errno. Preserve that original errno so callers see the real failure cause.
static int retry_close_preserve_errno(int fd) {
  int saved_errno = errno;
  int close_result;
  do {
    close_result = close(fd);
  } while (close_result < 0 && errno == EINTR);
  errno = saved_errno;
  return close_result;
}

// Directory fsync is what makes a successful rename/unlink durable across a
// crash on common Unix filesystems. It is optional because some callers may not
// need the extra durability cost.
static int sync_dir_if_requested(int dir_fd, int sync_dir) {
  if (!sync_dir) {
    return 0;
  }

  for (;;) {
    if (fsync(dir_fd) == 0) {
      return 0;
    }
    if (errno != EINTR) {
      return -1;
    }
  }
}

// Split an owned, mutable C path into parent directory and basename. This edits
// the path buffer in place by replacing the final slash with NUL.
static int split_parent_and_basename(unsigned char *path, char **parent_out,
                                     char **base_out) {
  char *path_str = (char *)path;
  char *last_slash = strrchr(path_str, '/');

  if (last_slash == NULL) {
    *parent_out = ".";
    *base_out = path_str;
    return 0;
  }

  if (last_slash[1] == '\0') {
    errno = EINVAL;
    return -1;
  }

  *base_out = last_slash + 1;

  if (last_slash == path_str) {
    last_slash[1] = '\0';
    *parent_out = path_str;
    return 0;
  }

  *last_slash = '\0';
  *parent_out = path_str;
  return 0;
}

// Build a deterministic candidate name from a user pattern. The caller opens it
// with O_EXCL and retries on EEXIST, so predictability here does not overwrite
// existing files.
static int make_temp_name(const char *pattern, unsigned attempt, char *out,
                          size_t out_cap) {
  char suffix[64];
  int suffix_len =
      snprintf(suffix, sizeof(suffix), "%ld-%u", (long)getpid(), attempt);
  if (suffix_len < 0 || (size_t)suffix_len >= sizeof(suffix)) {
    errno = EINVAL;
    return -1;
  }

  const char *star = strchr(pattern, '*');
  int written;
  if (star != NULL) {
    size_t prefix_len = (size_t)(star - pattern);
    written = snprintf(out, out_cap, "%.*s%s%s", (int)prefix_len, pattern,
                       suffix, star + 1);
  } else {
    written = snprintf(out, out_cap, "%s%s", pattern, suffix);
  }

  if (written < 0 || (size_t)written >= out_cap) {
    errno = ENAMETOOLONG;
    return -1;
  }

  return written;
}

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
  return set_nonblocking_or_close((sys_fd_t)socket(AF_INET, SOCK_STREAM, 0));
}

int kaede_sys_set_reuseaddr(sys_fd_t fd) {
  int yes = 1;
  return setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &yes, (socklen_t)sizeof(yes));
}

int kaede_sys_bind_ipv4(sys_fd_t fd, const unsigned char *ip_or_null,
                        uint16_t port) {
  struct sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));

  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);

  if (ip_or_null == NULL) {
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
  } else {
    // inet_pton returns 1 on success, 0 invalid, -1 error
    int ok = inet_pton(AF_INET, (const char *)ip_or_null, &addr.sin_addr);
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
      KaedeIoWaitResult wait_result = kaede_io_wait_readable((int)listen_fd);
      if (wait_result == KAEDE_IO_WAIT_READY)
        continue;
      if (wait_result == KAEDE_IO_WAIT_CLOSED) {
        errno = EBADF;
      }
      return (sys_fd_t)-1;
    }
    return (sys_fd_t)-1;
  }
}

sys_fd_t kaede_sys_open_file(const unsigned char *path, size_t path_len,
                             int read, int write, int create, int create_new,
                             int truncate, int append, uint32_t mode) {
  unsigned char *owned_path = path_slice_to_c_string(path, path_len);
  if (owned_path == NULL)
    return (sys_fd_t)-1;

  int flags = build_open_flags(read, write, create, create_new, truncate, append);

  for (;;) {
    int fd = open((const char *)owned_path, flags, (mode_t)mode);
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

sys_fd_t kaede_sys_open_dir(const unsigned char *path, size_t path_len) {
  unsigned char *owned_path = path_slice_to_c_string(path, path_len);
  if (owned_path == NULL)
    return (sys_fd_t)-1;

  for (;;) {
    int fd = open((const char *)owned_path, O_RDONLY | O_DIRECTORY);
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

sys_fd_t kaede_sys_open_file_at(sys_fd_t dir_fd, const unsigned char *path,
                                size_t path_len, int read, int write,
                                int create, int create_new, int truncate,
                                int append, uint32_t mode) {
  unsigned char *owned_path = path_slice_to_c_string(path, path_len);
  if (owned_path == NULL)
    return (sys_fd_t)-1;

  int flags = build_open_flags(read, write, create, create_new, truncate, append);

  for (;;) {
    int fd = openat((int)dir_fd, (const char *)owned_path, flags, (mode_t)mode);
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

sys_fd_t kaede_sys_create_temp_at(sys_fd_t dir_fd,
                                  const unsigned char *pattern,
                                  size_t pattern_len, uint32_t mode,
                                  unsigned char *name_out, size_t name_cap) {
  unsigned char *owned_pattern = path_slice_to_c_string(pattern, pattern_len);
  if (owned_pattern == NULL)
    return (sys_fd_t)-1;

  if (name_out == NULL || name_cap == 0) {
    free(owned_pattern);
    errno = EINVAL;
    return (sys_fd_t)-1;
  }

  char candidate[256];
  for (unsigned attempt = 0; attempt < 1000; ++attempt) {
    int candidate_len =
        make_temp_name((const char *)owned_pattern, attempt, candidate,
                       sizeof(candidate));
    if (candidate_len < 0) {
      free(owned_pattern);
      return (sys_fd_t)-1;
    }

    if ((size_t)candidate_len >= name_cap) {
      free(owned_pattern);
      errno = ENAMETOOLONG;
      return (sys_fd_t)-1;
    }

    int fd = openat((int)dir_fd, candidate, O_WRONLY | O_CREAT | O_EXCL,
                    (mode_t)mode);
    if (fd >= 0) {
      memcpy(name_out, candidate, (size_t)candidate_len + 1);
      free(owned_pattern);
      return (sys_fd_t)fd;
    }

    if (errno == EINTR) {
      --attempt;
      continue;
    }
    if (errno == EEXIST) {
      continue;
    }

    free(owned_pattern);
    return (sys_fd_t)-1;
  }

  free(owned_pattern);
  errno = EEXIST;
  return (sys_fd_t)-1;
}

long kaede_sys_read(sys_fd_t fd, unsigned char *buf, size_t len) {
  for (;;) {
    ssize_t n = read((int)fd, buf, len);
    if (n >= 0)
      return (long)n;
    if (errno == EINTR)
      continue;
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      KaedeIoWaitResult wait_result = kaede_io_wait_readable((int)fd);
      if (wait_result == KAEDE_IO_WAIT_READY)
        continue;
      if (wait_result == KAEDE_IO_WAIT_CLOSED) {
        errno = EBADF;
      }
      return -1;
    }
    return -1;
  }
}

long kaede_sys_write(sys_fd_t fd, const unsigned char *buf, size_t len) {
  for (;;) {
    ssize_t n = write((int)fd, buf, len);
    if (n >= 0)
      return (long)n;
    if (errno == EINTR)
      continue;
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      KaedeIoWaitResult wait_result = kaede_io_wait_writable((int)fd);
      if (wait_result == KAEDE_IO_WAIT_READY)
        continue;
      if (wait_result == KAEDE_IO_WAIT_CLOSED) {
        errno = EBADF;
      }
      return -1;
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

int kaede_sys_fsync(sys_fd_t fd) {
  for (;;) {
    if (fsync((int)fd) == 0)
      return 0;
    if (errno != EINTR)
      return -1;
  }
}

int kaede_sys_fdatasync(sys_fd_t fd) {
#if defined(__APPLE__)
  return kaede_sys_fsync(fd);
#else
  for (;;) {
    if (fdatasync((int)fd) == 0)
      return 0;
    if (errno != EINTR)
      return -1;
  }
#endif
}

int kaede_sys_rename(const unsigned char *old_path, size_t old_path_len,
                     const unsigned char *new_path, size_t new_path_len) {
  unsigned char *owned_old_path =
      path_slice_to_c_string(old_path, old_path_len);
  if (owned_old_path == NULL)
    return -1;

  unsigned char *owned_new_path =
      path_slice_to_c_string(new_path, new_path_len);
  if (owned_new_path == NULL) {
    free(owned_old_path);
    return -1;
  }

  for (;;) {
    if (rename((const char *)owned_old_path, (const char *)owned_new_path) == 0) {
      free(owned_old_path);
      free(owned_new_path);
      return 0;
    }
    if (errno == EINTR)
      continue;
    free(owned_old_path);
    free(owned_new_path);
    return -1;
  }
}

int kaede_sys_rename_at(sys_fd_t dir_fd, const unsigned char *old_path,
                        size_t old_path_len, const unsigned char *new_path,
                        size_t new_path_len) {
  unsigned char *owned_old_path =
      path_slice_to_c_string(old_path, old_path_len);
  if (owned_old_path == NULL)
    return -1;

  unsigned char *owned_new_path =
      path_slice_to_c_string(new_path, new_path_len);
  if (owned_new_path == NULL) {
    free(owned_old_path);
    return -1;
  }

  for (;;) {
    if (renameat((int)dir_fd, (const char *)owned_old_path, (int)dir_fd,
                 (const char *)owned_new_path) == 0) {
      free(owned_old_path);
      free(owned_new_path);
      return 0;
    }
    if (errno == EINTR)
      continue;
    free(owned_old_path);
    free(owned_new_path);
    return -1;
  }
}

int kaede_sys_unlink(const unsigned char *path, size_t path_len) {
  unsigned char *owned_path = path_slice_to_c_string(path, path_len);
  if (owned_path == NULL)
    return -1;

  for (;;) {
    if (unlink((const char *)owned_path) == 0) {
      free(owned_path);
      return 0;
    }
    if (errno == EINTR)
      continue;
    free(owned_path);
    return -1;
  }
}

int kaede_sys_unlink_at(sys_fd_t dir_fd, const unsigned char *path,
                        size_t path_len) {
  unsigned char *owned_path = path_slice_to_c_string(path, path_len);
  if (owned_path == NULL)
    return -1;

  for (;;) {
    if (unlinkat((int)dir_fd, (const char *)owned_path, 0) == 0) {
      free(owned_path);
      return 0;
    }
    if (errno == EINTR)
      continue;
    free(owned_path);
    return -1;
  }
}

int kaede_sys_write_atomic(const unsigned char *path, size_t path_len,
                           const unsigned char *data, size_t data_len,
                           uint32_t mode, int sync_file, int sync_dir,
                           int replace) {
  unsigned char *owned_path = path_slice_to_c_string(path, path_len);
  if (owned_path == NULL)
    return -1;

  char *parent_path = NULL;
  char *base_name = NULL;
  if (split_parent_and_basename(owned_path, &parent_path, &base_name) < 0) {
    free(owned_path);
    return -1;
  }

  int dir_fd = open(parent_path, O_RDONLY | O_DIRECTORY);
  if (dir_fd < 0) {
    free(owned_path);
    return -1;
  }

  char temp_name[256];
  int fd = -1;
  for (unsigned attempt = 0; attempt < 1000; ++attempt) {
    int written = snprintf(temp_name, sizeof(temp_name), ".%s.tmp.%ld-%u",
                           base_name, (long)getpid(), attempt);
    if (written < 0 || (size_t)written >= sizeof(temp_name)) {
      retry_close_preserve_errno(dir_fd);
      free(owned_path);
      errno = ENAMETOOLONG;
      return -1;
    }

    fd = openat(dir_fd, temp_name, O_WRONLY | O_CREAT | O_EXCL, (mode_t)mode);
    if (fd >= 0)
      break;
    if (errno == EINTR) {
      --attempt;
      continue;
    }
    if (errno == EEXIST)
      continue;

    int saved_errno = errno;
    retry_close_preserve_errno(dir_fd);
    free(owned_path);
    errno = saved_errno;
    return -1;
  }

  if (fd < 0) {
    int saved_errno = errno;
    retry_close_preserve_errno(dir_fd);
    free(owned_path);
    errno = saved_errno;
    return -1;
  }

  if (write_all_blocking(fd, data, data_len) < 0) {
    int saved_errno = errno;
    retry_close_preserve_errno(fd);
    unlinkat(dir_fd, temp_name, 0);
    retry_close_preserve_errno(dir_fd);
    free(owned_path);
    errno = saved_errno;
    return -1;
  }

  if (sync_file && kaede_sys_fsync((sys_fd_t)fd) < 0) {
    int saved_errno = errno;
    retry_close_preserve_errno(fd);
    unlinkat(dir_fd, temp_name, 0);
    retry_close_preserve_errno(dir_fd);
    free(owned_path);
    errno = saved_errno;
    return -1;
  }

  if (close(fd) < 0) {
    int saved_errno = errno;
    unlinkat(dir_fd, temp_name, 0);
    retry_close_preserve_errno(dir_fd);
    free(owned_path);
    errno = saved_errno;
    return -1;
  }

  if (replace) {
    if (renameat(dir_fd, temp_name, dir_fd, base_name) < 0) {
      int saved_errno = errno;
      unlinkat(dir_fd, temp_name, 0);
      retry_close_preserve_errno(dir_fd);
      free(owned_path);
      errno = saved_errno;
      return -1;
    }
  } else {
    if (linkat(dir_fd, temp_name, dir_fd, base_name, 0) < 0) {
      int saved_errno = errno;
      unlinkat(dir_fd, temp_name, 0);
      retry_close_preserve_errno(dir_fd);
      free(owned_path);
      errno = saved_errno;
      return -1;
    }
    if (unlinkat(dir_fd, temp_name, 0) < 0) {
      int saved_errno = errno;
      retry_close_preserve_errno(dir_fd);
      free(owned_path);
      errno = saved_errno;
      return -1;
    }
  }

  if (sync_dir_if_requested(dir_fd, sync_dir) < 0) {
    int saved_errno = errno;
    retry_close_preserve_errno(dir_fd);
    free(owned_path);
    errno = saved_errno;
    return -1;
  }

  if (close(dir_fd) < 0) {
    int saved_errno = errno;
    free(owned_path);
    errno = saved_errno;
    return -1;
  }

  free(owned_path);
  return 0;
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

int kaede_strcmp(const unsigned char *s1, size_t len1, const unsigned char *s2,
                 size_t len2) {
  if (len1 != len2)
    return 1;

  return memcmp(s1, s2, len1);
}

static uint32_t kaede_utf8_decode_one(const unsigned char *s, uint64_t len,
                                      uint64_t offset, uint64_t *width_out) {
  // Out-of-bounds means "no character here". Callers use width==0 to detect
  // that they reached the end of the byte slice.
  if (offset >= len) {
    *width_out = 0;
    return 0;
  }

  unsigned char first = s[offset];
  // Fast path for ASCII: UTF-8 encodes U+0000..U+007F as a single byte.
  if ((first & 0x80u) == 0) {
    *width_out = 1;
    return first;
  }

  // 110xxxxx 10xxxxxx
  // Rejects truncated sequences, non-continuation trailing bytes, and
  // overlong encodings such as representing ASCII with 2 bytes.
  if ((first & 0xE0u) == 0xC0u && offset + 1 < len) {
    unsigned char b1 = s[offset + 1];
    if ((b1 & 0xC0u) == 0x80u) {
      uint32_t code = ((uint32_t)(first & 0x1Fu) << 6) | (uint32_t)(b1 & 0x3Fu);
      if (code >= 0x80u) {
        *width_out = 2;
        return code;
      }
    }
  // 1110xxxx 10xxxxxx 10xxxxxx
  // Rejects overlong encodings and UTF-16 surrogate code points, which are
  // not valid Unicode scalar values.
  } else if ((first & 0xF0u) == 0xE0u && offset + 2 < len) {
    unsigned char b1 = s[offset + 1];
    unsigned char b2 = s[offset + 2];
    if ((b1 & 0xC0u) == 0x80u && (b2 & 0xC0u) == 0x80u) {
      uint32_t code = ((uint32_t)(first & 0x0Fu) << 12) |
                      ((uint32_t)(b1 & 0x3Fu) << 6) | (uint32_t)(b2 & 0x3Fu);
      if (code >= 0x800u && !(code >= 0xD800u && code <= 0xDFFFu)) {
        *width_out = 3;
        return code;
      }
    }
  // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
  // Rejects overlong encodings and code points past Unicode's upper bound.
  } else if ((first & 0xF8u) == 0xF0u && offset + 3 < len) {
    unsigned char b1 = s[offset + 1];
    unsigned char b2 = s[offset + 2];
    unsigned char b3 = s[offset + 3];
    if ((b1 & 0xC0u) == 0x80u && (b2 & 0xC0u) == 0x80u &&
        (b3 & 0xC0u) == 0x80u) {
      uint32_t code = ((uint32_t)(first & 0x07u) << 18) |
                      ((uint32_t)(b1 & 0x3Fu) << 12) |
                      ((uint32_t)(b2 & 0x3Fu) << 6) | (uint32_t)(b3 & 0x3Fu);
      if (code >= 0x10000u && code <= 0x10FFFFu) {
        *width_out = 4;
        return code;
      }
    }
  }

  // Invalid UTF-8 is surfaced as U+FFFD and consumes one byte so callers
  // always make forward progress instead of looping forever on bad input.
  *width_out = 1;
  return 0xFFFDu;
}

uint64_t kaede_utf8_char_count(const unsigned char *s, uint64_t len) {
  const unsigned char *bytes = s;
  uint64_t offset = 0;
  uint64_t count = 0;

  // Count Unicode scalar values, not bytes. Invalid sequences count as one
  // replacement character because decode_one consumes one byte on failure.
  while (offset < len) {
    uint64_t width = 0;
    kaede_utf8_decode_one(bytes, len, offset, &width);
    offset += width == 0 ? 1 : width;
    count += 1;
  }

  return count;
}

uint64_t kaede_utf8_byte_index_of_char(const unsigned char *s, uint64_t len,
                                       uint64_t index) {
  const unsigned char *bytes = s;
  uint64_t offset = 0;
  uint64_t current = 0;

  // Walk character-by-character until we reach the requested character index,
  // then return the corresponding byte offset into the UTF-8 buffer.
  while (offset < len && current < index) {
    uint64_t width = 0;
    kaede_utf8_decode_one(bytes, len, offset, &width);
    offset += width == 0 ? 1 : width;
    current += 1;
  }

  return offset;
}

uint32_t kaede_utf8_char_at(const unsigned char *s, uint64_t len,
                            uint64_t index) {
  const unsigned char *bytes = s;
  // Convert the character index to a byte offset, then decode exactly one
  // scalar value from that position.
  uint64_t offset = kaede_utf8_byte_index_of_char(s, len, index);
  uint64_t width = 0;

  if (offset >= len)
    return 0;

  return kaede_utf8_decode_one(bytes, len, offset, &width);
}

int kaede_sys_somaxconn(void) {
#ifdef SOMAXCONN
  return SOMAXCONN;
#else
  return 128;
#endif
}

uint64_t kaede_f64_to_string(double value, unsigned char *buf, uint64_t cap) {
  if (cap == 0)
    return 0;
  int n = snprintf((char *)buf, (size_t)cap, "%g", value);
  if (n < 0)
    return 0;
  if ((uint64_t)n >= cap)
    return cap - 1;
  return (uint64_t)n;
}
