#ifndef KAEDE_CHANNEL_H
#define KAEDE_CHANNEL_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

enum KaedeChannelSendResult {
    KAEDE_CHANNEL_SEND_OK = 0,
    KAEDE_CHANNEL_SEND_CLOSED = 1,
};

enum KaedeChannelRecvResult {
    KAEDE_CHANNEL_RECV_VALUE = 0,
    KAEDE_CHANNEL_RECV_EMPTY = 1,
    KAEDE_CHANNEL_RECV_CLOSED = 2,
};

struct KaedeChannel;

struct KaedeChannel *kaede_channel_new(size_t elem_size, size_t capacity);
int32_t kaede_channel_send(struct KaedeChannel *channel, void *value);
int32_t kaede_channel_try_send(struct KaedeChannel *channel, void *value);
int32_t kaede_channel_recv(struct KaedeChannel *channel, void *out);
int32_t kaede_channel_try_recv(struct KaedeChannel *channel, void *out);
void kaede_channel_close(struct KaedeChannel *channel);
bool kaede_channel_is_closed(struct KaedeChannel *channel);

#endif // KAEDE_CHANNEL_H
