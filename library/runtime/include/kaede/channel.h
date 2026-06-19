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

enum KaedeSelectOp {
    KAEDE_SELECT_OP_SEND = 0,
    KAEDE_SELECT_OP_RECV = 1,
};

enum KaedeSelectStatus {
    KAEDE_SELECT_STATUS_VALUE = 0,   // recv: value received
    KAEDE_SELECT_STATUS_CLOSED = 1,  // recv: channel was closed
    KAEDE_SELECT_STATUS_SENT = 2,    // send: value sent
};

// Returned by kaede_select when has_default is true and no case is ready.
#define KAEDE_SELECT_DEFAULT_INDEX (-1)

struct KaedeChannel;

// Layout must match the LLVM struct emitted by codegen for select cases.
struct KaedeSelectCase {
    struct KaedeChannel *channel;
    uint32_t op;        // KaedeSelectOp
    uint32_t _pad;      // explicit padding to keep ABI deterministic
    void *value_slot;   // send: pointer to value; recv: pointer to out slot
    int32_t status;     // out: meaningful for the chosen case only
};

struct KaedeChannel *kaede_channel_new(size_t elem_size, size_t capacity);
int32_t kaede_channel_send(struct KaedeChannel *channel, void *value);
int32_t kaede_channel_try_send(struct KaedeChannel *channel, void *value);
int32_t kaede_channel_recv(struct KaedeChannel *channel, void *out);
int32_t kaede_channel_try_recv(struct KaedeChannel *channel, void *out);
void kaede_channel_close(struct KaedeChannel *channel);
bool kaede_channel_is_closed(struct KaedeChannel *channel);

int32_t kaede_select(struct KaedeSelectCase *cases, size_t n, bool has_default);

#endif // KAEDE_CHANNEL_H
