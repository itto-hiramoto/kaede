#include <gc/gc.h>
#include <kaede/channel.h>
#include <kaede/task.h>
#include <kaede/worker.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// -----------------------------------------------------------------------------
// Channel waiters
//
// A parked send/recv is represented by a waiter that the channel owns, linked
// into one of the channel's queues. The waiter lives on the parked task's own
// stack frame, so it stays valid until that task resumes.
//
// The list is doubly linked and each waiter keeps a back-pointer to the queue's
// channel, so a waiter can be removed without walking the list.
// -----------------------------------------------------------------------------

struct KaedeChannel;

struct ChannelWaiter {
    struct ChannelWaiter *prev;
    struct ChannelWaiter *next;
    struct KaedeChannel *channel;  // queue this waiter is on, NULL once removed
    struct Task *task;             // task to wake
    void *value_slot;              // send: source; recv: destination
};

struct ChannelWaitQueue {
    struct ChannelWaiter *head;
    struct ChannelWaiter *tail;
};

struct KaedeChannel {
    size_t elem_size;
    size_t capacity;
    bool closed;
    uint8_t *buffer;
    size_t len;
    size_t head;
    size_t tail;
    struct ChannelWaitQueue send_waiters;
    struct ChannelWaitQueue recv_waiters;
};

static void wait_queue_push_tail(struct ChannelWaitQueue *q,
                                 struct ChannelWaiter *w,
                                 struct KaedeChannel *channel) {
    w->prev = q->tail;
    w->next = NULL;
    w->channel = channel;
    if (q->tail) {
        q->tail->next = w;
    } else {
        q->head = w;
    }
    q->tail = w;
}

static struct ChannelWaiter *wait_queue_pop_head(struct ChannelWaitQueue *q) {
    struct ChannelWaiter *w = q->head;
    if (!w) {
        return NULL;
    }
    q->head = w->next;
    if (q->head) {
        q->head->prev = NULL;
    } else {
        q->tail = NULL;
    }
    w->prev = NULL;
    w->next = NULL;
    w->channel = NULL;
    return w;
}

static void wait_queue_unlink(struct ChannelWaitQueue *q, struct ChannelWaiter *w) {
    if (w->prev) {
        w->prev->next = w->next;
    } else if (q->head == w) {
        q->head = w->next;
    }
    if (w->next) {
        w->next->prev = w->prev;
    } else if (q->tail == w) {
        q->tail = w->prev;
    }
    w->prev = NULL;
    w->next = NULL;
    w->channel = NULL;
}

static uint8_t *buffer_slot(struct KaedeChannel *channel, size_t index) {
    if (!channel->buffer || channel->elem_size == 0) {
        return channel->buffer;
    }
    return channel->buffer + (index * channel->elem_size);
}

static void copy_value(void *dst, const void *src, size_t len) {
    if (len == 0) {
        return;
    }
    memcpy(dst, src, len);
}

static void buffer_push(struct KaedeChannel *channel, const void *value) {
    copy_value(buffer_slot(channel, channel->tail), value, channel->elem_size);
    channel->tail = (channel->tail + 1) % channel->capacity;
    channel->len++;
}

static void buffer_pop(struct KaedeChannel *channel, void *out) {
    copy_value(out, buffer_slot(channel, channel->head), channel->elem_size);
    channel->head = (channel->head + 1) % channel->capacity;
    channel->len--;
}

// Wake a waiter that completed successfully. The caller must have already done
// any value transfer the woken task expects to find in its slot.
static void wake_waiter_locked(struct ChannelWaiter *waiter) {
    if (!worker_wake_task_locked(waiter->task, true)) {
        abort();
    }
}

static bool wake_waiting_receiver_locked(struct KaedeChannel *channel,
                                         const void *value) {
    struct ChannelWaiter *receiver = wait_queue_pop_head(&channel->recv_waiters);
    if (!receiver) {
        return false;
    }

    copy_value(receiver->value_slot, value, channel->elem_size);
    wake_waiter_locked(receiver);
    return true;
}

static bool wake_waiting_sender_direct_locked(struct KaedeChannel *channel,
                                              void *out) {
    struct ChannelWaiter *sender = wait_queue_pop_head(&channel->send_waiters);
    if (!sender) {
        return false;
    }

    copy_value(out, sender->value_slot, channel->elem_size);
    wake_waiter_locked(sender);
    return true;
}

static bool buffer_one_waiting_sender_locked(struct KaedeChannel *channel) {
    if (channel->capacity == 0 || channel->len >= channel->capacity) {
        return false;
    }

    struct ChannelWaiter *sender = wait_queue_pop_head(&channel->send_waiters);
    if (!sender) {
        return false;
    }

    buffer_push(channel, sender->value_slot);
    wake_waiter_locked(sender);
    return true;
}

// Drain `queue`, waking every waiter with `wake_success = false` so the parked
// send/recv reports the channel as closed.
static void wake_all_as_closed_locked(struct ChannelWaitQueue *queue) {
    struct ChannelWaiter *w;
    while ((w = wait_queue_pop_head(queue)) != NULL) {
        if (!worker_wake_task_locked(w->task, false)) {
            abort();
        }
    }
}

// Internal sentinel used only by try_send_locked, distinct from the public
// KaedeChannelSendResult values. Callers translate it to a parking decision.
#define TRY_SEND_WOULD_BLOCK 2

static int32_t try_send_locked(struct KaedeChannel *channel, void *value) {
    if (channel->closed || worker_shutdown_requested_locked()) {
        return KAEDE_CHANNEL_SEND_CLOSED;
    }

    if (wake_waiting_receiver_locked(channel, value)) {
        return KAEDE_CHANNEL_SEND_OK;
    }

    if (channel->capacity > 0 && channel->len < channel->capacity) {
        buffer_push(channel, value);
        return KAEDE_CHANNEL_SEND_OK;
    }

    return TRY_SEND_WOULD_BLOCK;
}

static int32_t try_recv_locked(struct KaedeChannel *channel, void *out) {
    if (channel->len > 0) {
        buffer_pop(channel, out);
        if (!channel->closed) {
            (void)buffer_one_waiting_sender_locked(channel);
        }
        return KAEDE_CHANNEL_RECV_VALUE;
    }

    if (wake_waiting_sender_direct_locked(channel, out)) {
        return KAEDE_CHANNEL_RECV_VALUE;
    }

    if (channel->closed || worker_shutdown_requested_locked()) {
        return KAEDE_CHANNEL_RECV_CLOSED;
    }

    return KAEDE_CHANNEL_RECV_EMPTY;
}

struct KaedeChannel *kaede_channel_new(size_t elem_size, size_t capacity) {
    struct KaedeChannel *channel = GC_malloc(sizeof(struct KaedeChannel));
    if (!channel) {
        return NULL;
    }

    channel->elem_size = elem_size;
    channel->capacity = capacity;
    channel->closed = false;
    channel->len = 0;
    channel->head = 0;
    channel->tail = 0;
    channel->send_waiters.head = NULL;
    channel->send_waiters.tail = NULL;
    channel->recv_waiters.head = NULL;
    channel->recv_waiters.tail = NULL;

    if (capacity == 0) {
        channel->buffer = NULL;
        return channel;
    }

    size_t buffer_size = elem_size * capacity;
    if (buffer_size == 0) {
        buffer_size = 1;
    }

    channel->buffer = GC_malloc(buffer_size);
    if (!channel->buffer) {
        return NULL;
    }

    return channel;
}

int32_t kaede_channel_send(struct KaedeChannel *channel, void *value) {
    if (!channel || !value) {
        return KAEDE_CHANNEL_SEND_CLOSED;
    }

    worker_scheduler_lock();
    const int32_t result = try_send_locked(channel, value);
    if (result == KAEDE_CHANNEL_SEND_OK) {
        worker_scheduler_unlock();
        return result;
    }

    if (result != TRY_SEND_WOULD_BLOCK) {
        worker_scheduler_unlock();
        return KAEDE_CHANNEL_SEND_CLOSED;
    }

    struct Task *task = worker_current_task();
    if (!task) {
        worker_scheduler_unlock();
        return KAEDE_CHANNEL_SEND_CLOSED;
    }

    struct ChannelWaiter waiter = {0};
    waiter.task = task;
    waiter.value_slot = value;
    wait_queue_push_tail(&channel->send_waiters, &waiter, channel);

    if (!worker_park_current_on_channel_locked()) {
        // Parking failed, so nothing will ever wake this waiter. Take it back
        // out of the queue before the stack frame holding it goes away.
        worker_scheduler_lock();
        if (waiter.channel) {
            wait_queue_unlink(&channel->send_waiters, &waiter);
        }
        worker_scheduler_unlock();
        return KAEDE_CHANNEL_SEND_CLOSED;
    }

    return task->channel_wait.wake_success ? KAEDE_CHANNEL_SEND_OK
                                           : KAEDE_CHANNEL_SEND_CLOSED;
}

int32_t kaede_channel_try_send(struct KaedeChannel *channel, void *value) {
    if (!channel || !value) {
        return 0;
    }

    worker_scheduler_lock();
    const int32_t result = try_send_locked(channel, value);
    worker_scheduler_unlock();
    return result == KAEDE_CHANNEL_SEND_OK ? 1 : 0;
}

int32_t kaede_channel_recv(struct KaedeChannel *channel, void *out) {
    if (!channel || !out) {
        return KAEDE_CHANNEL_RECV_CLOSED;
    }

    worker_scheduler_lock();
    const int32_t result = try_recv_locked(channel, out);
    if (result != KAEDE_CHANNEL_RECV_EMPTY) {
        worker_scheduler_unlock();
        return result;
    }

    struct Task *task = worker_current_task();
    if (!task) {
        worker_scheduler_unlock();
        return KAEDE_CHANNEL_RECV_EMPTY;
    }

    struct ChannelWaiter waiter = {0};
    waiter.task = task;
    waiter.value_slot = out;
    wait_queue_push_tail(&channel->recv_waiters, &waiter, channel);

    if (!worker_park_current_on_channel_locked()) {
        worker_scheduler_lock();
        if (waiter.channel) {
            wait_queue_unlink(&channel->recv_waiters, &waiter);
        }
        worker_scheduler_unlock();
        return KAEDE_CHANNEL_RECV_CLOSED;
    }

    return task->channel_wait.wake_success ? KAEDE_CHANNEL_RECV_VALUE
                                           : KAEDE_CHANNEL_RECV_CLOSED;
}

int32_t kaede_channel_try_recv(struct KaedeChannel *channel, void *out) {
    if (!channel || !out) {
        return 0;
    }

    worker_scheduler_lock();
    const int32_t result = try_recv_locked(channel, out);
    worker_scheduler_unlock();
    return result == KAEDE_CHANNEL_RECV_VALUE ? 1 : 0;
}

void kaede_channel_close(struct KaedeChannel *channel) {
    if (!channel) {
        return;
    }

    worker_scheduler_lock();
    if (channel->closed) {
        worker_scheduler_unlock();
        return;
    }

    channel->closed = true;

    wake_all_as_closed_locked(&channel->send_waiters);
    wake_all_as_closed_locked(&channel->recv_waiters);

    worker_scheduler_unlock();
}

bool kaede_channel_is_closed(struct KaedeChannel *channel) {
    if (!channel) {
        return true;
    }

    worker_scheduler_lock();
    const bool closed = channel->closed;
    worker_scheduler_unlock();
    return closed;
}

void *kaede_chan_new(size_t elem_size, size_t capacity) {
    return kaede_channel_new(elem_size, capacity);
}

int32_t kaede_chan_send(void *chan, void *value) {
    return kaede_channel_send((struct KaedeChannel *)chan, value);
}

int32_t kaede_chan_try_send(void *chan, void *value) {
    return kaede_channel_try_send((struct KaedeChannel *)chan, value);
}

int32_t kaede_chan_recv(void *chan, void *out) {
    return kaede_channel_recv((struct KaedeChannel *)chan, out);
}

int32_t kaede_chan_try_recv(void *chan, void *out) {
    return kaede_channel_try_recv((struct KaedeChannel *)chan, out);
}

void kaede_chan_close(void *chan) {
    kaede_channel_close((struct KaedeChannel *)chan);
}

int32_t kaede_chan_is_closed(void *chan) {
    return kaede_channel_is_closed((struct KaedeChannel *)chan) ? 1 : 0;
}
