#include <gc/gc.h>
#include <kaede/channel.h>
#include <kaede/task.h>
#include <kaede/worker.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct TaskWaitQueue {
    struct Task *head;
    struct Task *tail;
};

struct KaedeChannel {
    size_t elem_size;
    size_t capacity;
    bool closed;
    uint8_t *buffer;
    size_t len;
    size_t head;
    size_t tail;
    struct TaskWaitQueue send_waiters;
    struct TaskWaitQueue recv_waiters;
};

static void wait_queue_push(struct TaskWaitQueue *queue, struct Task *task) {
    task->channel_wait.next = NULL;
    if (queue->tail) {
        queue->tail->channel_wait.next = task;
    } else {
        queue->head = task;
    }
    queue->tail = task;
}

static struct Task *wait_queue_pop(struct TaskWaitQueue *queue) {
    struct Task *task = queue->head;
    if (!task) {
        return NULL;
    }

    queue->head = task->channel_wait.next;
    if (!queue->head) {
        queue->tail = NULL;
    }
    task->channel_wait.next = NULL;
    return task;
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

static bool wake_waiting_receiver_locked(struct KaedeChannel *channel,
                                         const void *value) {
    struct Task *receiver = wait_queue_pop(&channel->recv_waiters);
    if (!receiver) {
        return false;
    }

    copy_value(receiver->channel_wait.value_slot, value, channel->elem_size);
    if (!worker_wake_task_locked(receiver, true)) {
        abort();
    }
    return true;
}

static bool wake_waiting_sender_direct_locked(struct KaedeChannel *channel,
                                              void *out) {
    struct Task *sender = wait_queue_pop(&channel->send_waiters);
    if (!sender) {
        return false;
    }

    copy_value(out, sender->channel_wait.value_slot, channel->elem_size);
    if (!worker_wake_task_locked(sender, true)) {
        abort();
    }
    return true;
}

static bool buffer_one_waiting_sender_locked(struct KaedeChannel *channel) {
    if (channel->capacity == 0 || channel->len >= channel->capacity) {
        return false;
    }

    struct Task *sender = wait_queue_pop(&channel->send_waiters);
    if (!sender) {
        return false;
    }

    buffer_push(channel, sender->channel_wait.value_slot);
    if (!worker_wake_task_locked(sender, true)) {
        abort();
    }
    return true;
}

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

    return KAEDE_CHANNEL_SEND_CLOSED + 1;
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

    if (result != KAEDE_CHANNEL_SEND_CLOSED + 1) {
        worker_scheduler_unlock();
        return KAEDE_CHANNEL_SEND_CLOSED;
    }

    struct Task *task = worker_current_task();
    if (!task) {
        worker_scheduler_unlock();
        return KAEDE_CHANNEL_SEND_CLOSED;
    }

    wait_queue_push(&channel->send_waiters, task);
    if (!worker_park_current_on_channel_locked(
            channel, KAEDE_TASK_WAIT_CHANNEL_SEND, value)) {
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

    wait_queue_push(&channel->recv_waiters, task);
    if (!worker_park_current_on_channel_locked(
            channel, KAEDE_TASK_WAIT_CHANNEL_RECV, out)) {
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

    struct Task *task = NULL;
    while ((task = wait_queue_pop(&channel->send_waiters)) != NULL) {
        if (!worker_wake_task_locked(task, false)) {
            abort();
        }
    }
    while ((task = wait_queue_pop(&channel->recv_waiters)) != NULL) {
        if (!worker_wake_task_locked(task, false)) {
            abort();
        }
    }

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
