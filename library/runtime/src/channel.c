#include <gc/gc.h>
#include <kaede/channel.h>
#include <kaede/task.h>
#include <kaede/worker.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// -----------------------------------------------------------------------------
// Generic channel waiter (regular send/recv and select cases share one queue).
// -----------------------------------------------------------------------------

enum ChannelWaiterKind {
    CW_REGULAR = 0,
    CW_SELECT = 1,
};

struct KaedeSelectState;
struct KaedeChannel;

struct ChannelWaiter {
    int kind;                      // ChannelWaiterKind
    int op;                        // KaedeSelectOp (SEND/RECV)
    struct ChannelWaiter *prev;
    struct ChannelWaiter *next;
    struct KaedeChannel *channel;  // queue this waiter is parked on (or NULL once removed)
    struct Task *task;             // task to wake; for select, == state->task
    void *value_slot;              // send: source; recv: destination
    int32_t recv_status;           // for regular recv: VALUE/CLOSED
    struct KaedeSelectState *state;  // NULL for regular waiters
    uint32_t case_index;             // for select waiters
};

struct TaskWaitQueue {
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
    struct TaskWaitQueue send_waiters;
    struct TaskWaitQueue recv_waiters;
};

struct KaedeSelectState {
    bool done;
    int32_t chosen_index;
    int32_t chosen_status;
    struct Task *task;
};

static void wait_queue_push_tail(struct TaskWaitQueue *q,
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

static struct ChannelWaiter *wait_queue_pop_head(struct TaskWaitQueue *q) {
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

static void wait_queue_unlink(struct TaskWaitQueue *q,
                              struct ChannelWaiter *w) {
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

// -----------------------------------------------------------------------------
// Buffer helpers
// -----------------------------------------------------------------------------

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

// -----------------------------------------------------------------------------
// Wake helpers: pop one waiter and complete it. For select waiters that have
// already been claimed by another case, skip and continue popping.
// -----------------------------------------------------------------------------

// Pop the next waiter still eligible for waking (regular, or select whose
// shared state isn't `done`). Returns NULL if the queue is exhausted.
static struct ChannelWaiter *pop_live_waiter(struct TaskWaitQueue *queue) {
    for (;;) {
        struct ChannelWaiter *w = wait_queue_pop_head(queue);
        if (!w) {
            return NULL;
        }
        if (w->kind == CW_SELECT && w->state && w->state->done) {
            // Already fired via another case; drop and continue.
            continue;
        }
        return w;
    }
}

// Wake `waiter` after copying `value` (of `elem_size` bytes) into its slot.
// For select waiters, also marks the shared state as done with VALUE status.
static void complete_recv_waiter_with_value(struct ChannelWaiter *waiter,
                                            const void *value,
                                            size_t elem_size) {
    copy_value(waiter->value_slot, value, elem_size);
    if (waiter->kind == CW_SELECT) {
        waiter->state->done = true;
        waiter->state->chosen_index = (int32_t)waiter->case_index;
        waiter->state->chosen_status = KAEDE_SELECT_STATUS_VALUE;
    } else {
        waiter->recv_status = KAEDE_SELECT_STATUS_VALUE;
    }
    if (!worker_wake_task_locked(waiter->task, true)) {
        abort();
    }
}

// Wake a parked sender by copying its value into `out` and marking it sent.
static void complete_send_waiter_into(struct ChannelWaiter *waiter, void *out,
                                      size_t elem_size) {
    copy_value(out, waiter->value_slot, elem_size);
    if (waiter->kind == CW_SELECT) {
        waiter->state->done = true;
        waiter->state->chosen_index = (int32_t)waiter->case_index;
        waiter->state->chosen_status = KAEDE_SELECT_STATUS_SENT;
    }
    if (!worker_wake_task_locked(waiter->task, true)) {
        abort();
    }
}

// Wake a parked sender by transferring its value into the channel buffer.
static void complete_send_waiter_into_buffer(struct ChannelWaiter *waiter,
                                             struct KaedeChannel *channel) {
    buffer_push(channel, waiter->value_slot);
    if (waiter->kind == CW_SELECT) {
        waiter->state->done = true;
        waiter->state->chosen_index = (int32_t)waiter->case_index;
        waiter->state->chosen_status = KAEDE_SELECT_STATUS_SENT;
    }
    if (!worker_wake_task_locked(waiter->task, true)) {
        abort();
    }
}

static bool wake_waiting_receiver_locked(struct KaedeChannel *channel,
                                         const void *value) {
    struct ChannelWaiter *receiver = pop_live_waiter(&channel->recv_waiters);
    if (!receiver) {
        return false;
    }
    complete_recv_waiter_with_value(receiver, value, channel->elem_size);
    return true;
}

static bool wake_waiting_sender_direct_locked(struct KaedeChannel *channel,
                                              void *out) {
    struct ChannelWaiter *sender = pop_live_waiter(&channel->send_waiters);
    if (!sender) {
        return false;
    }
    complete_send_waiter_into(sender, out, channel->elem_size);
    return true;
}

static bool buffer_one_waiting_sender_locked(struct KaedeChannel *channel) {
    if (channel->capacity == 0 || channel->len >= channel->capacity) {
        return false;
    }
    struct ChannelWaiter *sender = pop_live_waiter(&channel->send_waiters);
    if (!sender) {
        return false;
    }
    complete_send_waiter_into_buffer(sender, channel);
    return true;
}

// -----------------------------------------------------------------------------
// Non-blocking attempt primitives (used by both regular ops and select Phase A)
// -----------------------------------------------------------------------------

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

    return KAEDE_CHANNEL_SEND_CLOSED + 1; // sentinel: would block
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

// -----------------------------------------------------------------------------
// Channel allocation and core API
// -----------------------------------------------------------------------------

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

    struct ChannelWaiter waiter = {0};
    waiter.kind = CW_REGULAR;
    waiter.op = KAEDE_SELECT_OP_SEND;
    waiter.task = task;
    waiter.value_slot = value;
    wait_queue_push_tail(&channel->send_waiters, &waiter, channel);

    if (!worker_park_current_on_channel_locked()) {
        // park failed (shutdown): waiter may still be linked
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
    waiter.kind = CW_REGULAR;
    waiter.op = KAEDE_SELECT_OP_RECV;
    waiter.task = task;
    waiter.value_slot = out;
    waiter.recv_status = KAEDE_CHANNEL_RECV_CLOSED;
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

    // Wake all parked senders as failed (closed).
    struct ChannelWaiter *w = NULL;
    while ((w = wait_queue_pop_head(&channel->send_waiters)) != NULL) {
        if (w->kind == CW_SELECT) {
            if (w->state && !w->state->done) {
                w->state->done = true;
                w->state->chosen_index = (int32_t)w->case_index;
                w->state->chosen_status = KAEDE_SELECT_STATUS_CLOSED;
                if (!worker_wake_task_locked(w->task, true)) {
                    abort();
                }
            }
            // already-done select waiters: nothing to do.
        } else {
            if (!worker_wake_task_locked(w->task, false)) {
                abort();
            }
        }
    }
    // Wake all parked receivers; for select recv cases this fires the case
    // with CLOSED status. For regular recv it triggers RECV_CLOSED return.
    while ((w = wait_queue_pop_head(&channel->recv_waiters)) != NULL) {
        if (w->kind == CW_SELECT) {
            if (w->state && !w->state->done) {
                w->state->done = true;
                w->state->chosen_index = (int32_t)w->case_index;
                w->state->chosen_status = KAEDE_SELECT_STATUS_CLOSED;
                if (!worker_wake_task_locked(w->task, true)) {
                    abort();
                }
            }
        } else {
            if (!worker_wake_task_locked(w->task, false)) {
                abort();
            }
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

// -----------------------------------------------------------------------------
// select implementation
// -----------------------------------------------------------------------------

// Try to satisfy a single case immediately (must hold scheduler lock).
// Returns true if the case fired; updates the case's `status` field on success.
static bool try_select_case_locked(struct KaedeSelectCase *cs) {
    struct KaedeChannel *channel = cs->channel;
    if (!channel) {
        return false;
    }

    if (cs->op == KAEDE_SELECT_OP_SEND) {
        int32_t r = try_send_locked(channel, cs->value_slot);
        if (r == KAEDE_CHANNEL_SEND_OK) {
            cs->status = KAEDE_SELECT_STATUS_SENT;
            return true;
        }
        // Closed sends in select fire with CLOSED status (so the caller can
        // observe and panic if appropriate, mirroring Go where send on closed
        // panics even in select).
        if (r == KAEDE_CHANNEL_SEND_CLOSED) {
            cs->status = KAEDE_SELECT_STATUS_CLOSED;
            return true;
        }
        return false;
    }

    if (cs->op == KAEDE_SELECT_OP_RECV) {
        int32_t r = try_recv_locked(channel, cs->value_slot);
        if (r == KAEDE_CHANNEL_RECV_VALUE) {
            cs->status = KAEDE_SELECT_STATUS_VALUE;
            return true;
        }
        if (r == KAEDE_CHANNEL_RECV_CLOSED) {
            cs->status = KAEDE_SELECT_STATUS_CLOSED;
            return true;
        }
        return false;
    }

    return false;
}

// Fisher-Yates shuffle on an index array using rand(). The runtime does not
// seed rand() explicitly; tests that depend on randomness call srand().
static void shuffle_indices(uint32_t *idx, size_t n) {
    for (size_t i = n; i > 1; --i) {
        size_t j = (size_t)((unsigned)rand() % (unsigned)i);
        uint32_t tmp = idx[i - 1];
        idx[i - 1] = idx[j];
        idx[j] = tmp;
    }
}

int32_t kaede_select(struct KaedeSelectCase *cases, size_t n, bool has_default) {
    if (n == 0) {
        // Empty select: no cases. With default, return default; without it,
        // there is nothing to wait on — this is treated like Go's `select {}`
        // (blocks forever). Approximate by parking, which the parser should
        // make unreachable.
        if (has_default) {
            return KAEDE_SELECT_DEFAULT_INDEX;
        }
        worker_scheduler_lock();
        (void)worker_park_current_on_channel_locked();
        return KAEDE_SELECT_DEFAULT_INDEX;
    }

    // Stack-allocated shuffled index order for fairness.
    uint32_t order_buf[32];
    uint32_t *order = order_buf;
    uint32_t *order_heap = NULL;
    if (n > sizeof(order_buf) / sizeof(order_buf[0])) {
        order_heap = malloc(n * sizeof(uint32_t));
        if (!order_heap) {
            return KAEDE_SELECT_DEFAULT_INDEX;
        }
        order = order_heap;
    }
    for (size_t i = 0; i < n; ++i) {
        order[i] = (uint32_t)i;
    }
    shuffle_indices(order, n);

    worker_scheduler_lock();

    // Phase A: try every case once in randomized order.
    for (size_t i = 0; i < n; ++i) {
        struct KaedeSelectCase *cs = &cases[order[i]];
        if (try_select_case_locked(cs)) {
            int32_t chosen = (int32_t)order[i];
            worker_scheduler_unlock();
            free(order_heap);
            return chosen;
        }
    }

    if (has_default) {
        worker_scheduler_unlock();
        free(order_heap);
        return KAEDE_SELECT_DEFAULT_INDEX;
    }

    // Phase B: nothing was ready and no default — register waiters and park.
    struct Task *task = worker_current_task();
    if (!task) {
        worker_scheduler_unlock();
        free(order_heap);
        return KAEDE_SELECT_DEFAULT_INDEX;
    }

    struct KaedeSelectState state = {0};
    state.done = false;
    state.chosen_index = -1;
    state.chosen_status = 0;
    state.task = task;

    struct ChannelWaiter waiters_buf[32];
    struct ChannelWaiter *waiters = waiters_buf;
    struct ChannelWaiter *waiters_heap = NULL;
    if (n > sizeof(waiters_buf) / sizeof(waiters_buf[0])) {
        waiters_heap = calloc(n, sizeof(struct ChannelWaiter));
        if (!waiters_heap) {
            worker_scheduler_unlock();
            free(order_heap);
            return KAEDE_SELECT_DEFAULT_INDEX;
        }
        waiters = waiters_heap;
    } else {
        memset(waiters_buf, 0, sizeof(waiters_buf));
    }

    for (size_t i = 0; i < n; ++i) {
        struct ChannelWaiter *w = &waiters[i];
        w->kind = CW_SELECT;
        w->op = (int)cases[i].op;
        w->task = task;
        w->value_slot = cases[i].value_slot;
        w->state = &state;
        w->case_index = (uint32_t)i;
        struct KaedeChannel *ch = cases[i].channel;
        if (!ch) {
            continue;
        }
        struct TaskWaitQueue *q = (cases[i].op == KAEDE_SELECT_OP_SEND)
                                      ? &ch->send_waiters
                                      : &ch->recv_waiters;
        wait_queue_push_tail(q, w, ch);
    }

    if (!worker_park_current_on_channel_locked()) {
        // Park failed (shutdown). Re-acquire lock to scrub residual waiters.
        worker_scheduler_lock();
        for (size_t i = 0; i < n; ++i) {
            struct ChannelWaiter *w = &waiters[i];
            if (w->channel) {
                struct TaskWaitQueue *q = (w->op == KAEDE_SELECT_OP_SEND)
                                              ? &w->channel->send_waiters
                                              : &w->channel->recv_waiters;
                wait_queue_unlink(q, w);
            }
        }
        worker_scheduler_unlock();
        free(waiters_heap);
        free(order_heap);
        return KAEDE_SELECT_DEFAULT_INDEX;
    }

    // We woke; re-acquire scheduler lock to remove any leftover waiters that
    // were not the firing case.
    worker_scheduler_lock();
    for (size_t i = 0; i < n; ++i) {
        struct ChannelWaiter *w = &waiters[i];
        if (w->channel) {
            struct TaskWaitQueue *q = (w->op == KAEDE_SELECT_OP_SEND)
                                          ? &w->channel->send_waiters
                                          : &w->channel->recv_waiters;
            wait_queue_unlink(q, w);
        }
    }
    worker_scheduler_unlock();

    int32_t chosen = state.chosen_index;
    if (chosen >= 0 && (size_t)chosen < n) {
        cases[chosen].status = state.chosen_status;
    }

    free(waiters_heap);
    free(order_heap);
    return chosen;
}

// -----------------------------------------------------------------------------
// void* shims for Kaede FFI
// -----------------------------------------------------------------------------

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
