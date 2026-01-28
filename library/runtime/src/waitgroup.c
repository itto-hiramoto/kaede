#include <gc/gc.h>
#include <kaede/runtime.h>
#include <kaede/waitgroup.h>
#include <pthread.h>
#include <stdatomic.h>

struct KaedeWaitGroup {
    atomic_int counter;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
};

KaedeWaitGroup *kaede_wg_new(void) {
    KaedeWaitGroup *wg = GC_malloc(sizeof(KaedeWaitGroup));
    atomic_init(&wg->counter, 0);
    pthread_mutex_init(&wg->mutex, NULL);
    pthread_cond_init(&wg->cond, NULL);
    return wg;
}

void kaede_wg_add(KaedeWaitGroup *wg, int32_t delta) {
    int old = atomic_fetch_add(&wg->counter, delta);
    if (old + delta == 0) {
        pthread_mutex_lock(&wg->mutex);
        pthread_cond_broadcast(&wg->cond);
        pthread_mutex_unlock(&wg->mutex);
    }
}

void kaede_wg_done(KaedeWaitGroup *wg) {
    kaede_wg_add(wg, -1);
}

void kaede_wg_wait(KaedeWaitGroup *wg) {
    // Cooperative wait: yield to other tasks while waiting
    while (atomic_load(&wg->counter) > 0) {
        kaede_yield();
    }
}
