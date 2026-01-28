#include <gc/gc.h>
#include <kaede/mutex.h>
#include <pthread.h>

struct KaedeMutex {
    pthread_mutex_t inner;
};

KaedeMutex *kaede_mutex_new(void) {
    KaedeMutex *m = GC_malloc(sizeof(KaedeMutex));
    pthread_mutex_init(&m->inner, NULL);
    return m;
}

void kaede_mutex_lock(KaedeMutex *m) {
    pthread_mutex_lock(&m->inner);
}

void kaede_mutex_unlock(KaedeMutex *m) {
    pthread_mutex_unlock(&m->inner);
}
