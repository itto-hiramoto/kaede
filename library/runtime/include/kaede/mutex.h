#ifndef KAEDE_MUTEX_H
#define KAEDE_MUTEX_H

typedef struct KaedeMutex KaedeMutex;

// Create a new Mutex (GC-allocated)
KaedeMutex *kaede_mutex_new(void);

// Acquire the lock (blocks until available)
void kaede_mutex_lock(KaedeMutex *m);

// Release the lock
void kaede_mutex_unlock(KaedeMutex *m);

#endif // KAEDE_MUTEX_H
