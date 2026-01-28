#ifndef KAEDE_WAITGROUP_H
#define KAEDE_WAITGROUP_H

#include <stdint.h>

typedef struct KaedeWaitGroup KaedeWaitGroup;

// Create a new WaitGroup (GC-allocated)
KaedeWaitGroup *kaede_wg_new(void);

// Add delta to the counter
void kaede_wg_add(KaedeWaitGroup *wg, int32_t delta);

// Decrement counter by 1
void kaede_wg_done(KaedeWaitGroup *wg);

// Block until counter reaches zero
void kaede_wg_wait(KaedeWaitGroup *wg);

#endif // KAEDE_WAITGROUP_H
