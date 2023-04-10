#pragma once

#include "number/int.h"



#ifdef __cplusplus
extern "C" {
#endif


// Allocate chunks of memory up to the engine's page size. Generally, may not
// exceed 2kb. Slow. Intended for the rare instances when we actually need to
// malloc something.
//
typedef enum _skyland_malloc_flag {
    // Placeholder value representing no malloc options.
    smf_none = 0,

    // The allocated value can never be deallocated. Allows more compact
    // storage. Undefined behavior to call skyland_free with something allocated
    // with permanent storage duration.
    smf_permanent = (1 << 0),


} skyland_malloc_flag;
//
void* skyland_malloc(u32 sz, u32 flags);


void skyland_free(void* ptr);


#ifdef __cplusplus
}
#endif
