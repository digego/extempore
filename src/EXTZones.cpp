#include <EXTZones.h>
#include <EXTMutex.h>
#include <BranchPrediction.h>

#include <cstdlib>
#include <cstring>

// TODO / NOMERGE: delete this from EXTLLVM.cpp
#define DEBUG_ZONE_ALLOC 0
#define EXTENSIBLE_ZONES 1
#define LEAKY_ZONES 1

thread_local llvm_zone_stack* tls_llvm_zone_stack = 0;
thread_local uint64_t tls_llvm_zone_stacksize = 0;

namespace extemp {
namespace EXTLLVM {

llvm_zone_t* llvm_zone_create(uint64_t size)
{
    auto zone(reinterpret_cast<llvm_zone_t*>(malloc(sizeof(llvm_zone_t))));
    if (unlikely(!zone)) {
        abort(); // in case a leak can be analyzed post-mortem
    }
#ifdef _WIN32
	if (size == 0) {
		zone->memory = NULL;
	}
	else {
		// this crashes extempore but I have no idea why????
		// zone->memory = _aligned_malloc((size_t)size, (size_t)LLVM_ZONE_ALIGN);
		zone->memory = malloc(size_t(size));
	}
#else
    posix_memalign(&zone->memory, LLVM_ZONE_ALIGN, size_t(size));
#endif
    zone->mark = 0;
    zone->offset = 0;
    if (unlikely(!zone->memory)) {
        size = 0;
    }
    zone->size = size;
    zone->cleanup_hooks = nullptr;
    zone->memories = nullptr;
    return zone;
}

EXPORT void llvm_zone_destroy(llvm_zone_t* Zone)
{
#if DEBUG_ZONE_ALLOC
    printf("DestroyZone: %p:%p:%lld:%lld\n", Zone, Zone->memory, Zone->offset, Zone->size);
#endif
    if (Zone->memories) {
        llvm_zone_destroy(Zone->memories);
    }
    free(Zone->memory);
    free(Zone);
}

// NOMERGE: dropped 'inline' on lots of these functions
llvm_zone_t* llvm_zone_reset(llvm_zone_t* Zone)
{
    Zone->offset = 0;
    return Zone;
}

EXPORT void* llvm_zone_malloc(llvm_zone_t* zone, uint64_t size)
{
    // NOMERGE: changed this
    static extemp::EXTMutex alloc_mutex = []() {
        extemp::EXTMutex m("alloc mutex");
        m.init();
        return m;
    }();
    extemp::EXTMutex::ScopedLock lock(alloc_mutex);
#if DEBUG_ZONE_ALLOC
    printf("MallocZone: %p:%p:%lld:%lld:%lld\n",zone,zone->memory,zone->offset,zone->size,size);
#endif
    size += LLVM_ZONE_ALIGN; // for storing size information
    if (unlikely(zone->offset + size >= zone->size)) {
#if EXTENSIBLE_ZONES // if extensible_zones is true then extend zone size by zone->size
        int old_zone_size = zone->size;
        bool iszero(!zone->size);
        if (size > zone->size) {
            zone->size = size;
        }
        zone->size *= 2; // keep doubling zone size for each new allocation // TODO: 1.5???
        if (zone->size < 1024) {
            zone->size = 1024; // allocate a min size of 1024 bytes
        }
        llvm_zone_t* newzone = llvm_zone_create(zone->size);
        void* tmp = newzone->memory;
        if (iszero) { // if initial zone is 0 - then replace don't extend
          zone->memory = tmp;
          free(newzone);
        } else {
            // printf("adding new memory %p:%lld to existing %p:%lld\n",newzone,newzone->size,zone,zone->size);
            newzone->memories = zone->memories;
            newzone->memory = zone->memory;
            newzone->size = old_zone_size;
            zone->memory = tmp;
            zone->memories = newzone;
        }
        llvm_zone_reset(zone);
#elif LEAKY_ZONES       // if LEAKY ZONE is TRUE then just print a warning and just leak the memory
        printf("\nZone:%p size:%lld is full ... leaking %lld bytes\n",zone,zone->size,size);
        printf("Leaving a leaky zone can be dangerous ... particularly for concurrency\n");
        fflush(NULL);
        return malloc((size_t)size);  // TODO: what about the stored size????
#else
        printf("\nZone:%p size:%lld is full ... exiting!\n",zone,zone->size,size);
        fflush(NULL);
        exit(1);
#endif
    }
    size = (size + LLVM_ZONE_ALIGNPAD) & ~LLVM_ZONE_ALIGNPAD;
    auto newptr = reinterpret_cast<void*>(reinterpret_cast<char*>(zone->memory) + zone->offset);
    memset(newptr, 0, size); // clear memory
    newptr = reinterpret_cast<char*>(newptr) + LLVM_ZONE_ALIGN; // skip past size
    *(reinterpret_cast<uint64_t*>(newptr) - 1) = size;
    zone->offset += size;
    return newptr;
}

llvm_zone_stack* llvm_threads_get_zone_stack()
{
    return tls_llvm_zone_stack;
}

void llvm_threads_set_zone_stack(llvm_zone_stack* Stack)
{
    tls_llvm_zone_stack = Stack;
}

} // namespace EXTLLVM
} // namespace extemp
