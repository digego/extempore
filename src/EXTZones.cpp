#include <EXTZones.h>
#include <BranchPrediction.h>

#include <cstdlib>

// TODO / NOMERGE: delete this from EXTLLVM.cpp
#define DEBUG_ZONE_ALLOC 0

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

} // namespace EXTLLVM
} // namespace extemp
