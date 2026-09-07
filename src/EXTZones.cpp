#include <EXTZones.h>

#include <cstdio>
#include <cstdlib>
#include <cstring>

#ifdef _WIN32
#include <malloc.h>  // _aligned_malloc / _aligned_free
#endif

constexpr bool DEBUG_ZONE_ALLOC = false;
constexpr bool DEBUG_ZONE_STACK = false;

thread_local llvm_zone_stack* tls_llvm_zone_stack = nullptr;

namespace extemp {
namespace EXTZones {

// Zone memory is allocated aligned to LLVM_ZONE_ALIGN and must be released with
// the matching free. The old Windows path used plain malloc (only 16-byte
// aligned, not the 32 the zones promise) because _aligned_malloc "crashed" --
// the real cause was llvm_zone_destroy below freeing with plain free(), which
// is undefined for _aligned_malloc. Pairing _aligned_malloc with _aligned_free
// fixes both the under-alignment and the crash.
static void* zone_aligned_alloc(size_t size) {
#ifdef _WIN32
    return _aligned_malloc(size, LLVM_ZONE_ALIGN);
#else
    void* ptr = nullptr;
    if (posix_memalign(&ptr, LLVM_ZONE_ALIGN, size) != 0) {
        ptr = nullptr;
    }
    return ptr;
#endif
}

static void zone_aligned_free(void* ptr) {
#ifdef _WIN32
    _aligned_free(ptr);
#else
    free(ptr);
#endif
}

llvm_zone_t* llvm_zone_create(uint64_t size) {
    auto zone(reinterpret_cast<llvm_zone_t*>(malloc(sizeof(llvm_zone_t))));
    if (!zone) [[unlikely]] {
        abort();  // in case a leak can be analyzed post-mortem
    }
    zone->memory = size ? zone_aligned_alloc(size_t(size)) : nullptr;
    zone->mark = 0;
    zone->offset = 0;
    if (!zone->memory) [[unlikely]] {
        size = 0;
    }
    zone->size = size;
    zone->cleanup_hooks = nullptr;
    zone->memories = nullptr;
    return zone;
}

EXPORT void llvm_zone_destroy(llvm_zone_t* Zone) {
    if constexpr (DEBUG_ZONE_ALLOC) {
        printf("DestroyZone: %p:%p:%" PRIu64 ":%" PRIu64 "\n", static_cast<void*>(Zone),
               Zone->memory, Zone->offset, Zone->size);
    }
    if (Zone->memories) {
        llvm_zone_destroy(Zone->memories);
    }
    zone_aligned_free(Zone->memory);
    free(Zone);
}

llvm_zone_t* llvm_zone_reset(llvm_zone_t* Zone) {
    Zone->offset = 0;
    return Zone;
}

// Every allocation is laid out as [size header: LLVM_ZONE_ALIGN bytes][payload],
// rounded up as a whole to a multiple of LLVM_ZONE_ALIGN. The footprint is
// computed once, up front, and both the capacity check and the memset use that
// same number -- checking the unrounded size and then rounding up is how the
// old allocator wrote past the end of a nearly-full zone.
static uint64_t zone_footprint(uint64_t size) {
    // Refuse sizes that would wrap when the header and padding are added.
    if (size > UINT64_MAX - LLVM_ZONE_ALIGN - LLVM_ZONE_ALIGNPAD) [[unlikely]] {
        fprintf(stderr, "\nZone allocation of %" PRIu64 " bytes is not representable ... exiting!\n",
                size);
        fflush(nullptr);
        abort();
    }
    return (size + LLVM_ZONE_ALIGN + LLVM_ZONE_ALIGNPAD) & ~LLVM_ZONE_ALIGNPAD;
}

// Grow `zone` so that at least `footprint` bytes are free. The current memory
// block is pushed onto the zone's `memories` chain (so pointers handed out from
// it stay valid) and a fresh, larger block becomes the active one.
static void zone_extend(llvm_zone_t* zone, uint64_t footprint) {
    const bool iszero = !zone->size;
    uint64_t new_size = zone->size > footprint ? zone->size : footprint;
    if (new_size > UINT64_MAX / 2) [[unlikely]] {
        fprintf(stderr, "\nZone:%p cannot grow to hold %" PRIu64 " bytes ... exiting!\n",
                static_cast<void*>(zone), footprint);
        fflush(nullptr);
        abort();
    }
    new_size *= 2;  // keep doubling zone size for each new allocation
    if (new_size < 1024) {
        new_size = 1024;  // allocate a min size of 1024 bytes
    }
    llvm_zone_t* newzone = llvm_zone_create(new_size);
    if (!newzone->memory) [[unlikely]] {
        free(newzone);
        fprintf(stderr, "\nZone:%p out of memory growing to %" PRIu64 " bytes ... exiting!\n",
                static_cast<void*>(zone), new_size);
        fflush(nullptr);
        abort();
    }
    if (iszero) {  // an empty zone is replaced rather than chained
        zone->memory = newzone->memory;
        free(newzone);
    } else {  // newzone takes over the old block; the fresh block becomes active
        void* fresh = newzone->memory;
        newzone->memories = zone->memories;
        newzone->memory = zone->memory;
        newzone->size = zone->size;
        zone->memory = fresh;
        zone->memories = newzone;
    }
    zone->size = new_size;
    zone->offset = 0;
}

EXPORT void* llvm_zone_malloc(llvm_zone_t* zone, uint64_t size) {
    if constexpr (DEBUG_ZONE_ALLOC) {
        printf("MallocZone: %p:%p:%" PRIu64 ":%" PRIu64 ":%" PRIu64 "\n",
               static_cast<void*>(zone), zone->memory, zone->offset, zone->size, size);
    }
    const uint64_t footprint = zone_footprint(size);
    if (zone->offset + footprint >= zone->size) [[unlikely]] {
        zone_extend(zone, footprint);
    }
    auto block = reinterpret_cast<char*>(zone->memory) + zone->offset;
    memset(block, 0, size_t(footprint));
    auto newptr = block + LLVM_ZONE_ALIGN;  // skip past size header
    *(reinterpret_cast<uint64_t*>(newptr) - 1) = footprint;
    zone->offset += footprint;
    return newptr;
}

llvm_zone_stack* llvm_threads_get_zone_stack() {
    return tls_llvm_zone_stack;
}

void llvm_threads_set_zone_stack(llvm_zone_stack* Stack) {
    tls_llvm_zone_stack = Stack;
}

void llvm_push_zone_stack(llvm_zone_t* Zone) {
    auto stack(reinterpret_cast<llvm_zone_stack*>(malloc(sizeof(llvm_zone_stack))));
    stack->head = Zone;
    stack->tail = llvm_threads_get_zone_stack();
    llvm_threads_set_zone_stack(stack);
}

llvm_zone_t* llvm_peek_zone_stack() {
    llvm_zone_stack* stack = llvm_threads_get_zone_stack();
    if (!stack) [[unlikely]] {  // for the moment create a "DEFAULT" zone if stack is nullptr
        if constexpr (DEBUG_ZONE_STACK) {
            printf("TRYING TO PEEK AT A nullptr ZONE STACK\n");
        }
        llvm_zone_t* z = llvm_zone_create(1024 * 1024 * 1);  // default root zone is 1M
        llvm_push_zone_stack(z);
        if constexpr (DEBUG_ZONE_STACK) {
            printf("Creating new 1M default zone %p:%" PRIu64 " on ZStack:%p\n",
                   static_cast<void*>(z), z->size,
                   static_cast<void*>(llvm_threads_get_zone_stack()));
        }
        return z;
    }
    llvm_zone_t* z = stack->head;
    if constexpr (DEBUG_ZONE_STACK) {
        printf("%p: peeking at zone %p:%" PRIu64 "\n", static_cast<void*>(stack),
               static_cast<void*>(z), z->size);
    }
    return z;
}

EXPORT llvm_zone_t* llvm_pop_zone_stack() {
    auto stack(llvm_threads_get_zone_stack());
    if (!stack) [[unlikely]] {
        if constexpr (DEBUG_ZONE_STACK) {
            printf("TRYING TO POP A ZONE FROM AN EMPTY ZONE STACK\n");
        }
        return nullptr;
    }
    llvm_zone_t* head = stack->head;
    llvm_zone_stack* tail = stack->tail;
    if constexpr (DEBUG_ZONE_STACK) {
        if (!tail) {
            printf("%p: popping zone %p:%" PRIu64 " from stack with no tail\n",
                   static_cast<void*>(stack), static_cast<void*>(head), head->size);
        } else {
            printf("%p: popping new zone %p:%" PRIu64 " back to old zone %p:%" PRIu64 "\n",
                   static_cast<void*>(stack), static_cast<void*>(head), head->size,
                   static_cast<void*>(tail->head), tail->head->size);
        }
    }
    free(stack);
    llvm_threads_set_zone_stack(tail);
    return head;
}

EXPORT void llvm_zone_print(llvm_zone_t* zone) {
    auto tmp(zone);
    auto total_size(zone->size);
    int64_t segments(1);
    while (tmp->memories) {
        tmp = tmp->memories;
        total_size += tmp->size;
        segments++;
    }
    printf("<MemZone(%p) size(%" PRIu64 ") free(%" PRIu64 ") segs(%" PRId64 ")>",
           static_cast<void*>(zone), total_size, (zone->size - zone->offset), segments);
}

EXPORT uint64_t llvm_zone_ptr_size(void* ptr)  // could be inline version in llvm (as well)
{
    return *(reinterpret_cast<uint64_t*>(ptr) - 1);
}

// Returns true on failure (generated code treats a non-zero result as an
// error, see llvm_runtime_error in EXTLLVM.cpp).
EXPORT bool llvm_zone_copy_ptr(void* ptr1, void* ptr2) {
    uint64_t size1 = llvm_zone_ptr_size(ptr1);
    uint64_t size2 = llvm_zone_ptr_size(ptr2);
    if (size1 != size2 || !size1) [[unlikely]] {
        return true;
    }
    std::memcpy(ptr2, ptr1, size_t(size1));
    return false;
}

EXPORT bool llvm_ptr_in_zone(llvm_zone_t* zone, void* ptr) {
    while (zone && (ptr < zone->memory ||
                    ptr >= reinterpret_cast<char*>(zone->memory) + zone->size)) {
        zone = zone->memories;
    }
    return zone != nullptr;
}

EXPORT void* llvm_zone_malloc_from_current_zone(uint64_t size) {
    return llvm_zone_malloc(llvm_peek_zone_stack(), size);
}

EXPORT bool llvm_ptr_in_current_zone(void* ptr) {
    return llvm_ptr_in_zone(llvm_peek_zone_stack(), ptr);
}

EXPORT llvm_zone_t* llvm_peek_zone_stack_extern() {
    return llvm_peek_zone_stack();
}

EXPORT void llvm_push_zone_stack_extern(llvm_zone_t* Zone) {
    llvm_push_zone_stack(Zone);
}

EXPORT llvm_zone_t* llvm_zone_create_extern(uint64_t Size) {
    return llvm_zone_create(Size);
}

static thread_local llvm_zone_t* tls_llvm_callback_zone = nullptr;

static inline llvm_zone_t* llvm_threads_get_callback_zone() {
    if (!tls_llvm_callback_zone) [[unlikely]] {
        tls_llvm_callback_zone = llvm_zone_create(1024 * 1024);  // default callback zone 1M
    }
    return tls_llvm_callback_zone;
}

EXPORT llvm_zone_t* llvm_zone_callback_setup() {
    auto zone(llvm_threads_get_callback_zone());
    llvm_push_zone_stack(zone);
    return llvm_zone_reset(zone);
}

}  // namespace EXTZones
}  // namespace extemp
