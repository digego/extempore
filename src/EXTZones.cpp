#include <EXTZones.h>
#include <EXTMutex.h>
#include <BranchPrediction.h>

#include <cstdlib>
#include <cstring>

#define DEBUG_ZONE_ALLOC 0
#define DEBUG_ZONE_STACK 0
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

void llvm_push_zone_stack(llvm_zone_t* Zone)
{
    auto stack(reinterpret_cast<llvm_zone_stack*>(malloc(sizeof(llvm_zone_stack))));
    stack->head = Zone;
    stack->tail = llvm_threads_get_zone_stack();
    llvm_threads_set_zone_stack(stack);
    return;
}

llvm_zone_t* llvm_peek_zone_stack()
{
    llvm_zone_t* z = 0;
    llvm_zone_stack* stack = llvm_threads_get_zone_stack();
    if (unlikely(!stack)) {  // for the moment create a "DEFAULT" zone if stack is NULL
#if DEBUG_ZONE_STACK
        printf("TRYING TO PEEK AT A NULL ZONE STACK\n");
#endif
        llvm_zone_t* z = llvm_zone_create(1024 * 1024 * 1); // default root zone is 1M
        llvm_push_zone_stack(z);
        stack = llvm_threads_get_zone_stack();
#if DEBUG_ZONE_STACK
        printf("Creating new 1M default zone %p:%lld on ZStack:%p\n",z,z->size,stack);
#endif
        return z;
    }
    z = stack->head;
#if DEBUG_ZONE_STACK
    printf("%p: peeking at zone %p:%lld\n",stack,z,z->size);
#endif
    return z;
}

EXPORT llvm_zone_t* llvm_pop_zone_stack()
{
    auto stack(llvm_threads_get_zone_stack());
    if (unlikely(!stack)) {
#if DEBUG_ZONE_STACK
        printf("TRYING TO POP A ZONE FROM AN EMPTY ZONE STACK\n");
#endif
        return nullptr;
    }
    llvm_zone_t* head = stack->head;
    llvm_zone_stack* tail = stack->tail;
#if DEBUG_ZONE_STACK
    llvm_threads_dec_zone_stacksize();
    if (!tail) {
        printf("%p: popping zone %p:%lld from stack with no tail\n",stack,head,head->size);
    } else {
        printf("%p: popping new zone %p:%lld back to old zone %p:%lld\n",stack,head,head->size,tail->head,tail->head->size);
    }
#endif
    free(stack);
    llvm_threads_set_zone_stack(tail);
    return head;
}

void llvm_threads_inc_zone_stacksize() {
    ++tls_llvm_zone_stacksize;
}

void llvm_threads_dec_zone_stacksize() {
    --tls_llvm_zone_stacksize;
}

uint64_t llvm_threads_get_zone_stacksize() {
    return tls_llvm_zone_stacksize;
}

// merge note - the following were not exposed from EXTLLVM.h before, and still aren't in any header.
EXPORT void llvm_zone_print(llvm_zone_t* zone)
{
  auto tmp(zone);
  auto total_size(zone->size);
  int64_t segments(1);
  while (tmp->memories) {
    tmp = tmp->memories;
    total_size += tmp->size;
    segments++;
  }
  printf("<MemZone(%p) size(%" PRId64 ") free(%" PRId64 ") segs(%" PRId64 ")>",zone,total_size,(zone->size - zone->offset),segments);
  return;
}

EXPORT uint64_t llvm_zone_ptr_size(void* ptr) // could be inline version in llvm (as well)
{
    return *(reinterpret_cast<uint64_t*>(ptr) - 1);
}

EXPORT bool llvm_zone_copy_ptr(void* ptr1, void* ptr2)
{
    uint64_t size1 = llvm_zone_ptr_size(ptr1);
    uint64_t size2 = llvm_zone_ptr_size(ptr2);

    if (unlikely(size1 != size2)) {
  //printf("Bad LLVM ptr copy - size mismatch setting %p:%lld -> %p:%lld\n",ptr1,size1,ptr2,size2);
      return 1;
    }
    if (unlikely(!size1)) {
  //printf("Bad LLVM ptr copy - size mismatch setting %p:%lld -> %p:%lld\n",ptr1,size1,ptr2,size2);
      return 1;
    }
    //printf("zone_copy_ptr: %p,%p,%lld,%lld\n",ptr2,ptr1,size1,size2);
    std::memcpy(ptr2, ptr1, size1);
    return 0;
}

EXPORT bool llvm_ptr_in_zone(llvm_zone_t* zone, void* ptr)
{
    while (unlikely(zone && (ptr < zone->memory || ptr >= reinterpret_cast<char*>(zone->memory) + zone->size))) {
      zone = zone->memories;
    }
    return zone;
}

} // namespace EXTLLVM
} // namespace extemp
