#pragma once

// Zone (region) allocator used by generated xtlang code.
//
// Zones are thread-confined: every thread has its own zone stack
// (tls_llvm_zone_stack), and a zone is only ever pushed, popped, allocated
// from, reset or destroyed by the thread that owns that stack. None of the
// functions below take a lock. Sharing a single llvm_zone_t between threads
// is a bug in the caller, not something this allocator defends against.

#include <UNIV.h>

#include <cinttypes>

struct zone_hooks_t {
    uint64_t space;  // here just so we don't get <i8*,i8*>
    void* hook;      // xtlang closure of type [void]*
    zone_hooks_t* hooks;
};

// WARNING WARNING WARNING - HERE BE DRAGONS
// THIS STRUCTURE IS REFERENCED FROM GENERATED CODE (runtime/bitcode.ll)
// DO NOT ALTER IT!!!

struct llvm_zone_t {
    void* memory;
    uint64_t offset;
    uint64_t mark;
    uint64_t size;
    zone_hooks_t* cleanup_hooks;
    llvm_zone_t* memories;
};

struct llvm_zone_stack {
    llvm_zone_t* head;
    llvm_zone_stack* tail;
};

extern thread_local llvm_zone_stack* tls_llvm_zone_stack;

constexpr uint64_t LLVM_ZONE_ALIGN = 32;  // MUST BE POWER OF 2!
constexpr uint64_t LLVM_ZONE_ALIGNPAD = LLVM_ZONE_ALIGN - 1;
static_assert((LLVM_ZONE_ALIGN & LLVM_ZONE_ALIGNPAD) == 0, "LLVM_ZONE_ALIGN must be a power of 2");

namespace extemp {
namespace EXTZones {
llvm_zone_t* llvm_zone_create(uint64_t size);
EXPORT void llvm_zone_destroy(llvm_zone_t* Zone);
llvm_zone_t* llvm_zone_reset(llvm_zone_t* Zone);
EXPORT void* llvm_zone_malloc(llvm_zone_t* zone, uint64_t size);
EXPORT void* llvm_zone_malloc_from_current_zone(uint64_t size);
EXPORT void llvm_zone_print(llvm_zone_t* zone);
EXPORT uint64_t llvm_zone_ptr_size(void* ptr);
EXPORT bool llvm_zone_copy_ptr(void* ptr1, void* ptr2);
EXPORT bool llvm_ptr_in_zone(llvm_zone_t* zone, void* ptr);
EXPORT bool llvm_ptr_in_current_zone(void* ptr);
llvm_zone_stack* llvm_threads_get_zone_stack();
void llvm_threads_set_zone_stack(llvm_zone_stack* Stack);
void llvm_push_zone_stack(llvm_zone_t* Zone);
llvm_zone_t* llvm_peek_zone_stack();
EXPORT llvm_zone_t* llvm_pop_zone_stack();
EXPORT llvm_zone_t* llvm_peek_zone_stack_extern();
EXPORT void llvm_push_zone_stack_extern(llvm_zone_t* Zone);
EXPORT llvm_zone_t* llvm_zone_create_extern(uint64_t Size);
EXPORT llvm_zone_t* llvm_zone_callback_setup();
}  // namespace EXTZones
}  // namespace extemp
