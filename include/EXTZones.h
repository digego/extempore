#pragma once


#include <cinttypes>

struct zone_hooks_t {
  uint64_t space; // here just so we don't get <i8*,i8*>
  void* hook; // xtlang closure of type [void]*
  zone_hooks_t* hooks;
};

// WARNING WARNING WARNING - HERE BE DRAGONS
// THIS STRUCTURE IS REFERENCED FROM GENERATED CODE
// DO NOT ALTER IT!!!

struct llvm_zone_t {
  void* memory;
  uint64_t offset;
  uint64_t mark;
  uint64_t size;
  zone_hooks_t* cleanup_hooks;
  llvm_zone_t* memories;
};

struct llvm_zone_stack
{
    llvm_zone_t* head;
    llvm_zone_stack* tail;
};

extern thread_local llvm_zone_stack* tls_llvm_zone_stack;
extern thread_local uint64_t tls_llvm_zone_stacksize;
