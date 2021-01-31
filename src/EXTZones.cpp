#include <EXTZones.h>

thread_local llvm_zone_stack* tls_llvm_zone_stack = 0;
thread_local uint64_t tls_llvm_zone_stacksize = 0;
