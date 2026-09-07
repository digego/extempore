#pragma once

// Runtime entry points that the Scheme FFI needs without pulling in the LLVM
// headers behind EXTLLVM.h. Defined in EXTLLVM.cpp.

#include <cstdint>

#include "EXTZones.h"
#include "UNIV.h"

extern "C" {

EXPORT void llvm_destroy_zone_after_delay(llvm_zone_t* zone, uint64_t delay);

static inline uint64_t string_hash(const char* str) {
    uint64_t result(0);
    unsigned char c;
    while ((c = *(str++))) {
        result = result * 33 + uint8_t(c);
    }
    return result;
}

EXPORT double xtc_randd();
EXPORT int64_t xtc_rand1_i64(int64_t a);
}
