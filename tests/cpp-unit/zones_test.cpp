#include <gtest/gtest.h>

#include <cstdint>
#include <cstring>

#include "EXTZones.h"

using extemp::EXTZones::llvm_ptr_in_zone;
using extemp::EXTZones::llvm_zone_create;
using extemp::EXTZones::llvm_zone_destroy;
using extemp::EXTZones::llvm_zone_malloc;
using extemp::EXTZones::llvm_zone_ptr_size;

namespace {

struct Zone {
    llvm_zone_t* z;
    explicit Zone(uint64_t size) : z(llvm_zone_create(size)) {}
    ~Zone() {
        llvm_zone_destroy(z);
    }
    char* base() const {
        return static_cast<char*>(z->memory);
    }
};

// Footprint of an allocation as the allocator lays it out: a 32-byte size
// header plus the payload, the whole rounded up to a multiple of 32.
uint64_t footprint(uint64_t size) {
    return (size + LLVM_ZONE_ALIGN + LLVM_ZONE_ALIGNPAD) & ~LLVM_ZONE_ALIGNPAD;
}

}  // namespace

TEST(Zones, AllocationThatFitsStaysInsideTheZone) {
    Zone zone(1000);
    char* old_memory = zone.base();
    // 960 + 32 header = 992, already a multiple of 32: the largest request that
    // fits in a 1000-byte zone.
    auto* p = static_cast<char*>(llvm_zone_malloc(zone.z, 960));
    ASSERT_NE(p, nullptr);
    EXPECT_EQ(zone.z->memory, old_memory) << "no extension expected";
    EXPECT_EQ(zone.z->memories, nullptr);
    EXPECT_GE(p, old_memory + LLVM_ZONE_ALIGN);
    EXPECT_LE(p + 960, old_memory + 1000);
    EXPECT_EQ(llvm_zone_ptr_size(p), footprint(960));
    EXPECT_EQ(zone.z->offset, footprint(960));
    EXPECT_TRUE(llvm_ptr_in_zone(zone.z, p));
}

TEST(Zones, RoundedFootprintIsCheckedNotTheRawSize) {
    // 961 bytes needs 961 + 32 = 993 < 1000 unrounded, but rounds up to 1024.
    // Checking the unrounded size let the memset run 24 bytes past the end;
    // the rounded footprint must go to the extension path instead.
    Zone zone(1000);
    char* old_memory = zone.base();
    auto* p = static_cast<char*>(llvm_zone_malloc(zone.z, 961));
    ASSERT_NE(p, nullptr);
    ASSERT_NE(zone.z->memories, nullptr) << "961 bytes must extend a 1000-byte zone";
    EXPECT_NE(zone.z->memory, old_memory);
    EXPECT_EQ(zone.z->memories->memory, old_memory) << "old block is kept on the chain";
    EXPECT_EQ(zone.z->memories->size, 1000u);
    EXPECT_EQ(zone.z->size, 2048u) << "max(1000, 1024) doubled";
    EXPECT_EQ(zone.z->offset, footprint(961));
    EXPECT_GE(p, zone.base() + LLVM_ZONE_ALIGN);
    EXPECT_LE(p + 961, zone.base() + zone.z->size);
    EXPECT_EQ(llvm_zone_ptr_size(p), 1024u);
}

TEST(Zones, PointersFromEarlierBlocksRemainValidAfterExtension) {
    Zone zone(1000);
    auto* first = static_cast<char*>(llvm_zone_malloc(zone.z, 100));
    std::memset(first, 0x5a, 100);
    auto* second = static_cast<char*>(llvm_zone_malloc(zone.z, 4000));
    ASSERT_NE(zone.z->memories, nullptr);
    EXPECT_TRUE(llvm_ptr_in_zone(zone.z, first));
    EXPECT_TRUE(llvm_ptr_in_zone(zone.z, second));
    for (int i = 0; i < 100; ++i) {
        EXPECT_EQ(first[i], 0x5a);
    }
    for (int i = 0; i < 4000; ++i) {
        ASSERT_EQ(second[i], 0) << "fresh allocations are zeroed";
    }
    int on_stack = 0;
    EXPECT_FALSE(llvm_ptr_in_zone(zone.z, &on_stack));
}

TEST(Zones, EmptyZoneIsReplacedRatherThanChained) {
    Zone zone(0);
    EXPECT_EQ(zone.z->memory, nullptr);
    auto* p = llvm_zone_malloc(zone.z, 10);
    ASSERT_NE(p, nullptr);
    EXPECT_EQ(zone.z->memories, nullptr);
    EXPECT_EQ(zone.z->size, 1024u);
    EXPECT_TRUE(llvm_ptr_in_zone(zone.z, p));
}

TEST(Zones, RequestTooLargeToRepresentAborts) {
    Zone zone(1000);
    EXPECT_DEATH(llvm_zone_malloc(zone.z, UINT64_MAX - 8), "not representable");
}
