#include <gtest/gtest.h>

#include "ext/NetUtil.h"

TEST(ResolveIpv4, LocalhostResolves) {
    uint32_t a = extemp::net_util::resolve_ipv4("127.0.0.1");
    EXPECT_NE(a, 0u);
    // 127.0.0.1 in network byte order
    EXPECT_EQ(a, htonl(0x7f000001));
}

TEST(ResolveIpv4, NullReturnsZero) {
    EXPECT_EQ(extemp::net_util::resolve_ipv4(nullptr), 0u);
}

TEST(ResolveIpv4, BogusHostReturnsZero) {
    // Using a TLD that cannot resolve per RFC 2606 / RFC 6761.
    uint32_t a = extemp::net_util::resolve_ipv4("does-not-resolve.invalid");
    EXPECT_EQ(a, 0u);
}
