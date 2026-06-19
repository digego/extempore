#ifndef OSCPP_ENDIAN_HPP_INCLUDED
#define OSCPP_ENDIAN_HPP_INCLUDED

// Defines either OSCPP_LITTLE_ENDIAN or OSCPP_BIG_ENDIAN based on the target platform.
//
// GCC and Clang (including Apple Clang) expose __BYTE_ORDER__ as a predefined
// compiler macro, so no platform-specific headers are needed.
// MSVC does not define __BYTE_ORDER__, but all MSVC targets (x86, x64, ARM on
// Windows) are little-endian.

#if defined(__BYTE_ORDER__) && defined(__ORDER_LITTLE_ENDIAN__) && defined(__ORDER_BIG_ENDIAN__)
#    if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#        define OSCPP_LITTLE_ENDIAN
#    elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#        define OSCPP_BIG_ENDIAN
#    else
#        error Unknown machine endianness detected.
#    endif
#elif defined(_MSC_VER)
#    define OSCPP_LITTLE_ENDIAN
#else
#    error Cannot determine endianness: define OSCPP_LITTLE_ENDIAN or OSCPP_BIG_ENDIAN manually.
#endif

#endif // OSCPP_ENDIAN_HPP_INCLUDED
