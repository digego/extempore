# Building Extempore on Alpine

Currently, this is tricky - LLVM 3.8.0 won't build cleanly with musl
libc. I've parked this for now, when we upgrade Extempore to LLVM
3.9.0 we might revisit it if the LLVM-musl stuff gets cleaned up (or
is easy enough to patch - see `extras/musl-libc.patch`).

It would be super nice to have lightweight alpine-based Extempore
containers, though. Pull requests welcome :)
