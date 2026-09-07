static pointer createMallocZone(scheme* Scheme, pointer Args)
{
    if (Args == Scheme->NIL) {
        return mk_cptr(Scheme, extemp::EXTZones::llvm_zone_create(1024 * 100));
    }
    return mk_cptr(Scheme, extemp::EXTZones::llvm_zone_create(argInt(Scheme, Args, 1)));
}

static pointer defaultMallocZone(scheme* Scheme, pointer Args)
{
    return mk_cptr(Scheme,Scheme->m_process->getDefaultZone());
}

static pointer destroyMallocZone(scheme* Scheme, pointer Args)
{
    llvm_zone_t* ptr = reinterpret_cast<llvm_zone_t*>(argCptr(Scheme, Args, 1));
    if (pair_cdr(Args) != Scheme->NIL) {
        llvm_destroy_zone_after_delay(ptr, argInt(Scheme, Args, 2));
    } else {
        extemp::EXTZones::llvm_zone_destroy(ptr);
    }
    return Scheme->T;
}

static pointer resetMallocZone(scheme* Scheme, pointer Args)
{
    llvm_zone_t* zone = reinterpret_cast<llvm_zone_t*>(argCptr(Scheme, Args, 1));
    extemp::EXTZones::llvm_zone_reset(zone);
    return Scheme->T;
}

static pointer peekMemoryZone(scheme* Scheme, pointer Args)
{
    return mk_cptr(Scheme, extemp::EXTZones::llvm_peek_zone_stack());
}

static pointer popMemoryZone(scheme* Scheme, pointer Args)
{
    return mk_cptr(Scheme, extemp::EXTZones::llvm_pop_zone_stack());
}

static pointer pushMemoryZone(scheme* Scheme, pointer Args)
{
    extemp::EXTZones::llvm_push_zone_stack(reinterpret_cast<llvm_zone_t*>(
            argCptr(Scheme, Args, 1)));
    return Scheme->T;
}

#define SYS_ZONE_DEFS \
        FFI_DEF("sys:create-mzone", createMallocZone, 0, 1, false), \
        FFI_DEF("sys:default-mzone", defaultMallocZone, 0, 0, false), \
        FFI_DEF("sys:destroy-mzone", destroyMallocZone, 1, 1, false), \
        FFI_DEF("sys:reset-mzone", resetMallocZone, 1, 0, false), \
        FFI_DEF("sys:peek-memzone", peekMemoryZone, 0, 0, false), \
        FFI_DEF("sys:pop-memzone", popMemoryZone, 0, 0, false), \
        FFI_DEF("sys:push-memzone", pushMemoryZone, 1, 0, false)
