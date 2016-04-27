static pointer createMallocZone(scheme* Scheme, pointer Args)
{
    if (Args == Scheme->NIL) {
        return mk_cptr(Scheme, extemp::EXTLLVM::llvm_zone_create(1024 * 100));
    }
    return mk_cptr(Scheme, extemp::EXTLLVM::llvm_zone_create(ivalue(pair_car(Args))));
}

static pointer defaultMallocZone(scheme* Scheme, pointer Args)
{
    return mk_cptr(Scheme,Scheme->m_process->getDefaultZone());
}

static pointer destroyMallocZone(scheme* Scheme, pointer Args)
{
    llvm_zone_t* ptr = reinterpret_cast<llvm_zone_t*>(cptr_value(pair_car(Args)));
    if (pair_cdr(Args) != Scheme->NIL) {
        llvm_destroy_zone_after_delay(ptr, ivalue(pair_cadr(Args)));
    } else {
        extemp::EXTLLVM::llvm_zone_destroy(ptr);
    }
    return Scheme->T;
}

static pointer copyToDefaultZone(scheme* Scheme, pointer Args)
{
    return Scheme->NIL;
}

static pointer resetMallocZone(scheme* Scheme, pointer Args)
{
    llvm_zone_t* zone = reinterpret_cast<llvm_zone_t*>(cptr_value(pair_car(Args)));
    extemp::EXTLLVM::llvm_zone_reset(zone);
    return Scheme->T;
}

static pointer peekMemoryZone(scheme* Scheme, pointer Args)
{
    return mk_cptr(Scheme, extemp::EXTLLVM::llvm_peek_zone_stack());
}

static pointer popMemoryZone(scheme* Scheme, pointer Args)
{
    return mk_cptr(Scheme, extemp::EXTLLVM::llvm_pop_zone_stack());
}

static pointer pushMemoryZone(scheme* Scheme, pointer Args)
{
    extemp::EXTLLVM::llvm_push_zone_stack(reinterpret_cast<llvm_zone_t*>(cptr_value(pair_car(Args))));
    return Scheme->T;
}

#define SYS_ZONE_DEFS \
        { "sys:create-mzone", &createMallocZone }, \
        { "sys:default-mzone", &defaultMallocZone }, \
        { "sys:destroy-mzone", &destroyMallocZone }, \
        { "sys:copy-to-dmzone", &copyToDefaultZone }, \
        { "sys:reset-mzone", &resetMallocZone }, \
        { "sys:peek-memzone", &peekMemoryZone }, \
        { "sys:pop-memzone", &popMemoryZone }, \
        { "sys:push-memzone", &pushMemoryZone }
