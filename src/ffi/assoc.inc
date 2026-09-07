static pointer assocstrcmp(scheme* Scheme, pointer Args)
{
    return assoc_strcmp(Scheme, pair_car(Args), pair_cadr(Args));
}

static pointer assocstrcmpall(scheme* Scheme, pointer Args)
{
    return assoc_strcmp(Scheme, pair_car(Args), pair_cadr(Args), true);
}

#define ASSOC_DEFS \
        FFI_DEF("assoc-strcmp", assocstrcmp, 2, 0, false), \
        FFI_DEF("assoc-strcmp-all", assocstrcmpall, 2, 0, false)
