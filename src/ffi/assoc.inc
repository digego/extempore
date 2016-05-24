static pointer assocstrcmp(scheme* Scheme, pointer Args)
{
    return assoc_strcmp(Scheme, pair_car(Args), pair_cadr(Args));
}

static pointer assocstrcmpall(scheme* Scheme, pointer Args)
{
    return assoc_strcmp(Scheme, pair_car(Args), pair_cadr(Args), true);
}

#define ASSOC_DEFS \
        { "assoc-strcmp", &assocstrcmp }, \
        { "assoc-strcmp-all", &assocstrcmpall }
