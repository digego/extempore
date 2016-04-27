static pointer assocstrcmp(scheme* Scheme, pointer Args)
{
    return assoc_strcmp(Scheme, pair_car(Args), pair_cadr(Args));
}

static pointer assocstrcmpall(scheme* Scheme, pointer Args)
{
    return assoc_strcmp_all(Scheme, pair_car(Args), pair_cadr(Args));
}

#define ASSOC_DEFS \
        { "assoc-strcmp", &assocstrcmp }, \
        { "assoc-strcmp-all", &assocstrcmpall }
