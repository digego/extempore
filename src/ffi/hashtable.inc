// String-keyed hash table built on top of Scheme vectors.
//
// A hash table is a Scheme vector where each slot holds an alist of
// (key . value) pairs.  Keys are strings or symbols; lookup uses the
// same comparison semantics as assoc-strcmp.  The underlying vector is
// GC-traced, so values are protected from collection.
//
// Scheme API:
//   (make-hashtable size)           -> vector
//   (hashtable-ref ht key)          -> value or #f
//   (hashtable-set! ht key value)   -> value
//   (hashtable-remove! ht key)      -> #t or #f
//   (hashtable-count ht)            -> integer

static inline uint64_t ht_str_hash(const char* str)
{
    uint64_t result = 5381;
    unsigned char c;
    while ((c = *(str++)))
        result = result * 33 + c;
    return result;
}

static inline const char* ht_key_string(scheme* sc, pointer key)
{
    if (is_symbol(key))
        return strvalue(car(key));
    if (is_string(key))
        return strvalue(key);
    return nullptr;
}

// (make-hashtable size) -> vector
// Creates a vector of `size` empty lists.
static pointer hashtable_make(scheme* sc, pointer args)
{
    int size = static_cast<int>(ivalue(pair_car(args)));
    if (size < 1) size = 256;
    return mk_vector(sc, size);
}

// (hashtable-ref ht key) -> value or #f
// Hashes key, looks up in the corresponding bucket via assoc-strcmp.
static pointer hashtable_ref(scheme* sc, pointer args)
{
    pointer ht  = pair_car(args);
    pointer key = pair_cadr(args);

    const char* skey = ht_key_string(sc, key);
    if (!skey) return sc->F;

    int idx = ht_str_hash(skey) % ht->_size;
    pointer bucket = vector_elem(ht, idx);
    pointer entry = assoc_strcmp(sc, key, bucket);
    if (entry == sc->F) return sc->F;
    return cdr(entry);
}

// (hashtable-set! ht key value) -> value
// If key exists in the bucket, updates its cdr.  Otherwise conses a
// new (key . value) pair onto the bucket.
static pointer hashtable_set(scheme* sc, pointer args)
{
    pointer ht    = pair_car(args);
    pointer key   = pair_cadr(args);
    pointer value = pair_caddr(args);

    const char* skey = ht_key_string(sc, key);
    if (!skey) return sc->F;

    int idx = ht_str_hash(skey) % ht->_size;
    pointer bucket = vector_elem(ht, idx);
    pointer entry = assoc_strcmp(sc, key, bucket);

    if (entry != sc->F) {
        cdr(entry) = value;
    } else {
        pointer new_pair = cons(sc, key, value);
        set_vector_elem(sc, ht, idx, cons(sc, new_pair, bucket));
    }
    return value;
}

// (hashtable-remove! ht key) -> #t if removed, #f if not found
static pointer hashtable_remove(scheme* sc, pointer args)
{
    pointer ht  = pair_car(args);
    pointer key = pair_cadr(args);

    const char* skey = ht_key_string(sc, key);
    if (!skey) return sc->F;

    int idx = ht_str_hash(skey) % ht->_size;
    pointer bucket = vector_elem(ht, idx);

    // Build a new bucket list without the matching entry
    pointer prev = sc->NIL;
    pointer result = sc->NIL;
    bool found = false;

    for (pointer x = bucket; is_pair(x); x = cdr(x)) {
        pointer pair = car(x);
        if (!found && is_pair(pair)) {
            const char* ekey = nullptr;
            pointer ekey_ptr = car(pair);
            if (is_symbol(ekey_ptr))
                ekey = strvalue(car(ekey_ptr));
            else if (is_string(ekey_ptr))
                ekey = strvalue(ekey_ptr);
            if (ekey && strcmp(ekey, skey) == 0) {
                found = true;
                continue;  // skip this entry
            }
        }
        result = cons(sc, pair, result);
    }

    if (found) {
        // Reverse result to maintain order (not strictly necessary but tidy)
        pointer reversed = sc->NIL;
        for (pointer x = result; is_pair(x); x = cdr(x))
            reversed = cons(sc, car(x), reversed);
        set_vector_elem(sc, ht, idx, reversed);
        return sc->T;
    }
    return sc->F;
}

// (hashtable-count ht) -> integer
// Counts total entries across all buckets.
static pointer hashtable_count(scheme* sc, pointer args)
{
    pointer ht = pair_car(args);
    int count = 0;
    int size = ht->_size;
    for (int i = 0; i < size; i++) {
        for (pointer x = vector_elem(ht, i); is_pair(x); x = cdr(x))
            count++;
    }
    return mk_integer(sc, count);
}

// (hashtable-clear! ht) -> ht
// Sets all buckets to NIL.
static pointer hashtable_clear(scheme* sc, pointer args)
{
    pointer ht = pair_car(args);
    int size = ht->_size;
    for (int i = 0; i < size; i++)
        set_vector_elem(sc, ht, i, sc->NIL);
    return ht;
}

// (hashtable-keys ht) -> list of keys
static pointer hashtable_keys(scheme* sc, pointer args)
{
    pointer ht = pair_car(args);
    pointer result = sc->NIL;
    int size = ht->_size;
    for (int i = 0; i < size; i++) {
        for (pointer x = vector_elem(ht, i); is_pair(x); x = cdr(x)) {
            if (is_pair(car(x)))
                result = cons(sc, car(car(x)), result);
        }
    }
    return result;
}

// (hashtable->alist ht) -> list of (key . value) pairs
static pointer hashtable_to_alist(scheme* sc, pointer args)
{
    pointer ht = pair_car(args);
    pointer result = sc->NIL;
    int size = ht->_size;
    for (int i = 0; i < size; i++) {
        for (pointer x = vector_elem(ht, i); is_pair(x); x = cdr(x)) {
            if (is_pair(car(x)))
                result = cons(sc, car(x), result);
        }
    }
    return result;
}

#define HASHTABLE_DEFS \
        { "make-hashtable", &hashtable_make }, \
        { "hashtable-ref", &hashtable_ref }, \
        { "hashtable-set!", &hashtable_set }, \
        { "hashtable-remove!", &hashtable_remove }, \
        { "hashtable-clear!", &hashtable_clear }, \
        { "hashtable-count", &hashtable_count }, \
        { "hashtable-keys", &hashtable_keys }, \
        { "hashtable->alist", &hashtable_to_alist }
