// Hash table FFI using s7's built-in hash tables.
//
// Provides compatibility with the original vector-of-alists implementation.
// s7 hash tables use equal? for key comparison, which matches the previous
// semantics for symbol and string keys (symbols are interned in s7).

static pointer hashtable_make(scheme* sc, pointer args)
{
    int size = static_cast<int>(ivalue(pair_car(args)));
    if (size < 1) size = 256;
    return s7_make_hash_table(sc->sc, size);
}

static pointer hashtable_ref(scheme* sc, pointer args)
{
    pointer ht  = pair_car(args);
    pointer key = pair_cadr(args);
    pointer result = s7_hash_table_ref(sc->sc, ht, key);
    return result;
}

static pointer hashtable_set(scheme* sc, pointer args)
{
    pointer ht    = pair_car(args);
    pointer key   = pair_cadr(args);
    pointer value = pair_caddr(args);
    s7_hash_table_set(sc->sc, ht, key, value);
    return value;
}

static pointer hashtable_remove(scheme* sc, pointer args)
{
    pointer ht  = pair_car(args);
    pointer key = pair_cadr(args);
    pointer existing = s7_hash_table_ref(sc->sc, ht, key);
    if (existing == sc->F) return sc->F;
    // s7_hash_table_set with value #f (the missing-key value) triggers removal
    s7_hash_table_set(sc->sc, ht, key, sc->F);
    return sc->T;
}

static pointer hashtable_count(scheme* sc, pointer args)
{
    pointer ht = pair_car(args);
    s7_pointer entries_sym = s7_make_symbol(sc->sc, "hash-table-entries");
    s7_pointer result = s7_call(sc->sc, s7_symbol_value(sc->sc, entries_sym),
                                s7_list(sc->sc, 1, ht));
    return result;
}

static pointer hashtable_clear(scheme* sc, pointer args)
{
    pointer ht = pair_car(args);
    // Collect keys first (can't mutate while iterating)
    s7_pointer keys_fn = s7_eval_c_string(sc->sc, "(lambda (ht) (map car ht))");
    s7_int gc_loc = s7_gc_protect(sc->sc, keys_fn);
    s7_pointer keys = s7_call(sc->sc, keys_fn, s7_list(sc->sc, 1, ht));
    s7_gc_unprotect_at(sc->sc, gc_loc);
    // Remove each key by setting to #f (triggers s7 removal)
    for (s7_pointer k = keys; s7_is_pair(k); k = s7_cdr(k))
        s7_hash_table_set(sc->sc, ht, s7_car(k), sc->F);
    return ht;
}

static pointer hashtable_keys(scheme* sc, pointer args)
{
    pointer ht = pair_car(args);
    pointer result = sc->NIL;
    // Use for-each via Scheme to collect keys
    s7_pointer keys_fn = s7_eval_c_string(sc->sc,
        "(lambda (ht) (map car ht))");
    s7_int gc_loc = s7_gc_protect(sc->sc, keys_fn);
    result = s7_call(sc->sc, keys_fn, s7_list(sc->sc, 1, ht));
    s7_gc_unprotect_at(sc->sc, gc_loc);
    return result;
}

static pointer hashtable_to_alist(scheme* sc, pointer args)
{
    pointer ht = pair_car(args);
    // s7 hash tables can be iterated as lists of (key . value) pairs
    s7_pointer alist_fn = s7_eval_c_string(sc->sc,
        "(lambda (ht) (map values ht))");
    s7_int gc_loc = s7_gc_protect(sc->sc, alist_fn);
    pointer result = s7_call(sc->sc, alist_fn, s7_list(sc->sc, 1, ht));
    s7_gc_unprotect_at(sc->sc, gc_loc);
    return result;
}

static pointer hashtable_for_each(scheme* sc, pointer args)
{
    pointer proc = pair_car(args);
    pointer ht   = pair_cadr(args);
    s7_pointer fe_fn = s7_eval_c_string(sc->sc,
        "(lambda (proc ht) (for-each (lambda (entry) (proc (car entry) (cdr entry))) ht))");
    s7_int gc_loc = s7_gc_protect(sc->sc, fe_fn);
    s7_call(sc->sc, fe_fn, s7_list(sc->sc, 2, proc, ht));
    s7_gc_unprotect_at(sc->sc, gc_loc);
    return sc->T;
}

#define HASHTABLE_DEFS \
        { "make-hashtable", &hashtable_make }, \
        { "hashtable-ref", &hashtable_ref }, \
        { "hashtable-set!", &hashtable_set }, \
        { "hashtable-remove!", &hashtable_remove }, \
        { "hashtable-clear!", &hashtable_clear }, \
        { "hashtable-count", &hashtable_count }, \
        { "hashtable-keys", &hashtable_keys }, \
        { "hashtable->alist", &hashtable_to_alist }, \
        { "hashtable-for-each", &hashtable_for_each }
