static pointer asciiColor(scheme* Scheme, pointer Args)
{
    ascii_text_color(bool(ivalue(pair_car(Args))), ivalue(pair_cadr(Args)), ivalue(pair_caddr(Args)));
    return Scheme->T;
}

static pointer emit(scheme* Scheme, pointer Args)
{
    int lgth = list_length(Scheme, Args);
    pointer io = list_ref(Scheme, lgth - 1, Args);

    if (s7_is_output_port(Scheme->sc, io)) {
        pointer a = Args;
        for (int i = 0; i < lgth - 1; ++i) {
            pointer arg = pair_car(a);
            if (is_string(arg)) {
                s7_display(Scheme->sc, arg, io);
            }
            a = pair_cdr(a);
        }
        return io;
    }

    std::stringstream ss;
    if (!is_string(io)) {
        PRINT_ERROR("Emit accepts only string arguments!\n");
        return Scheme->F;
    }
    ss << string_value(io);
    for (int i = 0; i < lgth - 1; ++i) {
        pointer arg = pair_car(Args);
        if (!is_string(arg)) {
            PRINT_ERROR("Emit accepts only string arguments!\n");
            return Scheme->F;
        }
        ss << string_value(arg);
        Args = pair_cdr(Args);
    }
    return mk_string(Scheme, ss.str().c_str());
}

static pointer exit_extempore(scheme* Scheme, pointer Args)
{
    int rc(ivalue(pair_car(Args)));
    std::_Exit(rc);
}

#define UTILITY_DEFS \
        { "ascii-print-color", &asciiColor }, \
        { "emit", &emit }, \
        { "quit", &exit_extempore }
