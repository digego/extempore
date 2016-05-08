static pointer asciiColor(scheme* Scheme, pointer Args)
{
    ascii_text_color(bool(ivalue(pair_car(Args))), ivalue(pair_cadr(Args)), ivalue(pair_caddr(Args)));
    return Scheme->T;
}

static pointer emit(scheme* Scheme, pointer Args)
{
    std::stringstream ss;
    int lgth = list_length(Scheme, Args);
    pointer io = list_ref(Scheme, lgth - 1, Args);
    if (!is_string(io)) {
        PRINT_ERROR("Emit accepts only string arguments!\n");
        return Scheme->F;
    }
    ss << string_value(io);
    pointer arg = 0;
    for (int i=0; i < lgth - 1; ++i) {
        arg = pair_car(Args);
        if (!is_string(arg)) {
            PRINT_ERROR("Emit accepts only string arguments!\n");
            return Scheme->F;
        }
        ss << string_value(arg);
        Args = pair_cdr(Args);
    }
    auto tmp(ss.str());
    int l = tmp.length();
    auto s(strdup(tmp.c_str()));
    free(io->_object._string._svalue);
    io->_object._string._svalue = s;
    io->_object._string._length = l;
    return io;
}

static pointer exit_extempore(scheme* Scheme, pointer Args)
{
    int rc(ivalue(pair_car(Args)));
    std::_Exit(rc); // currently Windows is very unhappy trying to clean up resources
}

#define UTILITY_DEFS \
        { "ascii-print-color", &asciiColor }, \
        { "emit", &emit }, \
        { "quit", &exit_extempore }
