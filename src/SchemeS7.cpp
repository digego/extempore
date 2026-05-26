#include "SchemeS7Private.h"
#include <array>
#include <atomic>
#include <iostream>
#include <mutex>
#include <unordered_map>
#include <vector>

static std::mutex s_wrapperMapMutex;
static std::unordered_map<s7_scheme*, scheme*> s_wrapperMap;

scheme* scheme_wrapper_from_s7(s7_scheme* raw_sc) {
    std::lock_guard<std::mutex> lock(s_wrapperMapMutex);
    auto it = s_wrapperMap.find(raw_sc);
    if (it != s_wrapperMap.end())
        return it->second;
    return nullptr;
}

void scheme_register_wrapper(s7_scheme* raw_sc, scheme* wrapper) {
    std::lock_guard<std::mutex> lock(s_wrapperMapMutex);
    s_wrapperMap[raw_sc] = wrapper;
}

static constexpr int MAX_FFI_FUNCS = 4096;
// Registration can race across scheme instances; reads happen from every
// scheme call.  Atomics give us a happens-before between the store in
// mk_foreign_func and the load here with no mutex on the hot path.
static std::atomic<foreign_func> s_ffiTable[MAX_FFI_FUNCS];
static std::atomic<int> s_ffiCount{0};

template <int N> static s7_pointer ffi_trampoline(s7_scheme* raw_sc, s7_pointer args) {
    scheme* wrapper = scheme_wrapper_from_s7(raw_sc);
    try {
        return s_ffiTable[N].load(std::memory_order_acquire)(wrapper, args);
    } catch (const ScmRuntimeError& e) {
        return s7_error(raw_sc, s7_make_symbol(raw_sc, "wrong-type-arg"),
                        s7_list(raw_sc, 1, s7_make_string(raw_sc, e.msg)));
    } catch (const std::exception& e) {
        return s7_error(raw_sc, s7_make_symbol(raw_sc, "c-error"),
                        s7_list(raw_sc, 1, s7_make_string(raw_sc, e.what())));
    } catch (...) {
        return s7_error(raw_sc, s7_make_symbol(raw_sc, "c-error"),
                        s7_list(raw_sc, 1, s7_make_string(raw_sc, "unknown C++ exception in FFI")));
    }
}

using s7_function_ptr = s7_pointer (*)(s7_scheme*, s7_pointer);

template <int... Is>
static constexpr std::array<s7_function_ptr, sizeof...(Is)>
make_trampoline_table(std::integer_sequence<int, Is...>) {
    return {{ffi_trampoline<Is>...}};
}

static const auto s_trampolineTable =
    make_trampoline_table(std::make_integer_sequence<int, MAX_FFI_FUNCS>{});

static s7_pointer lenient_string_ref(s7_scheme* sc, s7_pointer args) {
    s7_pointer s = s7_car(args);
    s7_pointer idx_p = s7_cadr(args);
    s7_int idx = s7_integer(idx_p);
    s7_int len = s7_string_length(s);
    if (idx < 0 || idx >= len) {
        return s7_make_character(sc, 0);
    }
    return s7_make_character(sc, static_cast<uint8_t>(s7_string(s)[idx]));
}

static void begin_hook(s7_scheme* raw_sc, bool* val) {
    scheme* wrapper = scheme_wrapper_from_s7(raw_sc);
    if (!wrapper)
        return;
    uint64_t now = extemp::UNIV::TIME;
    if (wrapper->call_end_time > 0 && now > wrapper->call_end_time) {
        *val = true;
    }
}

scheme* scheme_init_new() {
    scheme* sc = new scheme{};

    sc->sc = s7_init();
    if (!sc->sc) {
        delete sc;
        return nullptr;
    }

    scheme_register_wrapper(sc->sc, sc);

    sc->NIL = s7_nil(sc->sc);
    sc->T = s7_t(sc->sc);
    sc->F = s7_f(sc->sc);
    sc->EOF_OBJ = s7_eof_object(sc->sc);
    sc->global_env = s7_rootlet(sc->sc);
    sc->retcode = 0;
    sc->value = sc->NIL;
    sc->m_process = nullptr;
    sc->ext_data = nullptr;
    sc->call_default_time = 44100 * 60;
    sc->call_start_time = 0;
    sc->call_end_time = 0;

    s7_set_begin_hook(sc->sc, begin_hook);

    sc->output_port = s7_open_output_string(sc->sc);
    s7_gc_protect(sc->sc, sc->output_port);
    sc->outport = sc->output_port;

    // TinyScheme compatibility shims

    // string-ref: return #\nul for out-of-bounds (TinyScheme was lenient)
    s7_define_function(sc->sc, "string-ref", lenient_string_ref, 2, 0, false,
                       "(string-ref str i) returns str[i], #\\nul if out of bounds");

    // make-string: (make-string 0) returns an output-string-port (mutable emit buffer)
    // TinyScheme's emit mutated its last string arg in-place; s7 strings are immutable.
    // Using output string ports as mutable accumulators preserves the emit pattern.
    s7_eval_c_string(sc->sc, "(let ((old-make-string make-string))"
                             "  (set! make-string"
                             "    (lambda args"
                             "      (if (and (pair? args) (null? (cdr args)) (eqv? (car args) 0))"
                             "          (open-output-string)"
                             "          (apply old-make-string args)))))");

    // Helper to extract string from emit buffer (output string port) or pass through
    s7_eval_c_string(sc->sc, "(define (emit-buffer->string x)"
                             "  (if (output-port? x) (get-output-string x) x))");

    // string->atom and atom->string
    s7_eval_c_string(sc->sc, "(define (string->atom s) (with-input-from-string s read))");
    s7_eval_c_string(sc->sc, "(define (atom->string x) (object->string x))");

    // current-environment -> curlet, interaction-environment -> rootlet
    s7_eval_c_string(sc->sc, "(define current-environment curlet)");
    s7_eval_c_string(sc->sc, "(define interaction-environment rootlet)");

    // get-closure-code -> procedure-source
    s7_eval_c_string(sc->sc, "(define get-closure-code procedure-source)");

    // closure? -> procedure?
    s7_eval_c_string(sc->sc, "(define closure? procedure?)");

    // cptr? -> c-pointer? (TinyScheme predicate)
    s7_eval_c_string(sc->sc, "(define cptr? c-pointer?)");

    // make-closure (best-effort: create a new lambda with given body and env)
    s7_eval_c_string(sc->sc, "(define (make-closure code env) code)");

    // set-input-port and set-output-port
    s7_eval_c_string(sc->sc, "(define set-input-port set-current-input-port)");
    s7_eval_c_string(sc->sc, "(define set-output-port set-current-output-port)");

    // foldr (TinyScheme's foldr is actually foldl - left fold)
    s7_eval_c_string(sc->sc, "(define (foldr f x lst)"
                             "  (if (null? lst) x"
                             "      (foldr f (f x (car lst)) (cdr lst))))");

    // list* (like cons* - creates list with last arg as tail)
    s7_eval_c_string(sc->sc, "(define (list* . args)"
                             "  (if (null? (cdr args)) (car args)"
                             "      (cons (car args) (apply list* (cdr args)))))");

    // Override list? to match R5RS (s7's list? returns #t for dotted pairs)
    s7_eval_c_string(sc->sc, "(define (list? x)"
                             "  (or (null? x)"
                             "      (and (pair? x)"
                             "           (let loop ((slow x) (fast (cdr x)))"
                             "             (cond ((null? fast) #t)"
                             "                   ((not (pair? fast)) #f)"
                             "                   ((null? (cdr fast)) #t)"
                             "                   ((not (pair? (cdr fast))) #f)"
                             "                   ((eq? slow fast) #f)"
                             "                   (else (loop (cdr slow) (cddr fast))))))))");

    // println (if not already defined)
    s7_eval_c_string(sc->sc, "(unless (defined? 'println)"
                             "  (define (println . args)"
                             "    (for-each display args)"
                             "    (newline)))");

    // gensym: s7's gensym returns {gensym}-N with curly braces, which are
    // invalid in LLVM IR identifiers. Override with clean alphanumeric names.
    s7_eval_c_string(
        sc->sc,
        "(let ((counter 0))"
        "  (set! gensym"
        "    (lambda args"
        "      (let ((prefix (if (pair? args) (car args) \"gensym\")))"
        "        (set! counter (+ counter 1))"
        "        (string->symbol (string-append prefix \"_\" (number->string counter)))))))");

    // open-input-file: s7 throws an error when the file doesn't exist,
    // but TinyScheme returned #f. Extempore code relies on the #f behavior.
    s7_eval_c_string(sc->sc, "(let ((s7-open-input-file open-input-file))"
                             "  (set! open-input-file"
                             "    (lambda (path . mode)"
                             "      (catch #t"
                             "        (lambda () (apply s7-open-input-file path mode))"
                             "        (lambda (type info) #f)))))");

    // file-exists?: not built into s7, was defined in TinyScheme
    s7_eval_c_string(sc->sc, "(define (file-exists? path)"
                             "  (let ((port (open-input-file path)))"
                             "    (if port (begin (close-input-port port) #t) #f)))");

    return sc;
}

void scheme_deinit(scheme* sc) {
    if (sc && sc->sc) {
        {
            std::lock_guard<std::mutex> lock(s_wrapperMapMutex);
            s_wrapperMap.erase(sc->sc);
        }
        s7_free(sc->sc);
        sc->sc = nullptr;
    }
    delete sc;
}

void scheme_set_input_port_file(scheme* sc, FILE* fin) {
    // Not directly supported in s7 — use s7_load for file loading
}

void scheme_set_input_port_string(scheme* sc, char* start, char* past_the_end) {
    // Not directly supported — use s7_eval_c_string instead
}

void scheme_set_output_port_file(scheme* sc, FILE* fout) {
    if (fout) {
        s7_pointer port = s7_open_output_file(sc->sc, "/dev/stdout", "w");
        s7_set_current_output_port(sc->sc, port);
    }
}

void scheme_set_output_port_string(scheme* sc, char* start, char* past_the_end) {
    sc->output_port = s7_open_output_string(sc->sc);
    s7_gc_protect(sc->sc, sc->output_port);
    s7_set_current_error_port(sc->sc, sc->output_port);
    sc->outport = sc->output_port;
}

static void eval_with_error_trap(scheme* sc, const char* str) {
    sc->retcode = 0;

    s7_pointer old_port = s7_current_error_port(sc->sc);
    s7_pointer err_port = s7_open_output_string(sc->sc);
    s7_int gc_loc = s7_gc_protect(sc->sc, err_port);
    s7_set_current_error_port(sc->sc, err_port);

    // s7_load_c_string iterates all top-level forms; s7_eval_c_string reads
    // only the first and silently discards the rest.
    s7_pointer result = s7_load_c_string(sc->sc, str, strlen(str));

    const char* errmsg = s7_get_output_string(sc->sc, err_port);
    if (errmsg && errmsg[0] != '\0') {
        sc->retcode = 1;
        printf("%s", errmsg);
        fflush(stdout);
    }

    s7_set_current_error_port(sc->sc, old_port);
    s7_gc_unprotect_at(sc->sc, gc_loc);

    sc->value = result ? result : sc->NIL;
}

void scheme_load_file(scheme* sc, FILE* fin) {
    if (!fin)
        return;

    std::string content;
    char buf[4096];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), fin)) > 0) {
        content.append(buf, n);
    }
    fclose(fin);

    uint64_t now = extemp::UNIV::TIME;
    sc->call_start_time = now;
    sc->call_end_time = now + sc->call_default_time * 10;

    sc->retcode = 0;
    s7_pointer old_port = s7_current_error_port(sc->sc);
    s7_pointer err_port = s7_open_output_string(sc->sc);
    s7_int gc_loc = s7_gc_protect(sc->sc, err_port);
    s7_set_current_error_port(sc->sc, err_port);

    s7_pointer result = s7_load_c_string(sc->sc, content.c_str(), content.size());

    const char* errmsg = s7_get_output_string(sc->sc, err_port);
    if (errmsg && errmsg[0] != '\0') {
        sc->retcode = 1;
        printf("%s", errmsg);
        fflush(stdout);
    }

    s7_set_current_error_port(sc->sc, old_port);
    s7_gc_unprotect_at(sc->sc, gc_loc);

    sc->value = result ? result : sc->NIL;
}

void scheme_load_string(scheme* sc, const char* cmd, uint64_t start_time, uint64_t end_time) {
    sc->call_start_time = start_time;
    sc->call_end_time = end_time;

    eval_with_error_trap(sc, cmd);
}

void scheme_apply0(scheme* sc, const char* name) {
    std::string expr = std::string("(") + name + ")";
    eval_with_error_trap(sc, expr.c_str());
}

pointer scheme_apply1(scheme* sc, const char* name, pointer arg) {
    s7_pointer sym = s7_make_symbol(sc->sc, name);
    s7_pointer func = s7_symbol_value(sc->sc, sym);
    s7_pointer args = s7_cons(sc->sc, arg, s7_nil(sc->sc));
    s7_pointer result = s7_call(sc->sc, func, args);
    sc->value = result;
    return result;
}

void scheme_call(scheme* sc, pointer func, pointer args, uint64_t start_time,
                 uint64_t call_duration) {
    sc->call_start_time = start_time;
    sc->call_end_time = start_time + call_duration;
    sc->retcode = 0;

    s7_pointer old_port = s7_current_error_port(sc->sc);
    s7_pointer err_port = s7_open_output_string(sc->sc);
    s7_int gc_loc = s7_gc_protect(sc->sc, err_port);
    s7_set_current_error_port(sc->sc, err_port);

    s7_pointer result = s7_call(sc->sc, func, args);

    const char* errmsg = s7_get_output_string(sc->sc, err_port);
    if (errmsg && errmsg[0] != '\0') {
        sc->retcode = 1;
        printf("%s", errmsg);
        fflush(stdout);
    }

    s7_set_current_error_port(sc->sc, old_port);
    s7_gc_unprotect_at(sc->sc, gc_loc);

    sc->value = result ? result : sc->NIL;
}

void scheme_call_without_stack_reset(scheme* sc, pointer func, pointer args) {
    scheme_call(sc, func, args, sc->call_start_time, sc->call_end_time - sc->call_start_time);
}

void scheme_set_external_data(scheme* sc, void* p) {
    sc->ext_data = p;
}

void scheme_define(scheme* sc, pointer env, pointer symbol, pointer value) {
    s7_define(sc->sc, env, symbol, value);
}

pointer find_slot_in_env(scheme* sc, pointer env, pointer sym, int all) {
    s7_pointer val = s7_symbol_local_value(sc->sc, sym, env);
    if (val == s7_undefined(sc->sc)) {
        if (all) {
            val = s7_symbol_value(sc->sc, sym);
        }
        if (val == s7_undefined(sc->sc)) {
            return sc->NIL;
        }
    }
    return s7_cons(sc->sc, sym, val);
}

void set_slot_in_env(scheme* sc, pointer slot, pointer value) {
    s7_pointer sym = s7_car(slot);
    s7_symbol_set_value(sc->sc, sym, value);
}

pointer slot_value_in_env(pointer slot) {
    return s7_cdr(slot);
}

void new_slot_in_env(scheme* sc, pointer variable, pointer value) {
    s7_define(sc->sc, s7_curlet(sc->sc), variable, value);
}

pointer reverse(scheme* sc, pointer a) {
    return s7_reverse(sc->sc, a);
}

pointer reverse_in_place(scheme* sc, pointer term, pointer list) {
    return s7_reverse(sc->sc, list);
}

pointer append(scheme* sc, pointer a, pointer b) {
    return s7_append(sc->sc, a, b);
}

pointer assoc_strcmp(scheme* sc, pointer key, pointer alist, bool all) {
    if (!s7_is_string(key) && !s7_is_symbol(key))
        return sc->F;
    const char* key_str = s7_is_string(key) ? s7_string(key) : s7_symbol_name(key);

    pointer result = sc->NIL;
    pointer last = sc->NIL;

    for (pointer p = alist; s7_is_pair(p); p = s7_cdr(p)) {
        pointer entry = s7_car(p);
        if (!s7_is_pair(entry))
            continue;
        pointer ekey = s7_car(entry);
        const char* ekey_str = nullptr;
        if (s7_is_symbol(ekey))
            ekey_str = s7_symbol_name(ekey);
        else if (s7_is_string(ekey))
            ekey_str = s7_string(ekey);
        else
            continue;

        if (strcmp(key_str, ekey_str) == 0) {
            if (!all)
                return entry;
            pointer node = s7_cons(sc->sc, entry, sc->NIL);
            if (result == sc->NIL) {
                result = node;
            } else {
                s7_set_cdr(last, node);
            }
            last = node;
        }
    }
    return (result == sc->NIL) ? sc->F : result;
}

pointer _cons(scheme* sc, pointer a, pointer b, int immutable) {
    pointer p = s7_cons(sc->sc, a, b);
    if (immutable)
        s7_set_immutable(sc->sc, p);
    return p;
}

pointer cons(scheme* sc, pointer a, pointer b) {
    return s7_cons(sc->sc, a, b);
}

pointer immutable_cons(scheme* sc, pointer a, pointer b) {
    pointer p = s7_cons(sc->sc, a, b);
    s7_set_immutable(sc->sc, p);
    return p;
}

pointer mk_integer(scheme* sc, long long num) {
    return s7_make_integer(sc->sc, num);
}
pointer mk_i64(scheme* sc, long long num) {
    return s7_make_integer(sc->sc, num);
}
pointer mk_i32(scheme* sc, int num) {
    return s7_make_integer(sc->sc, num);
}
pointer mk_i16(scheme* sc, short num) {
    return s7_make_integer(sc->sc, num);
}
pointer mk_i8(scheme* sc, char num) {
    return s7_make_integer(sc->sc, num);
}
pointer mk_i1(scheme* sc, bool num) {
    return s7_make_integer(sc->sc, num ? 1 : 0);
}
pointer mk_real(scheme* sc, double num) {
    return s7_make_real(sc->sc, num);
}
pointer mk_double(scheme* sc, double num) {
    return s7_make_real(sc->sc, num);
}
pointer mk_float(scheme* sc, float num) {
    return s7_make_real(sc->sc, static_cast<double>(num));
}

pointer mk_rational(scheme* sc, long long n, long long d) {
    return s7_make_ratio(sc->sc, n, d);
}

pointer mk_symbol(scheme* sc, const char* name) {
    return s7_make_symbol(sc->sc, name);
}

pointer gensym(scheme* sc) {
    static int counter = 0;
    char buf[64];
    snprintf(buf, sizeof(buf), "gensym_%d", counter++);
    return s7_make_symbol(sc->sc, buf);
}

pointer mk_string(scheme* sc, const char* str) {
    return s7_make_string(sc->sc, str);
}

pointer mk_counted_string(scheme* sc, const char* str, int len) {
    return s7_make_string_with_length(sc->sc, str, len);
}

pointer mk_character(scheme* sc, int c) {
    return s7_make_character(sc->sc, static_cast<uint8_t>(c));
}

pointer mk_foreign_func(scheme* sc, foreign_func f) {
    int slot = s_ffiCount.fetch_add(1, std::memory_order_relaxed);
    if (slot >= MAX_FFI_FUNCS) {
        printf("Too many FFI functions registered (slot %d, max %d)\n", slot, MAX_FFI_FUNCS);
        fflush(stdout);
        return sc->F;
    }
    s_ffiTable[slot].store(f, std::memory_order_release);
    return s7_make_function(sc->sc, "#<foreign>", s_trampolineTable[slot], 0, 0, true, nullptr);
}

pointer mk_cptr(scheme* sc, void* p) {
    return s7_make_c_pointer(sc->sc, p);
}

pointer mk_vector(scheme* sc, int len) {
    return s7_make_vector(sc->sc, len);
}

void fill_vector(scheme* sc, pointer vec, pointer obj) {
    s7_vector_fill(sc->sc, vec, obj);
}

pointer set_vector_elem(scheme* sc, pointer vec, int ielem, pointer a) {
    s7_vector_set(sc->sc, vec, ielem, a);
    return a;
}

pointer vector_elem(pointer vec, int ielem) {
    return s7_vector_elements(vec)[ielem];
}

long long vector_length(pointer vec) {
    return s7_vector_length(vec);
}

pointer mk_continuation(scheme* sc) {
    return s7_make_continuation(sc->sc);
}

pointer mk_closure(scheme* sc, pointer c, pointer e) {
    // Create a lambda from code c in environment e
    // This is a best-effort shim — closures are typically created by eval
    return s7_eval(sc->sc, s7_cons(sc->sc, s7_make_symbol(sc->sc, "lambda"), c), e);
}

void putstr(scheme* sc, const char* s) {
    s7_display(sc->sc, s7_make_string(sc->sc, s), s7_current_output_port(sc->sc));
}

void putcharacter(scheme* sc, int c) {
    s7_write_char(sc->sc, s7_make_character(sc->sc, static_cast<uint8_t>(c)),
                  s7_current_output_port(sc->sc));
}

int pointer_type(pointer p) {
    if (s7_is_string(p))
        return T_STRING;
    if (s7_is_number(p))
        return T_NUMBER;
    if (s7_is_symbol(p))
        return T_SYMBOL;
    if (s7_is_pair(p))
        return T_PAIR;
    if (s7_is_character(p))
        return T_CHARACTER;
    if (s7_is_vector(p))
        return T_VECTOR;
    if (s7_is_c_pointer(p))
        return T_CPTR;
    if (s7_is_boolean(p))
        return T_NUMBER;  // approximate
    if (s7_is_let(p))
        return T_ENVIRONMENT;
    return T_PAIR;  // fallback
}

const char* procname(pointer x) {
    // s7 doesn't expose proc names the same way
    return "#<procedure>";
}

int is_string(pointer p) {
    return s7_is_string(p);
}
int is_number(pointer p) {
    return s7_is_number(p);
}
int is_symbol(pointer p) {
    return s7_is_symbol(p);
}
int is_pair(pointer p) {
    return s7_is_pair(p);
}
int is_vector(pointer p) {
    return s7_is_vector(p);
}
int is_character(pointer p) {
    return s7_is_character(p);
}
int is_port(pointer) {
    return 0;
}  // s7 has ports but we don't expose them this way
int is_cptr(pointer p) {
    return s7_is_c_pointer(p);
}
int is_cptr_or_str(pointer p) {
    return s7_is_c_pointer(p) || s7_is_string(p);
}
int is_environment(pointer p) {
    return s7_is_let(p);
}
int is_integer(pointer p) {
    return s7_is_integer(p);
}
int is_real(pointer p) {
    return s7_is_number(p);
}
int is_rational(pointer p) {
    return s7_is_ratio(p);
}

int is_syntax(pointer p) {
    return s7_is_syntax(p);
}
int is_proc(pointer p) {
    return s7_is_procedure(p) && !s7_is_c_pointer(p);
}
int is_foreign(pointer p) {
    return s7_is_function(p);
}

int is_closure(pointer p) {
    return s7_is_procedure(p) && !s7_is_function(p) && !s7_is_c_pointer(p);
}
int is_macro(pointer) {
    return 0;
}  // TODO: s7_is_macro requires s7_scheme*

int is_continuation(pointer p) {
    // s7 doesn't have a simple is_continuation without sc
    return 0;
}

int is_promise(pointer) {
    return 0;
}

int is_immutable(pointer p) {
    return s7_is_immutable(p);
}
void setimmutable(pointer p) { /* need sc for s7_set_immutable */ }

pointer closure_code(pointer p) {
    // Can't call s7_lambda_body without sc — will be handled at call sites
    return nullptr;
}

pointer closure_env(pointer p) {
    // Can't call s7_funclet without sc — will be handled at call sites
    return nullptr;
}

pointer pair_car(pointer p) {
    return s7_car(p);
}
pointer pair_cdr(pointer p) {
    return s7_cdr(p);
}

pointer set_car(scheme* sc, pointer p, pointer q) {
    s7_set_car(p, q);
    return q;
}

pointer set_cdr(scheme* sc, pointer p, pointer q) {
    s7_set_cdr(p, q);
    return q;
}

int64_t ivalue(pointer p) {
    if (s7_is_integer(p))
        return s7_integer(p);
    if (s7_is_real(p))
        return static_cast<int64_t>(s7_real(p));
    if (s7_is_ratio(p))
        return s7_numerator(p) / s7_denominator(p);
    return 0;
}

double rvalue(pointer p) {
    if (s7_is_real(p) && !s7_is_ratio(p) && !s7_is_integer(p))
        return s7_real(p);
    if (s7_is_integer(p))
        return static_cast<double>(s7_integer(p));
    if (s7_is_ratio(p))
        return static_cast<double>(s7_numerator(p)) / s7_denominator(p);
    if (s7_is_number(p))
        return s7_real(p);
    return 0.0;
}

int64_t i64value(pointer p) {
    return ivalue(p);
}
int32_t i32value(pointer p) {
    return static_cast<int32_t>(ivalue(p));
}
int16_t i16value(pointer p) {
    return static_cast<int16_t>(ivalue(p));
}
int8_t i8value(pointer p) {
    return static_cast<int8_t>(ivalue(p));
}
bool i1value(scheme* sc, pointer p) {
    return p == sc->T;
}
double r64value(pointer p) {
    return rvalue(p);
}
float r32value(pointer p) {
    return static_cast<float>(rvalue(p));
}
long long charvalue(pointer p) {
    return s7_character(p);
}
int is_integer_extern(pointer p) {
    return s7_is_integer(p);
}

char* string_value(pointer p) {
    if (unlikely(!s7_is_string(p))) {
        throw ScmRuntimeError("Attempting to return a string from a non-string obj", p);
    }
    return const_cast<char*>(s7_string(p));
}

char* symname(pointer p) {
    return const_cast<char*>(s7_symbol_name(p));
}

char* syntaxname(pointer p) {
    return const_cast<char*>(s7_symbol_name(p));
}

void* cptr_value(pointer p) {
    if (s7_is_string(p))
        return const_cast<char*>(s7_string(p));
    return s7_c_pointer(p);
}

pointer list_ref(scheme* sc, int pos, pointer a) {
    return s7_list_ref(sc->sc, a, pos);
}

int eqv(pointer a, pointer b) {
    return s7_is_eq(a, b);
}

void load_file(scheme* sc, FILE* fin) {
    scheme_load_file(sc, fin);
}

void load_string(scheme* sc, const char* input) {
    uint64_t now = extemp::UNIV::TIME;
    scheme_load_string(sc, input, now, now + sc->call_default_time);
}

unsigned long string_hash(const char* s) {
    unsigned long hash = 5381;
    int c;
    while ((c = *s++))
        hash = ((hash << 5) + hash) + c;
    return hash;
}
