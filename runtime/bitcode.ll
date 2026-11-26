;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPE DEFINITIONS

; Zone and closure variable table types (for ORC JIT symbol resolution)
%mzone = type {ptr, i64, i64, i64, ptr, ptr}
%clsvar = type {ptr, i32, ptr, ptr}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTERNAL RUNTIME FUNCTION DECLARATIONS

; Closure address table functions (implemented in C++)
declare ptr @add_address_table(ptr, ptr, i32, ptr, i32, ptr) nounwind
declare ptr @get_address_table(ptr, ptr) nounwind
declare i32 @get_address_offset(i64, ptr) nounwind
declare i1 @check_address_type(i64, ptr, ptr) nounwind
declare i1 @check_address_exists(i64, ptr) nounwind

; Zone memory management functions (implemented in C++)
declare ptr @llvm_zone_callback_setup() nounwind
declare ptr @llvm_pop_zone_stack() nounwind
declare void @llvm_zone_destroy(ptr) nounwind
declare void @llvm_zone_print(ptr) nounwind
declare ptr @llvm_zone_malloc(ptr, i64) nounwind
declare ptr @llvm_zone_malloc_from_current_zone(i64) nounwind
declare i1 @llvm_ptr_in_zone(ptr, ptr) nounwind
declare i1 @llvm_zone_copy_ptr(ptr, ptr) nounwind
declare i64 @llvm_zone_ptr_size(ptr) nounwind
declare i1 @llvm_ptr_in_current_zone(ptr) nounwind
declare void @llvm_destroy_zone_after_delay(ptr, i64)

; Scheme value constructor functions (implemented in C++)
; The narrow integers carry the C ABI's extension: without it the bits above
; the value are undefined, and C++ reads a true i1 as whatever byte it gets.
declare ptr @mk_i64(ptr, i64)
declare ptr @mk_i32(ptr, i32)
declare ptr @mk_i16(ptr, i16 signext)
declare ptr @mk_i8(ptr, i8 signext)
declare ptr @mk_i1(ptr, i1 zeroext)
declare ptr @mk_double(ptr, double)
declare ptr @mk_float(ptr, float)
declare ptr @mk_string(ptr, ptr)
declare ptr @mk_cptr(ptr, ptr)

; Scheme value accessor functions (implemented in C++)
declare i64 @i64value(ptr)
declare i32 @i32value(ptr)
declare i16 @i16value(ptr)
declare i8 @i8value(ptr)
declare i1 @i1value(ptr)
declare double @r64value(ptr)
declare float @r32value(ptr)
declare ptr @string_value(ptr)
declare ptr @cptr_value(ptr)

; Encoding/decoding utility functions (implemented in C++)
declare ptr @base64_encode(ptr, i64, ptr) nounwind
declare ptr @base64_decode(ptr, i64, ptr) nounwind
declare ptr @cname_encode(ptr, i64, ptr) nounwind
declare ptr @cname_decode(ptr, i64, ptr) nounwind

; Standard C library math functions
declare i64 @llabs(i64) nounwind
declare float @sinhf(float) nounwind
declare float @tanf(float) nounwind
declare float @tanhf(float) nounwind

; Standard C library file I/O functions
declare i32 @remove(ptr) nounwind

declare ptr @list_ref(ptr, i32, ptr)

; System functions
declare ptr @sys_sharedir() nounwind
declare ptr @sys_slurp_file(ptr) nounwind

; Standard C library functions
declare ptr @malloc(i64) nounwind
declare ptr @realloc(ptr, i64) nounwind
declare void @free(ptr) nounwind
declare ptr @memset(ptr, i32, i64) nounwind
declare ptr @memcpy(ptr, ptr, i64) nounwind
declare i32 @memcmp(ptr, ptr, i64) nounwind
declare i32 @putchar(i32) nounwind
declare i64 @strlen(ptr) nounwind
declare ptr @strcpy(ptr, ptr) nounwind
declare ptr @strncpy(ptr, ptr, i64) nounwind
declare ptr @strcat(ptr, ptr) nounwind
declare ptr @strncat(ptr, ptr, i64) nounwind
declare i32 @strcmp(ptr, ptr) nounwind
declare i32 @strncmp(ptr, ptr, i64) nounwind
declare ptr @strchr(ptr, i32) nounwind
declare ptr @strstr(ptr, ptr) nounwind

; Extempore runtime functions (implemented in C++)
declare i1 @rmatch(ptr, ptr) nounwind
declare i64 @rmatches(ptr, ptr, ptr, i64) nounwind
declare i1 @rsplit(ptr, ptr, ptr, ptr, i64) nounwind
declare ptr @rreplace(ptr, ptr, ptr, ptr, i64) nounwind

; Random number generators (implemented in C++)
declare double @xtc_randd() nounwind
declare float @xtc_randf() nounwind
declare i64 @xtc_rand1_i64(i64) nounwind
declare i64 @xtc_rand2_i64(i64, i64) nounwind
declare i32 @xtc_rand1_i32(i32) nounwind
declare i32 @xtc_rand2_i32(i32, i32) nounwind
declare double @xtc_rand1_d(double) nounwind
declare double @xtc_rand2_d(double, double) nounwind
declare float @xtc_rand1_f(float) nounwind
declare float @xtc_rand2_f(float, float) nounwind

; Standard math library functions
declare double @atan2(double, double) nounwind
declare float @atan2f(float, float) nounwind

; Additional Extempore runtime functions (implemented in C++)
; Note: Many zone/inline functions are defined in inline.ll or later in this file
declare ptr @llvm_get_function_ptr(ptr) nounwind
declare void @llvm_runtime_error(i64, ptr) nounwind
declare void @llvm_print_pointer(ptr) nounwind
declare void @llvm_print_i32(i32) nounwind
declare void @llvm_print_i64(i64) nounwind
declare void @llvm_print_f32(float) nounwind
declare void @llvm_print_f64(double) nounwind
declare void @log_error(ptr) nounwind
declare void @log_warn(ptr) nounwind
declare void @log_info(ptr) nounwind
declare void @log_debug(ptr) nounwind
declare ptr @extitoa(i64) nounwind
declare i64 @string_hash(ptr) nounwind
declare void @llvm_schedule_callback(i64, ptr) nounwind
declare void @llvm_send_udp(ptr, i32, ptr, i32) nounwind
declare i64 @next_prime(i64) nounwind
declare void @free_after_delay(ptr, double) nounwind
declare ptr @llvm_disassemble(ptr, i32) nounwind

; Thread functions
declare ptr @thread_fork(ptr, ptr) nounwind
declare void @thread_destroy(ptr) nounwind
declare i32 @thread_join(ptr) nounwind
declare i32 @thread_kill(ptr) nounwind
declare ptr @thread_self() nounwind
declare i32 @thread_equal_self(ptr) nounwind
declare i32 @thread_equal(ptr, ptr) nounwind
declare i64 @thread_sleep(i64, i64) nounwind
declare ptr @mutex_create() nounwind
declare i32 @mutex_destroy(ptr) nounwind
declare i32 @mutex_lock(ptr) nounwind
declare i32 @mutex_unlock(ptr) nounwind
declare i32 @mutex_trylock(ptr) nounwind

; Clock functions
declare double @clock_clock() nounwind
declare double @clock_offset() nounwind
declare double @audio_clock_base() nounwind
declare double @audio_clock_now() nounwind

; Byte-swap functions (OSC, network byte order)
declare i64 @swap64f(double) nounwind
declare double @unswap64f(i64) nounwind
declare i32 @swap32f(float) nounwind
declare float @unswap32f(i32) nounwind
declare i64 @swap64i(i64) nounwind
declare i64 @unswap64i(i64) nounwind
declare i32 @swap32i(i32) nounwind
declare i32 @unswap32i(i32) nounwind

; Callback registration
declare i32 @register_for_window_events() nounwind

; 16-byte aligned memory
declare ptr @malloc16(i64) nounwind
declare void @free16(ptr) nounwind

; Standard C library - math
declare double @acos(double) nounwind
declare double @asin(double) nounwind
declare double @atan(double) nounwind
declare double @sinh(double) nounwind
declare double @cosh(double) nounwind
declare double @tanh(double) nounwind
declare double @tan(double) nounwind
declare double @trunc(double) nounwind
declare double @acosh(double) nounwind
declare double @asinh(double) nounwind
declare double @atanh(double) nounwind
declare double @cbrt(double) nounwind
declare double @copysign(double, double) nounwind
declare double @erf(double) nounwind
declare double @erfc(double) nounwind
declare double @expm1(double) nounwind
declare double @fdim(double, double) nounwind
declare double @fmax(double, double) nounwind
declare double @fmin(double, double) nounwind
declare double @fmod(double, double) nounwind
declare double @hypot(double, double) nounwind
declare double @lgamma(double) nounwind
declare double @log1p(double) nounwind
declare double @nan(ptr) nounwind
declare double @nextafter(double, double) nounwind
declare double @remainder(double, double) nounwind
declare double @scalbn(double, i32) nounwind
declare double @tgamma(double) nounwind
declare float @acosf(float) nounwind
declare float @asinf(float) nounwind
declare float @atanf(float) nounwind
declare float @coshf(float) nounwind
declare float @acoshf(float) nounwind
declare float @asinhf(float) nounwind
declare float @atanhf(float) nounwind
declare float @cbrtf(float) nounwind
declare float @copysignf(float, float) nounwind
declare float @erff(float) nounwind
declare float @erfcf(float) nounwind
declare float @expm1f(float) nounwind
declare float @fdimf(float, float) nounwind
declare float @fmaxf(float, float) nounwind
declare float @fminf(float, float) nounwind
declare float @fmodf(float, float) nounwind
declare float @hypotf(float, float) nounwind
declare float @lgammaf(float) nounwind
declare float @log1pf(float) nounwind
declare float @log2f(float) nounwind
declare float @nanf(ptr) nounwind
declare float @nextafterf(float, float) nounwind
declare float @remainderf(float, float) nounwind
declare float @scalbnf(float, i32) nounwind
declare float @tgammaf(float) nounwind
declare i32 @abs(i32) nounwind
declare i64 @llrint(double) nounwind
declare i64 @llrintf(float) nounwind
declare i64 @llround(double) nounwind
declare i64 @llroundf(float) nounwind

; Standard C library - string/memory
declare ptr @strdup(ptr) nounwind
declare ptr @strrchr(ptr, i32) nounwind
declare ptr @strpbrk(ptr, ptr) nounwind
declare ptr @strtok(ptr, ptr) nounwind
declare ptr @strerror(i32) nounwind
declare i64 @strcspn(ptr, ptr) nounwind
declare i64 @strspn(ptr, ptr) nounwind
declare ptr @memmove(ptr, ptr, i64) nounwind
declare ptr @memchr(ptr, i32, i64) nounwind

; Standard C library - conversion
declare double @atof(ptr) nounwind
declare i32 @atoi(ptr) nounwind
declare i64 @atol(ptr) nounwind

; Standard C library - file I/O
declare ptr @fopen(ptr, ptr) nounwind
declare i32 @fclose(ptr) nounwind
declare i64 @fread(ptr, i64, i64, ptr) nounwind
declare i64 @fwrite(ptr, i64, i64, ptr) nounwind
declare ptr @fgets(ptr, i32, ptr) nounwind
declare i32 @fputc(i32, ptr) nounwind
declare i32 @fputs(ptr, ptr) nounwind
declare i32 @fgetc(ptr) nounwind
declare i32 @feof(ptr) nounwind
declare i32 @ferror(ptr) nounwind
declare i32 @fflush(ptr) nounwind
declare i32 @fseek(ptr, i64, i32) nounwind
declare i64 @ftell(ptr) nounwind
declare void @rewind(ptr) nounwind
declare i32 @fileno(ptr) nounwind

; Standard C library - process/system
declare void @abort() nounwind
declare void @exit(i32) nounwind
declare i32 @system(ptr) nounwind
declare ptr @getenv(ptr) nounwind
declare i32 @setenv(ptr, ptr, i32) nounwind
declare i32 @raise(i32) nounwind
declare i32 @rand() nounwind
declare ptr @calloc(i64, i64) nounwind
declare ptr @dlsym(ptr, ptr) nounwind
declare i32 @puts(ptr) nounwind

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCHEME STUFF

define private ptr @impc_null() nounwind alwaysinline
{
ret ptr null
}

define private i1 @impc_true() nounwind alwaysinline
{
ret i1 1
}

define private i1 @impc_false() nounwind alwaysinline
{
ret i1 0
}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATOMICS
;;
;; For a value one thread writes and another reads, such as the indices of a
;; single-writer, single-reader ring buffer: the writer fills a slot and then
;; stores the index, and a reader which loads that index sees the slot filled.

define private i64 @atomic_load_i64(ptr %p) nounwind alwaysinline
{
%v = load atomic i64, ptr %p acquire, align 8
ret i64 %v
}

define private void @atomic_store_i64(ptr %p, i64 %v) nounwind alwaysinline
{
store atomic i64 %v, ptr %p release, align 8
ret void
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CASTING STUFF

define private i64 @i1toi64(i1 %a) alwaysinline
{
%return = zext i1 %a to i64
ret i64 %return
}

define private i32 @i1toi32(i1 %a) alwaysinline
{
%return = zext i1 %a to i32
ret i32 %return
}

define private i16 @i1toi16(i1 %a) alwaysinline
{
%return = zext i1 %a to i16
ret i16 %return
}

define private i8 @i1toi8(i1 %a) alwaysinline
{
%return = zext i1 %a to i8
ret i8 %return
}

define private i1 @i64toi1(i64 %a) alwaysinline
{
entry:
%return = trunc i64 %a to i1
ret i1 %return
}

define private i1 @i32toi1(i32 %a) alwaysinline
{
%return = trunc i32 %a to i1
ret i1 %return
}

define private i1 @i16toi1(i16 %a) alwaysinline
{
%return = trunc i16 %a to i1
ret i1 %return
}

define private i1 @i8toi1(i8 %a) alwaysinline
{
%return = trunc i8 %a to i1
ret i1 %return
}

;; i8 casts
define private i64 @i8toi64(i8 %a) alwaysinline
{
%return = sext i8 %a to i64
ret i64 %return
}

define private i64 @i8toui64(i8 %a) alwaysinline
{
%return = zext i8 %a to i64
ret i64 %return
}

define private i32 @i8toi32(i8 %a) alwaysinline
{
%return = sext i8 %a to i32
ret i32 %return
}

define private i16 @i8toi16(i8 %a) alwaysinline
{
%return = sext i8 %a to i16
ret i16 %return
}

define private i32 @i8toui32(i8 %a) alwaysinline
{
%return = zext i8 %a to i32
ret i32 %return
}

define private i8 @i64toi8(i64 %a) alwaysinline
{
%return = trunc i64 %a to i8
ret i8 %return
}

define private i8 @i32toi8(i32 %a) alwaysinline
{
%return = trunc i32 %a to i8
ret i8 %return
}

define private i8 @i16toi8(i16 %a) alwaysinline
{
%return = trunc i16 %a to i8
ret i8 %return
}

;; i16 casts
define private i64 @i16toi64(i16 %a) alwaysinline
{
%return = sext i16 %a to i64
ret i64 %return
}

define private i64 @i16toui64(i16 %a) alwaysinline
{
%return = zext i16 %a to i64
ret i64 %return
}

define private i32 @i16toi32(i16 %a) alwaysinline
{
%return = sext i16 %a to i32
ret i32 %return
}

define private i32 @i16toui32(i16 %a) alwaysinline
{
%return = zext i16 %a to i32
ret i32 %return
}

define private i16 @i64toi16(i64 %a) alwaysinline
{
entry:
%return = trunc i64 %a to i16
ret i16 %return
}

define private i16 @i32toi16(i32 %a) alwaysinline
{
%return = trunc i32 %a to i16
ret i16 %return
}

;; i32 casts
define private i64 @i32toi64(i32 %a) alwaysinline
{
%return = sext i32 %a to i64
ret i64 %return
}

define private i64 @i32toui64(i32 %a) alwaysinline
{
%return = zext i32 %a to i64
ret i64 %return
}

define private i32 @i64toi32(i64 %a) alwaysinline
{
entry:
%return = trunc i64 %a to i32
ret i32 %return
}


;; float casts
define private double @ftod(float %a) alwaysinline
{
%return = fpext float %a to double
ret double %return
}

define private i64 @ftoi64(float %a) alwaysinline
{
%return = fptosi float %a to i64
ret i64 %return
}

define private i32 @ftoi32(float %a) alwaysinline
{
%return = fptosi float %a to i32
ret i32 %return
}

define private i16 @ftoi16(float %a) alwaysinline
{
%return = fptosi float %a to i16
ret i16 %return
}

define private i8 @ftoi8(float %a) alwaysinline
{
%return = fptosi float %a to i8
ret i8 %return
}

define private i1 @ftoi1(float %a) alwaysinline
{
%return = fptosi float %a to i1
ret i1 %return
}

define private i64 @ftoui64(float %a) alwaysinline
{
%return = fptoui float %a to i64
ret i64 %return
}

define private i32 @ftoui32(float %a) alwaysinline
{
%return = fptoui float %a to i32
ret i32 %return
}

define private i16 @ftoui16(float %a) alwaysinline
{
%return = fptoui float %a to i16
ret i16 %return
}

define private i8 @ftoui8(float %a) alwaysinline
{
%return = fptoui float %a to i8
ret i8 %return
}

define private i1 @ftoui1(float %a) alwaysinline
{
%return = fptoui float %a to i1
ret i1 %return
}

define private float @i64tof(i64 %a) alwaysinline
{
entry:
%return = sitofp i64 %a to float
ret float %return
}

define private float @i32tof(i32 %a) alwaysinline
{
%return = sitofp i32 %a to float
ret float %return
}

define private float @i16tof(i16 %a) alwaysinline
{
%return = sitofp i16 %a to float
ret float %return
}

define private float @i8tof(i8 %a) alwaysinline
{
%return = sitofp i8 %a to float
ret float %return
}

define private float @i1tof(i1 %a) alwaysinline
{
%return = sitofp i1 %a to float
ret float %return
}

define private float @ui64tof(i64 %a) alwaysinline
{
entry:
%return = uitofp i64 %a to float
ret float %return
}

define private float @ui32tof(i32 %a) alwaysinline
{
entry:
%return = uitofp i32 %a to float
ret float %return
}

define private float @ui16tof(i16 %a) alwaysinline
{
entry:
%return = uitofp i16 %a to float
ret float %return
}

define private float @ui8tof(i8 %a) alwaysinline
{
entry:
%return = uitofp i8 %a to float
ret float %return
}

define private float @ui1tof(i1 %a) alwaysinline
{
entry:
%return = uitofp i1 %a to float
ret float %return
}

;; double casts

define private float @dtof(double %a) alwaysinline
{
%return = fptrunc double %a to float
ret float %return
}

define private i64 @dtoi64(double %a) alwaysinline
{
%return = fptosi double %a to i64
ret i64 %return
}

define private i32 @dtoi32(double %a) alwaysinline
{
%return = fptosi double %a to i32
ret i32 %return
}

define private i16 @dtoi16(double %a) alwaysinline
{
%return = fptosi double %a to i16
ret i16 %return
}

define private i8 @dtoi8(double %a) alwaysinline
{
%return = fptosi double %a to i8
ret i8 %return
}

define private i1 @dtoi1(double %a) alwaysinline
{
%return = fptosi double %a to i1
ret i1 %return
}

define private i64 @dtoui64(double %a) alwaysinline
{
%return = fptoui double %a to i64
ret i64 %return
}

define private i32 @dtoui32(double %a) alwaysinline
{
%return = fptoui double %a to i32
ret i32 %return
}

define private i16 @dtoui16(double %a) alwaysinline
{
%return = fptoui double %a to i16
ret i16 %return
}

define private i8 @dtoui8(double %a) alwaysinline
{
%return = fptoui double %a to i8
ret i8 %return
}

define private i1 @dtoui1(double %a) alwaysinline
{
%return = fptoui double %a to i1
ret i1 %return
}

define private double @i64tod(i64 %a) alwaysinline
{
entry:
%return = sitofp i64 %a to double
ret double %return
}

define private double @i32tod(i32 %a) alwaysinline
{
%return = sitofp i32 %a to double
ret double %return
}

define private double @i16tod(i16 %a) alwaysinline
{
%return = sitofp i16 %a to double
ret double %return
}

define private double @i8tod(i8 %a) alwaysinline
{
%return = sitofp i8 %a to double
ret double %return
}

define private double @i1tod(i1 %a) alwaysinline
{
%return = sitofp i1 %a to double
ret double %return
}

define private double @ui64tod(i64 %a) alwaysinline
{
entry:
%return = uitofp i64 %a to double
ret double %return
}

define private double @ui32tod(i32 %a) alwaysinline
{
entry:
%return = uitofp i32 %a to double
ret double %return
}

define private double @ui16tod(i16 %a) alwaysinline
{
entry:
%return = uitofp i16 %a to double
ret double %return
}

define private double @ui8tod(i8 %a) alwaysinline
{
entry:
%return = uitofp i8 %a to double
ret double %return
}

define private double @ui1tod(i1 %a) alwaysinline
{
entry:
%return = uitofp i1 %a to double
ret double %return
}

define private i64 @ptrtoi64(ptr %a) alwaysinline
{
entry:
%return = ptrtoint ptr %a to i64
ret i64 %return
}

define private ptr @i64toptr(i64 %a) alwaysinline
{
entry:
%return = inttoptr i64 %a to ptr
ret ptr %return
}

define private i32 @ptrtoi32(ptr %a) alwaysinline
{
entry:
%return = ptrtoint ptr %a to i32
ret i32 %return
}

define private i16 @ptrtoi16(ptr %a) alwaysinline
{
entry:
%return = ptrtoint ptr %a to i16
ret i16 %return
}

define private ptr @i32toptr(i32 %a) alwaysinline
{
%return = inttoptr i32 %a to ptr
ret ptr %return
}

define private ptr @i16toptr(i16 %a) alwaysinline
{
%return = inttoptr i16 %a to ptr
ret ptr %return
}

; Portable 80-bit extended precision to double conversion
; Works on both x86_64 and ARM64 by manually parsing IEEE 754 extended format
; The 80-bit format is: 1 sign bit, 15 exponent bits, 64 mantissa bits (explicit integer bit)
; Input is big-endian (as used in AIFF files)
declare double @fp80_to_double_portable(ptr) nounwind

define private double @fp80ptrtod(ptr %fp80ptr)
{
entry:
%result = call double @fp80_to_double_portable(ptr %fp80ptr)
ret double %result
}

declare i32 @printf(ptr noalias nocapture, ...)
declare i32 @sprintf(ptr, ptr noalias nocapture, ...)
declare i32 @sscanf(ptr, ptr noalias nocapture, ...)
declare i32 @fprintf(ptr, ptr noalias nocapture, ...)
declare i32 @fscanf(ptr, ptr noalias nocapture, ...)

;; scheme helpers

declare i32 @is_string(ptr) nounwind
declare i32 @is_real(ptr) nounwind
declare i32 @is_cptr(ptr) nounwind
declare i32 @is_cptr_or_str(ptr) nounwind

define private void @llvm_zone_ptr_set_size(ptr %zone, i64 %size) nounwind alwaysinline
{
%ptr = bitcast ptr %zone to ptr
%size_ptr = getelementptr i64, ptr %ptr, i32 -1
store i64 %size, ptr %size_ptr
ret void
}

@TIME = external global i64 ; extemp::UNIV::TIME
define private i64 @llvm_now() nounwind alwaysinline
{
%res = load i64, ptr @TIME
ret i64 %res
}

declare void @ascii_text_color_extern(i32 %bold, i32 %fg, i32 %bg)
define private void @ascii_text_color(i32 %bold, i32 %fg, i32 %bg) nounwind alwaysinline "thunk"
{
call void @ascii_text_color_extern(i32 %bold, i32 %fg, i32 %bg)
ret void
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ZONE MANAGEMENT INLINE FUNCTIONS
;; (merged from inline.ll)

define private ptr @new_address_table() nounwind alwaysinline
{
ret ptr null
}

declare ptr @llvm_peek_zone_stack_extern() nounwind
define private ptr @llvm_peek_zone_stack() nounwind alwaysinline "thunk"
{
%zone = call ptr @llvm_peek_zone_stack_extern()
ret ptr %zone
}

declare void @llvm_push_zone_stack_extern(ptr) nounwind
define private void @llvm_push_zone_stack(ptr %zone) nounwind alwaysinline "thunk"
{
call void @llvm_push_zone_stack_extern(ptr %zone)
ret void
}

declare ptr @llvm_zone_create_extern(i64) nounwind
define private ptr @llvm_zone_create(i64 %size) nounwind alwaysinline "thunk"
{
%zone = call ptr @llvm_zone_create_extern(i64 %size)
ret ptr %zone
}

define private void @llvm_zone_mark(ptr %zone) nounwind alwaysinline
{
%offset_ptr = getelementptr inbounds %mzone, ptr %zone, i32 0, i32 1
%offset_val = load i64, ptr %offset_ptr
%mark_ptr = getelementptr %mzone, ptr %zone, i32 0, i32 2
store i64 %offset_val, ptr %mark_ptr
ret void
}

define private i64 @llvm_zone_mark_size(ptr %zone) nounwind alwaysinline
{
%offset_ptr = getelementptr inbounds %mzone, ptr %zone, i32 0, i32 1
%offset_val = load i64, ptr %offset_ptr
%mark_ptr = getelementptr %mzone, ptr %zone, i32 0, i32 2
%mark_val = load i64, ptr %mark_ptr
%res = sub i64 %offset_val, %mark_val
ret i64 %res
}

define private ptr @llvm_zone_reset(ptr %zone) nounwind alwaysinline
{
%offset_ptr = getelementptr inbounds %mzone, ptr %zone, i32 0, i32 1
store i64 0, ptr %offset_ptr
ret ptr %zone
}

declare i32 @is_integer_extern(ptr)
declare i32 @is_integer(ptr) nounwind
