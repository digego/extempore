;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPE DEFINITIONS

; Zone and closure variable table types (for ORC JIT symbol resolution)
%mzone = type {i8*, i64, i64, i64, i8*, %mzone*}
%clsvar = type {i8*, i32, i8*, %clsvar*}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTERNAL RUNTIME FUNCTION DECLARATIONS

; Closure address table functions (implemented in C++)
declare %clsvar* @add_address_table(%mzone*, i8*, i32, i8*, i32, %clsvar*) nounwind
declare %clsvar* @get_address_table(i8*, %clsvar*) nounwind
declare i32 @get_address_offset(i64, %clsvar*) nounwind
declare i1 @check_address_type(i64, %clsvar*, i8*) nounwind
declare i1 @check_address_exists(i64, %clsvar*) nounwind

; Zone memory management functions (implemented in C++)
declare %mzone* @llvm_zone_callback_setup() nounwind
declare %mzone* @llvm_pop_zone_stack() nounwind
declare void @llvm_zone_destroy(%mzone*) nounwind
declare void @llvm_zone_print(%mzone*) nounwind
declare i8* @llvm_zone_malloc(%mzone*, i64) nounwind
declare i8* @llvm_zone_malloc_from_current_zone(i64) nounwind
declare i1 @llvm_ptr_in_zone(%mzone*, i8*) nounwind
declare i1 @llvm_zone_copy_ptr(i8*, i8*) nounwind
declare i64 @llvm_zone_ptr_size(i8*) nounwind
declare i1 @llvm_ptr_in_current_zone(i8*) nounwind
declare void @llvm_destroy_zone_after_delay(%mzone*, i64)

; Scheme value constructor functions (implemented in C++)
declare i8* @mk_i64(i8*, i64)
declare i8* @mk_i32(i8*, i32)
declare i8* @mk_i16(i8*, i16)
declare i8* @mk_i8(i8*, i8)
declare i8* @mk_i1(i8*, i1)
declare i8* @mk_double(i8*, double)
declare i8* @mk_float(i8*, float)
declare i8* @mk_string(i8*, i8*)
declare i8* @mk_cptr(i8*, i8*)

; Scheme value accessor functions (implemented in C++)
declare i64 @i64value(i8*)
declare i32 @i32value(i8*)
declare i16 @i16value(i8*)
declare i8 @i8value(i8*)
declare i1 @i1value(i8*)
declare double @r64value(i8*)
declare float @r32value(i8*)
declare i8* @string_value(i8*)
declare i8* @cptr_value(i8*)

; Encoding/decoding utility functions (implemented in C++)
declare i8* @base64_encode(i8*, i64, i64*) nounwind
declare i8* @base64_decode(i8*, i64, i64*) nounwind
declare i8* @cname_encode(i8*, i64, i64*) nounwind
declare i8* @cname_decode(i8*, i64, i64*) nounwind

; Standard C library math functions
declare i64 @llabs(i64) nounwind
declare float @sinhf(float) nounwind
declare float @tanf(float) nounwind
declare float @tanhf(float) nounwind

; Standard C library file I/O functions
declare i32 @remove(i8*) nounwind

declare i8* @list_ref(i8*, i32, i8*)

; System functions
declare i8* @sys_sharedir() nounwind
declare i8* @sys_slurp_file(i8*) nounwind

; Standard C library functions
declare i8* @malloc(i64) nounwind
declare i8* @realloc(i8*, i64) nounwind
declare void @free(i8*) nounwind
declare i8* @memset(i8*, i32, i64) nounwind
declare i8* @memcpy(i8*, i8*, i64) nounwind
declare i32 @memcmp(i8*, i8*, i64) nounwind
declare i32 @putchar(i32) nounwind
declare i64 @strlen(i8*) nounwind
declare i8* @strcpy(i8*, i8*) nounwind
declare i8* @strncpy(i8*, i8*, i64) nounwind
declare i8* @strcat(i8*, i8*) nounwind
declare i8* @strncat(i8*, i8*, i64) nounwind
declare i32 @strcmp(i8*, i8*) nounwind
declare i32 @strncmp(i8*, i8*, i64) nounwind
declare i8* @strchr(i8*, i32) nounwind
declare i8* @strstr(i8*, i8*) nounwind

; Extempore runtime functions (implemented in C++)
declare i1 @rmatch(i8*, i8*) nounwind
declare i64 @rmatches(i8*, i8*, i8**, i64) nounwind
declare i8** @rsplit(i8*, i8*, i8**, i64) nounwind
declare i8* @rreplace(i8*, i8*, i8*) nounwind

; Random number generators (implemented in C++)
declare double @imp_randd() nounwind
declare float @imp_randf() nounwind
declare i64 @imp_rand1_i64(i64) nounwind
declare i64 @imp_rand2_i64(i64, i64) nounwind
declare i32 @imp_rand1_i32(i32) nounwind
declare i32 @imp_rand2_i32(i32, i32) nounwind
declare double @imp_rand1_d(double) nounwind
declare double @imp_rand2_d(double, double) nounwind
declare float @imp_rand1_f(float) nounwind
declare float @imp_rand2_f(float, float) nounwind

; Standard math library functions
declare double @atan2(double, double) nounwind
declare float @atan2f(float, float) nounwind

; Additional Extempore runtime functions (implemented in C++)
; Note: Many zone/inline functions are defined in inline.ll or later in this file
declare i8* @llvm_get_function_ptr(i8*) nounwind
declare void @llvm_runtime_error(i64, i8*) nounwind
declare void @llvm_print_pointer(i8*) nounwind
declare void @llvm_print_i32(i32) nounwind
declare void @llvm_print_i64(i64) nounwind
declare void @llvm_print_f32(float) nounwind
declare void @llvm_print_f64(double) nounwind
declare i8* @extitoa(i64) nounwind
declare i64 @string_hash(i8*) nounwind
declare void @llvm_schedule_callback(i64, i8*) nounwind
declare void @llvm_send_udp(i8*, i32, i8*, i32) nounwind
declare i64 @next_prime(i64) nounwind
declare void @free_after_delay(i8*, double) nounwind
declare i8* @llvm_disassemble(i8*, i32) nounwind

; Thread functions
declare i8* @thread_fork(i8*, i8*) nounwind
declare void @thread_destroy(i8*) nounwind
declare i32 @thread_join(i8*) nounwind
declare i32 @thread_kill(i8*) nounwind
declare i8* @thread_self() nounwind
declare i32 @thread_equal_self(i8*) nounwind
declare i32 @thread_equal(i8*, i8*) nounwind
declare i64 @thread_sleep(i64, i64) nounwind
declare i8* @mutex_create() nounwind
declare i32 @mutex_destroy(i8*) nounwind
declare i32 @mutex_lock(i8*) nounwind
declare i32 @mutex_unlock(i8*) nounwind
declare i32 @mutex_trylock(i8*) nounwind

; Clock functions
declare double @clock_clock() nounwind
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
declare void @xtm_set_main_callback(i8*) nounwind
declare i32 @register_for_window_events() nounwind

; 16-byte aligned memory
declare i8* @malloc16(i64) nounwind
declare void @free16(i8*) nounwind

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
declare double @nan(i8*) nounwind
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
declare float @nanf(i8*) nounwind
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
declare i8* @strdup(i8*) nounwind
declare i8* @strrchr(i8*, i32) nounwind
declare i8* @strpbrk(i8*, i8*) nounwind
declare i8* @strtok(i8*, i8*) nounwind
declare i8* @strerror(i32) nounwind
declare i64 @strcspn(i8*, i8*) nounwind
declare i64 @strspn(i8*, i8*) nounwind
declare i8* @memmove(i8*, i8*, i64) nounwind
declare i8* @memchr(i8*, i32, i64) nounwind

; Standard C library - conversion
declare double @atof(i8*) nounwind
declare i32 @atoi(i8*) nounwind
declare i64 @atol(i8*) nounwind

; Standard C library - file I/O
declare i8* @fopen(i8*, i8*) nounwind
declare i32 @fclose(i8*) nounwind
declare i64 @fread(i8*, i64, i64, i8*) nounwind
declare i64 @fwrite(i8*, i64, i64, i8*) nounwind
declare i8* @fgets(i8*, i32, i8*) nounwind
declare i32 @fputc(i32, i8*) nounwind
declare i32 @fputs(i8*, i8*) nounwind
declare i32 @fgetc(i8*) nounwind
declare i32 @feof(i8*) nounwind
declare i32 @ferror(i8*) nounwind
declare i32 @fflush(i8*) nounwind
declare i32 @fseek(i8*, i64, i32) nounwind
declare i64 @ftell(i8*) nounwind
declare void @rewind(i8*) nounwind
declare i32 @fileno(i8*) nounwind

; Standard C library - process/system
declare void @abort() nounwind
declare void @exit(i32) nounwind
declare i32 @system(i8*) nounwind
declare i8* @getenv(i8*) nounwind
declare i32 @setenv(i8*, i8*, i32) nounwind
declare i32 @raise(i32) nounwind
declare i32 @rand() nounwind
declare i8* @calloc(i64, i64) nounwind
declare i8* @dlsym(i8*, i8*) nounwind
declare i32 @puts(i8*) nounwind

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCHEME STUFF

define private i8* @impc_null() nounwind alwaysinline
{
  ret i8* null
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

define private i64 @ptrtoi64(i8* %a) alwaysinline
{
entry:
%return = ptrtoint i8* %a to i64
ret i64 %return
}

define private i8* @i64toptr(i64 %a) alwaysinline
{
entry:
%return = inttoptr i64 %a to i8*
ret i8* %return
}

define private i32 @ptrtoi32(i8* %a) alwaysinline
{
entry:
%return = ptrtoint i8* %a to i32
ret i32 %return
}

define private i16 @ptrtoi16(i8* %a) alwaysinline
{
entry:
%return = ptrtoint i8* %a to i16
ret i16 %return
}

define private i8* @i32toptr(i32 %a) alwaysinline
{
  %return = inttoptr i32 %a to i8*
  ret i8* %return
}

define private i8* @i16toptr(i16 %a) alwaysinline
{
  %return = inttoptr i16 %a to i8*
  ret i8* %return
}

; Portable 80-bit extended precision to double conversion
; Works on both x86_64 and ARM64 by manually parsing IEEE 754 extended format
; The 80-bit format is: 1 sign bit, 15 exponent bits, 64 mantissa bits (explicit integer bit)
; Input is big-endian (as used in AIFF files)
declare double @fp80_to_double_portable(i8*) nounwind

define private double @fp80ptrtod(i8* %fp80ptr)
{
entry:
  %result = call double @fp80_to_double_portable(i8* %fp80ptr)
  ret double %result
}

declare i32 @printf(i8* noalias nocapture, ...)
declare i32 @sprintf(i8*, i8* noalias nocapture, ...)
declare i32 @sscanf(i8*, i8* noalias nocapture, ...)
declare i32 @fprintf(i8*, i8* noalias nocapture, ...)
declare i32 @fscanf(i8*, i8* noalias nocapture, ...)

;; scheme helpers

define private i32 @is_type(i8* %ptr, i32 %val) alwaysinline
{
  %1 = bitcast i8* %ptr to i32*
  %2 = load i32, i32* %1
  %3 = and i32 %2, 15
  %set = icmp eq i32 %3, %val
  %res = zext i1 %set to i32
  ret i32 %res
}

define private i32 @is_string(i8* %ptr) alwaysinline
{
  %res = call i32 @is_type(i8* %ptr, i32 1)
  ret i32 %res
}

define private i32 @is_real(i8* %ptr) alwaysinline
{
  %res = call i32 @is_type(i8* %ptr, i32 2)
  ret i32 %res
}

define private i32 @is_cptr(i8* %ptr) alwaysinline
{
  %res = call i32 @is_type(i8* %ptr, i32 15)
  ret i32 %res
}

define private i32 @is_cptr_or_str(i8* %ptr) alwaysinline
{
  %v1 = call i32 @is_cptr(i8* %ptr)
  %v2 = call i32 @is_string(i8* %ptr)
  %res = or i32 %v1, %v2
  ret i32 %res
}

define private void @llvm_zone_ptr_set_size(i8* %zone, i64 %size) nounwind alwaysinline
{
  %ptr = bitcast i8* %zone to i64*
  %size_ptr = getelementptr i64, i64* %ptr, i32 -1
  store i64 %size, i64* %size_ptr
  ret void
}

@TIME = external global i64 ; extemp::UNIV::TIME
define private i64 @llvm_now() nounwind alwaysinline
{
  %res = load i64, i64* @TIME
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

define private %clsvar* @new_address_table() nounwind alwaysinline
{
  ret %clsvar* null
}

declare %mzone* @llvm_peek_zone_stack_extern() nounwind
define private %mzone* @llvm_peek_zone_stack() nounwind alwaysinline "thunk"
{
  %zone = call %mzone* @llvm_peek_zone_stack_extern()
  ret %mzone* %zone
}

declare void @llvm_push_zone_stack_extern(%mzone*) nounwind
define private void @llvm_push_zone_stack(%mzone* %zone) nounwind alwaysinline "thunk"
{
  call void @llvm_push_zone_stack_extern(%mzone* %zone)
  ret void
}

declare %mzone* @llvm_zone_create_extern(i64) nounwind
define private %mzone* @llvm_zone_create(i64 %size) nounwind alwaysinline "thunk"
{
  %zone = call %mzone* @llvm_zone_create_extern(i64 %size)
  ret %mzone* %zone
}

define private void @llvm_zone_mark(%mzone* %zone) nounwind alwaysinline
{
  %offset_ptr = getelementptr inbounds %mzone, %mzone* %zone, i32 0, i32 1
  %offset_val = load i64, i64* %offset_ptr
  %mark_ptr = getelementptr %mzone, %mzone* %zone, i32 0, i32 2
  store i64 %offset_val, i64* %mark_ptr
  ret void
}

define private i64 @llvm_zone_mark_size(%mzone* %zone) nounwind alwaysinline
{
  %offset_ptr = getelementptr inbounds %mzone, %mzone* %zone, i32 0, i32 1
  %offset_val = load i64, i64* %offset_ptr
  %mark_ptr = getelementptr %mzone, %mzone* %zone, i32 0, i32 2
  %mark_val = load i64, i64* %mark_ptr
  %res = sub i64 %offset_val, %mark_val
  ret i64 %res
}

define private %mzone* @llvm_zone_reset(%mzone* %zone) nounwind alwaysinline
{
  %offset_ptr = getelementptr inbounds %mzone, %mzone* %zone, i32 0, i32 1
  store i64 0, i64* %offset_ptr
  ret %mzone* %zone
}

declare i32 @is_integer_extern(i8*)
define private i32 @is_integer(i8* %ptr) nounwind alwaysinline
{
  %res = call i32 @is_integer_extern(i8* %ptr)
  ret i32 %res
}
