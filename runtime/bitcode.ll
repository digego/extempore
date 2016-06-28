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

define private double @fp80ptrtod(i8* %fp80ptr)
{
  %1 = alloca i8*, align 8
  store i8* %fp80ptr, i8** %1, align 8
  %2 = load i8*, i8** %1, align 8
  %3 = bitcast i8* %2 to x86_fp80*
  %4 = load x86_fp80, x86_fp80* %3, align 16
  %5 = fptrunc x86_fp80 %4 to double
  ret double %5
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