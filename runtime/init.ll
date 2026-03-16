;;
;; Copyright (c) 2011, Andrew Sorensen
;;
;; All rights reserved
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; Neither the name of the authors nor other contributors may be used to endorse
;; or promote products derived from this software without specific prior written
;; permission.
;;
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;;
;; DSP wrapper functions for audio callbacks.
;; These are compiled once at startup and looked up by name at runtime.
;; Note: Type definitions and declarations from bitcode.ll are already
;; available when this file is compiled, so we only define what's unique here.

;; DSP wrapper function pointer types
%wt = type double (i8*, i8*, double, i64, i64, double*)
%wts = type double (i8*, i8*, double*, i64, i64, double*)
%wt_f = type float (i8*, i8*, float, i64, i64, float*)
%wts_f = type float (i8*, i8*, float*, i64, i64, float*)
%wta = type void (i8*, i8*, float*, float*, i64, i8*)
%wta_s = type void (i8*, i8*, float**, float*, i64, i8*)

;; Double-precision sample DSP wrapper
define dllexport double @imp_dsp_wrapper(i8* %_impz, i8* %closure, double %sample, i64 %time, i64 %channel, double* %data)
{
entry:
  %closureVal = bitcast i8* %closure to { i8*, i8*, %wt* }*
  %fPtr = getelementptr { i8*, i8*, %wt* }, { i8*, i8*, %wt* }* %closureVal, i32 0, i32 2
  %ePtr = getelementptr { i8*, i8*, %wt* }, { i8*, i8*, %wt* }* %closureVal, i32 0, i32 1
  %f = load %wt*, %wt** %fPtr
  %e = load i8*, i8** %ePtr
  %result = tail call fastcc double %f(i8* %_impz, i8* %e, double %sample, i64 %time, i64 %channel, double* %data)
  ret double %result
}

;; Double-precision multi-channel sum DSP wrapper
define dllexport double @imp_dsp_sum_wrapper(i8* %_impz, i8* %closure, double* %sample, i64 %time, i64 %channel, double* %data)
{
entry:
  %closureVal = bitcast i8* %closure to { i8*, i8*, %wts* }*
  %fPtr = getelementptr { i8*, i8*, %wts* }, { i8*, i8*, %wts* }* %closureVal, i32 0, i32 2
  %ePtr = getelementptr { i8*, i8*, %wts* }, { i8*, i8*, %wts* }* %closureVal, i32 0, i32 1
  %f = load %wts*, %wts** %fPtr
  %e = load i8*, i8** %ePtr
  %result = tail call fastcc double %f(i8* %_impz, i8* %e, double* %sample, i64 %time, i64 %channel, double* %data)
  ret double %result
}

;; Single-precision sample DSP wrapper
define dllexport float @imp_dspf_wrapper(i8* %_impz, i8* %closure, float %sample, i64 %time, i64 %channel, float* %data)
{
entry:
  %closureVal = bitcast i8* %closure to { i8*, i8*, %wt_f* }*
  %fPtr = getelementptr { i8*, i8*, %wt_f* }, { i8*, i8*, %wt_f* }* %closureVal, i32 0, i32 2
  %ePtr = getelementptr { i8*, i8*, %wt_f* }, { i8*, i8*, %wt_f* }* %closureVal, i32 0, i32 1
  %f = load %wt_f*, %wt_f** %fPtr
  %e = load i8*, i8** %ePtr
  %result = tail call fastcc float %f(i8* %_impz, i8* %e, float %sample, i64 %time, i64 %channel, float* %data)
  ret float %result
}

;; Single-precision multi-channel sum DSP wrapper
define dllexport float @imp_dspf_sum_wrapper(i8* %_impz, i8* %closure, float* %sample, i64 %time, i64 %channel, float* %data)
{
entry:
  %closureVal = bitcast i8* %closure to { i8*, i8*, %wts_f* }*
  %fPtr = getelementptr { i8*, i8*, %wts_f* }, { i8*, i8*, %wts_f* }* %closureVal, i32 0, i32 2
  %ePtr = getelementptr { i8*, i8*, %wts_f* }, { i8*, i8*, %wts_f* }* %closureVal, i32 0, i32 1
  %f = load %wts_f*, %wts_f** %fPtr
  %e = load i8*, i8** %ePtr
  %result = tail call fastcc float %f(i8* %_impz, i8* %e, float* %sample, i64 %time, i64 %channel, float* %data)
  ret float %result
}

;; Array-based DSP wrapper (for block processing)
define dllexport void @imp_dsp_wrapper_array(i8* %_impz, i8* %closure, float* %datain, float* %dataout, i64 %time, i8* %data)
{
entry:
  %closureVal = bitcast i8* %closure to { i8*, i8*, %wta* }*
  %fPtr = getelementptr { i8*, i8*, %wta* }, { i8*, i8*, %wta* }* %closureVal, i32 0, i32 2
  %ePtr = getelementptr { i8*, i8*, %wta* }, { i8*, i8*, %wta* }* %closureVal, i32 0, i32 1
  %f = load %wta*, %wta** %fPtr
  %e = load i8*, i8** %ePtr
  tail call fastcc void %f(i8* %_impz, i8* %e, float* %datain, float* %dataout, i64 %time, i8* %data)
  ret void
}

;; Array-based multi-channel sum DSP wrapper
define dllexport void @imp_dsp_sum_wrapper_array(i8* %_impz, i8* %closure, float** %datain, float* %dataout, i64 %time, i8* %data)
{
entry:
  %closureVal = bitcast i8* %closure to { i8*, i8*, %wta_s* }*
  %fPtr = getelementptr { i8*, i8*, %wta_s* }, { i8*, i8*, %wta_s* }* %closureVal, i32 0, i32 2
  %ePtr = getelementptr { i8*, i8*, %wta_s* }, { i8*, i8*, %wta_s* }* %closureVal, i32 0, i32 1
  %f = load %wta_s*, %wta_s** %fPtr
  %e = load i8*, i8** %ePtr
  tail call fastcc void %f(i8* %_impz, i8* %e, float** %datain, float* %dataout, i64 %time, i8* %data)
  ret void
}

;; Get the environment pointer from a closure
define dllexport i8* @impc_get_env(i8* %impz, i8* %closure)
{
entry:
  %closureVal = bitcast i8* %closure to { i8*, i8*, i8* }*
  %ePtr = getelementptr { i8*, i8*, i8* }, { i8*, i8*, i8* }* %closureVal, i32 0, i32 1
  %e = load i8*, i8** %ePtr
  ret i8* %e
}
