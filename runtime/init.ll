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
%wt = type double (ptr, ptr, double, i64, i64, ptr)
%wts = type double (ptr, ptr, ptr, i64, i64, ptr)
%wt_f = type float (ptr, ptr, float, i64, i64, ptr)
%wts_f = type float (ptr, ptr, ptr, i64, i64, ptr)

;; Double-precision sample DSP wrapper
define dllexport double @imp_dsp_wrapper(ptr %_zonei8, ptr %closure, double %sample, i64 %time, i64 %channel, ptr %data)
{
entry:
  %fPtr = getelementptr { ptr, ptr, ptr }, ptr %closure, i32 0, i32 2
  %ePtr = getelementptr { ptr, ptr, ptr }, ptr %closure, i32 0, i32 1
  %f = load ptr, ptr %fPtr
  %e = load ptr, ptr %ePtr
  %result = tail call fastcc double %f(ptr %_zonei8, ptr %e, double %sample, i64 %time, i64 %channel, ptr %data)
  ret double %result
}

;; Double-precision multi-channel sum DSP wrapper
define dllexport double @imp_dsp_sum_wrapper(ptr %_zonei8, ptr %closure, ptr %sample, i64 %time, i64 %channel, ptr %data)
{
entry:
  %fPtr = getelementptr { ptr, ptr, ptr }, ptr %closure, i32 0, i32 2
  %ePtr = getelementptr { ptr, ptr, ptr }, ptr %closure, i32 0, i32 1
  %f = load ptr, ptr %fPtr
  %e = load ptr, ptr %ePtr
  %result = tail call fastcc double %f(ptr %_zonei8, ptr %e, ptr %sample, i64 %time, i64 %channel, ptr %data)
  ret double %result
}

;; Single-precision sample DSP wrapper
define dllexport float @imp_dspf_wrapper(ptr %_zonei8, ptr %closure, float %sample, i64 %time, i64 %channel, ptr %data)
{
entry:
  %fPtr = getelementptr { ptr, ptr, ptr }, ptr %closure, i32 0, i32 2
  %ePtr = getelementptr { ptr, ptr, ptr }, ptr %closure, i32 0, i32 1
  %f = load ptr, ptr %fPtr
  %e = load ptr, ptr %ePtr
  %result = tail call fastcc float %f(ptr %_zonei8, ptr %e, float %sample, i64 %time, i64 %channel, ptr %data)
  ret float %result
}

;; Single-precision multi-channel sum DSP wrapper
define dllexport float @imp_dspf_sum_wrapper(ptr %_zonei8, ptr %closure, ptr %sample, i64 %time, i64 %channel, ptr %data)
{
entry:
  %fPtr = getelementptr { ptr, ptr, ptr }, ptr %closure, i32 0, i32 2
  %ePtr = getelementptr { ptr, ptr, ptr }, ptr %closure, i32 0, i32 1
  %f = load ptr, ptr %fPtr
  %e = load ptr, ptr %ePtr
  %result = tail call fastcc float %f(ptr %_zonei8, ptr %e, ptr %sample, i64 %time, i64 %channel, ptr %data)
  ret float %result
}

;; Get the environment pointer from a closure
define dllexport ptr @impc_get_env(ptr %impz, ptr %closure)
{
entry:
  %ePtr = getelementptr { ptr, ptr, ptr }, ptr %closure, i32 0, i32 1
  %e = load ptr, ptr %ePtr
  ret ptr %e
}
