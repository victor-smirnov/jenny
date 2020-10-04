; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt < %s -instsimplify -S | FileCheck %s

; Overflow on a float to int or int to float conversion is undefined (PR21130).

define i8 @overflow_fptosi() {
; CHECK-LABEL: @overflow_fptosi(
; CHECK-NEXT:    ret i8 undef
;
  %i = fptosi double 1.56e+02 to i8
  ret i8 %i
}

define i8 @overflow_fptoui() {
; CHECK-LABEL: @overflow_fptoui(
; CHECK-NEXT:    ret i8 undef
;
  %i = fptoui double 2.56e+02 to i8
  ret i8 %i
}

; The maximum float is approximately 2 ** 128 which is 3.4E38.
; The constant below is 4E38. Use a 130 bit integer to hold that
; number; 129-bits for the value + 1 bit for the sign.

define float @overflow_uitofp() {
; CHECK-LABEL: @overflow_uitofp(
; CHECK-NEXT:    ret float 0x7FF0000000000000
;
  %i = uitofp i130 400000000000000000000000000000000000000 to float
  ret float %i
}

define float @overflow_sitofp() {
; CHECK-LABEL: @overflow_sitofp(
; CHECK-NEXT:    ret float 0x7FF0000000000000
;
  %i = sitofp i130 400000000000000000000000000000000000000 to float
  ret float %i
}

; https://llvm.org/PR43907 - make sure that NaN doesn't morph into Inf.
; SNaN remains SNaN.

define float @nan_f64_trunc() {
; CHECK-LABEL: @nan_f64_trunc(
; CHECK-NEXT:    ret float 0x7FF4000000000000
;
  %f = fptrunc double 0x7FF0000000000001 to float
  ret float %f
}

; Verify again with a vector and different destination type.
; SNaN remains SNaN (first two elements).
; QNaN remains QNaN (third element).
; Lower 42 bits of NaN source payload are lost.

define <3 x half> @nan_v3f64_trunc() {
; CHECK-LABEL: @nan_v3f64_trunc(
; CHECK-NEXT:    ret <3 x half> <half 0xH7D00, half 0xH7D00, half 0xH7E00>
;
  %f = fptrunc <3 x double> <double 0x7FF0020000000000, double 0x7FF003FFFFFFFFFF, double 0x7FF8000000000001> to <3 x half>
  ret <3 x half> %f
}
