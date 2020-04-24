; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=powerpc64le-unknown-unknown -mcpu=pwr8 \
; RUN:   -verify-machineinstrs | FileCheck %s
; RUN: llc < %s -mtriple=powerpc64le-unknown-unknown -mcpu=pwr7 \
; RUN:   -verify-machineinstrs | FileCheck %s --check-prefix=NOP8VEC
define <16 x i8> @getsmaxi8(<16 x i8> %a, <16 x i8> %b) {
; CHECK-LABEL: getsmaxi8:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vmaxsb 2, 2, 3
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsmaxi8:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    vmaxsb 2, 2, 3
; NOP8VEC-NEXT:    blr
entry:
  %0 = icmp sgt <16 x i8> %a, %b
  %1 = select <16 x i1> %0, <16 x i8> %a, <16 x i8> %b
  ret <16 x i8> %1
}

define <8 x i16> @getsmaxi16(<8 x i16> %a, <8 x i16> %b) {
; CHECK-LABEL: getsmaxi16:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vmaxsh 2, 2, 3
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsmaxi16:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    vmaxsh 2, 2, 3
; NOP8VEC-NEXT:    blr
entry:
  %0 = icmp sgt <8 x i16> %a, %b
  %1 = select <8 x i1> %0, <8 x i16> %a, <8 x i16> %b
  ret <8 x i16> %1
}

define <4 x i32> @getsmaxi32(<4 x i32> %a, <4 x i32> %b) {
; CHECK-LABEL: getsmaxi32:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vmaxsw 2, 2, 3
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsmaxi32:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    vmaxsw 2, 2, 3
; NOP8VEC-NEXT:    blr
entry:
  %0 = icmp sgt <4 x i32> %a, %b
  %1 = select <4 x i1> %0, <4 x i32> %a, <4 x i32> %b
  ret <4 x i32> %1
}

define <2 x i64> @getsmaxi64(<2 x i64> %a, <2 x i64> %b) {
; CHECK-LABEL: getsmaxi64:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vmaxsd 2, 2, 3
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsmaxi64:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    xxswapd 0, 35
; NOP8VEC-NEXT:    addi 3, 1, -32
; NOP8VEC-NEXT:    addi 4, 1, -48
; NOP8VEC-NEXT:    xxswapd 1, 34
; NOP8VEC-NEXT:    stxvd2x 0, 0, 3
; NOP8VEC-NEXT:    stxvd2x 1, 0, 4
; NOP8VEC-NEXT:    ld 3, -24(1)
; NOP8VEC-NEXT:    ld 4, -40(1)
; NOP8VEC-NEXT:    ld 6, -48(1)
; NOP8VEC-NEXT:    cmpd 4, 3
; NOP8VEC-NEXT:    li 3, 0
; NOP8VEC-NEXT:    li 4, -1
; NOP8VEC-NEXT:    isel 5, 4, 3, 1
; NOP8VEC-NEXT:    std 5, -8(1)
; NOP8VEC-NEXT:    ld 5, -32(1)
; NOP8VEC-NEXT:    cmpd 6, 5
; NOP8VEC-NEXT:    isel 3, 4, 3, 1
; NOP8VEC-NEXT:    std 3, -16(1)
; NOP8VEC-NEXT:    addi 3, 1, -16
; NOP8VEC-NEXT:    lxvd2x 0, 0, 3
; NOP8VEC-NEXT:    xxswapd 36, 0
; NOP8VEC-NEXT:    xxsel 34, 35, 34, 36
; NOP8VEC-NEXT:    blr
entry:
  %0 = icmp sgt <2 x i64> %a, %b
  %1 = select <2 x i1> %0, <2 x i64> %a, <2 x i64> %b
  ret <2 x i64> %1
}

define <4 x float> @getsmaxf32(<4 x float> %a, <4 x float> %b) {
; CHECK-LABEL: getsmaxf32:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    xvmaxsp 34, 34, 35
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsmaxf32:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    xvmaxsp 34, 34, 35
; NOP8VEC-NEXT:    blr
entry:
  %0 = fcmp fast oge <4 x float> %a, %b
  %1 = select <4 x i1> %0, <4 x float> %a, <4 x float> %b
  ret <4 x float> %1
}

define <2 x double> @getsmaxf64(<2 x double> %a, <2 x double> %b) {
; CHECK-LABEL: getsmaxf64:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    xvmaxdp 34, 34, 35
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsmaxf64:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    xvmaxdp 34, 34, 35
; NOP8VEC-NEXT:    blr
entry:
  %0 = fcmp fast oge <2 x double> %a, %b
  %1 = select <2 x i1> %0, <2 x double> %a, <2 x double> %b
  ret <2 x double> %1
}

define <16 x i8> @getsmini8(<16 x i8> %a, <16 x i8> %b) {
; CHECK-LABEL: getsmini8:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vminsb 2, 2, 3
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsmini8:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    vminsb 2, 2, 3
; NOP8VEC-NEXT:    blr
entry:
  %0 = icmp slt <16 x i8> %a, %b
  %1 = select <16 x i1> %0, <16 x i8> %a, <16 x i8> %b
  ret <16 x i8> %1
}

define <8 x i16> @getsmini16(<8 x i16> %a, <8 x i16> %b) {
; CHECK-LABEL: getsmini16:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vminsh 2, 2, 3
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsmini16:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    vminsh 2, 2, 3
; NOP8VEC-NEXT:    blr
entry:
  %0 = icmp slt <8 x i16> %a, %b
  %1 = select <8 x i1> %0, <8 x i16> %a, <8 x i16> %b
  ret <8 x i16> %1
}

define <4 x i32> @getsmini32(<4 x i32> %a, <4 x i32> %b) {
; CHECK-LABEL: getsmini32:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vminsw 2, 2, 3
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsmini32:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    vminsw 2, 2, 3
; NOP8VEC-NEXT:    blr
entry:
  %0 = icmp slt <4 x i32> %a, %b
  %1 = select <4 x i1> %0, <4 x i32> %a, <4 x i32> %b
  ret <4 x i32> %1
}

define <2 x i64> @getsmini64(<2 x i64> %a, <2 x i64> %b) {
; CHECK-LABEL: getsmini64:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vminsd 2, 2, 3
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsmini64:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    xxswapd 0, 35
; NOP8VEC-NEXT:    addi 3, 1, -32
; NOP8VEC-NEXT:    addi 4, 1, -48
; NOP8VEC-NEXT:    xxswapd 1, 34
; NOP8VEC-NEXT:    stxvd2x 0, 0, 3
; NOP8VEC-NEXT:    stxvd2x 1, 0, 4
; NOP8VEC-NEXT:    ld 3, -24(1)
; NOP8VEC-NEXT:    ld 4, -40(1)
; NOP8VEC-NEXT:    ld 6, -48(1)
; NOP8VEC-NEXT:    cmpd 4, 3
; NOP8VEC-NEXT:    li 3, 0
; NOP8VEC-NEXT:    li 4, -1
; NOP8VEC-NEXT:    isel 5, 4, 3, 0
; NOP8VEC-NEXT:    std 5, -8(1)
; NOP8VEC-NEXT:    ld 5, -32(1)
; NOP8VEC-NEXT:    cmpd 6, 5
; NOP8VEC-NEXT:    isel 3, 4, 3, 0
; NOP8VEC-NEXT:    std 3, -16(1)
; NOP8VEC-NEXT:    addi 3, 1, -16
; NOP8VEC-NEXT:    lxvd2x 0, 0, 3
; NOP8VEC-NEXT:    xxswapd 36, 0
; NOP8VEC-NEXT:    xxsel 34, 35, 34, 36
; NOP8VEC-NEXT:    blr
entry:
  %0 = icmp slt <2 x i64> %a, %b
  %1 = select <2 x i1> %0, <2 x i64> %a, <2 x i64> %b
  ret <2 x i64> %1
}

define <4 x float> @getsminf32(<4 x float> %a, <4 x float> %b) {
; CHECK-LABEL: getsminf32:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    xvminsp 34, 34, 35
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsminf32:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    xvminsp 34, 34, 35
; NOP8VEC-NEXT:    blr
entry:
  %0 = fcmp fast ole <4 x float> %a, %b
  %1 = select <4 x i1> %0, <4 x float> %a, <4 x float> %b
  ret <4 x float> %1
}

define <2 x double> @getsminf64(<2 x double> %a, <2 x double> %b) {
; CHECK-LABEL: getsminf64:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    xvmindp 34, 34, 35
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: getsminf64:
; NOP8VEC:       # %bb.0: # %entry
; NOP8VEC-NEXT:    xvmindp 34, 34, 35
; NOP8VEC-NEXT:    blr
entry:
  %0 = fcmp fast ole <2 x double> %a, %b
  %1 = select <2 x i1> %0, <2 x double> %a, <2 x double> %b
  ret <2 x double> %1
}

define i128 @invalidv1i128(<2 x i128> %v1, <2 x i128> %v2) {
; CHECK-LABEL: invalidv1i128:
; CHECK:       # %bb.0:
; CHECK-NEXT:    mfvsrd 3, 36
; CHECK-NEXT:    xxswapd 0, 36
; CHECK-NEXT:    mfvsrd 4, 34
; CHECK-NEXT:    xxswapd 1, 34
; CHECK-NEXT:    cmpld 4, 3
; CHECK-NEXT:    cmpd 1, 4, 3
; CHECK-NEXT:    mffprd 3, 0
; CHECK-NEXT:    crandc 20, 4, 2
; CHECK-NEXT:    mffprd 4, 1
; CHECK-NEXT:    cmpld 1, 4, 3
; CHECK-NEXT:    bc 12, 20, .LBB12_3
; CHECK-NEXT:  # %bb.1:
; CHECK-NEXT:    crand 20, 2, 4
; CHECK-NEXT:    bc 12, 20, .LBB12_3
; CHECK-NEXT:  # %bb.2:
; CHECK-NEXT:    vmr 2, 4
; CHECK-NEXT:  .LBB12_3:
; CHECK-NEXT:    xxswapd 0, 34
; CHECK-NEXT:    mfvsrd 4, 34
; CHECK-NEXT:    mffprd 3, 0
; CHECK-NEXT:    blr
;
; NOP8VEC-LABEL: invalidv1i128:
; NOP8VEC:       # %bb.0:
; NOP8VEC-NEXT:    cmpld 4, 8
; NOP8VEC-NEXT:    cmpd 1, 4, 8
; NOP8VEC-NEXT:    addi 5, 1, -32
; NOP8VEC-NEXT:    crandc 20, 4, 2
; NOP8VEC-NEXT:    cmpld 1, 3, 7
; NOP8VEC-NEXT:    crand 21, 2, 4
; NOP8VEC-NEXT:    cror 20, 21, 20
; NOP8VEC-NEXT:    isel 3, 3, 7, 20
; NOP8VEC-NEXT:    isel 4, 4, 8, 20
; NOP8VEC-NEXT:    std 3, -32(1)
; NOP8VEC-NEXT:    addi 3, 1, -16
; NOP8VEC-NEXT:    std 4, -24(1)
; NOP8VEC-NEXT:    lxvd2x 0, 0, 5
; NOP8VEC-NEXT:    stxvd2x 0, 0, 3
; NOP8VEC-NEXT:    ld 3, -16(1)
; NOP8VEC-NEXT:    ld 4, -8(1)
; NOP8VEC-NEXT:    blr
%1 = icmp slt <2 x i128> %v1, %v2
%2 = select <2 x i1> %1, <2 x i128> %v1, <2 x i128> %v2
%3 = extractelement <2 x i128> %2, i32 0
ret i128 %3
}
