; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=powerpc64le-unknown-linux-gnu -enable-no-signed-zeros-fp-math \
; RUN:     -enable-unsafe-fp-math  < %s | FileCheck -check-prefix=CHECK-FAST %s
; RUN: llc -mtriple=powerpc64le-unknown-linux-gnu -enable-no-signed-zeros-fp-math \
; RUN:     -enable-unsafe-fp-math -mattr=-vsx < %s | FileCheck -check-prefix=CHECK-FAST-NOVSX %s
; RUN: llc -mtriple=powerpc64le-unknown-linux-gnu < %s | FileCheck %s

define double @fma_combine1(double %a, double %b, double %c) {
; CHECK-FAST-LABEL: fma_combine1:
; CHECK-FAST:       # %bb.0: # %entry
; CHECK-FAST-NEXT:    xsnmaddadp 1, 3, 2
; CHECK-FAST-NEXT:    blr
;
; CHECK-FAST-NOVSX-LABEL: fma_combine1:
; CHECK-FAST-NOVSX:       # %bb.0: # %entry
; CHECK-FAST-NOVSX-NEXT:    fnmadd 1, 3, 2, 1
; CHECK-FAST-NOVSX-NEXT:    blr
;
; CHECK-LABEL: fma_combine1:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    xsnegdp 0, 3
; CHECK-NEXT:    xsmuldp 0, 0, 2
; CHECK-NEXT:    xssubdp 1, 0, 1
; CHECK-NEXT:    blr
entry:
  %fneg1 = fneg double %c
  %mul = fmul double %fneg1, %b
  %add = fsub double %mul, %a
  ret double %add
}

define double @fma_combine2(double %a, double %b, double %c) {
; CHECK-FAST-LABEL: fma_combine2:
; CHECK-FAST:       # %bb.0: # %entry
; CHECK-FAST-NEXT:    xsnmaddadp 1, 2, 3
; CHECK-FAST-NEXT:    blr
;
; CHECK-FAST-NOVSX-LABEL: fma_combine2:
; CHECK-FAST-NOVSX:       # %bb.0: # %entry
; CHECK-FAST-NOVSX-NEXT:    fnmadd 1, 2, 3, 1
; CHECK-FAST-NOVSX-NEXT:    blr
;
; CHECK-LABEL: fma_combine2:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    xsnegdp 0, 3
; CHECK-NEXT:    xsmuldp 0, 2, 0
; CHECK-NEXT:    xssubdp 1, 0, 1
; CHECK-NEXT:    blr
entry:
  %fneg1 = fneg double %c
  %mul = fmul double %b, %fneg1
  %add = fsub double %mul, %a
  ret double %add
}

@v = common local_unnamed_addr global double 0.000000e+00, align 8
@z = common local_unnamed_addr global double 0.000000e+00, align 8
define double @fma_combine_two_uses(double %a, double %b, double %c) {
; CHECK-FAST-LABEL: fma_combine_two_uses:
; CHECK-FAST:       # %bb.0: # %entry
; CHECK-FAST-NEXT:    xsnegdp 0, 1
; CHECK-FAST-NEXT:    addis 3, 2, v@toc@ha
; CHECK-FAST-NEXT:    addis 4, 2, z@toc@ha
; CHECK-FAST-NEXT:    xsnmaddadp 1, 3, 2
; CHECK-FAST-NEXT:    xsnegdp 2, 3
; CHECK-FAST-NEXT:    stfd 0, v@toc@l(3)
; CHECK-FAST-NEXT:    stfd 2, z@toc@l(4)
; CHECK-FAST-NEXT:    blr
;
; CHECK-FAST-NOVSX-LABEL: fma_combine_two_uses:
; CHECK-FAST-NOVSX:       # %bb.0: # %entry
; CHECK-FAST-NOVSX-NEXT:    fnmadd 0, 3, 2, 1
; CHECK-FAST-NOVSX-NEXT:    fneg 2, 1
; CHECK-FAST-NOVSX-NEXT:    addis 3, 2, v@toc@ha
; CHECK-FAST-NOVSX-NEXT:    addis 4, 2, z@toc@ha
; CHECK-FAST-NOVSX-NEXT:    fneg 3, 3
; CHECK-FAST-NOVSX-NEXT:    fmr 1, 0
; CHECK-FAST-NOVSX-NEXT:    stfd 2, v@toc@l(3)
; CHECK-FAST-NOVSX-NEXT:    stfd 3, z@toc@l(4)
; CHECK-FAST-NOVSX-NEXT:    blr
;
; CHECK-LABEL: fma_combine_two_uses:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    xsnegdp 3, 3
; CHECK-NEXT:    addis 3, 2, v@toc@ha
; CHECK-NEXT:    addis 4, 2, z@toc@ha
; CHECK-NEXT:    xsmuldp 0, 3, 2
; CHECK-NEXT:    stfd 3, z@toc@l(4)
; CHECK-NEXT:    xsnegdp 2, 1
; CHECK-NEXT:    xssubdp 0, 0, 1
; CHECK-NEXT:    stfd 2, v@toc@l(3)
; CHECK-NEXT:    fmr 1, 0
; CHECK-NEXT:    blr
entry:
  %fneg = fneg double %a
  store double %fneg, double* @v, align 8
  %fneg1 = fneg double %c
  store double %fneg1, double* @z, align 8
  %mul = fmul double %fneg1, %b
  %add = fsub double %mul, %a
  ret double %add
}

define double @fma_combine_one_use(double %a, double %b, double %c) {
; CHECK-FAST-LABEL: fma_combine_one_use:
; CHECK-FAST:       # %bb.0: # %entry
; CHECK-FAST-NEXT:    xsnegdp 0, 1
; CHECK-FAST-NEXT:    addis 3, 2, v@toc@ha
; CHECK-FAST-NEXT:    xsnmaddadp 1, 3, 2
; CHECK-FAST-NEXT:    stfd 0, v@toc@l(3)
; CHECK-FAST-NEXT:    blr
;
; CHECK-FAST-NOVSX-LABEL: fma_combine_one_use:
; CHECK-FAST-NOVSX:       # %bb.0: # %entry
; CHECK-FAST-NOVSX-NEXT:    fnmadd 0, 3, 2, 1
; CHECK-FAST-NOVSX-NEXT:    fneg 2, 1
; CHECK-FAST-NOVSX-NEXT:    addis 3, 2, v@toc@ha
; CHECK-FAST-NOVSX-NEXT:    fmr 1, 0
; CHECK-FAST-NOVSX-NEXT:    stfd 2, v@toc@l(3)
; CHECK-FAST-NOVSX-NEXT:    blr
;
; CHECK-LABEL: fma_combine_one_use:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    xsnegdp 0, 3
; CHECK-NEXT:    addis 3, 2, v@toc@ha
; CHECK-NEXT:    xsmuldp 0, 0, 2
; CHECK-NEXT:    xsnegdp 2, 1
; CHECK-NEXT:    xssubdp 0, 0, 1
; CHECK-NEXT:    stfd 2, v@toc@l(3)
; CHECK-NEXT:    fmr 1, 0
; CHECK-NEXT:    blr
entry:
  %fneg = fneg double %a
  store double %fneg, double* @v, align 8
  %fneg1 = fneg double %c
  %mul = fmul double %fneg1, %b
  %add = fsub double %mul, %a
  ret double %add
}

define float @fma_combine_no_ice() {
; CHECK-FAST-LABEL: fma_combine_no_ice:
; CHECK-FAST:       # %bb.0:
; CHECK-FAST-NEXT:    addis 3, 2, .LCPI4_0@toc@ha
; CHECK-FAST-NEXT:    addis 4, 2, .LCPI4_1@toc@ha
; CHECK-FAST-NEXT:    lfs 0, .LCPI4_0@toc@l(3)
; CHECK-FAST-NEXT:    lfsx 2, 0, 3
; CHECK-FAST-NEXT:    addis 3, 2, .LCPI4_2@toc@ha
; CHECK-FAST-NEXT:    lfs 3, .LCPI4_1@toc@l(4)
; CHECK-FAST-NEXT:    lfs 1, .LCPI4_2@toc@l(3)
; CHECK-FAST-NEXT:    xsmaddasp 3, 2, 0
; CHECK-FAST-NEXT:    xsmaddasp 1, 2, 3
; CHECK-FAST-NEXT:    xsnmsubasp 1, 3, 2
; CHECK-FAST-NEXT:    blr
;
; CHECK-FAST-NOVSX-LABEL: fma_combine_no_ice:
; CHECK-FAST-NOVSX:       # %bb.0:
; CHECK-FAST-NOVSX-NEXT:    addis 3, 2, .LCPI4_0@toc@ha
; CHECK-FAST-NOVSX-NEXT:    lfs 0, .LCPI4_0@toc@l(3)
; CHECK-FAST-NOVSX-NEXT:    addis 3, 2, .LCPI4_1@toc@ha
; CHECK-FAST-NOVSX-NEXT:    lfs 1, 0(3)
; CHECK-FAST-NOVSX-NEXT:    lfs 2, .LCPI4_1@toc@l(3)
; CHECK-FAST-NOVSX-NEXT:    addis 3, 2, .LCPI4_2@toc@ha
; CHECK-FAST-NOVSX-NEXT:    fmadds 0, 1, 2, 0
; CHECK-FAST-NOVSX-NEXT:    lfs 2, .LCPI4_2@toc@l(3)
; CHECK-FAST-NOVSX-NEXT:    fmadds 2, 1, 0, 2
; CHECK-FAST-NOVSX-NEXT:    fnmsubs 1, 0, 1, 2
; CHECK-FAST-NOVSX-NEXT:    blr
;
; CHECK-LABEL: fma_combine_no_ice:
; CHECK:       # %bb.0:
; CHECK-NEXT:    addis 3, 2, .LCPI4_0@toc@ha
; CHECK-NEXT:    addis 4, 2, .LCPI4_1@toc@ha
; CHECK-NEXT:    lfs 0, .LCPI4_0@toc@l(3)
; CHECK-NEXT:    lfsx 2, 0, 3
; CHECK-NEXT:    addis 3, 2, .LCPI4_2@toc@ha
; CHECK-NEXT:    lfs 3, .LCPI4_1@toc@l(4)
; CHECK-NEXT:    lfs 1, .LCPI4_2@toc@l(3)
; CHECK-NEXT:    fmr 4, 3
; CHECK-NEXT:    xsmaddasp 3, 2, 0
; CHECK-NEXT:    xsnmaddasp 4, 2, 0
; CHECK-NEXT:    xsmaddasp 1, 2, 3
; CHECK-NEXT:    xsmaddasp 1, 4, 2
; CHECK-NEXT:    blr
  %tmp = load float, float* undef, align 4
  %tmp2 = load float, float* undef, align 4
  %tmp3 = fmul reassoc float %tmp, 0x3FE372D780000000
  %tmp4 = fadd reassoc float %tmp3, 1.000000e+00
  %tmp5 = fmul reassoc float %tmp2, %tmp4
  %tmp6 = load float, float* undef, align 4
  %tmp7 = load float, float* undef, align 4
  %tmp8 = fmul reassoc float %tmp7, 0x3FE372D780000000
  %tmp9 = fsub reassoc nsz float -1.000000e+00, %tmp8
  %tmp10 = fmul reassoc float %tmp9, %tmp6
  %tmp11 = fadd reassoc float %tmp5, 5.000000e-01
  %tmp12 = fadd reassoc float %tmp11, %tmp10
  ret float %tmp12
}

; This would crash while trying getNegatedExpression().
define double @getNegatedExpression_crash(double %x, double %y) {
; CHECK-FAST-LABEL: getNegatedExpression_crash:
; CHECK-FAST:       # %bb.0:
; CHECK-FAST-NEXT:    addis 3, 2, .LCPI5_1@toc@ha
; CHECK-FAST-NEXT:    addis 4, 2, .LCPI5_0@toc@ha
; CHECK-FAST-NEXT:    lfs 3, .LCPI5_1@toc@l(3)
; CHECK-FAST-NEXT:    lfs 4, .LCPI5_0@toc@l(4)
; CHECK-FAST-NEXT:    xssubdp 0, 1, 3
; CHECK-FAST-NEXT:    xsmaddadp 3, 1, 4
; CHECK-FAST-NEXT:    xsmaddadp 0, 3, 2
; CHECK-FAST-NEXT:    fmr 1, 0
; CHECK-FAST-NEXT:    blr
;
; CHECK-FAST-NOVSX-LABEL: getNegatedExpression_crash:
; CHECK-FAST-NOVSX:       # %bb.0:
; CHECK-FAST-NOVSX-NEXT:    addis 3, 2, .LCPI5_0@toc@ha
; CHECK-FAST-NOVSX-NEXT:    addis 4, 2, .LCPI5_1@toc@ha
; CHECK-FAST-NOVSX-NEXT:    lfs 0, .LCPI5_0@toc@l(3)
; CHECK-FAST-NOVSX-NEXT:    lfs 3, .LCPI5_1@toc@l(4)
; CHECK-FAST-NOVSX-NEXT:    fmadd 3, 1, 3, 0
; CHECK-FAST-NOVSX-NEXT:    fsub 0, 1, 0
; CHECK-FAST-NOVSX-NEXT:    fmadd 1, 3, 2, 0
; CHECK-FAST-NOVSX-NEXT:    blr
;
; CHECK-LABEL: getNegatedExpression_crash:
; CHECK:       # %bb.0:
; CHECK-NEXT:    addis 3, 2, .LCPI5_1@toc@ha
; CHECK-NEXT:    addis 4, 2, .LCPI5_0@toc@ha
; CHECK-NEXT:    lfs 3, .LCPI5_1@toc@l(3)
; CHECK-NEXT:    lfs 4, .LCPI5_0@toc@l(4)
; CHECK-NEXT:    xssubdp 0, 1, 3
; CHECK-NEXT:    xsmaddadp 3, 1, 4
; CHECK-NEXT:    xsmaddadp 0, 3, 2
; CHECK-NEXT:    fmr 1, 0
; CHECK-NEXT:    blr
  %neg = fneg reassoc double %x
  %fma = call reassoc nsz double @llvm.fma.f64(double %neg, double 42.0, double -1.0)
  %add = fadd reassoc nsz double %x, 1.0
  %fma1 = call reassoc nsz double @llvm.fma.f64(double %fma, double %y, double %add)
  ret double %fma1
}

define double @fma_flag_propagation(double %a) {
; CHECK-FAST-LABEL: fma_flag_propagation:
; CHECK-FAST:       # %bb.0: # %entry
; CHECK-FAST-NEXT:    xxlxor 1, 1, 1
; CHECK-FAST-NEXT:    blr
;
; CHECK-FAST-NOVSX-LABEL: fma_flag_propagation:
; CHECK-FAST-NOVSX:       # %bb.0: # %entry
; CHECK-FAST-NOVSX-NEXT:    addis 3, 2, .LCPI6_0@toc@ha
; CHECK-FAST-NOVSX-NEXT:    lfs 1, .LCPI6_0@toc@l(3)
; CHECK-FAST-NOVSX-NEXT:    blr
;
; CHECK-LABEL: fma_flag_propagation:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    xxlxor 1, 1, 1
; CHECK-NEXT:    blr
entry:
  %0 = fneg double %a
  %1 = call reassoc nnan double @llvm.fma.f64(double %0, double 1.0, double %a)
  ret double %1
}

define double @neg_fma_flag_propagation(double %a) {
; CHECK-FAST-LABEL: neg_fma_flag_propagation:
; CHECK-FAST:       # %bb.0: # %entry
; CHECK-FAST-NEXT:    xxlxor 1, 1, 1
; CHECK-FAST-NEXT:    blr
;
; CHECK-FAST-NOVSX-LABEL: neg_fma_flag_propagation:
; CHECK-FAST-NOVSX:       # %bb.0: # %entry
; CHECK-FAST-NOVSX-NEXT:    addis 3, 2, .LCPI7_0@toc@ha
; CHECK-FAST-NOVSX-NEXT:    lfs 1, .LCPI7_0@toc@l(3)
; CHECK-FAST-NOVSX-NEXT:    blr
;
; CHECK-LABEL: neg_fma_flag_propagation:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    xxlxor 1, 1, 1
; CHECK-NEXT:    blr
entry:
  %0 = call reassoc nnan double @llvm.fma.f64(double %a, double -1.0, double %a)
  ret double %0
}

define <2 x double> @vec_neg_fma_flag_propagation(<2 x double> %a) {
; CHECK-FAST-LABEL: vec_neg_fma_flag_propagation:
; CHECK-FAST:       # %bb.0: # %entry
; CHECK-FAST-NEXT:    addis 3, 2, .LCPI8_0@toc@ha
; CHECK-FAST-NEXT:    addi 3, 3, .LCPI8_0@toc@l
; CHECK-FAST-NEXT:    lxvd2x 0, 0, 3
; CHECK-FAST-NEXT:    xxswapd 0, 0
; CHECK-FAST-NEXT:    xvmaddadp 34, 34, 0
; CHECK-FAST-NEXT:    blr
;
; CHECK-FAST-NOVSX-LABEL: vec_neg_fma_flag_propagation:
; CHECK-FAST-NOVSX:       # %bb.0: # %entry
; CHECK-FAST-NOVSX-NEXT:    addis 3, 2, .LCPI8_0@toc@ha
; CHECK-FAST-NOVSX-NEXT:    lfs 1, .LCPI8_0@toc@l(3)
; CHECK-FAST-NOVSX-NEXT:    fmr 2, 1
; CHECK-FAST-NOVSX-NEXT:    blr
;
; CHECK-LABEL: vec_neg_fma_flag_propagation:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    addis 3, 2, .LCPI8_0@toc@ha
; CHECK-NEXT:    addi 3, 3, .LCPI8_0@toc@l
; CHECK-NEXT:    lxvd2x 0, 0, 3
; CHECK-NEXT:    xxswapd 0, 0
; CHECK-NEXT:    xvmaddadp 34, 34, 0
; CHECK-NEXT:    blr
entry:
  %0 = call reassoc nnan <2 x double> @llvm.fma.v2f64(<2 x double> %a, <2 x double> <double -1.0, double -1.0>, <2 x double> %a)
  ret <2 x double> %0
}

declare double @llvm.fma.f64(double, double, double) nounwind readnone
declare <2 x double> @llvm.fma.v2f64(<2 x double>, <2 x double>, <2 x double>) nounwind readnone
