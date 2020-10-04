; NOTE: Assertions have been autogenerated by utils/update_mir_test_checks.py
; RUN: llc -global-isel -mtriple=amdgcn-mesa-mesa3d -mcpu=tonga -stop-after=legalizer -global-isel-abort=0 -o - %s | FileCheck -check-prefix=UNPACKED %s
; RUN: llc -global-isel -mtriple=amdgcn-mesa-mesa3d -mcpu=gfx810 -stop-after=legalizer -global-isel-abort=0 -o - %s | FileCheck -check-prefix=PACKED %s

define amdgpu_ps void @image_store_f16(<8 x i32> inreg %rsrc, i32 %s, i32 %t, half %data) {
  ; UNPACKED-LABEL: name: image_store_f16
  ; UNPACKED: bb.1 (%ir-block.0):
  ; UNPACKED:   liveins: $sgpr2, $sgpr3, $sgpr4, $sgpr5, $sgpr6, $sgpr7, $sgpr8, $sgpr9, $vgpr0, $vgpr1, $vgpr2
  ; UNPACKED:   [[COPY:%[0-9]+]]:_(s32) = COPY $sgpr2
  ; UNPACKED:   [[COPY1:%[0-9]+]]:_(s32) = COPY $sgpr3
  ; UNPACKED:   [[COPY2:%[0-9]+]]:_(s32) = COPY $sgpr4
  ; UNPACKED:   [[COPY3:%[0-9]+]]:_(s32) = COPY $sgpr5
  ; UNPACKED:   [[COPY4:%[0-9]+]]:_(s32) = COPY $sgpr6
  ; UNPACKED:   [[COPY5:%[0-9]+]]:_(s32) = COPY $sgpr7
  ; UNPACKED:   [[COPY6:%[0-9]+]]:_(s32) = COPY $sgpr8
  ; UNPACKED:   [[COPY7:%[0-9]+]]:_(s32) = COPY $sgpr9
  ; UNPACKED:   [[COPY8:%[0-9]+]]:_(s32) = COPY $vgpr0
  ; UNPACKED:   [[COPY9:%[0-9]+]]:_(s32) = COPY $vgpr1
  ; UNPACKED:   [[COPY10:%[0-9]+]]:_(s32) = COPY $vgpr2
  ; UNPACKED:   [[TRUNC:%[0-9]+]]:_(s16) = G_TRUNC [[COPY10]](s32)
  ; UNPACKED:   [[BUILD_VECTOR:%[0-9]+]]:_(<8 x s32>) = G_BUILD_VECTOR [[COPY]](s32), [[COPY1]](s32), [[COPY2]](s32), [[COPY3]](s32), [[COPY4]](s32), [[COPY5]](s32), [[COPY6]](s32), [[COPY7]](s32)
  ; UNPACKED:   [[BUILD_VECTOR1:%[0-9]+]]:_(<2 x s32>) = G_BUILD_VECTOR [[COPY8]](s32), [[COPY9]](s32)
  ; UNPACKED:   G_AMDGPU_INTRIN_IMAGE_STORE intrinsic(@llvm.amdgcn.image.store.2d), [[TRUNC]](s16), 1, [[BUILD_VECTOR1]](<2 x s32>), $noreg, [[BUILD_VECTOR]](<8 x s32>), 0, 0, 0 :: (dereferenceable store 2 into custom "TargetCustom8")
  ; UNPACKED:   S_ENDPGM 0
  ; PACKED-LABEL: name: image_store_f16
  ; PACKED: bb.1 (%ir-block.0):
  ; PACKED:   liveins: $sgpr2, $sgpr3, $sgpr4, $sgpr5, $sgpr6, $sgpr7, $sgpr8, $sgpr9, $vgpr0, $vgpr1, $vgpr2
  ; PACKED:   [[COPY:%[0-9]+]]:_(s32) = COPY $sgpr2
  ; PACKED:   [[COPY1:%[0-9]+]]:_(s32) = COPY $sgpr3
  ; PACKED:   [[COPY2:%[0-9]+]]:_(s32) = COPY $sgpr4
  ; PACKED:   [[COPY3:%[0-9]+]]:_(s32) = COPY $sgpr5
  ; PACKED:   [[COPY4:%[0-9]+]]:_(s32) = COPY $sgpr6
  ; PACKED:   [[COPY5:%[0-9]+]]:_(s32) = COPY $sgpr7
  ; PACKED:   [[COPY6:%[0-9]+]]:_(s32) = COPY $sgpr8
  ; PACKED:   [[COPY7:%[0-9]+]]:_(s32) = COPY $sgpr9
  ; PACKED:   [[COPY8:%[0-9]+]]:_(s32) = COPY $vgpr0
  ; PACKED:   [[COPY9:%[0-9]+]]:_(s32) = COPY $vgpr1
  ; PACKED:   [[COPY10:%[0-9]+]]:_(s32) = COPY $vgpr2
  ; PACKED:   [[TRUNC:%[0-9]+]]:_(s16) = G_TRUNC [[COPY10]](s32)
  ; PACKED:   [[BUILD_VECTOR:%[0-9]+]]:_(<8 x s32>) = G_BUILD_VECTOR [[COPY]](s32), [[COPY1]](s32), [[COPY2]](s32), [[COPY3]](s32), [[COPY4]](s32), [[COPY5]](s32), [[COPY6]](s32), [[COPY7]](s32)
  ; PACKED:   [[BUILD_VECTOR1:%[0-9]+]]:_(<2 x s32>) = G_BUILD_VECTOR [[COPY8]](s32), [[COPY9]](s32)
  ; PACKED:   G_AMDGPU_INTRIN_IMAGE_STORE intrinsic(@llvm.amdgcn.image.store.2d), [[TRUNC]](s16), 1, [[BUILD_VECTOR1]](<2 x s32>), $noreg, [[BUILD_VECTOR]](<8 x s32>), 0, 0, 0 :: (dereferenceable store 2 into custom "TargetCustom8")
  ; PACKED:   S_ENDPGM 0
  call void @llvm.amdgcn.image.store.2d.f16.i32(half %data, i32 1, i32 %s, i32 %t, <8 x i32> %rsrc, i32 0, i32 0)
  ret void
}

define amdgpu_ps void @image_store_v2f16(<8 x i32> inreg %rsrc, i32 %s, i32 %t, <2 x half> %in) {
  ; UNPACKED-LABEL: name: image_store_v2f16
  ; UNPACKED: bb.1 (%ir-block.0):
  ; UNPACKED:   liveins: $sgpr2, $sgpr3, $sgpr4, $sgpr5, $sgpr6, $sgpr7, $sgpr8, $sgpr9, $vgpr0, $vgpr1, $vgpr2
  ; UNPACKED:   [[COPY:%[0-9]+]]:_(s32) = COPY $sgpr2
  ; UNPACKED:   [[COPY1:%[0-9]+]]:_(s32) = COPY $sgpr3
  ; UNPACKED:   [[COPY2:%[0-9]+]]:_(s32) = COPY $sgpr4
  ; UNPACKED:   [[COPY3:%[0-9]+]]:_(s32) = COPY $sgpr5
  ; UNPACKED:   [[COPY4:%[0-9]+]]:_(s32) = COPY $sgpr6
  ; UNPACKED:   [[COPY5:%[0-9]+]]:_(s32) = COPY $sgpr7
  ; UNPACKED:   [[COPY6:%[0-9]+]]:_(s32) = COPY $sgpr8
  ; UNPACKED:   [[COPY7:%[0-9]+]]:_(s32) = COPY $sgpr9
  ; UNPACKED:   [[COPY8:%[0-9]+]]:_(s32) = COPY $vgpr0
  ; UNPACKED:   [[COPY9:%[0-9]+]]:_(s32) = COPY $vgpr1
  ; UNPACKED:   [[COPY10:%[0-9]+]]:_(<2 x s16>) = COPY $vgpr2
  ; UNPACKED:   [[BUILD_VECTOR:%[0-9]+]]:_(<8 x s32>) = G_BUILD_VECTOR [[COPY]](s32), [[COPY1]](s32), [[COPY2]](s32), [[COPY3]](s32), [[COPY4]](s32), [[COPY5]](s32), [[COPY6]](s32), [[COPY7]](s32)
  ; UNPACKED:   [[BUILD_VECTOR1:%[0-9]+]]:_(<2 x s32>) = G_BUILD_VECTOR [[COPY8]](s32), [[COPY9]](s32)
  ; UNPACKED:   [[BITCAST:%[0-9]+]]:_(s32) = G_BITCAST [[COPY10]](<2 x s16>)
  ; UNPACKED:   [[C:%[0-9]+]]:_(s32) = G_CONSTANT i32 16
  ; UNPACKED:   [[LSHR:%[0-9]+]]:_(s32) = G_LSHR [[BITCAST]], [[C]](s32)
  ; UNPACKED:   [[COPY11:%[0-9]+]]:_(s32) = COPY [[BITCAST]](s32)
  ; UNPACKED:   [[COPY12:%[0-9]+]]:_(s32) = COPY [[LSHR]](s32)
  ; UNPACKED:   [[BUILD_VECTOR2:%[0-9]+]]:_(<2 x s32>) = G_BUILD_VECTOR [[COPY11]](s32), [[COPY12]](s32)
  ; UNPACKED:   G_AMDGPU_INTRIN_IMAGE_STORE intrinsic(@llvm.amdgcn.image.store.2d), [[BUILD_VECTOR2]](<2 x s32>), 3, [[BUILD_VECTOR1]](<2 x s32>), $noreg, [[BUILD_VECTOR]](<8 x s32>), 0, 0, 0 :: (dereferenceable store 4 into custom "TargetCustom8")
  ; UNPACKED:   S_ENDPGM 0
  ; PACKED-LABEL: name: image_store_v2f16
  ; PACKED: bb.1 (%ir-block.0):
  ; PACKED:   liveins: $sgpr2, $sgpr3, $sgpr4, $sgpr5, $sgpr6, $sgpr7, $sgpr8, $sgpr9, $vgpr0, $vgpr1, $vgpr2
  ; PACKED:   [[COPY:%[0-9]+]]:_(s32) = COPY $sgpr2
  ; PACKED:   [[COPY1:%[0-9]+]]:_(s32) = COPY $sgpr3
  ; PACKED:   [[COPY2:%[0-9]+]]:_(s32) = COPY $sgpr4
  ; PACKED:   [[COPY3:%[0-9]+]]:_(s32) = COPY $sgpr5
  ; PACKED:   [[COPY4:%[0-9]+]]:_(s32) = COPY $sgpr6
  ; PACKED:   [[COPY5:%[0-9]+]]:_(s32) = COPY $sgpr7
  ; PACKED:   [[COPY6:%[0-9]+]]:_(s32) = COPY $sgpr8
  ; PACKED:   [[COPY7:%[0-9]+]]:_(s32) = COPY $sgpr9
  ; PACKED:   [[COPY8:%[0-9]+]]:_(s32) = COPY $vgpr0
  ; PACKED:   [[COPY9:%[0-9]+]]:_(s32) = COPY $vgpr1
  ; PACKED:   [[COPY10:%[0-9]+]]:_(<2 x s16>) = COPY $vgpr2
  ; PACKED:   [[BUILD_VECTOR:%[0-9]+]]:_(<8 x s32>) = G_BUILD_VECTOR [[COPY]](s32), [[COPY1]](s32), [[COPY2]](s32), [[COPY3]](s32), [[COPY4]](s32), [[COPY5]](s32), [[COPY6]](s32), [[COPY7]](s32)
  ; PACKED:   [[BUILD_VECTOR1:%[0-9]+]]:_(<2 x s32>) = G_BUILD_VECTOR [[COPY8]](s32), [[COPY9]](s32)
  ; PACKED:   G_AMDGPU_INTRIN_IMAGE_STORE intrinsic(@llvm.amdgcn.image.store.2d), [[COPY10]](<2 x s16>), 3, [[BUILD_VECTOR1]](<2 x s32>), $noreg, [[BUILD_VECTOR]](<8 x s32>), 0, 0, 0 :: (dereferenceable store 4 into custom "TargetCustom8")
  ; PACKED:   S_ENDPGM 0
  call void @llvm.amdgcn.image.store.2d.v2f16.i32(<2 x half> %in, i32 3, i32 %s, i32 %t, <8 x i32> %rsrc, i32 0, i32 0)
  ret void
}

define amdgpu_ps void @image_store_v3f16(<8 x i32> inreg %rsrc, i32 %s, i32 %t, <3 x half> %in) {
  ; UNPACKED-LABEL: name: image_store_v3f16
  ; UNPACKED: bb.1 (%ir-block.0):
  ; UNPACKED:   liveins: $sgpr2, $sgpr3, $sgpr4, $sgpr5, $sgpr6, $sgpr7, $sgpr8, $sgpr9, $vgpr0, $vgpr1, $vgpr2, $vgpr3
  ; UNPACKED:   [[COPY:%[0-9]+]]:_(s32) = COPY $sgpr2
  ; UNPACKED:   [[COPY1:%[0-9]+]]:_(s32) = COPY $sgpr3
  ; UNPACKED:   [[COPY2:%[0-9]+]]:_(s32) = COPY $sgpr4
  ; UNPACKED:   [[COPY3:%[0-9]+]]:_(s32) = COPY $sgpr5
  ; UNPACKED:   [[COPY4:%[0-9]+]]:_(s32) = COPY $sgpr6
  ; UNPACKED:   [[COPY5:%[0-9]+]]:_(s32) = COPY $sgpr7
  ; UNPACKED:   [[COPY6:%[0-9]+]]:_(s32) = COPY $sgpr8
  ; UNPACKED:   [[COPY7:%[0-9]+]]:_(s32) = COPY $sgpr9
  ; UNPACKED:   [[COPY8:%[0-9]+]]:_(s32) = COPY $vgpr0
  ; UNPACKED:   [[COPY9:%[0-9]+]]:_(s32) = COPY $vgpr1
  ; UNPACKED:   [[COPY10:%[0-9]+]]:_(<2 x s16>) = COPY $vgpr2
  ; UNPACKED:   [[COPY11:%[0-9]+]]:_(<2 x s16>) = COPY $vgpr3
  ; UNPACKED:   [[BUILD_VECTOR:%[0-9]+]]:_(<8 x s32>) = G_BUILD_VECTOR [[COPY]](s32), [[COPY1]](s32), [[COPY2]](s32), [[COPY3]](s32), [[COPY4]](s32), [[COPY5]](s32), [[COPY6]](s32), [[COPY7]](s32)
  ; UNPACKED:   [[DEF:%[0-9]+]]:_(<2 x s16>) = G_IMPLICIT_DEF
  ; UNPACKED:   [[CONCAT_VECTORS:%[0-9]+]]:_(<6 x s16>) = G_CONCAT_VECTORS [[COPY10]](<2 x s16>), [[COPY11]](<2 x s16>), [[DEF]](<2 x s16>)
  ; UNPACKED:   [[BITCAST:%[0-9]+]]:_(s96) = G_BITCAST [[CONCAT_VECTORS]](<6 x s16>)
  ; UNPACKED:   [[UV:%[0-9]+]]:_(s32), [[UV1:%[0-9]+]]:_(s32), [[UV2:%[0-9]+]]:_(s32) = G_UNMERGE_VALUES [[BITCAST]](s96)
  ; UNPACKED:   [[C:%[0-9]+]]:_(s32) = G_CONSTANT i32 16
  ; UNPACKED:   [[LSHR:%[0-9]+]]:_(s32) = G_LSHR [[UV]], [[C]](s32)
  ; UNPACKED:   [[LSHR1:%[0-9]+]]:_(s32) = G_LSHR [[UV1]], [[C]](s32)
  ; UNPACKED:   [[BUILD_VECTOR1:%[0-9]+]]:_(<2 x s32>) = G_BUILD_VECTOR [[COPY8]](s32), [[COPY9]](s32)
  ; UNPACKED:   [[COPY12:%[0-9]+]]:_(s32) = COPY [[UV]](s32)
  ; UNPACKED:   [[COPY13:%[0-9]+]]:_(s32) = COPY [[LSHR]](s32)
  ; UNPACKED:   [[COPY14:%[0-9]+]]:_(s32) = COPY [[UV1]](s32)
  ; UNPACKED:   [[BUILD_VECTOR2:%[0-9]+]]:_(<3 x s32>) = G_BUILD_VECTOR [[COPY12]](s32), [[COPY13]](s32), [[COPY14]](s32)
  ; UNPACKED:   G_AMDGPU_INTRIN_IMAGE_STORE intrinsic(@llvm.amdgcn.image.store.2d), [[BUILD_VECTOR2]](<3 x s32>), 7, [[BUILD_VECTOR1]](<2 x s32>), $noreg, [[BUILD_VECTOR]](<8 x s32>), 0, 0, 0 :: (dereferenceable store 6 into custom "TargetCustom8", align 8)
  ; UNPACKED:   S_ENDPGM 0
  ; PACKED-LABEL: name: image_store_v3f16
  ; PACKED: bb.1 (%ir-block.0):
  ; PACKED:   liveins: $sgpr2, $sgpr3, $sgpr4, $sgpr5, $sgpr6, $sgpr7, $sgpr8, $sgpr9, $vgpr0, $vgpr1, $vgpr2, $vgpr3
  ; PACKED:   [[COPY:%[0-9]+]]:_(s32) = COPY $sgpr2
  ; PACKED:   [[COPY1:%[0-9]+]]:_(s32) = COPY $sgpr3
  ; PACKED:   [[COPY2:%[0-9]+]]:_(s32) = COPY $sgpr4
  ; PACKED:   [[COPY3:%[0-9]+]]:_(s32) = COPY $sgpr5
  ; PACKED:   [[COPY4:%[0-9]+]]:_(s32) = COPY $sgpr6
  ; PACKED:   [[COPY5:%[0-9]+]]:_(s32) = COPY $sgpr7
  ; PACKED:   [[COPY6:%[0-9]+]]:_(s32) = COPY $sgpr8
  ; PACKED:   [[COPY7:%[0-9]+]]:_(s32) = COPY $sgpr9
  ; PACKED:   [[COPY8:%[0-9]+]]:_(s32) = COPY $vgpr0
  ; PACKED:   [[COPY9:%[0-9]+]]:_(s32) = COPY $vgpr1
  ; PACKED:   [[COPY10:%[0-9]+]]:_(<2 x s16>) = COPY $vgpr2
  ; PACKED:   [[COPY11:%[0-9]+]]:_(<2 x s16>) = COPY $vgpr3
  ; PACKED:   [[BUILD_VECTOR:%[0-9]+]]:_(<8 x s32>) = G_BUILD_VECTOR [[COPY]](s32), [[COPY1]](s32), [[COPY2]](s32), [[COPY3]](s32), [[COPY4]](s32), [[COPY5]](s32), [[COPY6]](s32), [[COPY7]](s32)
  ; PACKED:   [[DEF:%[0-9]+]]:_(<2 x s16>) = G_IMPLICIT_DEF
  ; PACKED:   [[CONCAT_VECTORS:%[0-9]+]]:_(<6 x s16>) = G_CONCAT_VECTORS [[COPY10]](<2 x s16>), [[COPY11]](<2 x s16>), [[DEF]](<2 x s16>)
  ; PACKED:   [[BITCAST:%[0-9]+]]:_(s96) = G_BITCAST [[CONCAT_VECTORS]](<6 x s16>)
  ; PACKED:   [[UV:%[0-9]+]]:_(s32), [[UV1:%[0-9]+]]:_(s32), [[UV2:%[0-9]+]]:_(s32) = G_UNMERGE_VALUES [[BITCAST]](s96)
  ; PACKED:   [[C:%[0-9]+]]:_(s32) = G_CONSTANT i32 16
  ; PACKED:   [[LSHR:%[0-9]+]]:_(s32) = G_LSHR [[UV]], [[C]](s32)
  ; PACKED:   [[LSHR1:%[0-9]+]]:_(s32) = G_LSHR [[UV1]], [[C]](s32)
  ; PACKED:   [[C1:%[0-9]+]]:_(s32) = G_CONSTANT i32 65535
  ; PACKED:   [[COPY12:%[0-9]+]]:_(s32) = COPY [[UV]](s32)
  ; PACKED:   [[AND:%[0-9]+]]:_(s32) = G_AND [[COPY12]], [[C1]]
  ; PACKED:   [[COPY13:%[0-9]+]]:_(s32) = COPY [[LSHR]](s32)
  ; PACKED:   [[AND1:%[0-9]+]]:_(s32) = G_AND [[COPY13]], [[C1]]
  ; PACKED:   [[SHL:%[0-9]+]]:_(s32) = G_SHL [[AND1]], [[C]](s32)
  ; PACKED:   [[OR:%[0-9]+]]:_(s32) = G_OR [[AND]], [[SHL]]
  ; PACKED:   [[BITCAST1:%[0-9]+]]:_(<2 x s16>) = G_BITCAST [[OR]](s32)
  ; PACKED:   [[COPY14:%[0-9]+]]:_(s32) = COPY [[UV1]](s32)
  ; PACKED:   [[AND2:%[0-9]+]]:_(s32) = G_AND [[COPY14]], [[C1]]
  ; PACKED:   [[C2:%[0-9]+]]:_(s32) = G_CONSTANT i32 0
  ; PACKED:   [[SHL1:%[0-9]+]]:_(s32) = G_SHL [[C2]], [[C]](s32)
  ; PACKED:   [[OR1:%[0-9]+]]:_(s32) = G_OR [[AND2]], [[SHL1]]
  ; PACKED:   [[BITCAST2:%[0-9]+]]:_(<2 x s16>) = G_BITCAST [[OR1]](s32)
  ; PACKED:   [[CONCAT_VECTORS1:%[0-9]+]]:_(<6 x s16>) = G_CONCAT_VECTORS [[BITCAST1]](<2 x s16>), [[BITCAST2]](<2 x s16>), [[DEF]](<2 x s16>)
  ; PACKED:   [[UV3:%[0-9]+]]:_(<3 x s16>), [[UV4:%[0-9]+]]:_(<3 x s16>) = G_UNMERGE_VALUES [[CONCAT_VECTORS1]](<6 x s16>)
  ; PACKED:   [[BUILD_VECTOR1:%[0-9]+]]:_(<2 x s32>) = G_BUILD_VECTOR [[COPY8]](s32), [[COPY9]](s32)
  ; PACKED:   G_AMDGPU_INTRIN_IMAGE_STORE intrinsic(@llvm.amdgcn.image.store.2d), [[UV3]](<3 x s16>), 7, [[BUILD_VECTOR1]](<2 x s32>), $noreg, [[BUILD_VECTOR]](<8 x s32>), 0, 0, 0 :: (dereferenceable store 6 into custom "TargetCustom8", align 8)
  ; PACKED:   S_ENDPGM 0
  call void @llvm.amdgcn.image.store.2d.v3f16.i32(<3 x half> %in, i32 7, i32 %s, i32 %t, <8 x i32> %rsrc, i32 0, i32 0)
  ret void
}

define amdgpu_ps void @image_store_v4f16(<8 x i32> inreg %rsrc, i32 %s, i32 %t, <4 x half> %in) {
  ; UNPACKED-LABEL: name: image_store_v4f16
  ; UNPACKED: bb.1 (%ir-block.0):
  ; UNPACKED:   liveins: $sgpr2, $sgpr3, $sgpr4, $sgpr5, $sgpr6, $sgpr7, $sgpr8, $sgpr9, $vgpr0, $vgpr1, $vgpr2, $vgpr3
  ; UNPACKED:   [[COPY:%[0-9]+]]:_(s32) = COPY $sgpr2
  ; UNPACKED:   [[COPY1:%[0-9]+]]:_(s32) = COPY $sgpr3
  ; UNPACKED:   [[COPY2:%[0-9]+]]:_(s32) = COPY $sgpr4
  ; UNPACKED:   [[COPY3:%[0-9]+]]:_(s32) = COPY $sgpr5
  ; UNPACKED:   [[COPY4:%[0-9]+]]:_(s32) = COPY $sgpr6
  ; UNPACKED:   [[COPY5:%[0-9]+]]:_(s32) = COPY $sgpr7
  ; UNPACKED:   [[COPY6:%[0-9]+]]:_(s32) = COPY $sgpr8
  ; UNPACKED:   [[COPY7:%[0-9]+]]:_(s32) = COPY $sgpr9
  ; UNPACKED:   [[COPY8:%[0-9]+]]:_(s32) = COPY $vgpr0
  ; UNPACKED:   [[COPY9:%[0-9]+]]:_(s32) = COPY $vgpr1
  ; UNPACKED:   [[COPY10:%[0-9]+]]:_(<2 x s16>) = COPY $vgpr2
  ; UNPACKED:   [[COPY11:%[0-9]+]]:_(<2 x s16>) = COPY $vgpr3
  ; UNPACKED:   [[BUILD_VECTOR:%[0-9]+]]:_(<8 x s32>) = G_BUILD_VECTOR [[COPY]](s32), [[COPY1]](s32), [[COPY2]](s32), [[COPY3]](s32), [[COPY4]](s32), [[COPY5]](s32), [[COPY6]](s32), [[COPY7]](s32)
  ; UNPACKED:   [[BUILD_VECTOR1:%[0-9]+]]:_(<2 x s32>) = G_BUILD_VECTOR [[COPY8]](s32), [[COPY9]](s32)
  ; UNPACKED:   [[BITCAST:%[0-9]+]]:_(s32) = G_BITCAST [[COPY10]](<2 x s16>)
  ; UNPACKED:   [[C:%[0-9]+]]:_(s32) = G_CONSTANT i32 16
  ; UNPACKED:   [[LSHR:%[0-9]+]]:_(s32) = G_LSHR [[BITCAST]], [[C]](s32)
  ; UNPACKED:   [[BITCAST1:%[0-9]+]]:_(s32) = G_BITCAST [[COPY11]](<2 x s16>)
  ; UNPACKED:   [[LSHR1:%[0-9]+]]:_(s32) = G_LSHR [[BITCAST1]], [[C]](s32)
  ; UNPACKED:   [[COPY12:%[0-9]+]]:_(s32) = COPY [[BITCAST]](s32)
  ; UNPACKED:   [[COPY13:%[0-9]+]]:_(s32) = COPY [[LSHR]](s32)
  ; UNPACKED:   [[COPY14:%[0-9]+]]:_(s32) = COPY [[BITCAST1]](s32)
  ; UNPACKED:   [[COPY15:%[0-9]+]]:_(s32) = COPY [[LSHR1]](s32)
  ; UNPACKED:   [[BUILD_VECTOR2:%[0-9]+]]:_(<4 x s32>) = G_BUILD_VECTOR [[COPY12]](s32), [[COPY13]](s32), [[COPY14]](s32), [[COPY15]](s32)
  ; UNPACKED:   G_AMDGPU_INTRIN_IMAGE_STORE intrinsic(@llvm.amdgcn.image.store.2d), [[BUILD_VECTOR2]](<4 x s32>), 15, [[BUILD_VECTOR1]](<2 x s32>), $noreg, [[BUILD_VECTOR]](<8 x s32>), 0, 0, 0 :: (dereferenceable store 8 into custom "TargetCustom8")
  ; UNPACKED:   S_ENDPGM 0
  ; PACKED-LABEL: name: image_store_v4f16
  ; PACKED: bb.1 (%ir-block.0):
  ; PACKED:   liveins: $sgpr2, $sgpr3, $sgpr4, $sgpr5, $sgpr6, $sgpr7, $sgpr8, $sgpr9, $vgpr0, $vgpr1, $vgpr2, $vgpr3
  ; PACKED:   [[COPY:%[0-9]+]]:_(s32) = COPY $sgpr2
  ; PACKED:   [[COPY1:%[0-9]+]]:_(s32) = COPY $sgpr3
  ; PACKED:   [[COPY2:%[0-9]+]]:_(s32) = COPY $sgpr4
  ; PACKED:   [[COPY3:%[0-9]+]]:_(s32) = COPY $sgpr5
  ; PACKED:   [[COPY4:%[0-9]+]]:_(s32) = COPY $sgpr6
  ; PACKED:   [[COPY5:%[0-9]+]]:_(s32) = COPY $sgpr7
  ; PACKED:   [[COPY6:%[0-9]+]]:_(s32) = COPY $sgpr8
  ; PACKED:   [[COPY7:%[0-9]+]]:_(s32) = COPY $sgpr9
  ; PACKED:   [[COPY8:%[0-9]+]]:_(s32) = COPY $vgpr0
  ; PACKED:   [[COPY9:%[0-9]+]]:_(s32) = COPY $vgpr1
  ; PACKED:   [[COPY10:%[0-9]+]]:_(<2 x s16>) = COPY $vgpr2
  ; PACKED:   [[COPY11:%[0-9]+]]:_(<2 x s16>) = COPY $vgpr3
  ; PACKED:   [[BUILD_VECTOR:%[0-9]+]]:_(<8 x s32>) = G_BUILD_VECTOR [[COPY]](s32), [[COPY1]](s32), [[COPY2]](s32), [[COPY3]](s32), [[COPY4]](s32), [[COPY5]](s32), [[COPY6]](s32), [[COPY7]](s32)
  ; PACKED:   [[CONCAT_VECTORS:%[0-9]+]]:_(<4 x s16>) = G_CONCAT_VECTORS [[COPY10]](<2 x s16>), [[COPY11]](<2 x s16>)
  ; PACKED:   [[BUILD_VECTOR1:%[0-9]+]]:_(<2 x s32>) = G_BUILD_VECTOR [[COPY8]](s32), [[COPY9]](s32)
  ; PACKED:   G_AMDGPU_INTRIN_IMAGE_STORE intrinsic(@llvm.amdgcn.image.store.2d), [[CONCAT_VECTORS]](<4 x s16>), 15, [[BUILD_VECTOR1]](<2 x s32>), $noreg, [[BUILD_VECTOR]](<8 x s32>), 0, 0, 0 :: (dereferenceable store 8 into custom "TargetCustom8")
  ; PACKED:   S_ENDPGM 0
  call void @llvm.amdgcn.image.store.2d.v4f16.i32(<4 x half> %in, i32 15, i32 %s, i32 %t, <8 x i32> %rsrc, i32 0, i32 0)
  ret void
}

declare void @llvm.amdgcn.image.store.2d.f16.i32(half, i32 immarg, i32, i32, <8 x i32>, i32 immarg, i32 immarg) #0
declare void @llvm.amdgcn.image.store.2d.v2f16.i32(<2 x half>, i32 immarg, i32, i32, <8 x i32>, i32 immarg, i32 immarg) #0
declare void @llvm.amdgcn.image.store.2d.v3f16.i32(<3 x half>, i32 immarg, i32, i32, <8 x i32>, i32 immarg, i32 immarg) #0
declare void @llvm.amdgcn.image.store.2d.v4f16.i32(<4 x half>, i32 immarg, i32, i32, <8 x i32>, i32 immarg, i32 immarg) #0

attributes #0 = { nounwind writeonly }
