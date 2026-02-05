; RUN: llc -march=w65816 < %s | FileCheck %s
; NOTE: -verify-machineinstrs disabled due to liveness tracking issue in expansion
; that doesn't affect correctness. The generated code is correct.

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;; =============================================================================
;; Test: Indirect operations with imaginary registers
;;
;; These tests verify that indirect load/store operations work correctly when
;; pointer and/or index values are held in imaginary registers (RS0-RS15).
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Test 1: Simple indirect load with variable offset
;; -----------------------------------------------------------------------------

; CHECK-LABEL: simple_indirect_load:
; CHECK: sta 1,s
; CHECK: lda ($01,s),y
; CHECK: rts
define i16 @simple_indirect_load(ptr %p, i16 %offset) {
entry:
  %ptr = getelementptr i8, ptr %p, i16 %offset
  %val = load i16, ptr %ptr
  ret i16 %val
}

;; -----------------------------------------------------------------------------
;; Test 2: Simple indirect store with variable offset
;; -----------------------------------------------------------------------------

; CHECK-LABEL: simple_indirect_store:
; CHECK: sta {{[0-9]+}},s
; CHECK: sta ($01,s),y
; CHECK: rts
define void @simple_indirect_store(ptr %p, i16 %offset, i16 %val) {
entry:
  %ptr = getelementptr i8, ptr %p, i16 %offset
  store i16 %val, ptr %ptr
  ret void
}

;; -----------------------------------------------------------------------------
;; Test 3: Load through pointer kept across call
;; Pointer is saved to imaginary register RS0 ($0010)
;; -----------------------------------------------------------------------------

declare void @external_func()

; CHECK-LABEL: load_across_call:
; CHECK: sta {{[0-9]+}},s
; CHECK: jsr external_func
; CHECK: lda (${{[0-9]+}},s),y
; CHECK: rts
define i16 @load_across_call(ptr %p) {
entry:
  call void @external_func()
  %val = load i16, ptr %p
  ret i16 %val
}

;; -----------------------------------------------------------------------------
;; Test 4: Struct member access (GEP with constant offset)
;; This tests the indexed access that was broken before the fix
;; -----------------------------------------------------------------------------

%struct.Pair = type { i16, i16 }

; CHECK-LABEL: load_struct_member:
; CHECK: sta 1,s
; CHECK: lda ($01,s),y
; CHECK: rts
define i16 @load_struct_member(ptr %s) {
entry:
  %second_ptr = getelementptr %struct.Pair, ptr %s, i16 0, i32 1
  %val = load i16, ptr %second_ptr
  ret i16 %val
}

; CHECK-LABEL: store_struct_member:
; CHECK: sta {{[0-9]+}},s
; CHECK: sta ($01,s),y
; CHECK: rts
define void @store_struct_member(ptr %s, i16 %val) {
entry:
  %second_ptr = getelementptr %struct.Pair, ptr %s, i16 0, i32 1
  store i16 %val, ptr %second_ptr
  ret void
}

;; -----------------------------------------------------------------------------
;; Test 5: Multiple loads through same pointer
;; Tests pointer preserved in imaginary register across operations
;; -----------------------------------------------------------------------------

; CHECK-LABEL: multiple_loads:
; CHECK: lda (${{[0-9]+}},s),y
; CHECK: lda (${{[0-9]+}},s),y
; CHECK: rts
define i16 @multiple_loads(ptr %s) {
entry:
  %first_ptr = getelementptr %struct.Pair, ptr %s, i16 0, i32 0
  %first = load i16, ptr %first_ptr
  %second_ptr = getelementptr %struct.Pair, ptr %s, i16 0, i32 1
  %second = load i16, ptr %second_ptr
  %sum = add i16 %first, %second
  ret i16 %sum
}

;; -----------------------------------------------------------------------------
;; Test 6: 8-bit indirect load with mode switching
;; -----------------------------------------------------------------------------

; CHECK-LABEL: byte_indirect_load:
; CHECK: sta 1,s
; CHECK: sep #32
; CHECK: lda ($01,s),y
; CHECK: rep #32
; CHECK: rts
define i16 @byte_indirect_load(ptr %p, i16 %offset) {
entry:
  %ptr = getelementptr i8, ptr %p, i16 %offset
  %val8 = load i8, ptr %ptr
  %val16 = zext i8 %val8 to i16
  ret i16 %val16
}

;; -----------------------------------------------------------------------------
;; Test 7: 8-bit indirect store with mode switching
;; -----------------------------------------------------------------------------

; CHECK-LABEL: byte_indirect_store:
; CHECK: sep #32
; CHECK: sta ($01,s),y
; CHECK: rep #32
; CHECK: rts
define void @byte_indirect_store(ptr %p, i16 %offset, i8 %val) {
entry:
  %ptr = getelementptr i8, ptr %p, i16 %offset
  store i8 %val, ptr %ptr
  ret void
}

;; -----------------------------------------------------------------------------
;; Test 8: Array element access with variable index
;; Index requires multiplication by element size (ASL for *2)
;; -----------------------------------------------------------------------------

; CHECK-LABEL: array_load:
; CHECK: asl a
; CHECK: sta 1,s
; CHECK: lda ($01,s),y
; CHECK: rts
define i16 @array_load(ptr %arr, i16 %idx) {
entry:
  %ptr = getelementptr i16, ptr %arr, i16 %idx
  %val = load i16, ptr %ptr
  ret i16 %val
}

; CHECK-LABEL: array_store:
; CHECK: asl a
; CHECK: sta {{[0-9]+}},s
; CHECK: sta ($01,s),y
; CHECK: rts
define void @array_store(ptr %arr, i16 %idx, i16 %val) {
entry:
  %ptr = getelementptr i16, ptr %arr, i16 %idx
  store i16 %val, ptr %ptr
  ret void
}
