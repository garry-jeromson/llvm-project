; RUN: llc -march=w65816 < %s | FileCheck %s
; Test memory operation optimizations (INC/DEC/ASL/LSR on memory)

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

@counter = global i16 0
@dp_counter = global i16 0, section ".zeropage"

;===----------------------------------------------------------------------===
; Increment memory
;===----------------------------------------------------------------------===

; CHECK-LABEL: inc_absolute:
; GISel doesn't have memory INC optimization - uses load/inc/store
; CHECK: lda counter
; CHECK: inc a
; CHECK: sta counter
; CHECK: rts
define void @inc_absolute() {
  %val = load i16, ptr @counter
  %inc = add i16 %val, 1
  store i16 %inc, ptr @counter
  ret void
}

; CHECK-LABEL: inc_dp:
; GISel doesn't have memory INC optimization - uses load/inc/store
; CHECK: lda dp_counter
; CHECK: inc a
; CHECK: sta dp_counter
; CHECK: rts
define void @inc_dp() {
  %val = load i16, ptr @dp_counter
  %inc = add i16 %val, 1
  store i16 %inc, ptr @dp_counter
  ret void
}

;===----------------------------------------------------------------------===
; Decrement memory
;===----------------------------------------------------------------------===

; CHECK-LABEL: dec_absolute:
; GISel doesn't have memory DEC optimization - uses load/add -1/store
; CHECK: lda counter
; CHECK: clc
; CHECK: adc #65535
; CHECK: sta counter
; CHECK: rts
define void @dec_absolute() {
  %val = load i16, ptr @counter
  %dec = sub i16 %val, 1
  store i16 %dec, ptr @counter
  ret void
}

; CHECK-LABEL: dec_absolute_via_add:
; GISel doesn't have memory DEC optimization - uses load/add -1/store
; CHECK: lda counter
; CHECK: clc
; CHECK: adc #65535
; CHECK: sta counter
; CHECK: rts
define void @dec_absolute_via_add() {
  %val = load i16, ptr @counter
  %dec = add i16 %val, -1
  store i16 %dec, ptr @counter
  ret void
}

;===----------------------------------------------------------------------===
; Shift left memory (ASL)
;===----------------------------------------------------------------------===

; CHECK-LABEL: asl_absolute:
; GISel doesn't have memory ASL optimization - uses load/asl/store
; CHECK: lda counter
; CHECK: asl a
; CHECK: sta counter
; CHECK: rts
define void @asl_absolute() {
  %val = load i16, ptr @counter
  %shl = shl i16 %val, 1
  store i16 %shl, ptr @counter
  ret void
}

;===----------------------------------------------------------------------===
; Shift right memory (LSR)
;===----------------------------------------------------------------------===

; CHECK-LABEL: lsr_absolute:
; GISel doesn't have memory LSR optimization - uses load/lsr/store
; CHECK: lda counter
; CHECK: lsr a
; CHECK: sta counter
; CHECK: rts
define void @lsr_absolute() {
  %val = load i16, ptr @counter
  %shr = lshr i16 %val, 1
  store i16 %shr, ptr @counter
  ret void
}

;===----------------------------------------------------------------------===
; Non-optimizable cases (value used elsewhere)
;===----------------------------------------------------------------------===

; When value is used elsewhere, can't use memory operation
; CHECK-LABEL: inc_with_return:
; CHECK: lda counter
; CHECK: inc a
; CHECK: sta counter
; CHECK: rts
define i16 @inc_with_return() {
  %val = load i16, ptr @counter
  %inc = add i16 %val, 1
  store i16 %inc, ptr @counter
  ret i16 %inc
}
