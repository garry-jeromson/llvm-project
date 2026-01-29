; RUN: llc -march=w65816 < %s | FileCheck %s
; Test arithmetic operations

target datalayout = "e-m:e-p:16:16-i16:16-n8:16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; Addition (register-register)
;===----------------------------------------------------------------------===

; CHECK-LABEL: add_regs:
; CHECK: clc
; CHECK: adc
; CHECK: rts
define i16 @add_regs(i16 %a, i16 %b) {
  %r = add i16 %a, %b
  ret i16 %r
}

;===----------------------------------------------------------------------===
; Subtraction (register-register)
;===----------------------------------------------------------------------===

; CHECK-LABEL: sub_regs:
; CHECK: sec
; CHECK: sbc
; CHECK: rts
define i16 @sub_regs(i16 %a, i16 %b) {
  %r = sub i16 %a, %b
  ret i16 %r
}

;===----------------------------------------------------------------------===
; Increment/Decrement (optimized to single instruction)
;===----------------------------------------------------------------------===

; CHECK-LABEL: inc_reg:
; CHECK: inc a
; CHECK: rts
define i16 @inc_reg(i16 %a) {
  %r = add i16 %a, 1
  ret i16 %r
}

; CHECK-LABEL: dec_reg:
; CHECK: dec a
; CHECK: rts
define i16 @dec_reg(i16 %a) {
  %r = add i16 %a, -1
  ret i16 %r
}
