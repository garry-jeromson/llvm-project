; RUN: llc -march=w65816 < %s | FileCheck %s
; Test edge cases: zero values, max values, boundary conditions

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Zero value operations
;===----------------------------------------------------------------------===;

; CHECK-LABEL: add_zero:
; CHECK: rts
define i16 @add_zero(i16 %a) {
  %r = add i16 %a, 0
  ret i16 %r
}

; CHECK-LABEL: sub_zero:
; CHECK: rts
define i16 @sub_zero(i16 %a) {
  %r = sub i16 %a, 0
  ret i16 %r
}

; CHECK-LABEL: and_zero:
; CHECK: lda #0
; CHECK: rts
define i16 @and_zero(i16 %a) {
  %r = and i16 %a, 0
  ret i16 %r
}

; CHECK-LABEL: or_zero:
; CHECK: rts
define i16 @or_zero(i16 %a) {
  %r = or i16 %a, 0
  ret i16 %r
}

; CHECK-LABEL: xor_zero:
; CHECK: rts
define i16 @xor_zero(i16 %a) {
  %r = xor i16 %a, 0
  ret i16 %r
}

; CHECK-LABEL: shift_zero:
; CHECK: rts
define i16 @shift_zero(i16 %a) {
  %r = shl i16 %a, 0
  ret i16 %r
}

;===----------------------------------------------------------------------===;
; Max value operations (0xFFFF = 65535)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: load_max:
; CHECK: lda #-1
; CHECK: rts
define i16 @load_max() {
  ret i16 65535
}

; CHECK-LABEL: and_max:
; CHECK: rts
define i16 @and_max(i16 %a) {
  %r = and i16 %a, 65535
  ret i16 %r
}

; CHECK-LABEL: or_max:
; CHECK: lda #-1
; CHECK: rts
define i16 @or_max(i16 %a) {
  %r = or i16 %a, 65535
  ret i16 %r
}

; CHECK-LABEL: xor_max:
; CHECK: eor #-1
; CHECK: rts
define i16 @xor_max(i16 %a) {
  %r = xor i16 %a, 65535
  ret i16 %r
}

;===----------------------------------------------------------------------===;
; Comparisons with zero
;===----------------------------------------------------------------------===;

; CHECK-LABEL: compare_eq_zero:
; CHECK: cmp #0
define i16 @compare_eq_zero(i16 %a, i16 %t, i16 %f) {
  %cmp = icmp eq i16 %a, 0
  %r = select i1 %cmp, i16 %t, i16 %f
  ret i16 %r
}

; CHECK-LABEL: compare_ne_zero:
; CHECK: cmp #0
define i16 @compare_ne_zero(i16 %a, i16 %t, i16 %f) {
  %cmp = icmp ne i16 %a, 0
  %r = select i1 %cmp, i16 %t, i16 %f
  ret i16 %r
}

; CHECK-LABEL: compare_slt_zero:
; Signed less than zero = negative = check high bit
define i16 @compare_slt_zero(i16 %a, i16 %t, i16 %f) {
  %cmp = icmp slt i16 %a, 0
  %r = select i1 %cmp, i16 %t, i16 %f
  ret i16 %r
}

; CHECK-LABEL: compare_sge_zero:
; Signed >= 0 = non-negative = check high bit clear
define i16 @compare_sge_zero(i16 %a, i16 %t, i16 %f) {
  %cmp = icmp sge i16 %a, 0
  %r = select i1 %cmp, i16 %t, i16 %f
  ret i16 %r
}

;===----------------------------------------------------------------------===;
; Comparisons with constants
;===----------------------------------------------------------------------===;

; CHECK-LABEL: compare_eq_const:
; CHECK: cmp #42
define i16 @compare_eq_const(i16 %a, i16 %t, i16 %f) {
  %cmp = icmp eq i16 %a, 42
  %r = select i1 %cmp, i16 %t, i16 %f
  ret i16 %r
}

; CHECK-LABEL: compare_ult_const:
; CHECK: cmp #100
define i16 @compare_ult_const(i16 %a, i16 %t, i16 %f) {
  %cmp = icmp ult i16 %a, 100
  %r = select i1 %cmp, i16 %t, i16 %f
  ret i16 %r
}

;===----------------------------------------------------------------------===;
; Functions with zero arguments
;===----------------------------------------------------------------------===;

; CHECK-LABEL: no_args_return_const:
; CHECK: lda #123
; CHECK: rts
define i16 @no_args_return_const() {
  ret i16 123
}

@global_var = global i16 0

; CHECK-LABEL: no_args_load_global:
; CHECK: lda global_var
; CHECK: rts
define i16 @no_args_load_global() {
  %v = load i16, ptr @global_var
  ret i16 %v
}

;===----------------------------------------------------------------------===;
; Void return functions
;===----------------------------------------------------------------------===;

; CHECK-LABEL: void_return:
; CHECK: rts
define void @void_return() {
  ret void
}

; CHECK-LABEL: void_store_global:
; CHECK: sta global_var
; CHECK: rts
define void @void_store_global(i16 %v) {
  store i16 %v, ptr @global_var
  ret void
}

;===----------------------------------------------------------------------===;
; Arithmetic overflow/wraparound
;===----------------------------------------------------------------------===;

; CHECK-LABEL: add_overflow:
; 0x8000 + 0x8000 = 0x10000 wraps to 0x0000
; CHECK: clc
; CHECK: adc #-32768
; CHECK: rts
define i16 @add_overflow(i16 %a) {
  %r = add i16 %a, 32768
  ret i16 %r
}

; CHECK-LABEL: sub_underflow:
; Subtraction by 1 is optimized to DEC
; CHECK: dec a
; CHECK: rts
define i16 @sub_underflow(i16 %a) {
  %r = sub i16 %a, 1
  ret i16 %r
}
