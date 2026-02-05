; RUN: llc -march=w65816 < %s | FileCheck %s
; Test that frame index used as a value (e.g., passed to a call) generates
; proper address computation using LEA_fi pseudo (TSC + ADC #offset)

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

declare void @use_ptr(ptr)

;===----------------------------------------------------------------------===;
; Basic case: pass address of stack allocation to function
;===----------------------------------------------------------------------===;

; CHECK-LABEL: pass_stack_addr:
; The frame index should be materialized using TSC + INC (offset=1, optimized from CLC+ADC#1)
; CHECK: tsc
; CHECK-NEXT: inc a
; CHECK: jsr use_ptr
define void @pass_stack_addr() {
  %var = alloca i16, align 2
  call void @use_ptr(ptr %var)
  ret void
}

;===----------------------------------------------------------------------===;
; Pass address of struct member on stack
;===----------------------------------------------------------------------===;

%struct.pair = type { i16, i16 }

; CHECK-LABEL: pass_struct_member_addr:
; CHECK: tsc
; CHECK-NEXT: inc a
; CHECK: clc
; CHECK-NEXT: adc #2
; CHECK: jsr use_ptr
define void @pass_struct_member_addr() {
  %s = alloca %struct.pair, align 2
  %second = getelementptr inbounds %struct.pair, ptr %s, i16 0, i32 1
  call void @use_ptr(ptr %second)
  ret void
}

;===----------------------------------------------------------------------===;
; Multiple alloca addresses passed to calls
;===----------------------------------------------------------------------===;

; CHECK-LABEL: multiple_stack_addrs:
; CHECK: tsc
; CHECK: clc
; CHECK: adc
; CHECK: jsr use_ptr
; CHECK: jsr use_ptr
define void @multiple_stack_addrs() {
  %a = alloca i16, align 2
  %b = alloca i16, align 2
  call void @use_ptr(ptr %a)
  call void @use_ptr(ptr %b)
  ret void
}
