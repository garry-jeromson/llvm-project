; RUN: llc -march=w65816 < %s | FileCheck %s
; Test varargs support

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

declare void @llvm.va_start(ptr)
declare void @llvm.va_end(ptr)

;===----------------------------------------------------------------------===
; Vararg function definition
; va_start stores the frame index of the first vararg on the stack
;===----------------------------------------------------------------------===

; CHECK-LABEL: vararg_func:
; CHECK: rts
define i16 @vararg_func(i16 %first, ...) {
entry:
  %ap = alloca ptr, align 2
  call void @llvm.va_start(ptr %ap)
  call void @llvm.va_end(ptr %ap)
  ret i16 %first
}

;===----------------------------------------------------------------------===
; Calling a vararg function
; First 3 args go in A, X, Y; rest on stack
;===----------------------------------------------------------------------===

declare i16 @external_vararg(i16, ...)

; CHECK-LABEL: call_vararg_4args:
; CHECK: lda #30
; CHECK: sta {{[0-9]+}},s
; CHECK: lda #3
; CHECK: ldx #10
; CHECK: ldy #20
; CHECK: jsr external_vararg
; CHECK: rts
define i16 @call_vararg_4args() {
  %result = call i16 (i16, ...) @external_vararg(i16 3, i16 10, i16 20, i16 30)
  ret i16 %result
}
