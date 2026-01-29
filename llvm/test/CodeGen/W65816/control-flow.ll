; RUN: llc -march=w65816 < %s | FileCheck %s
; Test control flow (calls, returns)
; Note: Conditional branches with icmp have known issues (setcc legalization)

target datalayout = "e-m:e-p:16:16-i16:16-n8:16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; Function Calls
;===----------------------------------------------------------------------===

declare i16 @external_func(i16)

; CHECK-LABEL: call_func:
; CHECK: jsr external_func
; CHECK: rts
define i16 @call_func(i16 %a) {
  %r = call i16 @external_func(i16 %a)
  ret i16 %r
}

; CHECK-LABEL: call_with_two_args:
; CHECK: jsr external_func2
; CHECK: rts
declare i16 @external_func2(i16, i16)
define i16 @call_with_two_args(i16 %a, i16 %b) {
  %r = call i16 @external_func2(i16 %a, i16 %b)
  ret i16 %r
}

;===----------------------------------------------------------------------===
; Return Values
;===----------------------------------------------------------------------===

; CHECK-LABEL: return_const:
; CHECK: lda #42
; CHECK: rts
define i16 @return_const() {
  ret i16 42
}

; CHECK-LABEL: return_zero:
; CHECK: lda #0
; CHECK: rts
define i16 @return_zero() {
  ret i16 0
}

; CHECK-LABEL: return_arg:
; CHECK: rts
define i16 @return_arg(i16 %a) {
  ret i16 %a
}

; CHECK-LABEL: return_max:
; CHECK: lda #-1
; CHECK: rts
define i16 @return_max() {
  ret i16 65535
}

;===----------------------------------------------------------------------===
; Unconditional Branch
;===----------------------------------------------------------------------===

; CHECK-LABEL: infinite_loop:
; CHECK: .L{{.*}}:
; CHECK: bra .L
define void @infinite_loop() {
entry:
  br label %loop
loop:
  br label %loop
}
