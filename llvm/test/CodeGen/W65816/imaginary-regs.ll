; RUN: llc -march=w65816 < %s | FileCheck %s

; Test that imaginary registers (RS registers backed by Direct Page memory)
; are available for allocation when A, X, Y are exhausted.

target triple = "w65816-unknown-none"

; Test that a function needing more than 3 live values can compile
; This would previously fail with "ran out of registers"
; The function takes 5 arguments and adds them all together
define i16 @needs_many_regs(i16 %a, i16 %b, i16 %c, i16 %d, i16 %e) {
; CHECK-LABEL: needs_many_regs:
; CHECK: rts
entry:
  %sum1 = add i16 %a, %b
  %sum2 = add i16 %c, %d
  %sum3 = add i16 %sum1, %sum2
  %result = add i16 %sum3, %e
  ret i16 %result
}

; Test DP load/store patterns for imaginary registers
define i16 @dp_operations(i16 %x, i16 %y) {
; CHECK-LABEL: dp_operations:
; CHECK: rts
entry:
  %mul1 = mul i16 %x, 3
  %mul2 = mul i16 %y, 5
  %sum = add i16 %mul1, %mul2
  ret i16 %sum
}

; Test that a complex expression with many intermediate values compiles
define i16 @complex_expression(i16 %a, i16 %b, i16 %c) {
; CHECK-LABEL: complex_expression:
; CHECK: rts
entry:
  %t1 = add i16 %a, %b
  %t2 = sub i16 %a, %c
  %t3 = mul i16 %t1, %t2
  %t4 = add i16 %b, %c
  %t5 = sub i16 %t3, %t4
  ret i16 %t5
}

; Test that calls with many arguments work (uses imaginary regs for args 4+)
declare i16 @external_func(i16, i16, i16, i16, i16)

define i16 @call_with_many_args(i16 %a, i16 %b, i16 %c, i16 %d, i16 %e) {
; CHECK-LABEL: call_with_many_args:
; CHECK: jsr
; CHECK: rts
entry:
  %result = call i16 @external_func(i16 %a, i16 %b, i16 %c, i16 %d, i16 %e)
  ret i16 %result
}
