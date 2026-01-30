; RUN: llc -march=w65816 < %s | FileCheck %s

; Test that redundant register transfers are eliminated by the peephole pass.
; Patterns like TAX;TXA, TAY;TYA should be removed when the intermediate
; register is not used.

target triple = "w65816-unknown-none"

; Test 1: Array access should not have TAY;TYA at the end
; The original codegen produces: lda array,x; tay; tya; rts
; After peephole: lda array,x; rts
define i16 @array_access(i16 %idx) {
; CHECK-LABEL: array_access:
; CHECK:       asl a
; CHECK-NEXT:  tax
; CHECK-NEXT:  lda array,x
; CHECK-NOT:   tay
; CHECK-NOT:   tya
; CHECK-NEXT:  rts
  %ptr = getelementptr [10 x i16], ptr @array, i16 0, i16 %idx
  %val = load i16, ptr %ptr
  ret i16 %val
}

@array = global [10 x i16] zeroinitializer

; Test 2: Simple add uses DP optimization (no redundant transfers to eliminate)
define i16 @simple_add(i16 %a, i16 %b) {
; CHECK-LABEL: simple_add:
; CHECK:       clc
; CHECK-NEXT:  stx $00fe
; CHECK-NEXT:  adc $00fe
; CHECK-NEXT:  rts
  %sum = add i16 %a, %b
  ret i16 %sum
}

; Test 3: Multiple adds use DP optimization
define i16 @multi_add(i16 %a, i16 %b, i16 %c) {
; CHECK-LABEL: multi_add:
; CHECK:       clc
; CHECK-NEXT:  stx $00fe
; CHECK-NEXT:  adc $00fe
; CHECK-NEXT:  clc
; CHECK-NEXT:  sty $00fe
; CHECK-NEXT:  adc $00fe
; CHECK-NEXT:  rts
  %sum1 = add i16 %a, %b
  %sum2 = add i16 %sum1, %c
  ret i16 %sum2
}

