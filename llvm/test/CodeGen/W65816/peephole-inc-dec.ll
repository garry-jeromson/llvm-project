; RUN: llc -march=w65816 < %s | FileCheck %s

; Test that CLC; ADC #1 is optimized to INC A
; and SEC; SBC #1 is optimized to DEC A

target triple = "w65816-unknown-none"

; Test 1: Add 1 should use INC A instead of CLC; ADC #1
define i16 @add_one(i16 %x) {
; CHECK-LABEL: add_one:
; CHECK:       inc a
; CHECK-NOT:   clc
; CHECK-NOT:   adc #1
; CHECK-NEXT:  rts
entry:
  %result = add i16 %x, 1
  ret i16 %result
}

; Test 2: Subtract 1 should use DEC A instead of SEC; SBC #1
define i16 @sub_one(i16 %x) {
; CHECK-LABEL: sub_one:
; CHECK:       dec a
; CHECK-NOT:   sec
; CHECK-NOT:   sbc #1
; CHECK-NEXT:  rts
entry:
  %result = sub i16 %x, 1
  ret i16 %result
}

; Test 3: Add 2 should still use CLC; ADC #2
define i16 @add_two(i16 %x) {
; CHECK-LABEL: add_two:
; CHECK:       clc
; CHECK-NEXT:  adc #2
; CHECK-NEXT:  rts
entry:
  %result = add i16 %x, 2
  ret i16 %result
}
