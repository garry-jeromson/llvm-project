; RUN: llc -march=w65816 < %s | FileCheck %s

; Test JSL/RTL for far function calls

; A far function that returns with RTL
define i16 @far_add(i16 %a, i16 %b) #0 {
; CHECK-LABEL: far_add:
; CHECK: clc
; CHECK: adc
; CHECK: rtl
  %sum = add i16 %a, %b
  ret i16 %sum
}

; A normal function calling a far function
define i16 @caller(i16 %x) {
; CHECK-LABEL: caller:
; CHECK: jsl far_add
; CHECK: rts
  %result = call i16 @far_add(i16 %x, i16 10)
  ret i16 %result
}

; A far function calling another far function
define i16 @far_caller(i16 %x) #0 {
; CHECK-LABEL: far_caller:
; CHECK: jsl far_add
; CHECK: rtl
  %result = call i16 @far_add(i16 %x, i16 20)
  ret i16 %result
}

; A normal function calling a normal function (unchanged behavior)
define i16 @normal_add(i16 %a, i16 %b) {
; CHECK-LABEL: normal_add:
; CHECK: rts
  %sum = add i16 %a, %b
  ret i16 %sum
}

define i16 @normal_caller(i16 %x) {
; CHECK-LABEL: normal_caller:
; CHECK: jsr normal_add
; CHECK: rts
  %result = call i16 @normal_add(i16 %x, i16 5)
  ret i16 %result
}

attributes #0 = { "w65816_farfunc" }
