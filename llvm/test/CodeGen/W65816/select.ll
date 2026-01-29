; RUN: llc -march=w65816 < %s | FileCheck %s
; Test select operations (conditional value selection)

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; Basic Select with Comparison
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_select_eq:
; CHECK: rts
define i16 @test_select_eq(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp eq i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Min/Max Operations
;===----------------------------------------------------------------------===

; CHECK-LABEL: min_unsigned:
; CHECK: rts
define i16 @min_unsigned(i16 %a, i16 %b) {
  %cmp = icmp ult i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %b
  ret i16 %result
}

; CHECK-LABEL: max_unsigned:
; CHECK: rts
define i16 @max_unsigned(i16 %a, i16 %b) {
  %cmp = icmp ugt i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %b
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Select with Constants
;===----------------------------------------------------------------------===

; CHECK-LABEL: select_const:
; CHECK: rts
define i16 @select_const(i16 %a, i16 %b) {
  %cmp = icmp eq i16 %a, %b
  %result = select i1 %cmp, i16 42, i16 0
  ret i16 %result
}
