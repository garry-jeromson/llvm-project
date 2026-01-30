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

;===----------------------------------------------------------------------===
; Signed Comparisons
;===----------------------------------------------------------------------===

; CHECK-LABEL: min_signed:
; CHECK: rts
define i16 @min_signed(i16 %a, i16 %b) {
  %cmp = icmp slt i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %b
  ret i16 %result
}

; CHECK-LABEL: max_signed:
; CHECK: rts
define i16 @max_signed(i16 %a, i16 %b) {
  %cmp = icmp sgt i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %b
  ret i16 %result
}

; CHECK-LABEL: select_sle:
; CHECK: rts
define i16 @select_sle(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp sle i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}

; CHECK-LABEL: select_sge:
; CHECK: rts
define i16 @select_sge(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp sge i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Unsigned Greater/Less or Equal (compound flag conditions)
;===----------------------------------------------------------------------===

; Test unsigned greater than (SETUGT) - requires C=1 AND Z=0
; After comparison (via SBC): C=1 means A >= B, Z=0 means A != B, so C=1 AND Z=0 means A > B
; CHECK-LABEL: test_select_ugt:
; CHECK: sbc
; CHECK: beq
; CHECK: bcs
; CHECK: rts
define i16 @test_select_ugt(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp ugt i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}

; Test unsigned less or equal (SETULE) - requires C=0 OR Z=1
; After comparison (via SBC): C=0 means A < B, Z=1 means A == B, so C=0 OR Z=1 means A <= B
; CHECK-LABEL: test_select_ule:
; CHECK: sbc
; CHECK: beq
; CHECK: bcc
; CHECK: rts
define i16 @test_select_ule(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp ule i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}
