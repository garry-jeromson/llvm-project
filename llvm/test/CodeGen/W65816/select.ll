; RUN: llc -march=w65816 < %s | FileCheck %s
; Test select operations (conditional value selection)

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; Basic Select with Comparison
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_select_eq:
; Unsigned comparison (EQ) uses non-destructive CMP
; CHECK: cmp
; CHECK: bne
; CHECK: rts
define i16 @test_select_eq(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp eq i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Unsigned Select Operations
; (Avoid icmp+select where true/false match compare operands to prevent G_UMIN/G_UMAX)
;===----------------------------------------------------------------------===

; CHECK-LABEL: select_ult:
; Unsigned comparison uses non-destructive CMP
; CHECK: cmp
; CHECK: bcs
; CHECK: rts
define i16 @select_ult(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp ult i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}

; CHECK-LABEL: select_ugt:
; Unsigned comparison uses non-destructive CMP
; CHECK: cmp
; CHECK: beq
; CHECK: bcc
; CHECK: rts
define i16 @select_ugt(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp ugt i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Select with Constants
;===----------------------------------------------------------------------===

; CHECK-LABEL: select_const:
; Unsigned comparison (EQ) uses non-destructive CMP
; CHECK: cmp
; CHECK: bne
; CHECK: rts
define i16 @select_const(i16 %a, i16 %b) {
  %cmp = icmp eq i16 %a, %b
  %result = select i1 %cmp, i16 42, i16 0
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Signed Comparisons
; (Avoid icmp+select where true/false match compare operands to prevent G_SMIN/G_SMAX)
;===----------------------------------------------------------------------===

; CHECK-LABEL: select_slt:
; Signed comparison uses SEC + SBC (sets V flag)
; CHECK: sec
; CHECK: sbc
; CHECK: bvs
; CHECK: rts
define i16 @select_slt(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp slt i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}

; CHECK-LABEL: select_sgt:
; Signed comparison uses SEC + SBC (sets V flag)
; CHECK: sec
; CHECK: sbc
; CHECK: bvs
; CHECK: rts
define i16 @select_sgt(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp sgt i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}

; CHECK-LABEL: select_sle:
; Signed comparison uses SEC + SBC (sets V flag)
; CHECK: sec
; CHECK: sbc
; CHECK: beq
; CHECK: bvs
; CHECK: rts
define i16 @select_sle(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp sle i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}

; CHECK-LABEL: select_sge:
; Signed comparison uses SEC + SBC (sets V flag)
; CHECK: sec
; CHECK: sbc
; CHECK: bvs
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
; CHECK-LABEL: test_select_ugt:
; Unsigned comparison uses non-destructive CMP
; CHECK: cmp
; CHECK: beq
; CHECK: bcc
; CHECK: rts
define i16 @test_select_ugt(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp ugt i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}

; Test unsigned less or equal (SETULE) - requires C=0 OR Z=1
; CHECK-LABEL: test_select_ule:
; Unsigned comparison uses non-destructive CMP
; CHECK: cmp
; CHECK: beq
; CHECK: bcs
; CHECK: rts
define i16 @test_select_ule(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp ule i16 %a, %b
  %result = select i1 %cmp, i16 %a, i16 %c
  ret i16 %result
}
