; RUN: llc -march=w65816 < %s | FileCheck %s
; Test i8 comparison operations (promoted to i16)
; Tests i8 comparisons against constants (which work without register pressure issues)

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; i8 signed comparison with branch
; The i8 value gets sign-extended to i16 before comparison
;===----------------------------------------------------------------------===;

; CHECK-LABEL: i8_sgt_branch:
; Sign extension from i8 to i16 uses XBA-based shift-by-8, then compare with 0
; CHECK: xba
; CHECK: cmp
; CHECK: rts
define i16 @i8_sgt_branch(i8 %v) {
entry:
  ; icmp sgt i8 %v, -1 is equivalent to checking if %v >= 0 (non-negative)
  %cmp = icmp sgt i8 %v, -1
  br i1 %cmp, label %then, label %else
then:
  ret i16 1
else:
  ret i16 0
}

;===----------------------------------------------------------------------===;
; i8 unsigned comparison against constant
;===----------------------------------------------------------------------===;

; CHECK-LABEL: i8_ugt_const:
; Zero extension uses AND with 0xFF, then compare
; CHECK: and
; CHECK: cmp
; CHECK: rts
define i16 @i8_ugt_const(i8 %v) {
entry:
  %cmp = icmp ugt i8 %v, 100
  br i1 %cmp, label %then, label %else
then:
  ret i16 1
else:
  ret i16 0
}

;===----------------------------------------------------------------------===;
; i8 equality comparison against constant
;===----------------------------------------------------------------------===;

; CHECK-LABEL: i8_eq_const:
; CHECK: and
; CHECK: cmp
; CHECK: rts
define i16 @i8_eq_const(i8 %v) {
entry:
  %cmp = icmp eq i8 %v, 42
  br i1 %cmp, label %then, label %else
then:
  ret i16 1
else:
  ret i16 0
}
