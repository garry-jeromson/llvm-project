; RUN: llc -march=w65816 < %s | FileCheck %s
; Test comparison register pressure: unsigned CMP preserves A, signed SEC+SBC destroys it

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; Unsigned comparison preserves A (non-destructive CMP, no SEC+SBC)
;===----------------------------------------------------------------------===

; CHECK-LABEL: unsigned_cmp_preserves_a:
; Unsigned compare uses CMP which doesn't destroy A
; CHECK: cmp
; CHECK-NOT: sec
; CHECK-NOT: sbc
; CHECK: bcs
; CHECK: rts
define i16 @unsigned_cmp_preserves_a(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp ult i16 %a, %b
  %sel = select i1 %cmp, i16 %a, i16 %c
  ret i16 %sel
}

;===----------------------------------------------------------------------===
; Signed comparison requires SEC+SBC (destructive, needed for V flag)
;===----------------------------------------------------------------------===

; CHECK-LABEL: signed_cmp_uses_sbc:
; Signed compare needs SEC+SBC to set V flag for overflow detection
; CHECK: sec
; CHECK: sbc
; CHECK: bvs
; CHECK: rts
define i16 @signed_cmp_uses_sbc(i16 %a, i16 %b, i16 %c) {
  %cmp = icmp slt i16 %a, %b
  %sel = select i1 %cmp, i16 %a, i16 %c
  ret i16 %sel
}

;===----------------------------------------------------------------------===
; Multiple unsigned comparisons without spills
; A survives both CMP instructions (non-destructive)
;===----------------------------------------------------------------------===

; CHECK-LABEL: multiple_unsigned_cmp:
; First unsigned comparison: A preserved
; CHECK: cmp #10
; CHECK: bcs .L
; Return 1 for small case
; CHECK: lda #1
; CHECK: rts
; Second unsigned comparison: A still valid, no reload needed
; CHECK: cmp #100
; CHECK: bcs .L
; Return 2 for medium case
; CHECK: lda #2
; CHECK: rts
; Return 3 for large case
; CHECK: lda #3
; CHECK: rts
define i16 @multiple_unsigned_cmp(i16 %x) {
entry:
  %cmp1 = icmp ult i16 %x, 10
  br i1 %cmp1, label %small, label %check
small:
  ret i16 1
check:
  %cmp2 = icmp ult i16 %x, 100
  br i1 %cmp2, label %medium, label %large
medium:
  ret i16 2
large:
  ret i16 3
}
