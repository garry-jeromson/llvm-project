; RUN: llc -march=w65816 < %s | FileCheck %s
; Test shift and rotate operations

target datalayout = "e-m:e-p:16:16-i16:16-n8:16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; Shift Left (ASL)
;===----------------------------------------------------------------------===

; CHECK-LABEL: shl_1:
; CHECK: asl a
; CHECK: rts
define i16 @shl_1(i16 %a) {
  %r = shl i16 %a, 1
  ret i16 %r
}

; CHECK-LABEL: shl_2:
; CHECK: asl a
; CHECK: asl a
; CHECK: rts
define i16 @shl_2(i16 %a) {
  %r = shl i16 %a, 2
  ret i16 %r
}

; CHECK-LABEL: shl_4:
; CHECK: asl a
; CHECK: asl a
; CHECK: asl a
; CHECK: asl a
; CHECK: rts
define i16 @shl_4(i16 %a) {
  %r = shl i16 %a, 4
  ret i16 %r
}

;===----------------------------------------------------------------------===
; Logical Shift Right (LSR)
;===----------------------------------------------------------------------===

; CHECK-LABEL: lshr_1:
; CHECK: lsr a
; CHECK: rts
define i16 @lshr_1(i16 %a) {
  %r = lshr i16 %a, 1
  ret i16 %r
}

; CHECK-LABEL: lshr_2:
; CHECK: lsr a
; CHECK: lsr a
; CHECK: rts
define i16 @lshr_2(i16 %a) {
  %r = lshr i16 %a, 2
  ret i16 %r
}

; Test lshr by 15 - extracts sign bit efficiently
; Instead of 15 LSR instructions, uses: ASL A, LDA #0, ROL A
; CHECK-LABEL: lshr_15:
; CHECK: asl a
; CHECK-NEXT: lda #0
; CHECK-NEXT: rol a
; CHECK: rts
define i16 @lshr_15(i16 %a) {
  %r = lshr i16 %a, 15
  ret i16 %r
}

; Test icmp slt with 0 - GISel uses signed compare + select pattern
; CHECK-LABEL: test_slt_zero:
; CHECK: ldx #1
; CHECK: ldy #0
; CHECK: cmp #0
; CHECK: bvs
; CHECK: rts
define i16 @test_slt_zero(i16 %val) {
  %cmp = icmp slt i16 %val, 0
  %result = zext i1 %cmp to i16
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Variable Shifts (uses loop)
;===----------------------------------------------------------------------===

; CHECK-LABEL: shl_var:
; CHECK: cpx #0
; CHECK: beq
; CHECK: asl a
; CHECK: dex
; CHECK: bne
; CHECK: rts
define i16 @shl_var(i16 %a, i16 %n) {
  %r = shl i16 %a, %n
  ret i16 %r
}

; CHECK-LABEL: lshr_var:
; CHECK: cpx #0
; CHECK: beq
; CHECK: lsr a
; CHECK: dex
; CHECK: bne
; CHECK: rts
define i16 @lshr_var(i16 %a, i16 %n) {
  %r = lshr i16 %a, %n
  ret i16 %r
}
