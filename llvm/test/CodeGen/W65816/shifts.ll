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

;===----------------------------------------------------------------------===
; Variable Shifts (uses loop)
;===----------------------------------------------------------------------===

; CHECK-LABEL: shl_var:
; CHECK: asl a
; CHECK: dex
; CHECK: bne
; CHECK: rts
define i16 @shl_var(i16 %a, i16 %n) {
  %r = shl i16 %a, %n
  ret i16 %r
}

; CHECK-LABEL: lshr_var:
; CHECK: lsr a
; CHECK: dex
; CHECK: bne
; CHECK: rts
define i16 @lshr_var(i16 %a, i16 %n) {
  %r = lshr i16 %a, %n
  ret i16 %r
}
