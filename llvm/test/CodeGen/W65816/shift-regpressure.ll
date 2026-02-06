; RUN: llc -march=w65816 < %s | FileCheck %s
; Test that shift results can be placed in non-A registers,
; reducing register pressure and unnecessary COPYs.

target datalayout = "e-m:e-p:16:16-i16:16-n8:16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; Value in A preserved across shift that goes to X
;===----------------------------------------------------------------------===

; When A holds a live value (first arg) and a shift result goes to X,
; A should be saved/restored around the shift.
; CHECK-LABEL: shift_preserves_a:
; CHECK: pha
; CHECK: txa
; CHECK: asl a
; CHECK: asl a
; CHECK: tax
; CHECK: pla
; CHECK: rts
define i16 @shift_preserves_a(i16 %a, i16 %b) {
  %shifted = shl i16 %b, 2
  %result = add i16 %a, %shifted
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Multiple shifts with different destinations
;===----------------------------------------------------------------------===

; Two shift results both live - the second shift goes to X while
; the first shift result is saved to stack.
; CHECK-LABEL: multiple_shifts:
; CHECK: asl a
; CHECK: pha
; CHECK: txa
; CHECK: asl a
; CHECK: asl a
; CHECK: tax
; CHECK: pla
; CHECK: rts
define i16 @multiple_shifts(i16 %a, i16 %b) {
  %s1 = shl i16 %a, 1
  %s2 = shl i16 %b, 2
  %result = add i16 %s1, %s2
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Logical shift right to non-A register
;===----------------------------------------------------------------------===

; CHECK-LABEL: lshr_to_nonA:
; CHECK: pha
; CHECK: txa
; CHECK: lsr a
; CHECK: tax
; CHECK: pla
; CHECK: rts
define i16 @lshr_to_nonA(i16 %a, i16 %b) {
  %shifted = lshr i16 %b, 1
  %result = add i16 %a, %shifted
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Arithmetic shift right to non-A register
;===----------------------------------------------------------------------===

; CHECK-LABEL: ashr_to_nonA:
; CHECK: pha
; CHECK: txa
; CHECK: cmp #32768
; CHECK: ror a
; CHECK: tax
; CHECK: pla
; CHECK: rts
define i16 @ashr_to_nonA(i16 %a, i16 %b) {
  %shifted = ashr i16 %b, 1
  %result = add i16 %a, %shifted
  ret i16 %result
}
