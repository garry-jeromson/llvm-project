; RUN: llc -march=w65816 < %s | FileCheck %s

; Test that shift-by-8 operations use XBA instruction for efficiency
; XBA swaps high and low bytes of the accumulator

target triple = "w65816-unknown-none"

; Test 1: Shift left by 8 uses XBA + AND
; Old codegen: 8x ASL A (8 bytes, 16 cycles)
; New codegen: XBA + AND #$FF00 (4 bytes, 5 cycles)
define i16 @shl8(i16 %x) {
; CHECK-LABEL: shl8:
; CHECK:       xba
; CHECK-NEXT:  and #65280
; CHECK-NEXT:  rts
entry:
  %r = shl i16 %x, 8
  ret i16 %r
}

; Test 2: Logical shift right by 8 uses XBA + AND
; Old codegen: 8x LSR A (8 bytes, 16 cycles)
; New codegen: XBA + AND #$00FF (4 bytes, 5 cycles)
define i16 @lshr8(i16 %x) {
; CHECK-LABEL: lshr8:
; CHECK:       xba
; CHECK-NEXT:  and #255
; CHECK-NEXT:  rts
entry:
  %r = lshr i16 %x, 8
  ret i16 %r
}

; Test 3: Arithmetic shift right by 8 uses XBA + sign extension
; Old codegen: 8x (CMP #$8000 + ROR A) (32 bytes)
; New codegen: XBA + AND + EOR + SEC + SBC (11 bytes)
define i16 @ashr8(i16 %x) {
; CHECK-LABEL: ashr8:
; CHECK:       xba
; CHECK:       and #255
; CHECK:       eor #128
; CHECK:       sec
; CHECK:       sbc #128
; CHECK:       rts
entry:
  %r = ashr i16 %x, 8
  ret i16 %r
}

; Test 4: Shift by other amounts still uses ASL/LSR
define i16 @shl4(i16 %x) {
; CHECK-LABEL: shl4:
; CHECK:       asl a
; CHECK:       asl a
; CHECK:       asl a
; CHECK:       asl a
; CHECK-NEXT:  rts
entry:
  %r = shl i16 %x, 4
  ret i16 %r
}

define i16 @lshr4(i16 %x) {
; CHECK-LABEL: lshr4:
; CHECK:       lsr a
; CHECK:       lsr a
; CHECK:       lsr a
; CHECK:       lsr a
; CHECK-NEXT:  rts
entry:
  %r = lshr i16 %x, 4
  ret i16 %r
}
