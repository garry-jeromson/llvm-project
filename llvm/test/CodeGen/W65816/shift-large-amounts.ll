; RUN: llc -march=w65816 < %s | FileCheck %s

; Test optimizations for large constant shift amounts (>= 8)
; These use XBA-based optimizations to avoid long sequences of single-bit shifts

target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Shift Left (SHL) Optimizations
;===----------------------------------------------------------------------===;

; SHL by 10: XBA + AND + 2 ASLs (6 bytes vs 10 bytes for 10x ASL)
define i16 @shl10(i16 %x) {
; CHECK-LABEL: shl10:
; CHECK:       xba
; CHECK-NEXT:  and #65280
; CHECK-NEXT:  asl a
; CHECK-NEXT:  asl a
; CHECK-NEXT:  rts
entry:
  %r = shl i16 %x, 10
  ret i16 %r
}

; SHL by 15: XBA + AND + 7 ASLs
define i16 @shl15(i16 %x) {
; CHECK-LABEL: shl15:
; CHECK:       xba
; CHECK-NEXT:  and #65280
; CHECK:       asl a
; CHECK:       rts
entry:
  %r = shl i16 %x, 15
  ret i16 %r
}

; SHL by 16 or more: undefined behavior in LLVM IR, result may be optimized away
; The backend handles it but LLVM frontend treats it as undef

;===----------------------------------------------------------------------===;
; Logical Shift Right (LSHR) Optimizations
;===----------------------------------------------------------------------===;

; LSHR by 10: XBA + AND + 2 LSRs
define i16 @lshr10(i16 %x) {
; CHECK-LABEL: lshr10:
; CHECK:       xba
; CHECK-NEXT:  and #255
; CHECK-NEXT:  lsr a
; CHECK-NEXT:  lsr a
; CHECK-NEXT:  rts
entry:
  %r = lshr i16 %x, 10
  ret i16 %r
}

; LSHR by 15: Extracts bit 15 (sign bit) -> 0 or 1
; Uses: ASL (shift bit 15 to carry) + LDA #0 + ROL (carry to bit 0)
define i16 @lshr15(i16 %x) {
; CHECK-LABEL: lshr15:
; CHECK:       asl a
; CHECK-NEXT:  lda #0
; CHECK-NEXT:  rol a
; CHECK-NEXT:  rts
entry:
  %r = lshr i16 %x, 15
  ret i16 %r
}

; LSHR by 16 or more: undefined behavior in LLVM IR, result may be optimized away

;===----------------------------------------------------------------------===;
; Arithmetic Shift Right (ASHR) Optimizations
;===----------------------------------------------------------------------===;

; ASHR by 10: XBA sign-extend + 2 more CMP+ROR pairs
define i16 @ashr10(i16 %x) {
; CHECK-LABEL: ashr10:
; CHECK:       xba
; CHECK:       and #255
; CHECK:       eor #128
; CHECK:       sec
; CHECK:       sbc #128
; CHECK:       cmp #32768
; CHECK:       ror a
; CHECK:       cmp #32768
; CHECK:       ror a
; CHECK:       rts
entry:
  %r = ashr i16 %x, 10
  ret i16 %r
}

; ASHR by 15: Sign extension - produces 0 (positive) or -1 (negative)
; Uses: ASL + LDA #0 + ROL + EOR #1 + DEC
; If original bit 15 was 1 (negative): result = $FFFF
; If original bit 15 was 0 (positive): result = $0000
define i16 @ashr15(i16 %x) {
; CHECK-LABEL: ashr15:
; CHECK:       asl a
; CHECK-NEXT:  lda #0
; CHECK-NEXT:  rol a
; CHECK-NEXT:  eor #1
; CHECK-NEXT:  dec a
; CHECK-NEXT:  rts
entry:
  %r = ashr i16 %x, 15
  ret i16 %r
}

; ASHR by 16 or more: undefined behavior in LLVM IR, result may be optimized away
