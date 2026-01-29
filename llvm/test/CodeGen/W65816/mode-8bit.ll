; RUN: llc -march=w65816 < %s | FileCheck %s --check-prefix=CHECK-16BIT
; RUN: llc -march=w65816 -mattr=+acc8bit < %s | FileCheck %s --check-prefix=CHECK-ACC8
; RUN: llc -march=w65816 -mattr=+idx8bit < %s | FileCheck %s --check-prefix=CHECK-IDX8

; Test that feature flags work and affect code generation
; Note: The combination of +acc8bit,+idx8bit is not fully supported for i16 values
; as there would be no 16-bit registers available. This is a documented limitation.

target triple = "w65816-unknown-none"

; Test prologue mode setup
; 16-bit mode (default) should NOT emit SEP
; 8-bit accumulator mode (M=1) should emit SEP #$20 (decimal 32)
; 8-bit index mode (X=1) should emit SEP #$10 (decimal 16)

; CHECK-16BIT-LABEL: simple_return:
; CHECK-16BIT-NOT: sep
; CHECK-16BIT: lda #42
; CHECK-16BIT: rts

; In 8-bit accumulator mode, the 16-bit return value uses X register
; since accumulator is only 8-bit
; CHECK-ACC8-LABEL: simple_return:
; CHECK-ACC8: sep #32
; CHECK-ACC8: ldx #42
; CHECK-ACC8: txa
; CHECK-ACC8: rts

; In 8-bit index mode, accumulator is still 16-bit so LDA is used
; CHECK-IDX8-LABEL: simple_return:
; CHECK-IDX8: sep #16
; CHECK-IDX8: lda #42
; CHECK-IDX8: rts
define i16 @simple_return() {
entry:
  ret i16 42
}

; Test that 16-bit mode (default) works as expected
; CHECK-16BIT-LABEL: add_16bit:
; CHECK-16BIT: clc
; CHECK-16BIT: adc
; CHECK-16BIT: rts
define i16 @add_16bit(i16 %a, i16 %b) {
entry:
  %result = add i16 %a, %b
  ret i16 %result
}
