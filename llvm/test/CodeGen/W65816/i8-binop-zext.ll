; RUN: llc -march=w65816 < %s | FileCheck %s
; Test binary operations on two zero-extended 8-bit loads
; This tests the pattern: (binop (zextload i8), (zextload i8))
; which previously caused register pressure issues

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

@byte_a = global i8 5
@byte_b = global i8 3

;===----------------------------------------------------------------------===;
; AND of two 8-bit loads
;===----------------------------------------------------------------------===;

; CHECK-LABEL: and_two_byte_loads:
; GISel loads both values, zero-extends, then AND in 16-bit
; CHECK: sep #32
; CHECK: lda byte_a
; CHECK: rep #32
; CHECK: and #255
; CHECK: sep #32
; CHECK: lda byte_b
; CHECK: rep #32
; CHECK: and #255
; CHECK: and
; CHECK: rts
define i16 @and_two_byte_loads() {
  %a = load i8, ptr @byte_a
  %b = load i8, ptr @byte_b
  %a16 = zext i8 %a to i16
  %b16 = zext i8 %b to i16
  %result = and i16 %a16, %b16
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; OR of two 8-bit loads
;===----------------------------------------------------------------------===;

; CHECK-LABEL: or_two_byte_loads:
; CHECK: sep #32
; CHECK: lda byte_a
; CHECK: rep #32
; CHECK: and #255
; CHECK: sep #32
; CHECK: lda byte_b
; CHECK: rep #32
; CHECK: and #255
; CHECK: ora
; CHECK: rts
define i16 @or_two_byte_loads() {
  %a = load i8, ptr @byte_a
  %b = load i8, ptr @byte_b
  %a16 = zext i8 %a to i16
  %b16 = zext i8 %b to i16
  %result = or i16 %a16, %b16
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; XOR of two 8-bit loads
;===----------------------------------------------------------------------===;

; CHECK-LABEL: xor_two_byte_loads:
; CHECK: sep #32
; CHECK: lda byte_a
; CHECK: rep #32
; CHECK: and #255
; CHECK: sep #32
; CHECK: lda byte_b
; CHECK: rep #32
; CHECK: and #255
; CHECK: eor
; CHECK: rts
define i16 @xor_two_byte_loads() {
  %a = load i8, ptr @byte_a
  %b = load i8, ptr @byte_b
  %a16 = zext i8 %a to i16
  %b16 = zext i8 %b to i16
  %result = xor i16 %a16, %b16
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; ADD of two 8-bit loads
;===----------------------------------------------------------------------===;

; CHECK-LABEL: add_two_byte_loads:
; CHECK: sep #32
; CHECK: lda byte_a
; CHECK: rep #32
; CHECK: and #255
; CHECK: sep #32
; CHECK: lda byte_b
; CHECK: rep #32
; CHECK: and #255
; CHECK: clc
; CHECK: adc
; CHECK: rts
define i16 @add_two_byte_loads() {
  %a = load i8, ptr @byte_a
  %b = load i8, ptr @byte_b
  %a16 = zext i8 %a to i16
  %b16 = zext i8 %b to i16
  %result = add i16 %a16, %b16
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; SUB of two 8-bit loads
;===----------------------------------------------------------------------===;

; CHECK-LABEL: sub_two_byte_loads:
; CHECK: sep #32
; CHECK: lda byte_a
; CHECK: rep #32
; CHECK: and #255
; CHECK: sep #32
; CHECK: lda byte_b
; CHECK: rep #32
; CHECK: and #255
; CHECK: sec
; CHECK: sbc
; CHECK: rts
define i16 @sub_two_byte_loads() {
  %a = load i8, ptr @byte_a
  %b = load i8, ptr @byte_b
  %a16 = zext i8 %a to i16
  %b16 = zext i8 %b to i16
  %result = sub i16 %a16, %b16
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Verify the 16-bit case still uses direct memory operand
;===----------------------------------------------------------------------===;

@word_a = global i16 5
@word_b = global i16 3

; CHECK-LABEL: and_two_word_loads:
; 16-bit case should use lda/ldx then AND via DP scratch
; CHECK: lda word_a
; CHECK: ldx word_b
; CHECK: and
; CHECK: rts
define i16 @and_two_word_loads() {
  %a = load i16, ptr @word_a
  %b = load i16, ptr @word_b
  %result = and i16 %a, %b
  ret i16 %result
}
