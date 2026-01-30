; RUN: llc -march=w65816 < %s | FileCheck %s

target triple = "w65816-unknown-none"

; CHECK-LABEL: add:
; CHECK: clc
; CHECK: stx $00fe
; CHECK: adc $00fe
; CHECK: rts
define i16 @add(i16 %a, i16 %b) {
entry:
  %result = add i16 %a, %b
  ret i16 %result
}

; CHECK-LABEL: sub:
; CHECK: sec
; CHECK: stx $00fe
; CHECK: sbc $00fe
; CHECK: rts
define i16 @sub(i16 %a, i16 %b) {
entry:
  %result = sub i16 %a, %b
  ret i16 %result
}

; CHECK-LABEL: return_constant:
; CHECK: lda #42
; CHECK: rts
define i16 @return_constant() {
entry:
  ret i16 42
}

; CHECK-LABEL: return_zero:
; CHECK: lda #0
; CHECK: rts
define i16 @return_zero() {
entry:
  ret i16 0
}
