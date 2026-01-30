; RUN: llc -march=w65816 < %s | FileCheck %s
; Test logical operations

target datalayout = "e-m:e-p:16:16-i16:16-n8:16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; AND
;===----------------------------------------------------------------------===

; CHECK-LABEL: and_imm:
; CHECK: and #255
; CHECK: rts
define i16 @and_imm(i16 %a) {
  %r = and i16 %a, 255
  ret i16 %r
}

; CHECK-LABEL: and_regs:
; Optimized: uses DP scratch at $00fe instead of stack (saves 4 cycles)
; CHECK: stx $00fe
; CHECK: and $00fe
; CHECK: rts
define i16 @and_regs(i16 %a, i16 %b) {
  %r = and i16 %a, %b
  ret i16 %r
}

;===----------------------------------------------------------------------===
; OR
;===----------------------------------------------------------------------===

; CHECK-LABEL: or_imm:
; CHECK: ora #256
; CHECK: rts
define i16 @or_imm(i16 %a) {
  %r = or i16 %a, 256
  ret i16 %r
}

; CHECK-LABEL: or_regs:
; Optimized: uses DP scratch at $00fe instead of stack (saves 4 cycles)
; CHECK: stx $00fe
; CHECK: ora $00fe
; CHECK: rts
define i16 @or_regs(i16 %a, i16 %b) {
  %r = or i16 %a, %b
  ret i16 %r
}

;===----------------------------------------------------------------------===
; XOR
;===----------------------------------------------------------------------===

; CHECK-LABEL: xor_imm:
; CHECK: eor #
; CHECK: rts
define i16 @xor_imm(i16 %a) {
  %r = xor i16 %a, 65535
  ret i16 %r
}

; CHECK-LABEL: xor_regs:
; Optimized: uses DP scratch at $00fe instead of stack (saves 4 cycles)
; CHECK: stx $00fe
; CHECK: eor $00fe
; CHECK: rts
define i16 @xor_regs(i16 %a, i16 %b) {
  %r = xor i16 %a, %b
  ret i16 %r
}
