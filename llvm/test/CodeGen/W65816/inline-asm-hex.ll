; RUN: llc -march=w65816 < %s | FileCheck %s
; Test Motorola-style integer literals ($hex, %binary) in inline assembly

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Test $ hex prefix support
; $FF should be parsed as 255 decimal
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_hex_prefix:
; CHECK: lda #255
define i16 @test_hex_prefix() {
entry:
  %result = call i16 asm sideeffect "lda #$$FF", "=r"()
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Test % binary prefix support
; %11110000 should be parsed as 240 decimal (0xF0)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_binary_prefix:
; CHECK: lda #240
define i16 @test_binary_prefix() {
entry:
  %result = call i16 asm sideeffect "lda #%11110000", "=r"()
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Test 16-bit hex value
; $1234 should be parsed as 4660 decimal
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_hex_16bit:
; CHECK: ldx #4660
define i16 @test_hex_16bit() {
entry:
  %result = call i16 asm sideeffect "ldx #$$1234", "=r"()
  ret i16 %result
}
