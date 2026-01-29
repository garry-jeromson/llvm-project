; RUN: llc -march=w65816 < %s | FileCheck %s
; Test W65816 instruction selection patterns

target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; INC/DEC - These have selection patterns for accumulator mode
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_inc_reg:
; CHECK: inc a
; CHECK: rts
define i16 @test_inc_reg(i16 %a) {
  %result = add i16 %a, 1
  ret i16 %result
}

; CHECK-LABEL: test_dec_reg:
; CHECK: dec a
; CHECK: rts
define i16 @test_dec_reg(i16 %a) {
  %result = add i16 %a, -1
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Shift operations - ASL/LSR have patterns
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_shl_1:
; CHECK: asl a
; CHECK: rts
define i16 @test_shl_1(i16 %a) {
  %result = shl i16 %a, 1
  ret i16 %result
}

; CHECK-LABEL: test_shl_2:
; CHECK: asl a
; CHECK: asl a
; CHECK: rts
define i16 @test_shl_2(i16 %a) {
  %result = shl i16 %a, 2
  ret i16 %result
}

; CHECK-LABEL: test_lshr_1:
; CHECK: lsr a
; CHECK: rts
define i16 @test_lshr_1(i16 %a) {
  %result = lshr i16 %a, 1
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Logical operations - AND/ORA/EOR have patterns
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_and_imm:
; CHECK: and #255
; CHECK: rts
define i16 @test_and_imm(i16 %a) {
  %result = and i16 %a, 255
  ret i16 %result
}

; CHECK-LABEL: test_or_imm:
; CHECK: ora #256
; CHECK: rts
define i16 @test_or_imm(i16 %a) {
  %result = or i16 %a, 256
  ret i16 %result
}

; CHECK-LABEL: test_xor_imm:
; CHECK: eor #
; CHECK: rts
define i16 @test_xor_imm(i16 %a) {
  %result = xor i16 %a, 65535
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Note: The following instructions are defined but don't have automatic
; selection patterns. They require an assembly parser for inline asm testing:
;
; BIT - Test memory bits (immediate, absolute, dp, indexed modes)
; STZ - Store zero to memory (absolute, dp, indexed modes)
; INC/DEC memory modes (absolute, dp, indexed)
; MVN/MVP - Block move instructions
; JMP indirect modes (absolute indirect, indexed indirect, indirect long)
;
; These instructions can be used via:
; 1. Direct assembly output (when assembler is available)
; 2. Future intrinsics
; 3. Custom selection patterns (TODO)
;===----------------------------------------------------------------------===
