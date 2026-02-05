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
; GISel uses add -1 instead of dec
; CHECK: clc
; CHECK: adc #65535
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
; Note: The following instructions are now defined (as of Jan 2025) but
; don't have automatic selection patterns yet. They can be used via:
; 1. Direct assembly output (when assembler is available)
; 2. Future intrinsics
; 3. Custom selection patterns (TODO)
;
; FULLY IMPLEMENTED (all addressing modes):
; - BIT: immediate, absolute, DP, abs+X, DP+X
; - STZ: absolute, DP, abs+X, DP+X
; - INC/DEC memory: absolute, DP, abs+X, DP+X
; - ASL/LSR/ROL/ROR memory: DP, DP+X, abs, abs+X
; - MVN/MVP: block move instructions
; - JMP indirect: (abs), (abs,X), [abs] (indirect long)
;
; LONG (24-BIT) ADDRESSING - Partial selection patterns:
; - LDA_long/STA_long: for globals in .fardata, .rodata, .romdata sections
; - LDA_longX/STA_longX: defined, no selection patterns yet
; - LDA_dpIndLong/STA_dpIndLong: DP indirect long (3-byte pointer)
; - LDA_dpIndLongY/STA_dpIndLongY: DP indirect long + Y
; - JML/JSL/RTL: already existed for long jumps/calls
;
; 65816-SPECIFIC INSTRUCTIONS:
; - TXY/TYX: inter-register transfers
; - TCD/TDC/TCS/TSC: 16-bit register transfers
; - XBA: exchange B and A bytes (has bswap pattern!)
; - XCE: exchange Carry/Emulation flags
; - PEA/PEI/PER: push effective address
; - TSB/TRB: test and set/reset bits
; - BRK/COP: software interrupts
; - BRL: branch long (16-bit offset)
; - WAI/STP: wait/stop processor
;===----------------------------------------------------------------------===
