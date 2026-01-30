; RUN: llc -march=w65816 < %s | FileCheck %s
; Test ROL/ROR rotate-through-carry instructions in inline assembly
;
; Note: W65816's ROL/ROR are "rotate through carry" operations (17-bit rotation
; including the carry flag), not true N-bit rotations. They cannot be directly
; mapped from LLVM's ROTL/ROTR operations. Use inline assembly for rotate-through-carry.

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; ROL (Rotate Left through Carry)
; Shifts bits left, carry goes to bit 0, bit 15 goes to carry
;===----------------------------------------------------------------------===;

; CHECK-LABEL: rol_accumulator:
; CHECK: rol a
define i16 @rol_accumulator() {
entry:
  %result = call i16 asm sideeffect "rol a", "=r"()
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; ROR (Rotate Right through Carry)
; Shifts bits right, carry goes to bit 15, bit 0 goes to carry
;===----------------------------------------------------------------------===;

; CHECK-LABEL: ror_accumulator:
; CHECK: ror a
define i16 @ror_accumulator() {
entry:
  %result = call i16 asm sideeffect "ror a", "=r"()
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Multi-word shift example (32-bit shift left using ASL + ROL)
; This is the primary use case for ROL/ROR - chaining shifts across words
;===----------------------------------------------------------------------===;

; CHECK-LABEL: shift_32bit_left:
; CHECK: asl
; CHECK: rol
define void @shift_32bit_left() {
entry:
  ; Shift a 32-bit value left by 1 using ASL on low word, ROL on high word
  ; The carry from ASL flows into the low bit via ROL
  call void asm sideeffect "asl $$0100\0Arol $$0102", ""()
  ret void
}
