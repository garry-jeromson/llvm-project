; RUN: llc -march=w65816 < %s | FileCheck %s
; RUN: llc -march=w65816 -mattr=+assume-d0 < %s | FileCheck %s --check-prefix=ASSUMED0

; Test Direct Page frame allocation for local variables.
; Functions with the "w65816_dpframe" attribute use the 256-byte direct page
; region ($00-$FF when D=0) instead of stack-relative addressing, providing
; faster 1-cycle access.

; Basic DP frame test - prologue should save D, set D=0, epilogue restores D
; CHECK-LABEL: dp_locals:
; CHECK: phd
; CHECK: lda #0
; CHECK: tcd
; CHECK: lda #42
; CHECK: sta ${{[0-9a-f]+}}
; CHECK: pld
; CHECK: rts

; With assume-d0, skip PHD/PLD since D is guaranteed to be 0
; ASSUMED0-LABEL: dp_locals:
; ASSUMED0-NOT: phd
; ASSUMED0-NOT: tcd
; ASSUMED0: lda #42
; ASSUMED0: sta ${{[0-9a-f]+}}
; ASSUMED0-NOT: pld
; ASSUMED0: rts
define i16 @dp_locals() #0 {
  %a = alloca i16
  %b = alloca i16
  store i16 42, ptr %a
  %v = load i16, ptr %a
  store i16 %v, ptr %b
  ret i16 %v
}

; Test that non-DP functions still use stack-relative addressing
; CHECK-LABEL: normal_stack_function:
; CHECK-NOT: phd
; CHECK-NOT: tcd
; CHECK: sta {{[0-9]+}},s
; CHECK: rts
define i16 @normal_stack_function() {
  %a = alloca i16
  store i16 123, ptr %a
  %v = load i16, ptr %a
  ret i16 %v
}

; Test DP frame with return value - return value should not be clobbered
; CHECK-LABEL: dp_return_value:
; CHECK: phd
; CHECK: lda #0
; CHECK: tcd
; CHECK: pld
; CHECK: rts
define i16 @dp_return_value(i16 %x) #0 {
  %a = alloca i16
  store i16 %x, ptr %a
  %v = load i16, ptr %a
  %result = add i16 %v, 1
  ret i16 %result
}

attributes #0 = { "w65816_dpframe" }
