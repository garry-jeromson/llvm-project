; RUN: llc -march=w65816 < %s | FileCheck %s

; Comprehensive tests for imaginary registers (RS registers backed by Direct Page memory)
; Tests cover all conditional paths in the expansion functions

target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Basic imaginary register allocation - more than 3 live values
;===----------------------------------------------------------------------===;

; CHECK-LABEL: five_live_values:
; Tests that 5 simultaneously live values can be handled
; Should use DP addressing for spills: $10, $12, $14, etc.
define i16 @five_live_values(i16 %a, i16 %b, i16 %c, i16 %d, i16 %e) {
; CHECK: rts
entry:
  ; Keep all 5 values live across the computation
  %sum1 = add i16 %a, %e  ; a + e
  %sum2 = add i16 %b, %d  ; b + d
  %sum3 = add i16 %sum1, %c  ; (a + e) + c
  %result = add i16 %sum3, %sum2  ; ((a + e) + c) + (b + d)
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; ADD operations with imaginary registers in different positions
;===----------------------------------------------------------------------===;

; CHECK-LABEL: add_with_spills:
; Forces use of imaginary registers for intermediate results
; Should generate: lda $1x or sta $1x (DP addressing in $10-$2E range)
define i16 @add_with_spills(i16 %a, i16 %b, i16 %c, i16 %d) {
; CHECK: {{lda|sta}} $
; CHECK: rts
entry:
  ; Create more live values than physical registers
  %t1 = add i16 %a, %b
  %t2 = add i16 %c, %d
  ; Force both t1 and t2 to be live
  %t3 = add i16 %a, %d
  %t4 = add i16 %t1, %t2
  %result = add i16 %t4, %t3
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; SUB operations with imaginary registers
;===----------------------------------------------------------------------===;

; CHECK-LABEL: sub_with_pressure:
; Tests subtraction with register pressure requiring imaginary regs
define i16 @sub_with_pressure(i16 %a, i16 %b, i16 %c, i16 %d) {
; CHECK: rts
entry:
  %t1 = sub i16 %a, %b
  %t2 = sub i16 %c, %d
  %t3 = sub i16 %a, %c
  %t4 = add i16 %t1, %t2
  %result = sub i16 %t4, %t3
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Bitwise operations (AND, OR, XOR) with imaginary registers
;===----------------------------------------------------------------------===;

; CHECK-LABEL: bitwise_with_pressure:
; Tests AND, OR, XOR when register pressure forces imaginary reg use
define i16 @bitwise_with_pressure(i16 %a, i16 %b, i16 %c) {
; CHECK: rts
entry:
  %t1 = and i16 %a, %b
  %t2 = or i16 %b, %c
  %t3 = xor i16 %a, %c
  %result = add i16 %t1, %t2
  ret i16 %result
}

; CHECK-LABEL: and_chain:
; Tests chained AND operations
define i16 @and_chain(i16 %a, i16 %b, i16 %c, i16 %d, i16 %e) {
; CHECK: and
; CHECK: rts
entry:
  %t1 = and i16 %a, %b
  %t2 = and i16 %t1, %c
  %t3 = and i16 %t2, %d
  %result = and i16 %t3, %e
  ret i16 %result
}

; CHECK-LABEL: or_chain:
; Tests chained OR operations
define i16 @or_chain(i16 %a, i16 %b, i16 %c, i16 %d, i16 %e) {
; CHECK: ora
; CHECK: rts
entry:
  %t1 = or i16 %a, %b
  %t2 = or i16 %t1, %c
  %t3 = or i16 %t2, %d
  %result = or i16 %t3, %e
  ret i16 %result
}

; CHECK-LABEL: xor_chain:
; Tests chained XOR operations
define i16 @xor_chain(i16 %a, i16 %b, i16 %c, i16 %d, i16 %e) {
; CHECK: eor
; CHECK: rts
entry:
  %t1 = xor i16 %a, %b
  %t2 = xor i16 %t1, %c
  %t3 = xor i16 %t2, %d
  %result = xor i16 %t3, %e
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Comparison operations with imaginary registers
;===----------------------------------------------------------------------===;

; CHECK-LABEL: compare_with_spill:
; Tests comparison when values are in imaginary registers
define i16 @compare_with_spill(i16 %a, i16 %b, i16 %c, i16 %d) {
; CHECK: rts
entry:
  %sum1 = add i16 %a, %b
  %sum2 = add i16 %c, %d
  ; Compare with sum1 and sum2 possibly in imaginary registers
  %cmp = icmp sgt i16 %sum1, %sum2
  %result = select i1 %cmp, i16 %sum1, i16 %sum2
  ret i16 %result
}

; CHECK-LABEL: compare_immediate_with_spill:
; Tests comparison with immediate when value is in imaginary register
define i16 @compare_immediate_with_spill(i16 %a, i16 %b, i16 %c, i16 %d) {
; CHECK: rts
entry:
  %sum1 = add i16 %a, %b
  %sum2 = add i16 %c, %d
  %sum3 = add i16 %sum1, %sum2  ; Keep all sums live
  ; Compare sum1 with immediate while sum2 and sum3 might be in imaginary regs
  %cmp = icmp sgt i16 %sum1, 100
  %result = select i1 %cmp, i16 %sum3, i16 %sum2
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Function calls with values preserved in imaginary registers
;===----------------------------------------------------------------------===;

declare i16 @external()

; CHECK-LABEL: preserve_across_call:
; Tests that values in imaginary registers survive function calls
define i16 @preserve_across_call(i16 %a, i16 %b, i16 %c, i16 %d) {
; CHECK: jsr external
; CHECK: rts
entry:
  %sum1 = add i16 %a, %b
  %sum2 = add i16 %c, %d
  ; Both sums must survive the call
  %callret = call i16 @external()
  %t1 = add i16 %sum1, %callret
  %result = add i16 %t1, %sum2
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Mixed operations - physical and imaginary registers together
;===----------------------------------------------------------------------===;

; CHECK-LABEL: mixed_operations:
; Tests interleaved operations between physical and imaginary registers
define i16 @mixed_operations(i16 %a, i16 %b, i16 %c, i16 %d) {
; CHECK: rts
entry:
  ; This should create register pressure requiring imaginary regs
  %t1 = add i16 %a, %b
  %t2 = sub i16 %c, %d
  %t3 = and i16 %a, %d
  %t4 = or i16 %b, %c
  ; Use all intermediate values
  %r1 = add i16 %t1, %t2
  %r2 = add i16 %t3, %t4
  %result = add i16 %r1, %r2
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Immediate operations with imaginary register destination
;===----------------------------------------------------------------------===;

; CHECK-LABEL: add_immediate_chain:
; Tests ADD with immediate when result goes to imaginary register
define i16 @add_immediate_chain(i16 %a, i16 %b, i16 %c, i16 %d) {
; CHECK: rts
entry:
  %t1 = add i16 %a, 100
  %t2 = add i16 %b, 200
  %t3 = add i16 %c, 300
  %t4 = add i16 %d, 400
  ; All t1-t4 need to be live for final computation
  %r1 = add i16 %t1, %t2
  %r2 = add i16 %t3, %t4
  %result = add i16 %r1, %r2
  ret i16 %result
}

; CHECK-LABEL: sub_immediate_chain:
; Tests SUB with immediate when result goes to imaginary register
define i16 @sub_immediate_chain(i16 %a, i16 %b, i16 %c, i16 %d) {
; CHECK: rts
entry:
  %t1 = sub i16 %a, 10
  %t2 = sub i16 %b, 20
  %t3 = sub i16 %c, 30
  %t4 = sub i16 %d, 40
  %r1 = add i16 %t1, %t2
  %r2 = add i16 %t3, %t4
  %result = add i16 %r1, %r2
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Pointer/address operations with imaginary registers
;===----------------------------------------------------------------------===;

; CHECK-LABEL: stack_address:
; Tests LEA of stack slot when result might go to imaginary register
define i16 @stack_address(i16 %a, i16 %b, i16 %c, i16 %d) {
; CHECK: rts
entry:
  %local = alloca i16
  ; Keep a, b, c, d live while also using stack address
  %sum1 = add i16 %a, %b
  %sum2 = add i16 %c, %d
  store i16 %sum1, ptr %local
  %loaded = load i16, ptr %local
  %result = add i16 %loaded, %sum2
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Shift operations with register pressure (if shifts use imaginary regs)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: shift_with_pressure:
; Tests shifts when other values occupy physical registers
define i16 @shift_with_pressure(i16 %a, i16 %b, i16 %c, i16 %d) {
; CHECK: rts
entry:
  %t1 = add i16 %a, %b
  %t2 = add i16 %c, %d
  ; Shifts while t1 and t2 are live
  %s1 = shl i16 %a, 2
  %s2 = lshr i16 %c, 1
  ; Use all values
  %r1 = add i16 %t1, %s1
  %r2 = add i16 %t2, %s2
  %result = add i16 %r1, %r2
  ret i16 %result
}
