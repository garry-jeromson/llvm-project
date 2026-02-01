; RUN: llc -march=w65816 < %s | FileCheck %s
; Test stack frame operations and deep spilling

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Nested function calls requiring stack frame
;===----------------------------------------------------------------------===;

declare i16 @external()

; CHECK-LABEL: nested_call_chain:
; Should have spill/reload between calls (either stack-relative or DP)
; CHECK: jsr external
; CHECK: jsr external
; CHECK: jsr external
; CHECK: rts
define i16 @nested_call_chain(i16 %start) {
  %r1 = call i16 @external()
  %r2 = call i16 @external()
  %r3 = call i16 @external()
  %s1 = add i16 %r1, %r2
  %s2 = add i16 %s1, %r3
  ret i16 %s2
}

;===----------------------------------------------------------------------===;
; Preserve value across function call
;===----------------------------------------------------------------------===;

; CHECK-LABEL: preserve_across_call:
; Should preserve value across call (either via stack or DP)
; CHECK: jsr external
; CHECK: rts
define i16 @preserve_across_call(i16 %a, i16 %b) {
  %sum = add i16 %a, %b
  %r = call i16 @external()
  %result = add i16 %sum, %r
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Multiple preserved values across call
;===----------------------------------------------------------------------===;

; CHECK-LABEL: multiple_preserves:
; Multiple values preserved across call (either via stack or DP)
; CHECK: jsr external
; CHECK: rts
define i16 @multiple_preserves(i16 %a, i16 %b, i16 %c) {
  %sum1 = add i16 %a, %b
  %sum2 = add i16 %b, %c
  %r = call i16 @external()
  %t1 = add i16 %sum1, %r
  %result = add i16 %t1, %sum2
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Deep call chain with accumulating results
;===----------------------------------------------------------------------===;

declare i16 @compute(i16)

; CHECK-LABEL: accumulate_calls:
; CHECK: jsr compute
; CHECK: jsr compute
; CHECK: rts
define i16 @accumulate_calls(i16 %input) {
  %r1 = call i16 @compute(i16 %input)
  %r2 = call i16 @compute(i16 %r1)
  %r3 = call i16 @compute(i16 %r2)
  ret i16 %r3
}

;===----------------------------------------------------------------------===;
; Call with result used in computation
;===----------------------------------------------------------------------===;

; CHECK-LABEL: call_and_compute:
; CHECK: jsr external
; CHECK: clc
; CHECK: adc
; CHECK: rts
define i16 @call_and_compute() {
  %r = call i16 @external()
  %result = add i16 %r, 100
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Simple leaf function (no frame needed)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: leaf_function:
; No pha/pla needed for simple computation
; CHECK-NOT: pha
; CHECK: clc
; CHECK: adc
; CHECK: rts
define i16 @leaf_function(i16 %a, i16 %b) {
  %result = add i16 %a, %b
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Function that just returns argument (pass-through)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: passthrough:
; CHECK: rts
define i16 @passthrough(i16 %x) {
  ret i16 %x
}

