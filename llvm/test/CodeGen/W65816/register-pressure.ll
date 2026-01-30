; RUN: llc -march=w65816 < %s | FileCheck %s
; Test register pressure scenarios requiring spills

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Four live values - requires spilling (only A, X, Y available)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: four_values:
; Need to spill at least one value to stack
; CHECK: {{sta|stx|sty}}
; CHECK: rts
define i16 @four_values(i16 %a, i16 %b, i16 %c) {
  %ab = add i16 %a, %b
  %abc = add i16 %ab, %c
  %d = add i16 %a, %c    ; reuse %a and %c, need %ab still live
  %result = add i16 %abc, %d
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Multiple independent computations
;===----------------------------------------------------------------------===;

; CHECK-LABEL: independent_ops:
; CHECK: rts
define i16 @independent_ops(i16 %a, i16 %b, i16 %c) {
  %x = add i16 %a, 1
  %y = add i16 %b, 2
  %z = add i16 %c, 3
  %xy = add i16 %x, %y
  %result = add i16 %xy, %z
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Chain of operations keeping values live
;===----------------------------------------------------------------------===;

; CHECK-LABEL: value_chain:
; CHECK: rts
define i16 @value_chain(i16 %input) {
  %a = add i16 %input, 1
  %b = add i16 %a, 2
  %c = add i16 %b, 3
  %d = add i16 %c, 4
  ; Now use all intermediate values
  %ab = add i16 %a, %b
  %cd = add i16 %c, %d
  %result = add i16 %ab, %cd
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Function call in middle - must preserve values across call
;===----------------------------------------------------------------------===;

declare i16 @external()

; CHECK-LABEL: preserve_across_call:
; Values must be saved before call and restored after
; CHECK: jsr external
; CHECK: rts
define i16 @preserve_across_call(i16 %a, i16 %b) {
  %sum1 = add i16 %a, %b
  %callret = call i16 @external()
  %result = add i16 %sum1, %callret
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Multiple calls with values live across each
;===----------------------------------------------------------------------===;

declare i16 @func1(i16)
declare i16 @func2(i16)

; CHECK-LABEL: multiple_calls:
; CHECK: jsr func1
; CHECK: jsr func2
; CHECK: rts
define i16 @multiple_calls(i16 %x) {
  %a = call i16 @func1(i16 %x)
  %b = call i16 @func2(i16 %a)
  %result = add i16 %a, %b  ; %a must survive the second call
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Spill and reload pattern
;===----------------------------------------------------------------------===;

; CHECK-LABEL: spill_reload:
; Should see stack-relative store and load
; CHECK: sta {{[0-9]+}},s
; CHECK: lda {{[0-9]+}},s
; CHECK: rts
define i16 @spill_reload(i16 %a, i16 %b, i16 %c) {
entry:
  %v1 = add i16 %a, %b
  %v2 = add i16 %b, %c
  %v3 = add i16 %c, %a
  ; Force all three values to be live
  %t1 = add i16 %v1, %v2
  %t2 = add i16 %t1, %v3
  %t3 = add i16 %v1, %v3  ; reuse v1 and v3
  %result = add i16 %t2, %t3
  ret i16 %result
}
