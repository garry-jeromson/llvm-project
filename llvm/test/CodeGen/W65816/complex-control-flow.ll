; RUN: llc -march=w65816 < %s | FileCheck %s
; Test complex control flow patterns

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Nested function calls
;===----------------------------------------------------------------------===;

declare i16 @external_func(i16)

; CHECK-LABEL: nested_calls:
; CHECK: jsr external_func
; CHECK: jsr external_func
; CHECK: rts
define i16 @nested_calls(i16 %x) {
  %a = call i16 @external_func(i16 %x)
  %b = call i16 @external_func(i16 %a)
  ret i16 %b
}

;===----------------------------------------------------------------------===;
; Diamond control flow (if-then-else with join)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: diamond_flow:
; CHECK: cmp #0
; CHECK: bne
; CHECK: rts
define i16 @diamond_flow(i16 %cond, i16 %a, i16 %b) {
entry:
  %c = icmp eq i16 %cond, 0
  br i1 %c, label %then, label %else

then:
  %r1 = add i16 %a, 1
  br label %join

else:
  %r2 = add i16 %b, 2
  br label %join

join:
  %result = phi i16 [ %r1, %then ], [ %r2, %else ]
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Loop with counter
;===----------------------------------------------------------------------===;

; CHECK-LABEL: count_loop:
; CHECK: bne
; CHECK: rts
define i16 @count_loop(i16 %n) {
entry:
  br label %loop

loop:
  %i = phi i16 [ 0, %entry ], [ %i.next, %loop ]
  %sum = phi i16 [ 0, %entry ], [ %sum.next, %loop ]
  %sum.next = add i16 %sum, %i
  %i.next = add i16 %i, 1
  %done = icmp eq i16 %i.next, %n
  br i1 %done, label %exit, label %loop

exit:
  ret i16 %sum.next
}

;===----------------------------------------------------------------------===;
; Early return (multiple exit points)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: early_return:
; CHECK: cpx #0
; CHECK: bne
; CHECK: rts
define i16 @early_return(i16 %x) {
entry:
  %is_zero = icmp eq i16 %x, 0
  br i1 %is_zero, label %return_zero, label %compute

return_zero:
  ret i16 0

compute:
  %r = add i16 %x, 1
  ret i16 %r
}

;===----------------------------------------------------------------------===;
; Chained comparisons
;===----------------------------------------------------------------------===;

; CHECK-LABEL: chained_compare:
; CHECK: cpx #10
; CHECK: bcs
; CHECK: cpx #100
; CHECK: bcs
define i16 @chained_compare(i16 %x) {
entry:
  %c1 = icmp ult i16 %x, 10
  br i1 %c1, label %small, label %check_medium

small:
  ret i16 1

check_medium:
  %c2 = icmp ult i16 %x, 100
  br i1 %c2, label %medium, label %large

medium:
  ret i16 2

large:
  ret i16 3
}

;===----------------------------------------------------------------------===;
; Loop with break condition
;===----------------------------------------------------------------------===;

@global_limit = global i16 100

; CHECK-LABEL: loop_with_break:
; CHECK: lda global_limit
; CHECK: beq
; CHECK: rts
define i16 @loop_with_break(i16 %start) {
entry:
  %limit = load i16, ptr @global_limit
  br label %loop

loop:
  %val = phi i16 [ %start, %entry ], [ %next, %continue ]
  %is_limit = icmp eq i16 %val, %limit
  br i1 %is_limit, label %exit, label %continue

continue:
  %next = add i16 %val, 1
  %is_max = icmp eq i16 %next, 1000
  br i1 %is_max, label %exit, label %loop

exit:
  ret i16 %val
}
