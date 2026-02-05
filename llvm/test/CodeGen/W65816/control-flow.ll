; RUN: llc -march=w65816 < %s | FileCheck %s
; Test control flow (calls, returns, conditional branches, loops)

target datalayout = "e-m:e-p:16:16-i16:16-n8:16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; Function Calls
;===----------------------------------------------------------------------===

declare i16 @external_func(i16)

; CHECK-LABEL: call_func:
; CHECK: jsr external_func
; CHECK: rts
define i16 @call_func(i16 %a) {
  %r = call i16 @external_func(i16 %a)
  ret i16 %r
}

; CHECK-LABEL: call_with_two_args:
; CHECK: jsr external_func2
; CHECK: rts
declare i16 @external_func2(i16, i16)
define i16 @call_with_two_args(i16 %a, i16 %b) {
  %r = call i16 @external_func2(i16 %a, i16 %b)
  ret i16 %r
}

; CHECK-LABEL: call_with_three_args:
; CHECK: jsr external_func3
; CHECK: rts
declare i16 @external_func3(i16, i16, i16)
define i16 @call_with_three_args(i16 %a, i16 %b, i16 %c) {
  %r = call i16 @external_func3(i16 %a, i16 %b, i16 %c)
  ret i16 %r
}

; Test 4 arguments - 4th arg goes on stack
; CHECK-LABEL: call_with_four_args:
; CHECK: jsr external_func4
; CHECK: rts
declare i16 @external_func4(i16, i16, i16, i16)
define i16 @call_with_four_args(i16 %a, i16 %b, i16 %c, i16 %d) {
  %r = call i16 @external_func4(i16 %a, i16 %b, i16 %c, i16 %d)
  ret i16 %r
}

; Test 5 arguments - 4th and 5th on stack
; CHECK-LABEL: call_with_five_args:
; CHECK: jsr external_func5
; CHECK: rts
declare i16 @external_func5(i16, i16, i16, i16, i16)
define i16 @call_with_five_args(i16 %a, i16 %b, i16 %c, i16 %d, i16 %e) {
  %r = call i16 @external_func5(i16 %a, i16 %b, i16 %c, i16 %d, i16 %e)
  ret i16 %r
}

;===----------------------------------------------------------------------===
; Return Values
;===----------------------------------------------------------------------===

; CHECK-LABEL: return_const:
; CHECK: lda #42
; CHECK: rts
define i16 @return_const() {
  ret i16 42
}

; CHECK-LABEL: return_zero:
; CHECK: lda #0
; CHECK: rts
define i16 @return_zero() {
  ret i16 0
}

; CHECK-LABEL: return_arg:
; CHECK: rts
define i16 @return_arg(i16 %a) {
  ret i16 %a
}

; CHECK-LABEL: return_max:
; CHECK: lda #65535
; CHECK: rts
define i16 @return_max() {
  ret i16 65535
}

;===----------------------------------------------------------------------===
; Unconditional Branch
;===----------------------------------------------------------------------===

; CHECK-LABEL: infinite_loop:
; CHECK: .L{{.*}}:
; CHECK: bra .L
define void @infinite_loop() {
entry:
  br label %loop
loop:
  br label %loop
}

;===----------------------------------------------------------------------===
; Conditional Branches with icmp
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_cmp_branch:
; CHECK: bne
; CHECK: rts
define i16 @test_cmp_branch(i16 %a, i16 %b) {
entry:
  %cmp = icmp eq i16 %a, %b
  br i1 %cmp, label %then, label %else
then:
  ret i16 1
else:
  ret i16 0
}

;===----------------------------------------------------------------------===
; Simple Loop with Counter
;===----------------------------------------------------------------------===

; CHECK-LABEL: count_loop:
; CHECK: .L{{.*}}:
; CHECK: clc
; CHECK: adc #65535
; CHECK: tax
; CHECK: cpx #1
; CHECK: bne .L
; CHECK: rts
define i16 @count_loop(i16 %n) {
entry:
  br label %loop
loop:
  %val = phi i16 [ %n, %entry ], [ %dec, %loop ]
  %dec = add i16 %val, -1
  %done = icmp eq i16 %dec, 0
  br i1 %done, label %exit, label %loop
exit:
  ret i16 %val
}

;===----------------------------------------------------------------------===
; Unsigned Greater Than (UGT) Branch
; Requires: C=1 AND Z=0
; Generated as: NOT(UGT) branches to else, which uses BEQ + BCC
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_ugt_branch:
; Comparison via SEC + SBC
; CHECK: sec
; CHECK: sbc
; UGT uses compound condition - branch to else if NOT(UGT): Z=1 OR C=0
; CHECK: beq
; CHECK: bcc
; CHECK: rts
define i16 @test_ugt_branch(i16 %a, i16 %b) {
entry:
  %cmp = icmp ugt i16 %a, %b
  br i1 %cmp, label %then, label %else
then:
  ret i16 1
else:
  ret i16 0
}

;===----------------------------------------------------------------------===
; Unsigned Less or Equal (ULE) Branch
; Requires: C=0 OR Z=1
; Generated as: BEQ to then (Z=1), BCS to else (C=1 means A>B)
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_ule_branch:
; Comparison via SEC + SBC
; CHECK: sec
; CHECK: sbc
; ULE: branch to then if equal, branch to else if strictly greater
; CHECK: beq
; CHECK: bcs
; CHECK: rts
define i16 @test_ule_branch(i16 %a, i16 %b) {
entry:
  %cmp = icmp ule i16 %a, %b
  br i1 %cmp, label %then, label %else
then:
  ret i16 1
else:
  ret i16 0
}
