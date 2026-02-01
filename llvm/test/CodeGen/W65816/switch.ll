; RUN: llc -march=w65816 < %s | FileCheck %s
; Test switch statement lowering

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Simple switch with few cases (becomes branch chain)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: switch_small:
; Small switches typically lower to comparison chains
; CHECK: cp{{x|y}}
; CHECK: rts
define i16 @switch_small(i16 %x) {
entry:
  switch i16 %x, label %default [
    i16 0, label %case0
    i16 1, label %case1
    i16 2, label %case2
  ]

case0:
  ret i16 100

case1:
  ret i16 200

case2:
  ret i16 300

default:
  ret i16 0
}

;===----------------------------------------------------------------------===;
; Switch with consecutive cases
;===----------------------------------------------------------------------===;

; CHECK-LABEL: switch_consecutive:
; CHECK: cp{{x|y}}
; CHECK: rts
define i16 @switch_consecutive(i16 %x) {
entry:
  switch i16 %x, label %default [
    i16 10, label %case10
    i16 11, label %case11
    i16 12, label %case12
    i16 13, label %case13
  ]

case10:
  ret i16 1

case11:
  ret i16 2

case12:
  ret i16 3

case13:
  ret i16 4

default:
  ret i16 0
}

;===----------------------------------------------------------------------===;
; Switch with sparse cases
;===----------------------------------------------------------------------===;

; CHECK-LABEL: switch_sparse:
; Sparse switches become comparison chains
; CHECK: cp{{x|y}}
; CHECK: rts
define i16 @switch_sparse(i16 %x) {
entry:
  switch i16 %x, label %default [
    i16 0, label %case0
    i16 10, label %case10
    i16 100, label %case100
    i16 1000, label %case1000
  ]

case0:
  ret i16 1

case10:
  ret i16 2

case100:
  ret i16 3

case1000:
  ret i16 4

default:
  ret i16 0
}

;===----------------------------------------------------------------------===;
; Two-case switch (essentially if-else)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: switch_two:
; CHECK: cp{{x|y}}
; CHECK: rts
define i16 @switch_two(i16 %x) {
entry:
  switch i16 %x, label %default [
    i16 0, label %zero
    i16 1, label %one
  ]

zero:
  ret i16 10

one:
  ret i16 20

default:
  ret i16 0
}

;===----------------------------------------------------------------------===;
; Switch with fallthrough patterns
;===----------------------------------------------------------------------===;

; CHECK-LABEL: switch_fallthrough:
; CHECK: cp{{x|y}}
; CHECK: rts
define i16 @switch_fallthrough(i16 %x) {
entry:
  switch i16 %x, label %default [
    i16 1, label %case1
    i16 2, label %case2
    i16 3, label %case3
    i16 4, label %case3  ; same as case 3
    i16 5, label %case3  ; same as case 3
  ]

case1:
  ret i16 100

case2:
  ret i16 200

case3:
  ; Cases 3, 4, 5 all return 300
  ret i16 300

default:
  ret i16 0
}

;===----------------------------------------------------------------------===;
; Switch returning argument value
;===----------------------------------------------------------------------===;

; CHECK-LABEL: switch_return_arg:
; CHECK: rts
define i16 @switch_return_arg(i16 %x) {
entry:
  switch i16 %x, label %default [
    i16 0, label %case0
  ]

case0:
  ret i16 %x

default:
  ret i16 %x
}

;===----------------------------------------------------------------------===;
; Switch with computation in cases
;===----------------------------------------------------------------------===;

; CHECK-LABEL: switch_compute:
; CHECK: cp{{x|y}}
; CHECK: rts
define i16 @switch_compute(i16 %x, i16 %y) {
entry:
  switch i16 %x, label %default [
    i16 0, label %add
    i16 1, label %sub
  ]

add:
  %r1 = add i16 %x, %y
  ret i16 %r1

sub:
  %r2 = sub i16 %y, %x
  ret i16 %r2

default:
  ret i16 %y
}

