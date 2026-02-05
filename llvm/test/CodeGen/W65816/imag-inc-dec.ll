; RUN: llc -march=w65816 -O0 < %s | FileCheck %s

; Test INC16/DEC16 with imaginary registers
; This test creates enough phi nodes to exhaust physical registers (A, X, Y)
; and force the register allocator to use imaginary registers for some values.

define i16 @test_inc_with_phi() {
; CHECK-LABEL: test_inc_with_phi:
entry:
  br label %loop

loop:
  ; These phi nodes with 4 variables will exhaust A, X, Y and require
  ; at least one imaginary register
  %a = phi i16 [ 1, %entry ], [ %next_a, %loop ]
  %b = phi i16 [ 2, %entry ], [ %next_b, %loop ]
  %c = phi i16 [ 3, %entry ], [ %next_c, %loop ]
  %d = phi i16 [ 4, %entry ], [ %next_d, %loop ]

  ; Adding 1 may generate INC16 operations
  %next_a = add i16 %a, 1
  %next_b = add i16 %b, 1
  %next_c = add i16 %c, 1
  %next_d = add i16 %d, 1

  %sum = add i16 %a, %b
  %sum2 = add i16 %sum, %c
  %result = add i16 %sum2, %d
  %done = icmp ugt i16 %result, 50
  br i1 %done, label %exit, label %loop

exit:
  ret i16 %result
}

; GISel uses clc/adc pattern instead of inc:
; CHECK: clc
; CHECK: adc
; CHECK: tax

define i16 @test_dec_with_phi() {
; CHECK-LABEL: test_dec_with_phi:
entry:
  br label %loop

loop:
  ; Create pressure to force imaginary register usage
  %a = phi i16 [ 100, %entry ], [ %next_a, %loop ]
  %b = phi i16 [ 90, %entry ], [ %next_b, %loop ]
  %c = phi i16 [ 80, %entry ], [ %next_c, %loop ]
  %d = phi i16 [ 70, %entry ], [ %next_d, %loop ]

  ; Subtracting 1 may generate DEC16 operations
  %next_a = sub i16 %a, 1
  %next_b = sub i16 %b, 1
  %next_c = sub i16 %c, 1
  %next_d = sub i16 %d, 1

  %sum = add i16 %a, %b
  %sum2 = add i16 %sum, %c
  %result = add i16 %sum2, %d
  %done = icmp ult i16 %result, 20
  br i1 %done, label %exit, label %loop

exit:
  ret i16 %result
}

; GISel uses clc/adc with negative value instead of dec:
; CHECK: clc
; CHECK: adc
; CHECK: tax
