; RUN: not --crash llc -march=w65816 < %s 2>&1 | FileCheck %s
; CHECK: LLVM ERROR: direct page frame exceeds 256-byte limit (offset {{[0-9]+}} out of range)

; Test that DP frame exceeding 256 bytes triggers an error.
; An array of 200 x i16 = 400 bytes, which exceeds DP limit.

define void @dp_overflow() #0 {
  %arr = alloca [200 x i16]
  %ptr = getelementptr [200 x i16], ptr %arr, i32 0, i32 0
  store i16 1, ptr %ptr
  ret void
}

attributes #0 = { "w65816_dpframe" }
