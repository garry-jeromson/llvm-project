; RUN: not --crash llc -march=w65816 < %s 2>&1 | FileCheck %s
; CHECK: LLVM ERROR: W65816 stack frame too large: {{[0-9]+}} bytes requested (max 65535 bytes)

; Test that stack frames exceeding 65535 bytes trigger an error.
; The W65816 has a 16-bit stack pointer, so frames cannot exceed 64KB.

define void @huge_stack() {
  %arr = alloca [70000 x i8]
  %ptr = getelementptr [70000 x i8], ptr %arr, i32 0, i32 0
  store i8 1, ptr %ptr
  ret void
}
