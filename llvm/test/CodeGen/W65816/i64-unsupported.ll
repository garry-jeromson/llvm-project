; RUN: not --crash llc -march=w65816 < %s 2>&1 | FileCheck %s

; Test that 64-bit types produce clear error messages

target triple = "w65816-unknown-none"

; CHECK: error:{{.*}}32-bit and 64-bit return values are not supported on W65816
define i64 @test_i64_return() {
  ret i64 42
}
