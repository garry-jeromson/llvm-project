; RUN: not llc -march=w65816 < %s 2>&1 | FileCheck %s

; Test that 32-bit types produce clear error messages instead of silent failures

target triple = "w65816-unknown-none"

; CHECK: error:{{.*}}32-bit and 64-bit return values are not supported on W65816
define i32 @test_i32_return() {
  ret i32 42
}
