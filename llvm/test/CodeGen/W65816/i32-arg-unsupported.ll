; RUN: not --crash llc -march=w65816 < %s 2>&1 | FileCheck %s

; Test that 32-bit arguments produce clear error messages

target triple = "w65816-unknown-none"

; CHECK: error:{{.*}}32-bit and 64-bit integer arguments are not supported on W65816
define i16 @test_i32_arg(i32 %x) {
  %trunc = trunc i32 %x to i16
  ret i16 %trunc
}
