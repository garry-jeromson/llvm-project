// REQUIRES: w65816-registered-target
// RUN: %clang_cc1 -triple w65816-unknown-none -emit-llvm -o - %s | FileCheck %s

// Test that register aliases are recognized for inline assembly.
// Aliases allow alternative names like "acc" for "a".

void test_acc_alias(void) {
  // "acc" should be recognized as alias for accumulator "a"
  // CHECK: call void asm sideeffect "nop", "~{a}"()
  __asm__ volatile("nop" : : : "acc");
}

void test_sp_alias(void) {
  // "sp" should be recognized as alias for stack pointer
  // CHECK: call void asm sideeffect "nop", "~{sp}"()
  __asm__ volatile("nop" : : : "sp");
}

void test_dp_alias(void) {
  // "dp" should be recognized as alias for direct page register "d"
  // CHECK: call void asm sideeffect "nop", "~{d}"()
  __asm__ volatile("nop" : : : "dp");
}
