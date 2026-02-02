// REQUIRES: w65816-registered-target
// RUN: %clang_cc1 -triple w65816-unknown-none -emit-llvm -o - %s | FileCheck %s

// Test that W65816-specific inline assembly constraints are recognized.

int data;

void test_acc_constraint(void) {
  // CHECK: call void asm sideeffect "nop", "~{a}"()
  __asm__ volatile("nop" : : : "a");
}

void test_x_constraint(void) {
  // CHECK: call void asm sideeffect "nop", "~{x}"()
  __asm__ volatile("nop" : : : "x");
}

void test_y_constraint(void) {
  // CHECK: call void asm sideeffect "nop", "~{y}"()
  __asm__ volatile("nop" : : : "y");
}

void test_d_constraint(void) {
  // CHECK: call void asm sideeffect "nop", "~{d}"()
  __asm__ volatile("nop" : : : "d");
}

void test_sp_constraint(void) {
  // CHECK: call void asm sideeffect "nop", "~{sp}"()
  __asm__ volatile("nop" : : : "sp");
}

// Test using constraint letters as input constraints
void test_a_input(void) {
  // CHECK: call void asm sideeffect "sta $$00", "a"(i16 %0)
  __asm__ volatile("sta $00" :: "a"(data));
}

void test_x_input(void) {
  // CHECK: call void asm sideeffect "stx $$00", "x"(i16 %0)
  __asm__ volatile("stx $00" :: "x"(data));
}

void test_y_input(void) {
  // CHECK: call void asm sideeffect "sty $$00", "y"(i16 %0)
  __asm__ volatile("sty $00" :: "y"(data));
}
