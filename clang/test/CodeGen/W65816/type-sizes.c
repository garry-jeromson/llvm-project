// REQUIRES: w65816-registered-target
// RUN: %clang_cc1 -triple w65816-unknown-none -emit-llvm -o - %s | FileCheck %s

// Test that type sizes are correctly configured for W65816.
// char should be 8-bit, short and int should be 16-bit.

// CHECK: @char_size = global i16 1
int char_size = sizeof(char);

// CHECK: @short_size = global i16 2
int short_size = sizeof(short);

// CHECK: @int_size = global i16 2
int int_size = sizeof(int);

// CHECK: @ptr_size = global i16 2
int ptr_size = sizeof(void *);

// Check alignments are as expected
// CHECK: @char_align = global i16 1
int char_align = _Alignof(char);

// CHECK: @short_align = global i16 2
int short_align = _Alignof(short);

// CHECK: @int_align = global i16 2
int int_align = _Alignof(int);
