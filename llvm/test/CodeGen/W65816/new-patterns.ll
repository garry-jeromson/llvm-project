; RUN: llc -march=w65816 < %s | FileCheck %s
; Test selection patterns for 65816-specific instructions

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; XBA - Exchange B and A (byte swap within 16-bit accumulator)
; This should match the bswap intrinsic for i16
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_bswap:
; CHECK: xba
; CHECK: rts
define i16 @test_bswap(i16 %a) {
  %result = call i16 @llvm.bswap.i16(i16 %a)
  ret i16 %result
}

declare i16 @llvm.bswap.i16(i16)

;===----------------------------------------------------------------------===
; Note: ROL/ROR on 65816 rotate through the carry flag, not within the
; register. True bit rotation requires saving MSB/LSB first, so we don't
; have simple patterns for LLVM's rotl/rotr operations.
;===----------------------------------------------------------------------===

;===----------------------------------------------------------------------===
; STZ - Store Zero to Memory
; This should match stores of constant 0 to global variables
;===----------------------------------------------------------------------===

@global_var = global i16 0
@global_array = global [10 x i16] zeroinitializer

; CHECK-LABEL: test_stz_global:
; CHECK: stz global_var
; CHECK: rts
define void @test_stz_global() {
  store i16 0, ptr @global_var
  ret void
}

;===----------------------------------------------------------------------===
; Bswap with storage (prevents optimization away)
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_bswap_store:
; CHECK: xba
; CHECK: sta global_var
; CHECK: rts
define void @test_bswap_store(i16 %a) {
  %swap = call i16 @llvm.bswap.i16(i16 %a)
  store i16 %swap, ptr @global_var
  ret void
}

;===----------------------------------------------------------------------===
; Bswap with arithmetic
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_bswap_add:
; CHECK: xba
; CHECK: clc
; CHECK: adc
; CHECK: rts
define i16 @test_bswap_add(i16 %a, i16 %b) {
  %swap = call i16 @llvm.bswap.i16(i16 %a)
  %result = add i16 %swap, %b
  ret i16 %result
}

;===----------------------------------------------------------------------===
; Memory INC/DEC - increment/decrement memory location
; TODO: This could be optimized to use INC_abs/DEC_abs directly via peephole
; Currently generates load-modify-store sequence
;===----------------------------------------------------------------------===

@counter = global i16 0

; CHECK-LABEL: test_inc_memory:
; CHECK: lda global_var
; CHECK: inc a
; CHECK: sta global_var
; CHECK: rts
define void @test_inc_memory() {
  %val = load i16, ptr @global_var
  %new = add i16 %val, 1
  store i16 %new, ptr @global_var
  ret void
}

; CHECK-LABEL: test_dec_memory:
; CHECK: lda global_var
; CHECK: dec a
; CHECK: sta global_var
; CHECK: rts
define void @test_dec_memory() {
  %val = load i16, ptr @global_var
  %new = add i16 %val, -1
  store i16 %new, ptr @global_var
  ret void
}

;===----------------------------------------------------------------------===
; STZ indexed - store zero to array elements
; Now optimized to use STZ addr,x
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_stz_indexed:
; CHECK: asl a
; CHECK: tax
; CHECK: stz global_array,x
; CHECK: rts
define void @test_stz_indexed(i16 %idx) {
  %ptr = getelementptr [10 x i16], ptr @global_array, i16 0, i16 %idx
  store i16 0, ptr %ptr
  ret void
}

