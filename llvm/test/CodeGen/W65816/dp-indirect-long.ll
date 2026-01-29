; RUN: llc -march=w65816 < %s | FileCheck %s
; Test direct page indirect long addressing
; This uses 24-bit pointers stored in direct page locations

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

; A 24-bit pointer stored in direct page (3 bytes)
; In real SNES code, this would point to ROM data in another bank
@far_ptr = global ptr null, section ".zeropage"

;===----------------------------------------------------------------------===
; For now, test that regular pointer operations still work
; DP indirect long patterns require special pointer types
;===----------------------------------------------------------------------===

@zp_var = global i16 0, section ".zeropage"

; Direct page load works
; CHECK-LABEL: test_dp_load:
; CHECK: lda zp_var
; CHECK: rts
define i16 @test_dp_load() {
  %val = load i16, ptr @zp_var
  ret i16 %val
}

; Direct page store works
; CHECK-LABEL: test_dp_store:
; CHECK: sta zp_var
; CHECK: rts
define void @test_dp_store(i16 %val) {
  store i16 %val, ptr @zp_var
  ret void
}

;===----------------------------------------------------------------------===
; STZ to direct page - store zero optimization
;===----------------------------------------------------------------------===

@zp_counter = global i16 0, section ".zeropage"

; CHECK-LABEL: test_stz_dp:
; CHECK: stz zp_counter
; CHECK: rts
define void @test_stz_dp() {
  store i16 0, ptr @zp_counter
  ret void
}

;===----------------------------------------------------------------------===
; Direct page indexed array operations
;===----------------------------------------------------------------------===

@zp_array = global [4 x i16] zeroinitializer, section ".zeropage"

; Direct page indexed STZ
; CHECK-LABEL: test_stz_dp_indexed:
; CHECK: asl a
; CHECK: tax
; CHECK: stz zp_array,x
; CHECK: rts
define void @test_stz_dp_indexed(i16 %idx) {
  %ptr = getelementptr [4 x i16], ptr @zp_array, i16 0, i16 %idx
  store i16 0, ptr %ptr
  ret void
}

; Direct page indexed store (with register shuffling for idx in A, val in X)
; CHECK-LABEL: test_sta_dp_indexed:
; CHECK: asl a
; CHECK: sta zp_array,x
; CHECK: rts
define void @test_sta_dp_indexed(i16 %idx, i16 %val) {
  %ptr = getelementptr [4 x i16], ptr @zp_array, i16 0, i16 %idx
  store i16 %val, ptr %ptr
  ret void
}

; Direct page indexed load
; CHECK-LABEL: test_lda_dp_indexed:
; CHECK: asl a
; CHECK: tax
; CHECK: lda zp_array,x
; CHECK: rts
define i16 @test_lda_dp_indexed(i16 %idx) {
  %ptr = getelementptr [4 x i16], ptr @zp_array, i16 0, i16 %idx
  %val = load i16, ptr %ptr
  ret i16 %val
}

