; RUN: llc -march=w65816 < %s | FileCheck %s
; Test various W65816 addressing modes

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

@array = global [16 x i16] zeroinitializer
@zp_array = global [4 x i16] zeroinitializer, section ".zeropage"
@global_var = global i16 0

;===----------------------------------------------------------------------===;
; Absolute addressing (LDA abs, STA abs)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: load_absolute:
; CHECK: lda global_var
; CHECK: rts
define i16 @load_absolute() {
  %val = load i16, ptr @global_var
  ret i16 %val
}

; CHECK-LABEL: store_absolute:
; CHECK: sta global_var
; CHECK: rts
define void @store_absolute(i16 %val) {
  store i16 %val, ptr @global_var
  ret void
}

;===----------------------------------------------------------------------===;
; Absolute indexed with X (LDA abs,x)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: load_indexed_x:
; Index calculation: multiply by 2 (asl), put in X
; CHECK: asl a
; CHECK: tax
; CHECK: lda array,x
; CHECK: rts
define i16 @load_indexed_x(i16 %idx) {
  %ptr = getelementptr [16 x i16], ptr @array, i16 0, i16 %idx
  %val = load i16, ptr %ptr
  ret i16 %val
}

; CHECK-LABEL: store_indexed_x:
; Multiply by 2, move to X, store with indexed addressing
; CHECK: asl a
; CHECK: sta array,x
; CHECK: rts
define void @store_indexed_x(i16 %idx, i16 %val) {
  ; Scale idx by 2 for i16 access
  %scaled = shl i16 %idx, 1
  %ptr = getelementptr i8, ptr @array, i16 %scaled
  store i16 %val, ptr %ptr
  ret void
}

;===----------------------------------------------------------------------===;
; Constant offset folded into address
;===----------------------------------------------------------------------===;

; CHECK-LABEL: load_const_offset:
; Constant offset (2*3=6) folded into address
; CHECK: lda array+6
; CHECK: rts
define i16 @load_const_offset() {
  %ptr = getelementptr [16 x i16], ptr @array, i16 0, i16 3
  %val = load i16, ptr %ptr
  ret i16 %val
}

; CHECK-LABEL: store_const_offset:
; CHECK: sta array+8
; CHECK: rts
define void @store_const_offset(i16 %val) {
  %ptr = getelementptr [16 x i16], ptr @array, i16 0, i16 4
  store i16 %val, ptr %ptr
  ret void
}

;===----------------------------------------------------------------------===;
; Direct page addressing (LDA dp, STA dp)
; Shorter and faster than absolute addressing
;===----------------------------------------------------------------------===;

; CHECK-LABEL: load_zeropage:
; CHECK: lda zp_array
; CHECK: rts
define i16 @load_zeropage() {
  %ptr = getelementptr [4 x i16], ptr @zp_array, i16 0, i16 0
  %val = load i16, ptr %ptr
  ret i16 %val
}

;===----------------------------------------------------------------------===;
; Immediate addressing (LDA #imm)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: load_immediate_small:
; CHECK: lda #42
; CHECK: rts
define i16 @load_immediate_small() {
  ret i16 42
}

; CHECK-LABEL: load_immediate_large:
; CHECK: lda #1234
; CHECK: rts
define i16 @load_immediate_large() {
  ret i16 1234
}

; CHECK-LABEL: load_immediate_max:
; -1 in two's complement = 65535
; CHECK: lda #65535
; CHECK: rts
define i16 @load_immediate_max() {
  ret i16 65535
}

; CHECK-LABEL: load_immediate_neg:
; CHECK: lda #65436
; CHECK: rts
define i16 @load_immediate_neg() {
  ret i16 65436  ; -100 in two's complement
}

;===----------------------------------------------------------------------===;
; Multiple addressing modes in one function
;===----------------------------------------------------------------------===;

; CHECK-LABEL: mixed_addressing:
; Load constant
; CHECK: lda #10
; Store to global (absolute)
; CHECK: sta global_var
; CHECK: rts
define void @mixed_addressing() {
  store i16 10, ptr @global_var
  ret void
}

;===----------------------------------------------------------------------===;
; Indexed with constant base address plus runtime index
;===----------------------------------------------------------------------===;

; CHECK-LABEL: indexed_runtime:
; CHECK: asl a
; CHECK: tax
; CHECK: lda array,x
; CHECK: rts
define i16 @indexed_runtime(i16 %i) {
  %ptr = getelementptr [16 x i16], ptr @array, i16 0, i16 %i
  %val = load i16, ptr %ptr
  ret i16 %val
}


