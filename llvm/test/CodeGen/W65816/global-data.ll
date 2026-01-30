; RUN: llc -march=w65816 < %s | FileCheck %s
; Test global data handling: initialized arrays, constants, struct-like access

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Simple global variables
;===----------------------------------------------------------------------===;

@simple_var = global i16 0
@init_var = global i16 42

; CHECK-LABEL: read_simple:
; CHECK: lda simple_var
; CHECK: rts
define i16 @read_simple() {
  %v = load i16, ptr @simple_var
  ret i16 %v
}

; CHECK-LABEL: read_init:
; CHECK: lda init_var
; CHECK: rts
define i16 @read_init() {
  %v = load i16, ptr @init_var
  ret i16 %v
}

; CHECK-LABEL: write_simple:
; CHECK: sta simple_var
; CHECK: rts
define void @write_simple(i16 %v) {
  store i16 %v, ptr @simple_var
  ret void
}

;===----------------------------------------------------------------------===;
; Initialized arrays
;===----------------------------------------------------------------------===;

@int_array = global [4 x i16] [i16 10, i16 20, i16 30, i16 40]

; CHECK-LABEL: read_array_const:
; Load element 2 (offset 4 bytes)
; CHECK: lda int_array+4
; CHECK: rts
define i16 @read_array_const() {
  %ptr = getelementptr [4 x i16], ptr @int_array, i16 0, i16 2
  %v = load i16, ptr %ptr
  ret i16 %v
}

; CHECK-LABEL: read_array_first:
; CHECK: lda int_array
; CHECK: rts
define i16 @read_array_first() {
  %ptr = getelementptr [4 x i16], ptr @int_array, i16 0, i16 0
  %v = load i16, ptr %ptr
  ret i16 %v
}

; CHECK-LABEL: read_array_last:
; Element 3 = offset 6 bytes
; CHECK: lda int_array+6
; CHECK: rts
define i16 @read_array_last() {
  %ptr = getelementptr [4 x i16], ptr @int_array, i16 0, i16 3
  %v = load i16, ptr %ptr
  ret i16 %v
}

;===----------------------------------------------------------------------===;
; Constant data (in .rodata section)
;===----------------------------------------------------------------------===;

@const_data = constant [3 x i16] [i16 100, i16 200, i16 300]

; CHECK-LABEL: read_const:
; CHECK: lda const_data+2
; CHECK: rts
define i16 @read_const() {
  %ptr = getelementptr [3 x i16], ptr @const_data, i16 0, i16 1
  %v = load i16, ptr %ptr
  ret i16 %v
}

;===----------------------------------------------------------------------===;
; Struct-like access (adjacent fields)
;===----------------------------------------------------------------------===;

; Simulate a simple struct { i16 x; i16 y; i16 z; }
@point = global [3 x i16] [i16 0, i16 0, i16 0]

; CHECK-LABEL: get_point_x:
; CHECK: lda point
; CHECK: rts
define i16 @get_point_x() {
  %ptr = getelementptr [3 x i16], ptr @point, i16 0, i16 0
  %v = load i16, ptr %ptr
  ret i16 %v
}

; CHECK-LABEL: get_point_y:
; CHECK: lda point+2
; CHECK: rts
define i16 @get_point_y() {
  %ptr = getelementptr [3 x i16], ptr @point, i16 0, i16 1
  %v = load i16, ptr %ptr
  ret i16 %v
}

; CHECK-LABEL: get_point_z:
; CHECK: lda point+4
; CHECK: rts
define i16 @get_point_z() {
  %ptr = getelementptr [3 x i16], ptr @point, i16 0, i16 2
  %v = load i16, ptr %ptr
  ret i16 %v
}

; CHECK-LABEL: set_point_y:
; CHECK: sta point+2
; CHECK: rts
define void @set_point_y(i16 %v) {
  %ptr = getelementptr [3 x i16], ptr @point, i16 0, i16 1
  store i16 %v, ptr %ptr
  ret void
}

;===----------------------------------------------------------------------===;
; Zero-page globals (faster access)
;===----------------------------------------------------------------------===;

@zp_var = global i16 0, section ".zeropage"

; CHECK-LABEL: read_zp:
; CHECK: lda zp_var
; CHECK: rts
define i16 @read_zp() {
  %v = load i16, ptr @zp_var
  ret i16 %v
}

;===----------------------------------------------------------------------===;
; 8-bit global data
;===----------------------------------------------------------------------===;

@byte_var = global i8 0
@init_byte = global i8 42

; CHECK-LABEL: read_byte:
; Should switch to 8-bit mode for the load
; CHECK: sep #32
; CHECK: lda byte_var
; CHECK: rep #32
; CHECK: and #255
; CHECK: rts
define i16 @read_byte() {
  %v = load i8, ptr @byte_var
  %ext = zext i8 %v to i16
  ret i16 %ext
}

; CHECK-LABEL: write_byte:
; CHECK: sep #32
; CHECK: sta byte_var
; CHECK: rep #32
; CHECK: rts
define void @write_byte(i16 %v) {
  %trunc = trunc i16 %v to i8
  store i8 %trunc, ptr @byte_var
  ret void
}

;===----------------------------------------------------------------------===;
; Data section verification
;===----------------------------------------------------------------------===;

; Verify initialized data appears correctly
; CHECK: .global	int_array
; CHECK: int_array:
; CHECK: .word	10
; CHECK: .word	20
; CHECK: .word	30
; CHECK: .word	40

; Verify constant data
; CHECK: .global	const_data
; CHECK: const_data:
; CHECK: .word	100
; CHECK: .word	200
; CHECK: .word	300

