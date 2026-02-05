; RUN: llc -march=w65816 < %s | FileCheck %s
; Test 8-bit operations and mode switching

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; 8-bit load and store
;===----------------------------------------------------------------------===;

@byte_var = global i8 0

; CHECK-LABEL: load_byte:
; CHECK: sep #32
; CHECK: lda byte_var
; CHECK: rep #32
; CHECK: and #255
; CHECK: rts
define i16 @load_byte() {
  %v = load i8, ptr @byte_var
  %ext = zext i8 %v to i16
  ret i16 %ext
}

; CHECK-LABEL: store_byte:
; CHECK: sep #32
; CHECK: sta byte_var
; CHECK: rep #32
; CHECK: rts
define void @store_byte(i16 %v) {
  %trunc = trunc i16 %v to i8
  store i8 %trunc, ptr @byte_var
  ret void
}

;===----------------------------------------------------------------------===;
; 8-bit truncation and zero extension
;===----------------------------------------------------------------------===;

; CHECK-LABEL: truncate_and_extend:
; Zero extension just masks with 0xFF
; CHECK: and #255
; CHECK: rts
define i16 @truncate_and_extend(i16 %x) {
  %byte = trunc i16 %x to i8
  %zext = zext i8 %byte to i16
  ret i16 %zext
}

;===----------------------------------------------------------------------===;
; Multiple 8-bit stores
;===----------------------------------------------------------------------===;

@byte_var2 = global i8 0

; CHECK-LABEL: two_byte_stores:
; Both stores use SEP/REP sequences (order may vary)
; CHECK: sep #32
; CHECK: sta byte_var
; CHECK: rep #32
; CHECK: rts
define void @two_byte_stores(i16 %v) {
  %trunc = trunc i16 %v to i8
  store i8 %trunc, ptr @byte_var
  store i8 %trunc, ptr @byte_var2
  ret void
}

;===----------------------------------------------------------------------===;
; 8-bit constant store
;===----------------------------------------------------------------------===;

; CHECK-LABEL: store_byte_const:
; Note: LDA happens in 16-bit mode, SEP switches to 8-bit for STA
; CHECK: lda #42
; CHECK: sep #32
; CHECK: sta byte_var
; CHECK: rep #32
; CHECK: rts
define void @store_byte_const() {
  store i8 42, ptr @byte_var
  ret void
}

;===----------------------------------------------------------------------===;
; Load byte and use in 16-bit operation
;===----------------------------------------------------------------------===;

; CHECK-LABEL: load_byte_use_word:
; CHECK: sep #32
; CHECK: lda byte_var
; CHECK: rep #32
; CHECK: and #255
; CHECK: clc
; CHECK: adc #100
; CHECK: rts
define i16 @load_byte_use_word() {
  %v = load i8, ptr @byte_var
  %ext = zext i8 %v to i16
  %result = add i16 %ext, 100
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; 8-bit indexed global array access
;===----------------------------------------------------------------------===;

@byte_array = global [16 x i8] zeroinitializer

; Test 8-bit load from global array with constant index
; GISel uses indirect addressing for byte array access
; CHECK-LABEL: load_byte_array_const:
; CHECK: lda #byte_array
; CHECK: sep #32
; CHECK: ldy #5
; CHECK: lda (${{[0-9]+}},s),y
; CHECK: rep #32
; CHECK: and #255
; CHECK: rts
define i16 @load_byte_array_const() {
  %ptr = getelementptr [16 x i8], ptr @byte_array, i16 0, i16 5
  %v = load i8, ptr %ptr
  %ext = zext i8 %v to i16
  ret i16 %ext
}

; Test 8-bit load from global array with variable index
; GISel uses indirect addressing for byte array access
; CHECK-LABEL: load_byte_array_var:
; CHECK: ldx #byte_array
; CHECK: sep #32
; CHECK: lda (${{[0-9]+}},s),y
; CHECK: rep #32
; CHECK: and #255
; CHECK: rts
define i16 @load_byte_array_var(i16 %idx) {
  %ptr = getelementptr [16 x i8], ptr @byte_array, i16 0, i16 %idx
  %v = load i8, ptr %ptr
  %ext = zext i8 %v to i16
  ret i16 %ext
}

; Test 8-bit store to global array with constant index
; CHECK-LABEL: store_byte_array_const:
; CHECK: sep #32
; CHECK: sta byte_array+3
; CHECK: rep #32
; CHECK: rts
define void @store_byte_array_const(i16 %val) {
  %trunc = trunc i16 %val to i8
  %ptr = getelementptr [16 x i8], ptr @byte_array, i16 0, i16 3
  store i8 %trunc, ptr %ptr
  ret void
}

; Test 8-bit store to global array with variable index
; GISel uses indirect addressing for byte array stores
; CHECK-LABEL: store_byte_array_var:
; CHECK: ldy #byte_array
; CHECK: sep #32
; CHECK: sta (${{[0-9]+}},s),y
; CHECK: rep #32
; CHECK: rts
define void @store_byte_array_var(i16 %val, i16 %idx) {
  %trunc = trunc i16 %val to i8
  %ptr = getelementptr [16 x i8], ptr @byte_array, i16 0, i16 %idx
  store i8 %trunc, ptr %ptr
  ret void
}
