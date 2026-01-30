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
