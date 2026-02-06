; RUN: llc -march=w65816 < %s | FileCheck %s
; Test runtime mode switching for 8-bit operations within 16-bit mode functions
; This tests the SEP/REP sequences for byte operations in default 16-bit mode

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

@byte_var = global i8 0
@word_var = global i16 0

;===----------------------------------------------------------------------===
; 8-bit Load (Zero-Extend) from Absolute Address
; Should generate: SEP #$20, LDA addr, REP #$20, AND #$00FF
;===----------------------------------------------------------------------===

; CHECK-LABEL: load_byte_zext:
; CHECK: sep #32
; CHECK: lda byte_var
; CHECK: rep #32
; CHECK: and #255
; CHECK: rts
define i16 @load_byte_zext() {
  %val = load i8, ptr @byte_var
  %ext = zext i8 %val to i16
  ret i16 %ext
}

;===----------------------------------------------------------------------===
; 8-bit Store (Truncate) to Absolute Address
; Should generate: SEP #$20, STA addr, REP #$20
;===----------------------------------------------------------------------===

; CHECK-LABEL: store_byte_trunc:
; CHECK: sep #32
; CHECK: sta byte_var
; CHECK: rep #32
; CHECK: rts
define void @store_byte_trunc(i16 %val) {
  %trunc = trunc i16 %val to i8
  store i8 %trunc, ptr @byte_var
  ret void
}

;===----------------------------------------------------------------------===
; Load byte and store back (round-trip)
; Uses volatile to prevent optimizer from removing the dead store
;===----------------------------------------------------------------------===

; CHECK-LABEL: byte_roundtrip:
; CHECK: sep #32
; CHECK: lda byte_var
; CHECK: rep #32
; CHECK: and #255
; CHECK: sep #32
; CHECK: sta byte_var
; CHECK: rep #32
; CHECK: rts
define void @byte_roundtrip() {
  %val = load i8, ptr @byte_var
  store volatile i8 %val, ptr @byte_var
  ret void
}

;===----------------------------------------------------------------------===
; Load byte, add 1, store back
; Should do mode switches for load and store, arithmetic in 16-bit mode
;===----------------------------------------------------------------------===

; CHECK-LABEL: inc_byte:
; CHECK: sep #32
; CHECK: lda byte_var
; CHECK: rep #32
; CHECK: and #255
; Register allocator may use A (inc a) or X (tax; inx; txa)
; CHECK: sep #32
; CHECK: sta byte_var
; CHECK: rep #32
; CHECK: rts
define void @inc_byte() {
  %val = load i8, ptr @byte_var
  %ext = zext i8 %val to i16
  %inc = add i16 %ext, 1
  %trunc = trunc i16 %inc to i8
  store volatile i8 %trunc, ptr @byte_var
  ret void
}

