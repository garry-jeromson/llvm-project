; RUN: llc -march=w65816 < %s | FileCheck %s
; Test 8-bit operations through pointers - mode switching sequences
;
; This test documents the current code generation for 8-bit operations.
; Currently, each 8-bit load/store generates its own SEP/REP pair.
; A future optimization could merge adjacent 8-bit operations to share
; a single SEP/REP pair, reducing code size.
;
; Current (correct but not optimal):
;   sep #32 / lda (n,s),y / rep #32 / and #255 / sep #32 / sta (m,s),y / rep #32
;
; Potential future optimization:
;   sep #32 / lda (n,s),y / sta (m,s),y / rep #32

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; Byte Copy Through Pointers
;
; Copies a byte from src pointer to dst pointer.
; Currently generates two SEP/REP pairs (one for load, one for store).
;===----------------------------------------------------------------------===

; CHECK-LABEL: byte_copy:
; CHECK: sep #32
; CHECK: lda (${{[0-9a-f]+}},s),y
; CHECK: rep #32
; CHECK: and #255
; CHECK: sep #32
; CHECK: sta (${{[0-9a-f]+}},s),y
; CHECK: rep #32
; CHECK: rts
define void @byte_copy(ptr %src, ptr %dst) {
  %val = load i8, ptr %src
  store i8 %val, ptr %dst
  ret void
}

;===----------------------------------------------------------------------===
; Byte Increment Through Pointer
;
; Load a byte, increment it, store back.
; The increment happens in 16-bit mode (after zero-extension), so
; the SEP/REP pairs cannot be merged in this case.
;===----------------------------------------------------------------------===

; CHECK-LABEL: byte_increment:
; CHECK: sep #32
; CHECK: lda (${{[0-9a-f]+}},s),y
; CHECK: rep #32
; CHECK: and #255
; Register allocator may use INC A, INX, or INY for increment
; CHECK: sep #32
; CHECK: sta (${{[0-9a-f]+}},s),y
; CHECK: rep #32
; CHECK: rts
define void @byte_increment(ptr %ptr) {
  %val = load i8, ptr %ptr
  %inc = add i8 %val, 1
  store i8 %inc, ptr %ptr
  ret void
}
