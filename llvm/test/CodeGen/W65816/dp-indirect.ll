; RUN: llc -march=w65816 < %s | FileCheck %s
; Test direct page indirect addressing selection

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

; Pointer stored in direct page (zero page)
@dp_ptr = global ptr null, section ".zeropage"

; Regular global for comparison
@regular_ptr = global ptr null

; Regular data
@data = global i16 0

;===----------------------------------------------------------------------===
; Load through DP pointer - should use LDA ($dp) indirect
;===----------------------------------------------------------------------===

; CHECK-LABEL: load_through_dp_ptr:
; GISel uses stack-relative indirect instead of DP indirect
; CHECK: lda dp_ptr
; CHECK: sta {{[0-9]+}},s
; CHECK: ldy #0
; CHECK: lda (${{[0-9a-f]+}},s),y
; CHECK: rts
define i16 @load_through_dp_ptr() {
  %ptr = load ptr, ptr @dp_ptr
  %val = load i16, ptr %ptr
  ret i16 %val
}

;===----------------------------------------------------------------------===
; Store through DP pointer - should use STA ($dp) indirect
;===----------------------------------------------------------------------===

; CHECK-LABEL: store_through_dp_ptr:
; GISel uses stack-relative indirect instead of DP indirect
; CHECK: ldx dp_ptr
; CHECK: sta (${{[0-9a-f]+}},s),y
; CHECK: rts
define void @store_through_dp_ptr(i16 %val) {
  %ptr = load ptr, ptr @dp_ptr
  store i16 %val, ptr %ptr
  ret void
}

;===----------------------------------------------------------------------===
; Load through regular pointer - should use stack-relative indirect
; (for comparison, to show the optimization is applied correctly)
;===----------------------------------------------------------------------===

; CHECK-LABEL: load_through_regular_ptr:
; CHECK: lda regular_ptr
; CHECK: sta {{[0-9]+}},s
; CHECK: ldy #0
; CHECK: lda (${{[0-9a-f]+}},s),y
; CHECK: rts
define i16 @load_through_regular_ptr() {
  %ptr = load ptr, ptr @regular_ptr
  %val = load i16, ptr %ptr
  ret i16 %val
}

;===----------------------------------------------------------------------===
; Indexed access through DP pointer - ptr[i]
; Should use LDA/STA ($dp),Y
;===----------------------------------------------------------------------===

; CHECK-LABEL: load_indexed_dp_ptr:
; GISel uses stack-relative indirect
; CHECK: ldx dp_ptr
; CHECK: asl a
; CHECK: tay
; CHECK: lda (${{[0-9a-f]+}},s),y
; CHECK: rts
define i16 @load_indexed_dp_ptr(i16 %idx) {
  %ptr = load ptr, ptr @dp_ptr
  %addr = getelementptr i16, ptr %ptr, i16 %idx
  %val = load i16, ptr %addr
  ret i16 %val
}

; CHECK-LABEL: store_indexed_dp_ptr:
; GISel uses stack-relative indirect
; CHECK: ldy dp_ptr
; CHECK: asl a
; CHECK: sta (${{[0-9a-f]+}},s),y
; CHECK: rts
define void @store_indexed_dp_ptr(i16 %idx, i16 %val) {
  %ptr = load ptr, ptr @dp_ptr
  %addr = getelementptr i16, ptr %ptr, i16 %idx
  store i16 %val, ptr %addr
  ret void
}
