; RUN: llc -march=w65816 < %s | FileCheck %s
; Test 8-bit operations through pointers (indirect addressing with mode switching)
;
; EXPECTED BEHAVIOR:
; When loading/storing 8-bit values through pointers in 16-bit mode, the backend
; must use SEP/REP sequences to temporarily switch to 8-bit accumulator mode.
; Since W65816 cannot load directly through a register-held pointer, the pointer
; must be stored to a stack slot and accessed via stack-relative indirect addressing.

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; 8-bit Load through Pointer (Indirect Addressing)
;
; Expected sequence:
;   1. Store pointer to stack slot (sta n,s)
;   2. Switch to 8-bit accumulator (sep #32)
;   3. Set Y index to 0 (ldy #0)
;   4. Load through pointer using stack-relative indirect (lda (n,s),y)
;   5. Switch back to 16-bit accumulator (rep #32)
;   6. Zero-extend result (and #255)
;===----------------------------------------------------------------------===

; CHECK-LABEL: load_byte_indirect:
; CHECK: sta {{[0-9]+}},s
; CHECK: sep #32
; CHECK: ldy #0
; CHECK: lda (${{[0-9a-f]+}},s),y
; CHECK: rep #32
; CHECK: and #255
; CHECK: rts
define i16 @load_byte_indirect(ptr %ptr) {
  %val = load i8, ptr %ptr
  %ext = zext i8 %val to i16
  ret i16 %ext
}

;===----------------------------------------------------------------------===
; 8-bit Store through Pointer (Indirect Addressing)
;
; Expected sequence:
;   1. Store pointer to stack slot (sta n,s)
;   2. Switch to 8-bit accumulator (sep #32)
;   3. Set Y index to 0 (ldy #0)
;   4. Store value through pointer using stack-relative indirect (sta (n,s),y)
;   5. Switch back to 16-bit accumulator (rep #32)
;===----------------------------------------------------------------------===

; CHECK-LABEL: store_byte_indirect:
; CHECK: sta {{[0-9]+}},s
; CHECK: sep #32
; CHECK: ldy #0
; CHECK: sta (${{[0-9a-f]+}},s),y
; CHECK: rep #32
; CHECK: rts
define void @store_byte_indirect(ptr %ptr, i16 %val) {
  %trunc = trunc i16 %val to i8
  store i8 %trunc, ptr %ptr
  ret void
}

;===----------------------------------------------------------------------===
; 8-bit Indexed Load through Pointer (ptr[i] for byte arrays)
;
; Expected sequence:
;   1. Compute effective address (ptr + index)
;   2. Store effective address to stack slot
;   3. Switch to 8-bit accumulator (sep #32)
;   4. Set Y index to 0 (ldy #0)
;   5. Load through pointer using stack-relative indirect (lda (n,s),y)
;   6. Switch back to 16-bit accumulator (rep #32)
;   7. Zero-extend result (and #255)
;===----------------------------------------------------------------------===

; CHECK-LABEL: load_byte_indexed:
; CHECK: sep #32
; CHECK: lda (${{[0-9a-f]+}},s),y
; CHECK: rep #32
; CHECK: and #255
; CHECK: rts
define i16 @load_byte_indexed(ptr %ptr, i16 %idx) {
  %gep = getelementptr i8, ptr %ptr, i16 %idx
  %val = load i8, ptr %gep
  %ext = zext i8 %val to i16
  ret i16 %ext
}

;===----------------------------------------------------------------------===
; 8-bit Indexed Store through Pointer (ptr[i] = val for byte arrays)
;
; Expected sequence:
;   1. Store base pointer to stack slot
;   2. Move index to Y
;   3. Move value to A
;   4. Switch to 8-bit accumulator (sep #32)
;   5. Store through pointer using stack-relative indirect (sta (n,s),y)
;   6. Switch back to 16-bit accumulator (rep #32)
;===----------------------------------------------------------------------===

; CHECK-LABEL: store_byte_indexed:
; CHECK: sep #32
; CHECK: sta (${{[0-9a-f]+}},s),y
; CHECK: rep #32
; CHECK: rts
define void @store_byte_indexed(ptr %ptr, i16 %idx, i16 %val) {
  %gep = getelementptr i8, ptr %ptr, i16 %idx
  %trunc = trunc i16 %val to i8
  store i8 %trunc, ptr %gep
  ret void
}
