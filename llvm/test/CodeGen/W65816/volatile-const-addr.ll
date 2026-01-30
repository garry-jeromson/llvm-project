; RUN: llc -march=w65816 < %s | FileCheck %s
; Test stores and loads to constant addresses (from inttoptr)
; This handles SNES PPU register access like poke(0x2115, 0x80)

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; 16-bit store to constant address
;===----------------------------------------------------------------------===;

; CHECK-LABEL: store_const_addr_16:
; Should use direct absolute addressing, not indirect through stack
; CHECK: sta $2100
; CHECK: rts
define void @store_const_addr_16(i16 %val) {
  store volatile i16 %val, ptr inttoptr (i16 8448 to ptr)
  ret void
}

;===----------------------------------------------------------------------===;
; 8-bit store to constant address
;===----------------------------------------------------------------------===;

; CHECK-LABEL: store_const_addr_8:
; Should use direct absolute addressing with mode switch
; CHECK: sep #32
; CHECK: sta $2115
; CHECK: rep #32
; CHECK: rts
define void @store_const_addr_8(i16 %val) {
  %trunc = trunc i16 %val to i8
  store volatile i8 %trunc, ptr inttoptr (i16 8469 to ptr)
  ret void
}

;===----------------------------------------------------------------------===;
; Store immediate to constant address
;===----------------------------------------------------------------------===;

; CHECK-LABEL: store_imm_const_addr:
; CHECK: lda #128
; CHECK: sep #32
; CHECK: sta $2115
; CHECK: rep #32
; CHECK: rts
define void @store_imm_const_addr() {
  store volatile i8 128, ptr inttoptr (i16 8469 to ptr)
  ret void
}

;===----------------------------------------------------------------------===;
; Negative multiply constant (regression test)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: mul_negative:
; Should not crash, should call __mulhi3
; -10000 as unsigned 16-bit = 55536
; CHECK: ldx #55536
; CHECK: jsr __mulhi3
; CHECK: rts
define i16 @mul_negative(i16 %x) {
  %mul = mul i16 %x, -10000
  ret i16 %mul
}

; CHECK-LABEL: mul_negative_small:
; -10 as unsigned 16-bit = 65526
; CHECK: ldx #65526
; CHECK: jsr __mulhi3
; CHECK: rts
define i16 @mul_negative_small(i16 %x) {
  %mul = mul i16 %x, -10
  ret i16 %mul
}
