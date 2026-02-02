; RUN: llvm-mc -triple w65816 -show-encoding %s | FileCheck %s

; Test load/store instructions

; === LDA (Load Accumulator) ===

; CHECK: lda #4660
; CHECK-SAME: encoding: [0xa9,0x34,0x12]
lda #0x1234

; CHECK: lda $1234
; CHECK-SAME: encoding: [0xad,0x34,0x12]
lda 0x1234

; CHECK: lda $1234,x
; CHECK-SAME: encoding: [0xbd,0x34,0x12]
lda 0x1234,x

; CHECK: lda $1234,y
; CHECK-SAME: encoding: [0xb9,0x34,0x12]
lda 0x1234,y

; CHECK: lda ($12)
; CHECK-SAME: encoding: [0xb2,0x12]
lda (0x12)

; CHECK: lda ($12),y
; CHECK-SAME: encoding: [0xb1,0x12]
lda (0x12),y

; CHECK: lda ($12,x)
; CHECK-SAME: encoding: [0xa1,0x12]
lda (0x12,x)

; CHECK: lda ($03,s),y
; CHECK-SAME: encoding: [0xb3,0x03]
lda (3,s),y

; === LDX (Load X Register) ===

; CHECK: ldx #4660
; CHECK-SAME: encoding: [0xa2,0x34,0x12]
ldx #0x1234

; CHECK: ldx $1234
; CHECK-SAME: encoding: [0xae,0x34,0x12]
ldx 0x1234

; === LDY (Load Y Register) ===

; CHECK: ldy #4660
; CHECK-SAME: encoding: [0xa0,0x34,0x12]
ldy #0x1234

; CHECK: ldy $1234
; CHECK-SAME: encoding: [0xac,0x34,0x12]
ldy 0x1234

; === STA (Store Accumulator) ===

; CHECK: sta $1234
; CHECK-SAME: encoding: [0x8d,0x34,0x12]
sta 0x1234

; CHECK: sta $1234,x
; CHECK-SAME: encoding: [0x9d,0x34,0x12]
sta 0x1234,x

; CHECK: sta $1234,y
; CHECK-SAME: encoding: [0x99,0x34,0x12]
sta 0x1234,y

; CHECK: sta ($12)
; CHECK-SAME: encoding: [0x92,0x12]
sta (0x12)

; CHECK: sta ($12),y
; CHECK-SAME: encoding: [0x91,0x12]
sta (0x12),y

; CHECK: sta ($12,x)
; CHECK-SAME: encoding: [0x81,0x12]
sta (0x12,x)

; CHECK: sta ($03,s),y
; CHECK-SAME: encoding: [0x93,0x03]
sta (3,s),y

; === STX (Store X Register) ===

; CHECK: stx $1234
; CHECK-SAME: encoding: [0x8e,0x34,0x12]
stx 0x1234

; === STY (Store Y Register) ===

; CHECK: sty $1234
; CHECK-SAME: encoding: [0x8c,0x34,0x12]
sty 0x1234

; === STZ (Store Zero) ===

; CHECK: stz $1234
; CHECK-SAME: encoding: [0x9c,0x34,0x12]
stz 0x1234
