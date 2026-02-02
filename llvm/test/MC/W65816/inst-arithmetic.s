; RUN: llvm-mc -triple w65816 -show-encoding %s | FileCheck %s

; Test arithmetic instructions (limited to supported addressing modes)

; === ADC (Add with Carry) ===

; CHECK: adc #4660
; CHECK-SAME: encoding: [0x69,0x34,0x12]
adc #0x1234

; CHECK: adc $1234
; CHECK-SAME: encoding: [0x6d,0x34,0x12]
adc 0x1234

; === SBC (Subtract with Borrow) ===

; CHECK: sbc #4660
; CHECK-SAME: encoding: [0xe9,0x34,0x12]
sbc #0x1234

; CHECK: sbc $1234
; CHECK-SAME: encoding: [0xed,0x34,0x12]
sbc 0x1234

; === INC/DEC ===

; CHECK: inc a
; CHECK-SAME: encoding: [0x1a]
inc a

; CHECK: inc $1234
; CHECK-SAME: encoding: [0xee,0x34,0x12]
inc 0x1234

; CHECK: dec a
; CHECK-SAME: encoding: [0x3a]
dec a

; CHECK: dec $1234
; CHECK-SAME: encoding: [0xce,0x34,0x12]
dec 0x1234

; === INX/DEX/INY/DEY ===

; CHECK: inx
; CHECK-SAME: encoding: [0xe8]
inx

; CHECK: dex
; CHECK-SAME: encoding: [0xca]
dex

; CHECK: iny
; CHECK-SAME: encoding: [0xc8]
iny

; CHECK: dey
; CHECK-SAME: encoding: [0x88]
dey

; === CMP (Compare) ===

; CHECK: cmp #4660
; CHECK-SAME: encoding: [0xc9,0x34,0x12]
cmp #0x1234

; CHECK: cmp $1234
; CHECK-SAME: encoding: [0xcd,0x34,0x12]
cmp 0x1234

; === CPX/CPY (Compare X/Y) ===

; CHECK: cpx #4660
; CHECK-SAME: encoding: [0xe0,0x34,0x12]
cpx #0x1234

; CHECK: cpy #4660
; CHECK-SAME: encoding: [0xc0,0x34,0x12]
cpy #0x1234
