; RUN: llvm-mc -triple w65816 -show-encoding %s | FileCheck %s

; Test control flow instructions

; === Branch instructions ===

; CHECK: bra target
; CHECK-SAME: encoding: [0x80,A]
bra target

; CHECK: beq target
; CHECK-SAME: encoding: [0xf0,A]
beq target

; CHECK: bne target
; CHECK-SAME: encoding: [0xd0,A]
bne target

; CHECK: bcs target
; CHECK-SAME: encoding: [0xb0,A]
bcs target

; CHECK: bcc target
; CHECK-SAME: encoding: [0x90,A]
bcc target

; CHECK: bmi target
; CHECK-SAME: encoding: [0x30,A]
bmi target

; CHECK: bpl target
; CHECK-SAME: encoding: [0x10,A]
bpl target

; CHECK: bvs target
; CHECK-SAME: encoding: [0x70,A]
bvs target

; CHECK: bvc target
; CHECK-SAME: encoding: [0x50,A]
bvc target

; CHECK: brl target
; CHECK-SAME: encoding: [0x82,A,A]
brl target

target:
  nop

; === Jump instructions ===

; CHECK: jmp $1234
; CHECK-SAME: encoding: [0x4c,0x34,0x12]
jmp 0x1234

; CHECK: jmp ($1234)
; CHECK-SAME: encoding: [0x6c,0x34,0x12]
jmp (0x1234)

; CHECK: jmp ($1234,x)
; CHECK-SAME: encoding: [0x7c,0x34,0x12]
jmp (0x1234,x)

; CHECK: jmp [$1234]
; CHECK-SAME: encoding: [0xdc,0x34,0x12]
jmp [0x1234]

; === Subroutine calls ===

; CHECK: jsr $1234
; CHECK-SAME: encoding: [0x20,0x34,0x12]
jsr 0x1234

; === Return instructions ===

; CHECK: rts
; CHECK-SAME: encoding: [0x60]
rts

; CHECK: rtl
; CHECK-SAME: encoding: [0x6b]
rtl

; CHECK: rti
; CHECK-SAME: encoding: [0x40]
rti

; === NOP and special ===

; CHECK: nop
; CHECK-SAME: encoding: [0xea]
nop

; CHECK: stp
; CHECK-SAME: encoding: [0xdb]
stp

; CHECK: wai
; CHECK-SAME: encoding: [0xcb]
wai
