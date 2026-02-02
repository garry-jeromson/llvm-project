; RUN: llvm-mc -triple w65816 -show-encoding %s | FileCheck %s

; Test various integer literal formats

; === C-style hex (0x prefix) ===

; CHECK: lda #255
; CHECK-SAME: encoding: [0xa9,0xff,0x00]
lda #0xFF

; CHECK: lda #4660
; CHECK-SAME: encoding: [0xa9,0x34,0x12]
lda #0x1234

; CHECK: lda $abcd
; CHECK-SAME: encoding: [0xad,0xcd,0xab]
lda 0xABCD

; === Decimal literals ===

; CHECK: lda #255
; CHECK-SAME: encoding: [0xa9,0xff,0x00]
lda #255

; CHECK: lda $0100
; CHECK-SAME: encoding: [0xad,0x00,0x01]
lda 256

; CHECK: lda #4660
; CHECK-SAME: encoding: [0xa9,0x34,0x12]
lda #4660

; === SEP/REP ===

; CHECK: sep #32
; CHECK-SAME: encoding: [0xe2,0x20]
sep #0x20

; CHECK: rep #48
; CHECK-SAME: encoding: [0xc2,0x30]
rep #0x30

; CHECK: rep #16
; CHECK-SAME: encoding: [0xc2,0x10]
rep #16
