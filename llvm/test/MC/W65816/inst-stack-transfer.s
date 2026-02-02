; RUN: llvm-mc -triple w65816 -show-encoding %s | FileCheck %s

; Test stack and transfer instructions

; === Push instructions ===

; CHECK: pha
; CHECK-SAME: encoding: [0x48]
pha

; CHECK: phx
; CHECK-SAME: encoding: [0xda]
phx

; CHECK: phy
; CHECK-SAME: encoding: [0x5a]
phy

; CHECK: php
; CHECK-SAME: encoding: [0x08]
php

; CHECK: phb
; CHECK-SAME: encoding: [0x8b]
phb

; CHECK: phd
; CHECK-SAME: encoding: [0x0b]
phd

; CHECK: phk
; CHECK-SAME: encoding: [0x4b]
phk

; CHECK: pei ($12)
; CHECK-SAME: encoding: [0xd4,0x12]
pei (0x12)

; === Pull instructions ===

; CHECK: pla
; CHECK-SAME: encoding: [0x68]
pla

; CHECK: plx
; CHECK-SAME: encoding: [0xfa]
plx

; CHECK: ply
; CHECK-SAME: encoding: [0x7a]
ply

; CHECK: plp
; CHECK-SAME: encoding: [0x28]
plp

; CHECK: plb
; CHECK-SAME: encoding: [0xab]
plb

; CHECK: pld
; CHECK-SAME: encoding: [0x2b]
pld

; === Transfer instructions ===

; CHECK: tax
; CHECK-SAME: encoding: [0xaa]
tax

; CHECK: tay
; CHECK-SAME: encoding: [0xa8]
tay

; CHECK: txa
; CHECK-SAME: encoding: [0x8a]
txa

; CHECK: tya
; CHECK-SAME: encoding: [0x98]
tya

; CHECK: txy
; CHECK-SAME: encoding: [0x9b]
txy

; CHECK: tyx
; CHECK-SAME: encoding: [0xbb]
tyx

; CHECK: txs
; CHECK-SAME: encoding: [0x9a]
txs

; CHECK: tsx
; CHECK-SAME: encoding: [0xba]
tsx

; CHECK: tcd
; CHECK-SAME: encoding: [0x5b]
tcd

; CHECK: tdc
; CHECK-SAME: encoding: [0x7b]
tdc

; CHECK: tcs
; CHECK-SAME: encoding: [0x1b]
tcs

; CHECK: tsc
; CHECK-SAME: encoding: [0x3b]
tsc

; CHECK: xba
; CHECK-SAME: encoding: [0xeb]
xba

; CHECK: xce
; CHECK-SAME: encoding: [0xfb]
xce

; === Flag instructions ===

; CHECK: clc
; CHECK-SAME: encoding: [0x18]
clc

; CHECK: sec
; CHECK-SAME: encoding: [0x38]
sec

; CHECK: cli
; CHECK-SAME: encoding: [0x58]
cli

; CHECK: sei
; CHECK-SAME: encoding: [0x78]
sei

; CHECK: cld
; CHECK-SAME: encoding: [0xd8]
cld

; CHECK: sed
; CHECK-SAME: encoding: [0xf8]
sed

; CHECK: clv
; CHECK-SAME: encoding: [0xb8]
clv

; CHECK: rep #48
; CHECK-SAME: encoding: [0xc2,0x30]
rep #0x30

; CHECK: sep #32
; CHECK-SAME: encoding: [0xe2,0x20]
sep #0x20
