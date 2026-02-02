; RUN: llvm-mc -triple w65816 -show-encoding %s | FileCheck %s

; Test shift and rotate instructions

; === ASL (Arithmetic Shift Left) ===

; CHECK: asl a
; CHECK-SAME: encoding: [0x0a]
asl a

; CHECK: asl $1234
; CHECK-SAME: encoding: [0x0e,0x34,0x12]
asl 0x1234

; CHECK: asl $1234,x
; CHECK-SAME: encoding: [0x1e,0x34,0x12]
asl 0x1234,x

; === LSR (Logical Shift Right) ===

; CHECK: lsr a
; CHECK-SAME: encoding: [0x4a]
lsr a

; CHECK: lsr $1234
; CHECK-SAME: encoding: [0x4e,0x34,0x12]
lsr 0x1234

; CHECK: lsr $1234,x
; CHECK-SAME: encoding: [0x5e,0x34,0x12]
lsr 0x1234,x

; === ROL (Rotate Left through Carry) ===

; CHECK: rol a
; CHECK-SAME: encoding: [0x2a]
rol a

; CHECK: rol $1234
; CHECK-SAME: encoding: [0x2e,0x34,0x12]
rol 0x1234

; CHECK: rol $1234,x
; CHECK-SAME: encoding: [0x3e,0x34,0x12]
rol 0x1234,x

; === ROR (Rotate Right through Carry) ===

; CHECK: ror a
; CHECK-SAME: encoding: [0x6a]
ror a

; CHECK: ror $1234
; CHECK-SAME: encoding: [0x6e,0x34,0x12]
ror 0x1234

; CHECK: ror $1234,x
; CHECK-SAME: encoding: [0x7e,0x34,0x12]
ror 0x1234,x
