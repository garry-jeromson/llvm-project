; RUN: llvm-mc -triple w65816 -show-encoding %s | FileCheck %s

; Test logical instructions (limited to supported addressing modes)

; === AND ===

; CHECK: and #4660
; CHECK-SAME: encoding: [0x29,0x34,0x12]
and #0x1234

; CHECK: and $1234
; CHECK-SAME: encoding: [0x2d,0x34,0x12]
and 0x1234

; === ORA ===

; CHECK: ora #4660
; CHECK-SAME: encoding: [0x09,0x34,0x12]
ora #0x1234

; CHECK: ora $1234
; CHECK-SAME: encoding: [0x0d,0x34,0x12]
ora 0x1234

; === EOR ===

; CHECK: eor #4660
; CHECK-SAME: encoding: [0x49,0x34,0x12]
eor #0x1234

; CHECK: eor $1234
; CHECK-SAME: encoding: [0x4d,0x34,0x12]
eor 0x1234

; === BIT ===

; CHECK: bit #4660
; CHECK-SAME: encoding: [0x89,0x34,0x12]
bit #0x1234

; CHECK: bit $1234
; CHECK-SAME: encoding: [0x2c,0x34,0x12]
bit 0x1234
