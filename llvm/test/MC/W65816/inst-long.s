; RUN: llvm-mc -triple w65816 -show-encoding %s | FileCheck %s

; Test indirect long addressing modes

; === DP Indirect Long ===

; CHECK: lda [$50]
; CHECK-SAME: encoding: [0xa7,0x50]
lda [0x50]

; CHECK: lda [$50],y
; CHECK-SAME: encoding: [0xb7,0x50]
lda [0x50],y

; CHECK: sta [$60]
; CHECK-SAME: encoding: [0x87,0x60]
sta [0x60]

; CHECK: sta [$60],y
; CHECK-SAME: encoding: [0x97,0x60]
sta [0x60],y

; === Long jump ===

; CHECK: jmp [$1234]
; CHECK-SAME: encoding: [0xdc,0x34,0x12]
jmp [0x1234]
