; RUN: llc -march=w65816 -filetype=obj %s -o %t.o
; RUN: llvm-objdump -d --triple=w65816-unknown-none %t.o | FileCheck %s

; Test that JSR to local functions gets the correct section-relative address
; In a relocatable object, JSR should have the section-relative offset of the target

target triple = "w65816-unknown-none"

; callee is at section offset 0x0000
; CHECK-LABEL: <callee>:
; CHECK: rts
define i16 @callee() {
  ret i16 42
}

; caller is at section offset 0x0004
; The JSR should target callee at offset 0x0000
; CHECK-LABEL: <caller>:
; CHECK: jsr $0000
define i16 @caller() {
  %r = call i16 @callee()
  ret i16 %r
}
