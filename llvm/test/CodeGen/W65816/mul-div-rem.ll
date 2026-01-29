; RUN: llc -march=w65816 < %s | FileCheck %s
; Test multiplication, division, and remainder library calls
;
; The W65816 has no hardware multiply or divide instructions.
; These operations are expanded to calls to runtime library functions:
;   MUL  -> __mulhi3
;   SDIV -> __divhi3
;   UDIV -> __udivhi3
;   SREM -> __modhi3
;   UREM -> __umodhi3

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Multiplication
;
; 16-bit multiplication expands to a call to __mulhi3
;===----------------------------------------------------------------------===;

; CHECK-LABEL: mul16:
; CHECK: jsr __mulhi3
; CHECK: rts
define i16 @mul16(i16 %a, i16 %b) {
  %result = mul i16 %a, %b
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Signed Division
;
; 16-bit signed division expands to a call to __divhi3
;===----------------------------------------------------------------------===;

; CHECK-LABEL: sdiv16:
; CHECK: jsr __divhi3
; CHECK: rts
define i16 @sdiv16(i16 %a, i16 %b) {
  %result = sdiv i16 %a, %b
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Unsigned Division
;
; 16-bit unsigned division expands to a call to __udivhi3
;===----------------------------------------------------------------------===;

; CHECK-LABEL: udiv16:
; CHECK: jsr __udivhi3
; CHECK: rts
define i16 @udiv16(i16 %a, i16 %b) {
  %result = udiv i16 %a, %b
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Signed Remainder
;
; 16-bit signed remainder expands to a call to __modhi3
;===----------------------------------------------------------------------===;

; CHECK-LABEL: srem16:
; CHECK: jsr __modhi3
; CHECK: rts
define i16 @srem16(i16 %a, i16 %b) {
  %result = srem i16 %a, %b
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; Unsigned Remainder
;
; 16-bit unsigned remainder expands to a call to __umodhi3
;===----------------------------------------------------------------------===;

; CHECK-LABEL: urem16:
; CHECK: jsr __umodhi3
; CHECK: rts
define i16 @urem16(i16 %a, i16 %b) {
  %result = urem i16 %a, %b
  ret i16 %result
}
