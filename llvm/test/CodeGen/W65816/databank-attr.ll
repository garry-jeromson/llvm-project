; RUN: llc -march=w65816 < %s | FileCheck %s
; Test w65816_databank attribute for function-level DBR management

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

; Function with data bank attribute - should emit PHB/PLB
; CHECK-LABEL: bank1_function:
; Prologue: save DBR, save X (argument), set bank 1 via 8-bit mode, restore X
; CHECK: phb
; CHECK: phx
; CHECK: sep #16
; CHECK: ldx #1
; CHECK: phx
; CHECK: plb
; CHECK: rep #16
; CHECK: plx
; Function body (adds 100 to argument)
; CHECK: clc
; CHECK: adc #100
; Epilogue: restore DBR
; CHECK: plb
; CHECK: rts
define i16 @bank1_function(i16 %x) #0 {
  %sum = add i16 %x, 100
  ret i16 %sum
}

; Function without data bank attribute - should NOT emit PHB/PLB
; CHECK-LABEL: normal_function:
; CHECK-NOT: phb
; CHECK-NOT: plb
; CHECK: rts
define i16 @normal_function(i16 %x) {
  %sum = add i16 %x, 50
  ret i16 %sum
}

; Function with bank 2
; CHECK-LABEL: bank2_function:
; CHECK: phb
; CHECK: phx
; CHECK: sep #16
; CHECK: ldx #2
; CHECK: phx
; CHECK: plb
; CHECK: rep #16
; CHECK: plx
; CHECK: plb
; CHECK: rts
define i16 @bank2_function(i16 %x) #1 {
  ret i16 %x
}

attributes #0 = { "w65816_databank"="1" }
attributes #1 = { "w65816_databank"="2" }
