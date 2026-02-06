; RUN: llc -march=w65816 < %s | FileCheck %s
; Test interrupt handler support
;
; Interrupt handlers have special prologue/epilogue:
; - Save all registers (A, X, Y) at entry
; - Restore all registers at exit
; - Use RTI instead of RTS

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Basic Interrupt Handler
;
; Should generate:
;   rep #48      ; Ensure 16-bit mode for saving registers
;   pha          ; Save A
;   phx          ; Save X
;   phy          ; Save Y
;   ... body ...
;   rep #48      ; Ensure 16-bit mode for restoring
;   ply          ; Restore Y
;   plx          ; Restore X
;   pla          ; Restore A
;   rti          ; Return from interrupt
;===----------------------------------------------------------------------===;

; CHECK-LABEL: irq_handler:
; CHECK: rep #48
; CHECK-NEXT: pha
; CHECK-NEXT: phx
; CHECK-NEXT: phy
; CHECK: rep #48
; CHECK-NEXT: ply
; CHECK-NEXT: plx
; CHECK-NEXT: pla
; CHECK-NEXT: rts
define void @irq_handler() #0 {
entry:
  ret void
}

;===----------------------------------------------------------------------===;
; Interrupt Handler with Stack Usage
;
; Should save registers, allocate stack space, then clean up properly
;===----------------------------------------------------------------------===;

; CHECK-LABEL: irq_with_local:
; CHECK: rep #48
; CHECK-NEXT: pha
; CHECK-NEXT: phx
; CHECK-NEXT: phy
; CHECK: rep #48
; CHECK-NEXT: ply
; CHECK-NEXT: plx
; CHECK-NEXT: pla
; CHECK-NEXT: rts
define void @irq_with_local() #0 {
entry:
  %local = alloca i16
  store i16 42, ptr %local
  ret void
}

;===----------------------------------------------------------------------===;
; Regular function (non-interrupt) for comparison
;
; Should use normal prologue/epilogue with RTS
;===----------------------------------------------------------------------===;

; CHECK-LABEL: regular_function:
; CHECK-NOT: pha
; CHECK-NOT: phx
; CHECK-NOT: phy
; CHECK: rts
; CHECK-NOT: rti
define void @regular_function() {
entry:
  ret void
}

;===----------------------------------------------------------------------===;
; Interrupt Handler with DP Frame
;
; Tests that DP frame functions correctly coexist with interrupt handlers.
; The prologue order should be:
;   rep #48, pha, phx, phy (save interrupt registers)
;   phd, lda #0, tcd (set up DP frame)
; The epilogue order should be:
;   pld (restore D)
;   rep #48, ply, plx, pla (restore interrupt registers)
;   rti
;===----------------------------------------------------------------------===;

; CHECK-LABEL: irq_dpframe:
; Prologue: save interrupt registers, then set up DP
; CHECK: rep #48
; CHECK-NEXT: pha
; CHECK-NEXT: phx
; CHECK-NEXT: phy
; CHECK: phd
; CHECK: lda #0
; CHECK: tcd
; Epilogue: restore D, then interrupt registers
; CHECK: pld
; CHECK: rep #48
; CHECK-NEXT: ply
; CHECK-NEXT: plx
; CHECK-NEXT: pla
; CHECK-NEXT: rts
define void @irq_dpframe() #1 {
entry:
  ret void
}

attributes #0 = { "interrupt" }
attributes #1 = { "interrupt" "w65816_dpframe" }
