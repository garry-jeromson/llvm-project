; RUN: llc -march=w65816 < %s | FileCheck %s

; Test that LDA #0; STA addr sequences are optimized to STZ addr

target triple = "w65816-unknown-none"

; Test 1: Store zero to global should use STZ
define void @store_zero_global() {
; CHECK-LABEL: store_zero_global:
; CHECK:       stz global_var
; CHECK-NOT:   lda #0
; CHECK-NEXT:  rts
entry:
  store i16 0, ptr @global_var
  ret void
}

; Test 2: Store non-zero should still use LDA/STA
define void @store_nonzero_global() {
; CHECK-LABEL: store_nonzero_global:
; CHECK:       lda #42
; CHECK-NEXT:  sta global_var
; CHECK-NEXT:  rts
entry:
  store i16 42, ptr @global_var
  ret void
}

@global_var = global i16 0
