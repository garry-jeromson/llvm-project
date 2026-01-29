; RUN: llc -march=w65816 < %s | FileCheck %s
; Test memory operations (loads, stores, addressing modes)

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

@global_var = global i16 0
@global_array = global [10 x i16] zeroinitializer

;===----------------------------------------------------------------------===
; Global Variable Access
;===----------------------------------------------------------------------===

; CHECK-LABEL: load_global:
; CHECK: lda global_var
; CHECK: rts
define i16 @load_global() {
  %val = load i16, ptr @global_var
  ret i16 %val
}

; CHECK-LABEL: store_global:
; CHECK: sta global_var
; CHECK: rts
define void @store_global(i16 %val) {
  store i16 %val, ptr @global_var
  ret void
}

;===----------------------------------------------------------------------===
; Array Access with Variable Index (uses indexed addressing)
;===----------------------------------------------------------------------===

; CHECK-LABEL: load_array_var:
; CHECK: asl a
; CHECK: tax
; CHECK: lda global_array,x
; CHECK: rts
define i16 @load_array_var(i16 %idx) {
  %ptr = getelementptr [10 x i16], ptr @global_array, i16 0, i16 %idx
  %val = load i16, ptr %ptr
  ret i16 %val
}

;===----------------------------------------------------------------------===
; Pointer Dereference (indirect addressing via stack)
;===----------------------------------------------------------------------===

; CHECK-LABEL: load_indirect:
; CHECK: sta {{[0-9]+}},s
; CHECK: ldy #0
; CHECK: lda (${{[0-9a-f]+}},s),y
; CHECK: rts
define i16 @load_indirect(ptr %ptr) {
  %val = load i16, ptr %ptr
  ret i16 %val
}

; CHECK-LABEL: store_indirect:
; CHECK: sta (${{[0-9a-f]+}},s),y
; CHECK: rts
define void @store_indirect(ptr %ptr, i16 %val) {
  store i16 %val, ptr %ptr
  ret void
}

;===----------------------------------------------------------------------===
; Double Dereference - **ptr
;===----------------------------------------------------------------------===

; CHECK-LABEL: double_deref:
; CHECK: lda (${{[0-9a-f]+}},s),y
; CHECK: sta {{[0-9]+}},s
; CHECK: lda (${{[0-9a-f]+}},s),y
; CHECK: rts
define i16 @double_deref(ptr %ptr) {
  %inner_ptr = load ptr, ptr %ptr
  %val = load i16, ptr %inner_ptr
  ret i16 %val
}
