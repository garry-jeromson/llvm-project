; RUN: llc -march=w65816 < %s | FileCheck %s
; Test boolean (i1) load and store operations

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; Boolean global variable load and store
;===----------------------------------------------------------------------===;

@bool_var = global i1 false

; Boolean load - should load as i8 and mask to ensure 0 or 1
; CHECK-LABEL: load_bool:
; CHECK: sep #32
; CHECK: lda bool_var
; CHECK: rep #32
; CHECK: and #255
; CHECK: and #1
; CHECK: rts
define i16 @load_bool() {
  %v = load i1, ptr @bool_var
  %ext = zext i1 %v to i16
  ret i16 %ext
}

; Boolean store - should store as i8 (0 or 1)
; CHECK-LABEL: store_bool:
; CHECK: and #1
; CHECK: sep #32
; CHECK: sta bool_var
; CHECK: rep #32
; CHECK: rts
define void @store_bool(i16 %v) {
  %trunc = trunc i16 %v to i1
  store i1 %trunc, ptr @bool_var
  ret void
}

;===----------------------------------------------------------------------===;
; Boolean used in conditional branch
;===----------------------------------------------------------------------===;

; CHECK-LABEL: branch_on_bool:
; CHECK: sep #32
; CHECK: lda bool_var
; CHECK: rep #32
; CHECK: and #255
; CHECK: and #1
; CHECK: cmp #1
; CHECK: bne
define i16 @branch_on_bool() {
entry:
  %cond = load i1, ptr @bool_var
  br i1 %cond, label %if.true, label %if.false

if.true:
  ret i16 1

if.false:
  ret i16 0
}

;===----------------------------------------------------------------------===;
; Boolean comparison result stored to memory
;===----------------------------------------------------------------------===;

; CHECK-LABEL: bool_from_cmp:
; The comparison result is stored as a boolean (0 or 1)
; CHECK: and #1
; CHECK: sep #32
; CHECK: sta bool_var
; CHECK: rep #32
; CHECK: rts
define void @bool_from_cmp(i16 %a, i16 %b) {
  %cmp = icmp eq i16 %a, %b
  store i1 %cmp, ptr @bool_var
  ret void
}

;===----------------------------------------------------------------------===;
; Boolean in struct (offset access)
;===----------------------------------------------------------------------===;

%struct.with_bool = type { i16, i1, i16 }

@struct_var = global %struct.with_bool zeroinitializer

; CHECK-LABEL: load_struct_bool:
; The bool is at offset 2 in the struct
; CHECK: sep #32
; CHECK: lda struct_var+2
; CHECK: rep #32
; CHECK: and #255
; CHECK: and #1
; CHECK: rts
define i16 @load_struct_bool() {
  %ptr = getelementptr %struct.with_bool, ptr @struct_var, i32 0, i32 1
  %v = load i1, ptr %ptr
  %ext = zext i1 %v to i16
  ret i16 %ext
}

; CHECK-LABEL: store_struct_bool:
; CHECK: and #1
; CHECK: sep #32
; CHECK: sta struct_var+2
; CHECK: rep #32
; CHECK: rts
define void @store_struct_bool(i16 %v) {
  %ptr = getelementptr %struct.with_bool, ptr @struct_var, i32 0, i32 1
  %trunc = trunc i16 %v to i1
  store i1 %trunc, ptr %ptr
  ret void
}

;===----------------------------------------------------------------------===;
; Boolean NOT (simple single-value operation)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: bool_not:
; CHECK: sep #32
; CHECK: lda bool_var
; CHECK: rep #32
; CHECK: and #255
; Full 16-bit NOT (eor #65535) then mask to get single-bit NOT
; CHECK: eor #65535
; CHECK: and #1
; CHECK: rts
define i16 @bool_not() {
  %v = load i1, ptr @bool_var
  %not = xor i1 %v, true
  %ext = zext i1 %not to i16
  ret i16 %ext
}
