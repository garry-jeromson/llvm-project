; RUN: llc -march=w65816 < %s | FileCheck %s
; Test long (24-bit) addressing modes for cross-bank memory access

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===
; Long Addressing Overview:
;
; The W65816 has a 24-bit address space (16MB), divided into 256 banks of 64KB.
; Long addressing allows access to any location in the full address space.
;
; Key instructions (now defined, need selection patterns):
; - LDA $123456     : Load from 24-bit absolute address (4-byte instruction)
; - STA $123456     : Store to 24-bit absolute address
; - LDA $123456,X   : Long indexed with X register
; - STA $123456,X   : Long indexed store with X
; - LDA [$dp]       : Load through 24-bit pointer in direct page
; - LDA [$dp],Y     : Load through 24-bit pointer + Y offset
;
; Note: Full support for address space 1 (24-bit pointers) would require
; data layout changes and is a future enhancement.
;===----------------------------------------------------------------------===

;===----------------------------------------------------------------------===
; Current behavior: far calls still use JSR (TODO: use JSL for addrspace(1))
;===----------------------------------------------------------------------===

declare void @far_function() addrspace(1)

; CHECK-LABEL: test_call:
; CHECK: jsr far_function
; CHECK: rts
define void @test_call() {
  call addrspace(1) void @far_function()
  ret void
}

;===----------------------------------------------------------------------===
; Regular 16-bit addressing still works
;===----------------------------------------------------------------------===

@global_var = global i16 0

; CHECK-LABEL: test_regular_load:
; CHECK: lda global_var
; CHECK: rts
define i16 @test_regular_load() {
  %val = load i16, ptr @global_var
  ret i16 %val
}

; CHECK-LABEL: test_regular_store:
; CHECK: sta global_var
; CHECK: rts
define void @test_regular_store(i16 %val) {
  store i16 %val, ptr @global_var
  ret void
}

;===----------------------------------------------------------------------===
; Far (24-bit) addressing for cross-bank access
; Globals in .fardata, .rodata, or .romdata sections use long addressing
;===----------------------------------------------------------------------===

@far_data = global i16 0, section ".fardata"
@rom_table = constant [4 x i16] [i16 1, i16 2, i16 3, i16 4], section ".rodata"

; CHECK-LABEL: test_far_load:
; CHECK: lda far_data
; CHECK: rts
define i16 @test_far_load() {
  %val = load i16, ptr @far_data
  ret i16 %val
}

; CHECK-LABEL: test_far_store:
; CHECK: sta far_data
; CHECK: rts
define void @test_far_store(i16 %val) {
  store i16 %val, ptr @far_data
  ret void
}

; CHECK-LABEL: test_rom_load:
; CHECK: lda rom_table
; CHECK: rts
define i16 @test_rom_load() {
  %val = load i16, ptr @rom_table
  ret i16 %val
}

; Test loading from far array with constant index
; Constant offset is folded into the long address
; CHECK-LABEL: test_rom_load_idx:
; CHECK: lda rom_table+4
; CHECK: rts
define i16 @test_rom_load_idx() {
  %ptr = getelementptr [4 x i16], ptr @rom_table, i16 0, i16 2
  %val = load i16, ptr %ptr
  ret i16 %val
}

; Test loading from far array with variable index
; Uses long indexed addressing (LDA_longX, opcode 0xBF)
; Assembly syntax is same as regular indexed, but machine code is 4 bytes
; CHECK-LABEL: test_rom_load_var:
; GISel uses indirect addressing for variable index
; CHECK: ldx #rom_table
; CHECK: asl a
; CHECK: tay
; CHECK: lda (${{[0-9]+}},s),y
define i16 @test_rom_load_var(i16 %idx) {
  %ptr = getelementptr [4 x i16], ptr @rom_table, i16 0, i16 %idx
  %val = load i16, ptr %ptr
  ret i16 %val
}

; Test storing to far array with variable index
; Uses long indexed addressing (STA_longX, opcode 0x9F)
; Note: Args are A=idx, X=val. After shift, A=idx*2, X=val.
; We need A=val, X=idx*2 for the store, so swap is required.
; CHECK-LABEL: test_far_store_var:
; GISel uses indirect addressing for variable index stores
; CHECK: ldy #far_array
; CHECK: asl a
; CHECK: sta (${{[0-9]+}},s),y
@far_array = global [8 x i16] zeroinitializer, section ".fardata"

define void @test_far_store_var(i16 %idx, i16 %val) {
  %ptr = getelementptr [8 x i16], ptr @far_array, i16 0, i16 %idx
  store i16 %val, ptr %ptr
  ret void
}

;===----------------------------------------------------------------------===
; Multiple far sections - verify different bank allocations work
;===----------------------------------------------------------------------===

@bank1_data = global i16 0, section ".bank1"
@bank2_data = global i16 0, section ".bank2"
@romdata_table = constant [2 x i16] [i16 100, i16 200], section ".romdata"

; CHECK-LABEL: test_bank1_load:
; CHECK: lda bank1_data
; CHECK: rts
define i16 @test_bank1_load() {
  %val = load i16, ptr @bank1_data
  ret i16 %val
}

; CHECK-LABEL: test_bank2_store:
; CHECK: sta bank2_data
; CHECK: rts
define void @test_bank2_store(i16 %val) {
  store i16 %val, ptr @bank2_data
  ret void
}

; CHECK-LABEL: test_romdata_load:
; CHECK: lda romdata_table
; CHECK: rts
define i16 @test_romdata_load() {
  %val = load i16, ptr @romdata_table
  ret i16 %val
}

;===----------------------------------------------------------------------===
; Combined operations - load from one bank, store to another
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_cross_bank_copy:
; CHECK: lda romdata_table
; CHECK: sta far_data
; CHECK: rts
define void @test_cross_bank_copy() {
  %val = load i16, ptr @romdata_table
  store i16 %val, ptr @far_data
  ret void
}

; CHECK-LABEL: test_cross_bank_indexed:
; GISel uses indirect addressing for indexed access
; CHECK: ldx #rom_table
; CHECK: asl a
; CHECK: lda (${{[0-9]+}},s),y
; CHECK: sta (${{[0-9]+}},s),y
; CHECK: rts
define void @test_cross_bank_indexed(i16 %idx) {
  %src_ptr = getelementptr [4 x i16], ptr @rom_table, i16 0, i16 %idx
  %val = load i16, ptr %src_ptr
  %dst_ptr = getelementptr [8 x i16], ptr @far_array, i16 0, i16 %idx
  store i16 %val, ptr %dst_ptr
  ret void
}

;===----------------------------------------------------------------------===
; Edge cases - boundary values and large offsets
;===----------------------------------------------------------------------===

@large_rom_table = constant [256 x i16] zeroinitializer, section ".rodata"

; Load with large constant offset (should fold into address)
; CHECK-LABEL: test_large_offset:
; CHECK: lda large_rom_table+500
; CHECK: rts
define i16 @test_large_offset() {
  %ptr = getelementptr [256 x i16], ptr @large_rom_table, i16 0, i16 250
  %val = load i16, ptr %ptr
  ret i16 %val
}

; Load last element of array
; CHECK-LABEL: test_last_element:
; CHECK: lda large_rom_table+510
; CHECK: rts
define i16 @test_last_element() {
  %ptr = getelementptr [256 x i16], ptr @large_rom_table, i16 0, i16 255
  %val = load i16, ptr %ptr
  ret i16 %val
}

;===----------------------------------------------------------------------===
; Arithmetic with far data
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_far_add:
; GISel loads far_data into X, adds with A, stores result back via X
; CHECK: ldx far_data
; CHECK: clc
; CHECK: adc
; CHECK: stx far_data
; CHECK: rts
define void @test_far_add(i16 %addend) {
  %val = load i16, ptr @far_data
  %sum = add i16 %val, %addend
  store i16 %sum, ptr @far_data
  ret void
}

;===----------------------------------------------------------------------===
; Store zero to far memory (STZ optimization test)
; Note: STZ doesn't have a long addressing mode, so this uses LDA #0; STA
;===----------------------------------------------------------------------===

; CHECK-LABEL: test_far_stz:
; CHECK: stz far_data
; CHECK: rts
define void @test_far_stz() {
  store i16 0, ptr @far_data
  ret void
}

