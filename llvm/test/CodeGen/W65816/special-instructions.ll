; RUN: llc -march=w65816 < %s | FileCheck %s
; Test W65816 special instructions via inline assembly
;
; These instructions don't have automatic selection patterns but are
; available for direct use when needed.

target datalayout = "e-m:e-p:16:16-i8:8-i16:16-n8:16-S16"
target triple = "w65816-unknown-none"

;===----------------------------------------------------------------------===;
; BIT - Bit Test (sets flags without modifying A)
; Sets N and V from memory, Z from A AND memory
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_bit_immediate:
; CHECK: bit #255
define void @test_bit_immediate() {
  call void asm sideeffect "bit #$$00ff", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; XBA - Exchange B and A (swap high and low bytes of A)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_xba:
; CHECK: xba
define i16 @test_xba() {
  %result = call i16 asm sideeffect "xba", "=r"()
  ret i16 %result
}

;===----------------------------------------------------------------------===;
; TXY/TYX - Transfer between index registers
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_txy:
; CHECK: txy
define void @test_txy() {
  call void asm sideeffect "txy", ""()
  ret void
}

; CHECK-LABEL: test_tyx:
; CHECK: tyx
define void @test_tyx() {
  call void asm sideeffect "tyx", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; PEA - Push Effective Absolute Address
; Pushes a 16-bit value onto the stack
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_pea:
; CHECK: pea $1234
define void @test_pea() {
  call void asm sideeffect "pea $$1234", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; SEP/REP - Set/Reset Processor Status Bits
; Used for switching between 8-bit and 16-bit modes
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_sep:
; Set M flag (8-bit accumulator)
; CHECK: sep #32
define void @test_sep() {
  call void asm sideeffect "sep #32", ""()
  ret void
}

; CHECK-LABEL: test_rep:
; Clear M flag (16-bit accumulator)
; CHECK: rep #32
define void @test_rep() {
  call void asm sideeffect "rep #32", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; CLC/SEC - Clear/Set Carry Flag
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_clc:
; CHECK: clc
define void @test_clc() {
  call void asm sideeffect "clc", ""()
  ret void
}

; CHECK-LABEL: test_sec:
; CHECK: sec
define void @test_sec() {
  call void asm sideeffect "sec", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; NOP - No Operation
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_nop:
; CHECK: nop
define void @test_nop() {
  call void asm sideeffect "nop", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; WAI - Wait for Interrupt
; Stops processor until an interrupt occurs
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_wai:
; CHECK: wai
define void @test_wai() {
  call void asm sideeffect "wai", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; WDM - Reserved for future expansion (acts as 2-byte NOP)
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_wdm:
; CHECK: wdm
define void @test_wdm() {
  call void asm sideeffect "wdm #0", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; PHB/PLB - Push/Pull Data Bank Register
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_phb_plb:
; CHECK: phb
; CHECK: plb
define void @test_phb_plb() {
  call void asm sideeffect "phb\0Aplb", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; PHD/PLD - Push/Pull Direct Page Register
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_phd_pld:
; CHECK: phd
; CHECK: pld
define void @test_phd_pld() {
  call void asm sideeffect "phd\0Apld", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; PHK - Push Program Bank Register
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_phk:
; CHECK: phk
define void @test_phk() {
  call void asm sideeffect "phk", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; TCD/TDC - Transfer 16-bit A to/from Direct Page
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_tcd:
; CHECK: tcd
define void @test_tcd() {
  call void asm sideeffect "tcd", ""()
  ret void
}

; CHECK-LABEL: test_tdc:
; CHECK: tdc
define void @test_tdc() {
  call void asm sideeffect "tdc", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; TCS/TSC - Transfer 16-bit A to/from Stack Pointer
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_tcs:
; CHECK: tcs
define void @test_tcs() {
  call void asm sideeffect "tcs", ""()
  ret void
}

; CHECK-LABEL: test_tsc:
; CHECK: tsc
define void @test_tsc() {
  call void asm sideeffect "tsc", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; LDX dp,Y - Load X from Direct Page indexed by Y
; Opcode: $B6
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_ldx_dpy:
; CHECK: ldx ${{[0-9a-f]+}},y
define void @test_ldx_dpy() {
  call void asm sideeffect "ldx 16,y", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; LDY dp,X - Load Y from Direct Page indexed by X
; Opcode: $B4
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_ldy_dpx:
; CHECK: ldy ${{[0-9a-f]+}},x
define void @test_ldy_dpx() {
  call void asm sideeffect "ldy 32,x", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; Long (24-bit) Addressing Instructions via Inline Assembly
; These access memory across bank boundaries
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_lda_long:
; CHECK: lda $123456
define void @test_lda_long() {
  call void asm sideeffect "lda $$123456", ""()
  ret void
}

; CHECK-LABEL: test_sta_long:
; CHECK: sta $654321
define void @test_sta_long() {
  call void asm sideeffect "sta $$654321", ""()
  ret void
}

; CHECK-LABEL: test_lda_longx:
; CHECK: lda $100000,x
define void @test_lda_longx() {
  call void asm sideeffect "lda $$100000,x", ""()
  ret void
}

; CHECK-LABEL: test_sta_longx:
; CHECK: sta $200000,x
define void @test_sta_longx() {
  call void asm sideeffect "sta $$200000,x", ""()
  ret void
}

;===----------------------------------------------------------------------===;
; DP Indirect Long Addressing - 24-bit pointer through direct page
; LDA [$dp] - Load through 24-bit pointer at dp
; LDA [$dp],Y - Load through 24-bit pointer + Y offset
;===----------------------------------------------------------------------===;

; CHECK-LABEL: test_lda_dp_ind_long:
; CHECK: lda [$10]
define void @test_lda_dp_ind_long() {
  call void asm sideeffect "lda [$$10]", ""()
  ret void
}

; CHECK-LABEL: test_lda_dp_ind_long_y:
; CHECK: lda [$20],y
define void @test_lda_dp_ind_long_y() {
  call void asm sideeffect "lda [$$20],y", ""()
  ret void
}

; CHECK-LABEL: test_sta_dp_ind_long:
; CHECK: sta [$30]
define void @test_sta_dp_ind_long() {
  call void asm sideeffect "sta [$$30]", ""()
  ret void
}

; CHECK-LABEL: test_sta_dp_ind_long_y:
; CHECK: sta [$40],y
define void @test_sta_dp_ind_long_y() {
  call void asm sideeffect "sta [$$40],y", ""()
  ret void
}

