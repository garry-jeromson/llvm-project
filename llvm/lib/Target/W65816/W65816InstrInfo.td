//===-- W65816InstrInfo.td - W65816 Instruction defs --------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes the W65816 instructions in TableGen format.
//
//===----------------------------------------------------------------------===//

include "W65816InstrFormats.td"

//===----------------------------------------------------------------------===//
// W65816 AsmOperandClass Definitions (for assembly parsing)
//===----------------------------------------------------------------------===//

// Immediate operand classes
def Imm8AsmOperand : AsmOperandClass {
  let Name = "Imm8";
  let RenderMethod = "addImmOperands";
}

def Imm16AsmOperand : AsmOperandClass {
  let Name = "Imm16";
  let RenderMethod = "addImmOperands";
}

// Address operand classes
def Addr16AsmOperand : AsmOperandClass {
  let Name = "Addr16";
  let RenderMethod = "addImmOperands";
}

def Addr24AsmOperand : AsmOperandClass {
  let Name = "Addr24";
  let RenderMethod = "addImmOperands";
}

def Addr8DPAsmOperand : AsmOperandClass {
  let Name = "Addr8DP";
  let RenderMethod = "addImmOperands";
}

// Branch target operand classes
def BrTarget8AsmOperand : AsmOperandClass {
  let Name = "BrTarget8";
  let RenderMethod = "addImmOperands";
}

def BrTarget16AsmOperand : AsmOperandClass {
  let Name = "BrTarget16";
  let RenderMethod = "addImmOperands";
}

// Indirect addressing operand classes (with parentheses/brackets as part of operand)
// These operand classes include the parentheses/brackets in their syntax
// The parser methods will handle consuming these delimiters

def IndirectAddr16AsmOperand : AsmOperandClass {
  let Name = "IndirectAddr16";
  let RenderMethod = "addImmOperands";
  let ParserMethod = "parseIndirectAddr";
}

def IndirectXAddr16AsmOperand : AsmOperandClass {
  let Name = "IndirectXAddr16";
  let RenderMethod = "addImmOperands";
  let ParserMethod = "parseIndirectXAddr";
}

def IndirectLongAddr16AsmOperand : AsmOperandClass {
  let Name = "IndirectLongAddr16";
  let RenderMethod = "addImmOperands";
  let ParserMethod = "parseIndirectLongAddr";
}

def IndirectDPAsmOperand : AsmOperandClass {
  let Name = "IndirectDP";
  let RenderMethod = "addImmOperands";
  let ParserMethod = "parseIndirectDP";
}

def IndirectDPYAsmOperand : AsmOperandClass {
  let Name = "IndirectDPY";
  let RenderMethod = "addImmOperands";
  let ParserMethod = "parseIndirectDPY";
}

def IndirectDPLongAsmOperand : AsmOperandClass {
  let Name = "IndirectDPLong";
  let RenderMethod = "addImmOperands";
  let ParserMethod = "parseIndirectDPLong";
}

def IndirectDPLongYAsmOperand : AsmOperandClass {
  let Name = "IndirectDPLongY";
  let RenderMethod = "addImmOperands";
  let ParserMethod = "parseIndirectDPLongY";
}

def StackRelIndirectYAsmOperand : AsmOperandClass {
  let Name = "StackRelIndirectY";
  let RenderMethod = "addImmOperands";
  let ParserMethod = "parseStackRelIndirectY";
}

def IndexedIndirectDPAsmOperand : AsmOperandClass {
  let Name = "IndexedIndirectDP";
  let RenderMethod = "addImmOperands";
  let ParserMethod = "parseIndexedIndirectDP";
}

//===----------------------------------------------------------------------===//
// W65816 Operand Definitions
//===----------------------------------------------------------------------===//

// 8-bit immediate operand
def imm8 : Operand<i8> {
  let ParserMatchClass = Imm8AsmOperand;
}

// 16-bit immediate operand
def imm16 : Operand<i16> {
  let ParserMatchClass = Imm16AsmOperand;
}

// 24-bit long address operand (stored in i32, high 8 bits ignored)
// Used for absolute long addressing to access any bank
def addr24 : Operand<i32> {
  let PrintMethod = "printLongAddr";
  let ParserMatchClass = Addr24AsmOperand;
}

// 16-bit absolute address operand
def addr16 : Operand<i16> {
  let PrintMethod = "printAddrModeMemSrc";
  let ParserMatchClass = Addr16AsmOperand;
}

// 8-bit direct page address operand
def addr8dp : Operand<i8> {
  let PrintMethod = "printAddrModeMemSrc";
  let ParserMatchClass = Addr8DPAsmOperand;
}

// PC-relative offset for branches
def brtarget8 : Operand<OtherVT> {
  let EncoderMethod = "encodePCRelImm";
  let ParserMatchClass = BrTarget8AsmOperand;
}

def brtarget16 : Operand<OtherVT> {
  let EncoderMethod = "encodePCRelImm16";
  let ParserMatchClass = BrTarget16AsmOperand;
}

//===----------------------------------------------------------------------===//
// Indirect Addressing Operand Definitions
//===----------------------------------------------------------------------===//
// These operands include the parentheses/brackets in their syntax.
// The parser methods consume the delimiters, and print methods add them back.

// Indirect 16-bit address: (addr)
def indaddr16 : Operand<i16> {
  let PrintMethod = "printIndirectAddr";
  let ParserMatchClass = IndirectAddr16AsmOperand;
}

// Indexed Indirect 16-bit address: (addr,x)
def indXaddr16 : Operand<i16> {
  let PrintMethod = "printIndirectXAddr";
  let ParserMatchClass = IndirectXAddr16AsmOperand;
}

// Indirect Long 16-bit address: [addr]
def indLaddr16 : Operand<i16> {
  let PrintMethod = "printIndirectLongAddr";
  let ParserMatchClass = IndirectLongAddr16AsmOperand;
}

// Indirect Direct Page: (dp)
def inddp : Operand<i8> {
  let PrintMethod = "printIndirectDP";
  let ParserMatchClass = IndirectDPAsmOperand;
}

// Indirect Direct Page Indexed Y: (dp),y
def inddpY : Operand<i8> {
  let PrintMethod = "printIndirectDPY";
  let ParserMatchClass = IndirectDPYAsmOperand;
}

// Indirect Long Direct Page: [dp]
def indLdp : Operand<i8> {
  let PrintMethod = "printIndirectDPLong";
  let ParserMatchClass = IndirectDPLongAsmOperand;
}

// Indirect Long Direct Page Indexed Y: [dp],y
def indLdpY : Operand<i8> {
  let PrintMethod = "printIndirectDPLongY";
  let ParserMatchClass = IndirectDPLongYAsmOperand;
}

// Stack Relative Indirect Indexed Y: (offset,s),y
def srIndY : Operand<i8> {
  let PrintMethod = "printStackRelIndirectY";
  let ParserMatchClass = StackRelIndirectYAsmOperand;
}

// Indexed Indirect Direct Page: (dp,x)
def indXdp : Operand<i8> {
  let PrintMethod = "printIndexedIndirectDP";
  let ParserMatchClass = IndexedIndirectDPAsmOperand;
}

//===----------------------------------------------------------------------===//
// W65816 Subtarget Feature Predicates
//===----------------------------------------------------------------------===//

// Accumulator width predicates (M flag)
def UseAcc8Bit : Predicate<"Subtarget->uses8BitAccumulator()">,
                 AssemblerPredicate<(all_of FeatureAcc8Bit), "8-bit accumulator">;
def UseAcc16Bit : Predicate<"!Subtarget->uses8BitAccumulator()">,
                  AssemblerPredicate<(all_of (not FeatureAcc8Bit)), "16-bit accumulator">;

// Index register width predicates (X flag)
def UseIdx8Bit : Predicate<"Subtarget->uses8BitIndex()">,
                 AssemblerPredicate<(all_of FeatureIdx8Bit), "8-bit index registers">;
def UseIdx16Bit : Predicate<"!Subtarget->uses8BitIndex()">,
                  AssemblerPredicate<(all_of (not FeatureIdx8Bit)), "16-bit index registers">;

//===----------------------------------------------------------------------===//
// W65816 Complex Pattern Definitions
//===----------------------------------------------------------------------===//

// Match an address
def addr : ComplexPattern<i16, 1, "SelectAddr", [], []>;

//===----------------------------------------------------------------------===//
// W65816 Instruction Pattern Fragments
//===----------------------------------------------------------------------===//

// Load operations
def load_op : PatFrag<(ops node:$ptr), (load node:$ptr)>;

// Store operations
def store_op : PatFrag<(ops node:$val, node:$ptr), (store node:$val, node:$ptr)>;

//===----------------------------------------------------------------------===//
// W65816 Target-Specific SDNode Definitions
//===----------------------------------------------------------------------===//

// SDT for call instruction
def SDT_W65816Call : SDTypeProfile<0, -1, [SDTCisVT<0, i16>]>;

def retflag : SDNode<"W65816ISD::RET_FLAG", SDTNone,
                     [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def W65816call : SDNode<"W65816ISD::CALL", SDT_W65816Call,
                        [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue, SDNPVariadic]>;

def W65816Wrapper : SDNode<"W65816ISD::WRAPPER", SDTIntUnaryOp>;

// Compare - takes two operands and produces flag output
def SDT_W65816Cmp : SDTypeProfile<0, 2, [SDTCisSameAs<0, 1>]>;
def W65816cmp : SDNode<"W65816ISD::CMP", SDT_W65816Cmp, [SDNPOutGlue]>;

// Select CC - takes trueVal, falseVal, condCode, and flag input
def SDT_W65816SelectCC : SDTypeProfile<1, 3, [SDTCisSameAs<0, 1>, SDTCisSameAs<1, 2>]>;
def W65816selectcc : SDNode<"W65816ISD::SELECT_CC", SDT_W65816SelectCC,
                            [SDNPInGlue]>;

// Branch conditional - takes chain, dest, condcode, and flag input
def SDT_W65816BrCond : SDTypeProfile<0, 2, [SDTCisVT<0, OtherVT>, SDTCisVT<1, i8>]>;
def W65816brcond : SDNode<"W65816ISD::BRCOND", SDT_W65816BrCond,
                          [SDNPHasChain, SDNPInGlue]>;

//===----------------------------------------------------------------------===//
// Accumulator Load/Store Instructions (16-bit mode, M=0)
//===----------------------------------------------------------------------===//

// LDA - Load Accumulator (16-bit)
let Defs = [P], Predicates = [UseAcc16Bit] in {
  // LDA immediate 16-bit
  // Note: operand name must match encoding field 'addr'
  def LDA_imm16 : W65816Inst24<0xA9, (outs ACC16:$dst), (ins imm16:$addr),
                               "lda\t#$addr",
                               [(set ACC16:$dst, imm:$addr)]>;

  // LDA absolute
  def LDA_abs : W65816Inst24<0xAD, (outs ACC16:$dst), (ins addr16:$addr),
                             "lda\t$addr",
                             [(set ACC16:$dst, (load addr:$addr))]>;
}

// STA - Store Accumulator (16-bit)
let Predicates = [UseAcc16Bit] in
def STA_abs : W65816Inst24<0x8D, (outs), (ins ACC16:$src, addr16:$addr),
                           "sta\t$addr",
                           [(store ACC16:$src, addr:$addr)]>;

//===----------------------------------------------------------------------===//
// Accumulator Load/Store Instructions (8-bit mode, M=1)
//===----------------------------------------------------------------------===//

let Defs = [P], Predicates = [UseAcc8Bit] in {
  // LDA immediate 8-bit (in 8-bit acc mode)
  // Note: Same opcode as 16-bit, but immediate is only 1 byte
  def LDA_imm8_m8 : W65816Inst16<0xA9, (outs ACC8:$dst), (ins imm8:$imm),
                                 "lda\t#$imm",
                                 [(set ACC8:$dst, imm:$imm)]>;

  // LDA absolute (8-bit mode) - uses 3-byte format (opcode + 16-bit address)
  def LDA_abs_m8 : W65816Inst24<0xAD, (outs ACC8:$dst), (ins addr16:$addr),
                                "lda\t$addr",
                                [(set ACC8:$dst, (load addr:$addr))]>;
}

// STA absolute (8-bit mode) - uses 3-byte format (opcode + 16-bit address)
let Predicates = [UseAcc8Bit] in
def STA_abs_m8 : W65816Inst24<0x8D, (outs), (ins ACC8:$src, addr16:$addr),
                              "sta\t$addr",
                              [(store ACC8:$src, addr:$addr)]>;

// Stack-relative addressing modes for load/store
let Defs = [P] in {
  // LDA Stack Relative - lda offset,s
  // Note: operand name must match encoding field 'imm'
  def LDA_sr : W65816Inst16<0xA3, (outs ACC16:$dst), (ins imm8:$imm),
                            "lda\t$imm,s", []>;
}

// STA Stack Relative - sta offset,s
// Note: operand name must match encoding field 'imm'
def STA_sr : W65816Inst16<0x83, (outs), (ins ACC16:$src, imm8:$imm),
                          "sta\t$imm,s", []>;

//===----------------------------------------------------------------------===//
// Spill/Reload Pseudo Instructions
//===----------------------------------------------------------------------===//
// These pseudos are used by the register allocator for spilling.
// They output GPR16 (not ACC16) to give the allocator flexibility.
// Expanded after register allocation to actual LDA_sr/STA_sr + transfers.

// RELOAD_GPR16 - Load from stack slot to any GPR16 register
// Expanded to: LDA_sr (+ TAX/TAY if dest is X/Y)
let mayLoad = 1, hasSideEffects = 0, Defs = [P] in {
  def RELOAD_GPR16 : Pseudo<(outs GPR16:$dst), (ins i16imm:$fi),
                            "# RELOAD_GPR16 $dst, $fi", []>;
}

// SPILL_GPR16 - Store from any GPR16 register to stack slot
// Expanded to: (TXA/TYA if src is X/Y +) STA_sr
let mayStore = 1, hasSideEffects = 0 in {
  def SPILL_GPR16 : Pseudo<(outs), (ins GPR16:$src, i16imm:$fi),
                           "# SPILL_GPR16 $src, $fi", []>;
}

//===----------------------------------------------------------------------===//
// Indexed Addressing Mode Instructions (abs,X and abs,Y)
//===----------------------------------------------------------------------===//

let Defs = [P] in {
  // LDA absolute,X - Load A from (absolute address + X)
  def LDA_absX : W65816Inst24<0xBD, (outs ACC16:$dst), (ins addr16:$addr),
                              "lda\t$addr,x", []> {
    let Uses = [X];
  }

  // LDA absolute,Y - Load A from (absolute address + Y)
  def LDA_absY : W65816Inst24<0xB9, (outs ACC16:$dst), (ins addr16:$addr),
                              "lda\t$addr,y", []> {
    let Uses = [Y];
  }
}

// STA absolute,X - Store A to (absolute address + X)
def STA_absX : W65816Inst24<0x9D, (outs), (ins ACC16:$src, addr16:$addr),
                            "sta\t$addr,x", []> {
  let Uses = [X];
}

// STA absolute,Y - Store A to (absolute address + Y)
def STA_absY : W65816Inst24<0x99, (outs), (ins ACC16:$src, addr16:$addr),
                            "sta\t$addr,y", []> {
  let Uses = [Y];
}

//===----------------------------------------------------------------------===//
// Direct Page Addressing (8-bit address, faster for zero page)
//===----------------------------------------------------------------------===//

let Defs = [P] in {
  // LDA direct page - 2-byte instruction, faster than absolute
  // Note: operand name must match encoding field 'imm'
  def LDA_dp : W65816Inst16<0xA5, (outs ACC16:$dst), (ins addr8dp:$imm),
                            "lda\t$imm", []>;

  // LDA direct page,X
  def LDA_dpX : W65816Inst16<0xB5, (outs ACC16:$dst), (ins addr8dp:$imm),
                             "lda\t$imm,x", []> {
    let Uses = [X];
  }

  // LDA direct page,Y (actually indirect - LDA (dp),Y)
  // Note: There's no LDA dp,Y - only LDA (dp),Y which is indirect
}

// STA direct page
def STA_dp : W65816Inst16<0x85, (outs), (ins ACC16:$src, addr8dp:$imm),
                          "sta\t$imm", []>;

// STA direct page,X
def STA_dpX : W65816Inst16<0x95, (outs), (ins ACC16:$src, addr8dp:$imm),
                           "sta\t$imm,x", []> {
  let Uses = [X];
}

//===----------------------------------------------------------------------===//
// Indirect Addressing Modes
//===----------------------------------------------------------------------===//

let Defs = [P], mayLoad = 1 in {
  // LDA (dp) - Load from address stored at direct page location
  def LDA_dpInd : W65816Inst16<0xB2, (outs ACC16:$dst), (ins inddp:$imm),
                               "lda\t$imm", []>;

  // LDA (dp),Y - Load from (address at dp) + Y
  def LDA_dpIndY : W65816Inst16<0xB1, (outs ACC16:$dst), (ins inddpY:$imm),
                                "lda\t$imm", []> {
    let Uses = [Y];
  }

  // LDA (dp,X) - Load from address at (dp + X)
  def LDA_dpXInd : W65816Inst16<0xA1, (outs ACC16:$dst), (ins indXdp:$imm),
                                "lda\t$imm", []> {
    let Uses = [X];
  }
}

let mayStore = 1 in {
  // STA (dp) - Store to address stored at direct page location
  def STA_dpInd : W65816Inst16<0x92, (outs), (ins ACC16:$src, inddp:$imm),
                               "sta\t$imm", []>;

  // STA (dp),Y - Store to (address at dp) + Y
  def STA_dpIndY : W65816Inst16<0x91, (outs), (ins ACC16:$src, inddpY:$imm),
                                "sta\t$imm", []> {
    let Uses = [Y];
  }

  // STA (dp,X) - Store to address at (dp + X)
  def STA_dpXInd : W65816Inst16<0x81, (outs), (ins ACC16:$src, indXdp:$imm),
                                "sta\t$imm", []> {
    let Uses = [X];
  }
}

//===----------------------------------------------------------------------===//
// Stack-Relative Indirect Indexed Addressing
//===----------------------------------------------------------------------===//

// These are crucial for dereferencing pointers stored on the stack.
// The pointer is at (SP + offset), and Y is added for array indexing.

let Defs = [P] in {
  // LDA (offset,S),Y - Load from (address at SP+offset) + Y
  // This is the key instruction for *ptr or ptr[i] where ptr is a local var
  def LDA_srIndY : W65816Inst16<0xB3, (outs ACC16:$dst), (ins srIndY:$imm),
                                "lda\t$imm", []> {
    let Uses = [Y];
    let mayLoad = 1;
  }
}

// STA (offset,S),Y - Store to (address at SP+offset) + Y
def STA_srIndY : W65816Inst16<0x93, (outs), (ins ACC16:$src, srIndY:$imm),
                              "sta\t$imm", []> {
  let Uses = [Y];
  let mayStore = 1;
}

//===----------------------------------------------------------------------===//
// Long (24-bit) Addressing Instructions
// These instructions access the full 24-bit address space (16MB)
// Used for cross-bank memory access on SNES and similar systems
//===----------------------------------------------------------------------===//

let Defs = [P] in {
  // LDA absolute long - Load from 24-bit address
  // Opcode: 0xAF, 4 bytes: [AF][addr_lo][addr_mid][addr_hi]
  def LDA_long : W65816Inst32<0xAF, (outs ACC16:$dst), (ins addr24:$addr),
                              "lda\t$addr", []> {
    let mayLoad = 1;
  }

  // LDA absolute long,X - Load from 24-bit address + X
  // Opcode: 0xBF, 4 bytes
  def LDA_longX : W65816Inst32<0xBF, (outs ACC16:$dst), (ins addr24:$addr),
                               "lda\t$addr,x", []> {
    let Uses = [X];
    let mayLoad = 1;
  }
}

// STA absolute long - Store to 24-bit address
// Opcode: 0x8F, 4 bytes
def STA_long : W65816Inst32<0x8F, (outs), (ins ACC16:$src, addr24:$addr),
                            "sta\t$addr", []> {
  let mayStore = 1;
}

// STA absolute long,X - Store to 24-bit address + X
// Opcode: 0x9F, 4 bytes
def STA_longX : W65816Inst32<0x9F, (outs), (ins ACC16:$src, addr24:$addr),
                             "sta\t$addr,x", []> {
  let Uses = [X];
  let mayStore = 1;
}

//===----------------------------------------------------------------------===//
// Direct Page Indirect Long Addressing
// The DP location contains a 24-bit pointer (3 bytes)
// Syntax uses square brackets: LDA [$dp] instead of LDA ($dp)
//===----------------------------------------------------------------------===//

let Defs = [P] in {
  // LDA [$dp] - Load through 24-bit pointer at direct page location
  // Opcode: 0xA7, 2 bytes: [A7][dp_offset]
  def LDA_dpIndLong : W65816Inst16<0xA7, (outs ACC16:$dst), (ins indLdp:$imm),
                                   "lda\t$imm", []> {
    let mayLoad = 1;
  }

  // LDA [$dp],Y - Load through 24-bit pointer + Y offset
  // Opcode: 0xB7, 2 bytes
  def LDA_dpIndLongY : W65816Inst16<0xB7, (outs ACC16:$dst), (ins indLdpY:$imm),
                                    "lda\t$imm", []> {
    let Uses = [Y];
    let mayLoad = 1;
  }
}

// STA [$dp] - Store through 24-bit pointer at direct page location
// Opcode: 0x87, 2 bytes
def STA_dpIndLong : W65816Inst16<0x87, (outs), (ins ACC16:$src, indLdp:$imm),
                                 "sta\t$imm", []> {
  let mayStore = 1;
}

// STA [$dp],Y - Store through 24-bit pointer + Y offset
// Opcode: 0x97, 2 bytes
def STA_dpIndLongY : W65816Inst16<0x97, (outs), (ins ACC16:$src, indLdpY:$imm),
                                  "sta\t$imm", []> {
  let Uses = [Y];
  let mayStore = 1;
}

//===----------------------------------------------------------------------===//
// Index Register Load/Store Instructions (16-bit mode, X=0)
//===----------------------------------------------------------------------===//

let Defs = [P], Predicates = [UseIdx16Bit] in {
  // LDX - Load X Register (16-bit)
  // Note: output is X register class to enable pattern matching for X reg
  def LDX_imm16 : W65816Inst24<0xA2, (outs X16:$dst), (ins imm16:$addr),
                               "ldx\t#$addr",
                               [(set X16:$dst, imm:$addr)]>;

  def LDX_abs : W65816Inst24<0xAE, (outs X16:$dst), (ins addr16:$addr),
                             "ldx\t$addr",
                             [(set X16:$dst, (load addr:$addr))]>;

  // LDY - Load Y Register (16-bit)
  def LDY_imm16 : W65816Inst24<0xA0, (outs Y16:$dst), (ins imm16:$addr),
                               "ldy\t#$addr",
                               [(set Y16:$dst, imm:$addr)]>;

  def LDY_abs : W65816Inst24<0xAC, (outs IDX16:$dst), (ins addr16:$addr),
                             "ldy\t$addr", []>;
}

//===----------------------------------------------------------------------===//
// Index Register Load/Store Instructions (8-bit mode, X=1)
//===----------------------------------------------------------------------===//

let Defs = [P], Predicates = [UseIdx8Bit] in {
  // LDX - Load X Register (8-bit)
  // Same opcode as 16-bit, but immediate is only 1 byte
  def LDX_imm8_x8 : W65816Inst16<0xA2, (outs IDX8:$dst), (ins imm8:$imm),
                                 "ldx\t#$imm",
                                 [(set IDX8:$dst, imm:$imm)]>;

  // LDX absolute (8-bit index mode) - uses 3-byte format
  def LDX_abs_x8 : W65816Inst24<0xAE, (outs IDX8:$dst), (ins addr16:$addr),
                                "ldx\t$addr",
                                [(set IDX8:$dst, (load addr:$addr))]>;

  // LDY - Load Y Register (8-bit)
  def LDY_imm8_x8 : W65816Inst16<0xA0, (outs IDX8:$dst), (ins imm8:$imm),
                                 "ldy\t#$imm",
                                 [(set IDX8:$dst, imm:$imm)]>;

  // LDY absolute (8-bit index mode) - uses 3-byte format
  def LDY_abs_x8 : W65816Inst24<0xAC, (outs IDX8:$dst), (ins addr16:$addr),
                                "ldy\t$addr",
                                [(set IDX8:$dst, (load addr:$addr))]>;
}

// STX - Store X Register (16-bit mode)
let Predicates = [UseIdx16Bit] in
def STX_abs : W65816Inst24<0x8E, (outs), (ins IDX16:$src, addr16:$addr),
                           "stx\t$addr",
                           []>;

// STX - Store X Register to Direct Page (16-bit mode)
let Predicates = [UseIdx16Bit], mayStore = 1 in
def STX_dp : W65816Inst16<0x86, (outs), (ins IDX16:$src, addr8dp:$imm),
                          "stx\t$imm",
                          []>;

// STY - Store Y Register (16-bit mode)
let Predicates = [UseIdx16Bit] in
def STY_abs : W65816Inst24<0x8C, (outs), (ins IDX16:$src, addr16:$addr),
                           "sty\t$addr",
                           []>;

// STY - Store Y Register to Direct Page (16-bit mode)
let Predicates = [UseIdx16Bit], mayStore = 1 in
def STY_dp : W65816Inst16<0x84, (outs), (ins IDX16:$src, addr8dp:$imm),
                          "sty\t$imm",
                          []>;

// STX - Store X Register (8-bit mode) - uses 3-byte format
let Predicates = [UseIdx8Bit] in
def STX_abs_x8 : W65816Inst24<0x8E, (outs), (ins IDX8:$src, addr16:$addr),
                              "stx\t$addr",
                              []>;

// STY - Store Y Register (8-bit mode) - uses 3-byte format
let Predicates = [UseIdx8Bit] in
def STY_abs_x8 : W65816Inst24<0x8C, (outs), (ins IDX8:$src, addr16:$addr),
                              "sty\t$addr",
                              []>;

//===----------------------------------------------------------------------===//
// STZ - Store Zero to Memory
// Stores zero without needing to load a register first
//===----------------------------------------------------------------------===//

let mayStore = 1, hasSideEffects = 0 in {
  // STZ absolute
  def STZ_abs : W65816Inst24<0x9C, (outs), (ins addr16:$addr),
                             "stz\t$addr", []>;

  // STZ direct page
  def STZ_dp : W65816Inst16<0x64, (outs), (ins addr8dp:$imm),
                            "stz\t$imm", []>;

  // STZ absolute,X
  def STZ_absX : W65816Inst24<0x9E, (outs), (ins addr16:$addr),
                              "stz\t$addr,x", []> {
    let Uses = [X];
  }

  // STZ direct page,X
  def STZ_dpX : W65816Inst16<0x74, (outs), (ins addr8dp:$imm),
                             "stz\t$imm,x", []> {
    let Uses = [X];
  }
}

//===----------------------------------------------------------------------===//
// Arithmetic Instructions
//===----------------------------------------------------------------------===//

let Defs = [P], Uses = [P, A] in {
  // ADC - Add with Carry
  // Note: Uses A implicitly since it's always the source/destination
  def ADC_imm16 : W65816Inst24<0x69, (outs ACC16:$dst),
                               (ins imm16:$addr),
                               "adc\t#$addr", []>;

  def ADC_abs : W65816Inst24<0x6D, (outs ACC16:$dst),
                             (ins addr16:$addr),
                             "adc\t$addr", []>;

  // ADC Stack Relative - adc offset,s
  def ADC_sr : W65816Inst16<0x63, (outs ACC16:$dst),
                            (ins imm8:$imm),
                            "adc\t$imm,s", []>;

  // ADC Direct Page - adc dp
  def ADC_dp : W65816Inst16<0x65, (outs ACC16:$dst),
                            (ins addr8dp:$imm),
                            "adc\t$imm", []>;

  // SBC - Subtract with Carry
  // Note: Uses A implicitly since it's always the source/destination
  def SBC_imm16 : W65816Inst24<0xE9, (outs ACC16:$dst),
                               (ins imm16:$addr),
                               "sbc\t#$addr", []>;

  def SBC_abs : W65816Inst24<0xED, (outs ACC16:$dst),
                             (ins addr16:$addr),
                             "sbc\t$addr", []>;

  // SBC Stack Relative - sbc offset,s
  def SBC_sr : W65816Inst16<0xE3, (outs ACC16:$dst),
                            (ins imm8:$imm),
                            "sbc\t$imm,s", []>;

  // SBC Direct Page - sbc dp
  def SBC_dp : W65816Inst16<0xE5, (outs ACC16:$dst),
                            (ins addr8dp:$imm),
                            "sbc\t$imm", []>;
}

let Defs = [P] in {
  // CMP - Compare Accumulator
  def CMP_imm16 : W65816Inst24<0xC9, (outs), (ins ACC16:$src, imm16:$addr),
                               "cmp\t#$addr", []>;

  def CMP_abs : W65816Inst24<0xCD, (outs), (ins ACC16:$src, addr16:$addr),
                             "cmp\t$addr", []>;

  // CPX - Compare X Register
  // In 16-bit index mode, the immediate is 16-bit
  def CPX_imm8 : W65816Inst16<0xE0, (outs), (ins imm8:$imm),
                              "cpx\t#$imm", []>;

  def CPX_imm16 : W65816Inst24<0xE0, (outs), (ins imm16:$addr),
                               "cpx\t#$addr", []>;

  // CPY - Compare Y Register
  def CPY_imm8 : W65816Inst16<0xC0, (outs), (ins imm8:$imm),
                              "cpy\t#$imm", []>;

  def CPY_imm16 : W65816Inst24<0xC0, (outs), (ins imm16:$addr),
                               "cpy\t#$addr", []>;
}

//===----------------------------------------------------------------------===//
// Logical Instructions
//===----------------------------------------------------------------------===//

let Defs = [P] in {
  // AND - Logical AND
  def AND_imm16 : W65816Inst24<0x29, (outs ACC16:$dst),
                               (ins ACC16:$src, imm16:$addr),
                               "and\t#$addr",
                               [(set ACC16:$dst, (and ACC16:$src, imm:$addr))]> {
    let Constraints = "$src = $dst";
  }

  def AND_abs : W65816Inst24<0x2D, (outs ACC16:$dst),
                             (ins ACC16:$src, addr16:$addr),
                             "and\t$addr",
                             [(set ACC16:$dst, (and ACC16:$src, (load addr:$addr)))]> {
    let Constraints = "$src = $dst";
  }

  // ORA - Logical OR
  def ORA_imm16 : W65816Inst24<0x09, (outs ACC16:$dst),
                               (ins ACC16:$src, imm16:$addr),
                               "ora\t#$addr",
                               [(set ACC16:$dst, (or ACC16:$src, imm:$addr))]> {
    let Constraints = "$src = $dst";
  }

  def ORA_abs : W65816Inst24<0x0D, (outs ACC16:$dst),
                             (ins ACC16:$src, addr16:$addr),
                             "ora\t$addr",
                             [(set ACC16:$dst, (or ACC16:$src, (load addr:$addr)))]> {
    let Constraints = "$src = $dst";
  }

  // EOR - Exclusive OR
  def EOR_imm16 : W65816Inst24<0x49, (outs ACC16:$dst),
                               (ins ACC16:$src, imm16:$addr),
                               "eor\t#$addr",
                               [(set ACC16:$dst, (xor ACC16:$src, imm:$addr))]> {
    let Constraints = "$src = $dst";
  }

  def EOR_abs : W65816Inst24<0x4D, (outs ACC16:$dst),
                             (ins ACC16:$src, addr16:$addr),
                             "eor\t$addr",
                             [(set ACC16:$dst, (xor ACC16:$src, (load addr:$addr)))]> {
    let Constraints = "$src = $dst";
  }

  // Stack-relative versions for register-to-register operations
  def AND_sr : W65816Inst16<0x23, (outs ACC16:$dst),
                            (ins ACC16:$src, imm8:$imm),
                            "and\t$imm,s", []> {
    let Constraints = "$src = $dst";
  }

  def ORA_sr : W65816Inst16<0x03, (outs ACC16:$dst),
                            (ins ACC16:$src, imm8:$imm),
                            "ora\t$imm,s", []> {
    let Constraints = "$src = $dst";
  }

  def EOR_sr : W65816Inst16<0x43, (outs ACC16:$dst),
                            (ins ACC16:$src, imm8:$imm),
                            "eor\t$imm,s", []> {
    let Constraints = "$src = $dst";
  }

  // Direct Page versions for optimized register-to-register operations
  // These are faster than stack-relative (4 cycles vs 5 cycles for DP vs SR)
  def AND_dp : W65816Inst16<0x25, (outs ACC16:$dst),
                            (ins addr8dp:$imm),
                            "and\t$imm", []>;

  def ORA_dp : W65816Inst16<0x05, (outs ACC16:$dst),
                            (ins addr8dp:$imm),
                            "ora\t$imm", []>;

  def EOR_dp : W65816Inst16<0x45, (outs ACC16:$dst),
                            (ins addr8dp:$imm),
                            "eor\t$imm", []>;
}

//===----------------------------------------------------------------------===//
// BIT - Test Memory Bits Against Accumulator
// Sets Z based on (A AND memory), sets N/V from memory bits (except immediate)
//===----------------------------------------------------------------------===//

let Defs = [P], Uses = [A], hasSideEffects = 0 in {
  // BIT immediate - only affects Z flag (N/V unaffected)
  def BIT_imm16 : W65816Inst24<0x89, (outs), (ins imm16:$addr),
                               "bit\t#$addr", []>;

  // BIT absolute - sets Z from AND, N from bit 15, V from bit 14
  def BIT_abs : W65816Inst24<0x2C, (outs), (ins addr16:$addr),
                             "bit\t$addr", []> {
    let mayLoad = 1;
  }

  // BIT direct page
  def BIT_dp : W65816Inst16<0x24, (outs), (ins addr8dp:$imm),
                            "bit\t$imm", []> {
    let mayLoad = 1;
  }

  // BIT absolute,X
  def BIT_absX : W65816Inst24<0x3C, (outs), (ins addr16:$addr),
                              "bit\t$addr,x", []> {
    let mayLoad = 1;
    let Uses = [A, X];
  }

  // BIT direct page,X
  def BIT_dpX : W65816Inst16<0x34, (outs), (ins addr8dp:$imm),
                             "bit\t$imm,x", []> {
    let mayLoad = 1;
    let Uses = [A, X];
  }
}

//===----------------------------------------------------------------------===//
// Shift and Rotate Instructions
//===----------------------------------------------------------------------===//

let Defs = [P] in {
  // ASL - Arithmetic Shift Left (accumulator)
  // Shifts all bits left by 1, bit 7 goes to carry, 0 enters bit 0
  def ASL_A : W65816Inst8<0x0A, (outs ACC16:$dst), (ins ACC16:$src),
                          "asl\ta", []> {
    let Constraints = "$src = $dst";
  }

  // LSR - Logical Shift Right (accumulator)
  // Shifts all bits right by 1, bit 0 goes to carry, 0 enters bit 7
  def LSR_A : W65816Inst8<0x4A, (outs ACC16:$dst), (ins ACC16:$src),
                          "lsr\ta", []> {
    let Constraints = "$src = $dst";
  }

  // ROL - Rotate Left through Carry (accumulator)
  // Shifts left, carry enters bit 0, bit 7 goes to carry
  def ROL_A : W65816Inst8<0x2A, (outs ACC16:$dst), (ins ACC16:$src),
                          "rol\ta", []> {
    let Constraints = "$src = $dst";
    let Uses = [P];  // Uses carry flag
  }

  // ROR - Rotate Right through Carry (accumulator)
  // Shifts right, carry enters bit 7, bit 0 goes to carry
  def ROR_A : W65816Inst8<0x6A, (outs ACC16:$dst), (ins ACC16:$src),
                          "ror\ta", []> {
    let Constraints = "$src = $dst";
    let Uses = [P];  // Uses carry flag
  }
}

//===----------------------------------------------------------------------===//
// Memory Shift and Rotate Instructions
// These modify memory in place (read-modify-write operations)
//===----------------------------------------------------------------------===//

// ASL - Arithmetic Shift Left (memory)
let Defs = [P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  // ASL direct page
  def ASL_dp : W65816Inst16<0x06, (outs), (ins addr8dp:$imm),
                            "asl\t$imm", []>;

  // ASL direct page,X
  def ASL_dpX : W65816Inst16<0x16, (outs), (ins addr8dp:$imm),
                             "asl\t$imm,x", []> {
    let Uses = [X];
  }

  // ASL absolute
  def ASL_abs : W65816Inst24<0x0E, (outs), (ins addr16:$addr),
                             "asl\t$addr", []>;

  // ASL absolute,X
  def ASL_absX : W65816Inst24<0x1E, (outs), (ins addr16:$addr),
                              "asl\t$addr,x", []> {
    let Uses = [X];
  }
}

// LSR - Logical Shift Right (memory)
let Defs = [P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  // LSR direct page
  def LSR_dp : W65816Inst16<0x46, (outs), (ins addr8dp:$imm),
                            "lsr\t$imm", []>;

  // LSR direct page,X
  def LSR_dpX : W65816Inst16<0x56, (outs), (ins addr8dp:$imm),
                             "lsr\t$imm,x", []> {
    let Uses = [X];
  }

  // LSR absolute
  def LSR_abs : W65816Inst24<0x4E, (outs), (ins addr16:$addr),
                             "lsr\t$addr", []>;

  // LSR absolute,X
  def LSR_absX : W65816Inst24<0x5E, (outs), (ins addr16:$addr),
                              "lsr\t$addr,x", []> {
    let Uses = [X];
  }
}

// ROL - Rotate Left through Carry (memory)
let Defs = [P], Uses = [P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  // ROL direct page
  def ROL_dp : W65816Inst16<0x26, (outs), (ins addr8dp:$imm),
                            "rol\t$imm", []>;

  // ROL direct page,X
  def ROL_dpX : W65816Inst16<0x36, (outs), (ins addr8dp:$imm),
                             "rol\t$imm,x", []> {
    let Uses = [P, X];
  }

  // ROL absolute
  def ROL_abs : W65816Inst24<0x2E, (outs), (ins addr16:$addr),
                             "rol\t$addr", []>;

  // ROL absolute,X
  def ROL_absX : W65816Inst24<0x3E, (outs), (ins addr16:$addr),
                              "rol\t$addr,x", []> {
    let Uses = [P, X];
  }
}

// ROR - Rotate Right through Carry (memory)
let Defs = [P], Uses = [P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  // ROR direct page
  def ROR_dp : W65816Inst16<0x66, (outs), (ins addr8dp:$imm),
                            "ror\t$imm", []>;

  // ROR direct page,X
  def ROR_dpX : W65816Inst16<0x76, (outs), (ins addr8dp:$imm),
                             "ror\t$imm,x", []> {
    let Uses = [P, X];
  }

  // ROR absolute
  def ROR_abs : W65816Inst24<0x6E, (outs), (ins addr16:$addr),
                             "ror\t$addr", []>;

  // ROR absolute,X
  def ROR_absX : W65816Inst24<0x7E, (outs), (ins addr16:$addr),
                              "ror\t$addr,x", []> {
    let Uses = [P, X];
  }
}

//===----------------------------------------------------------------------===//
// Increment/Decrement Instructions
//===----------------------------------------------------------------------===//

let Defs = [P] in {
  // INC - Increment Accumulator (no pattern - use INC16 pseudo instead)
  def INC_A : W65816Inst8<0x1A, (outs ACC16:$dst), (ins ACC16:$src),
                          "inc\ta", []> {
    let Constraints = "$src = $dst";
  }

  // DEC - Decrement Accumulator (no pattern - use DEC16 pseudo instead)
  def DEC_A : W65816Inst8<0x3A, (outs ACC16:$dst), (ins ACC16:$src),
                          "dec\ta", []> {
    let Constraints = "$src = $dst";
  }

  // INX - Increment X
  def INX : W65816Inst8<0xE8, (outs), (ins),
                        "inx",
                        []> {
    let Defs = [X, P];
    let Uses = [X];
  }

  // INY - Increment Y
  def INY : W65816Inst8<0xC8, (outs), (ins),
                        "iny",
                        []> {
    let Defs = [Y, P];
    let Uses = [Y];
  }

  // DEX - Decrement X
  def DEX : W65816Inst8<0xCA, (outs), (ins),
                        "dex",
                        []> {
    let Defs = [X, P];
    let Uses = [X];
  }

  // DEY - Decrement Y
  def DEY : W65816Inst8<0x88, (outs), (ins),
                        "dey",
                        []> {
    let Defs = [Y, P];
    let Uses = [Y];
  }
}

//===----------------------------------------------------------------------===//
// Memory Increment/Decrement Instructions (read-modify-write)
//===----------------------------------------------------------------------===//

// INC - Increment (memory)
let Defs = [P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  // INC absolute
  def INC_abs : W65816Inst24<0xEE, (outs), (ins addr16:$addr),
                             "inc\t$addr", []>;

  // INC direct page
  def INC_dp : W65816Inst16<0xE6, (outs), (ins addr8dp:$imm),
                            "inc\t$imm", []>;

  // INC absolute,X
  def INC_absX : W65816Inst24<0xFE, (outs), (ins addr16:$addr),
                              "inc\t$addr,x", []> {
    let Uses = [X];
  }

  // INC direct page,X
  def INC_dpX : W65816Inst16<0xF6, (outs), (ins addr8dp:$imm),
                             "inc\t$imm,x", []> {
    let Uses = [X];
  }
}

// DEC - Decrement (memory)
let Defs = [P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  // DEC absolute
  def DEC_abs : W65816Inst24<0xCE, (outs), (ins addr16:$addr),
                             "dec\t$addr", []>;

  // DEC direct page
  def DEC_dp : W65816Inst16<0xC6, (outs), (ins addr8dp:$imm),
                            "dec\t$imm", []>;

  // DEC absolute,X
  def DEC_absX : W65816Inst24<0xDE, (outs), (ins addr16:$addr),
                              "dec\t$addr,x", []> {
    let Uses = [X];
  }

  // DEC direct page,X
  def DEC_dpX : W65816Inst16<0xD6, (outs), (ins addr8dp:$imm),
                             "dec\t$imm,x", []> {
    let Uses = [X];
  }
}

let Defs = [P] in {
  // INC16 - Increment any GPR16 register
  // Expanded to INC_A, INX, or INY based on allocated register
  def INC16 : Pseudo<(outs GPR16:$dst), (ins GPR16:$src),
                     "# INC16 $dst, $src",
                     [(set GPR16:$dst, (add GPR16:$src, 1))]> {
    let Constraints = "$src = $dst";
  }

  // DEC16 - Decrement any GPR16 register
  // Expanded to DEC_A, DEX, or DEY based on allocated register
  def DEC16 : Pseudo<(outs GPR16:$dst), (ins GPR16:$src),
                     "# DEC16 $dst, $src",
                     [(set GPR16:$dst, (add GPR16:$src, -1))]> {
    let Constraints = "$src = $dst";
  }
}

//===----------------------------------------------------------------------===//
// Transfer Instructions
//===----------------------------------------------------------------------===//

let Defs = [P] in {
  // TAX - Transfer A to X
  def TAX : W65816Inst8<0xAA, (outs), (ins),
                        "tax",
                        []> {
    let Defs = [X, P];
    let Uses = [A];
  }

  // TAY - Transfer A to Y
  def TAY : W65816Inst8<0xA8, (outs), (ins),
                        "tay",
                        []> {
    let Defs = [Y, P];
    let Uses = [A];
  }

  // TXA - Transfer X to A
  def TXA : W65816Inst8<0x8A, (outs), (ins),
                        "txa",
                        []> {
    let Defs = [A, P];
    let Uses = [X];
  }

  // TYA - Transfer Y to A
  def TYA : W65816Inst8<0x98, (outs), (ins),
                        "tya",
                        []> {
    let Defs = [A, P];
    let Uses = [Y];
  }
}

// Stack transfers (no flags affected)
// TXS - Transfer X to Stack Pointer
def TXS : W65816Inst8<0x9A, (outs), (ins),
                      "txs",
                      []> {
  let Defs = [SP];
  let Uses = [X];
}

// TSX - Transfer Stack Pointer to X
def TSX : W65816Inst8<0xBA, (outs), (ins),
                      "tsx",
                      []> {
  let Defs = [X, P];
  let Uses = [SP];
}

//===----------------------------------------------------------------------===//
// Status Flag Instructions
//===----------------------------------------------------------------------===//

// CLC - Clear Carry
def CLC : W65816Inst8<0x18, (outs), (ins), "clc", []> {
  let Defs = [P];
  let Uses = [P];
}

// SEC - Set Carry
def SEC : W65816Inst8<0x38, (outs), (ins), "sec", []> {
  let Defs = [P];
  let Uses = [P];
}

// CLD - Clear Decimal
def CLD : W65816Inst8<0xD8, (outs), (ins), "cld", []> {
  let Defs = [P];
  let Uses = [P];
}

// SED - Set Decimal
def SED : W65816Inst8<0xF8, (outs), (ins), "sed", []> {
  let Defs = [P];
  let Uses = [P];
}

// CLI - Clear Interrupt Disable
def CLI : W65816Inst8<0x58, (outs), (ins), "cli", []> {
  let Defs = [P];
  let Uses = [P];
}

// SEI - Set Interrupt Disable
def SEI : W65816Inst8<0x78, (outs), (ins), "sei", []> {
  let Defs = [P];
  let Uses = [P];
}

// CLV - Clear Overflow
def CLV : W65816Inst8<0xB8, (outs), (ins), "clv", []> {
  let Defs = [P];
  let Uses = [P];
}

// REP - Reset Processor Status Bits (65816 specific)
def REP : W65816Inst16<0xC2, (outs), (ins imm8:$imm), "rep\t#$imm", []> {
  let Defs = [P];
  let Uses = [P];
}

// SEP - Set Processor Status Bits (65816 specific)
def SEP : W65816Inst16<0xE2, (outs), (ins imm8:$imm), "sep\t#$imm", []> {
  let Defs = [P];
  let Uses = [P];
}

//===----------------------------------------------------------------------===//
// Branch Instructions
//===----------------------------------------------------------------------===//

let isBranch = 1, isTerminator = 1 in {
  // BRA - Branch Always (8-bit offset)
  let isBarrier = 1 in
  def BRA : W65816Inst16<0x80, (outs), (ins brtarget8:$imm),
                         "bra\t$imm",
                         [(br bb:$imm)]>;

  // BRL - Branch Long Always (16-bit offset)
  def BRL : W65816Inst24<0x82, (outs), (ins brtarget16:$addr),
                         "brl\t$addr", []>;

  // Conditional branches (8-bit offset)
  let Uses = [P] in {
    // BEQ - Branch if Equal (Z=1)
    def BEQ : W65816Inst16<0xF0, (outs), (ins brtarget8:$imm),
                           "beq\t$imm", []>;

    // BNE - Branch if Not Equal (Z=0)
    def BNE : W65816Inst16<0xD0, (outs), (ins brtarget8:$imm),
                           "bne\t$imm", []>;

    // BCS - Branch if Carry Set (C=1)
    def BCS : W65816Inst16<0xB0, (outs), (ins brtarget8:$imm),
                           "bcs\t$imm", []>;

    // BCC - Branch if Carry Clear (C=0)
    def BCC : W65816Inst16<0x90, (outs), (ins brtarget8:$imm),
                           "bcc\t$imm", []>;

    // BMI - Branch if Minus (N=1)
    def BMI : W65816Inst16<0x30, (outs), (ins brtarget8:$imm),
                           "bmi\t$imm", []>;

    // BPL - Branch if Plus (N=0)
    def BPL : W65816Inst16<0x10, (outs), (ins brtarget8:$imm),
                           "bpl\t$imm", []>;

    // BVS - Branch if Overflow Set (V=1)
    def BVS : W65816Inst16<0x70, (outs), (ins brtarget8:$imm),
                           "bvs\t$imm", []>;

    // BVC - Branch if Overflow Clear (V=0)
    def BVC : W65816Inst16<0x50, (outs), (ins brtarget8:$imm),
                           "bvc\t$imm", []>;
  }
}

//===----------------------------------------------------------------------===//
// Jump Instructions
//===----------------------------------------------------------------------===//

let isBarrier = 1, isBranch = 1, isTerminator = 1 in {
  // JMP - Jump Absolute
  def JMP_abs : W65816Inst24<0x4C, (outs), (ins addr16:$addr),
                             "jmp\t$addr", []>;

  // JML - Jump Long (24-bit)
  def JML : W65816Inst32<0x5C, (outs), (ins i32imm:$addr),
                         "jml\t$addr", []>;

  // JMP - Jump Absolute Indirect
  // Jump through pointer at specified address
  def JMP_ind : W65816Inst24<0x6C, (outs), (ins indaddr16:$addr),
                             "jmp\t$addr", []>;

  // JMP - Jump Absolute Indexed Indirect
  // Jump through pointer at (address + X) - useful for jump tables
  def JMP_indX : W65816Inst24<0x7C, (outs), (ins indXaddr16:$addr),
                              "jmp\t$addr", []> {
    let Uses = [X];
  }

  // JMP - Jump Absolute Indirect Long (24-bit)
  // Jump through 24-bit pointer at specified address
  def JMP_indL : W65816Inst24<0xDC, (outs), (ins indLaddr16:$addr),
                              "jmp\t$addr", []>;
}

//===----------------------------------------------------------------------===//
// Subroutine Instructions
//===----------------------------------------------------------------------===//

let isCall = 1 in {
  // JSR - Jump to Subroutine
  def JSR : W65816Inst24<0x20, (outs), (ins addr16:$addr),
                         "jsr\t$addr", []> {
    let Defs = [SP, A, X, Y, P];  // Caller-saved registers
    let Uses = [SP];
  }

  // JSL - Jump to Subroutine Long (24-bit)
  def JSL : W65816Inst32<0x22, (outs), (ins i32imm:$addr),
                         "jsl\t$addr", []> {
    let Defs = [SP, A, X, Y, P];  // Caller-saved registers
    let Uses = [SP];
  }
}

let isReturn = 1, isTerminator = 1, isBarrier = 1 in {
  // RTS - Return from Subroutine
  def RTS : W65816Inst8<0x60, (outs), (ins), "rts", []> {
    let Uses = [SP];
    let Defs = [SP];
  }

  // RTL - Return from Subroutine Long
  def RTL : W65816Inst8<0x6B, (outs), (ins), "rtl", []> {
    let Uses = [SP];
    let Defs = [SP];
  }

  // RTI - Return from Interrupt
  def RTI : W65816Inst8<0x40, (outs), (ins), "rti", []> {
    let Uses = [SP];
    let Defs = [SP, P];
  }
}

//===----------------------------------------------------------------------===//
// Stack Operations
//===----------------------------------------------------------------------===//

// PHA - Push Accumulator
def PHA : W65816Inst8<0x48, (outs), (ins), "pha", []> {
  let Uses = [A, SP];
  let Defs = [SP];
}

// PLA - Pull Accumulator
def PLA : W65816Inst8<0x68, (outs), (ins), "pla", []> {
  let Uses = [SP];
  let Defs = [A, P, SP];
}

// PHX - Push X
def PHX : W65816Inst8<0xDA, (outs), (ins), "phx", []> {
  let Uses = [X, SP];
  let Defs = [SP];
}

// PLX - Pull X
def PLX : W65816Inst8<0xFA, (outs), (ins), "plx", []> {
  let Uses = [SP];
  let Defs = [X, P, SP];
}

// PHY - Push Y
def PHY : W65816Inst8<0x5A, (outs), (ins), "phy", []> {
  let Uses = [Y, SP];
  let Defs = [SP];
}

// PLY - Pull Y
def PLY : W65816Inst8<0x7A, (outs), (ins), "ply", []> {
  let Uses = [SP];
  let Defs = [Y, P, SP];
}

// PHP - Push Processor Status
def PHP : W65816Inst8<0x08, (outs), (ins), "php", []> {
  let Uses = [P, SP];
  let Defs = [SP];
}

// PLP - Pull Processor Status
def PLP : W65816Inst8<0x28, (outs), (ins), "plp", []> {
  let Uses = [SP];
  let Defs = [P, SP];
}

// PHD - Push Direct Page Register (65816 specific)
def PHD : W65816Inst8<0x0B, (outs), (ins), "phd", []> {
  let Uses = [D, SP];
  let Defs = [SP];
}

// PLD - Pull Direct Page Register (65816 specific)
def PLD : W65816Inst8<0x2B, (outs), (ins), "pld", []> {
  let Uses = [SP];
  let Defs = [D, P, SP];
}

// PHB - Push Data Bank Register (65816 specific)
def PHB : W65816Inst8<0x8B, (outs), (ins), "phb", []> {
  let Uses = [DBR, SP];
  let Defs = [SP];
}

// PLB - Pull Data Bank Register (65816 specific)
def PLB : W65816Inst8<0xAB, (outs), (ins), "plb", []> {
  let Uses = [SP];
  let Defs = [DBR, P, SP];
}

// PHK - Push Program Bank Register (65816 specific)
def PHK : W65816Inst8<0x4B, (outs), (ins), "phk", []> {
  let Uses = [PBR, SP];
  let Defs = [SP];
}

//===----------------------------------------------------------------------===//
// Miscellaneous Instructions
//===----------------------------------------------------------------------===//

// NOP - No Operation
def NOP : W65816Inst8<0xEA, (outs), (ins), "nop", []>;

// XCE - Exchange Carry and Emulation Bits (65816 specific)
// This is the only way to switch between 6502 emulation mode and native mode
// To enter native mode: CLC; XCE (clear carry, then exchange)
// To enter emulation mode: SEC; XCE (set carry, then exchange)
def XCE : W65816Inst8<0xFB, (outs), (ins), "xce", []> {
  let Defs = [P];
  let Uses = [P];
  let hasSideEffects = 1;  // Changes processor mode
}

// WDM - Reserved for future expansion (treated as 2-byte NOP)
def WDM : W65816Inst16<0x42, (outs), (ins imm8:$imm), "wdm\t#$imm", []>;

// STP - Stop the Clock
def STP : W65816Inst8<0xDB, (outs), (ins), "stp", []>;

// WAI - Wait for Interrupt
def WAI : W65816Inst8<0xCB, (outs), (ins), "wai", []>;

//===----------------------------------------------------------------------===//
// Inter-register Transfers (65816 only)
//===----------------------------------------------------------------------===//

// TXY - Transfer X to Y
let Defs = [Y, P], Uses = [X], hasSideEffects = 0 in
def TXY : W65816Inst8<0x9B, (outs), (ins), "txy", []>;

// TYX - Transfer Y to X
let Defs = [X, P], Uses = [Y], hasSideEffects = 0 in
def TYX : W65816Inst8<0xBB, (outs), (ins), "tyx", []>;

//===----------------------------------------------------------------------===//
// 16-bit Register Transfers (65816 only)
//===----------------------------------------------------------------------===//

// TCD - Transfer 16-bit Accumulator to Direct Page Register
let Defs = [D, P], Uses = [A], hasSideEffects = 0 in
def TCD : W65816Inst8<0x5B, (outs), (ins), "tcd", []>;

// TDC - Transfer Direct Page Register to 16-bit Accumulator
let Defs = [A, P], Uses = [D], hasSideEffects = 0 in
def TDC : W65816Inst8<0x7B, (outs), (ins), "tdc", []>;

// TCS - Transfer 16-bit Accumulator to Stack Pointer
let Defs = [SP], Uses = [A], hasSideEffects = 0 in
def TCS : W65816Inst8<0x1B, (outs), (ins), "tcs", []>;

// TSC - Transfer Stack Pointer to 16-bit Accumulator
let Defs = [A, P], Uses = [SP], hasSideEffects = 0 in
def TSC : W65816Inst8<0x3B, (outs), (ins), "tsc", []>;

//===----------------------------------------------------------------------===//
// Byte Manipulation (65816 only)
//===----------------------------------------------------------------------===//

// XBA - Exchange B and A (swap high and low bytes of 16-bit accumulator)
// Maps directly to llvm.bswap.i16 intrinsic
let Defs = [P], hasSideEffects = 0 in
def XBA : W65816Inst8<0xEB, (outs ACC16:$dst), (ins ACC16:$src),
                      "xba", [(set ACC16:$dst, (bswap ACC16:$src))]> {
  let Constraints = "$src = $dst";
}

//===----------------------------------------------------------------------===//
// Stack Push Effective Address (65816 only)
//===----------------------------------------------------------------------===//

// PEA - Push Effective Absolute Address
// Pushes the 16-bit operand value onto the stack
let Defs = [SP], Uses = [SP], mayStore = 1 in
def PEA : W65816Inst24<0xF4, (outs), (ins addr16:$addr),
                       "pea\t$addr", []>;

// PEI - Push Effective Indirect Address
// Pushes 16-bit value from direct page location onto stack
let Defs = [SP], Uses = [SP], mayLoad = 1, mayStore = 1 in
def PEI : W65816Inst16<0xD4, (outs), (ins inddp:$imm),
                       "pei\t$imm", []>;

// PER - Push Effective PC Relative Address
// Pushes PC + signed 16-bit offset onto stack
let Defs = [SP], Uses = [SP], mayStore = 1 in
def PER : W65816Inst24<0x62, (outs), (ins brtarget16:$addr),
                       "per\t$addr", []>;

//===----------------------------------------------------------------------===//
// Test and Modify Bits (65816 only)
//===----------------------------------------------------------------------===//

// TSB - Test and Set Bits (Memory |= A, sets Z based on original AND)
let Defs = [P], Uses = [A], mayLoad = 1, mayStore = 1 in {
  def TSB_dp  : W65816Inst16<0x04, (outs), (ins imm8:$imm),
                             "tsb\t$imm", []>;
  def TSB_abs : W65816Inst24<0x0C, (outs), (ins addr16:$addr),
                             "tsb\t$addr", []>;
}

// TRB - Test and Reset Bits (Memory &= ~A, sets Z based on original AND)
let Defs = [P], Uses = [A], mayLoad = 1, mayStore = 1 in {
  def TRB_dp  : W65816Inst16<0x14, (outs), (ins imm8:$imm),
                             "trb\t$imm", []>;
  def TRB_abs : W65816Inst24<0x1C, (outs), (ins addr16:$addr),
                             "trb\t$addr", []>;
}

//===----------------------------------------------------------------------===//
// Software Interrupts
//===----------------------------------------------------------------------===//

// BRK - Software Break
// Takes a signature byte (often used as a system call number)
let isCall = 1, hasSideEffects = 1 in
def BRK : W65816Inst16<0x00, (outs), (ins imm8:$imm),
                       "brk\t#$imm", []>;

// COP - Co-processor Enable (software interrupt)
// Takes a signature byte for co-processor selection
let isCall = 1, hasSideEffects = 1 in
def COP : W65816Inst16<0x02, (outs), (ins imm8:$imm),
                       "cop\t#$imm", []>;

//===----------------------------------------------------------------------===//
// Pseudo Instructions for Code Generation
//===----------------------------------------------------------------------===//

// Return pseudo - will be lowered to RTS or RTL
let isReturn = 1, isTerminator = 1, isBarrier = 1 in {
  def RETW : Pseudo<(outs), (ins), "# RETW",
                    [(retflag)]>;
}

// 16-bit add pseudo - will need to expand to CLC + ADC sequence
// Output must be ACC16 (A register) since ADC always produces result in A.
// The register allocator will insert copies if the result is needed elsewhere.
let Defs = [P] in {
  // These pseudos output to GPR16 to give the register allocator flexibility.
  // The expansion code handles moving the result from A to X/Y if needed.
  def ADD16rr : Pseudo<(outs GPR16:$dst), (ins GPR16:$src1, GPR16:$src2),
                       "# ADD16rr",
                       [(set GPR16:$dst, (add GPR16:$src1, GPR16:$src2))]>;

  def ADD16ri : Pseudo<(outs GPR16:$dst), (ins GPR16:$src, imm16:$imm),
                       "# ADD16ri",
                       [(set GPR16:$dst, (add GPR16:$src, imm:$imm))]>;

  def SUB16rr : Pseudo<(outs GPR16:$dst), (ins GPR16:$src1, GPR16:$src2),
                       "# SUB16rr",
                       [(set GPR16:$dst, (sub GPR16:$src1, GPR16:$src2))]>;

  def SUB16ri : Pseudo<(outs GPR16:$dst), (ins GPR16:$src, imm16:$imm),
                       "# SUB16ri",
                       [(set GPR16:$dst, (sub GPR16:$src, imm:$imm))]>;
}

//===----------------------------------------------------------------------===//
// Call Frame Pseudo Instructions
//===----------------------------------------------------------------------===//

// These are used by the frame lowering code and are expanded to actual
// stack manipulation instructions.
// callseq_start and callseq_end are the standard way to mark call sequences

def SDT_W65816CallSeqStart : SDCallSeqStart<[SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;
def SDT_W65816CallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_W65816CallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END", SDT_W65816CallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

let Defs = [SP], Uses = [SP], hasSideEffects = 1 in {
  def ADJCALLSTACKDOWN : Pseudo<(outs), (ins i16imm:$amt, i16imm:$amt2),
                                "# ADJCALLSTACKDOWN $amt, $amt2",
                                [(callseq_start timm:$amt, timm:$amt2)]>;

  def ADJCALLSTACKUP : Pseudo<(outs), (ins i16imm:$amt1, i16imm:$amt2),
                              "# ADJCALLSTACKUP $amt1, $amt2",
                              [(callseq_end timm:$amt1, timm:$amt2)]>;
}

//===----------------------------------------------------------------------===//
// Select Pseudo Instructions
//===----------------------------------------------------------------------===//

// Select16 pseudo - expands to a diamond control flow pattern
// Operands: dst, trueVal, falseVal, condCode
// For unsigned/equality conditions, uses usesCustomInserter for early expansion
// Note: condCode uses i16 because i8 is not a legal type in the backend
let usesCustomInserter = 1, Uses = [P] in {
  def Select16 : Pseudo<(outs GPR16:$dst),
                        (ins GPR16:$trueVal, GPR16:$falseVal, i16imm:$cc),
                        "# Select16 $dst, $trueVal, $falseVal, $cc",
                        [(set GPR16:$dst,
                          (W65816selectcc GPR16:$trueVal, GPR16:$falseVal,
                                          (i16 imm:$cc)))]>;
}

// Signed select pseudos - expanded in ExpandPseudo pass (after branch folding)
// These require multi-instruction sequences for proper N!=V or N==V checks
let Uses = [P] in {
  def Select16_SLT : Pseudo<(outs GPR16:$dst),
                            (ins GPR16:$trueVal, GPR16:$falseVal),
                            "# Select16_SLT $dst, $trueVal, $falseVal", []>;
  def Select16_SGE : Pseudo<(outs GPR16:$dst),
                            (ins GPR16:$trueVal, GPR16:$falseVal),
                            "# Select16_SGE $dst, $trueVal, $falseVal", []>;
  def Select16_SGT : Pseudo<(outs GPR16:$dst),
                            (ins GPR16:$trueVal, GPR16:$falseVal),
                            "# Select16_SGT $dst, $trueVal, $falseVal", []>;
  def Select16_SLE : Pseudo<(outs GPR16:$dst),
                            (ins GPR16:$trueVal, GPR16:$falseVal),
                            "# Select16_SLE $dst, $trueVal, $falseVal", []>;
  // Unsigned compound comparisons (require multi-instruction sequences)
  def Select16_UGT : Pseudo<(outs GPR16:$dst),
                            (ins GPR16:$trueVal, GPR16:$falseVal),
                            "# Select16_UGT $dst, $trueVal, $falseVal", []>;
  def Select16_ULE : Pseudo<(outs GPR16:$dst),
                            (ins GPR16:$trueVal, GPR16:$falseVal),
                            "# Select16_ULE $dst, $trueVal, $falseVal", []>;
}

// Compare two registers pseudo - sets flags for subsequent branch
// Expands to: get src1 to A, push src2, cmp 1,s, pull
let Defs = [P] in {
  def CMP16rr : Pseudo<(outs), (ins GPR16:$src1, GPR16:$src2),
                       "# CMP16rr $src1, $src2",
                       [(W65816cmp GPR16:$src1, GPR16:$src2)]>;

  // Compare register with immediate - more efficient for constants
  // Expands to: get src to A, cmp #imm
  def CMP16ri : Pseudo<(outs), (ins GPR16:$src, i16imm:$imm),
                       "# CMP16ri $src, $imm",
                       [(W65816cmp GPR16:$src, imm:$imm)]>;
}

// Conditional branch pseudo - branches based on condition code
let isBranch = 1, isTerminator = 1, Uses = [P] in {
  def BR_CC : Pseudo<(outs), (ins brtarget8:$target, i8imm:$cc),
                     "# BR_CC $target, $cc",
                     [(W65816brcond bb:$target, (i8 imm:$cc))]>;
}

//===----------------------------------------------------------------------===//
// Shift Pseudo Instructions
//===----------------------------------------------------------------------===//

// Shift left by immediate amount - expands to multiple ASL instructions
// Note: All shifts use A internally, so they clobber A even if dst/src is X/Y
// Note: Shift amounts are i16 (getScalarShiftAmountTy returns i16 since i8 isn't legal)
let Defs = [A, P] in {
  def SHL16ri : Pseudo<(outs GPR16:$dst), (ins GPR16:$src, imm16:$amt),
                       "# SHL16ri $dst, $src, $amt",
                       [(set GPR16:$dst, (shl GPR16:$src, (i16 imm:$amt)))]>;

  def SRL16ri : Pseudo<(outs GPR16:$dst), (ins GPR16:$src, imm16:$amt),
                       "# SRL16ri $dst, $src, $amt",
                       [(set GPR16:$dst, (srl GPR16:$src, (i16 imm:$amt)))]>;

  def SRA16ri : Pseudo<(outs GPR16:$dst), (ins GPR16:$src, imm16:$amt),
                       "# SRA16ri $dst, $src, $amt",
                       [(set GPR16:$dst, (sra GPR16:$src, (i16 imm:$amt)))]>;

  // Variable amount shifts - expand to loops
  // Shift amount is i16 (promoted from i8)
  def SHL16rv : Pseudo<(outs GPR16:$dst), (ins GPR16:$src, GPR16:$amt),
                       "# SHL16rv $dst, $src, $amt",
                       [(set GPR16:$dst, (shl GPR16:$src, GPR16:$amt))]>;

  def SRL16rv : Pseudo<(outs GPR16:$dst), (ins GPR16:$src, GPR16:$amt),
                       "# SRL16rv $dst, $src, $amt",
                       [(set GPR16:$dst, (srl GPR16:$src, GPR16:$amt))]>;

  def SRA16rv : Pseudo<(outs GPR16:$dst), (ins GPR16:$src, GPR16:$amt),
                       "# SRA16rv $dst, $src, $amt",
                       [(set GPR16:$dst, (sra GPR16:$src, GPR16:$amt))]>;
}

//===----------------------------------------------------------------------===//
// Logical Operation Pseudo Instructions (register-to-register)
//===----------------------------------------------------------------------===//

// These expand to push one operand to stack, perform operation via stack-relative,
// then pull to restore stack.
// Output must be ACC16 (A register) since AND/ORA/EOR always produce result in A.
let Defs = [P] in {
  // These pseudos output to GPR16 to give the register allocator flexibility.
  // The expansion code handles moving the result from A to X/Y if needed.
  def AND16rr : Pseudo<(outs GPR16:$dst), (ins GPR16:$src1, GPR16:$src2),
                       "# AND16rr $dst, $src1, $src2",
                       [(set GPR16:$dst, (and GPR16:$src1, GPR16:$src2))]>;

  def OR16rr : Pseudo<(outs GPR16:$dst), (ins GPR16:$src1, GPR16:$src2),
                      "# OR16rr $dst, $src1, $src2",
                      [(set GPR16:$dst, (or GPR16:$src1, GPR16:$src2))]>;

  def XOR16rr : Pseudo<(outs GPR16:$dst), (ins GPR16:$src1, GPR16:$src2),
                       "# XOR16rr $dst, $src1, $src2",
                       [(set GPR16:$dst, (xor GPR16:$src1, GPR16:$src2))]>;
}

//===----------------------------------------------------------------------===//
// Indexed Store Pseudo Instructions
//===----------------------------------------------------------------------===//

// These handle indexed loads with proper register allocation
// The expansion handles setting up X register properly
let Defs = [A, X, P], mayLoad = 1, hasSideEffects = 0 in {
  def LDAindexedX : Pseudo<(outs GPR16:$dst), (ins addr16:$addr, GPR16:$idx),
                           "# LDAindexedX $dst, $addr, $idx", []>;
}

// Long (24-bit) indexed load - for far arrays with variable index
let Defs = [A, X, P], mayLoad = 1, hasSideEffects = 0 in {
  def LDAindexedLongX : Pseudo<(outs GPR16:$dst), (ins addr24:$addr, GPR16:$idx),
                               "# LDAindexedLongX $dst, $addr, $idx", []>;
}

// These handle indexed stores with proper register allocation
// The expansion handles saving/restoring registers as needed
let Defs = [A, X, P], mayStore = 1, hasSideEffects = 0 in {
  def STAindexedX : Pseudo<(outs), (ins GPR16:$val, addr16:$addr, GPR16:$idx),
                           "# STAindexedX $val, $addr, $idx", []>;
}

// Long (24-bit) indexed store - for far arrays with variable index
let Defs = [A, X, P], mayStore = 1, hasSideEffects = 0 in {
  def STAindexedLongX : Pseudo<(outs), (ins GPR16:$val, addr24:$addr, GPR16:$idx),
                               "# STAindexedLongX $val, $addr, $idx", []>;
}

// STZ indexed - store zero to array element (optimization for store 0)
let Defs = [X, P], mayStore = 1, hasSideEffects = 0 in {
  def STZindexedX : Pseudo<(outs), (ins addr16:$addr, GPR16:$idx),
                           "# STZindexedX $addr, $idx", []>;
}

//===----------------------------------------------------------------------===//
// Indirect Addressing Pseudo Instructions
//===----------------------------------------------------------------------===//

// Load through a pointer value with optional index
// This stores the pointer to a stack slot, then uses LDA (offset,S),Y
// stack_slot: frame index where pointer will be stored
// ptr: the pointer value to dereference
// idx: the index (byte offset) to add after dereferencing (0 for *ptr)
let Defs = [A, Y, P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  def LDAindirect : Pseudo<(outs GPR16:$dst),
                           (ins i32imm:$stack_slot, GPR16:$ptr, i16imm:$idx),
                           "# LDAindirect $dst, [$stack_slot], $ptr, $idx", []>;
}

// Load through a pointer with dynamic index (ptr[i])
// stack_slot: frame index where pointer will be stored
// ptr: the base pointer value
// idx: the byte offset (in a register) to add
let Defs = [A, Y, P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  def LDAindirectIdx : Pseudo<(outs GPR16:$dst),
                              (ins i32imm:$stack_slot, GPR16:$ptr, GPR16:$idx),
                              "# LDAindirectIdx $dst, [$stack_slot], $ptr, $idx", []>;
}

// Store through a pointer value with optional index
// This stores the pointer to a stack slot, then uses STA (offset,S),Y
let Defs = [A, Y, P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  def STAindirect : Pseudo<(outs),
                           (ins GPR16:$val, i32imm:$stack_slot, GPR16:$ptr, i16imm:$idx),
                           "# STAindirect $val, [$stack_slot], $ptr, $idx", []>;
}

// Store through a pointer with dynamic index (ptr[i] = val)
let Defs = [A, Y, P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  def STAindirectIdx : Pseudo<(outs),
                              (ins GPR16:$val, i32imm:$stack_slot, GPR16:$ptr, GPR16:$idx),
                              "# STAindirectIdx $val, [$stack_slot], $ptr, $idx", []>;
}

//===----------------------------------------------------------------------===//
// Direct Page Indirect Pseudo Instructions
// These provide efficient access through pointers stored in direct page
//===----------------------------------------------------------------------===//

// Load through DP pointer with index: LDA ($dp),Y
// dp_addr: direct page address containing the pointer
// idx: byte offset to add (will be moved to Y)
let Defs = [A, Y, P], mayLoad = 1, hasSideEffects = 0 in {
  def LDAindexedDPY : Pseudo<(outs GPR16:$dst),
                             (ins imm8:$dp_addr, GPR16:$idx),
                             "# LDAindexedDPY $dst, ($dp_addr), $idx", []>;
}

// Store through DP pointer with index: STA ($dp),Y
// val: value to store
// dp_addr: direct page address containing the pointer
// idx: byte offset to add (will be moved to Y)
let Defs = [A, Y, P], mayStore = 1, hasSideEffects = 0 in {
  def STAindexedDPY : Pseudo<(outs),
                             (ins GPR16:$val, imm8:$dp_addr, GPR16:$idx),
                             "# STAindexedDPY $val, ($dp_addr), $idx", []>;
}

//===----------------------------------------------------------------------===//
// Block Move Instructions (MVN/MVP)
// These copy blocks of memory between banks
//===----------------------------------------------------------------------===//

// Note: Block move operands are two bank bytes.
// Assembler syntax: MVN srcbank,destbank
// Object code order: opcode, destbank, srcbank (reversed!)
// X = source address, Y = dest address, C = length-1
// These instructions use and modify A (C), X, Y

let Defs = [A, X, Y, P], Uses = [A, X, Y], hasSideEffects = 1 in {
  // MVN - Block Move Negative (incrementing addresses)
  // Use when destination is lower than source (or no overlap)
  // Copies from X++ to Y++ until C becomes $FFFF
  def MVN : W65816InstBlockMove<0x54, (outs), (ins imm8:$srcbank, imm8:$destbank),
                                "mvn\t$srcbank,$destbank", []>;

  // MVP - Block Move Positive (decrementing addresses)
  // Use when destination is higher than source
  // Copies from X-- to Y-- until C becomes $FFFF
  def MVP : W65816InstBlockMove<0x44, (outs), (ins imm8:$srcbank, imm8:$destbank),
                                "mvp\t$srcbank,$destbank", []>;
}

//===----------------------------------------------------------------------===//
// 8-bit Load/Store Pseudo Instructions (with mode switching)
//===----------------------------------------------------------------------===//

// These pseudo instructions handle 8-bit memory operations by switching
// the processor to 8-bit mode (SEP #$20), doing the operation, then
// switching back to 16-bit mode (REP #$20).

// LDA8_abs - Load 8-bit value from absolute address and zero-extend to 16-bit
// Expands to: SEP #$20; LDA addr; REP #$20; AND #$00FF (for zext)
// Note: Don't list A in Defs since it's the explicit output
let Defs = [P], mayLoad = 1, hasSideEffects = 0 in {
  def LDA8_abs : Pseudo<(outs ACC16:$dst), (ins addr16:$addr),
                        "# LDA8_abs $dst, $addr", []>;
}

// LDA8_sr - Load 8-bit value from stack-relative address
let Defs = [P], mayLoad = 1, hasSideEffects = 0 in {
  def LDA8_sr : Pseudo<(outs ACC16:$dst), (ins imm8:$offset),
                       "# LDA8_sr $dst, $offset", []>;
}

// STA8_abs - Store 8-bit value (low byte of 16-bit reg) to absolute address
// Expands to: SEP #$20; STA addr; REP #$20
// Note: Clobbers A during mode switch
let Defs = [A, P], mayStore = 1, hasSideEffects = 0 in {
  def STA8_abs : Pseudo<(outs), (ins ACC16:$src, addr16:$addr),
                        "# STA8_abs $src, $addr", []>;
}

// STA8_sr - Store 8-bit value to stack-relative address
let Defs = [A, P], mayStore = 1, hasSideEffects = 0 in {
  def STA8_sr : Pseudo<(outs), (ins ACC16:$src, imm8:$offset),
                       "# STA8_sr $src, $offset", []>;
}

// LDA8_srIndY - Load 8-bit value using stack-relative indirect indexed addressing
// The address of the target is stored at (SP + offset), then load from that address + Y
// Expands to: SEP #$20; LDY #0; LDA (offset,S),Y; REP #$20; AND #$00FF
let Defs = [A, Y, P], mayLoad = 1, hasSideEffects = 0 in {
  def LDA8_srIndY : Pseudo<(outs ACC16:$dst), (ins imm8:$offset),
                           "# LDA8_srIndY $dst, $offset", []>;
}

// STA8_srIndY - Store 8-bit value using stack-relative indirect indexed addressing
// Expands to: SEP #$20; LDY #0; STA (offset,S),Y; REP #$20
let Defs = [Y, P], Uses = [A], mayStore = 1, hasSideEffects = 0 in {
  def STA8_srIndY : Pseudo<(outs), (ins ACC16:$src, imm8:$offset),
                           "# STA8_srIndY $src, $offset", []>;
}

// LDA8indirect - Load 8-bit value through pointer with mode switching
// Operands: (dst, stack_slot, ptr, index)
// Expands to: STA ptr to stack_slot, SEP #$20, LDY index, LDA (slot,S),Y, REP #$20, AND #$FF
// Note: Only defines Y and P, A is the output operand
let Defs = [Y, P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  def LDA8indirect : Pseudo<(outs ACC16:$dst), (ins i16imm:$slot, GPR16:$ptr, i16imm:$idx),
                            "# LDA8indirect $dst, [$slot], $ptr, $idx", []>;
}

// STA8indirect - Store 8-bit value through pointer with mode switching
// Operands: (src, stack_slot, ptr, index)
// Src can be in any GPR - expansion handles moving to A before store
// Expands to: store ptr to stack_slot, move src to A, SEP #$20, LDY index, STA (slot,S),Y, REP #$20
let Defs = [A, Y, P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  def STA8indirect : Pseudo<(outs), (ins GPR16:$src, i16imm:$slot, GPR16:$ptr, i16imm:$idx),
                            "# STA8indirect $src, [$slot], $ptr, $idx", []>;
}

// LDA8indirectIdx - Load 8-bit value through pointer+index with mode switching
// Operands: (dst, stack_slot, ptr, idx)
// ptr is base pointer, idx is byte offset (in a GPR)
// Expands to: STA ptr to stack_slot, move idx to Y, SEP #$20, LDA (slot,S),Y, REP #$20, AND #$FF
let Defs = [Y, P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  def LDA8indirectIdx : Pseudo<(outs ACC16:$dst),
                               (ins i32imm:$stack_slot, GPR16:$ptr, GPR16:$idx),
                               "# LDA8indirectIdx $dst, [$stack_slot], $ptr, $idx", []>;
}

// STA8indirectIdx - Store 8-bit value through pointer+index with mode switching
// Operands: (src, stack_slot, ptr, idx)
// src is value, ptr is base pointer, idx is byte offset (in a GPR)
// Expands to: store ptr to stack_slot, move idx to Y, move src to A, SEP #$20, STA (slot,S),Y, REP #$20
let Defs = [A, Y, P], mayLoad = 1, mayStore = 1, hasSideEffects = 0 in {
  def STA8indirectIdx : Pseudo<(outs),
                               (ins GPR16:$src, i32imm:$stack_slot, GPR16:$ptr, GPR16:$idx),
                               "# STA8indirectIdx $src, [$stack_slot], $ptr, $idx", []>;
}

//===----------------------------------------------------------------------===//
// Type Conversion Patterns
//===----------------------------------------------------------------------===//

// Truncate i16 to i8: just extract the low byte (sub_lo sub-register)
def : Pat<(i8 (trunc GPR16:$src)),
          (EXTRACT_SUBREG GPR16:$src, sub_lo)>;

//===----------------------------------------------------------------------===//
// 8-bit Load/Store Patterns
//===----------------------------------------------------------------------===//

// Zero-extending load from i8 to i16 (from absolute address)
def : Pat<(i16 (zextloadi8 addr:$addr)),
          (LDA8_abs addr16:$addr)>;

// Sign-extending load from i8 to i16 (from absolute address)
// Note: For sign extension, we'd need additional logic after the load
// For now, treat as zero-extend (caller may need to handle sign extension)
def : Pat<(i16 (sextloadi8 addr:$addr)),
          (LDA8_abs addr16:$addr)>;

// Any-extending load from i8 to i16 (from absolute address)
def : Pat<(i16 (extloadi8 addr:$addr)),
          (LDA8_abs addr16:$addr)>;

// Truncating store from i16 to i8 (to absolute address)
def : Pat<(truncstorei8 GPR16:$val, addr:$addr),
          (STA8_abs GPR16:$val, addr16:$addr)>;
