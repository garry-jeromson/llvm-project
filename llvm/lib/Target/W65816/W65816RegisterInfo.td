//===-- W65816RegisterInfo.td - W65816 Register defs -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//  Declarations that describe the W65816 register file
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Register class and subregister definitions
//===----------------------------------------------------------------------===//

class W65816Reg<bits<16> num, string name, list<Register> subregs = []>
    : RegisterWithSubRegs<name, subregs> {
  field bits<16> Num = num;
  let HWEncoding = num;
  let Namespace = "W65816";
  let SubRegs = subregs;
}

// Subregister indices
let Namespace = "W65816" in {
  def sub_lo : SubRegIndex<8>;
  def sub_hi : SubRegIndex<8, 8>;
}

//===----------------------------------------------------------------------===//
// 8-bit register components (low and high halves of 16-bit registers)
//===----------------------------------------------------------------------===//

// Accumulator 8-bit halves
def AL : W65816Reg<0, "al">, DwarfRegNum<[0]>;
def AH : W65816Reg<1, "ah">, DwarfRegNum<[1]>;

// X index register 8-bit halves
def XL : W65816Reg<2, "xl">, DwarfRegNum<[2]>;
def XH : W65816Reg<3, "xh">, DwarfRegNum<[3]>;

// Y index register 8-bit halves
def YL : W65816Reg<4, "yl">, DwarfRegNum<[4]>;
def YH : W65816Reg<5, "yh">, DwarfRegNum<[5]>;

// Stack pointer 8-bit halves
def SPL : W65816Reg<6, "spl">, DwarfRegNum<[6]>;
def SPH : W65816Reg<7, "sph">, DwarfRegNum<[7]>;

// Direct page register 8-bit halves
def DL : W65816Reg<8, "dl">, DwarfRegNum<[8]>;
def DH : W65816Reg<9, "dh">, DwarfRegNum<[9]>;

//===----------------------------------------------------------------------===//
// 16-bit registers
//===----------------------------------------------------------------------===//

let SubRegIndices = [sub_lo, sub_hi], CoveredBySubRegs = 1 in {
  // Accumulator - primary arithmetic register
  def A : W65816Reg<0, "a", [AL, AH]>, DwarfRegNum<[0]>;

  // X index register
  def X : W65816Reg<1, "x", [XL, XH]>, DwarfRegNum<[2]>;

  // Y index register
  def Y : W65816Reg<2, "y", [YL, YH]>, DwarfRegNum<[4]>;

  // Stack pointer (16-bit in native mode)
  def SP : W65816Reg<3, "sp", [SPL, SPH]>, DwarfRegNum<[6]>;

  // Direct page base register
  def D : W65816Reg<4, "d", [DL, DH]>, DwarfRegNum<[8]>;
}

//===----------------------------------------------------------------------===//
// 8-bit special purpose registers
//===----------------------------------------------------------------------===//

// Data bank register (8-bit, extends 16-bit addr to 24-bit for data access)
def DBR : W65816Reg<5, "dbr">, DwarfRegNum<[10]>;

// Program bank register (8-bit, extends 16-bit PC to 24-bit)
def PBR : W65816Reg<6, "pbr">, DwarfRegNum<[11]>;

// Processor status register
def P : W65816Reg<7, "p">, DwarfRegNum<[12]>;

//===----------------------------------------------------------------------===//
// Register classes
//===----------------------------------------------------------------------===//

// 8-bit accumulator class
def ACC8 : RegisterClass<"W65816", [i8], 8, (add AL)>;

// 16-bit accumulator class
def ACC16 : RegisterClass<"W65816", [i16], 16, (add A)>;

// 8-bit index register class
def IDX8 : RegisterClass<"W65816", [i8], 8, (add XL, YL)>;

// 16-bit index register class
def IDX16 : RegisterClass<"W65816", [i16], 16, (add X, Y)>;

// Individual 16-bit register classes for instruction patterns
def X16 : RegisterClass<"W65816", [i16], 16, (add X)>;
def Y16 : RegisterClass<"W65816", [i16], 16, (add Y)>;

// 8-bit general purpose (A, X, Y low bytes)
def GPR8 : RegisterClass<"W65816", [i8], 8, (add AL, XL, YL)>;

// 16-bit general purpose (A, X, Y)
def GPR16 : RegisterClass<"W65816", [i16], 16, (add A, X, Y)>;

// All 16-bit registers including special purpose (for moves)
def ALL16 : RegisterClass<"W65816", [i16], 16, (add A, X, Y, SP, D)>;

// 8-bit status/bank registers
def STATUS : RegisterClass<"W65816", [i8], 8, (add P, DBR, PBR)>;

// Stack pointer class
def SPREG : RegisterClass<"W65816", [i16], 16, (add SP)> {
  let CopyCost = -1;  // Don't allow copies
  let isAllocatable = 0;
}

// Direct page register class (callee-saved, used for optimization)
def DPREG : RegisterClass<"W65816", [i16], 16, (add D)> {
  let CopyCost = -1;
  let isAllocatable = 0;
}
