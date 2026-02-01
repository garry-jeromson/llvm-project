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
// Imaginary Registers (backed by Direct Page memory)
//===----------------------------------------------------------------------===//
// These registers are not physical CPU registers but are backed by
// Direct Page (zero page) memory locations. The register allocator
// can use these when A, X, Y are exhausted. This is the same approach
// used by LLVM-MOS for the 6502.
//
// Memory layout:
//   $00-$0F: System reserved (interrupt vectors, hardware scratch)
//   $10-$2F: Imaginary registers (RS0-RS15)
//   $30-$7F: User DP variables (w65816_dpframe locals)
//   $80-$FB: Additional user/stack scratch
//   $FC-$FD: Scratch for spill/reload
//   $FE-$FF: Scratch for ALU operations
//
// The imaginary registers use slightly higher allocation cost than
// physical registers, so the allocator prefers A, X, Y when possible.

// 16-bit imaginary registers (each uses 2 consecutive bytes in DP)
// RS0 = $10-$11, RS1 = $12-$13, ..., RS15 = $2E-$2F
def RS0  : W65816Reg<16, "rs0">,  DwarfRegNum<[16]>;
def RS1  : W65816Reg<17, "rs1">,  DwarfRegNum<[17]>;
def RS2  : W65816Reg<18, "rs2">,  DwarfRegNum<[18]>;
def RS3  : W65816Reg<19, "rs3">,  DwarfRegNum<[19]>;
def RS4  : W65816Reg<20, "rs4">,  DwarfRegNum<[20]>;
def RS5  : W65816Reg<21, "rs5">,  DwarfRegNum<[21]>;
def RS6  : W65816Reg<22, "rs6">,  DwarfRegNum<[22]>;
def RS7  : W65816Reg<23, "rs7">,  DwarfRegNum<[23]>;
def RS8  : W65816Reg<24, "rs8">,  DwarfRegNum<[24]>;
def RS9  : W65816Reg<25, "rs9">,  DwarfRegNum<[25]>;
def RS10 : W65816Reg<26, "rs10">, DwarfRegNum<[26]>;
def RS11 : W65816Reg<27, "rs11">, DwarfRegNum<[27]>;
def RS12 : W65816Reg<28, "rs12">, DwarfRegNum<[28]>;
def RS13 : W65816Reg<29, "rs13">, DwarfRegNum<[29]>;
def RS14 : W65816Reg<30, "rs14">, DwarfRegNum<[30]>;
def RS15 : W65816Reg<31, "rs15">, DwarfRegNum<[31]>;

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

// 16-bit general purpose (A, X, Y + imaginary registers)
// Physical registers are listed first (preferred by allocator)
// Imaginary registers are used when physical registers are exhausted
def GPR16 : RegisterClass<"W65816", [i16], 16,
  (add A, X, Y,
       RS0, RS1, RS2, RS3, RS4, RS5, RS6, RS7,
       RS8, RS9, RS10, RS11, RS12, RS13, RS14, RS15)>;

// Physical-only 16-bit GPR class (for cases where only A, X, Y are valid)
def GPR16Phys : RegisterClass<"W65816", [i16], 16, (add A, X, Y)>;

// Imaginary-only register class (for explicit DP operations)
// Higher CopyCost than physical registers to make allocator prefer A, X, Y
def IMAG16 : RegisterClass<"W65816", [i16], 16,
  (add RS0, RS1, RS2, RS3, RS4, RS5, RS6, RS7,
       RS8, RS9, RS10, RS11, RS12, RS13, RS14, RS15)> {
  let CopyCost = 4;  // Higher cost than physical registers (default is 1)
}

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
