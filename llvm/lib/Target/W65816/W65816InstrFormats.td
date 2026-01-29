//===-- W65816InstrFormats.td - W65816 Instruction Formats --*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// W65816 Instruction Format Definitions.
//
// The W65816 has variable-length instructions from 1-4 bytes:
// - 1 byte:  Implied/Accumulator addressing
// - 2 bytes: Immediate (8-bit), Direct Page, Relative
// - 3 bytes: Immediate (16-bit), Absolute, Direct Page Indirect
// - 4 bytes: Absolute Long (24-bit address)
//
//===----------------------------------------------------------------------===//

// A generic W65816 instruction
class W65816Inst<dag outs, dag ins, string asmstr, list<dag> pattern>
    : Instruction {
  let Namespace = "W65816";

  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;
}

//===----------------------------------------------------------------------===//
// 1-byte instructions (implied/accumulator addressing)
//===----------------------------------------------------------------------===//
class W65816Inst8<bits<8> opcode, dag outs, dag ins, string asmstr,
                  list<dag> pattern>
    : W65816Inst<outs, ins, asmstr, pattern> {
  field bits<8> Inst;
  let Inst = opcode;
  let Size = 1;
}

//===----------------------------------------------------------------------===//
// 2-byte instructions (immediate 8-bit, direct page, relative branch)
// Memory layout: [opcode][operand]
// Encoding: opcode in low bits (emitted first), operand in high bits
//===----------------------------------------------------------------------===//
class W65816Inst16<bits<8> opcode, dag outs, dag ins, string asmstr,
                   list<dag> pattern>
    : W65816Inst<outs, ins, asmstr, pattern> {
  field bits<16> Inst;
  bits<8> imm;

  let Inst{7-0} = opcode;
  let Inst{15-8} = imm;
  let Size = 2;
}

//===----------------------------------------------------------------------===//
// 3-byte instructions (immediate 16-bit, absolute addressing)
// Memory layout: [opcode][addr_lo][addr_hi]
// Encoding: opcode in low bits (emitted first), addr in high bits (little-endian)
//===----------------------------------------------------------------------===//
class W65816Inst24<bits<8> opcode, dag outs, dag ins, string asmstr,
                   list<dag> pattern>
    : W65816Inst<outs, ins, asmstr, pattern> {
  field bits<24> Inst;
  bits<16> addr;

  let Inst{7-0} = opcode;
  let Inst{23-8} = addr;
  let Size = 3;
}

//===----------------------------------------------------------------------===//
// 4-byte instructions (absolute long - 24-bit address)
// Memory layout: [opcode][addr_lo][addr_mid][addr_hi]
// Encoding: opcode in low bits (emitted first), addr in high bits (little-endian)
//===----------------------------------------------------------------------===//
class W65816Inst32<bits<8> opcode, dag outs, dag ins, string asmstr,
                   list<dag> pattern>
    : W65816Inst<outs, ins, asmstr, pattern> {
  field bits<32> Inst;
  bits<24> addr;

  let Inst{7-0} = opcode;
  let Inst{31-8} = addr;
  let Size = 4;
}

//===----------------------------------------------------------------------===//
// Pseudo instructions
//===----------------------------------------------------------------------===//
class Pseudo<dag outs, dag ins, string asmstr, list<dag> pattern>
    : W65816Inst<outs, ins, asmstr, pattern> {
  let isPseudo = 1;
  let isCodeGenOnly = 1;
  let Size = 0;
}
