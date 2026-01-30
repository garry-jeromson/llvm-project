//===-- W65816InstPrinter.cpp - Convert W65816 MCInst to asm syntax -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This class prints a W65816 MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "W65816InstPrinter.h"

#include "MCTargetDesc/W65816MCTargetDesc.h"

#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"

#include <cstring>

#define DEBUG_TYPE "asm-printer"

using namespace llvm;

// Include the auto-generated portion of the assembly writer
#define PRINT_ALIAS_INSTR
#include "W65816GenAsmWriter.inc"

void W65816InstPrinter::printInst(const MCInst *MI, uint64_t Address,
                                  StringRef Annot, const MCSubtargetInfo &STI,
                                  raw_ostream &OS) {
  if (!printAliasInstr(MI, Address, OS))
    printInstruction(MI, Address, OS);

  printAnnotation(OS, Annot);
}

void W65816InstPrinter::printRegName(raw_ostream &OS, MCRegister Reg) {
  OS << getRegisterName(Reg);
}

void W65816InstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                     raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);

  if (Op.isReg()) {
    printRegName(OS, Op.getReg());
  } else if (Op.isImm()) {
    OS << formatImm(Op.getImm());
  } else {
    assert(Op.isExpr() && "Unknown operand kind in printOperand");
    MAI.printExpr(OS, *Op.getExpr());
  }
}

// Print 16-bit immediate as unsigned value
// This ensures negative numbers are printed as unsigned to avoid assembler issues
void W65816InstPrinter::printImm16(const MCInst *MI, unsigned OpNo,
                                   raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);

  if (Op.isImm()) {
    // Mask to 16 bits to ensure unsigned output
    uint16_t Val = Op.getImm() & 0xFFFF;
    OS << Val;
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  } else {
    OS << "<unknown>";
  }
}

void W65816InstPrinter::printAddrModeMemSrc(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);

  if (Op.isImm()) {
    OS << "$" << format_hex_no_prefix(Op.getImm(), 4);
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  } else if (Op.isReg()) {
    // This shouldn't normally happen for memory addresses
    OS << "%" << getRegisterName(Op.getReg());
  } else {
    // Print something useful for debugging
    OS << "<unknown>";
  }
}

void W65816InstPrinter::printLongAddr(const MCInst *MI, unsigned OpNo,
                                      raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);

  if (Op.isImm()) {
    // Print 24-bit address as $BBAAAA (bank:address)
    uint32_t Addr = Op.getImm() & 0xFFFFFF;
    OS << "$" << format_hex_no_prefix(Addr, 6);
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  } else {
    OS << "<unknown>";
  }
}

//===----------------------------------------------------------------------===//
// Indirect Addressing Mode Print Methods
//===----------------------------------------------------------------------===//

// Print (addr) - Indirect 16-bit address
void W65816InstPrinter::printIndirectAddr(const MCInst *MI, unsigned OpNo,
                                          raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);
  OS << "(";
  if (Op.isImm()) {
    OS << "$" << format_hex_no_prefix(Op.getImm(), 4);
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  }
  OS << ")";
}

// Print (addr,x) - Indexed Indirect 16-bit address
void W65816InstPrinter::printIndirectXAddr(const MCInst *MI, unsigned OpNo,
                                           raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);
  OS << "(";
  if (Op.isImm()) {
    OS << "$" << format_hex_no_prefix(Op.getImm(), 4);
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  }
  OS << ",x)";
}

// Print [addr] - Indirect Long 16-bit address
void W65816InstPrinter::printIndirectLongAddr(const MCInst *MI, unsigned OpNo,
                                              raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);
  OS << "[";
  if (Op.isImm()) {
    OS << "$" << format_hex_no_prefix(Op.getImm(), 4);
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  }
  OS << "]";
}

// Print (dp) - Indirect Direct Page
void W65816InstPrinter::printIndirectDP(const MCInst *MI, unsigned OpNo,
                                        raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);
  OS << "(";
  if (Op.isImm()) {
    OS << "$" << format_hex_no_prefix(Op.getImm(), 2);
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  }
  OS << ")";
}

// Print (dp),y - Indirect Direct Page Indexed Y
void W65816InstPrinter::printIndirectDPY(const MCInst *MI, unsigned OpNo,
                                         raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);
  OS << "(";
  if (Op.isImm()) {
    OS << "$" << format_hex_no_prefix(Op.getImm(), 2);
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  }
  OS << "),y";
}

// Print (dp,x) - Indexed Indirect Direct Page
void W65816InstPrinter::printIndexedIndirectDP(const MCInst *MI, unsigned OpNo,
                                               raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);
  OS << "(";
  if (Op.isImm()) {
    OS << "$" << format_hex_no_prefix(Op.getImm(), 2);
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  }
  OS << ",x)";
}

// Print [dp] - Indirect Long Direct Page
void W65816InstPrinter::printIndirectDPLong(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);
  OS << "[";
  if (Op.isImm()) {
    OS << "$" << format_hex_no_prefix(Op.getImm(), 2);
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  }
  OS << "]";
}

// Print [dp],y - Indirect Long Direct Page Indexed Y
void W65816InstPrinter::printIndirectDPLongY(const MCInst *MI, unsigned OpNo,
                                             raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);
  OS << "[";
  if (Op.isImm()) {
    OS << "$" << format_hex_no_prefix(Op.getImm(), 2);
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  }
  OS << "],y";
}

// Print (offset,s),y - Stack Relative Indirect Indexed Y
void W65816InstPrinter::printStackRelIndirectY(const MCInst *MI, unsigned OpNo,
                                               raw_ostream &OS) {
  const MCOperand &Op = MI->getOperand(OpNo);
  OS << "(";
  if (Op.isImm()) {
    OS << "$" << format_hex_no_prefix(Op.getImm(), 2);
  } else if (Op.isExpr()) {
    MAI.printExpr(OS, *Op.getExpr());
  }
  OS << ",s),y";
}
