//===-- W65816AsmPrinter.cpp - W65816 LLVM Assembly Printer ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains a printer that converts from our internal representation
// of machine-dependent LLVM code to GAS-format W65816 assembly language.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/W65816InstPrinter.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"
#include "TargetInfo/W65816TargetInfo.h"
#include "W65816.h"
#include "W65816MCInstLower.h"
#include "W65816Subtarget.h"
#include "W65816TargetMachine.h"

#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "asm-printer"

namespace {
class W65816AsmPrinter : public AsmPrinter {
public:
  explicit W65816AsmPrinter(TargetMachine &TM,
                            std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)) {}

  StringRef getPassName() const override { return "W65816 Assembly Printer"; }

  void emitInstruction(const MachineInstr *MI) override;

  bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                       const char *ExtraCode, raw_ostream &O) override;

  bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                             const char *ExtraCode, raw_ostream &O) override;
};
} // end anonymous namespace

void W65816AsmPrinter::emitInstruction(const MachineInstr *MI) {
  W65816MCInstLower MCInstLowering(OutContext, *this);

  MCInst TmpInst;
  MCInstLowering.Lower(MI, TmpInst);
  EmitToStreamer(*OutStreamer, TmpInst);
}

bool W65816AsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                       const char *ExtraCode, raw_ostream &O) {
  // Print the operand if there is no modifier
  if (!ExtraCode || !ExtraCode[0]) {
    const MachineOperand &MO = MI->getOperand(OpNo);
    switch (MO.getType()) {
    case MachineOperand::MO_Register:
      O << W65816InstPrinter::getRegisterName(MO.getReg());
      return false;
    case MachineOperand::MO_Immediate:
      O << MO.getImm();
      return false;
    default:
      return true;
    }
  }

  return AsmPrinter::PrintAsmOperand(MI, OpNo, ExtraCode, O);
}

bool W65816AsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                             unsigned OpNo,
                                             const char *ExtraCode,
                                             raw_ostream &O) {
  if (ExtraCode && ExtraCode[0])
    return true; // Unknown modifier

  const MachineOperand &MO = MI->getOperand(OpNo);
  if (MO.isReg()) {
    O << '(' << W65816InstPrinter::getRegisterName(MO.getReg()) << ')';
    return false;
  }

  return true;
}

// Force static initialization
extern "C" LLVM_ABI LLVM_EXTERNAL_VISIBILITY void
LLVMInitializeW65816AsmPrinter() {
  RegisterAsmPrinter<W65816AsmPrinter> X(getTheW65816Target());
}
