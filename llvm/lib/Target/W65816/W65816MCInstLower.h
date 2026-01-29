//===-- W65816MCInstLower.h - Lower MachineInstr to MCInst ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_W65816MCINSTLOWER_H
#define LLVM_LIB_TARGET_W65816_W65816MCINSTLOWER_H

#include "llvm/Support/Compiler.h"

namespace llvm {

class AsmPrinter;
class MachineInstr;
class MachineOperand;
class MCContext;
class MCInst;
class MCOperand;
class MCSymbol;

class W65816MCInstLower {
public:
  W65816MCInstLower(MCContext &Ctx, AsmPrinter &Printer)
      : Ctx(Ctx), Printer(Printer) {}

  void Lower(const MachineInstr *MI, MCInst &OutMI) const;

  MCOperand LowerOperand(const MachineOperand &MO, unsigned Offset = 0) const;
  MCOperand LowerSymbolOperand(const MachineOperand &MO, MCSymbol *Sym) const;

  MCSymbol *GetGlobalAddressSymbol(const MachineOperand &MO) const;
  MCSymbol *GetExternalSymbolSymbol(const MachineOperand &MO) const;
  MCSymbol *GetBlockAddressSymbol(const MachineOperand &MO) const;

private:
  MCContext &Ctx;
  AsmPrinter &Printer;
};

} // namespace llvm

#endif // LLVM_LIB_TARGET_W65816_W65816MCINSTLOWER_H
