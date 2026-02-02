//===-- W65816MCInstLower.cpp - Convert W65816 MachineInstr to an MCInst --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains code to lower W65816 MachineInstrs to their corresponding
// MCInst records.
//
//===----------------------------------------------------------------------===//

#include "W65816MCInstLower.h"

#include "MCTargetDesc/W65816MCTargetDesc.h"
#include "W65816InstrInfo.h"

#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

MCSymbol *
W65816MCInstLower::GetGlobalAddressSymbol(const MachineOperand &MO) const {
  return Printer.getSymbol(MO.getGlobal());
}

MCSymbol *
W65816MCInstLower::GetExternalSymbolSymbol(const MachineOperand &MO) const {
  return Printer.GetExternalSymbolSymbol(MO.getSymbolName());
}

MCSymbol *
W65816MCInstLower::GetBlockAddressSymbol(const MachineOperand &MO) const {
  return Printer.GetBlockAddressSymbol(MO.getBlockAddress());
}

MCOperand W65816MCInstLower::LowerSymbolOperand(const MachineOperand &MO,
                                                MCSymbol *Sym) const {
  const MCExpr *Expr = MCSymbolRefExpr::create(Sym, Ctx);

  if (!MO.isJTI() && MO.getOffset())
    Expr = MCBinaryExpr::createAdd(
        Expr, MCConstantExpr::create(MO.getOffset(), Ctx), Ctx);

  return MCOperand::createExpr(Expr);
}

MCOperand W65816MCInstLower::LowerOperand(const MachineOperand &MO,
                                          unsigned Offset) const {
  switch (MO.getType()) {
  case MachineOperand::MO_Register:
    // Ignore all implicit register operands
    if (MO.isImplicit())
      return MCOperand();
    return MCOperand::createReg(MO.getReg());

  case MachineOperand::MO_Immediate:
    return MCOperand::createImm(MO.getImm() + Offset);

  case MachineOperand::MO_GlobalAddress:
    return LowerSymbolOperand(MO, GetGlobalAddressSymbol(MO));

  case MachineOperand::MO_ExternalSymbol:
    return LowerSymbolOperand(MO, GetExternalSymbolSymbol(MO));

  case MachineOperand::MO_MachineBasicBlock:
    return MCOperand::createExpr(
        MCSymbolRefExpr::create(MO.getMBB()->getSymbol(), Ctx));

  case MachineOperand::MO_BlockAddress:
    return LowerSymbolOperand(MO, GetBlockAddressSymbol(MO));

  case MachineOperand::MO_RegisterMask:
    return MCOperand();

  default:
    llvm_unreachable("Unknown operand type");
  }
}

void W65816MCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const {
  OutMI.setOpcode(MI->getOpcode());

  for (const MachineOperand &MO : MI->operands()) {
    MCOperand MCOp = LowerOperand(MO);

    if (MCOp.isValid())
      OutMI.addOperand(MCOp);
  }
}
