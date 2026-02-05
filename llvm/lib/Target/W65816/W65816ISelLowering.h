//===-- W65816ISelLowering.h - W65816 DAG Lowering Interface ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the W65816 target lowering interface used by both
// GlobalISel and general LLVM infrastructure (type legality, inline asm, etc.).
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_W65816ISELLOWERING_H
#define LLVM_LIB_TARGET_W65816_W65816ISELLOWERING_H

#include "llvm/CodeGen/TargetLowering.h"

namespace llvm {

class W65816Subtarget;

class W65816TargetLowering : public TargetLowering {
public:
  explicit W65816TargetLowering(const TargetMachine &TM,
                                const W65816Subtarget &STI);

  MachineBasicBlock *
  EmitInstrWithCustomInserter(MachineInstr &MI,
                              MachineBasicBlock *MBB) const override;

  MVT getScalarShiftAmountTy(const DataLayout &DL, EVT VT) const override {
    return MVT::i16;
  }

  Register getRegisterByName(const char *RegName, LLT VT,
                             const MachineFunction &MF) const override;

  std::pair<unsigned, const TargetRegisterClass *>
  getRegForInlineAsmConstraint(const TargetRegisterInfo *TRI,
                               StringRef Constraint, MVT VT) const override;

  EVT getSetCCResultType(const DataLayout &DL, LLVMContext &Context,
                         EVT VT) const override {
    return MVT::i16;
  }

private:
  const W65816Subtarget &Subtarget;
};

} // namespace llvm

#endif // LLVM_LIB_TARGET_W65816_W65816ISELLOWERING_H
