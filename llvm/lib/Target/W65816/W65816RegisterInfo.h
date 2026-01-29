//===-- W65816RegisterInfo.h - W65816 Register Information Impl -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the W65816 implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_W65816REGISTERINFO_H
#define LLVM_LIB_TARGET_W65816_W65816REGISTERINFO_H

#include "llvm/CodeGen/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "W65816GenRegisterInfo.inc"

namespace llvm {

class W65816Subtarget;

class W65816RegisterInfo : public W65816GenRegisterInfo {
public:
  W65816RegisterInfo();

  const uint16_t *
  getCalleeSavedRegs(const MachineFunction *MF = nullptr) const override;

  BitVector getReservedRegs(const MachineFunction &MF) const override;

  const TargetRegisterClass *
  getPointerRegClass(unsigned Kind = 0) const override;

  bool eliminateFrameIndex(MachineBasicBlock::iterator MI, int SPAdj,
                           unsigned FIOperandNum,
                           RegScavenger *RS = nullptr) const override;

  Register getFrameRegister(const MachineFunction &MF) const override;

  bool requiresRegisterScavenging(const MachineFunction &MF) const override {
    return true;
  }

  bool trackLivenessAfterRegAlloc(const MachineFunction &MF) const override {
    return true;
  }

  bool requiresFrameIndexScavenging(const MachineFunction &MF) const override {
    return true;
  }
};

} // namespace llvm

#endif // LLVM_LIB_TARGET_W65816_W65816REGISTERINFO_H
