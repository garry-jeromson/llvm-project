//===-- W65816RegisterBankInfo.h --------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares the targeting of the RegisterBankInfo class for W65816.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_GISEL_W65816REGISTERBANKINFO_H
#define LLVM_LIB_TARGET_W65816_GISEL_W65816REGISTERBANKINFO_H

#include "llvm/CodeGen/RegisterBankInfo.h"

#define GET_REGBANK_DECLARATIONS
#include "W65816GenRegisterBank.inc"
#undef GET_REGBANK_DECLARATIONS

namespace llvm {

class TargetRegisterInfo;

class W65816GenRegisterBankInfo : public RegisterBankInfo {
protected:
#define GET_TARGET_REGBANK_CLASS
#include "W65816GenRegisterBank.inc"
#undef GET_TARGET_REGBANK_CLASS
};

class W65816RegisterBankInfo final : public W65816GenRegisterBankInfo {
public:
  W65816RegisterBankInfo(const TargetRegisterInfo &TRI);

  const InstructionMapping &
  getInstrMapping(const MachineInstr &MI) const override;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_W65816_GISEL_W65816REGISTERBANKINFO_H
