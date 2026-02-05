//===-- W65816RegisterBankInfo.cpp ------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the targeting of the RegisterBankInfo class for W65816.
//
//===----------------------------------------------------------------------===//

#include "W65816RegisterBankInfo.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"
#include "W65816InstrInfo.h"
#include "W65816Subtarget.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterBank.h"
#include "llvm/CodeGen/RegisterBankInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"

#define GET_TARGET_REGBANK_IMPL
#include "W65816GenRegisterBank.inc"

using namespace llvm;

namespace llvm {
namespace W65816 {
enum PartialMappingIdx {
  PMI_GPR16,
  PMI_GPR8,
  PMI_GPR1,
  PMI_Min = PMI_GPR16,
};

const RegisterBankInfo::PartialMapping PartMappings[] = {
    {0, 16, GPRRegBank}, // PMI_GPR16
    {0, 8, GPRRegBank},  // PMI_GPR8
    {0, 1, GPRRegBank},  // PMI_GPR1
};

const RegisterBankInfo::ValueMapping ValMappings[] = {
    {&PartMappings[PMI_GPR16 - PMI_Min], 1}, // GPR 16-bit
    {&PartMappings[PMI_GPR8 - PMI_Min], 1},  // GPR 8-bit
    {&PartMappings[PMI_GPR1 - PMI_Min], 1},  // GPR 1-bit
};

const RegisterBankInfo::ValueMapping *getValueMapping(unsigned SizeInBits) {
  if (SizeInBits <= 1)
    return &ValMappings[2];
  if (SizeInBits <= 8)
    return &ValMappings[1];
  return &ValMappings[0];
}

} // end namespace W65816
} // end namespace llvm

W65816RegisterBankInfo::W65816RegisterBankInfo(const TargetRegisterInfo &TRI)
    : W65816GenRegisterBankInfo() {}

const RegisterBankInfo::InstructionMapping &
W65816RegisterBankInfo::getInstrMapping(const MachineInstr &MI) const {
  const MachineFunction &MF = *MI.getParent()->getParent();
  const MachineRegisterInfo &MRI = MF.getRegInfo();
  auto Opc = MI.getOpcode();

  // For non-generic opcodes, use the default implementation.
  if (!isPreISelGenericOpcode(Opc)) {
    const InstructionMapping &Mapping = getInstrMappingImpl(MI);
    if (Mapping.isValid())
      return Mapping;
    return getInvalidInstructionMapping();
  }

  // PHI instructions are copy-like: mapping has NumOperands=1 (dest only).
  if (MI.isPHI()) {
    Register DstReg = MI.getOperand(0).getReg();
    LLT Ty = MRI.getType(DstReg);
    if (!Ty.isValid())
      return getInvalidInstructionMapping();

    SmallVector<const ValueMapping *, 1> OpdsMapping(1);
    OpdsMapping[0] = W65816::getValueMapping(Ty.getSizeInBits());
    return getInstructionMapping(DefaultMappingID, /*Cost=*/1,
                                 getOperandsMapping(OpdsMapping), 1);
  }

  // Build a mapping where every register operand maps to GPR,
  // and non-register operands get nullptr.
  unsigned NumOperands = MI.getNumOperands();
  SmallVector<const ValueMapping *, 4> OpdsMapping(NumOperands, nullptr);

  for (unsigned I = 0; I < NumOperands; ++I) {
    const MachineOperand &MO = MI.getOperand(I);
    if (!MO.isReg() || !MO.getReg())
      continue;

    // All virtual registers map to GPR bank.
    Register Reg = MO.getReg();
    if (!Reg.isVirtual())
      continue;

    LLT Ty = MRI.getType(Reg);
    if (!Ty.isValid())
      continue;

    OpdsMapping[I] = W65816::getValueMapping(Ty.getSizeInBits());
  }

  return getInstructionMapping(DefaultMappingID, /*Cost=*/1,
                               getOperandsMapping(OpdsMapping), NumOperands);
}
