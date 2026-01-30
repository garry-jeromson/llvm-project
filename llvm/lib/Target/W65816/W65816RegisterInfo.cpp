//===-- W65816RegisterInfo.cpp - W65816 Register Information --------------===//
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

#include "W65816RegisterInfo.h"

#include "W65816.h"
#include "W65816Subtarget.h"
#include "W65816TargetMachine.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"

#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/ErrorHandling.h"
#include "W65816MachineFunctionInfo.h"

#define GET_REGINFO_TARGET_DESC
#include "W65816GenRegisterInfo.inc"

using namespace llvm;

W65816RegisterInfo::W65816RegisterInfo() : W65816GenRegisterInfo(W65816::SP) {}

const uint16_t *
W65816RegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_W65816_SaveList;
}

BitVector W65816RegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());

  // Stack pointer is always reserved
  Reserved.set(W65816::SP);
  Reserved.set(W65816::SPL);
  Reserved.set(W65816::SPH);

  // Program bank register is reserved (managed by hardware)
  Reserved.set(W65816::PBR);

  // Data bank register is typically reserved
  Reserved.set(W65816::DBR);

  // Processor status is reserved
  Reserved.set(W65816::P);

  return Reserved;
}

const TargetRegisterClass *
W65816RegisterInfo::getPointerRegClass(unsigned Kind) const {
  return &W65816::GPR16RegClass;
}

bool W65816RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator MI,
                                             int SPAdj, unsigned FIOperandNum,
                                             RegScavenger *RS) const {
  MachineInstr &Inst = *MI;
  MachineBasicBlock &MBB = *Inst.getParent();
  MachineFunction &MF = *MBB.getParent();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const W65816MachineFunctionInfo *AFI =
      MF.getInfo<W65816MachineFunctionInfo>();
  const W65816Subtarget &STI = MF.getSubtarget<W65816Subtarget>();
  const W65816InstrInfo &TII = *STI.getInstrInfo();

  int FrameIndex = Inst.getOperand(FIOperandNum).getIndex();
  unsigned Opcode = Inst.getOpcode();

  // Handle Direct Page frame functions
  // Convert stack-relative addressing to Direct Page addressing
  if (AFI->usesDPFrame()) {
    // For DP frame, offset is directly from $0000 (when D=0)
    // Object offsets are negative, so negate to get positive DP offset
    int64_t Offset = -MFI.getObjectOffset(FrameIndex);

    // Validate offset fits in DP range (0-255 bytes)
    if (Offset < 0 || Offset >= 256) {
      report_fatal_error("Direct Page frame exceeds 256-byte limit (offset " +
                         Twine(Offset) + " out of range). Use stack frame instead.");
    }

    // Convert instruction to DP addressing mode
    unsigned DPOpcode = 0;
    switch (Opcode) {
    case W65816::LDA_sr:
      DPOpcode = W65816::LDA_dp;
      break;
    case W65816::STA_sr:
      DPOpcode = W65816::STA_dp;
      break;
    case W65816::ADC_sr:
      // ADC_dp doesn't exist in our ISA, would need to add it
      // For now, fall through to regular handling
      break;
    case W65816::SBC_sr:
      // SBC_dp doesn't exist in our ISA, would need to add it
      break;
    default:
      // Other instructions - handle with stack-relative for now
      break;
    }

    if (DPOpcode != 0) {
      // Change instruction to DP version
      Inst.setDesc(TII.get(DPOpcode));
      Inst.getOperand(FIOperandNum).ChangeToImmediate(Offset);
      return false;
    }

    // Fallback: use DP offset but keep instruction type
    // This may not be correct for all cases but handles simple loads/stores
    Inst.getOperand(FIOperandNum).ChangeToImmediate(Offset);
    return false;
  }

  // Regular stack-relative addressing
  // Calculate the offset from the stack pointer
  // Object offset is negative (below the frame), StackSize gives total frame size
  int64_t Offset = MFI.getObjectOffset(FrameIndex);
  uint64_t StackSize = MFI.getStackSize();

  // The offset from SP is: StackSize + ObjectOffset + SPAdj
  // Since ObjectOffset is typically negative (object below frame pointer),
  // and stack grows down, we add StackSize to get offset from SP
  Offset += StackSize + SPAdj;

  // Stack-relative addressing on W65816 uses offset from current SP
  // The offset must be positive and fit in 8 bits (0-255)
  // Add 1 because SP points to last used byte, not next free
  Offset += 1;

  // For stack-relative instructions, replace frame index with immediate offset
  if (Opcode == W65816::LDA_sr || Opcode == W65816::STA_sr ||
      Opcode == W65816::ADC_sr || Opcode == W65816::SBC_sr ||
      Opcode == W65816::LDA_srIndY || Opcode == W65816::STA_srIndY ||
      Opcode == W65816::RELOAD_GPR16 || Opcode == W65816::SPILL_GPR16) {
    // Change the frame index operand to an immediate offset
    Inst.getOperand(FIOperandNum).ChangeToImmediate(Offset);
  } else {
    // For other instructions, we may need different handling
    // For now, just replace with immediate
    Inst.getOperand(FIOperandNum).ChangeToImmediate(Offset);
  }

  return false;
}

Register W65816RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  // W65816 uses direct page (D) register for frame pointer when needed
  // but typically uses SP directly
  return W65816::SP;
}
