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

#include "MCTargetDesc/W65816MCTargetDesc.h"
#include "W65816.h"
#include "W65816Subtarget.h"
#include "W65816TargetMachine.h"

#include "W65816MachineFunctionInfo.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/ErrorHandling.h"

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
      report_fatal_error("direct page frame exceeds 256-byte limit (offset " +
                         Twine(Offset) + " out of range)");
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
    case W65816::SBC_sr:
      // ADC_dp/SBC_dp not defined - use stack-relative addressing
      break;
    default:
      // Other instructions use stack-relative addressing
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
  int64_t Offset = MFI.getObjectOffset(FrameIndex);
  uint64_t StackSize = MFI.getStackSize();
  bool IsFixed = MFI.isFixedObjectIndex(FrameIndex);
  uint64_t MaxCallFrameSize = MFI.getMaxCallFrameSize();

  // The prologue uses PHA to save A before arithmetic stack adjustment when
  // StackSize > 8. This adds 2 extra bytes to the actual frame size that
  // aren't reflected in StackSize. Account for this here.
  uint64_t ProloguePHASize = (StackSize > 8) ? 2 : 0;

  // Stack layout (addresses grow down, SP points to last used byte):
  //
  // For OUTGOING args (in caller, created by LowerCall):
  //   With hasReservedCallFrame() = true, space is pre-allocated in prologue.
  //   Args at offsets 0, 2, 4 map to SP+1, SP+3, SP+5.
  //
  // For INCOMING args (in callee, created by LowerFormalArguments):
  //   After JSR pushes return address (2 bytes), then callee pushes its frame:
  //   - SP+1 to SP+StackSize = callee's locals/spills
  //   - SP+StackSize+1, SP+StackSize+2 = return address
  //   - SP+StackSize+3+ = incoming args (offset 0, 2, 4...)
  //   So incoming arg at offset N is at SP + StackSize + 2 + N + 1.
  //
  // For LOCALS (negative offsets from frame):
  //   Local at offset -N is at SP + StackSize - N + 1.
  //
  // We distinguish outgoing from incoming by checking MaxCallFrameSize:
  // - Outgoing: fixed, offset >= 0, offset < MaxCallFrameSize
  // - Incoming: fixed, offset >= 0, but not in outgoing area (or no calls)

  if (IsFixed && Offset >= 0) {
    if (MaxCallFrameSize > 0 &&
        static_cast<uint64_t>(Offset) < MaxCallFrameSize) {
      // Outgoing call argument - at bottom of caller's pre-allocated frame
      Offset += 1;
    } else {
      // Incoming argument - above return address
      Offset += StackSize + ProloguePHASize + SPAdj;
      Offset += 2; // return address size
      Offset += 1; // SP points to last used byte
    }
  } else {
    // Locals (negative offset from frame base)
    Offset += StackSize + ProloguePHASize + SPAdj;
    Offset += 1; // SP points to last used byte
  }

  // For stack-relative instructions, replace frame index with immediate offset
  if (Opcode == W65816::LDA_sr || Opcode == W65816::STA_sr ||
      Opcode == W65816::ADC_sr || Opcode == W65816::SBC_sr ||
      Opcode == W65816::LDA_srIndY || Opcode == W65816::STA_srIndY ||
      Opcode == W65816::RELOAD_GPR16 || Opcode == W65816::SPILL_GPR16) {
    // Change the frame index operand to an immediate offset
    Inst.getOperand(FIOperandNum).ChangeToImmediate(Offset);
  } else if (Opcode == W65816::LDA_sr_off) {
    // LDA_sr_off $dst, $fi, $constant_offset
    // Combine frame offset + constant offset, then convert to regular load
    int64_t ConstOffset = Inst.getOperand(FIOperandNum + 1).getImm();
    int64_t CombinedOffset = Offset + ConstOffset;

    // Change to RELOAD_GPR16 which handles the GPR16 destination
    // (will be expanded to LDA_sr + transfer if needed)
    Inst.setDesc(TII.get(W65816::RELOAD_GPR16));
    Inst.getOperand(FIOperandNum).ChangeToImmediate(CombinedOffset);
    // Remove the extra offset operand
    Inst.removeOperand(FIOperandNum + 1);
  } else if (Opcode == W65816::STA_sr_off) {
    // STA_sr_off $src, $fi, $constant_offset
    // Combine frame offset + constant offset, then convert to regular store
    int64_t ConstOffset = Inst.getOperand(FIOperandNum + 1).getImm();
    int64_t CombinedOffset = Offset + ConstOffset;

    // Change to SPILL_GPR16 which handles the GPR16 source
    // (will be expanded to transfer + STA_sr if needed)
    Inst.setDesc(TII.get(W65816::SPILL_GPR16));
    Inst.getOperand(FIOperandNum).ChangeToImmediate(CombinedOffset);
    // Remove the extra offset operand
    Inst.removeOperand(FIOperandNum + 1);
  } else {
    // Default: replace frame index with computed stack offset
    Inst.getOperand(FIOperandNum).ChangeToImmediate(Offset);
  }

  return false;
}

Register W65816RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  // W65816 uses direct page (D) register for frame pointer when needed
  // but typically uses SP directly
  return W65816::SP;
}
