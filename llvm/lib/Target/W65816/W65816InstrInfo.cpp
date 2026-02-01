//===-- W65816InstrInfo.cpp - W65816 Instruction Information --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the W65816 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "W65816InstrInfo.h"

#include "W65816.h"
#include "W65816Subtarget.h"
#include "W65816TargetMachine.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"

#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#define GET_INSTRINFO_CTOR_DTOR
#include "W65816GenInstrInfo.inc"

using namespace llvm;

W65816InstrInfo::W65816InstrInfo(const W65816Subtarget &STI)
    : W65816GenInstrInfo(STI, RI, W65816::ADJCALLSTACKDOWN, W65816::ADJCALLSTACKUP),
      RI(), STI(STI) {}

// Helper to check if a register is an imaginary register (RS0-RS15)
static bool isImaginaryReg(Register Reg) {
  return W65816::IMAG16RegClass.contains(Reg);
}

// Helper to check if a register is a physical GPR (A, X, Y)
static bool isPhysicalGPR(Register Reg) {
  return Reg == W65816::A || Reg == W65816::X || Reg == W65816::Y;
}

void W65816InstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MI,
                                  const DebugLoc &DL, Register DestReg,
                                  Register SrcReg, bool KillSrc,
                                  bool RenamableDest, bool RenamableSrc) const {
  // W65816 register copy capabilities:
  // - A <-> X: TAX, TXA
  // - A <-> Y: TAY, TYA
  // - X <-> Y: TXY, TYX (direct transfers, don't clobber A!)
  // - Physical <-> Imaginary: via pseudos (expanded later)
  // - Imaginary <-> Imaginary: via pseudo (expanded later)

  if (W65816::ACC16RegClass.contains(DestReg, SrcReg)) {
    // A to A - no-op
    return;
  }

  // Handle imaginary registers
  bool SrcIsImag = isImaginaryReg(SrcReg);
  bool DstIsImag = isImaginaryReg(DestReg);

  if (SrcIsImag && DstIsImag) {
    // Imaginary to imaginary: use COPY_IMAG_TO_IMAG pseudo
    BuildMI(MBB, MI, DL, get(W65816::COPY_IMAG_TO_IMAG), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }

  if (SrcIsImag && isPhysicalGPR(DestReg)) {
    // Imaginary to physical: use COPY_FROM_IMAG pseudo
    BuildMI(MBB, MI, DL, get(W65816::COPY_FROM_IMAG), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }

  if (DstIsImag && isPhysicalGPR(SrcReg)) {
    // Physical to imaginary: use COPY_TO_IMAG pseudo
    BuildMI(MBB, MI, DL, get(W65816::COPY_TO_IMAG), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }

  // Physical to physical copies
  if (W65816::GPR16RegClass.contains(DestReg) &&
      W65816::GPR16RegClass.contains(SrcReg)) {
    if (SrcReg == W65816::A) {
      // A -> X or A -> Y
      if (DestReg == W65816::X) {
        BuildMI(MBB, MI, DL, get(W65816::TAX));
      } else if (DestReg == W65816::Y) {
        BuildMI(MBB, MI, DL, get(W65816::TAY));
      }
    } else if (SrcReg == W65816::X) {
      if (DestReg == W65816::A) {
        // X -> A
        BuildMI(MBB, MI, DL, get(W65816::TXA));
      } else if (DestReg == W65816::Y) {
        // X -> Y: Use direct TXY instruction (doesn't clobber A!)
        BuildMI(MBB, MI, DL, get(W65816::TXY));
      }
    } else if (SrcReg == W65816::Y) {
      if (DestReg == W65816::A) {
        // Y -> A
        BuildMI(MBB, MI, DL, get(W65816::TYA));
      } else if (DestReg == W65816::X) {
        // Y -> X: Use direct TYX instruction (doesn't clobber A!)
        BuildMI(MBB, MI, DL, get(W65816::TYX));
      }
    }
    return;
  }

  llvm_unreachable("Unsupported register copy");
}

void W65816InstrInfo::storeRegToStackSlot(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI, Register SrcReg,
    bool isKill, int FrameIndex, const TargetRegisterClass *RC,
    Register VReg, MachineInstr::MIFlag Flags) const {
  DebugLoc DL;
  if (MI != MBB.end()) {
    DL = MI->getDebugLoc();
  }

  MachineFunction &MF = *MBB.getParent();
  MachineFrameInfo &MFI = MF.getFrameInfo();

  MachineMemOperand *MMO = MF.getMachineMemOperand(
      MachinePointerInfo::getFixedStack(MF, FrameIndex),
      MachineMemOperand::MOStore, MFI.getObjectSize(FrameIndex),
      MFI.getObjectAlign(FrameIndex));

  // Use stack-relative store instructions
  // The frame index will be resolved to an offset in eliminateFrameIndex
  if (W65816::ACC16RegClass.hasSubClassEq(RC) || SrcReg == W65816::A) {
    // Store accumulator to stack slot
    // STA_sr takes: src reg, frame index (resolved to offset later)
    BuildMI(MBB, MI, DL, get(W65816::STA_sr))
        .addReg(SrcReg, getKillRegState(isKill))
        .addFrameIndex(FrameIndex)
        .addMemOperand(MMO);
  } else if (W65816::IDX16RegClass.hasSubClassEq(RC) ||
             SrcReg == W65816::X || SrcReg == W65816::Y) {
    // For X/Y, we need to transfer to A first, then store
    // Problem: TXA/TYA clobbers A, but A might have a live value!
    // Solution: Save A to the hardware stack first, then restore after
    // This is expensive but correct for register pressure situations
    BuildMI(MBB, MI, DL, get(W65816::PHA));  // Save A to hardware stack
    if (SrcReg == W65816::X) {
      BuildMI(MBB, MI, DL, get(W65816::TXA));
    } else {
      BuildMI(MBB, MI, DL, get(W65816::TYA));
    }
    BuildMI(MBB, MI, DL, get(W65816::STA_sr))
        .addReg(W65816::A, RegState::Kill)
        .addFrameIndex(FrameIndex)
        .addMemOperand(MMO);
    BuildMI(MBB, MI, DL, get(W65816::PLA));  // Restore A from hardware stack
  } else if (W65816::GPR16RegClass.hasSubClassEq(RC)) {
    // GPR16 includes A, X, Y - handle based on physical reg
    // During spilling, SrcReg might be a virtual register
    if (SrcReg == W65816::A) {
      BuildMI(MBB, MI, DL, get(W65816::STA_sr))
          .addReg(SrcReg, getKillRegState(isKill))
          .addFrameIndex(FrameIndex)
          .addMemOperand(MMO);
    } else if (SrcReg == W65816::X) {
      // Save A first since TXA will clobber it
      BuildMI(MBB, MI, DL, get(W65816::PHA));
      BuildMI(MBB, MI, DL, get(W65816::TXA));
      BuildMI(MBB, MI, DL, get(W65816::STA_sr))
          .addReg(W65816::A, RegState::Kill)
          .addFrameIndex(FrameIndex)
          .addMemOperand(MMO);
      BuildMI(MBB, MI, DL, get(W65816::PLA));
    } else if (SrcReg == W65816::Y) {
      // Save A first since TYA will clobber it
      BuildMI(MBB, MI, DL, get(W65816::PHA));
      BuildMI(MBB, MI, DL, get(W65816::TYA));
      BuildMI(MBB, MI, DL, get(W65816::STA_sr))
          .addReg(W65816::A, RegState::Kill)
          .addFrameIndex(FrameIndex)
          .addMemOperand(MMO);
      BuildMI(MBB, MI, DL, get(W65816::PLA));
    } else if (isImaginaryReg(SrcReg)) {
      // Imaginary register: load from DP to A, then store to stack
      // Use COPY_FROM_IMAG to get value to A, then STA_sr
      BuildMI(MBB, MI, DL, get(W65816::COPY_FROM_IMAG), W65816::A)
          .addReg(SrcReg, getKillRegState(isKill));
      BuildMI(MBB, MI, DL, get(W65816::STA_sr))
          .addReg(W65816::A, RegState::Kill)
          .addFrameIndex(FrameIndex)
          .addMemOperand(MMO);
    } else if (SrcReg.isVirtual()) {
      // Use SPILL_GPR16 pseudo for virtual registers.
      // This doesn't constrain to ACC16, giving the allocator flexibility.
      // The pseudo is expanded after register allocation.
      BuildMI(MBB, MI, DL, get(W65816::SPILL_GPR16))
          .addReg(SrcReg, getKillRegState(isKill))
          .addFrameIndex(FrameIndex)
          .addMemOperand(MMO);
    } else {
      llvm_unreachable("Unknown GPR16 register");
    }
  } else if (W65816::IMAG16RegClass.hasSubClassEq(RC)) {
    // Imaginary register class: load from DP to A, then store to stack
    BuildMI(MBB, MI, DL, get(W65816::COPY_FROM_IMAG), W65816::A)
        .addReg(SrcReg, getKillRegState(isKill));
    BuildMI(MBB, MI, DL, get(W65816::STA_sr))
        .addReg(W65816::A, RegState::Kill)
        .addFrameIndex(FrameIndex)
        .addMemOperand(MMO);
  } else {
    llvm_unreachable("Can't store this register class to stack slot");
  }
}

void W65816InstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                           MachineBasicBlock::iterator MI,
                                           Register DestReg, int FrameIndex,
                                           const TargetRegisterClass *RC,
                                           Register VReg, unsigned SubReg,
                                           MachineInstr::MIFlag Flags) const {
  DebugLoc DL;
  if (MI != MBB.end()) {
    DL = MI->getDebugLoc();
  }

  MachineFunction &MF = *MBB.getParent();
  MachineFrameInfo &MFI = MF.getFrameInfo();

  MachineMemOperand *MMO = MF.getMachineMemOperand(
      MachinePointerInfo::getFixedStack(MF, FrameIndex),
      MachineMemOperand::MOLoad, MFI.getObjectSize(FrameIndex),
      MFI.getObjectAlign(FrameIndex));

  // Use stack-relative load instructions
  if (W65816::ACC16RegClass.hasSubClassEq(RC) || DestReg == W65816::A) {
    // Load accumulator from stack slot
    BuildMI(MBB, MI, DL, get(W65816::LDA_sr), DestReg)
        .addFrameIndex(FrameIndex)
        .addMemOperand(MMO);
  } else if (W65816::IDX16RegClass.hasSubClassEq(RC) ||
             DestReg == W65816::X || DestReg == W65816::Y) {
    // For X/Y, we need to use a pseudo that hides the internal A usage
    // from the register allocator. This will be expanded after RA.
    BuildMI(MBB, MI, DL, get(W65816::RELOAD_GPR16), DestReg)
        .addFrameIndex(FrameIndex)
        .addMemOperand(MMO);
  } else if (W65816::GPR16RegClass.hasSubClassEq(RC)) {
    // GPR16 includes A, X, Y and imaginary registers
    if (isImaginaryReg(DestReg)) {
      // Imaginary register: load from stack to A, then store to DP
      BuildMI(MBB, MI, DL, get(W65816::LDA_sr), W65816::A)
          .addFrameIndex(FrameIndex)
          .addMemOperand(MMO);
      BuildMI(MBB, MI, DL, get(W65816::COPY_TO_IMAG), DestReg)
          .addReg(W65816::A, RegState::Kill);
    } else {
      // The pseudo expansion will handle the A save/restore when needed
      BuildMI(MBB, MI, DL, get(W65816::RELOAD_GPR16), DestReg)
          .addFrameIndex(FrameIndex)
          .addMemOperand(MMO);
    }
  } else if (W65816::IMAG16RegClass.hasSubClassEq(RC)) {
    // Imaginary register class: load from stack to A, then store to DP
    BuildMI(MBB, MI, DL, get(W65816::LDA_sr), W65816::A)
        .addFrameIndex(FrameIndex)
        .addMemOperand(MMO);
    BuildMI(MBB, MI, DL, get(W65816::COPY_TO_IMAG), DestReg)
        .addReg(W65816::A, RegState::Kill);
  } else {
    llvm_unreachable("Can't load this register class from stack slot");
  }
}

bool W65816InstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                    MachineBasicBlock *&TBB,
                                    MachineBasicBlock *&FBB,
                                    SmallVectorImpl<MachineOperand> &Cond,
                                    bool AllowModify) const {
  // Start from the bottom of the block and work up
  MachineBasicBlock::iterator I = MBB.end();
  while (I != MBB.begin()) {
    --I;

    if (I->isDebugInstr())
      continue;

    // If we've hit a non-terminator, we're done
    if (!I->isTerminator())
      break;

    // Handle unconditional branches
    if (I->getOpcode() == W65816::BRA || I->getOpcode() == W65816::JMP_abs) {
      if (TBB) {
        return true; // Can't handle this
      }
      TBB = I->getOperand(0).getMBB();
      continue;
    }

    // Handle conditional branches
    if (I->getOpcode() == W65816::BEQ || I->getOpcode() == W65816::BNE ||
        I->getOpcode() == W65816::BCS || I->getOpcode() == W65816::BCC ||
        I->getOpcode() == W65816::BMI || I->getOpcode() == W65816::BPL ||
        I->getOpcode() == W65816::BVS || I->getOpcode() == W65816::BVC) {
      if (Cond.empty()) {
        FBB = TBB;
        TBB = I->getOperand(0).getMBB();
        Cond.push_back(MachineOperand::CreateImm(I->getOpcode()));
      }
      continue;
    }

    return true; // Unknown terminator
  }

  return false;
}

unsigned W65816InstrInfo::insertBranch(MachineBasicBlock &MBB,
                                       MachineBasicBlock *TBB,
                                       MachineBasicBlock *FBB,
                                       ArrayRef<MachineOperand> Cond,
                                       const DebugLoc &DL,
                                       int *BytesAdded) const {
  if (BytesAdded)
    *BytesAdded = 0;

  // Unconditional branch
  if (Cond.empty()) {
    BuildMI(&MBB, DL, get(W65816::BRA)).addMBB(TBB);
    if (BytesAdded)
      *BytesAdded = 2;
    return 1;
  }

  // Conditional branch
  unsigned Opc = Cond[0].getImm();
  BuildMI(&MBB, DL, get(Opc)).addMBB(TBB);
  if (BytesAdded)
    *BytesAdded = 2;

  if (FBB) {
    BuildMI(&MBB, DL, get(W65816::BRA)).addMBB(FBB);
    if (BytesAdded)
      *BytesAdded += 2;
    return 2;
  }

  return 1;
}

unsigned W65816InstrInfo::removeBranch(MachineBasicBlock &MBB,
                                       int *BytesRemoved) const {
  if (BytesRemoved)
    *BytesRemoved = 0;

  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;

  while (I != MBB.begin()) {
    --I;
    if (I->isDebugInstr())
      continue;
    if (!I->isBranch())
      break;

    if (BytesRemoved)
      *BytesRemoved += 2; // Assume 2 bytes per branch

    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }

  return Count;
}

bool W65816InstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  if (Cond.empty())
    return true;

  unsigned Opc = Cond[0].getImm();
  unsigned NewOpc;

  switch (Opc) {
  case W65816::BEQ:
    NewOpc = W65816::BNE;
    break;
  case W65816::BNE:
    NewOpc = W65816::BEQ;
    break;
  case W65816::BCS:
    NewOpc = W65816::BCC;
    break;
  case W65816::BCC:
    NewOpc = W65816::BCS;
    break;
  case W65816::BMI:
    NewOpc = W65816::BPL;
    break;
  case W65816::BPL:
    NewOpc = W65816::BMI;
    break;
  case W65816::BVS:
    NewOpc = W65816::BVC;
    break;
  case W65816::BVC:
    NewOpc = W65816::BVS;
    break;
  default:
    return true;
  }

  Cond[0].setImm(NewOpc);
  return false;
}

unsigned W65816InstrInfo::getInstSizeInBytes(const MachineInstr &MI) const {
  unsigned Opcode = MI.getOpcode();

  switch (Opcode) {
  default: {
    const MCInstrDesc &Desc = get(Opcode);
    return Desc.getSize();
  }
  case TargetOpcode::EH_LABEL:
  case TargetOpcode::IMPLICIT_DEF:
  case TargetOpcode::KILL:
  case TargetOpcode::DBG_VALUE:
    return 0;
  case TargetOpcode::INLINEASM:
  case TargetOpcode::INLINEASM_BR: {
    const MachineFunction &MF = *MI.getParent()->getParent();
    const TargetInstrInfo &TII = *STI.getInstrInfo();
    return TII.getInlineAsmLength(MI.getOperand(0).getSymbolName(),
                                  *MF.getTarget().getMCAsmInfo());
  }
  }
}

MachineBasicBlock *
W65816InstrInfo::getBranchDestBlock(const MachineInstr &MI) const {
  unsigned Opcode = MI.getOpcode();
  switch (Opcode) {
  case W65816::BRA:
  case W65816::BRL:
  case W65816::BEQ:
  case W65816::BNE:
  case W65816::BCS:
  case W65816::BCC:
  case W65816::BMI:
  case W65816::BPL:
  case W65816::BVS:
  case W65816::BVC:
  case W65816::JMP_abs:
  case W65816::BR_CC:
    return MI.getOperand(0).getMBB();
  default:
    llvm_unreachable("Unknown branch instruction");
  }
}

bool W65816InstrInfo::isBranchOffsetInRange(unsigned BranchOpc,
                                            int64_t Offset) const {
  switch (BranchOpc) {
  default:
    llvm_unreachable("Unknown branch instruction");
  case W65816::BRA:
  case W65816::BEQ:
  case W65816::BNE:
  case W65816::BCS:
  case W65816::BCC:
  case W65816::BMI:
  case W65816::BPL:
  case W65816::BVS:
  case W65816::BVC:
    // 8-bit signed offset: -128 to +127
    return Offset >= -128 && Offset <= 127;
  case W65816::BRL:
    // 16-bit signed offset
    return Offset >= -32768 && Offset <= 32767;
  case W65816::JMP_abs:
    // JMP can reach any address in the current bank
    return true;
  }
}

void W65816InstrInfo::insertIndirectBranch(MachineBasicBlock &MBB,
                                           MachineBasicBlock &NewDestBB,
                                           MachineBasicBlock &RestoreBB,
                                           const DebugLoc &DL, int64_t BrOffset,
                                           RegScavenger *RS) const {
  // For the W65816, when a branch is out of range, we use JMP instead
  // JMP can reach any address within the current 64KB bank
  BuildMI(&MBB, DL, get(W65816::JMP_abs)).addMBB(&NewDestBB);
}
