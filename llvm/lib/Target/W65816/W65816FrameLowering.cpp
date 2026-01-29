//===-- W65816FrameLowering.cpp - W65816 Frame Information ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the W65816 implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "W65816FrameLowering.h"

#include "W65816.h"
#include "W65816InstrInfo.h"
#include "W65816Subtarget.h"
#include "W65816TargetMachine.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"

#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

W65816FrameLowering::W65816FrameLowering()
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown,
                          /*StackAlignment=*/Align(2),
                          /*LocalAreaOffset=*/0,
                          /*TransientStackAlignment=*/Align(2)) {}

bool W65816FrameLowering::hasFPImpl(const MachineFunction &MF) const {
  // For now, never use a frame pointer
  // The D register could be used as a frame pointer in the future
  return false;
}

void W65816FrameLowering::emitPrologue(MachineFunction &MF,
                                       MachineBasicBlock &MBB) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const W65816Subtarget &STI = MF.getSubtarget<W65816Subtarget>();
  const W65816InstrInfo &TII = *STI.getInstrInfo();
  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc DL;

  // Skip any debug instructions at the beginning
  while (MBBI != MBB.end() && MBBI->isDebugInstr())
    ++MBBI;

  // Set processor mode based on subtarget features
  // This ensures the hardware mode matches what the compiler expects
  // SEP #$20 sets M=1 (8-bit accumulator), SEP #$10 sets X=1 (8-bit index)
  unsigned ModeSetBits = 0;
  if (STI.uses8BitAccumulator())
    ModeSetBits |= 0x20;  // M flag
  if (STI.uses8BitIndex())
    ModeSetBits |= 0x10;  // X flag

  if (ModeSetBits != 0) {
    // Emit SEP to set the 8-bit mode(s)
    BuildMI(MBB, MBBI, DL, TII.get(W65816::SEP)).addImm(ModeSetBits);
  }

  // Get the number of bytes to allocate from the FrameInfo
  uint64_t StackSize = MFI.getStackSize();

  if (StackSize == 0)
    return;

  // Adjust the stack pointer by subtracting from it
  // W65816 doesn't have a direct subtract from SP instruction,
  // so we need to: TSX, TXA, SEC, SBC #imm, TAX, TXS
  // But this clobbers A and X which may be used for arguments!
  //
  // Alternative approach for small stack sizes: use PHA repeatedly
  // Each PHA decrements SP by 2 (in 16-bit mode)
  //
  // For now, use the transfer approach but note this is problematic
  // for functions with arguments in A/X

  if (StackSize <= 8) {
    // For small stack sizes, use PHA to adjust (each PHA = 2 bytes)
    // Note: this puts garbage on the stack but that's okay for space allocation
    unsigned PHACount = (StackSize + 1) / 2;
    for (unsigned i = 0; i < PHACount; ++i) {
      BuildMI(MBB, MBBI, DL, TII.get(W65816::PHA));
    }
  } else {
    // For larger stack sizes, use the TSX/arithmetic/TXS approach
    // Save A first if needed (push it)
    BuildMI(MBB, MBBI, DL, TII.get(W65816::PHA));

    // TSX - Transfer SP to X
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TSX));

    // TXA - Transfer X to A
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TXA));

    // SEC - Set Carry for subtraction
    BuildMI(MBB, MBBI, DL, TII.get(W65816::SEC));

    // SBC #StackSize - Subtract stack size
    BuildMI(MBB, MBBI, DL, TII.get(W65816::SBC_imm16), W65816::A)
        .addImm(StackSize);

    // TAX - Transfer A to X
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TAX));

    // TXS - Transfer X to SP
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TXS));

    // Restore A from where we pushed it (now at offset StackSize+2 from SP)
    BuildMI(MBB, MBBI, DL, TII.get(W65816::LDA_sr), W65816::A)
        .addImm(StackSize + 1);
  }
}

void W65816FrameLowering::emitEpilogue(MachineFunction &MF,
                                       MachineBasicBlock &MBB) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const W65816Subtarget &STI = MF.getSubtarget<W65816Subtarget>();
  const W65816InstrInfo &TII = *STI.getInstrInfo();

  MachineBasicBlock::iterator MBBI = MBB.getFirstTerminator();
  DebugLoc DL;

  if (MBBI != MBB.end())
    DL = MBBI->getDebugLoc();

  uint64_t StackSize = MFI.getStackSize();

  if (StackSize == 0)
    return;

  // Restore the stack pointer by adding back the stack size
  // Use PLA for small sizes, arithmetic for larger

  if (StackSize <= 8) {
    // For small stack sizes, use PLA to adjust
    // But we need to preserve A since it may contain the return value
    unsigned PLACount = (StackSize + 1) / 2;

    // Save return value (A) to Y temporarily
    // This is safe because Y is caller-saved and we're about to return
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TAY));

    for (unsigned i = 0; i < PLACount; ++i) {
      BuildMI(MBB, MBBI, DL, TII.get(W65816::PLA));
    }

    // Restore return value from Y
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TYA));
  } else {
    // For larger stack sizes, use TSX/arithmetic/TXS
    // We need to preserve A since it may contain the return value

    // Save return value (A) to stack temporarily
    BuildMI(MBB, MBBI, DL, TII.get(W65816::PHA));

    // TSX - Transfer SP to X
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TSX));

    // TXA - Transfer X to A
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TXA));

    // CLC - Clear Carry for addition
    BuildMI(MBB, MBBI, DL, TII.get(W65816::CLC));

    // ADC #StackSize - Add stack size (plus 2 for the PHA we just did)
    BuildMI(MBB, MBBI, DL, TII.get(W65816::ADC_imm16), W65816::A)
        .addImm(StackSize + 2);

    // TAX - Transfer A to X
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TAX));

    // TXS - Transfer X to SP
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TXS));

    // Restore return value (A) - now it's at offset 1 from SP
    BuildMI(MBB, MBBI, DL, TII.get(W65816::PLA));
  }
}

MachineBasicBlock::iterator W65816FrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator MI) const {
  // Simply remove ADJCALLSTACKDOWN and ADJCALLSTACKUP pseudos
  return MBB.erase(MI);
}

bool W65816FrameLowering::spillCalleeSavedRegisters(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    ArrayRef<CalleeSavedInfo> CSI, const TargetRegisterInfo *TRI) const {
  if (CSI.empty())
    return false;

  MachineFunction &MF = *MBB.getParent();
  const W65816Subtarget &STI = MF.getSubtarget<W65816Subtarget>();
  const W65816InstrInfo &TII = *STI.getInstrInfo();
  DebugLoc DL;

  if (MI != MBB.end())
    DL = MI->getDebugLoc();

  for (const CalleeSavedInfo &I : CSI) {
    Register Reg = I.getReg();

    // Use push instructions to save callee-saved registers
    if (Reg == W65816::D) {
      BuildMI(MBB, MI, DL, TII.get(W65816::PHD));
    }
  }

  return true;
}

bool W65816FrameLowering::restoreCalleeSavedRegisters(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    MutableArrayRef<CalleeSavedInfo> CSI, const TargetRegisterInfo *TRI) const {
  if (CSI.empty())
    return false;

  MachineFunction &MF = *MBB.getParent();
  const W65816Subtarget &STI = MF.getSubtarget<W65816Subtarget>();
  const W65816InstrInfo &TII = *STI.getInstrInfo();
  DebugLoc DL;

  if (MI != MBB.end())
    DL = MI->getDebugLoc();

  // Restore in reverse order
  for (const CalleeSavedInfo &I : reverse(CSI)) {
    Register Reg = I.getReg();

    if (Reg == W65816::D) {
      BuildMI(MBB, MI, DL, TII.get(W65816::PLD));
    }
  }

  return true;
}

void W65816FrameLowering::determineCalleeSaves(MachineFunction &MF,
                                               BitVector &SavedRegs,
                                               RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);

  // D register is callee-saved if used
  // This will be determined by the register allocator
}
