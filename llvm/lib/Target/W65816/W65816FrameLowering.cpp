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
#include "W65816MachineFunctionInfo.h"
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

//===----------------------------------------------------------------------===//
// Recursive Function Detection and Imaginary Register Save/Restore
//===----------------------------------------------------------------------===//

/// Check if a function is recursive (calls itself directly).
static bool isRecursiveFunction(const MachineFunction &MF) {
  StringRef FuncName = MF.getName();

  for (const MachineBasicBlock &MBB : MF) {
    for (const MachineInstr &MI : MBB) {
      if (MI.isCall()) {
        for (const MachineOperand &MO : MI.operands()) {
          if (MO.isGlobal()) {
            if (const Function *F = dyn_cast<Function>(MO.getGlobal())) {
              if (F->getName() == FuncName)
                return true;
            }
          } else if (MO.isSymbol()) {
            if (StringRef(MO.getSymbolName()) == FuncName)
              return true;
          }
        }
      }
    }
  }
  return false;
}

/// Get the list of imaginary register DP addresses used by the function.
/// Returns addresses in order: $10, $12, $14, etc.
static SmallVector<unsigned, 16> getUsedImaginaryRegAddrs(const MachineFunction &MF) {
  SmallVector<unsigned, 16> UsedAddrs;
  const MachineRegisterInfo &MRI = MF.getRegInfo();

  // List of imaginary registers and their DP addresses
  static const struct {
    unsigned Reg;
    unsigned Addr;
  } ImagRegs[] = {
    { W65816::RS0,  0x10 }, { W65816::RS1,  0x12 },
    { W65816::RS2,  0x14 }, { W65816::RS3,  0x16 },
    { W65816::RS4,  0x18 }, { W65816::RS5,  0x1A },
    { W65816::RS6,  0x1C }, { W65816::RS7,  0x1E },
    { W65816::RS8,  0x20 }, { W65816::RS9,  0x22 },
    { W65816::RS10, 0x24 }, { W65816::RS11, 0x26 },
    { W65816::RS12, 0x28 }, { W65816::RS13, 0x2A },
    { W65816::RS14, 0x2C }, { W65816::RS15, 0x2E },
  };

  for (const auto &IR : ImagRegs) {
    if (!MRI.reg_nodbg_empty(IR.Reg)) {
      UsedAddrs.push_back(IR.Addr);
    }
  }

  return UsedAddrs;
}

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
  const W65816MachineFunctionInfo *AFI =
      MF.getInfo<W65816MachineFunctionInfo>();
  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc DL;

  // Skip any debug instructions at the beginning
  while (MBBI != MBB.end() && MBBI->isDebugInstr())
    ++MBBI;

  // Handle Direct Page frame functions
  // These use the 256-byte direct page region instead of the stack for locals
  if (AFI->usesDPFrame()) {
    // If assume-d0 is set, D register is guaranteed to be 0, skip setup
    if (!STI.assumeD0()) {
      // Save D register (callee-saved requirement)
      BuildMI(MBB, MBBI, DL, TII.get(W65816::PHD))
          .setMIFlag(MachineInstr::FrameSetup);
      // Set D to 0 (DP base at $0000)
      BuildMI(MBB, MBBI, DL, TII.get(W65816::LDA_imm16), W65816::A)
          .addImm(0)
          .setMIFlag(MachineInstr::FrameSetup);
      BuildMI(MBB, MBBI, DL, TII.get(W65816::TCD))
          .setMIFlag(MachineInstr::FrameSetup);
    }
    // DP frame functions don't use stack allocation for locals
    return;
  }

  // For interrupt handlers, save all registers first
  // The 65816 interrupt sequence automatically pushes P and PC,
  // but we need to save A, X, Y ourselves
  if (AFI->isInterruptOrNMIHandler()) {
    // Save A, X, Y (P is already saved by hardware on interrupt entry)
    // Use REP #$30 first to ensure we're in 16-bit mode for saving
    BuildMI(MBB, MBBI, DL, TII.get(W65816::REP)).addImm(0x30)
        .setMIFlag(MachineInstr::FrameSetup);
    BuildMI(MBB, MBBI, DL, TII.get(W65816::PHA))
        .setMIFlag(MachineInstr::FrameSetup);
    BuildMI(MBB, MBBI, DL, TII.get(W65816::PHX))
        .setMIFlag(MachineInstr::FrameSetup);
    BuildMI(MBB, MBBI, DL, TII.get(W65816::PHY))
        .setMIFlag(MachineInstr::FrameSetup);
  }

  // For recursive functions, save imaginary registers in prologue.
  // This treats them like callee-saved registers, preserving values across
  // recursive calls. Each stack frame saves the imaginary registers it inherited
  // and restores them before returning.
  //
  // We use PEI (Push Effective Indirect) which pushes from a DP address
  // without clobbering A, X, or Y (important because they may hold arguments).
  if (isRecursiveFunction(MF)) {
    SmallVector<unsigned, 16> ImagRegAddrs = getUsedImaginaryRegAddrs(MF);
    for (unsigned Addr : ImagRegAddrs) {
      BuildMI(MBB, MBBI, DL, TII.get(W65816::PEI))
          .addImm(Addr)
          .setMIFlag(MachineInstr::FrameSetup);
    }
  }

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
  const W65816MachineFunctionInfo *AFI =
      MF.getInfo<W65816MachineFunctionInfo>();

  MachineBasicBlock::iterator MBBI = MBB.getFirstTerminator();
  DebugLoc DL;

  if (MBBI != MBB.end())
    DL = MBBI->getDebugLoc();

  // Handle Direct Page frame functions - restore D register
  if (AFI->usesDPFrame()) {
    // If assume-d0 is set, D register was never modified, skip restore
    if (!STI.assumeD0()) {
      BuildMI(MBB, MBBI, DL, TII.get(W65816::PLD))
          .setMIFlag(MachineInstr::FrameDestroy);
    }
    return;
  }

  uint64_t StackSize = MFI.getStackSize();

  // Check if this is a recursive function with imaginary registers to restore
  bool IsRecursive = isRecursiveFunction(MF);
  SmallVector<unsigned, 16> ImagRegAddrs;
  if (IsRecursive) {
    ImagRegAddrs = getUsedImaginaryRegAddrs(MF);
  }

  // For interrupt handlers with no stack allocation, still need to restore regs
  // Also continue if we have imaginary registers to restore for recursive functions
  if (StackSize == 0 && !AFI->isInterruptOrNMIHandler() && ImagRegAddrs.empty())
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
    //
    // Use TAY/TYA to save the return value (like small frame case).
    // The old PHA/PLA approach was broken because the stack adjustment
    // moved SP past the pushed value, causing PLA to pop from wrong location.

    // Save return value (A) to Y temporarily
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TAY));

    // TSX - Transfer SP to X
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TSX));

    // TXA - Transfer X to A
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TXA));

    // CLC - Clear Carry for addition
    BuildMI(MBB, MBBI, DL, TII.get(W65816::CLC));

    // ADC #StackSize+2 - Add stack size plus 2 for prologue PHA
    // Prologue: PHA (2 bytes) + SBC #StackSize (StackSize bytes) = StackSize + 2
    BuildMI(MBB, MBBI, DL, TII.get(W65816::ADC_imm16), W65816::A)
        .addImm(StackSize + 2);

    // TAX - Transfer A to X
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TAX));

    // TXS - Transfer X to SP
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TXS));

    // Restore return value from Y
    BuildMI(MBB, MBBI, DL, TII.get(W65816::TYA));
  }

  // For recursive functions, restore imaginary registers from the stack.
  // These were saved in the prologue using PEI. Now we pop them back using
  // PLA + STA, being careful to preserve the return value in A.
  if (!ImagRegAddrs.empty()) {
    // Re-get the terminator position after any stack cleanup
    MachineBasicBlock::iterator TermMBBI = MBB.getFirstTerminator();

    // Save return value (A) to X temporarily
    BuildMI(MBB, TermMBBI, DL, TII.get(W65816::TAX))
        .setMIFlag(MachineInstr::FrameDestroy);

    // Pop and restore imaginary registers in reverse order (LIFO)
    for (auto It = ImagRegAddrs.rbegin(); It != ImagRegAddrs.rend(); ++It) {
      BuildMI(MBB, TermMBBI, DL, TII.get(W65816::PLA))
          .setMIFlag(MachineInstr::FrameDestroy);
      BuildMI(MBB, TermMBBI, DL, TII.get(W65816::STA_dp))
          .addReg(W65816::A)
          .addImm(*It)
          .setMIFlag(MachineInstr::FrameDestroy);
    }

    // Restore return value from X
    BuildMI(MBB, TermMBBI, DL, TII.get(W65816::TXA))
        .setMIFlag(MachineInstr::FrameDestroy);
  }

  // For interrupt handlers, restore saved registers
  // Note: RTI (instead of RTS) is handled in ExpandPseudo pass
  if (AFI->isInterruptOrNMIHandler()) {
    // Re-get the terminator position after any stack cleanup
    MachineBasicBlock::iterator TermMBBI = MBB.getFirstTerminator();

    // Ensure we're in 16-bit mode for restoring
    BuildMI(MBB, TermMBBI, DL, TII.get(W65816::REP)).addImm(0x30)
        .setMIFlag(MachineInstr::FrameDestroy);
    // Restore Y, X, A (reverse order of save)
    BuildMI(MBB, TermMBBI, DL, TII.get(W65816::PLY))
        .setMIFlag(MachineInstr::FrameDestroy);
    BuildMI(MBB, TermMBBI, DL, TII.get(W65816::PLX))
        .setMIFlag(MachineInstr::FrameDestroy);
    BuildMI(MBB, TermMBBI, DL, TII.get(W65816::PLA))
        .setMIFlag(MachineInstr::FrameDestroy);
  }
}

MachineBasicBlock::iterator W65816FrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator MI) const {
  // With hasReservedCallFrame() = true, the call frame space is pre-allocated
  // in the prologue. ADJCALLSTACKDOWN/UP become no-ops.
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

void W65816FrameLowering::processFunctionBeforeFrameFinalized(
    MachineFunction &MF, RegScavenger *RS) const {
  // Note: DP frame size validation is done in W65816RegisterInfo::eliminateFrameIndex
  // because the final frame object offsets are only known at that point.
}
