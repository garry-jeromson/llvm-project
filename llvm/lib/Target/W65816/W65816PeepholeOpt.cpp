//===-- W65816PeepholeOpt.cpp - Peephole optimizations --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that performs peephole optimizations on W65816
// machine code. This includes:
//
// 1. Eliminating redundant register transfers:
//    - TAX; TXA -> (delete both, value already in A)
//    - TXA; TAX -> (delete both, value already in X)
//    - TAY; TYA -> (delete both, value already in A)
//    - TYA; TAY -> (delete both, value already in Y)
//
// 2. Eliminating redundant branches:
//    - BRA to fall-through block -> (delete, execution falls through anyway)
//
// 3. Eliminating redundant flag operations:
//    - CLC; CLC -> CLC (remove duplicate)
//    - SEC; SEC -> SEC (remove duplicate)
//    - SEC; CLC -> CLC (first is cancelled by second)
//    - CLC; SEC -> SEC (first is cancelled by second)
//
// 4. Eliminating redundant push/pop pairs:
//    - PHA; PLA -> (delete both, value stays in A)
//    - PHX; PLX -> (delete both, value stays in X)
//    - PHY; PLY -> (delete both, value stays in Y)
//
// 5. Eliminating redundant increment/decrement pairs:
//    - INX; DEX -> (delete both)
//    - DEX; INX -> (delete both)
//    - INY; DEY -> (delete both)
//    - DEY; INY -> (delete both)
//
// 6. Simplifying load-transfer sequences:
//    - LDA #imm; TAX -> LDX #imm (direct load is more efficient)
//    - LDA #imm; TAY -> LDY #imm (direct load is more efficient)
//
// 7. Eliminating redundant SEP/REP mode switches:
//    - REP #N; SEP #N -> SEP #N (first is cancelled by second)
//    - SEP #N; REP #N -> REP #N (first is cancelled by second)
//
// 8. Eliminating redundant DP load/store sequences:
//    - STA dpN; LDA dpN -> (delete LDA, value already in A)
//    - LDA dpN; STA dpN -> (delete both, no-op)
//
// 9. Optimizing zero stores:
//    - LDA #0; STA addr -> STZ addr (direct zero store)
//
// 10. Optimizing increment/decrement by 1:
//    - CLC; ADC #1 -> INC A (saves 2 bytes, 2 cycles)
//    - SEC; SBC #1 -> DEC A (saves 2 bytes, 2 cycles)
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/W65816MCTargetDesc.h"
#include "W65816.h"
#include "W65816InstrInfo.h"

#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"

using namespace llvm;

#define DEBUG_TYPE "w65816-peephole"
#define W65816_PEEPHOLE_OPT_NAME "W65816 peephole optimization pass"

namespace {

class W65816PeepholeOpt : public MachineFunctionPass {
public:
  static char ID;

  W65816PeepholeOpt() : MachineFunctionPass(ID) {
    initializeW65816PeepholeOptPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  StringRef getPassName() const override { return W65816_PEEPHOLE_OPT_NAME; }

private:
  bool optimizeMBB(MachineBasicBlock &MBB);
  bool isRedundantTransferPair(unsigned First, unsigned Second);
  bool removeRedundantBranches(MachineFunction &MF);
};

char W65816PeepholeOpt::ID = 0;

/// Check if two transfer opcodes form a redundant pair.
/// Returns true for: TAX/TXA, TXA/TAX, TAY/TYA, TYA/TAY
bool W65816PeepholeOpt::isRedundantTransferPair(unsigned First,
                                                unsigned Second) {
  return (First == W65816::TAX && Second == W65816::TXA) ||
         (First == W65816::TXA && Second == W65816::TAX) ||
         (First == W65816::TAY && Second == W65816::TYA) ||
         (First == W65816::TYA && Second == W65816::TAY);
}

/// Check if two opcodes form a redundant push/pop pair.
/// Returns true for: PHA/PLA, PHX/PLX, PHY/PLY
static bool isRedundantPushPopPair(unsigned First, unsigned Second) {
  return (First == W65816::PHA && Second == W65816::PLA) ||
         (First == W65816::PHX && Second == W65816::PLX) ||
         (First == W65816::PHY && Second == W65816::PLY);
}

/// Check if opcode is a duplicate flag operation (CLC or SEC).
static bool isDuplicateFlagOp(unsigned First, unsigned Second) {
  return (First == W65816::CLC && Second == W65816::CLC) ||
         (First == W65816::SEC && Second == W65816::SEC);
}

/// Check if two flag operations cancel each other out.
/// SEC followed by CLC means the SEC was pointless (and vice versa).
static bool isCancellingFlagOp(unsigned First, unsigned Second) {
  return (First == W65816::SEC && Second == W65816::CLC) ||
         (First == W65816::CLC && Second == W65816::SEC);
}

/// Check if two opcodes form a redundant increment/decrement pair.
/// Returns true for: INX/DEX, DEX/INX, INY/DEY, DEY/INY
static bool isRedundantIncDecPair(unsigned First, unsigned Second) {
  return (First == W65816::INX && Second == W65816::DEX) ||
         (First == W65816::DEX && Second == W65816::INX) ||
         (First == W65816::INY && Second == W65816::DEY) ||
         (First == W65816::DEY && Second == W65816::INY);
}

/// Check if two SEP/REP operations cancel each other out.
/// REP followed by SEP with same mask means REP was pointless (and vice versa).
static bool isCancellingSepRepPair(MachineInstr &First, MachineInstr &Second) {
  unsigned FirstOp = First.getOpcode();
  unsigned SecondOp = Second.getOpcode();

  // Check for REP/SEP or SEP/REP with same immediate value
  if ((FirstOp == W65816::REP && SecondOp == W65816::SEP) ||
      (FirstOp == W65816::SEP && SecondOp == W65816::REP)) {
    if (First.getNumOperands() > 0 && Second.getNumOperands() > 0 &&
        First.getOperand(0).isImm() && Second.getOperand(0).isImm()) {
      return First.getOperand(0).getImm() == Second.getOperand(0).getImm();
    }
  }
  return false;
}

/// Check if instruction is LDA #0 (immediate zero load)
static bool isLoadImmZero(MachineInstr &MI) {
  if (MI.getOpcode() != W65816::LDA_imm16)
    return false;
  if (MI.getNumOperands() > 0 && MI.getOperand(0).isImm())
    return MI.getOperand(0).getImm() == 0;
  return false;
}

/// Check if instruction is ADC #1 or SBC #1 (immediate add/sub 1)
static bool isAddSubImm1(MachineInstr &MI, bool &IsAdd) {
  unsigned Opcode = MI.getOpcode();
  if (Opcode == W65816::ADC_imm16) {
    IsAdd = true;
  } else if (Opcode == W65816::SBC_imm16) {
    IsAdd = false;
  } else {
    return false;
  }
  if (MI.getNumOperands() > 0 && MI.getOperand(0).isImm())
    return MI.getOperand(0).getImm() == 1;
  return false;
}

/// Get the DP address from a DP instruction, or -1 if not a DP instruction.
static int getDPAddress(MachineInstr &MI) {
  unsigned Opcode = MI.getOpcode();
  if (Opcode == W65816::STA_dp || Opcode == W65816::LDA_dp) {
    // DP instructions have the address as an operand
    // STA_dp: (outs), (ins ACC16:$src, addr8dp:$imm)
    // LDA_dp: (outs ACC16:$dst), (ins addr8dp:$imm)
    unsigned OpIdx = (Opcode == W65816::STA_dp) ? 1 : 0;
    if (MI.getNumOperands() > OpIdx && MI.getOperand(OpIdx).isImm())
      return MI.getOperand(OpIdx).getImm();
  }
  return -1;
}

bool W65816PeepholeOpt::optimizeMBB(MachineBasicBlock &MBB) {
  bool Modified = false;

  auto MBBI = MBB.begin();
  while (MBBI != MBB.end()) {
    MachineInstr &MI = *MBBI;
    auto NextMBBI = std::next(MBBI);

    // Skip if we're at the last instruction
    if (NextMBBI == MBB.end()) {
      ++MBBI;
      continue;
    }

    MachineInstr &NextMI = *NextMBBI;
    unsigned Opcode = MI.getOpcode();
    unsigned NextOpcode = NextMI.getOpcode();

    // Check for redundant transfer pairs (TAX/TXA, TAY/TYA, etc.)
    if (isRedundantTransferPair(Opcode, NextOpcode)) {
      LLVM_DEBUG(dbgs() << "Removing redundant transfer pair:\n  " << MI << "  "
                        << NextMI);
      auto AfterNext = std::next(NextMBBI);
      MBB.erase(MBBI);
      MBB.erase(NextMBBI);
      MBBI = AfterNext;
      Modified = true;
      continue;
    }

    // Check for redundant push/pop pairs (PHA/PLA, PHX/PLX, PHY/PLY)
    if (isRedundantPushPopPair(Opcode, NextOpcode)) {
      LLVM_DEBUG(dbgs() << "Removing redundant push/pop pair:\n  " << MI << "  "
                        << NextMI);
      auto AfterNext = std::next(NextMBBI);
      MBB.erase(MBBI);
      MBB.erase(NextMBBI);
      MBBI = AfterNext;
      Modified = true;
      continue;
    }

    // Check for duplicate flag operations (CLC/CLC, SEC/SEC)
    if (isDuplicateFlagOp(Opcode, NextOpcode)) {
      LLVM_DEBUG(dbgs() << "Removing duplicate flag operation:\n  " << NextMI);
      MBB.erase(NextMBBI);
      // Don't advance MBBI - check if there are more duplicates
      Modified = true;
      continue;
    }

    // Check for cancelling flag operations (SEC/CLC, CLC/SEC)
    // The first instruction is pointless since the second overwrites it
    if (isCancellingFlagOp(Opcode, NextOpcode)) {
      LLVM_DEBUG(dbgs() << "Removing cancelled flag operation:\n  " << MI);
      MBB.erase(MBBI);
      MBBI = NextMBBI;
      Modified = true;
      continue;
    }

    // Check for redundant increment/decrement pairs (INX/DEX, INY/DEY, etc.)
    if (isRedundantIncDecPair(Opcode, NextOpcode)) {
      LLVM_DEBUG(dbgs() << "Removing redundant inc/dec pair:\n  " << MI << "  "
                        << NextMI);
      auto AfterNext = std::next(NextMBBI);
      MBB.erase(MBBI);
      MBB.erase(NextMBBI);
      MBBI = AfterNext;
      Modified = true;
      continue;
    }

    // Check for LDA #imm; TAX -> LDX #imm optimization
    if (Opcode == W65816::LDA_imm16 && NextOpcode == W65816::TAX) {
      // Verify LDA has an immediate operand
      if (MI.getNumOperands() > 0 && MI.getOperand(0).isImm()) {
        LLVM_DEBUG(dbgs() << "Replacing LDA #imm; TAX with LDX #imm:\n  " << MI
                          << "  " << NextMI);
        // Replace LDA with LDX (keeps the same immediate operand)
        MI.setDesc(MBB.getParent()->getSubtarget().getInstrInfo()->get(
            W65816::LDX_imm16));
        // Remove TAX
        MBB.erase(NextMBBI);
        Modified = true;
        // Don't advance - check for more optimizations on this instruction
        continue;
      }
    }

    // Check for LDA #imm; TAY -> LDY #imm optimization
    if (Opcode == W65816::LDA_imm16 && NextOpcode == W65816::TAY) {
      // Verify LDA has an immediate operand
      if (MI.getNumOperands() > 0 && MI.getOperand(0).isImm()) {
        LLVM_DEBUG(dbgs() << "Replacing LDA #imm; TAY with LDY #imm:\n  " << MI
                          << "  " << NextMI);
        // Replace LDA with LDY (keeps the same immediate operand)
        MI.setDesc(MBB.getParent()->getSubtarget().getInstrInfo()->get(
            W65816::LDY_imm16));
        // Remove TAY
        MBB.erase(NextMBBI);
        Modified = true;
        // Don't advance - check for more optimizations on this instruction
        continue;
      }
    }

    // Check for cancelling SEP/REP pairs (REP #N; SEP #N or SEP #N; REP #N)
    // The first instruction is pointless since the second overwrites it
    if (isCancellingSepRepPair(MI, NextMI)) {
      LLVM_DEBUG(dbgs() << "Removing cancelled SEP/REP operation:\n  " << MI);
      MBB.erase(MBBI);
      MBBI = NextMBBI;
      Modified = true;
      continue;
    }

    // Check for redundant DP store/load pairs (STA dpN; LDA dpN)
    // The load is pointless since the value is already in A
    if (Opcode == W65816::STA_dp && NextOpcode == W65816::LDA_dp) {
      int StoreAddr = getDPAddress(MI);
      int LoadAddr = getDPAddress(NextMI);
      if (StoreAddr >= 0 && StoreAddr == LoadAddr) {
        LLVM_DEBUG(dbgs() << "Removing redundant LDA after STA to same DP:\n  "
                          << NextMI);
        MBB.erase(NextMBBI);
        // Don't advance - check for more optimizations
        Modified = true;
        continue;
      }
    }

    // Check for redundant DP load/store pairs (LDA dpN; STA dpN)
    // This is a no-op - delete both
    if (Opcode == W65816::LDA_dp && NextOpcode == W65816::STA_dp) {
      int LoadAddr = getDPAddress(MI);
      int StoreAddr = getDPAddress(NextMI);
      if (LoadAddr >= 0 && LoadAddr == StoreAddr) {
        LLVM_DEBUG(dbgs() << "Removing redundant LDA/STA pair to same DP:\n  "
                          << MI << "  " << NextMI);
        auto AfterNext = std::next(NextMBBI);
        MBB.erase(MBBI);
        MBB.erase(NextMBBI);
        MBBI = AfterNext;
        Modified = true;
        continue;
      }
    }

    // Check for LDA #0; STA addr -> STZ addr optimization
    if (isLoadImmZero(MI) && NextOpcode == W65816::STA_abs) {
      LLVM_DEBUG(dbgs() << "Replacing LDA #0; STA with STZ:\n  " << MI << "  "
                        << NextMI);
      const TargetInstrInfo *TII =
          MBB.getParent()->getSubtarget().getInstrInfo();
      // Get the address operand from STA
      MachineOperand &AddrOp = NextMI.getOperand(1);
      // Build STZ instruction
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(W65816::STZ_abs))
          .add(AddrOp);
      // Remove both LDA and STA
      auto AfterNext = std::next(NextMBBI);
      MBB.erase(MBBI);
      MBB.erase(NextMBBI);
      MBBI = AfterNext;
      Modified = true;
      continue;
    }

    // Check for LDA #0; STA dp -> STZ dp optimization
    if (isLoadImmZero(MI) && NextOpcode == W65816::STA_dp) {
      LLVM_DEBUG(dbgs() << "Replacing LDA #0; STA dp with STZ dp:\n  " << MI
                        << "  " << NextMI);
      const TargetInstrInfo *TII =
          MBB.getParent()->getSubtarget().getInstrInfo();
      // Get the address operand from STA (operand 1)
      MachineOperand &AddrOp = NextMI.getOperand(1);
      // Build STZ instruction
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(W65816::STZ_dp))
          .add(AddrOp);
      // Remove both LDA and STA
      auto AfterNext = std::next(NextMBBI);
      MBB.erase(MBBI);
      MBB.erase(NextMBBI);
      MBBI = AfterNext;
      Modified = true;
      continue;
    }

    // Check for CLC; ADC #1 -> INC A optimization
    if (Opcode == W65816::CLC) {
      bool IsAdd;
      if (isAddSubImm1(NextMI, IsAdd) && IsAdd) {
        LLVM_DEBUG(dbgs() << "Replacing CLC; ADC #1 with INC A:\n  " << MI
                          << "  " << NextMI);
        const TargetInstrInfo *TII =
            MBB.getParent()->getSubtarget().getInstrInfo();
        // INC_A takes ACC16 input and produces ACC16 output
        // The ADC instruction uses A implicitly, so we do the same
        BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(W65816::INC_A))
            .addReg(W65816::A)
            .addReg(W65816::A);
        // Remove both CLC and ADC
        auto AfterNext = std::next(NextMBBI);
        MBB.erase(MBBI);
        MBB.erase(NextMBBI);
        MBBI = AfterNext;
        Modified = true;
        continue;
      }
    }

    // Check for SEC; SBC #1 -> DEC A optimization
    if (Opcode == W65816::SEC) {
      bool IsAdd;
      if (isAddSubImm1(NextMI, IsAdd) && !IsAdd) {
        LLVM_DEBUG(dbgs() << "Replacing SEC; SBC #1 with DEC A:\n  " << MI
                          << "  " << NextMI);
        const TargetInstrInfo *TII =
            MBB.getParent()->getSubtarget().getInstrInfo();
        // DEC_A takes ACC16 input and produces ACC16 output
        BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(W65816::DEC_A))
            .addReg(W65816::A)
            .addReg(W65816::A);
        // Remove both SEC and SBC
        auto AfterNext = std::next(NextMBBI);
        MBB.erase(MBBI);
        MBB.erase(NextMBBI);
        MBBI = AfterNext;
        Modified = true;
        continue;
      }
    }

    ++MBBI;
  }

  return Modified;
}

/// Remove BRA instructions that branch to the fall-through block.
/// These are redundant since execution would fall through anyway.
bool W65816PeepholeOpt::removeRedundantBranches(MachineFunction &MF) {
  bool Modified = false;

  for (auto MBBI = MF.begin(), MBBE = MF.end(); MBBI != MBBE; ++MBBI) {
    MachineBasicBlock &MBB = *MBBI;

    // Get the next block in layout order
    auto NextMBBI = std::next(MBBI);
    if (NextMBBI == MBBE)
      continue;

    MachineBasicBlock *NextMBB = &*NextMBBI;

    // Check if the last instruction is a BRA to the next block
    if (MBB.empty())
      continue;

    MachineInstr &LastMI = MBB.back();
    if (LastMI.getOpcode() != W65816::BRA)
      continue;

    // Check if the branch target is the next block
    if (LastMI.getNumOperands() > 0 && LastMI.getOperand(0).isMBB()) {
      MachineBasicBlock *TargetMBB = LastMI.getOperand(0).getMBB();
      if (TargetMBB == NextMBB) {
        LLVM_DEBUG(dbgs() << "Removing redundant BRA to fall-through block\n");
        LastMI.eraseFromParent();
        Modified = true;
      }
    }
  }

  return Modified;
}

bool W65816PeepholeOpt::runOnMachineFunction(MachineFunction &MF) {
  bool Modified = false;

  // First pass: optimize within each basic block
  for (MachineBasicBlock &MBB : MF) {
    Modified |= optimizeMBB(MBB);
  }

  // Second pass: remove redundant branches between blocks
  Modified |= removeRedundantBranches(MF);

  return Modified;
}

} // end anonymous namespace

INITIALIZE_PASS(W65816PeepholeOpt, DEBUG_TYPE, W65816_PEEPHOLE_OPT_NAME, false,
                false)

FunctionPass *llvm::createW65816PeepholeOptPass() {
  return new W65816PeepholeOpt();
}
