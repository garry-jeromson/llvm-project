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
//===----------------------------------------------------------------------===//

#include "W65816.h"
#include "W65816InstrInfo.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"

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
bool W65816PeepholeOpt::isRedundantTransferPair(unsigned First, unsigned Second) {
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
      LLVM_DEBUG(dbgs() << "Removing redundant transfer pair:\n  "
                        << MI << "  " << NextMI);
      auto AfterNext = std::next(NextMBBI);
      MBB.erase(MBBI);
      MBB.erase(NextMBBI);
      MBBI = AfterNext;
      Modified = true;
      continue;
    }

    // Check for redundant push/pop pairs (PHA/PLA, PHX/PLX, PHY/PLY)
    if (isRedundantPushPopPair(Opcode, NextOpcode)) {
      LLVM_DEBUG(dbgs() << "Removing redundant push/pop pair:\n  "
                        << MI << "  " << NextMI);
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
