//===-- W65816PeepholeOpt.cpp - Peephole optimizations --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that performs peephole optimizations on W65816
// machine code. This includes eliminating redundant register transfers.
//
// Patterns eliminated:
// - TAX; TXA -> (delete both, value already in A)
// - TXA; TAX -> (delete both, value already in X)
// - TAY; TYA -> (delete both, value already in A)
// - TYA; TAY -> (delete both, value already in Y)
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

    // Check for redundant transfer pairs
    unsigned Opcode = MI.getOpcode();
    unsigned NextOpcode = NextMI.getOpcode();

    if (isRedundantTransferPair(Opcode, NextOpcode)) {
      // Remove both instructions
      LLVM_DEBUG(dbgs() << "Removing redundant transfer pair:\n  "
                        << MI << "  " << NextMI);

      auto AfterNext = std::next(NextMBBI);
      MBB.erase(MBBI);
      MBB.erase(NextMBBI);
      MBBI = AfterNext;
      Modified = true;
      continue;
    }

    ++MBBI;
  }

  return Modified;
}

bool W65816PeepholeOpt::runOnMachineFunction(MachineFunction &MF) {
  bool Modified = false;

  for (MachineBasicBlock &MBB : MF) {
    Modified |= optimizeMBB(MBB);
  }

  return Modified;
}

} // end anonymous namespace

INITIALIZE_PASS(W65816PeepholeOpt, DEBUG_TYPE, W65816_PEEPHOLE_OPT_NAME, false,
                false)

FunctionPass *llvm::createW65816PeepholeOptPass() {
  return new W65816PeepholeOpt();
}
