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
// 7. Eliminating and coalescing SEP/REP mode switches:
//    - REP #N; SEP #N -> SEP #N (first is cancelled by second)
//    - SEP #N; REP #N -> REP #N (first is cancelled by second)
//    - SEP #M; SEP #N -> SEP #(M|N) (coalesce consecutive SEP)
//    - REP #M; REP #N -> REP #(M|N) (coalesce consecutive REP)
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
// 11. Complementing conditional branch + BRA pairs:
//    - Bcond .next; BRA .other -> B!cond .other (when .next is fall-through)
//
// 12. Simplifying compound branch sequences:
//    - CMP #N; BEQ target; BCC target -> CMP #(N+1); BCC target (ULE to ULT)
//    - BEQ target; BCS target -> BCS target (BEQ redundant, UGE)
//
// 13. Eliminating redundant CMP #0:
//    - LDA ...; CMP #0; BEQ/BNE -> LDA ...; BEQ/BNE (flags already set)
//    - AND/ORA/EOR ...; CMP #0; BEQ/BNE -> AND/ORA/EOR ...; BEQ/BNE
//
// 14. Load-Use Folding (memory-direct operations):
//    - LDA dp; INC A; STA dp -> INC dp (when A is dead after)
//    - LDA dp; DEC A; STA dp -> DEC dp (when A is dead after)
//    - LDA dp; ASL A; STA dp -> ASL dp (when A is dead after)
//    - LDA dp; LSR A; STA dp -> LSR dp (when A is dead after)
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
  // The immediate operand position varies. Scan for it.
  for (unsigned i = 0; i < MI.getNumOperands(); ++i) {
    if (MI.getOperand(i).isImm())
      return MI.getOperand(i).getImm() == 0;
  }
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
  // The immediate operand position varies depending on how the instruction
  // was created (with or without tied source register). Scan for it.
  for (unsigned i = 0; i < MI.getNumOperands(); ++i) {
    if (MI.getOperand(i).isImm())
      return MI.getOperand(i).getImm() == 1;
  }
  return false;
}

/// Get the DP address from a DP instruction, or -1 if not a DP instruction.
static int getDPAddress(MachineInstr &MI) {
  unsigned Opcode = MI.getOpcode();
  if (Opcode == W65816::STA_dp || Opcode == W65816::LDA_dp) {
    // DP instructions have the address at operand index 1:
    // STA_dp: operand 0 = $src (reg), operand 1 = addr (imm)
    // LDA_dp: operand 0 = $dst (reg), operand 1 = addr (imm)
    if (MI.getNumOperands() > 1 && MI.getOperand(1).isImm())
      return MI.getOperand(1).getImm();
  }
  return -1;
}

/// Get the DP memory version of an accumulator operation, or 0 if not foldable.
/// Maps INC_A -> INC_dp, DEC_A -> DEC_dp, ASL_A -> ASL_dp, LSR_A -> LSR_dp.
static unsigned getMemoryOpForAccOp(unsigned AccOp) {
  switch (AccOp) {
  case W65816::INC_A:
    return W65816::INC_dp;
  case W65816::DEC_A:
    return W65816::DEC_dp;
  case W65816::ASL_A:
    return W65816::ASL_dp;
  case W65816::LSR_A:
    return W65816::LSR_dp;
  default:
    return 0;
  }
}

/// Check if an instruction reads the A register.
static bool readsA(unsigned Opcode) {
  switch (Opcode) {
  case W65816::TAX:
  case W65816::TAY:
  case W65816::PHA:
  case W65816::STA_dp:
  case W65816::STA_abs:
  case W65816::STA_sr:
  case W65816::STA_absX:
  case W65816::STA_absY:
  case W65816::STA_dpX:
  case W65816::STA_dpIndY:
  case W65816::STA_srIndY:
  case W65816::ADC_imm16:
  case W65816::ADC_dp:
  case W65816::ADC_abs:
  case W65816::SBC_imm16:
  case W65816::SBC_dp:
  case W65816::SBC_abs:
  case W65816::AND_imm16:
  case W65816::AND_dp:
  case W65816::AND_abs:
  case W65816::ORA_imm16:
  case W65816::ORA_dp:
  case W65816::ORA_abs:
  case W65816::EOR_imm16:
  case W65816::EOR_dp:
  case W65816::EOR_abs:
  case W65816::CMP_imm16:
  case W65816::CMP_dp:
  case W65816::CMP_abs:
  case W65816::INC_A:
  case W65816::DEC_A:
  case W65816::ASL_A:
  case W65816::LSR_A:
  case W65816::ROL_A:
  case W65816::ROR_A:
  case W65816::XBA:
    return true;
  default:
    return false;
  }
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

    // Coalesce consecutive SEP instructions: SEP #M; SEP #N -> SEP #(M|N)
    if (Opcode == W65816::SEP && NextOpcode == W65816::SEP) {
      if (MI.getOperand(0).isImm() && NextMI.getOperand(0).isImm()) {
        int64_t Mask1 = MI.getOperand(0).getImm();
        int64_t Mask2 = NextMI.getOperand(0).getImm();
        int64_t Combined = Mask1 | Mask2;
        LLVM_DEBUG(dbgs() << "Coalescing SEP #" << Mask1 << "; SEP #" << Mask2
                          << " -> SEP #" << Combined << "\n");
        MI.getOperand(0).setImm(Combined);
        MBB.erase(NextMBBI);
        Modified = true;
        // Don't advance - check for more coalescing
        continue;
      }
    }

    // Coalesce consecutive REP instructions: REP #M; REP #N -> REP #(M|N)
    if (Opcode == W65816::REP && NextOpcode == W65816::REP) {
      if (MI.getOperand(0).isImm() && NextMI.getOperand(0).isImm()) {
        int64_t Mask1 = MI.getOperand(0).getImm();
        int64_t Mask2 = NextMI.getOperand(0).getImm();
        int64_t Combined = Mask1 | Mask2;
        LLVM_DEBUG(dbgs() << "Coalescing REP #" << Mask1 << "; REP #" << Mask2
                          << " -> REP #" << Combined << "\n");
        MI.getOperand(0).setImm(Combined);
        MBB.erase(NextMBBI);
        Modified = true;
        // Don't advance - check for more coalescing
        continue;
      }
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
        // INC_A: (outs ACC16:$dst), (ins ACC16:$src), $src = $dst
        BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(W65816::INC_A), W65816::A)
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
        // DEC_A: (outs ACC16:$dst), (ins ACC16:$src), $src = $dst
        BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(W65816::DEC_A), W65816::A)
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

    // Check for Load-Use Folding: LDA dp; OP A; STA dp -> OP dp
    // This is more efficient when A's value is not needed after the sequence.
    // Works for INC, DEC, ASL, LSR.
    if (Opcode == W65816::LDA_dp) {
      unsigned MemOp = getMemoryOpForAccOp(NextOpcode);
      if (MemOp) {
        auto ThirdIt = std::next(NextMBBI);
        if (ThirdIt != MBB.end() && ThirdIt->getOpcode() == W65816::STA_dp) {
          int LoadAddr = getDPAddress(MI);
          int StoreAddr = getDPAddress(*ThirdIt);
          if (LoadAddr >= 0 && LoadAddr == StoreAddr) {
            // Check if A is dead after the STA (i.e., not read before written)
            // Scan forward until we find an instruction that reads or writes A.
            bool AIsDeadAfter = true;
            auto AfterThird = std::next(ThirdIt);
            for (auto ScanIt = AfterThird; ScanIt != MBB.end() && AIsDeadAfter;
                 ++ScanIt) {
              unsigned ScanOpc = ScanIt->getOpcode();
              // If instruction reads A before we find a write to A, A is live
              if (readsA(ScanOpc)) {
                AIsDeadAfter = false;
                break;
              }
              // If instruction writes A (but doesn't read it), A is dead
              if (ScanOpc == W65816::LDA_dp || ScanOpc == W65816::LDA_abs ||
                  ScanOpc == W65816::LDA_imm16 || ScanOpc == W65816::LDA_sr ||
                  ScanOpc == W65816::TXA || ScanOpc == W65816::TYA ||
                  ScanOpc == W65816::PLA) {
                break; // A is written, so it was dead after STA
              }
              // Branch/call terminates analysis - be conservative
              if (ScanIt->isTerminator() || ScanIt->isCall()) {
                // For terminators, check if A is live-out of the block
                // For now, be conservative and assume A might be live
                AIsDeadAfter = false;
                break;
              }
            }
            // Also be conservative if we hit end of block (A might be live-out)
            if (AfterThird == MBB.end()) {
              AIsDeadAfter = false;
            }

            if (AIsDeadAfter) {
              LLVM_DEBUG(dbgs() << "Load-Use Folding: LDA dp; "
                                << (NextOpcode == W65816::INC_A   ? "INC"
                                    : NextOpcode == W65816::DEC_A ? "DEC"
                                    : NextOpcode == W65816::ASL_A ? "ASL"
                                                                  : "LSR")
                                << " A; STA dp -> memory op:\n  " << MI << "  "
                                << NextMI << "  " << *ThirdIt);
              const TargetInstrInfo *TII =
                  MBB.getParent()->getSubtarget().getInstrInfo();
              // Build memory operation with same DP address
              BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(MemOp))
                  .addImm(LoadAddr);
              // Remove all three instructions
              auto AfterIt = std::next(ThirdIt);
              MBB.erase(MBBI);
              MBB.erase(NextMBBI);
              MBB.erase(ThirdIt);
              MBBI = AfterIt;
              Modified = true;
              continue;
            }
          }
        }
      }
    }

    // Check for TYA; INC_A; TAY -> INY / TXA; INC_A; TAX -> INX
    // These patterns arise from ADD16ri expansion for index register increment.
    // Also handles DEC_A variants -> DEY/DEX.
    // Only safe when A's intermediate value is not used after the sequence.
    if ((Opcode == W65816::TYA || Opcode == W65816::TXA) &&
        (NextOpcode == W65816::INC_A || NextOpcode == W65816::DEC_A)) {
      auto ThirdIt = std::next(NextMBBI);
      if (ThirdIt != MBB.end()) {
        unsigned ThirdOpc = ThirdIt->getOpcode();
        bool IsY = (Opcode == W65816::TYA);
        unsigned ExpectedTransfer = IsY ? W65816::TAY : W65816::TAX;
        if (ThirdOpc == ExpectedTransfer) {
          // Check if A is dead after the third instruction (TAX/TAY).
          // INY/INX don't modify A, so if A was expected to hold the
          // incremented value after the sequence, we can't replace.
          bool AIsDeadAfter = true;
          auto AfterThird = std::next(ThirdIt);
          if (AfterThird != MBB.end()) {
            // Quick check: if the next instruction reads A, A is live.
            unsigned AfterOpc = AfterThird->getOpcode();
            // Instructions that explicitly read A
            if (AfterOpc == W65816::TAX || AfterOpc == W65816::TAY ||
                AfterOpc == W65816::PHA || AfterOpc == W65816::STA_dp ||
                AfterOpc == W65816::STA_abs || AfterOpc == W65816::STA_sr ||
                AfterOpc == W65816::ADC_imm16 ||
                AfterOpc == W65816::SBC_imm16 ||
                AfterOpc == W65816::AND_imm16 ||
                AfterOpc == W65816::ORA_imm16 ||
                AfterOpc == W65816::EOR_imm16 ||
                AfterOpc == W65816::CMP_imm16 || AfterOpc == W65816::CMP_abs ||
                AfterOpc == W65816::CMP_dp) {
              AIsDeadAfter = false;
            }
          }
          if (AIsDeadAfter) {
            unsigned NewOpc;
            if (NextOpcode == W65816::INC_A) {
              NewOpc = IsY ? W65816::INY : W65816::INX;
            } else {
              NewOpc = IsY ? W65816::DEY : W65816::DEX;
            }
            LLVM_DEBUG(dbgs() << "Replacing transfer-inc/dec-transfer with "
                              << (IsY ? "INY/DEY" : "INX/DEX") << "\n");
            const TargetInstrInfo *TII =
                MBB.getParent()->getSubtarget().getInstrInfo();
            BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(NewOpc));
            auto AfterIt = std::next(ThirdIt);
            MBB.erase(MBBI);
            MBB.erase(NextMBBI);
            MBB.erase(ThirdIt);
            MBBI = AfterIt;
            Modified = true;
            continue;
          }
        }
      }
    }

    // Check for redundant CMP #0 after flag-setting instructions.
    // LDA/AND/ORA/EOR already set Z and N flags, so CMP #0 is redundant
    // when ALL following branch instructions only test Z or N flags.
    // We must NOT eliminate CMP #0 if any following branch uses V (BVS/BVC)
    // or C (BCS/BCC) flags, since LDA doesn't set those correctly for
    // comparison purposes (signed comparison patterns use BVS).
    if (Opcode == W65816::CMP_imm16 && MI.getNumOperands() > 1 &&
        MI.getOperand(1).isImm() && MI.getOperand(1).getImm() == 0) {
      // Scan all following branch instructions in this block to ensure
      // none use V or C flags.
      bool AllBranchesUseOnlyZN = true;
      bool HasBranch = false;
      for (auto ScanIt = NextMBBI; ScanIt != MBB.end(); ++ScanIt) {
        unsigned ScanOpc = ScanIt->getOpcode();
        if (ScanOpc == W65816::BEQ || ScanOpc == W65816::BNE ||
            ScanOpc == W65816::BMI || ScanOpc == W65816::BPL) {
          HasBranch = true;
          continue;
        }
        if (ScanOpc == W65816::BRA || ScanOpc == W65816::JMP_abs) {
          // Unconditional branch doesn't use flags, but terminates the
          // branch sequence.
          break;
        }
        if (ScanOpc == W65816::BVS || ScanOpc == W65816::BVC ||
            ScanOpc == W65816::BCS || ScanOpc == W65816::BCC) {
          // These branches use V or C flags which LDA doesn't set correctly.
          AllBranchesUseOnlyZN = false;
          break;
        }
        // Non-branch instruction terminates the scan.
        break;
      }

      if (HasBranch && AllBranchesUseOnlyZN && MBBI != MBB.begin()) {
        auto PrevMBBI = std::prev(MBBI);
        unsigned PrevOpc = PrevMBBI->getOpcode();
        // These instructions set Z and N flags based on the result
        if (PrevOpc == W65816::LDA_dp || PrevOpc == W65816::LDA_abs ||
            PrevOpc == W65816::LDA_imm16 || PrevOpc == W65816::LDA_sr ||
            PrevOpc == W65816::AND_dp || PrevOpc == W65816::AND_imm16 ||
            PrevOpc == W65816::ORA_dp || PrevOpc == W65816::ORA_imm16 ||
            PrevOpc == W65816::EOR_dp || PrevOpc == W65816::EOR_imm16) {
          LLVM_DEBUG(dbgs() << "Removing redundant CMP #0 after flag-setting "
                               "instruction:\n  "
                            << MI);
          MBB.erase(MBBI);
          MBBI = NextMBBI;
          Modified = true;
          continue;
        }
      }
    }

    // Check for compound branch simplifications:
    // BEQ target; BCC target (same target, ULE) ->
    //   CMP #(N+1); BCC target (when preceded by CMP #N, N < 65535)
    // BEQ target; BCS target (same target, UGE) ->
    //   BCS target (BEQ is redundant since BCS covers equal case)
    if (Opcode == W65816::BEQ &&
        (NextOpcode == W65816::BCC || NextOpcode == W65816::BCS) &&
        MI.getNumOperands() > 0 && MI.getOperand(0).isMBB() &&
        NextMI.getNumOperands() > 0 && NextMI.getOperand(0).isMBB() &&
        MI.getOperand(0).getMBB() == NextMI.getOperand(0).getMBB()) {

      if (NextOpcode == W65816::BCS) {
        // BEQ target; BCS target -> BCS target (BEQ redundant after CMP)
        LLVM_DEBUG(dbgs() << "Removing redundant BEQ before BCS:\n  " << MI);
        MBB.erase(MBBI);
        MBBI = NextMBBI;
        Modified = true;
        continue;
      }

      // BEQ target; BCC target -> CMP/CPX/CPY #(N+1); BCC target
      if (MBBI != MBB.begin()) {
        auto PrevMBBI = std::prev(MBBI);
        MachineInstr &PrevMI = *PrevMBBI;
        unsigned PrevOpc = PrevMI.getOpcode();
        if (PrevOpc == W65816::CMP_imm16 || PrevOpc == W65816::CPX_imm16 ||
            PrevOpc == W65816::CPY_imm16) {
          // For CPX/CPY, the immediate is operand 0 (no src register)
          // For CMP_imm16, the immediate is operand 1 (operand 0 is src)
          unsigned ImmIdx = (PrevOpc == W65816::CMP_imm16) ? 1 : 0;
          if (PrevMI.getNumOperands() > ImmIdx &&
              PrevMI.getOperand(ImmIdx).isImm()) {
            int64_t OldImm = PrevMI.getOperand(ImmIdx).getImm();
            if (OldImm < 65535) {
              LLVM_DEBUG(dbgs()
                         << "Normalizing ULE: CMP/CPX/CPY #" << OldImm
                         << "; BEQ; BCC -> #" << (OldImm + 1) << "; BCC\n");
              PrevMI.getOperand(ImmIdx).setImm(OldImm + 1);
              MBB.erase(MBBI);
              MBBI = NextMBBI;
              Modified = true;
              continue;
            }
          }
        }
      }
    }

    ++MBBI;
  }

  return Modified;
}

/// Get the complemented branch opcode, or 0 if not a conditional branch.
static unsigned getComplementBranch(unsigned Opcode) {
  switch (Opcode) {
  case W65816::BEQ:
    return W65816::BNE;
  case W65816::BNE:
    return W65816::BEQ;
  case W65816::BCS:
    return W65816::BCC;
  case W65816::BCC:
    return W65816::BCS;
  case W65816::BMI:
    return W65816::BPL;
  case W65816::BPL:
    return W65816::BMI;
  case W65816::BVS:
    return W65816::BVC;
  case W65816::BVC:
    return W65816::BVS;
  default:
    return 0;
  }
}

/// Remove BRA instructions that branch to the fall-through block, and
/// complement conditional branch + BRA pairs.
///
/// Pattern: Bcond .Ltarget; BRA .Lother where .Ltarget is the next block
/// becomes: B!cond .Lother (fall through to .Ltarget)
///
/// Pattern: BRA .Ltarget where .Ltarget is the next block
/// becomes: (deleted, fall through)
bool W65816PeepholeOpt::removeRedundantBranches(MachineFunction &MF) {
  bool Modified = false;

  for (auto MBBI = MF.begin(), MBBE = MF.end(); MBBI != MBBE; ++MBBI) {
    MachineBasicBlock &MBB = *MBBI;

    // Get the next block in layout order
    auto NextMBBI = std::next(MBBI);
    if (NextMBBI == MBBE)
      continue;

    MachineBasicBlock *NextMBB = &*NextMBBI;

    if (MBB.empty())
      continue;

    // Check for Bcond .Ltarget; BRA .Lother where .Ltarget is next block.
    // Replace with B!cond .Lother (complemented condition, fall through).
    if (MBB.size() >= 2) {
      MachineInstr &LastMI = MBB.back();
      MachineInstr &PrevMI = *std::prev(MBB.end(), 2);

      unsigned CompOpc = getComplementBranch(PrevMI.getOpcode());
      if (CompOpc &&
          (LastMI.getOpcode() == W65816::BRA ||
           LastMI.getOpcode() == W65816::JMP_abs) &&
          PrevMI.getNumOperands() > 0 && PrevMI.getOperand(0).isMBB() &&
          LastMI.getNumOperands() > 0 && LastMI.getOperand(0).isMBB()) {
        MachineBasicBlock *CondTarget = PrevMI.getOperand(0).getMBB();
        if (CondTarget == NextMBB) {
          // Bcond .next; BRA .other -> B!cond .other
          MachineBasicBlock *BraTarget = LastMI.getOperand(0).getMBB();
          LLVM_DEBUG(dbgs()
                     << "Complementing branch: " << PrevMI << "  " << LastMI);
          const TargetInstrInfo *TII =
              MBB.getParent()->getSubtarget().getInstrInfo();
          BuildMI(MBB, PrevMI, PrevMI.getDebugLoc(), TII->get(CompOpc))
              .addMBB(BraTarget);
          PrevMI.eraseFromParent();
          LastMI.eraseFromParent();
          Modified = true;
          continue;
        }
      }
    }

    // Check if the last instruction is an unconditional branch to the next
    // block
    MachineInstr &LastMI = MBB.back();
    if (LastMI.getOpcode() != W65816::BRA &&
        LastMI.getOpcode() != W65816::JMP_abs)
      continue;

    // Check if the branch target is the next block
    if (LastMI.getNumOperands() > 0 && LastMI.getOperand(0).isMBB()) {
      MachineBasicBlock *TargetMBB = LastMI.getOperand(0).getMBB();
      if (TargetMBB == NextMBB) {
        LLVM_DEBUG(dbgs() << "Removing redundant unconditional branch "
                             "to fall-through block\n");
        LastMI.eraseFromParent();
        Modified = true;
      }
    }
  }

  return Modified;
}

bool W65816PeepholeOpt::runOnMachineFunction(MachineFunction &MF) {
  bool Modified = false;

  // Run passes in a loop until convergence. Branch complement
  // (removeRedundantBranches) can create new patterns that optimizeMBB
  // can simplify (e.g., BEQ+BCS→BCC complement creates BEQ+BCC same-target
  // patterns that ULE normalization can fold).
  bool Changed;
  do {
    Changed = false;

    for (MachineBasicBlock &MBB : MF) {
      Changed |= optimizeMBB(MBB);
    }

    Changed |= removeRedundantBranches(MF);

    Modified |= Changed;
  } while (Changed);

  return Modified;
}

} // end anonymous namespace

INITIALIZE_PASS(W65816PeepholeOpt, DEBUG_TYPE, W65816_PEEPHOLE_OPT_NAME, false,
                false)

FunctionPass *llvm::createW65816PeepholeOptPass() {
  return new W65816PeepholeOpt();
}
