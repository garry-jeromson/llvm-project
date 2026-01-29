//===-- W65816MCCodeEmitter.cpp - Convert W65816 Code to Machine Code -----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the W65816MCCodeEmitter class.
//
//===----------------------------------------------------------------------===//

#include "W65816MCCodeEmitter.h"
#include "W65816FixupKinds.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "mccodeemitter"

namespace llvm {

/// Helper to add a fixup.
static void addFixup(SmallVectorImpl<MCFixup> &Fixups, uint32_t Offset,
                     const MCExpr *Value, uint16_t Kind, bool PCRel = false) {
  Fixups.push_back(MCFixup::create(Offset, Value, Kind, PCRel));
}

unsigned W65816MCCodeEmitter::getMachineOpValue(const MCInst &MI,
                                                 const MCOperand &MO,
                                                 SmallVectorImpl<MCFixup> &Fixups,
                                                 const MCSubtargetInfo &STI) const {
  if (MO.isReg()) {
    // Return the register encoding
    return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());
  }

  if (MO.isImm()) {
    return static_cast<unsigned>(MO.getImm());
  }

  if (MO.isExpr()) {
    // For expressions, we'll need a fixup - handled by specific encode functions
    return 0;
  }

  llvm_unreachable("Unhandled operand type in getMachineOpValue");
}

unsigned W65816MCCodeEmitter::encodeImmediate(const MCInst &MI, unsigned OpNo,
                                               SmallVectorImpl<MCFixup> &Fixups,
                                               const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  if (MO.isImm()) {
    return static_cast<unsigned>(MO.getImm());
  }

  if (MO.isExpr()) {
    // Create a fixup for the expression
    addFixup(Fixups, 1, MO.getExpr(), W65816::fixup_w65816_imm16);
    return 0;
  }

  llvm_unreachable("Unhandled operand in encodeImmediate");
}

unsigned W65816MCCodeEmitter::encodeAddr16(const MCInst &MI, unsigned OpNo,
                                            SmallVectorImpl<MCFixup> &Fixups,
                                            const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  if (MO.isImm()) {
    return static_cast<unsigned>(MO.getImm());
  }

  if (MO.isExpr()) {
    // Create a 16-bit address fixup
    addFixup(Fixups, 1, MO.getExpr(), W65816::fixup_w65816_16);
    return 0;
  }

  llvm_unreachable("Unhandled operand in encodeAddr16");
}

unsigned W65816MCCodeEmitter::encodePCRelTarget(const MCInst &MI, unsigned OpNo,
                                                 SmallVectorImpl<MCFixup> &Fixups,
                                                 const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  if (MO.isImm()) {
    // Already calculated PC-relative offset
    return static_cast<unsigned>(MO.getImm());
  }

  if (MO.isExpr()) {
    // Create a PC-relative fixup
    addFixup(Fixups, 1, MO.getExpr(), W65816::fixup_w65816_pcrel_8, true);
    return 0;
  }

  llvm_unreachable("Unhandled operand in encodePCRelTarget");
}

unsigned W65816MCCodeEmitter::encodeStackRelOffset(const MCInst &MI, unsigned OpNo,
                                                    SmallVectorImpl<MCFixup> &Fixups,
                                                    const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  if (MO.isImm()) {
    return static_cast<unsigned>(MO.getImm());
  }

  llvm_unreachable("Unhandled operand in encodeStackRelOffset");
}

/// Encode a PC-relative 8-bit branch target (for short branches).
unsigned encodePCRelImm(const MCInst &MI, unsigned OpNo,
                        SmallVectorImpl<MCFixup> &Fixups,
                        const MCSubtargetInfo &STI) {
  const MCOperand &MO = MI.getOperand(OpNo);

  if (MO.isImm()) {
    // Immediate value - already computed offset
    return static_cast<unsigned>(MO.getImm()) & 0xFF;
  }

  if (MO.isExpr()) {
    // Expression - need a fixup
    Fixups.push_back(MCFixup::create(1, MO.getExpr(),
                                     static_cast<MCFixupKind>(W65816::fixup_w65816_pcrel_8),
                                     true));
    return 0;
  }

  return 0;
}

/// Encode a PC-relative 16-bit branch target (for BRL).
unsigned encodePCRelImm16(const MCInst &MI, unsigned OpNo,
                          SmallVectorImpl<MCFixup> &Fixups,
                          const MCSubtargetInfo &STI) {
  const MCOperand &MO = MI.getOperand(OpNo);

  if (MO.isImm()) {
    // Immediate value - already computed offset
    return static_cast<unsigned>(MO.getImm()) & 0xFFFF;
  }

  if (MO.isExpr()) {
    // Expression - need a fixup
    Fixups.push_back(MCFixup::create(1, MO.getExpr(),
                                     static_cast<MCFixupKind>(W65816::fixup_w65816_pcrel_16),
                                     true));
    return 0;
  }

  return 0;
}

void W65816MCCodeEmitter::encodeInstruction(const MCInst &MI,
                                             SmallVectorImpl<char> &CB,
                                             SmallVectorImpl<MCFixup> &Fixups,
                                             const MCSubtargetInfo &STI) const {
  const MCInstrDesc &Desc = MCII.get(MI.getOpcode());
  uint64_t Size = Desc.getSize();

  // Get the binary encoding for this instruction from TableGen
  uint64_t Binary = getBinaryCodeForInstr(MI, Fixups, STI);

  // Emit the instruction bytes in little-endian order
  // The 65816 is little-endian
  for (uint64_t i = 0; i < Size; ++i) {
    CB.push_back(static_cast<char>(Binary & 0xFF));
    Binary >>= 8;
  }
}

// Include the TableGen'd encoder functions
#define ENABLE_INSTR_PREDICATE_VERIFIER
#include "W65816GenMCCodeEmitter.inc"

MCCodeEmitter *createW65816MCCodeEmitter(const MCInstrInfo &MCII,
                                          MCContext &Ctx) {
  return new W65816MCCodeEmitter(MCII, Ctx);
}

} // namespace llvm
