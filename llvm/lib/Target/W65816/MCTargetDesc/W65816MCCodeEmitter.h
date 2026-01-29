//===-- W65816MCCodeEmitter.h - Convert W65816 Code to Machine Code -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the W65816MCCodeEmitter class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816MCCODEEMITTER_H
#define LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816MCCODEEMITTER_H

#include "W65816FixupKinds.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/Support/DataTypes.h"

namespace llvm {

class MCContext;
class MCExpr;
class MCFixup;
class MCInst;
class MCInstrInfo;
class MCOperand;
class MCSubtargetInfo;
class raw_ostream;

/// Writes W65816 machine code to a stream.
class W65816MCCodeEmitter : public MCCodeEmitter {
public:
  W65816MCCodeEmitter(const MCInstrInfo &MCII, MCContext &Ctx)
      : MCII(MCII), Ctx(Ctx) {}

private:
  /// TableGen'ed function to get the binary encoding for an instruction.
  uint64_t getBinaryCodeForInstr(const MCInst &MI,
                                 SmallVectorImpl<MCFixup> &Fixups,
                                 const MCSubtargetInfo &STI) const;

  /// Returns the binary encoding of an operand.
  unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;

  /// Encodes an immediate operand.
  unsigned encodeImmediate(const MCInst &MI, unsigned OpNo,
                           SmallVectorImpl<MCFixup> &Fixups,
                           const MCSubtargetInfo &STI) const;

  /// Encodes a 16-bit address operand.
  unsigned encodeAddr16(const MCInst &MI, unsigned OpNo,
                        SmallVectorImpl<MCFixup> &Fixups,
                        const MCSubtargetInfo &STI) const;

  /// Encodes a PC-relative branch target.
  unsigned encodePCRelTarget(const MCInst &MI, unsigned OpNo,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;

  /// Encodes a stack-relative offset.
  unsigned encodeStackRelOffset(const MCInst &MI, unsigned OpNo,
                                SmallVectorImpl<MCFixup> &Fixups,
                                const MCSubtargetInfo &STI) const;

  void encodeInstruction(const MCInst &MI, SmallVectorImpl<char> &CB,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const override;

  W65816MCCodeEmitter(const W65816MCCodeEmitter &) = delete;
  void operator=(const W65816MCCodeEmitter &) = delete;

  const MCInstrInfo &MCII;
  MCContext &Ctx;
};

} // namespace llvm

#endif // LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816MCCODEEMITTER_H
