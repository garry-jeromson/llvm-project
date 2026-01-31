//===-- W65816AsmBackend.cpp - W65816 Asm Backend -------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the W65816AsmBackend class.
//
//===----------------------------------------------------------------------===//

#include "W65816FixupKinds.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"

#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"

using namespace llvm;

namespace {

class W65816AsmBackend : public MCAsmBackend {
  Triple::OSType OSType;

public:
  W65816AsmBackend(Triple::OSType OSType)
      : MCAsmBackend(llvm::endianness::little), OSType(OSType) {}

  void adjustFixupValue(const MCFixup &Fixup, const MCValue &Target,
                        uint64_t &Value, MCContext *Ctx) const;

  std::unique_ptr<MCObjectTargetWriter>
  createObjectTargetWriter() const override;

  void applyFixup(const MCFragment &F, const MCFixup &Fixup,
                  const MCValue &Target, uint8_t *Data, uint64_t Value,
                  bool IsResolved) override;

  std::optional<MCFixupKind> getFixupKind(StringRef Name) const override;

  MCFixupKindInfo getFixupKindInfo(MCFixupKind Kind) const override;

  unsigned getNumFixupKinds() const {
    return W65816::NumTargetFixupKinds;
  }

  bool writeNopData(raw_ostream &OS, uint64_t Count,
                    const MCSubtargetInfo *STI) const override;
};

void W65816AsmBackend::adjustFixupValue(const MCFixup &Fixup,
                                         const MCValue &Target, uint64_t &Value,
                                         MCContext *Ctx) const {
  unsigned Kind = Fixup.getKind();

  switch (Kind) {
  default:
    llvm_unreachable("Unknown fixup kind!");

  case W65816::fixup_w65816_16:
    // 16-bit absolute address - no adjustment needed
    Value &= 0xFFFF;
    break;

  case W65816::fixup_w65816_24:
    // 24-bit long address - no adjustment needed
    Value &= 0xFFFFFF;
    break;

  case W65816::fixup_w65816_pcrel_8: {
    // 8-bit PC-relative branch
    // The 65816 calculates: target = PC + signed_offset
    // where PC = address after the 2-byte instruction = opcode_addr + 2
    // Fixup location is at opcode_addr + 1 (the offset byte)
    // Value from LLVM = target - fixup_location
    // We need: offset = target - (opcode_addr + 2) = target - (fixup_location + 1)
    //        = (target - fixup_location) - 1 = Value - 1
    int64_t Offset = static_cast<int64_t>(Value) - 1;
    if (!isIntN(8, Offset)) {
      Ctx->reportError(Fixup.getLoc(),
                       "branch target out of range (must be -128 to +127)");
    }
    Value = static_cast<uint64_t>(Offset) & 0xFF;
    break;
  }

  case W65816::fixup_w65816_pcrel_16: {
    // 16-bit PC-relative branch (BRL)
    // BRL is 3 bytes: opcode + 16-bit offset
    // Fixup is at offset byte (opcode_addr + 1)
    // PC after instruction = opcode_addr + 3
    // offset = target - (opcode_addr + 3) = target - (fixup_location + 2) = Value - 2
    int64_t Offset = static_cast<int64_t>(Value) - 2;
    if (!isIntN(16, Offset)) {
      Ctx->reportError(Fixup.getLoc(),
                       "branch target out of range for BRL");
    }
    Value = static_cast<uint64_t>(Offset) & 0xFFFF;
    break;
  }

  case W65816::fixup_w65816_dp:
    // 8-bit direct page address
    if (!isUIntN(8, Value)) {
      Ctx->reportError(Fixup.getLoc(),
                       "direct page address out of range (must be 0-255)");
    }
    Value &= 0xFF;
    break;

  case W65816::fixup_w65816_imm8:
    // 8-bit immediate
    Value &= 0xFF;
    break;

  case W65816::fixup_w65816_imm16:
    // 16-bit immediate
    Value &= 0xFFFF;
    break;

  // Standard fixups
  case FK_Data_1:
  case FK_Data_2:
  case FK_Data_4:
    break;
  }
}

std::unique_ptr<MCObjectTargetWriter>
W65816AsmBackend::createObjectTargetWriter() const {
  return createW65816ELFObjectWriter(MCELFObjectTargetWriter::getOSABI(OSType));
}

void W65816AsmBackend::applyFixup(const MCFragment &F, const MCFixup &Fixup,
                                   const MCValue &Target, uint8_t *Data,
                                   uint64_t Value, bool IsResolved) {
  // Record relocation if fixup is not resolved
  maybeAddReloc(F, Fixup, Target, Value, IsResolved);

  // If not resolved, we've recorded a relocation; nothing more to do
  if (!IsResolved)
    return;

  adjustFixupValue(Fixup, Target, Value, &getContext());

  // Note: We removed the early return for Value==0 because symbols at
  // offset 0 in a section are valid targets and need their fixups applied.

  MCFixupKindInfo Info = getFixupKindInfo(Fixup.getKind());
  unsigned NumBytes = (Info.TargetSize + 7) / 8;

  // Note: Data already points to the fixup location within the fragment,
  // so we write starting at Data[0], not Data[Fixup.getOffset()]
  // We use assignment, not OR, because the instruction bytes were already
  // initialized to 0 and we need to write the actual fixup value.
  for (unsigned i = 0; i < NumBytes; ++i) {
    Data[i] = static_cast<uint8_t>((Value >> (i * 8)) & 0xFF);
  }
}

std::optional<MCFixupKind> W65816AsmBackend::getFixupKind(StringRef Name) const {
  // Map relocation names to fixup kinds if needed
  return std::nullopt;
}

MCFixupKindInfo W65816AsmBackend::getFixupKindInfo(MCFixupKind Kind) const {
  // Table of fixup information
  // Must be in the same order as W65816::Fixups enum
  const static MCFixupKindInfo Infos[W65816::NumTargetFixupKinds] = {
      // name                    offset  bits  flags
      {"fixup_w65816_16", 0, 16, 0},
      {"fixup_w65816_24", 0, 24, 0},
      {"fixup_w65816_pcrel_8", 0, 8, 0},  // PC-rel handled in adjustFixupValue
      {"fixup_w65816_pcrel_16", 0, 16, 0},
      {"fixup_w65816_dp", 0, 8, 0},
      {"fixup_w65816_imm8", 0, 8, 0},
      {"fixup_w65816_imm16", 0, 16, 0},
  };

  if (mc::isRelocation(Kind))
    return {};

  if (Kind < FirstTargetFixupKind)
    return MCAsmBackend::getFixupKindInfo(Kind);

  assert(unsigned(Kind - FirstTargetFixupKind) < W65816::NumTargetFixupKinds &&
         "Invalid kind!");

  return Infos[Kind - FirstTargetFixupKind];
}

bool W65816AsmBackend::writeNopData(raw_ostream &OS, uint64_t Count,
                                     const MCSubtargetInfo *STI) const {
  // The 65816 NOP instruction is a single byte (0xEA)
  for (uint64_t i = 0; i < Count; ++i) {
    OS << static_cast<char>(0xEA);
  }
  return true;
}

} // end anonymous namespace

namespace llvm {

MCAsmBackend *createW65816AsmBackend(const Target &T,
                                      const MCSubtargetInfo &STI,
                                      const MCRegisterInfo &MRI,
                                      const MCTargetOptions &Options) {
  return new W65816AsmBackend(STI.getTargetTriple().getOS());
}

} // end namespace llvm
