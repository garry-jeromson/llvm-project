//===-- W65816ELFObjectWriter.cpp - W65816 ELF Writer ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "W65816FixupKinds.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"

#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {

// Custom ELF machine type for W65816
// Using a value in the experimental range
// A proper implementation would add EM_W65816 to ELF.h
constexpr unsigned EM_W65816 = 0x1002;

// W65816 ELF relocation types
// Since there's no standard, we define our own
enum {
  R_W65816_NONE = 0,
  R_W65816_16 = 1,
  R_W65816_24 = 2,
  R_W65816_8_PCREL = 3,
  R_W65816_16_PCREL = 4,
  R_W65816_8 = 5,
};

/// Writes W65816 machine code into an ELF32 object file.
class W65816ELFObjectWriter : public MCELFObjectTargetWriter {
public:
  W65816ELFObjectWriter(uint8_t OSABI);

  ~W65816ELFObjectWriter() override = default;

  unsigned getRelocType(const MCFixup &Fixup, const MCValue &Target,
                        bool IsPCRel) const override;
};

W65816ELFObjectWriter::W65816ELFObjectWriter(uint8_t OSABI)
    : MCELFObjectTargetWriter(/*Is64Bit=*/false, OSABI, EM_W65816,
                               /*HasRelocationAddend=*/true) {}

unsigned W65816ELFObjectWriter::getRelocType(const MCFixup &Fixup,
                                              const MCValue &Target,
                                              bool IsPCRel) const {
  switch (static_cast<unsigned>(Fixup.getKind())) {
  case FK_Data_1:
    return R_W65816_8;
  case FK_Data_2:
    return R_W65816_16;
  case FK_Data_4:
    // 32-bit data - use 16-bit reloc for lower half
    return R_W65816_16;

  case W65816::fixup_w65816_16:
    return R_W65816_16;

  case W65816::fixup_w65816_24:
    return R_W65816_24;

  case W65816::fixup_w65816_pcrel_8:
    return R_W65816_8_PCREL;

  case W65816::fixup_w65816_pcrel_16:
    return R_W65816_16_PCREL;

  case W65816::fixup_w65816_dp:
  case W65816::fixup_w65816_imm8:
    return R_W65816_8;

  case W65816::fixup_w65816_imm16:
    return R_W65816_16;

  default:
    llvm_unreachable("Invalid fixup kind!");
  }
}

} // end anonymous namespace

namespace llvm {

std::unique_ptr<MCObjectTargetWriter>
createW65816ELFObjectWriter(uint8_t OSABI) {
  return std::make_unique<W65816ELFObjectWriter>(OSABI);
}

} // end namespace llvm
