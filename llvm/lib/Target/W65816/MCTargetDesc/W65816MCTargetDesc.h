//===-- W65816MCTargetDesc.h - W65816 Target Descriptions -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides W65816 specific target descriptions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816MCTARGETDESC_H
#define LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816MCTARGETDESC_H

#include "llvm/Support/DataTypes.h"

#include <memory>

namespace llvm {

class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectTargetWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class MCTargetOptions;
class Target;

MCInstrInfo *createW65816MCInstrInfo();

/// Creates a machine code emitter for W65816.
MCCodeEmitter *createW65816MCCodeEmitter(const MCInstrInfo &MCII,
                                         MCContext &Ctx);

/// Creates an assembly backend for W65816.
MCAsmBackend *createW65816AsmBackend(const Target &T,
                                     const MCSubtargetInfo &STI,
                                     const MCRegisterInfo &MRI,
                                     const llvm::MCTargetOptions &TO);

/// Creates an ELF object writer for W65816.
std::unique_ptr<MCObjectTargetWriter>
createW65816ELFObjectWriter(uint8_t OSABI);

} // end namespace llvm

#define GET_REGINFO_ENUM
#include "W65816GenRegisterInfo.inc"

#define GET_INSTRINFO_ENUM
#define GET_INSTRINFO_MC_HELPER_DECLS
#include "W65816GenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "W65816GenSubtargetInfo.inc"

#endif // LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816MCTARGETDESC_H
