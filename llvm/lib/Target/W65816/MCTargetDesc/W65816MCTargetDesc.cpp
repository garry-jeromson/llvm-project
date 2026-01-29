//===-- W65816MCTargetDesc.cpp - W65816 Target Descriptions ---------------===//
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

#include "W65816MCTargetDesc.h"
#include "W65816InstPrinter.h"
#include "W65816MCAsmInfo.h"
#include "TargetInfo/W65816TargetInfo.h"

#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Compiler.h"

#define GET_INSTRINFO_MC_DESC
#define ENABLE_INSTR_PREDICATE_VERIFIER
#include "W65816GenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "W65816GenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "W65816GenRegisterInfo.inc"

using namespace llvm;

MCInstrInfo *llvm::createW65816MCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitW65816MCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createW65816MCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitW65816MCRegisterInfo(X, W65816::SP);
  return X;
}

static MCSubtargetInfo *createW65816MCSubtargetInfo(const Triple &TT,
                                                    StringRef CPU,
                                                    StringRef FS) {
  return createW65816MCSubtargetInfoImpl(TT, CPU, /*TuneCPU*/ CPU, FS);
}

static MCInstPrinter *createW65816MCInstPrinter(const Triple &T,
                                                unsigned SyntaxVariant,
                                                const MCAsmInfo &MAI,
                                                const MCInstrInfo &MII,
                                                const MCRegisterInfo &MRI) {
  if (SyntaxVariant == 0) {
    return new W65816InstPrinter(MAI, MII, MRI);
  }
  return nullptr;
}

extern "C" LLVM_ABI LLVM_EXTERNAL_VISIBILITY void
LLVMInitializeW65816TargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfo<W65816MCAsmInfo> X(getTheW65816Target());

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(getTheW65816Target(),
                                      createW65816MCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(getTheW65816Target(),
                                    createW65816MCRegisterInfo);

  // Register the MC subtarget info.
  TargetRegistry::RegisterMCSubtargetInfo(getTheW65816Target(),
                                          createW65816MCSubtargetInfo);

  // Register the MCInstPrinter.
  TargetRegistry::RegisterMCInstPrinter(getTheW65816Target(),
                                        createW65816MCInstPrinter);

  // Register the MC code emitter.
  TargetRegistry::RegisterMCCodeEmitter(getTheW65816Target(),
                                        createW65816MCCodeEmitter);

  // Register the asm backend.
  TargetRegistry::RegisterMCAsmBackend(getTheW65816Target(),
                                       createW65816AsmBackend);
}
