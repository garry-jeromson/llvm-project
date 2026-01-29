//===-- W65816Subtarget.cpp - W65816 Subtarget Information ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the W65816 specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "W65816Subtarget.h"

#include "W65816.h"
#include "W65816TargetMachine.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"

#include "llvm/MC/TargetRegistry.h"

#define DEBUG_TYPE "w65816-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "W65816GenSubtargetInfo.inc"

using namespace llvm;

W65816Subtarget::W65816Subtarget(const Triple &TT, const std::string &CPU,
                                 const std::string &FS,
                                 const W65816TargetMachine &TM)
    : W65816GenSubtargetInfo(TT, CPU, /*TuneCPU*/ CPU, FS),
      InstrInfo(*this), FrameLowering(), TLInfo(TM, *this) {

  // Parse features string
  ParseSubtargetFeatures(CPU, /*TuneCPU*/ CPU, FS);
}

W65816Subtarget &
W65816Subtarget::initializeSubtargetDependencies(StringRef CPU, StringRef FS,
                                                 const TargetMachine &TM) {
  // Set the default CPU if none was specified
  if (CPU.empty())
    CPU = "w65816";

  ParseSubtargetFeatures(CPU, /*TuneCPU*/ CPU, FS);
  return *this;
}
