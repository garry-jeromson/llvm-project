//===-- W65816Subtarget.h - Define Subtarget for W65816 ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares the W65816 specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_W65816SUBTARGET_H
#define LLVM_LIB_TARGET_W65816_W65816SUBTARGET_H

#include "W65816FrameLowering.h"
#include "W65816ISelLowering.h"
#include "W65816InstrInfo.h"
#include "W65816RegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGTargetInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetMachine.h"

#define GET_SUBTARGETINFO_HEADER
#include "W65816GenSubtargetInfo.inc"

namespace llvm {

class W65816TargetMachine;

class W65816Subtarget : public W65816GenSubtargetInfo {
public:
  W65816Subtarget(const Triple &TT, const std::string &CPU,
                  const std::string &FS, const W65816TargetMachine &TM);

  const W65816InstrInfo *getInstrInfo() const override { return &InstrInfo; }
  const W65816FrameLowering *getFrameLowering() const override {
    return &FrameLowering;
  }
  const W65816TargetLowering *getTargetLowering() const override {
    return &TLInfo;
  }
  const W65816RegisterInfo *getRegisterInfo() const override {
    return &InstrInfo.getRegisterInfo();
  }
  const SelectionDAGTargetInfo *getSelectionDAGInfo() const override {
    return &TSInfo;
  }

  /// Parses a subtarget feature string, setting appropriate options.
  void ParseSubtargetFeatures(StringRef CPU, StringRef TuneCPU, StringRef FS);

  W65816Subtarget &initializeSubtargetDependencies(StringRef CPU, StringRef FS,
                                                   const TargetMachine &TM);

  // Feature predicates
  bool hasLongAddressing() const { return HasLongAddressing; }
  bool isSNES() const { return IsSNES; }

  // 8-bit mode predicates
  bool uses8BitAccumulator() const { return UseAcc8Bit; }
  bool uses8BitIndex() const { return UseIdx8Bit; }

  // Direct Page optimization predicate
  bool assumeD0() const { return AssumeD0; }

  bool enableSubRegLiveness() const override { return true; }

private:
  // Subtarget features
  bool HasLongAddressing = false;
  bool IsSNES = false;

  // 8-bit mode flags (M and X processor status bits)
  bool UseAcc8Bit = false;  // M flag: true = 8-bit accumulator
  bool UseIdx8Bit = false;  // X flag: true = 8-bit index registers

  // Direct Page optimization flag
  bool AssumeD0 = false;    // Assume D register is always 0

  W65816InstrInfo InstrInfo;
  W65816FrameLowering FrameLowering;
  W65816TargetLowering TLInfo;
  SelectionDAGTargetInfo TSInfo;
};

} // namespace llvm

#endif // LLVM_LIB_TARGET_W65816_W65816SUBTARGET_H
