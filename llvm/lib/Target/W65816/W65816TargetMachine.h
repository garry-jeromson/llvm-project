//===-- W65816TargetMachine.h - Define TargetMachine for W65816 -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares the W65816 specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_W65816TARGETMACHINE_H
#define LLVM_LIB_TARGET_W65816_W65816TARGETMACHINE_H

#include "W65816Subtarget.h"
#include "llvm/CodeGen/CodeGenTargetMachineImpl.h"
#include "llvm/IR/DataLayout.h"

#include <optional>

namespace llvm {

class W65816TargetMachine : public CodeGenTargetMachineImpl {
public:
  W65816TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                      StringRef FS, const TargetOptions &Options,
                      std::optional<Reloc::Model> RM,
                      std::optional<CodeModel::Model> CM, CodeGenOptLevel OL,
                      bool JIT);

  const W65816Subtarget *getSubtargetImpl() const { return &Subtarget; }
  const W65816Subtarget *getSubtargetImpl(const Function &) const override {
    return &Subtarget;
  }

  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;

  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }

  MachineFunctionInfo *
  createMachineFunctionInfo(BumpPtrAllocator &Allocator, const Function &F,
                            const TargetSubtargetInfo *STI) const override;

private:
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  W65816Subtarget Subtarget;
};

} // namespace llvm

#endif // LLVM_LIB_TARGET_W65816_W65816TARGETMACHINE_H
