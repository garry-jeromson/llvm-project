//===-- W65816TargetMachine.cpp - Define TargetMachine for W65816 ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the W65816 specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#include "W65816TargetMachine.h"

#include "MCTargetDesc/W65816MCTargetDesc.h"
#include "TargetInfo/W65816TargetInfo.h"
#include "W65816.h"
#include "W65816MachineFunctionInfo.h"
#include "W65816TargetObjectFile.h"

#include "llvm/CodeGen/GlobalISel/CSEInfo.h"
#include "llvm/CodeGen/GlobalISel/IRTranslator.h"
#include "llvm/CodeGen/GlobalISel/InstructionSelect.h"
#include "llvm/CodeGen/GlobalISel/Legalizer.h"
#include "llvm/CodeGen/GlobalISel/RegBankSelect.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/InitializePasses.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Compiler.h"

#include <optional>

using namespace llvm;

static StringRef getCPU(StringRef CPU) {
  if (CPU.empty() || CPU == "generic")
    return "w65816";
  return CPU;
}

static Reloc::Model getEffectiveRelocModel(std::optional<Reloc::Model> RM) {
  return RM.value_or(Reloc::Static);
}

static std::string computeDataLayoutString() {
  // e = little endian
  // m:e = ELF mangling
  // p:16:16 = 16-bit pointers, 16-bit aligned (prevents decomposition to byte
  // loads) i8:8 = 8-bit integers, 8-bit aligned i16:16 = 16-bit integers,
  // 16-bit aligned i32:16 = 32-bit integers, 16-bit aligned (matches Clang's
  // LongAlign) i64:16 = 64-bit integers, 16-bit aligned f32:16 = 32-bit floats,
  // 16-bit aligned f64:16 = 64-bit floats, 16-bit aligned a:8 = aggregates,
  // 8-bit aligned n8:16 = native integer widths are 8 and 16 bits S16 = stack
  // natural alignment is 16 bits
  return "e-m:e-p:16:16-i8:8-i16:16-i32:16-i64:16-f32:16-f64:16-a:8-n8:16-S16";
}

W65816TargetMachine::W65816TargetMachine(const Target &T, const Triple &TT,
                                         StringRef CPU, StringRef FS,
                                         const TargetOptions &Options,
                                         std::optional<Reloc::Model> RM,
                                         std::optional<CodeModel::Model> CM,
                                         CodeGenOptLevel OL, bool JIT)
    : CodeGenTargetMachineImpl(T, computeDataLayoutString(), TT, getCPU(CPU),
                               FS, Options, getEffectiveRelocModel(RM),
                               getEffectiveCodeModel(CM, CodeModel::Small), OL),
      Subtarget(TT, std::string(getCPU(CPU)), std::string(FS), *this) {
  TLOF = std::make_unique<W65816TargetObjectFile>();
  initAsmInfo();
  setGlobalISel(true);
  setFastISel(false);
  setO0WantsFastISel(false);
}

namespace {
class W65816PassConfig : public TargetPassConfig {
public:
  W65816PassConfig(W65816TargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  W65816TargetMachine &getW65816TargetMachine() const {
    return getTM<W65816TargetMachine>();
  }

  bool addInstSelector() override;
  bool addIRTranslator() override;
  void addPreLegalizeMachineIR() override;
  bool addLegalizeMachineIR() override;
  void addPreRegBankSelect() override;
  bool addRegBankSelect() override;
  bool addGlobalInstructionSelect() override;
  void addPreEmitPass() override;
};
} // namespace

TargetPassConfig *W65816TargetMachine::createPassConfig(PassManagerBase &PM) {
  return new W65816PassConfig(*this, PM);
}

bool W65816PassConfig::addInstSelector() {
  // SDAG removed — GlobalISel is the sole instruction selector.
  return false;
}

bool W65816PassConfig::addIRTranslator() {
  addPass(new IRTranslator());
  return false;
}

void W65816PassConfig::addPreLegalizeMachineIR() {
  addPass(createW65816PreLegalizerCombiner());
}

bool W65816PassConfig::addLegalizeMachineIR() {
  addPass(new Legalizer());
  return false;
}

void W65816PassConfig::addPreRegBankSelect() {
  addPass(createW65816PostLegalizerCombiner());
}

bool W65816PassConfig::addRegBankSelect() {
  addPass(new RegBankSelect());
  return false;
}

bool W65816PassConfig::addGlobalInstructionSelect() {
  addPass(new InstructionSelect());
  return false;
}

void W65816PassConfig::addPreEmitPass() {
  // Expand pseudo instructions before branch relaxation
  addPass(createW65816ExpandPseudoPass());
  // Peephole optimizations (eliminate redundant transfers)
  addPass(createW65816PeepholeOptPass());
  // Add branch relaxation pass before emission
  addPass(&BranchRelaxationPassID);
}

MachineFunctionInfo *W65816TargetMachine::createMachineFunctionInfo(
    BumpPtrAllocator &Allocator, const Function &F,
    const TargetSubtargetInfo *STI) const {
  return W65816MachineFunctionInfo::create<W65816MachineFunctionInfo>(Allocator,
                                                                      F, STI);
}

extern "C" LLVM_ABI LLVM_EXTERNAL_VISIBILITY void LLVMInitializeW65816Target() {
  // Register the target
  RegisterTargetMachine<W65816TargetMachine> X(getTheW65816Target());

  auto &PR = *PassRegistry::getPassRegistry();
  initializeGlobalISel(PR);

  initializeW65816ExpandPseudoPass(PR);
  initializeW65816PeepholeOptPass(PR);
  initializeW65816PreLegalizerCombinerPass(PR);
  initializeW65816PostLegalizerCombinerPass(PR);
}
