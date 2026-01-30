//===- W65816.cpp ---------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ABIInfoImpl.h"
#include "TargetInfo.h"

using namespace clang;
using namespace clang::CodeGen;

//===----------------------------------------------------------------------===//
// W65816 ABI Implementation
//===----------------------------------------------------------------------===//

namespace {

class W65816ABIInfo : public DefaultABIInfo {
public:
  W65816ABIInfo(CodeGenTypes &CGT) : DefaultABIInfo(CGT) {}

  ABIArgInfo classifyReturnType(QualType RetTy) const {
    if (RetTy->isAnyComplexType()) {
      ABIArgInfo Info = ABIArgInfo::getDirect();
      Info.setCanBeFlattened(false);
      return Info;
    }
    return DefaultABIInfo::classifyReturnType(RetTy);
  }

  ABIArgInfo classifyArgumentType(QualType ArgTy) const {
    if (ArgTy->isAnyComplexType()) {
      ABIArgInfo Info = ABIArgInfo::getDirect();
      Info.setCanBeFlattened(false);
      return Info;
    }
    return DefaultABIInfo::classifyArgumentType(ArgTy);
  }

  void computeInfo(CGFunctionInfo &FI) const override {
    if (!getCXXABI().classifyReturnType(FI))
      FI.getReturnInfo() = classifyReturnType(FI.getReturnType());
    for (auto &I : FI.arguments())
      I.info = classifyArgumentType(I.type);
  }

  RValue EmitVAArg(CodeGenFunction &CGF, Address VAListAddr, QualType Ty,
                   AggValueSlot Slot) const override {
    return CGF.EmitLoadOfAnyValue(
        CGF.MakeAddrLValue(
            EmitVAArgInstr(CGF, VAListAddr, Ty, classifyArgumentType(Ty)), Ty),
        Slot);
  }
};

class W65816TargetCodeGenInfo : public TargetCodeGenInfo {
public:
  W65816TargetCodeGenInfo(CodeGenTypes &CGT)
      : TargetCodeGenInfo(std::make_unique<W65816ABIInfo>(CGT)) {}

  void setTargetAttributes(const Decl *D, llvm::GlobalValue *GV,
                           CodeGen::CodeGenModule &M) const override;
};

} // namespace

void W65816TargetCodeGenInfo::setTargetAttributes(
    const Decl *D, llvm::GlobalValue *GV, CodeGen::CodeGenModule &M) const {
  if (GV->isDeclaration())
    return;
  if (const FunctionDecl *FD = dyn_cast_or_null<FunctionDecl>(D)) {
    llvm::Function *F = cast<llvm::Function>(GV);

    // Handle w65816_farfunc attribute via annotation
    for (const auto *Ann : FD->specific_attrs<AnnotateAttr>()) {
      if (Ann->getAnnotation() == "w65816_farfunc") {
        F->addFnAttr("w65816-far");
      }
    }
  }
}

std::unique_ptr<TargetCodeGenInfo>
CodeGen::createW65816TargetCodeGenInfo(CodeGenModule &CGM) {
  return std::make_unique<W65816TargetCodeGenInfo>(CGM.getTypes());
}
