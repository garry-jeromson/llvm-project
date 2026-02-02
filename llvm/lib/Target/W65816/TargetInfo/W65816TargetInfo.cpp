//===-- W65816TargetInfo.cpp - W65816 Target Implementation ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "TargetInfo/W65816TargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Compiler.h"

namespace llvm {

Target &getTheW65816Target() {
  static Target TheW65816Target;
  return TheW65816Target;
}

} // namespace llvm

extern "C" LLVM_ABI LLVM_EXTERNAL_VISIBILITY void
LLVMInitializeW65816TargetInfo() {
  llvm::RegisterTarget<llvm::Triple::w65816> X(
      llvm::getTheW65816Target(), "w65816", "WDC 65816 16-bit Processor",
      "W65816");
}
