//===--- W65816.cpp - Implement W65816 target feature support -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements W65816 TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#include "W65816.h"
#include "clang/Basic/MacroBuilder.h"

using namespace clang;
using namespace clang::targets;

const char *const W65816TargetInfo::GCCRegNames[] = {"a", "x",   "y",  "sp",
                                                     "d", "dbr", "pbr"};

ArrayRef<const char *> W65816TargetInfo::getGCCRegNames() const {
  return llvm::ArrayRef(GCCRegNames);
}

void W65816TargetInfo::getTargetDefines(const LangOptions &Opts,
                                        MacroBuilder &Builder) const {
  Builder.defineMacro("W65816");
  Builder.defineMacro("__W65816__");
}
