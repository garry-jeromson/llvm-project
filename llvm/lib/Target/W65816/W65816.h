//===-- W65816.h - Top-level interface for W65816 ---------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the LLVM
// W65816 back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_W65816_H
#define LLVM_LIB_TARGET_W65816_W65816_H

#include "llvm/Target/TargetMachine.h"

namespace llvm {

class W65816TargetMachine;
class FunctionPass;
class PassRegistry;

FunctionPass *createW65816ISelDag(W65816TargetMachine &TM,
                                  CodeGenOptLevel OptLevel);
FunctionPass *createW65816ExpandPseudoPass();
FunctionPass *createW65816PeepholeOptPass();

void initializeW65816DAGToDAGISelLegacyPass(PassRegistry &);
void initializeW65816ExpandPseudoPass(PassRegistry &);
void initializeW65816PeepholeOptPass(PassRegistry &);

} // namespace llvm

#endif // LLVM_LIB_TARGET_W65816_W65816_H
