//===-- W65816MCAsmInfo.h - W65816 Asm Info ---------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the W65816MCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816MCASMINFO_H
#define LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816MCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {

class Triple;

class W65816MCAsmInfo : public MCAsmInfoELF {
public:
  explicit W65816MCAsmInfo(const Triple &TT, const MCTargetOptions &Options);
};

} // namespace llvm

#endif // LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816MCASMINFO_H
