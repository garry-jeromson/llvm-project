//===-- W65816LegalizerInfo.h ------------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares the targeting of the MachineLegalizer class for W65816.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_GISEL_W65816LEGALIZERINFO_H
#define LLVM_LIB_TARGET_W65816_GISEL_W65816LEGALIZERINFO_H

#include "llvm/CodeGen/GlobalISel/LegalizerInfo.h"

namespace llvm {

class MachineIRBuilder;
class W65816Subtarget;

struct W65816LegalizerInfo : public LegalizerInfo {
public:
  W65816LegalizerInfo(const W65816Subtarget &ST);

  bool legalizeCustom(LegalizerHelper &Helper, MachineInstr &MI,
                      LostDebugLocObserver &LocObserver) const override;

private:
  bool legalizeVAStart(MachineInstr &MI, MachineIRBuilder &MIRBuilder) const;
  bool legalizeExtLoad(MachineInstr &MI, MachineIRBuilder &MIRBuilder) const;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_W65816_GISEL_W65816LEGALIZERINFO_H
