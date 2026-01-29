//===-- W65816MachineFunctionInfo.h - W65816 machine func info --*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares W65816-specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_W65816MACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_W65816_W65816MACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/IR/Function.h"

namespace llvm {

/// W65816MachineFunctionInfo - This class is derived from MachineFunctionInfo
/// and contains private W65816-specific information for each MachineFunction.
class W65816MachineFunctionInfo : public MachineFunctionInfo {
  /// Whether or not the function is an interrupt handler.
  /// Interrupt handlers have special prologue/epilogue requirements:
  /// - Save all registers (P, A, X, Y)
  /// - Use RTI instead of RTS
  bool IsInterruptHandler = false;

  /// Whether or not the function is an NMI handler.
  /// NMI handlers are similar to interrupt handlers but cannot be interrupted.
  bool IsNMIHandler = false;

  /// Size of the callee-saved register portion of the stack frame in bytes.
  unsigned CalleeSavedFrameSize = 0;

  /// FrameIndex for start of varargs area.
  int VarArgsFrameIndex = 0;

public:
  W65816MachineFunctionInfo(const Function &F, const TargetSubtargetInfo *STI) {
    // Check for interrupt handler attribute
    // Usage: __attribute__((interrupt)) or __attribute__((interrupt("IRQ")))
    IsInterruptHandler = F.hasFnAttribute("interrupt");

    // Check for NMI handler attribute
    // Usage: __attribute__((interrupt("NMI")))
    if (F.hasFnAttribute("interrupt")) {
      Attribute IntAttr = F.getFnAttribute("interrupt");
      if (IntAttr.isStringAttribute()) {
        StringRef Kind = IntAttr.getValueAsString();
        if (Kind == "NMI" || Kind == "nmi") {
          IsNMIHandler = true;
        }
      }
    }
  }

  MachineFunctionInfo *
  clone(BumpPtrAllocator &Allocator, MachineFunction &DestMF,
        const DenseMap<MachineBasicBlock *, MachineBasicBlock *> &Src2DstMBB)
      const override {
    return DestMF.cloneInfo<W65816MachineFunctionInfo>(*this);
  }

  /// Returns true if this function is any kind of interrupt handler.
  bool isInterruptOrNMIHandler() const {
    return IsInterruptHandler || IsNMIHandler;
  }

  bool isInterruptHandler() const { return IsInterruptHandler; }
  bool isNMIHandler() const { return IsNMIHandler; }

  unsigned getCalleeSavedFrameSize() const { return CalleeSavedFrameSize; }
  void setCalleeSavedFrameSize(unsigned Size) { CalleeSavedFrameSize = Size; }

  int getVarArgsFrameIndex() const { return VarArgsFrameIndex; }
  void setVarArgsFrameIndex(int Index) { VarArgsFrameIndex = Index; }
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_W65816_W65816MACHINEFUNCTIONINFO_H
