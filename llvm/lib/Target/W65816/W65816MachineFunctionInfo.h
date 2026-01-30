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

  /// Whether this function uses Direct Page (DP) for local variable allocation.
  /// When enabled, locals are allocated in the 256-byte direct page region
  /// (addresses $00-$FF when D=0) instead of on the stack. This allows using
  /// faster 2-byte DP instructions instead of 2-byte stack-relative instructions.
  /// Enabled via the "w65816_dpframe" function attribute.
  bool UsesDPFrame = false;

  /// Whether this function uses far/long calling convention (JSL/RTL).
  /// When enabled, callers use JSL (24-bit) and the function returns with RTL.
  /// Enabled via the "w65816_farfunc" function attribute.
  bool IsFarFunction = false;

  /// Size of locals allocated in Direct Page (must be <= 256 bytes).
  unsigned DPFrameSize = 0;

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

    // Check for Direct Page frame attribute
    // Usage: __attribute__((annotate("w65816_dpframe"))) or via IR attribute
    UsesDPFrame = F.hasFnAttribute("w65816_dpframe");

    // Check for far function attribute (uses JSL/RTL instead of JSR/RTS)
    // Usage: __attribute__((w65816_farfunc))
    IsFarFunction = F.hasFnAttribute("w65816_farfunc");
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

  /// Returns true if this function uses Direct Page for local allocation.
  bool usesDPFrame() const { return UsesDPFrame; }

  /// Returns true if this function uses far/long calling convention (JSL/RTL).
  bool isFarFunction() const { return IsFarFunction; }

  /// Get the size of locals allocated in Direct Page.
  unsigned getDPFrameSize() const { return DPFrameSize; }

  /// Set the size of locals allocated in Direct Page.
  void setDPFrameSize(unsigned Size) { DPFrameSize = Size; }
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_W65816_W65816MACHINEFUNCTIONINFO_H
