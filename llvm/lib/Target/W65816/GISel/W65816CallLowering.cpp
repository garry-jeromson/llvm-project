//===-- W65816CallLowering.cpp - Call lowering -------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the lowering of LLVM calls to machine code calls for
// GlobalISel.
//
//===----------------------------------------------------------------------===//

#include "W65816CallLowering.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"
#include "W65816ISelLowering.h"
#include "W65816InstrInfo.h"
#include "W65816Subtarget.h"
#include "W65816TargetMachine.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/GlobalISel/CallLowering.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetCallingConv.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Function.h"

using namespace llvm;

// Generated calling convention implementations.
#include "W65816GenCallingConv.inc"

namespace {

struct W65816FormalArgHandler : public W65816IncomingValueHandler {
  W65816FormalArgHandler(MachineIRBuilder &MIRBuilder, MachineRegisterInfo &MRI)
      : W65816IncomingValueHandler(MIRBuilder, MRI) {}
};

struct CallReturnHandler : public W65816IncomingValueHandler {
  CallReturnHandler(MachineIRBuilder &MIRBuilder, MachineRegisterInfo &MRI,
                    MachineInstrBuilder &MIB)
      : W65816IncomingValueHandler(MIRBuilder, MRI), MIB(MIB) {}

private:
  void assignValueToReg(Register ValVReg, Register PhysReg,
                        const CCValAssign &VA) override;

  MachineInstrBuilder &MIB;
};

} // end anonymous namespace

W65816CallLowering::W65816CallLowering(const W65816TargetLowering &TLI)
    : CallLowering(&TLI) {}

struct W65816OutgoingArgHandler : public CallLowering::OutgoingValueHandler {
  W65816OutgoingArgHandler(MachineIRBuilder &MIRBuilder,
                           MachineRegisterInfo &MRI, MachineInstrBuilder MIB)
      : OutgoingValueHandler(MIRBuilder, MRI), MIB(MIB),
        DL(MIRBuilder.getMF().getDataLayout()),
        STI(MIRBuilder.getMF().getSubtarget<W65816Subtarget>()) {}

  void assignValueToReg(Register ValVReg, Register PhysReg,
                        const CCValAssign &VA) override {
    MIB.addUse(PhysReg, RegState::Implicit);
    Register ExtReg = extendRegister(ValVReg, VA);

    // Note: We previously had an optimization to materialize G_CONSTANT
    // directly into the physical register with MOV16ri. However, at this
    // stage the constant vreg may have other uses (e.g. G_ICMP) that
    // haven't been lowered yet. Erasing the G_CONSTANT leaves those uses
    // with an undefined register, causing crashes in the combiner's
    // known-bits analysis. The register allocator + peephole pass handle
    // constant materialization well enough, so this optimization is removed.

    MIRBuilder.buildCopy(PhysReg, ExtReg);
  }

  void assignValueToAddress(Register ValVReg, Register Addr, LLT MemTy,
                            const MachinePointerInfo &MPO,
                            const CCValAssign &VA) override {
    MachineFunction &MF = MIRBuilder.getMF();
    Register ExtReg = extendRegister(ValVReg, VA);
    auto *MMO = MF.getMachineMemOperand(MPO, MachineMemOperand::MOStore, MemTy,
                                        inferAlignFromPtrInfo(MF, MPO));
    MIRBuilder.buildStore(ExtReg, Addr, *MMO);
  }

  Register getStackAddress(uint64_t Size, int64_t Offset,
                           MachinePointerInfo &MPO,
                           ISD::ArgFlagsTy Flags) override {
    // Create a fixed stack object for the outgoing argument, matching the
    // SelectionDAG approach. Frame lowering resolves this to a stack-relative
    // offset. This produces G_FRAME_INDEX which selectStore already handles.
    auto &MFI = MIRBuilder.getMF().getFrameInfo();
    int FI = MFI.CreateFixedObject(Size, Offset, /*IsImmutable=*/false);
    MPO = MachinePointerInfo::getFixedStack(MIRBuilder.getMF(), FI);

    LLT FramePtr = LLT::pointer(0, 16);
    MachineInstrBuilder AddrReg = MIRBuilder.buildFrameIndex(FramePtr, FI);
    return AddrReg.getReg(0);
  }

  MachineInstrBuilder MIB;
  const DataLayout &DL;
  const W65816Subtarget &STI;
};

bool W65816CallLowering::lowerReturn(MachineIRBuilder &MIRBuilder,
                                     const Value *Val, ArrayRef<Register> VRegs,
                                     FunctionLoweringInfo &FLI,
                                     Register SwiftErrorVReg) const {
  auto MIB = MIRBuilder.buildInstrNoInsert(W65816::RTS);
  bool Success = true;
  MachineFunction &MF = MIRBuilder.getMF();
  const Function &F = MF.getFunction();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  auto &DL = F.getDataLayout();

  // Check for unsupported 32-bit and 64-bit return types.
  Type *RetTy = F.getReturnType();
  if (RetTy->isIntegerTy(32) || RetTy->isIntegerTy(64)) {
    F.getContext().diagnose(DiagnosticInfoUnsupported(
        F, "32-bit and 64-bit return values are not supported on W65816. "
           "Use 16-bit types (short, int16_t) instead."));
  }

  if (!VRegs.empty()) {
    SmallVector<ArgInfo, 8> SplitArgs;
    ArgInfo OrigArg{VRegs, Val->getType(), 0};
    setArgFlags(OrigArg, AttributeList::ReturnIndex, DL, F);
    splitToValueTypes(OrigArg, SplitArgs, DL, F.getCallingConv());
    OutgoingValueAssigner ArgAssigner(RetCC_W65816);
    W65816OutgoingArgHandler ArgHandler(MIRBuilder, MRI, MIB);
    Success = determineAndHandleAssignments(ArgHandler, ArgAssigner, SplitArgs,
                                            MIRBuilder, F.getCallingConv(),
                                            F.isVarArg());
  }
  MIRBuilder.insertInstr(MIB);
  return Success;
}

bool W65816CallLowering::lowerFormalArguments(
    MachineIRBuilder &MIRBuilder, const Function &F,
    ArrayRef<ArrayRef<Register>> VRegs, FunctionLoweringInfo &FLI) const {
  MachineFunction &MF = MIRBuilder.getMF();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  const auto &DL = F.getDataLayout();

  // Check for unsupported 32-bit and 64-bit argument types.
  for (const Argument &Arg : F.args()) {
    Type *ArgTy = Arg.getType();
    if (ArgTy->isIntegerTy(32) || ArgTy->isIntegerTy(64)) {
      F.getContext().diagnose(DiagnosticInfoUnsupported(
          F, "32-bit and 64-bit integer arguments are not supported on W65816. "
             "Use 16-bit types (short, int16_t) instead."));
      return false;
    }
  }

  SmallVector<ArgInfo, 8> SplitArgs;
  unsigned I = 0;
  for (const auto &Arg : F.args()) {
    ArgInfo OrigArg{VRegs[I], Arg.getType(), I};
    setArgFlags(OrigArg, I + AttributeList::FirstArgIndex, DL, F);
    splitToValueTypes(OrigArg, SplitArgs, DL, F.getCallingConv());
    ++I;
  }

  IncomingValueAssigner ArgAssigner(CC_W65816);
  W65816FormalArgHandler ArgHandler(MIRBuilder, MRI);
  return determineAndHandleAssignments(ArgHandler, ArgAssigner, SplitArgs,
                                       MIRBuilder, F.getCallingConv(),
                                       F.isVarArg());
}

void W65816IncomingValueHandler::assignValueToReg(Register ValVReg,
                                                  Register PhysReg,
                                                  const CCValAssign &VA) {
  MIRBuilder.getMRI()->addLiveIn(PhysReg);
  MIRBuilder.getMBB().addLiveIn(PhysReg);
  IncomingValueHandler::assignValueToReg(ValVReg, PhysReg, VA);
}

void W65816IncomingValueHandler::assignValueToAddress(
    Register ValVReg, Register Addr, LLT MemTy, const MachinePointerInfo &MPO,
    const CCValAssign &VA) {
  MachineFunction &MF = MIRBuilder.getMF();
  auto *MMO = MF.getMachineMemOperand(MPO, MachineMemOperand::MOLoad, MemTy,
                                      inferAlignFromPtrInfo(MF, MPO));
  MIRBuilder.buildLoad(ValVReg, Addr, *MMO);
}

Register W65816IncomingValueHandler::getStackAddress(uint64_t Size,
                                                     int64_t Offset,
                                                     MachinePointerInfo &MPO,
                                                     ISD::ArgFlagsTy Flags) {
  auto &MFI = MIRBuilder.getMF().getFrameInfo();
  const bool IsImmutable = !Flags.isByVal();
  int FI = MFI.CreateFixedObject(Size, Offset, IsImmutable);
  MPO = MachinePointerInfo::getFixedStack(MIRBuilder.getMF(), FI);

  llvm::LLT FramePtr = LLT::pointer(
      0, MIRBuilder.getMF().getDataLayout().getPointerSizeInBits());
  MachineInstrBuilder AddrReg = MIRBuilder.buildFrameIndex(FramePtr, FI);
  StackUsed = std::max(StackUsed, Size + Offset);
  return AddrReg.getReg(0);
}

void CallReturnHandler::assignValueToReg(Register ValVReg, Register PhysReg,
                                         const CCValAssign &VA) {
  MIB.addDef(PhysReg, RegState::Implicit);
  MIRBuilder.buildCopy(ValVReg, PhysReg);
}

bool W65816CallLowering::lowerCall(MachineIRBuilder &MIRBuilder,
                                   CallLoweringInfo &Info) const {
  MachineFunction &MF = MIRBuilder.getMF();
  Function &F = MF.getFunction();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  auto &DL = F.getDataLayout();
  const W65816Subtarget &STI = MF.getSubtarget<W65816Subtarget>();
  const TargetInstrInfo &TII = *STI.getInstrInfo();
  const W65816RegisterInfo *TRI = STI.getRegisterInfo();

  // Check for unsupported 32-bit and 64-bit types in call.
  if (Info.Callee.isGlobal()) {
    if (const auto *CalleeFunc = dyn_cast<Function>(Info.Callee.getGlobal())) {
      for (const Argument &Arg : CalleeFunc->args()) {
        Type *ArgTy = Arg.getType();
        if (ArgTy->isIntegerTy(32) || ArgTy->isIntegerTy(64)) {
          F.getContext().diagnose(DiagnosticInfoUnsupported(
              F, "32-bit and 64-bit integer arguments are not supported on "
                 "W65816. Use 16-bit types (short, int16_t) instead."));
          break;
        }
      }
      Type *RetTy = CalleeFunc->getReturnType();
      if (RetTy->isIntegerTy(32) || RetTy->isIntegerTy(64)) {
        F.getContext().diagnose(DiagnosticInfoUnsupported(
            F, "32-bit and 64-bit return values are not supported on W65816. "
               "Use 16-bit types (short, int16_t) instead."));
      }
    }
  }

  // Split outgoing arguments into value types.
  SmallVector<ArgInfo, 8> OutArgs;
  for (auto &OrigArg : Info.OrigArgs)
    splitToValueTypes(OrigArg, OutArgs, DL, Info.CallConv);

  // Split return values into value types (if non-void).
  SmallVector<ArgInfo, 8> InArgs;
  if (!Info.OrigRet.Ty->isVoidTy())
    splitToValueTypes(Info.OrigRet, InArgs, DL, Info.CallConv);

  // Emit ADJCALLSTACKDOWN to allocate space for outgoing stack arguments.
  unsigned AdjStackDown = TII.getCallFrameSetupOpcode();
  auto CallSeqStart = MIRBuilder.buildInstr(AdjStackDown);

  // Determine if this is a far call (JSL vs JSR).
  bool IsFarCall = false;
  if (Info.Callee.isGlobal()) {
    if (const auto *CalleeFunc = dyn_cast<Function>(Info.Callee.getGlobal()))
      IsFarCall = CalleeFunc->hasFnAttribute("w65816_farfunc");
  }

  unsigned CallOpc = IsFarCall ? W65816::JSL : W65816::JSR;

  // Build the call instruction with callee and call-preserved mask.
  auto MIB = MIRBuilder.buildInstrNoInsert(CallOpc)
                 .add(Info.Callee)
                 .addRegMask(TRI->getCallPreservedMask(MF, Info.CallConv));

  // Assign outgoing arguments via CC_W65816 (A, X, Y, then stack).
  OutgoingValueAssigner Assigner(CC_W65816);
  W65816OutgoingArgHandler Handler(MIRBuilder, MRI, MIB);
  if (!determineAndHandleAssignments(Handler, Assigner, OutArgs, MIRBuilder,
                                     Info.CallConv, Info.IsVarArg))
    return false;

  // Insert the call instruction.
  MIRBuilder.insertInstr(MIB);

  // Handle return values (copy from physical registers).
  if (!Info.OrigRet.Ty->isVoidTy()) {
    OutgoingValueAssigner RetAssigner(RetCC_W65816, RetCC_W65816);
    CallReturnHandler RetHandler(MIRBuilder, MRI, MIB);
    if (!determineAndHandleAssignments(RetHandler, RetAssigner, InArgs,
                                       MIRBuilder, Info.CallConv,
                                       Info.IsVarArg))
      return false;
  }

  // Update ADJCALLSTACKDOWN with the computed stack size.
  CallSeqStart.addImm(Assigner.StackSize).addImm(0);

  // Emit ADJCALLSTACKUP to restore the stack after the call.
  unsigned AdjStackUp = TII.getCallFrameDestroyOpcode();
  MIRBuilder.buildInstr(AdjStackUp).addImm(Assigner.StackSize).addImm(0);

  return true;
}
