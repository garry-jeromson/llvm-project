//===-- W65816InstructionSelector.cpp ----------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the targeting of the InstructionSelector class for
// W65816.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/W65816MCTargetDesc.h"
#include "W65816InstrInfo.h"
#include "W65816RegisterBankInfo.h"
#include "W65816Subtarget.h"
#include "W65816TargetMachine.h"
#include "llvm/CodeGen/GlobalISel/GIMatchTableExecutorImpl.h"
#include "llvm/CodeGen/GlobalISel/InstructionSelector.h"
#include "llvm/CodeGen/GlobalISel/Utils.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "w65816-gisel"

using namespace llvm;

#define GET_GLOBALISEL_PREDICATE_BITSET
#include "W65816GenGlobalISel.inc"
#undef GET_GLOBALISEL_PREDICATE_BITSET

namespace {

class W65816InstructionSelector : public InstructionSelector {
public:
  W65816InstructionSelector(const W65816TargetMachine &TM,
                            const W65816Subtarget &STI,
                            const W65816RegisterBankInfo &RBI);

  bool select(MachineInstr &I) override;
  static const char *getName() { return DEBUG_TYPE; }

private:
  bool selectImpl(MachineInstr &I, CodeGenCoverage &CoverageInfo) const;

  bool selectGlobalValue(MachineInstr &I) const;
  bool selectLoad(MachineInstr &I) const;
  bool selectStore(MachineInstr &I) const;
  bool trySelectMemoryOp(MachineInstr &I) const;
  bool isConstantZero(Register Reg, MachineRegisterInfo &MRI) const;
  bool selectLibcallBinOp(MachineInstr &I, const char *LibcallName) const;
  bool selectBinOp(MachineInstr &I) const;
  bool selectZExt(MachineInstr &I) const;
  bool selectSExt(MachineInstr &I) const;
  bool selectTrunc(MachineInstr &I) const;
  bool selectPtrCast(MachineInstr &I) const;
  bool selectICmp(MachineInstr &I) const;
  bool selectBrCond(MachineInstr &I) const;
  bool selectSelect(MachineInstr &I) const;

  /// Map CmpInst::Predicate to a Select16 pseudo opcode.
  unsigned getSelect16Opcode(CmpInst::Predicate Pred) const;

  /// Look through G_INTTOPTR to find the underlying address definition.
  MachineInstr *lookThroughIntToPtr(Register Reg,
                                    MachineRegisterInfo &MRI) const;

  const W65816InstrInfo &TII;
  const W65816RegisterInfo &TRI;
  const W65816RegisterBankInfo &RBI;

#define GET_GLOBALISEL_PREDICATES_DECL
#include "W65816GenGlobalISel.inc"
#undef GET_GLOBALISEL_PREDICATES_DECL

#define GET_GLOBALISEL_TEMPORARIES_DECL
#include "W65816GenGlobalISel.inc"
#undef GET_GLOBALISEL_TEMPORARIES_DECL
};

} // end anonymous namespace

#define GET_GLOBALISEL_IMPL
#include "W65816GenGlobalISel.inc"
#undef GET_GLOBALISEL_IMPL

W65816InstructionSelector::W65816InstructionSelector(
    const W65816TargetMachine &TM, const W65816Subtarget &STI,
    const W65816RegisterBankInfo &RBI)
    : InstructionSelector(), TII(*STI.getInstrInfo()),
      TRI(*STI.getRegisterInfo()), RBI(RBI),

#define GET_GLOBALISEL_PREDICATES_INIT
#include "W65816GenGlobalISel.inc"
#undef GET_GLOBALISEL_PREDICATES_INIT
#define GET_GLOBALISEL_TEMPORARIES_INIT
#include "W65816GenGlobalISel.inc"
#undef GET_GLOBALISEL_TEMPORARIES_INIT
{
}

MachineInstr *
W65816InstructionSelector::lookThroughIntToPtr(Register Reg,
                                               MachineRegisterInfo &MRI) const {
  MachineInstr *Def = MRI.getVRegDef(Reg);
  if (!Def)
    return nullptr;
  if (Def->getOpcode() == TargetOpcode::G_INTTOPTR)
    return MRI.getVRegDef(Def->getOperand(1).getReg());
  return Def;
}

bool W65816InstructionSelector::isConstantZero(Register Reg,
                                               MachineRegisterInfo &MRI) const {
  MachineInstr *Def = MRI.getVRegDef(Reg);
  if (!Def)
    return false;
  if (Def->getOpcode() != TargetOpcode::G_CONSTANT)
    return false;
  auto *CI = Def->getOperand(1).getCImm();
  return CI && CI->isZero();
}

/// Check if a G_GLOBAL_VALUE address matches between two instructions.
/// Returns the GlobalValue and offset if they match, nullptr otherwise.
static bool isSameGlobalAddress(MachineInstr *Def1, MachineInstr *Def2) {
  if (!Def1 || !Def2)
    return false;
  if (Def1->getOpcode() != TargetOpcode::G_GLOBAL_VALUE ||
      Def2->getOpcode() != TargetOpcode::G_GLOBAL_VALUE)
    return false;
  return Def1->getOperand(1).getGlobal() == Def2->getOperand(1).getGlobal() &&
         Def1->getOperand(1).getOffset() == Def2->getOperand(1).getOffset();
}

/// Check if a constant address matches between two instructions.
static bool isSameConstantAddress(MachineInstr *Def1, MachineInstr *Def2) {
  if (!Def1 || !Def2)
    return false;
  if (Def1->getOpcode() != TargetOpcode::G_CONSTANT ||
      Def2->getOpcode() != TargetOpcode::G_CONSTANT)
    return false;
  auto *CI1 = Def1->getOperand(1).getCImm();
  auto *CI2 = Def2->getOperand(1).getCImm();
  return CI1 && CI2 && CI1->getZExtValue() == CI2->getZExtValue();
}

bool W65816InstructionSelector::trySelectMemoryOp(MachineInstr &I) const {
  // Try to fold load-modify-store patterns into single memory operations:
  //   G_STORE(G_ADD(G_LOAD(addr), 1), addr)   → INC_abs
  //   G_STORE(G_ADD(G_LOAD(addr), -1), addr)  → DEC_abs
  //   G_STORE(G_SUB(G_LOAD(addr), 1), addr)   → DEC_abs
  //   G_STORE(G_SHL(G_LOAD(addr), 1), addr)   → ASL_abs
  //   G_STORE(G_LSHR(G_LOAD(addr), 1), addr)  → LSR_abs
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  Register ValReg = I.getOperand(0).getReg();
  Register StoreAddrReg = I.getOperand(1).getReg();

  // Only handle 16-bit stores.
  LLT ValTy = MRI.getType(ValReg);
  if (ValTy.getSizeInBits() != 16)
    return false;

  // The value must be defined by an arithmetic op.
  MachineInstr *ValDef = MRI.getVRegDef(ValReg);
  if (!ValDef)
    return false;

  unsigned ValOpc = ValDef->getOpcode();
  if (ValOpc != TargetOpcode::G_ADD && ValOpc != TargetOpcode::G_SUB &&
      ValOpc != TargetOpcode::G_SHL && ValOpc != TargetOpcode::G_LSHR)
    return false;

  // The arithmetic result must have exactly one use (the store).
  if (!MRI.hasOneNonDBGUse(ValReg))
    return false;

  Register ArithSrc = ValDef->getOperand(1).getReg();
  Register ArithConst = ValDef->getOperand(2).getReg();

  // The constant operand must be 1 (or -1 for G_ADD → DEC).
  MachineInstr *ConstDef = MRI.getVRegDef(ArithConst);
  if (!ConstDef || ConstDef->getOpcode() != TargetOpcode::G_CONSTANT)
    return false;
  auto *CI = ConstDef->getOperand(1).getCImm();
  if (!CI)
    return false;
  int64_t ConstVal = CI->getSExtValue();

  // Determine the memory operation.
  unsigned MemOpc = 0;
  if (ValOpc == TargetOpcode::G_ADD && ConstVal == 1)
    MemOpc = W65816::INC_abs;
  else if (ValOpc == TargetOpcode::G_ADD &&
           (ConstVal == -1 || ConstVal == 65535))
    MemOpc = W65816::DEC_abs;
  else if (ValOpc == TargetOpcode::G_SUB && ConstVal == 1)
    MemOpc = W65816::DEC_abs;
  else if (ValOpc == TargetOpcode::G_SHL && ConstVal == 1)
    MemOpc = W65816::ASL_abs;
  else if (ValOpc == TargetOpcode::G_LSHR && ConstVal == 1)
    MemOpc = W65816::LSR_abs;
  else
    return false;

  // The source of the arithmetic must be a G_LOAD.
  MachineInstr *LoadDef = MRI.getVRegDef(ArithSrc);
  if (!LoadDef || LoadDef->getOpcode() != TargetOpcode::G_LOAD)
    return false;

  // The load result must have exactly one use (the arithmetic op).
  if (!MRI.hasOneNonDBGUse(ArithSrc))
    return false;

  // The load and store must use the same address.
  Register LoadAddrReg = LoadDef->getOperand(1).getReg();
  MachineInstr *StoreAddrDef = lookThroughIntToPtr(StoreAddrReg, MRI);
  MachineInstr *LoadAddrDef = lookThroughIntToPtr(LoadAddrReg, MRI);

  if (!StoreAddrDef || !LoadAddrDef)
    return false;

  // Check if addresses match (global value or constant address).
  if (!isSameGlobalAddress(StoreAddrDef, LoadAddrDef) &&
      !isSameConstantAddress(StoreAddrDef, LoadAddrDef))
    return false;

  // Build the address operand.
  if (StoreAddrDef->getOpcode() == TargetOpcode::G_GLOBAL_VALUE) {
    const GlobalValue *GV = StoreAddrDef->getOperand(1).getGlobal();
    int64_t Offset = StoreAddrDef->getOperand(1).getOffset();

    BuildMI(MBB, I, I.getDebugLoc(), TII.get(MemOpc))
        .addGlobalAddress(GV, Offset);
  } else {
    // Constant address.
    auto *AddrCI = StoreAddrDef->getOperand(1).getCImm();
    BuildMI(MBB, I, I.getDebugLoc(), TII.get(MemOpc))
        .addImm(AddrCI->getZExtValue());
  }

  // Erase the entire load-modify-store chain.
  I.eraseFromParent();
  ValDef->eraseFromParent();
  LoadDef->eraseFromParent();
  return true;
}

bool W65816InstructionSelector::selectGlobalValue(MachineInstr &I) const {
  MachineBasicBlock &MBB = *I.getParent();

  Register DstReg = I.getOperand(0).getReg();
  const GlobalValue *GV = I.getOperand(1).getGlobal();
  int64_t Offset = I.getOperand(1).getOffset();

  auto NewMI =
      BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::MOV16ri), DstReg)
          .addGlobalAddress(GV, Offset);

  if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI)) {
    LLVM_DEBUG(dbgs() << "Failed to constrain G_GLOBAL_VALUE\n");
    return false;
  }

  I.eraseFromParent();
  return true;
}

bool W65816InstructionSelector::selectLibcallBinOp(
    MachineInstr &I, const char *LibcallName) const {
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  Register DstReg = I.getOperand(0).getReg();
  Register LHS = I.getOperand(1).getReg();
  Register RHS = I.getOperand(2).getReg();

  // Emit: ADJCALLSTACKDOWN 0, 0
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::ADJCALLSTACKDOWN))
      .addImm(0)
      .addImm(0);

  // Copy args: LHS → A, RHS → X (runtime convention: result = A op X).
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(TargetOpcode::COPY), W65816::A)
      .addReg(LHS);
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(TargetOpcode::COPY), W65816::X)
      .addReg(RHS);

  // Emit: JSR __libcall with call-preserved mask.
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::JSR))
      .addExternalSymbol(LibcallName)
      .addRegMask(TRI.getCallPreservedMask(MF, CallingConv::C));

  // Copy result: A → DstReg.
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(TargetOpcode::COPY), DstReg)
      .addReg(W65816::A);

  // Emit: ADJCALLSTACKUP 0, 0
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::ADJCALLSTACKUP))
      .addImm(0)
      .addImm(0);

  RBI.constrainGenericRegister(LHS, W65816::GPR16RegClass, MRI);
  RBI.constrainGenericRegister(RHS, W65816::GPR16RegClass, MRI);
  RBI.constrainGenericRegister(DstReg, W65816::GPR16RegClass, MRI);

  I.eraseFromParent();
  return true;
}

bool W65816InstructionSelector::selectBinOp(MachineInstr &I) const {
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  Register DstReg = I.getOperand(0).getReg();
  Register LHS = I.getOperand(1).getReg();
  Register RHS = I.getOperand(2).getReg();

  // Check if RHS is a G_CONSTANT - use immediate-form pseudos if so.
  // The immediate-form pseudos (AND16ri, OR16ri, etc.) use GPR16 register
  // class, avoiding ACC16 constraints while still generating efficient
  // immediate-mode instructions during expansion.
  MachineInstr *RHSDef = MRI.getVRegDef(RHS);
  if (RHSDef && RHSDef->getOpcode() == TargetOpcode::G_CONSTANT) {
    int64_t Val = RHSDef->getOperand(1).getCImm()->getSExtValue();

    unsigned ImmOpc;
    switch (I.getOpcode()) {
    case TargetOpcode::G_ADD:
      ImmOpc = W65816::ADD16ri;
      break;
    case TargetOpcode::G_SUB:
      ImmOpc = W65816::SUB16ri;
      break;
    case TargetOpcode::G_AND:
      ImmOpc = W65816::AND16ri;
      break;
    case TargetOpcode::G_OR:
      ImmOpc = W65816::OR16ri;
      break;
    case TargetOpcode::G_XOR:
      ImmOpc = W65816::XOR16ri;
      break;
    default:
      return false;
    }

    auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(ImmOpc), DstReg)
                     .addReg(LHS)
                     .addImm(Val);

    if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
      return false;

    RBI.constrainGenericRegister(LHS, W65816::GPR16RegClass, MRI);

    I.eraseFromParent();
    return true;
  }

  unsigned Opc;
  switch (I.getOpcode()) {
  case TargetOpcode::G_ADD:
    Opc = W65816::ADD16rr;
    break;
  case TargetOpcode::G_SUB:
    Opc = W65816::SUB16rr;
    break;
  case TargetOpcode::G_AND:
    Opc = W65816::AND16rr;
    break;
  case TargetOpcode::G_OR:
    Opc = W65816::OR16rr;
    break;
  case TargetOpcode::G_XOR:
    Opc = W65816::XOR16rr;
    break;
  default:
    return false;
  }

  auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc), DstReg)
                   .addReg(LHS)
                   .addReg(RHS);

  if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
    return false;

  RBI.constrainGenericRegister(I.getOperand(1).getReg(), W65816::GPR16RegClass,
                               MRI);
  RBI.constrainGenericRegister(I.getOperand(2).getReg(), W65816::GPR16RegClass,
                               MRI);

  I.eraseFromParent();
  return true;
}

bool W65816InstructionSelector::selectLoad(MachineInstr &I) const {
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  Register DstReg = I.getOperand(0).getReg();
  Register AddrReg = I.getOperand(1).getReg();

  LLT DstTy = MRI.getType(DstReg);
  unsigned MemSize = DstTy.getSizeInBits();

  MachineInstr *AddrDef = lookThroughIntToPtr(AddrReg, MRI);
  if (!AddrDef)
    return false;

  MachineMemOperand *MMO = nullptr;
  if (I.memoperands_begin() != I.memoperands_end())
    MMO = *I.memoperands_begin();

  // Load from constant address (inttoptr) → LOAD_GPR16_abs.
  if (AddrDef->getOpcode() == TargetOpcode::G_CONSTANT && MemSize == 16) {
    auto *CI = AddrDef->getOperand(1).getCImm();
    if (CI) {
      auto NewMI = BuildMI(MBB, I, I.getDebugLoc(),
                           TII.get(W65816::LOAD_GPR16_abs), DstReg)
                       .addImm(CI->getZExtValue());
      if (MMO)
        NewMI.addMemOperand(MMO);

      if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
        return false;

      I.eraseFromParent();
      return true;
    }
  }

  // Load from global address.
  if (AddrDef->getOpcode() == TargetOpcode::G_GLOBAL_VALUE &&
      (MemSize == 16 || MemSize == 8)) {
    const GlobalValue *GV = AddrDef->getOperand(1).getGlobal();
    int64_t Offset = AddrDef->getOperand(1).getOffset();

    unsigned Opc = (MemSize == 16) ? W65816::LOAD_GPR16_abs : W65816::LDA8_abs;
    auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc), DstReg)
                     .addGlobalAddress(GV, Offset);
    if (MMO)
      NewMI.addMemOperand(MMO);

    if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
      return false;

    I.eraseFromParent();
    return true;
  }

  // Load from frame index.
  if (AddrDef->getOpcode() == TargetOpcode::G_FRAME_INDEX) {
    int FI = AddrDef->getOperand(1).getIndex();

    unsigned Opc;
    if (MemSize == 16)
      Opc = W65816::RELOAD_GPR16;
    else if (MemSize == 8)
      Opc = W65816::LDA8_sr;
    else
      return false;

    auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc), DstReg)
                     .addFrameIndex(FI);
    if (MMO)
      NewMI.addMemOperand(MMO);

    if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
      return false;

    I.eraseFromParent();
    return true;
  }

  // Load from PTR_ADD (base + constant offset).
  if (AddrDef->getOpcode() == TargetOpcode::G_PTR_ADD) {
    Register BaseReg = AddrDef->getOperand(1).getReg();
    Register OffsetReg = AddrDef->getOperand(2).getReg();
    MachineInstr *BaseDef = MRI.getVRegDef(BaseReg);
    MachineInstr *OffsetDef = MRI.getVRegDef(OffsetReg);

    if (!BaseDef || !OffsetDef)
      return false;

    // Global + constant offset → LOAD_GPR16_abs with combined offset.
    if (BaseDef->getOpcode() == TargetOpcode::G_GLOBAL_VALUE &&
        OffsetDef->getOpcode() == TargetOpcode::G_CONSTANT && MemSize == 16) {
      const GlobalValue *GV = BaseDef->getOperand(1).getGlobal();
      int64_t BaseOffset = BaseDef->getOperand(1).getOffset();
      int64_t ConstOffset = OffsetDef->getOperand(1).getCImm()->getSExtValue();

      auto NewMI = BuildMI(MBB, I, I.getDebugLoc(),
                           TII.get(W65816::LOAD_GPR16_abs), DstReg)
                       .addGlobalAddress(GV, BaseOffset + ConstOffset);
      if (MMO)
        NewMI.addMemOperand(MMO);

      if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
        return false;

      I.eraseFromParent();
      return true;
    }

    // Frame index + constant offset → LDA_sr_off.
    if (BaseDef->getOpcode() == TargetOpcode::G_FRAME_INDEX &&
        OffsetDef->getOpcode() == TargetOpcode::G_CONSTANT && MemSize == 16) {
      int FI = BaseDef->getOperand(1).getIndex();
      int64_t ConstOffset = OffsetDef->getOperand(1).getCImm()->getSExtValue();

      auto NewMI =
          BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::LDA_sr_off), DstReg)
              .addFrameIndex(FI)
              .addImm(ConstOffset);
      if (MMO)
        NewMI.addMemOperand(MMO);

      if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
        return false;

      I.eraseFromParent();
      return true;
    }

    // G_PTR_ADD with constant offset (non-global, non-frame base) → indirect
    // with constant index. Base pointer goes to stack slot, idx = constant.
    if (OffsetDef && OffsetDef->getOpcode() == TargetOpcode::G_CONSTANT) {
      int64_t ConstOffset = OffsetDef->getOperand(1).getCImm()->getSExtValue();

      MachineFrameInfo &FrameInfo = MF.getFrameInfo();
      int FI = FrameInfo.CreateStackObject(2, Align(2), false);

      unsigned Opc =
          (MemSize == 16) ? W65816::LDAindirect : W65816::LDA8indirect;
      auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc), DstReg)
                       .addFrameIndex(FI)
                       .addReg(BaseReg)
                       .addImm(ConstOffset);
      if (MMO)
        NewMI.addMemOperand(MMO);

      if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
        return false;

      I.eraseFromParent();
      return true;
    }

    // G_PTR_ADD with dynamic offset → indexed indirect.
    // Base pointer goes to stack slot, offset goes to Y register.
    {
      MachineFrameInfo &FrameInfo = MF.getFrameInfo();
      int FI = FrameInfo.CreateStackObject(2, Align(2), false);

      unsigned Opc =
          (MemSize == 16) ? W65816::LDAindirectIdx : W65816::LDA8indirectIdx;
      auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc), DstReg)
                       .addFrameIndex(FI)
                       .addReg(BaseReg)
                       .addReg(OffsetReg);
      if (MMO)
        NewMI.addMemOperand(MMO);

      if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
        return false;

      I.eraseFromParent();
      return true;
    }
  }

  // Fallback: generic pointer in register → stack-relative indirect load.
  // Create a stack slot for the pointer, use LDAindirect/LDA8indirect with
  // idx=0.
  if (MemSize == 16 || MemSize == 8) {
    MachineFrameInfo &FrameInfo = MF.getFrameInfo();
    int FI = FrameInfo.CreateStackObject(2, Align(2), false);

    unsigned Opc = (MemSize == 16) ? W65816::LDAindirect : W65816::LDA8indirect;
    auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc), DstReg)
                     .addFrameIndex(FI)
                     .addReg(AddrReg)
                     .addImm(0);
    if (MMO)
      NewMI.addMemOperand(MMO);

    if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
      return false;

    I.eraseFromParent();
    return true;
  }

  return false;
}

bool W65816InstructionSelector::selectStore(MachineInstr &I) const {
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  // Try memory operation combining (INC/DEC/ASL/LSR on memory) first.
  if (trySelectMemoryOp(I))
    return true;

  Register ValReg = I.getOperand(0).getReg();
  Register AddrReg = I.getOperand(1).getReg();

  LLT ValTy = MRI.getType(ValReg);
  unsigned MemSize = ValTy.getSizeInBits();

  MachineInstr *AddrDef = lookThroughIntToPtr(AddrReg, MRI);
  if (!AddrDef)
    return false;

  MachineMemOperand *MMO = nullptr;
  if (I.memoperands_begin() != I.memoperands_end())
    MMO = *I.memoperands_begin();

  // Store to constant address (inttoptr).
  if (AddrDef->getOpcode() == TargetOpcode::G_CONSTANT) {
    auto *CI = AddrDef->getOperand(1).getCImm();
    if (CI) {
      // STZ optimization: store zero directly without loading into a register.
      if (MemSize == 16 && isConstantZero(ValReg, MRI)) {
        auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::STZ_abs))
                         .addImm(CI->getZExtValue());
        if (MMO)
          NewMI.addMemOperand(MMO);
        I.eraseFromParent();
        return true;
      }

      unsigned Opc;
      if (MemSize == 16)
        Opc = W65816::STORE_GPR16_abs;
      else if (MemSize == 8)
        Opc = W65816::STA8_abs;
      else
        return false;

      auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc))
                       .addReg(ValReg)
                       .addImm(CI->getZExtValue());
      if (MMO)
        NewMI.addMemOperand(MMO);

      if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
        return false;

      I.eraseFromParent();
      return true;
    }
  }

  // Store to global address (avoids ACC16 constraint).
  if (AddrDef->getOpcode() == TargetOpcode::G_GLOBAL_VALUE) {
    const GlobalValue *GV = AddrDef->getOperand(1).getGlobal();
    int64_t Offset = AddrDef->getOperand(1).getOffset();

    // STZ optimization: store zero directly without loading into a register.
    if (MemSize == 16 && isConstantZero(ValReg, MRI)) {
      auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::STZ_abs))
                       .addGlobalAddress(GV, Offset);
      if (MMO)
        NewMI.addMemOperand(MMO);
      I.eraseFromParent();
      return true;
    }

    unsigned Opc;
    if (MemSize == 16)
      Opc = W65816::STORE_GPR16_abs;
    else if (MemSize == 8)
      Opc = W65816::STA8_abs;
    else
      return false;

    auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc))
                     .addReg(ValReg)
                     .addGlobalAddress(GV, Offset);
    if (MMO)
      NewMI.addMemOperand(MMO);

    if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
      return false;

    I.eraseFromParent();
    return true;
  }

  // Store to frame index.
  if (AddrDef->getOpcode() == TargetOpcode::G_FRAME_INDEX) {
    int FI = AddrDef->getOperand(1).getIndex();

    unsigned Opc;
    if (MemSize == 16)
      Opc = W65816::SPILL_GPR16;
    else if (MemSize == 8)
      Opc = W65816::STA8_sr;
    else
      return false;

    auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc))
                     .addReg(ValReg)
                     .addFrameIndex(FI);
    if (MMO)
      NewMI.addMemOperand(MMO);

    if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
      return false;

    I.eraseFromParent();
    return true;
  }

  // Store to PTR_ADD (base + constant offset).
  if (AddrDef->getOpcode() == TargetOpcode::G_PTR_ADD) {
    Register BaseReg = AddrDef->getOperand(1).getReg();
    Register OffsetReg = AddrDef->getOperand(2).getReg();
    MachineInstr *BaseDef = MRI.getVRegDef(BaseReg);
    MachineInstr *OffsetDef = MRI.getVRegDef(OffsetReg);

    if (!BaseDef || !OffsetDef)
      return false;

    // Global + constant offset → store to absolute address.
    if (BaseDef->getOpcode() == TargetOpcode::G_GLOBAL_VALUE &&
        OffsetDef->getOpcode() == TargetOpcode::G_CONSTANT) {
      const GlobalValue *GV = BaseDef->getOperand(1).getGlobal();
      int64_t BaseOffset = BaseDef->getOperand(1).getOffset();
      int64_t ConstOffset = OffsetDef->getOperand(1).getCImm()->getSExtValue();

      // STZ optimization: store zero directly.
      if (MemSize == 16 && isConstantZero(ValReg, MRI)) {
        auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::STZ_abs))
                         .addGlobalAddress(GV, BaseOffset + ConstOffset);
        if (MMO)
          NewMI.addMemOperand(MMO);
        I.eraseFromParent();
        return true;
      }

      unsigned Opc;
      if (MemSize == 16)
        Opc = W65816::STORE_GPR16_abs;
      else if (MemSize == 8)
        Opc = W65816::STA8_abs;
      else
        return false;

      auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc))
                       .addReg(ValReg)
                       .addGlobalAddress(GV, BaseOffset + ConstOffset);
      if (MMO)
        NewMI.addMemOperand(MMO);

      if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
        return false;

      I.eraseFromParent();
      return true;
    }

    // Frame index + constant offset → STA_sr_off.
    if (BaseDef->getOpcode() == TargetOpcode::G_FRAME_INDEX &&
        OffsetDef->getOpcode() == TargetOpcode::G_CONSTANT && MemSize == 16) {
      int FI = BaseDef->getOperand(1).getIndex();
      int64_t ConstOffset = OffsetDef->getOperand(1).getCImm()->getSExtValue();

      auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::STA_sr_off))
                       .addReg(ValReg)
                       .addFrameIndex(FI)
                       .addImm(ConstOffset);
      if (MMO)
        NewMI.addMemOperand(MMO);

      if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
        return false;

      I.eraseFromParent();
      return true;
    }

    // G_PTR_ADD with constant offset (non-global, non-frame base) → indirect
    // store with constant index.
    if (OffsetDef && OffsetDef->getOpcode() == TargetOpcode::G_CONSTANT) {
      int64_t ConstOffset = OffsetDef->getOperand(1).getCImm()->getSExtValue();

      MachineFrameInfo &FrameInfo = MF.getFrameInfo();
      int FI = FrameInfo.CreateStackObject(2, Align(2), false);

      unsigned Opc =
          (MemSize == 16) ? W65816::STAindirect : W65816::STA8indirect;
      auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc))
                       .addReg(ValReg)
                       .addFrameIndex(FI)
                       .addReg(BaseReg)
                       .addImm(ConstOffset);
      if (MMO)
        NewMI.addMemOperand(MMO);

      if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
        return false;

      I.eraseFromParent();
      return true;
    }

    // G_PTR_ADD with dynamic offset → indexed indirect store.
    {
      MachineFrameInfo &FrameInfo = MF.getFrameInfo();
      int FI = FrameInfo.CreateStackObject(2, Align(2), false);

      unsigned Opc =
          (MemSize == 16) ? W65816::STAindirectIdx : W65816::STA8indirectIdx;
      auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc))
                       .addReg(ValReg)
                       .addFrameIndex(FI)
                       .addReg(BaseReg)
                       .addReg(OffsetReg);
      if (MMO)
        NewMI.addMemOperand(MMO);

      if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
        return false;

      I.eraseFromParent();
      return true;
    }
  }

  // Fallback: generic pointer in register → stack-relative indirect store.
  if (MemSize == 16 || MemSize == 8) {
    MachineFrameInfo &FrameInfo = MF.getFrameInfo();
    int FI = FrameInfo.CreateStackObject(2, Align(2), false);

    unsigned Opc = (MemSize == 16) ? W65816::STAindirect : W65816::STA8indirect;
    auto NewMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(Opc))
                     .addReg(ValReg)
                     .addFrameIndex(FI)
                     .addReg(AddrReg)
                     .addImm(0);
    if (MMO)
      NewMI.addMemOperand(MMO);

    if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
      return false;

    I.eraseFromParent();
    return true;
  }

  return false;
}

bool W65816InstructionSelector::selectZExt(MachineInstr &I) const {
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  Register DstReg = I.getOperand(0).getReg();
  Register SrcReg = I.getOperand(1).getReg();
  LLT SrcTy = MRI.getType(SrcReg);
  LLT DstTy = MRI.getType(DstReg);

  if (DstTy == LLT::scalar(8) && SrcTy == LLT::scalar(1)) {
    // i1→i8: COPY + AND with 0x0001.
    // On W65816, s8 values live in GPR16 registers, so this is the same
    // as i1→i16 - just mask off all but the low bit.
    Register TmpReg = MRI.createVirtualRegister(&W65816::GPR16RegClass);
    BuildMI(MBB, I, I.getDebugLoc(), TII.get(TargetOpcode::COPY), TmpReg)
        .addReg(SrcReg);

    Register MaskReg = MRI.createVirtualRegister(&W65816::GPR16RegClass);
    BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::MOV16ri), MaskReg)
        .addImm(0x0001);

    auto AndMI =
        BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::AND16rr), DstReg)
            .addReg(TmpReg)
            .addReg(MaskReg);

    if (!constrainSelectedInstRegOperands(*AndMI, TII, TRI, RBI))
      return false;

    RBI.constrainGenericRegister(SrcReg, W65816::GPR16RegClass, MRI);

    I.eraseFromParent();
    return true;
  }

  if (DstTy != LLT::scalar(16))
    return false;

  if (SrcTy == LLT::scalar(8)) {
    // Try to fold G_LOAD + G_ZEXT into a single LOAD8_ZEXT_GPR16_abs pseudo.
    // This avoids ACC16 pressure from separate load+zext.
    // Only fold if the load has exactly one user (the ZEXT).
    // Don't fold boolean (s1) loads: they are followed by AND with 1
    // (AND_imm16) which constrains the result to ACC16. LOAD8_ZEXT_GPR16_abs
    // has GPR16 output + implicit-def A, and constraining to ACC16 conflicts.
    MachineInstr *SrcDef = MRI.getVRegDef(SrcReg);
    bool IsBoolLoad = false;
    if (SrcDef && SrcDef->getOpcode() == TargetOpcode::G_LOAD) {
      if (SrcDef->memoperands_begin() != SrcDef->memoperands_end()) {
        MachineMemOperand *LoadMMO = *SrcDef->memoperands_begin();
        IsBoolLoad = LoadMMO && LoadMMO->getSizeInBits() == 1;
      }
    }

    if (!IsBoolLoad && SrcDef && SrcDef->getOpcode() == TargetOpcode::G_LOAD &&
        MRI.hasOneNonDBGUse(SrcReg)) {
      Register LoadAddrReg = SrcDef->getOperand(1).getReg();
      MachineInstr *LoadAddrDef = lookThroughIntToPtr(LoadAddrReg, MRI);

      MachineMemOperand *MMO = nullptr;
      if (SrcDef->memoperands_begin() != SrcDef->memoperands_end())
        MMO = *SrcDef->memoperands_begin();

      if (LoadAddrDef &&
          LoadAddrDef->getOpcode() == TargetOpcode::G_GLOBAL_VALUE) {
        const GlobalValue *GV = LoadAddrDef->getOperand(1).getGlobal();
        int64_t Offset = LoadAddrDef->getOperand(1).getOffset();

        auto NewMI = BuildMI(MBB, I, I.getDebugLoc(),
                             TII.get(W65816::LOAD8_ZEXT_GPR16_abs), DstReg)
                         .addGlobalAddress(GV, Offset);

        if (MMO)
          NewMI.addMemOperand(MMO);

        if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
          return false;

        // Erase both the ZEXT and the folded LOAD.
        I.eraseFromParent();
        SrcDef->eraseFromParent();
        return true;
      }

      // Frame index → LOAD8_ZEXT_GPR16_sr pseudo.
      if (LoadAddrDef &&
          LoadAddrDef->getOpcode() == TargetOpcode::G_FRAME_INDEX) {
        int FI = LoadAddrDef->getOperand(1).getIndex();

        auto NewMI = BuildMI(MBB, I, I.getDebugLoc(),
                             TII.get(W65816::LOAD8_ZEXT_GPR16_sr), DstReg)
                         .addFrameIndex(FI);

        if (MMO)
          NewMI.addMemOperand(MMO);

        if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
          return false;

        I.eraseFromParent();
        SrcDef->eraseFromParent();
        return true;
      }

      // PTR_ADD(global, const) → LOAD8_ZEXT_GPR16_abs with combined offset.
      if (LoadAddrDef && LoadAddrDef->getOpcode() == TargetOpcode::G_PTR_ADD) {
        Register BaseReg = LoadAddrDef->getOperand(1).getReg();
        Register OffReg = LoadAddrDef->getOperand(2).getReg();
        MachineInstr *BaseDef = MRI.getVRegDef(BaseReg);
        MachineInstr *OffDef = MRI.getVRegDef(OffReg);

        if (BaseDef && OffDef &&
            BaseDef->getOpcode() == TargetOpcode::G_GLOBAL_VALUE &&
            OffDef->getOpcode() == TargetOpcode::G_CONSTANT) {
          const GlobalValue *GV = BaseDef->getOperand(1).getGlobal();
          int64_t BaseOffset = BaseDef->getOperand(1).getOffset();
          int64_t ConstOffset = OffDef->getOperand(1).getCImm()->getSExtValue();

          auto NewMI = BuildMI(MBB, I, I.getDebugLoc(),
                               TII.get(W65816::LOAD8_ZEXT_GPR16_abs), DstReg)
                           .addGlobalAddress(GV, BaseOffset + ConstOffset);

          if (MMO)
            NewMI.addMemOperand(MMO);

          if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
            return false;

          I.eraseFromParent();
          SrcDef->eraseFromParent();
          return true;
        }
      }
    }

    if (IsBoolLoad) {
      // For boolean loads, the value will be masked by AND #1 downstream,
      // so ZEXT8_GPR16 (AND #$00FF) is redundant. Use COPY to avoid the
      // register conflict between ZEXT8_GPR16's implicit-def A and
      // AND_imm16's ACC16 constraint.
      BuildMI(MBB, I, I.getDebugLoc(), TII.get(TargetOpcode::COPY), DstReg)
          .addReg(SrcReg);

      RBI.constrainGenericRegister(DstReg, W65816::GPR16RegClass, MRI);
      RBI.constrainGenericRegister(SrcReg, W65816::GPR16RegClass, MRI);

      I.eraseFromParent();
      return true;
    }

    // Fallback: use ZEXT8_GPR16 pseudo (no tied operand, Defs=[A]).
    auto NewMI =
        BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::ZEXT8_GPR16), DstReg)
            .addReg(SrcReg);

    if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
      return false;

    RBI.constrainGenericRegister(SrcReg, W65816::GPR16RegClass, MRI);

    I.eraseFromParent();
    return true;
  }

  if (SrcTy != LLT::scalar(1))
    return false;

  // i1→i16: COPY + AND with 0x0001.
  Register TmpReg = MRI.createVirtualRegister(&W65816::GPR16RegClass);
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(TargetOpcode::COPY), TmpReg)
      .addReg(SrcReg);

  Register MaskReg = MRI.createVirtualRegister(&W65816::GPR16RegClass);
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::MOV16ri), MaskReg)
      .addImm(0x0001);

  auto AndMI =
      BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::AND16rr), DstReg)
          .addReg(TmpReg)
          .addReg(MaskReg);

  if (!constrainSelectedInstRegOperands(*AndMI, TII, TRI, RBI))
    return false;

  RBI.constrainGenericRegister(SrcReg, W65816::GPR16RegClass, MRI);

  I.eraseFromParent();
  return true;
}

bool W65816InstructionSelector::selectSExt(MachineInstr &I) const {
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  Register DstReg = I.getOperand(0).getReg();
  Register SrcReg = I.getOperand(1).getReg();
  LLT SrcTy = MRI.getType(SrcReg);
  LLT DstTy = MRI.getType(DstReg);

  if (DstTy != LLT::scalar(16))
    return false;

  if (SrcTy == LLT::scalar(1)) {
    // SEXT i1→i16: 0→0, 1→0xFFFF (-1).
    // Compute: 0 - val (two's complement negation).
    Register ZeroReg = MRI.createVirtualRegister(&W65816::GPR16RegClass);
    BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::MOV16ri), ZeroReg)
        .addImm(0);

    auto SubMI =
        BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::SUB16rr), DstReg)
            .addReg(ZeroReg)
            .addReg(SrcReg);

    if (!constrainSelectedInstRegOperands(*SubMI, TII, TRI, RBI))
      return false;

    RBI.constrainGenericRegister(SrcReg, W65816::GPR16RegClass, MRI);

    I.eraseFromParent();
    return true;
  }

  if (SrcTy == LLT::scalar(8)) {
    // i8→i16 SEXT: use SEXT8_GPR16 pseudo (branchless, no tied operand).
    auto NewMI =
        BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::SEXT8_GPR16), DstReg)
            .addReg(SrcReg);

    if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
      return false;

    RBI.constrainGenericRegister(SrcReg, W65816::GPR16RegClass, MRI);

    I.eraseFromParent();
    return true;
  }

  return false;
}

bool W65816InstructionSelector::selectTrunc(MachineInstr &I) const {
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  Register DstReg = I.getOperand(0).getReg();
  Register SrcReg = I.getOperand(1).getReg();

  // Truncation is just taking the low bits - replace with COPY.
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(TargetOpcode::COPY), DstReg)
      .addReg(SrcReg);

  RBI.constrainGenericRegister(DstReg, W65816::GPR16RegClass, MRI);
  RBI.constrainGenericRegister(SrcReg, W65816::GPR16RegClass, MRI);

  I.eraseFromParent();
  return true;
}

bool W65816InstructionSelector::selectPtrCast(MachineInstr &I) const {
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  Register DstReg = I.getOperand(0).getReg();
  Register SrcReg = I.getOperand(1).getReg();

  // Pointer casts are no-ops on W65816 (pointers = 16-bit integers).
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(TargetOpcode::COPY), DstReg)
      .addReg(SrcReg);

  RBI.constrainGenericRegister(DstReg, W65816::GPR16RegClass, MRI);
  RBI.constrainGenericRegister(SrcReg, W65816::GPR16RegClass, MRI);

  I.eraseFromParent();
  return true;
}

unsigned
W65816InstructionSelector::getSelect16Opcode(CmpInst::Predicate Pred) const {
  switch (Pred) {
  case CmpInst::ICMP_EQ:
    return W65816::Select16_EQ;
  case CmpInst::ICMP_NE:
    return W65816::Select16_NE;
  case CmpInst::ICMP_ULT:
    return W65816::Select16_CC;
  case CmpInst::ICMP_UGE:
    return W65816::Select16_CS;
  case CmpInst::ICMP_UGT:
    return W65816::Select16_UGT;
  case CmpInst::ICMP_ULE:
    return W65816::Select16_ULE;
  case CmpInst::ICMP_SLT:
    return W65816::Select16_SLT;
  case CmpInst::ICMP_SGE:
    return W65816::Select16_SGE;
  case CmpInst::ICMP_SGT:
    return W65816::Select16_SGT;
  case CmpInst::ICMP_SLE:
    return W65816::Select16_SLE;
  default:
    return 0;
  }
}

bool W65816InstructionSelector::selectICmp(MachineInstr &I) const {
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  Register DstReg = I.getOperand(0).getReg();
  auto Pred = static_cast<CmpInst::Predicate>(I.getOperand(1).getPredicate());
  Register LHS = I.getOperand(2).getReg();
  Register RHS = I.getOperand(3).getReg();

  // If the ICMP result has no uses (already folded into BRCOND/SELECT),
  // just erase it.
  if (MRI.use_nodbg_empty(DstReg)) {
    I.eraseFromParent();
    return true;
  }

  // Materialize the i1 result: CMP + Select16_xx(1, 0).
  unsigned SelectOpc = getSelect16Opcode(Pred);
  if (!SelectOpc)
    return false;

  // Load constants for true (1) and false (0) BEFORE the CMP.
  // MOV16ri has Defs=[P] (clobbers processor flags), so it must be
  // emitted before the CMP to avoid clobbering the comparison result
  // that the Select16 pseudo needs.
  Register TrueReg = MRI.createVirtualRegister(&W65816::GPR16RegClass);
  Register FalseReg = MRI.createVirtualRegister(&W65816::GPR16RegClass);
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::MOV16ri), TrueReg).addImm(1);
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::MOV16ri), FalseReg)
      .addImm(0);

  // Emit CMP after constants are materialized.
  MachineInstr *RHSDef = MRI.getVRegDef(RHS);
  if (RHSDef && RHSDef->getOpcode() == TargetOpcode::G_CONSTANT) {
    auto *CI = RHSDef->getOperand(1).getCImm();
    if (CI) {
      int64_t Imm = CI->getSExtValue();
      BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::CMP16ri))
          .addReg(LHS)
          .addImm(Imm);
    } else {
      BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::CMP16rr))
          .addReg(LHS)
          .addReg(RHS);
    }
  } else {
    BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::CMP16rr))
        .addReg(LHS)
        .addReg(RHS);
  }

  // Select based on condition.
  auto SelectMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(SelectOpc), DstReg)
                      .addReg(TrueReg)
                      .addReg(FalseReg);

  if (!constrainSelectedInstRegOperands(*SelectMI, TII, TRI, RBI))
    return false;

  RBI.constrainGenericRegister(LHS, W65816::GPR16RegClass, MRI);
  RBI.constrainGenericRegister(RHS, W65816::GPR16RegClass, MRI);

  I.eraseFromParent();
  return true;
}

bool W65816InstructionSelector::selectBrCond(MachineInstr &I) const {
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  Register CondReg = I.getOperand(0).getReg();
  MachineBasicBlock *DestMBB = I.getOperand(1).getMBB();

  // Try to fold G_ICMP into the branch.
  MachineInstr *CondDef = MRI.getVRegDef(CondReg);
  if (CondDef && CondDef->getOpcode() == TargetOpcode::G_ICMP) {
    auto Pred =
        static_cast<CmpInst::Predicate>(CondDef->getOperand(1).getPredicate());
    Register LHS = CondDef->getOperand(2).getReg();
    Register RHS = CondDef->getOperand(3).getReg();

    // Map predicate to W65816 BR_CC condition code.
    // These must match the W65816CC enum in ExpandPseudo.
    unsigned W65CC;
    switch (Pred) {
    case CmpInst::ICMP_EQ:
      W65CC = 0; // COND_EQ
      break;
    case CmpInst::ICMP_NE:
      W65CC = 1; // COND_NE
      break;
    case CmpInst::ICMP_UGE:
      W65CC = 2; // COND_CS
      break;
    case CmpInst::ICMP_ULT:
      W65CC = 3; // COND_CC
      break;
    case CmpInst::ICMP_SLT:
      W65CC = 8; // COND_SLT
      break;
    case CmpInst::ICMP_SGE:
      W65CC = 9; // COND_SGE
      break;
    case CmpInst::ICMP_SGT:
      W65CC = 10; // COND_SGT
      break;
    case CmpInst::ICMP_SLE:
      W65CC = 11; // COND_SLE
      break;
    case CmpInst::ICMP_UGT:
      W65CC = 12; // COND_UGT
      break;
    case CmpInst::ICMP_ULE:
      W65CC = 13; // COND_ULE
      break;
    default:
      return false;
    }

    // Emit CMP.
    MachineInstr *RHSDef = MRI.getVRegDef(RHS);
    if (RHSDef && RHSDef->getOpcode() == TargetOpcode::G_CONSTANT) {
      auto *CI = RHSDef->getOperand(1).getCImm();
      if (CI) {
        BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::CMP16ri))
            .addReg(LHS)
            .addImm(CI->getSExtValue());
      } else {
        BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::CMP16rr))
            .addReg(LHS)
            .addReg(RHS);
      }
    } else {
      BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::CMP16rr))
          .addReg(LHS)
          .addReg(RHS);
    }

    RBI.constrainGenericRegister(LHS, W65816::GPR16RegClass, MRI);
    RBI.constrainGenericRegister(RHS, W65816::GPR16RegClass, MRI);

    // Emit BR_CC.
    BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::BR_CC))
        .addMBB(DestMBB)
        .addImm(W65CC);

    I.eraseFromParent();
    return true;
  }

  // Non-ICMP condition: compare the boolean value with 0, branch if NE.
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::CMP16ri))
      .addReg(CondReg)
      .addImm(0);
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::BR_CC))
      .addMBB(DestMBB)
      .addImm(1); // COND_NE

  RBI.constrainGenericRegister(CondReg, W65816::GPR16RegClass, MRI);

  I.eraseFromParent();
  return true;
}

bool W65816InstructionSelector::selectSelect(MachineInstr &I) const {
  MachineBasicBlock &MBB = *I.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  Register DstReg = I.getOperand(0).getReg();
  Register CondReg = I.getOperand(1).getReg();
  Register TrueReg = I.getOperand(2).getReg();
  Register FalseReg = I.getOperand(3).getReg();

  // Try to fold G_ICMP into the select.
  MachineInstr *CondDef = MRI.getVRegDef(CondReg);
  if (CondDef && CondDef->getOpcode() == TargetOpcode::G_ICMP) {
    auto Pred =
        static_cast<CmpInst::Predicate>(CondDef->getOperand(1).getPredicate());
    Register LHS = CondDef->getOperand(2).getReg();
    Register RHS = CondDef->getOperand(3).getReg();

    unsigned SelectOpc = getSelect16Opcode(Pred);
    if (!SelectOpc)
      return false;

    // Emit CMP.
    MachineInstr *RHSDef = MRI.getVRegDef(RHS);
    if (RHSDef && RHSDef->getOpcode() == TargetOpcode::G_CONSTANT) {
      auto *CI = RHSDef->getOperand(1).getCImm();
      if (CI) {
        BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::CMP16ri))
            .addReg(LHS)
            .addImm(CI->getSExtValue());
      } else {
        BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::CMP16rr))
            .addReg(LHS)
            .addReg(RHS);
      }
    } else {
      BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::CMP16rr))
          .addReg(LHS)
          .addReg(RHS);
    }

    // Emit Select16_xx.
    auto SelectMI = BuildMI(MBB, I, I.getDebugLoc(), TII.get(SelectOpc), DstReg)
                        .addReg(TrueReg)
                        .addReg(FalseReg);

    if (!constrainSelectedInstRegOperands(*SelectMI, TII, TRI, RBI))
      return false;

    RBI.constrainGenericRegister(LHS, W65816::GPR16RegClass, MRI);
    RBI.constrainGenericRegister(RHS, W65816::GPR16RegClass, MRI);

    I.eraseFromParent();
    return true;
  }

  // Non-ICMP condition: compare boolean with 0, select based on NE.
  BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::CMP16ri))
      .addReg(CondReg)
      .addImm(0);
  auto SelectMI =
      BuildMI(MBB, I, I.getDebugLoc(), TII.get(W65816::Select16_NE), DstReg)
          .addReg(TrueReg)
          .addReg(FalseReg);

  if (!constrainSelectedInstRegOperands(*SelectMI, TII, TRI, RBI))
    return false;

  RBI.constrainGenericRegister(CondReg, W65816::GPR16RegClass, MRI);

  I.eraseFromParent();
  return true;
}

bool W65816InstructionSelector::select(MachineInstr &I) {
  if (!isPreISelGenericOpcode(I.getOpcode())) {
    // For COPY instructions involving physical registers, constrain the
    // virtual register to a register class matching the physical register.
    // Without this, COPYs emitted by call lowering (e.g., COPY $a -> %vreg)
    // leave the vreg without a register class, causing post-ISel failures.
    if (I.getOpcode() == TargetOpcode::COPY) {
      MachineFunction &MF = *I.getParent()->getParent();
      MachineRegisterInfo &MRI = MF.getRegInfo();
      for (auto &MO : I.operands()) {
        if (!MO.isReg() || !MO.getReg() || !MO.getReg().isVirtual())
          continue;
        if (MRI.getRegClassOrNull(MO.getReg()))
          continue;
        // Find the physical register in this COPY and constrain the vreg.
        // Use GPR16 for A/X/Y to give the register allocator flexibility.
        // Using getMinimalPhysRegClass would return ACC16 for $a, which
        // over-constrains the vreg to only the A register.
        for (auto &Other : I.operands()) {
          if (!Other.isReg() || !Other.getReg() || !Other.getReg().isPhysical())
            continue;
          const TargetRegisterClass *RC;
          if (W65816::GPR16RegClass.contains(Other.getReg()))
            RC = &W65816::GPR16RegClass;
          else
            RC = TRI.getMinimalPhysRegClass(Other.getReg());
          if (RC)
            RBI.constrainGenericRegister(MO.getReg(), *RC, MRI);
          break;
        }
      }
    }
    return true;
  }

  // Try custom selection first for operations without TableGen patterns.
  MachineFunction &MF = *I.getParent()->getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  switch (I.getOpcode()) {
  case TargetOpcode::G_PHI: {
    // Convert G_PHI to target PHI with GPR16 register class.
    Register DstReg = I.getOperand(0).getReg();
    I.setDesc(TII.get(TargetOpcode::PHI));
    RBI.constrainGenericRegister(DstReg, W65816::GPR16RegClass, MRI);
    return true;
  }
  case TargetOpcode::G_IMPLICIT_DEF: {
    Register DstReg = I.getOperand(0).getReg();
    I.setDesc(TII.get(TargetOpcode::IMPLICIT_DEF));
    RBI.constrainGenericRegister(DstReg, W65816::GPR16RegClass, MRI);
    return true;
  }
  case TargetOpcode::G_FRAME_INDEX: {
    // Frame index used as a pointer value (not as load/store address).
    // Materialize the stack address using LEA_fi pseudo.
    Register DstReg = I.getOperand(0).getReg();
    int FI = I.getOperand(1).getIndex();
    auto NewMI = BuildMI(*I.getParent(), I, I.getDebugLoc(),
                         TII.get(W65816::LEA_fi), DstReg)
                     .addFrameIndex(FI);
    if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
      return false;
    I.eraseFromParent();
    return true;
  }
  case TargetOpcode::G_PTR_ADD: {
    // PTR_ADD used as a value: check if it's frame index + constant,
    // global + constant, or generic ptr + constant.
    Register DstReg = I.getOperand(0).getReg();
    Register BaseReg = I.getOperand(1).getReg();
    Register OffsetReg = I.getOperand(2).getReg();
    MachineInstr *BaseDef = MRI.getVRegDef(BaseReg);
    MachineInstr *OffsetDef = MRI.getVRegDef(OffsetReg);

    if (BaseDef && OffsetDef) {
      // Frame index + constant → ADD16ri(LEA_fi(FI), constant)
      if (BaseDef->getOpcode() == TargetOpcode::G_FRAME_INDEX &&
          OffsetDef->getOpcode() == TargetOpcode::G_CONSTANT) {
        int FI = BaseDef->getOperand(1).getIndex();
        int64_t Offset = OffsetDef->getOperand(1).getCImm()->getSExtValue();

        // LEA_fi gets the base address, then ADD16ri adds the offset.
        Register TmpReg = MRI.createVirtualRegister(&W65816::GPR16RegClass);
        BuildMI(*I.getParent(), I, I.getDebugLoc(), TII.get(W65816::LEA_fi),
                TmpReg)
            .addFrameIndex(FI);

        auto AddMI = BuildMI(*I.getParent(), I, I.getDebugLoc(),
                             TII.get(W65816::ADD16ri), DstReg)
                         .addReg(TmpReg)
                         .addImm(Offset);

        if (!constrainSelectedInstRegOperands(*AddMI, TII, TRI, RBI))
          return false;

        I.eraseFromParent();
        return true;
      }

      // Global + constant → MOV16ri(global + offset)
      if (BaseDef->getOpcode() == TargetOpcode::G_GLOBAL_VALUE &&
          OffsetDef->getOpcode() == TargetOpcode::G_CONSTANT) {
        const GlobalValue *GV = BaseDef->getOperand(1).getGlobal();
        int64_t BaseOffset = BaseDef->getOperand(1).getOffset();
        int64_t ConstOffset =
            OffsetDef->getOperand(1).getCImm()->getSExtValue();

        auto NewMI = BuildMI(*I.getParent(), I, I.getDebugLoc(),
                             TII.get(W65816::MOV16ri), DstReg)
                         .addGlobalAddress(GV, BaseOffset + ConstOffset);

        if (!constrainSelectedInstRegOperands(*NewMI, TII, TRI, RBI))
          return false;

        I.eraseFromParent();
        return true;
      }
    }

    // Generic PTR_ADD: base + offset → ADD16rr.
    auto AddMI = BuildMI(*I.getParent(), I, I.getDebugLoc(),
                         TII.get(W65816::ADD16rr), DstReg)
                     .addReg(BaseReg)
                     .addReg(OffsetReg);

    if (!constrainSelectedInstRegOperands(*AddMI, TII, TRI, RBI))
      return false;

    I.eraseFromParent();
    return true;
  }
  case TargetOpcode::G_GLOBAL_VALUE:
    return selectGlobalValue(I);
  case TargetOpcode::G_ADD:
  case TargetOpcode::G_SUB:
  case TargetOpcode::G_AND:
  case TargetOpcode::G_OR:
  case TargetOpcode::G_XOR:
    if (selectBinOp(I))
      return true;
    break;
  case TargetOpcode::G_LOAD:
    if (selectLoad(I))
      return true;
    break;
  case TargetOpcode::G_STORE:
    if (selectStore(I))
      return true;
    break;
  case TargetOpcode::G_ZEXT:
  case TargetOpcode::G_ANYEXT:
    if (selectZExt(I))
      return true;
    break;
  case TargetOpcode::G_SEXT:
    if (selectSExt(I))
      return true;
    break;
  case TargetOpcode::G_TRUNC:
    if (selectTrunc(I))
      return true;
    break;
  case TargetOpcode::G_MUL:
    return selectLibcallBinOp(I, "__mulhi3");
  case TargetOpcode::G_SDIV:
    return selectLibcallBinOp(I, "__divhi3");
  case TargetOpcode::G_UDIV:
    return selectLibcallBinOp(I, "__udivhi3");
  case TargetOpcode::G_SREM:
    return selectLibcallBinOp(I, "__modhi3");
  case TargetOpcode::G_UREM:
    return selectLibcallBinOp(I, "__umodhi3");
  case TargetOpcode::G_INTTOPTR:
  case TargetOpcode::G_PTRTOINT:
    return selectPtrCast(I);
  case TargetOpcode::G_ICMP:
    return selectICmp(I);
  case TargetOpcode::G_BRCOND:
    return selectBrCond(I);
  case TargetOpcode::G_SELECT:
    if (selectSelect(I))
      return true;
    break;
  case TargetOpcode::G_BR:
    I.setDesc(TII.get(W65816::BRA));
    return constrainSelectedInstRegOperands(I, TII, TRI, RBI);
  default:
    break;
  }

  // Fall through to TableGen-generated patterns.
  if (selectImpl(I, *CoverageInfo))
    return true;

  return false;
}

namespace llvm {
InstructionSelector *
createW65816InstructionSelector(const W65816TargetMachine &TM,
                                const W65816Subtarget &Subtarget,
                                const W65816RegisterBankInfo &RBI) {
  return new W65816InstructionSelector(TM, Subtarget, RBI);
}
} // end namespace llvm
