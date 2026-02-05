//===-- W65816ISelLowering.cpp - W65816 DAG Lowering Implementation -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the W65816 target lowering interface used by both
// GlobalISel and general LLVM infrastructure (type legality, inline asm, etc.).
//
//===----------------------------------------------------------------------===//

#include "W65816ISelLowering.h"

#include "MCTargetDesc/W65816MCTargetDesc.h"
#include "W65816.h"
#include "W65816InstrInfo.h"
#include "W65816MachineFunctionInfo.h"
#include "W65816Subtarget.h"
#include "W65816TargetMachine.h"

#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define DEBUG_TYPE "w65816-lower"

#include "W65816GenCallingConv.inc"

W65816TargetLowering::W65816TargetLowering(const TargetMachine &TM,
                                           const W65816Subtarget &STI)
    : TargetLowering(TM, STI), Subtarget(STI) {

  // Set up the register classes based on M and X processor flags
  if (STI.uses8BitAccumulator()) {
    addRegisterClass(MVT::i8, &W65816::ACC8RegClass);
  } else {
    addRegisterClass(MVT::i16, &W65816::ACC16RegClass);
  }

  if (STI.uses8BitIndex()) {
    addRegisterClass(MVT::i8, &W65816::IDX8RegClass);
  } else {
    addRegisterClass(MVT::i16, &W65816::IDX16RegClass);
  }

  addRegisterClass(MVT::i16, &W65816::GPR16RegClass);

  computeRegisterProperties(STI.getRegisterInfo());

  setStackPointerRegisterToSaveRestore(W65816::SP);
  setSchedulingPreference(Sched::RegPressure);
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrOneBooleanContent);

  // Division and remainder (no hardware support)
  setOperationAction(ISD::SDIV, MVT::i16, Expand);
  setOperationAction(ISD::UDIV, MVT::i16, Expand);
  setOperationAction(ISD::SREM, MVT::i16, Expand);
  setOperationAction(ISD::UREM, MVT::i16, Expand);
  setOperationAction(ISD::SDIVREM, MVT::i16, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i16, Expand);

  // Multiplication (no hardware support)
  setOperationAction(ISD::MUL, MVT::i16, Expand);
  setOperationAction(ISD::MULHS, MVT::i16, Expand);
  setOperationAction(ISD::MULHU, MVT::i16, Expand);
  setOperationAction(ISD::SMUL_LOHI, MVT::i16, Expand);
  setOperationAction(ISD::UMUL_LOHI, MVT::i16, Expand);

  // Shifts
  setOperationAction(ISD::SHL, MVT::i16, Legal);
  setOperationAction(ISD::SRL, MVT::i16, Legal);
  setOperationAction(ISD::SRA, MVT::i16, Legal);
  setOperationAction(ISD::ROTL, MVT::i16, Expand);
  setOperationAction(ISD::ROTR, MVT::i16, Expand);

  // Sign/zero extend
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1, Expand);

  // Branch operations
  setOperationAction(ISD::BR_CC, MVT::i8, Promote);
  setOperationAction(ISD::BR_CC, MVT::i16, Custom);
  setOperationAction(ISD::BRCOND, MVT::Other, Custom);

  // Select operations
  setOperationAction(ISD::SELECT, MVT::i8, Promote);
  setOperationAction(ISD::SELECT, MVT::i16, Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i8, Promote);
  setOperationAction(ISD::SELECT_CC, MVT::i16, Custom);

  // Global addresses
  setOperationAction(ISD::GlobalAddress, MVT::i16, Custom);

  // Set condition code
  setOperationAction(ISD::SETCC, MVT::i8, Promote);
  setOperationAction(ISD::SETCC, MVT::i16, Custom);

  // Count leading/trailing zeros
  setOperationAction(ISD::CTLZ, MVT::i16, Expand);
  setOperationAction(ISD::CTTZ, MVT::i16, Expand);
  setOperationAction(ISD::CTPOP, MVT::i16, Expand);

  // Byte swap
  setOperationAction(ISD::BSWAP, MVT::i16, Legal);

  // 8-bit load/store support
  setLoadExtAction(ISD::ZEXTLOAD, MVT::i16, MVT::i8, Custom);
  setLoadExtAction(ISD::SEXTLOAD, MVT::i16, MVT::i8, Custom);
  setLoadExtAction(ISD::EXTLOAD, MVT::i16, MVT::i8, Custom);
  setLoadExtAction(ISD::ZEXTLOAD, MVT::i16, MVT::i1, Custom);
  setLoadExtAction(ISD::SEXTLOAD, MVT::i16, MVT::i1, Custom);
  setLoadExtAction(ISD::EXTLOAD, MVT::i16, MVT::i1, Custom);
  setTruncStoreAction(MVT::i16, MVT::i8, Custom);
  setTruncStoreAction(MVT::i16, MVT::i1, Custom);

  // i8 operations
  if (STI.uses8BitAccumulator()) {
    setOperationAction(ISD::ADD, MVT::i8, Legal);
    setOperationAction(ISD::SUB, MVT::i8, Legal);
    setOperationAction(ISD::AND, MVT::i8, Legal);
    setOperationAction(ISD::OR, MVT::i8, Legal);
    setOperationAction(ISD::XOR, MVT::i8, Legal);
    setOperationAction(ISD::SHL, MVT::i8, Legal);
    setOperationAction(ISD::SRL, MVT::i8, Legal);
    setOperationAction(ISD::SRA, MVT::i8, Legal);
  } else {
    setOperationAction(ISD::ADD, MVT::i8, Promote);
    setOperationAction(ISD::SUB, MVT::i8, Promote);
    setOperationAction(ISD::AND, MVT::i8, Promote);
    setOperationAction(ISD::OR, MVT::i8, Promote);
    setOperationAction(ISD::XOR, MVT::i8, Promote);
    setOperationAction(ISD::SHL, MVT::i8, Promote);
    setOperationAction(ISD::SRL, MVT::i8, Promote);
    setOperationAction(ISD::SRA, MVT::i8, Promote);
  }

  // Saturating arithmetic
  setOperationAction(ISD::USUBSAT, MVT::i16, Expand);
  setOperationAction(ISD::SSUBSAT, MVT::i16, Expand);
  setOperationAction(ISD::UADDSAT, MVT::i16, Expand);
  setOperationAction(ISD::SADDSAT, MVT::i16, Expand);

  // Min/max
  setOperationAction(ISD::UMIN, MVT::i16, Expand);
  setOperationAction(ISD::UMAX, MVT::i16, Expand);
  setOperationAction(ISD::SMIN, MVT::i16, Expand);
  setOperationAction(ISD::SMAX, MVT::i16, Expand);

  setMinimumJumpTableEntries(INT_MAX);

  // Varargs
  setOperationAction(ISD::VASTART, MVT::Other, Custom);
  setOperationAction(ISD::VAARG, MVT::Other, Expand);
  setOperationAction(ISD::VAEND, MVT::Other, Expand);
  setOperationAction(ISD::VACOPY, MVT::Other, Expand);
}

//===----------------------------------------------------------------------===//
// W65816 condition codes (used by EmitInstrWithCustomInserter)
//===----------------------------------------------------------------------===//

namespace W65816CC {
enum CondCode {
  COND_EQ = 0,
  COND_NE,
  COND_CS,
  COND_CC,
  COND_MI,
  COND_PL,
  COND_VS,
  COND_VC,
  COND_SLT,
  COND_SGE,
  COND_SGT,
  COND_SLE,
  COND_UGT,
  COND_ULE,
};
}

static unsigned getBranchOpcodeForCond(W65816CC::CondCode CC) {
  switch (CC) {
  case W65816CC::COND_EQ:
    return W65816::BEQ;
  case W65816CC::COND_NE:
    return W65816::BNE;
  case W65816CC::COND_CS:
    return W65816::BCS;
  case W65816CC::COND_CC:
    return W65816::BCC;
  case W65816CC::COND_MI:
    return W65816::BMI;
  case W65816CC::COND_PL:
    return W65816::BPL;
  case W65816CC::COND_VS:
    return W65816::BVS;
  case W65816CC::COND_VC:
    return W65816::BVC;
  case W65816CC::COND_SLT:
  case W65816CC::COND_SGE:
  case W65816CC::COND_SGT:
  case W65816CC::COND_SLE:
    return W65816::BNE;
  case W65816CC::COND_UGT:
  case W65816CC::COND_ULE:
    return W65816::BNE;
  }
  llvm_unreachable("Unknown condition code");
}

MachineBasicBlock *W65816TargetLowering::EmitInstrWithCustomInserter(
    MachineInstr &MI, MachineBasicBlock *MBB) const {
  unsigned Opc = MI.getOpcode();

  if (Opc != W65816::Select16) {
    llvm_unreachable("Unexpected instruction for custom insertion");
  }

  const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  Register DstReg = MI.getOperand(0).getReg();
  Register TrueReg = MI.getOperand(1).getReg();
  Register FalseReg = MI.getOperand(2).getReg();
  W65816CC::CondCode CondCode = (W65816CC::CondCode)MI.getOperand(3).getImm();

  // Handle signed and unsigned compound conditions
  unsigned CompoundSelectOpc = 0;
  switch (CondCode) {
  case W65816CC::COND_SLT:
    CompoundSelectOpc = W65816::Select16_SLT;
    break;
  case W65816CC::COND_SGE:
    CompoundSelectOpc = W65816::Select16_SGE;
    break;
  case W65816CC::COND_SGT:
    CompoundSelectOpc = W65816::Select16_SGT;
    break;
  case W65816CC::COND_SLE:
    CompoundSelectOpc = W65816::Select16_SLE;
    break;
  case W65816CC::COND_UGT:
    CompoundSelectOpc = W65816::Select16_UGT;
    break;
  case W65816CC::COND_ULE:
    CompoundSelectOpc = W65816::Select16_ULE;
    break;
  default:
    break;
  }

  if (CompoundSelectOpc) {
    BuildMI(*MBB, MI, DL, TII.get(CompoundSelectOpc), DstReg)
        .addReg(TrueReg)
        .addReg(FalseReg);
    MI.eraseFromParent();
    return MBB;
  }

  // For unsigned/equality conditions, create the diamond pattern
  const unsigned SELECT_SCRATCH_DP = 0xFA;

  MachineFunction *MF = MBB->getParent();
  const BasicBlock *BB = MBB->getBasicBlock();
  MachineFunction::iterator It = ++MBB->getIterator();

  MachineBasicBlock *TrueMBB = MF->CreateMachineBasicBlock(BB);
  MachineBasicBlock *FalseMBB = MF->CreateMachineBasicBlock(BB);
  MachineBasicBlock *SinkMBB = MF->CreateMachineBasicBlock(BB);

  MF->insert(It, TrueMBB);
  MF->insert(It, FalseMBB);
  MF->insert(It, SinkMBB);

  SinkMBB->splice(SinkMBB->begin(), MBB,
                  std::next(MachineBasicBlock::iterator(MI)), MBB->end());
  SinkMBB->transferSuccessorsAndUpdatePHIs(MBB);

  MBB->addSuccessor(TrueMBB);
  MBB->addSuccessor(FalseMBB);

  unsigned BranchOpc = getBranchOpcodeForCond(CondCode);
  BuildMI(MBB, DL, TII.get(BranchOpc)).addMBB(TrueMBB);
  BuildMI(MBB, DL, TII.get(W65816::BRA)).addMBB(FalseMBB);

  TrueMBB->addSuccessor(SinkMBB);
  if (TrueReg == W65816::X) {
    BuildMI(TrueMBB, DL, TII.get(W65816::TXA));
  } else if (TrueReg == W65816::Y) {
    BuildMI(TrueMBB, DL, TII.get(W65816::TYA));
  } else if (TrueReg != W65816::A) {
    BuildMI(TrueMBB, DL, TII.get(TargetOpcode::COPY), W65816::A)
        .addReg(TrueReg);
  }
  BuildMI(TrueMBB, DL, TII.get(W65816::STA_dp))
      .addReg(W65816::A)
      .addImm(SELECT_SCRATCH_DP);
  BuildMI(TrueMBB, DL, TII.get(W65816::BRA)).addMBB(SinkMBB);

  FalseMBB->addSuccessor(SinkMBB);
  if (FalseReg == W65816::X) {
    BuildMI(FalseMBB, DL, TII.get(W65816::TXA));
  } else if (FalseReg == W65816::Y) {
    BuildMI(FalseMBB, DL, TII.get(W65816::TYA));
  } else if (FalseReg != W65816::A) {
    BuildMI(FalseMBB, DL, TII.get(TargetOpcode::COPY), W65816::A)
        .addReg(FalseReg);
  }
  BuildMI(FalseMBB, DL, TII.get(W65816::STA_dp))
      .addReg(W65816::A)
      .addImm(SELECT_SCRATCH_DP);

  auto InsertPt = SinkMBB->begin();
  BuildMI(*SinkMBB, InsertPt, DL, TII.get(W65816::LDA_dp), W65816::A)
      .addImm(SELECT_SCRATCH_DP);
  if (DstReg == W65816::X) {
    BuildMI(*SinkMBB, InsertPt, DL, TII.get(W65816::TAX));
  } else if (DstReg == W65816::Y) {
    BuildMI(*SinkMBB, InsertPt, DL, TII.get(W65816::TAY));
  } else if (DstReg != W65816::A) {
    BuildMI(*SinkMBB, InsertPt, DL, TII.get(TargetOpcode::COPY), DstReg)
        .addReg(W65816::A);
  }

  MI.eraseFromParent();
  return SinkMBB;
}

Register
W65816TargetLowering::getRegisterByName(const char *RegName, LLT VT,
                                        const MachineFunction &MF) const {
  Register Reg = StringSwitch<Register>(RegName)
                     .Case("a", W65816::A)
                     .Case("x", W65816::X)
                     .Case("y", W65816::Y)
                     .Case("sp", W65816::SP)
                     .Case("d", W65816::D)
                     .Default(Register());

  if (Reg)
    return Reg;

  report_fatal_error(
      Twine("invalid register name \"" + StringRef(RegName) + "\""));
}

std::pair<unsigned, const TargetRegisterClass *>
W65816TargetLowering::getRegForInlineAsmConstraint(
    const TargetRegisterInfo *TRI, StringRef Constraint, MVT VT) const {
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    case 'a':
      return std::make_pair(W65816::A, &W65816::ACC16RegClass);
    case 'x':
      return std::make_pair(W65816::X, &W65816::IDX16RegClass);
    case 'y':
      return std::make_pair(W65816::Y, &W65816::IDX16RegClass);
    case 'r':
      return std::make_pair(0U, &W65816::GPR16RegClass);
    default:
      break;
    }
  }

  return TargetLowering::getRegForInlineAsmConstraint(TRI, Constraint, VT);
}
