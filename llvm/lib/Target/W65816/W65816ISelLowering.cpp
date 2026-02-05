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
    : TargetLowering(TM, STI) {

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
