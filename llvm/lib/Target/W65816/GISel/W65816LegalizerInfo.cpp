//===-- W65816LegalizerInfo.cpp ----------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the targeting of the Machinelegalizer class for W65816.
//
//===----------------------------------------------------------------------===//

#include "W65816LegalizerInfo.h"
#include "../W65816MachineFunctionInfo.h"
#include "llvm/CodeGen/GlobalISel/LegalizerHelper.h"
#include "llvm/CodeGen/GlobalISel/LegalizerInfo.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

using namespace llvm;

W65816LegalizerInfo::W65816LegalizerInfo(const W65816Subtarget &ST) {
  using namespace TargetOpcode;
  const LLT s1 = LLT::scalar(1);
  const LLT s8 = LLT::scalar(8);
  const LLT s16 = LLT::scalar(16);
  const LLT p0 = LLT::pointer(0, 16);

  // Arithmetic operations - i16 is native, i8 widens to i16.
  getActionDefinitionsBuilder({G_ADD, G_SUB})
      .legalFor({s16})
      .widenScalarToNextPow2(0, 16)
      .clampScalar(0, s16, s16);

  // Logical operations.
  getActionDefinitionsBuilder({G_AND, G_OR, G_XOR})
      .legalFor({s16})
      .widenScalarToNextPow2(0, 16)
      .clampScalar(0, s16, s16);

  // Shifts.
  getActionDefinitionsBuilder({G_SHL, G_LSHR, G_ASHR})
      .legalFor({{s16, s16}})
      .widenScalarToNextPow2(0, 16)
      .clampScalar(0, s16, s16)
      .clampScalar(1, s16, s16);

  // Multiply, divide, remainder - mark as legal; they will be expanded
  // to runtime library calls by SelectionDAG fallback or custom lowering.
  getActionDefinitionsBuilder({G_MUL, G_SDIV, G_UDIV, G_SREM, G_UREM})
      .legalFor({s16})
      .widenScalarToNextPow2(0, 16)
      .clampScalar(0, s16, s16);

  // Constants.
  getActionDefinitionsBuilder(G_CONSTANT)
      .legalFor({s16, p0})
      .widenScalarToNextPow2(0, 16)
      .clampScalar(0, s16, s16);

  // Frame index and global values produce pointers.
  getActionDefinitionsBuilder({G_FRAME_INDEX, G_GLOBAL_VALUE}).legalFor({p0});

  // Loads and stores - i8 and i16, plus i1 (booleans stored as bytes).
  getActionDefinitionsBuilder({G_STORE, G_LOAD})
      .legalForTypesWithMemDesc({{s16, p0, s16, 2},
                                 {s8, p0, s8, 1},
                                 {s8, p0, s1, 1},
                                 {p0, p0, s16, 2}})
      .clampScalar(0, s8, s16);

  // Extending loads - decompose to G_LOAD + G_ZEXT/G_SEXT.
  // The W65816 has no native extending load instructions.
  getActionDefinitionsBuilder({G_SEXTLOAD, G_ZEXTLOAD})
      .customFor({{s16, p0}})
      .clampScalar(0, s16, s16)
      .lower();

  // Pointer arithmetic.
  getActionDefinitionsBuilder(G_PTR_ADD).legalFor({{p0, s16}});

  // Comparisons - result is s1, operands are s16.
  getActionDefinitionsBuilder(G_ICMP)
      .legalFor({{s1, s16}})
      .clampScalar(1, s16, s16);

  // Select - condition is s1, value is s16.
  getActionDefinitionsBuilder(G_SELECT)
      .legalFor({{s16, s1}})
      .clampScalar(0, s16, s16);

  // Branches - condition is s1.
  getActionDefinitionsBuilder(G_BRCOND).legalFor({s1});
  getActionDefinitionsBuilder(G_BR).legalIf(
      [](const LegalityQuery &) { return true; });

  // Extensions and truncations.
  getActionDefinitionsBuilder({G_SEXT, G_ZEXT, G_ANYEXT})
      .legalFor({{s16, s8}, {s16, s1}, {s8, s1}})
      .clampScalar(0, s8, s16);

  getActionDefinitionsBuilder(G_TRUNC)
      .legalFor({{s8, s16}, {s1, s16}, {s1, s8}})
      .clampScalar(0, s1, s8);

  // PHI nodes.
  getActionDefinitionsBuilder(G_PHI)
      .legalFor({s16, p0})
      .widenScalarToNextPow2(0, 16)
      .clampScalar(0, s16, s16);

  // Pointer conversions.
  getActionDefinitionsBuilder(G_INTTOPTR).legalFor({{p0, s16}});
  getActionDefinitionsBuilder(G_PTRTOINT).legalFor({{s16, p0}});

  // Byte swap (XBA instruction).
  getActionDefinitionsBuilder(G_BSWAP).legalFor({s16}).clampScalar(0, s16, s16);

  // Implicit definitions.
  getActionDefinitionsBuilder(G_IMPLICIT_DEF)
      .legalFor({s16, p0})
      .clampScalar(0, s16, s16);

  // Varargs support.
  getActionDefinitionsBuilder(G_VASTART).customFor({p0});

  // Memory intrinsics - lower to library calls.
  getActionDefinitionsBuilder({G_MEMCPY, G_MEMMOVE, G_MEMSET}).libcall();

  // Merge/Unmerge values (for type decomposition).
  getActionDefinitionsBuilder(G_MERGE_VALUES)
      .legalIf([](const LegalityQuery &) { return true; });
  getActionDefinitionsBuilder(G_UNMERGE_VALUES)
      .legalIf([](const LegalityQuery &) { return true; });

  getLegacyLegalizerInfo().computeTables();
}

bool W65816LegalizerInfo::legalizeCustom(
    LegalizerHelper &Helper, MachineInstr &MI,
    LostDebugLocObserver &LocObserver) const {
  MachineIRBuilder &MIRBuilder = Helper.MIRBuilder;
  switch (MI.getOpcode()) {
  case TargetOpcode::G_VASTART:
    return legalizeVAStart(MI, MIRBuilder);
  case TargetOpcode::G_ZEXTLOAD:
  case TargetOpcode::G_SEXTLOAD:
    return legalizeExtLoad(MI, MIRBuilder);
  default:
    return false;
  }
}

bool W65816LegalizerInfo::legalizeVAStart(MachineInstr &MI,
                                          MachineIRBuilder &MIRBuilder) const {
  // Store the address of the VarArgsFrameIndex to the va_list pointer.
  MachineFunction &MF = MIRBuilder.getMF();
  W65816MachineFunctionInfo *FuncInfo = MF.getInfo<W65816MachineFunctionInfo>();
  int FI = FuncInfo->getVarArgsFrameIndex();
  LLT AddrTy = MIRBuilder.getMRI()->getType(MI.getOperand(0).getReg());
  auto FINAddr = MIRBuilder.buildFrameIndex(AddrTy, FI);
  assert(MI.hasOneMemOperand());
  MIRBuilder.buildStore(FINAddr, MI.getOperand(0).getReg(),
                        *MI.memoperands()[0]);
  MI.eraseFromParent();
  return true;
}

bool W65816LegalizerInfo::legalizeExtLoad(MachineInstr &MI,
                                          MachineIRBuilder &MIRBuilder) const {
  // Decompose G_ZEXTLOAD/G_SEXTLOAD into G_LOAD + G_ZEXT/G_SEXT.
  // The W65816 has no native extending load instructions.
  Register DstReg = MI.getOperand(0).getReg();
  Register PtrReg = MI.getOperand(1).getReg();
  MachineMemOperand &MMO = **MI.memoperands_begin();
  LLT MemTy = MMO.getMemoryType();

  auto Load = MIRBuilder.buildLoad(MemTy, PtrReg, MMO);
  if (MI.getOpcode() == TargetOpcode::G_ZEXTLOAD)
    MIRBuilder.buildZExt(DstReg, Load);
  else
    MIRBuilder.buildSExt(DstReg, Load);

  MI.eraseFromParent();
  return true;
}
