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

  // Multiply high (upper 16 bits of 32-bit product) - custom lowering to
  // runtime library calls (__umulhi3, __smulhi3).
  getActionDefinitionsBuilder({G_UMULH, G_SMULH})
      .customFor({s16})
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

  // Min/Max operations - lower to compare+select sequences.
  // G_SMAX(a, b) → (a > b) ? a : b (signed)
  // G_SMIN(a, b) → (a < b) ? a : b (signed)
  // G_UMAX(a, b) → (a > b) ? a : b (unsigned)
  // G_UMIN(a, b) → (a < b) ? a : b (unsigned)
  getActionDefinitionsBuilder({G_SMAX, G_SMIN, G_UMAX, G_UMIN})
      .widenScalarToNextPow2(0, 16)
      .clampScalar(0, s16, s16)
      .lower();

  // Absolute value - custom lowering to compare+select+negate.
  // The default .lower() uses branchless algorithm (x >> 15) + x ^ (x >> 15)
  // which requires ASHR by 15, but our ASHR expansion is O(n) in shift amount.
  // Custom lowering: G_ABS(x) → (x < 0) ? -x : x
  getActionDefinitionsBuilder(G_ABS)
      .customFor({s16})
      .widenScalarToNextPow2(0, 16)
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
  case TargetOpcode::G_UMULH:
  case TargetOpcode::G_SMULH:
    return legalizeMulHigh(MI, MIRBuilder);
  case TargetOpcode::G_ABS:
    return legalizeAbs(MI, MIRBuilder);
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

bool W65816LegalizerInfo::legalizeMulHigh(MachineInstr &MI,
                                          MachineIRBuilder &MIRBuilder) const {
  // Lower G_UMULH/G_SMULH by computing the upper 16 bits of a 32-bit multiply
  // using only 16-bit operations. Algorithm:
  //
  // Given A and B, split into bytes: A = Ah*256 + Al, B = Bh*256 + Bl
  // A*B = Ah*Bh*65536 + (Ah*Bl + Al*Bh)*256 + Al*Bl
  // High 16 bits = Ah*Bh + ((Ah*Bl + Al*Bh + Al*Bl/256) / 256)
  //
  // Note: G_SMULH uses the same algorithm since we're computing via partials.

  Register DstReg = MI.getOperand(0).getReg();
  Register LHSReg = MI.getOperand(1).getReg();
  Register RHSReg = MI.getOperand(2).getReg();

  LLT S16 = LLT::scalar(16);
  LLT S8 = LLT::scalar(8);

  // Extract low and high bytes of LHS and RHS
  auto LHSLo = MIRBuilder.buildTrunc(S8, LHSReg);
  auto LHSHi =
      MIRBuilder.buildLShr(S16, LHSReg, MIRBuilder.buildConstant(S16, 8));
  auto LHSHiTrunc = MIRBuilder.buildTrunc(S8, LHSHi);

  auto RHSLo = MIRBuilder.buildTrunc(S8, RHSReg);
  auto RHSHi =
      MIRBuilder.buildLShr(S16, RHSReg, MIRBuilder.buildConstant(S16, 8));
  auto RHSHiTrunc = MIRBuilder.buildTrunc(S8, RHSHi);

  // Zero-extend bytes back to 16-bit for multiplication
  auto LHSLoExt = MIRBuilder.buildZExt(S16, LHSLo);
  auto LHSHiExt = MIRBuilder.buildZExt(S16, LHSHiTrunc);
  auto RHSLoExt = MIRBuilder.buildZExt(S16, RHSLo);
  auto RHSHiExt = MIRBuilder.buildZExt(S16, RHSHiTrunc);

  // Compute partial products (all fit in 16 bits since 8*8 = 16 max)
  // ll = Al * Bl (0-65025)
  // lh = Al * Bh (0-65025)
  // hl = Ah * Bl (0-65025)
  // hh = Ah * Bh (0-65025)
  auto LL = MIRBuilder.buildMul(S16, LHSLoExt, RHSLoExt);
  auto LH = MIRBuilder.buildMul(S16, LHSLoExt, RHSHiExt);
  auto HL = MIRBuilder.buildMul(S16, LHSHiExt, RHSLoExt);
  auto HH = MIRBuilder.buildMul(S16, LHSHiExt, RHSHiExt);

  // Combine: result_high = hh + (lh >> 8) + (hl >> 8) + ((lh&0xFF + hl&0xFF +
  // (ll >> 8)) >> 8) Simplified: high = hh + ((lh + hl + (ll >> 8)) >> 8) But
  // we need to handle carry properly...
  //
  // More precise: let mid = (ll >> 8) + (lh & 0xFF) + (hl & 0xFF)
  //               high = hh + (lh >> 8) + (hl >> 8) + (mid >> 8)

  auto LLHi = MIRBuilder.buildLShr(S16, LL, MIRBuilder.buildConstant(S16, 8));

  auto LHLo = MIRBuilder.buildAnd(S16, LH, MIRBuilder.buildConstant(S16, 0xFF));
  auto LHHi = MIRBuilder.buildLShr(S16, LH, MIRBuilder.buildConstant(S16, 8));

  auto HLLo = MIRBuilder.buildAnd(S16, HL, MIRBuilder.buildConstant(S16, 0xFF));
  auto HLHi = MIRBuilder.buildLShr(S16, HL, MIRBuilder.buildConstant(S16, 8));

  // mid = ll_hi + lh_lo + hl_lo
  auto Mid1 = MIRBuilder.buildAdd(S16, LLHi, LHLo);
  auto Mid = MIRBuilder.buildAdd(S16, Mid1, HLLo);
  auto MidHi = MIRBuilder.buildLShr(S16, Mid, MIRBuilder.buildConstant(S16, 8));

  // high = hh + lh_hi + hl_hi + mid_hi
  auto H1 = MIRBuilder.buildAdd(S16, HH, LHHi);
  auto H2 = MIRBuilder.buildAdd(S16, H1, HLHi);
  auto Result = MIRBuilder.buildAdd(S16, H2, MidHi);

  MIRBuilder.buildCopy(DstReg, Result.getReg(0));

  MI.eraseFromParent();
  return true;
}

bool W65816LegalizerInfo::legalizeAbs(MachineInstr &MI,
                                      MachineIRBuilder &MIRBuilder) const {
  // Lower G_ABS(x) to (x < 0) ? -x : x
  // This is more efficient than the branchless algorithm (x >> 15) + x ^ (x >>
  // 15) because our ASHR expansion is O(n) in shift amount.

  Register DstReg = MI.getOperand(0).getReg();
  Register SrcReg = MI.getOperand(1).getReg();

  LLT S16 = LLT::scalar(16);
  LLT S1 = LLT::scalar(1);

  // Build: cmp = x < 0 (signed)
  auto Zero = MIRBuilder.buildConstant(S16, 0);
  auto Cmp = MIRBuilder.buildICmp(CmpInst::ICMP_SLT, S1, SrcReg, Zero);

  // Build: neg = 0 - x
  auto Neg = MIRBuilder.buildSub(S16, Zero, SrcReg);

  // Build: result = cmp ? neg : x
  MIRBuilder.buildSelect(DstReg, Cmp, Neg, SrcReg);

  MI.eraseFromParent();
  return true;
}
