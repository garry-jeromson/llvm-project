//===-- W65816ISelLowering.cpp - W65816 DAG Lowering Implementation -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that W65816 uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#include "W65816ISelLowering.h"

#include "W65816.h"
#include "W65816InstrInfo.h"
#include "W65816MachineFunctionInfo.h"
#include "W65816Subtarget.h"
#include "W65816TargetMachine.h"
#include "MCTargetDesc/W65816MCTargetDesc.h"

#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
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
  // The W65816 can independently control accumulator width (M flag)
  // and index register width (X flag)

  if (STI.uses8BitAccumulator()) {
    // 8-bit accumulator mode (M=1)
    addRegisterClass(MVT::i8, &W65816::ACC8RegClass);
  } else {
    // 16-bit accumulator mode (M=0, default)
    addRegisterClass(MVT::i16, &W65816::ACC16RegClass);
  }

  if (STI.uses8BitIndex()) {
    // 8-bit index register mode (X=1)
    addRegisterClass(MVT::i8, &W65816::IDX8RegClass);
  } else {
    // 16-bit index register mode (X=0, default)
    addRegisterClass(MVT::i16, &W65816::IDX16RegClass);
  }

  // Always add GPR16 for 16-bit operations (needed for pointers at minimum)
  addRegisterClass(MVT::i16, &W65816::GPR16RegClass);
  // Note: GPR8 is NOT registered as a legal type in 16-bit mode
  // All i8 operations get promoted to i16 by the type legalizer

  // Compute derived properties from the register classes
  computeRegisterProperties(STI.getRegisterInfo());

  // Set the stack pointer register
  setStackPointerRegisterToSaveRestore(W65816::SP);

  // Set scheduling preference
  setSchedulingPreference(Sched::RegPressure);

  // Boolean values (i1) are represented as 0 or 1 in an i16 register
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrOneBooleanContent);

  // W65816 has limited support for certain operations
  // We need to expand or custom lower many of them

  // Division and remainder must be expanded (no hardware support)
  setOperationAction(ISD::SDIV, MVT::i16, Expand);
  setOperationAction(ISD::UDIV, MVT::i16, Expand);
  setOperationAction(ISD::SREM, MVT::i16, Expand);
  setOperationAction(ISD::UREM, MVT::i16, Expand);
  setOperationAction(ISD::SDIVREM, MVT::i16, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i16, Expand);

  // Multiplication must be expanded (no hardware support)
  setOperationAction(ISD::MUL, MVT::i16, Expand);
  setOperationAction(ISD::MULHS, MVT::i16, Expand);
  setOperationAction(ISD::MULHU, MVT::i16, Expand);
  setOperationAction(ISD::SMUL_LOHI, MVT::i16, Expand);
  setOperationAction(ISD::UMUL_LOHI, MVT::i16, Expand);

  // Shifts - W65816 only has shift by 1, but we have pseudo instructions
  // for shift-by-immediate that expand to multiple ASL/LSR instructions.
  // Variable shifts would need custom lowering with loops.
  setOperationAction(ISD::SHL, MVT::i16, Legal);
  setOperationAction(ISD::SRL, MVT::i16, Legal);
  setOperationAction(ISD::SRA, MVT::i16, Legal);
  setOperationAction(ISD::ROTL, MVT::i16, Expand);
  setOperationAction(ISD::ROTR, MVT::i16, Expand);

  // Sign/zero extend
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1, Expand);

  // Branch operations
  // i8 operands get promoted to i16, then our i16 Custom handler runs
  setOperationAction(ISD::BR_CC, MVT::i8, Promote);
  setOperationAction(ISD::BR_CC, MVT::i16, Custom);
  // BRCOND: br i1 %cond - branch on boolean value
  // We handle this by converting to our BRCOND node that checks if value is non-zero
  setOperationAction(ISD::BRCOND, MVT::Other, Custom);

  // Select operations
  // i8 operands get promoted to i16, then our i16 Custom handler runs
  setOperationAction(ISD::SELECT, MVT::i8, Promote);
  setOperationAction(ISD::SELECT, MVT::i16, Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i8, Promote);
  setOperationAction(ISD::SELECT_CC, MVT::i16, Custom);

  // Global addresses need custom lowering
  setOperationAction(ISD::GlobalAddress, MVT::i16, Custom);

  // Set condition code actions
  // i8 operands get promoted to i16, then our i16 Custom handler runs
  setOperationAction(ISD::SETCC, MVT::i8, Promote);
  setOperationAction(ISD::SETCC, MVT::i16, Custom);

  // Count leading/trailing zeros - expand (no hardware support)
  setOperationAction(ISD::CTLZ, MVT::i16, Expand);
  setOperationAction(ISD::CTTZ, MVT::i16, Expand);
  setOperationAction(ISD::CTPOP, MVT::i16, Expand);

  // Byte swap - use XBA instruction
  setOperationAction(ISD::BSWAP, MVT::i16, Legal);

  // Note: We don't use Custom for LOAD/STORE because returning SDValue()
  // for cases we don't handle doesn't work properly. Instead, we rely on
  // SelectAddr returning false for register addresses, which causes a
  // clean "Cannot select" error for unsupported pointer loads.

  // 8-bit load/store support: use extending loads and truncating stores
  // For loads: zero-extend and sign-extend 8-bit values to 16-bit
  setLoadExtAction(ISD::ZEXTLOAD, MVT::i16, MVT::i8, Custom);
  setLoadExtAction(ISD::SEXTLOAD, MVT::i16, MVT::i8, Custom);
  setLoadExtAction(ISD::EXTLOAD, MVT::i16, MVT::i8, Custom);

  // Boolean (i1) loads: treat as i8 loads since booleans are stored as bytes
  setLoadExtAction(ISD::ZEXTLOAD, MVT::i16, MVT::i1, Custom);
  setLoadExtAction(ISD::SEXTLOAD, MVT::i16, MVT::i1, Custom);
  setLoadExtAction(ISD::EXTLOAD, MVT::i16, MVT::i1, Custom);

  // For stores: truncate 16-bit values to 8-bit
  setTruncStoreAction(MVT::i16, MVT::i8, Custom);
  // Boolean stores: treat as i8 truncating stores
  setTruncStoreAction(MVT::i16, MVT::i1, Custom);

  // i8 operation actions depend on accumulator mode
  if (STI.uses8BitAccumulator()) {
    // In 8-bit accumulator mode, i8 operations are legal
    setOperationAction(ISD::ADD, MVT::i8, Legal);
    setOperationAction(ISD::SUB, MVT::i8, Legal);
    setOperationAction(ISD::AND, MVT::i8, Legal);
    setOperationAction(ISD::OR, MVT::i8, Legal);
    setOperationAction(ISD::XOR, MVT::i8, Legal);
    setOperationAction(ISD::SHL, MVT::i8, Legal);
    setOperationAction(ISD::SRL, MVT::i8, Legal);
    setOperationAction(ISD::SRA, MVT::i8, Legal);
  } else {
    // In 16-bit accumulator mode, promote i8 to i16
    setOperationAction(ISD::ADD, MVT::i8, Promote);
    setOperationAction(ISD::SUB, MVT::i8, Promote);
    setOperationAction(ISD::AND, MVT::i8, Promote);
    setOperationAction(ISD::OR, MVT::i8, Promote);
    setOperationAction(ISD::XOR, MVT::i8, Promote);
    setOperationAction(ISD::SHL, MVT::i8, Promote);
    setOperationAction(ISD::SRL, MVT::i8, Promote);
    setOperationAction(ISD::SRA, MVT::i8, Promote);
  }

  // Saturating arithmetic - expand to basic operations
  setOperationAction(ISD::USUBSAT, MVT::i16, Expand);
  setOperationAction(ISD::SSUBSAT, MVT::i16, Expand);
  setOperationAction(ISD::UADDSAT, MVT::i16, Expand);
  setOperationAction(ISD::SADDSAT, MVT::i16, Expand);

  // Min/max - expand to select + compare
  setOperationAction(ISD::UMIN, MVT::i16, Expand);
  setOperationAction(ISD::UMAX, MVT::i16, Expand);
  setOperationAction(ISD::SMIN, MVT::i16, Expand);
  setOperationAction(ISD::SMAX, MVT::i16, Expand);

  setMinimumJumpTableEntries(INT_MAX); // Don't use jump tables

  // Varargs support
  // VASTART needs custom lowering, others can be expanded
  setOperationAction(ISD::VASTART, MVT::Other, Custom);
  setOperationAction(ISD::VAARG, MVT::Other, Expand);
  setOperationAction(ISD::VAEND, MVT::Other, Expand);
  setOperationAction(ISD::VACOPY, MVT::Other, Expand);

  // Enable DAG combine for AND/OR/XOR to optimize 8-bit load + binop patterns
  // This avoids register pressure issues when both operands are 8-bit loads
  setTargetDAGCombine({ISD::AND, ISD::OR, ISD::XOR});
}

const char *W65816TargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch (Opcode) {
  case W65816ISD::RET_FLAG:
    return "W65816ISD::RET_FLAG";
  case W65816ISD::CALL:
    return "W65816ISD::CALL";
  case W65816ISD::FAR_CALL:
    return "W65816ISD::FAR_CALL";
  case W65816ISD::WRAPPER:
    return "W65816ISD::WRAPPER";
  case W65816ISD::CMP:
    return "W65816ISD::CMP";
  case W65816ISD::BRCOND:
    return "W65816ISD::BRCOND";
  case W65816ISD::SELECT_CC:
    return "W65816ISD::SELECT_CC";
  default:
    return nullptr;
  }
}

Sched::Preference W65816TargetLowering::getSchedulingPreference(SDNode *N) const {
  // For nodes that directly feed into CopyToReg (call arguments, return values),
  // use source order to reduce register pressure from conflicting live ranges.
  // This helps the W65816 which only has 3 physical registers (A, X, Y).
  if (N->hasOneUse()) {
    SDNode *User = *N->user_begin();
    if (User->getOpcode() == ISD::CopyToReg) {
      return Sched::Source;
    }
  }

  // For nodes producing multiple values where some feed into CopyToReg,
  // also prefer source order
  for (SDNode *User : N->users()) {
    if (User->getOpcode() == ISD::CopyToReg) {
      return Sched::Source;
    }
  }

  // Default: let the global setting (RegPressure) handle it
  return Sched::None;
}

SDValue W65816TargetLowering::LowerOperation(SDValue Op,
                                             SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  case ISD::GlobalAddress:
    return LowerGlobalAddress(Op, DAG);
  case ISD::SELECT_CC:
    return LowerSELECT_CC(Op, DAG);
  case ISD::SETCC:
    return LowerSETCC(Op, DAG);
  case ISD::BR_CC:
    return LowerBR_CC(Op, DAG);
  case ISD::BRCOND:
    return LowerBRCOND(Op, DAG);
  case ISD::LOAD:
    return LowerLoad(Op, DAG);
  case ISD::STORE:
    return LowerStore(Op, DAG);
  case ISD::VASTART:
    return LowerVASTART(Op, DAG);
  default:
    llvm_unreachable("Unexpected operation to lower");
  }
}

SDValue W65816TargetLowering::LowerGlobalAddress(SDValue Op,
                                                 SelectionDAG &DAG) const {
  SDLoc DL(Op);
  const GlobalAddressSDNode *GN = cast<GlobalAddressSDNode>(Op);
  const GlobalValue *GV = GN->getGlobal();
  int64_t Offset = GN->getOffset();

  SDValue Result = DAG.getTargetGlobalAddress(GV, DL, MVT::i16, Offset);
  return DAG.getNode(W65816ISD::WRAPPER, DL, MVT::i16, Result);
}

// W65816 condition codes (used internally before expanding to branch opcodes)
namespace W65816CC {
enum CondCode {
  COND_EQ = 0,  // Equal (Z=1)
  COND_NE,      // Not equal (Z=0)
  COND_CS,      // Carry set (C=1) - unsigned >=
  COND_CC,      // Carry clear (C=0) - unsigned <
  COND_MI,      // Minus (N=1)
  COND_PL,      // Plus (N=0)
  COND_VS,      // Overflow set (V=1)
  COND_VC,      // Overflow clear (V=0)
  // Signed comparisons (require multi-instruction sequences)
  COND_SLT,     // Signed less than (N != V)
  COND_SGE,     // Signed greater or equal (N == V)
  COND_SGT,     // Signed greater than (Z == 0 && N == V)
  COND_SLE,     // Signed less or equal (Z == 1 || N != V)
  // Unsigned compound comparisons (require multi-instruction sequences)
  COND_UGT,     // Unsigned greater than (C == 1 && Z == 0)
  COND_ULE,     // Unsigned less or equal (C == 0 || Z == 1)
};
}

// Map ISD condition codes to W65816 condition codes
static W65816CC::CondCode getCondCodeForISD(ISD::CondCode CC) {
  switch (CC) {
  // Equality comparisons
  case ISD::SETEQ:
  case ISD::SETUEQ:
    return W65816CC::COND_EQ;
  case ISD::SETNE:
  case ISD::SETUNE:
    return W65816CC::COND_NE;
  // Unsigned comparisons (use carry flag)
  case ISD::SETULT:
    return W65816CC::COND_CC;  // Carry clear (A < B unsigned)
  case ISD::SETUGE:
    return W65816CC::COND_CS;  // Carry set (A >= B unsigned)
  case ISD::SETUGT:
    // For A > B unsigned: need C=1 (A >= B) AND Z=0 (A != B)
    return W65816CC::COND_UGT;
  case ISD::SETULE:
    // For A <= B unsigned: C=0 (A < B) OR Z=1 (A == B)
    return W65816CC::COND_ULE;
  // Signed comparisons (require multi-instruction sequences)
  case ISD::SETLT:
    return W65816CC::COND_SLT;  // N != V
  case ISD::SETGE:
    return W65816CC::COND_SGE;  // N == V
  case ISD::SETGT:
    return W65816CC::COND_SGT;  // Z == 0 && N == V
  case ISD::SETLE:
    return W65816CC::COND_SLE;  // Z == 1 || N != V
  default:
    return W65816CC::COND_NE;  // Default fallback
  }
}

// Map W65816 condition code to branch opcode
// Note: Signed condition codes (COND_SLT, etc.) are not simple single-branch
// conditions and should be handled specially by the caller.
static unsigned getBranchOpcodeForCond(W65816CC::CondCode CC) {
  switch (CC) {
  case W65816CC::COND_EQ: return W65816::BEQ;
  case W65816CC::COND_NE: return W65816::BNE;
  case W65816CC::COND_CS: return W65816::BCS;
  case W65816CC::COND_CC: return W65816::BCC;
  case W65816CC::COND_MI: return W65816::BMI;
  case W65816CC::COND_PL: return W65816::BPL;
  case W65816CC::COND_VS: return W65816::BVS;
  case W65816CC::COND_VC: return W65816::BVC;
  // Signed conditions require multi-instruction sequences
  // For Select16 with signed conditions, we fall back to BNE as the first branch
  // (the full sequence is generated in the pseudo expansion)
  case W65816CC::COND_SLT:
  case W65816CC::COND_SGE:
  case W65816CC::COND_SGT:
  case W65816CC::COND_SLE:
    // Fallback - these should be handled specially by EmitInstrWithCustomInserter
    return W65816::BNE;
  // Unsigned compound conditions require multi-instruction sequences
  case W65816CC::COND_UGT:
  case W65816CC::COND_ULE:
    // Fallback - these should be handled specially by pseudo expansion
    return W65816::BNE;
  }
  llvm_unreachable("Unknown condition code");
}

// Helper to evaluate an integer comparison at compile time
static bool evaluateICmp(ISD::CondCode CC, int64_t LHS, int64_t RHS) {
  switch (CC) {
  case ISD::SETEQ:
  case ISD::SETUEQ:
    return LHS == RHS;
  case ISD::SETNE:
  case ISD::SETUNE:
    return LHS != RHS;
  case ISD::SETLT:
    return LHS < RHS;
  case ISD::SETLE:
    return LHS <= RHS;
  case ISD::SETGT:
    return LHS > RHS;
  case ISD::SETGE:
    return LHS >= RHS;
  case ISD::SETULT:
    return (uint64_t)LHS < (uint64_t)RHS;
  case ISD::SETULE:
    return (uint64_t)LHS <= (uint64_t)RHS;
  case ISD::SETUGT:
    return (uint64_t)LHS > (uint64_t)RHS;
  case ISD::SETUGE:
    return (uint64_t)LHS >= (uint64_t)RHS;
  default:
    llvm_unreachable("Unknown condition code");
  }
}

SDValue W65816TargetLowering::LowerSELECT_CC(SDValue Op,
                                             SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDValue TrueVal = Op.getOperand(2);
  SDValue FalseVal = Op.getOperand(3);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(4))->get();

  // Constant fold: if both comparison operands are constant, evaluate now
  // This avoids infinite loops in instruction selection when trying to
  // materialize constants into registers for comparisons like `icmp eq i16 1, 1`
  if (auto *CLHS = dyn_cast<ConstantSDNode>(LHS)) {
    if (auto *CRHS = dyn_cast<ConstantSDNode>(RHS)) {
      bool Result = evaluateICmp(CC, CLHS->getSExtValue(), CRHS->getSExtValue());
      return Result ? TrueVal : FalseVal;
    }
  }

  // Operands should already be i16 after type legalization

  // Create a comparison that sets the processor flags
  // CMP subtracts RHS from LHS and sets flags
  SDValue Cmp = DAG.getNode(W65816ISD::CMP, DL, MVT::Glue, LHS, RHS);

  // Get the W65816 condition code for this ISD condition
  W65816CC::CondCode W65CC = getCondCodeForISD(CC);

  // Create the SELECT_CC node with the condition code
  SDVTList VTs = DAG.getVTList(Op.getValueType(), MVT::Glue);
  SDValue Ops[] = {TrueVal, FalseVal, DAG.getConstant(W65CC, DL, MVT::i16), Cmp};

  return DAG.getNode(W65816ISD::SELECT_CC, DL, VTs, Ops);
}

SDValue W65816TargetLowering::LowerSETCC(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(2))->get();
  EVT ResultVT = Op.getValueType();

  // Constant fold: if both comparison operands are constant, evaluate now
  if (auto *CLHS = dyn_cast<ConstantSDNode>(LHS)) {
    if (auto *CRHS = dyn_cast<ConstantSDNode>(RHS)) {
      bool Result = evaluateICmp(CC, CLHS->getSExtValue(), CRHS->getSExtValue());
      return DAG.getConstant(Result ? 1 : 0, DL, ResultVT);
    }
  }

  // Operands should already be i16 after type legalization
  // (i8 operands get promoted to i16 automatically)

  // Create a comparison node that sets flags
  SDValue Cmp = DAG.getNode(W65816ISD::CMP, DL, MVT::Glue, LHS, RHS);

  // Get the W65816 condition code for this ISD condition
  W65816CC::CondCode W65CC = getCondCodeForISD(CC);

  // SETCC returns 1 if true, 0 if false
  // We implement this as SELECT_CC with constants 1 and 0
  SDValue TrueVal = DAG.getConstant(1, DL, MVT::i16);
  SDValue FalseVal = DAG.getConstant(0, DL, MVT::i16);

  SDVTList VTs = DAG.getVTList(MVT::i16, MVT::Glue);
  SDValue Ops[] = {TrueVal, FalseVal, DAG.getConstant(W65CC, DL, MVT::i16), Cmp};

  SDValue Result = DAG.getNode(W65816ISD::SELECT_CC, DL, VTs, Ops);

  // Truncate result to the expected type if needed (e.g., i8 or i1)
  if (ResultVT != MVT::i16) {
    Result = DAG.getNode(ISD::TRUNCATE, DL, ResultVT, Result);
  }

  return Result;
}

SDValue W65816TargetLowering::LowerBR_CC(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Chain = Op.getOperand(0);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue Dest = Op.getOperand(4);

  // Operands should already be i16 after type legalization

  // Create a comparison node that sets flags
  SDValue Cmp = DAG.getNode(W65816ISD::CMP, DL, MVT::Glue, LHS, RHS);

  // Get the W65816 condition code
  W65816CC::CondCode W65CC = getCondCodeForISD(CC);

  // Create conditional branch: Chain, DestBB, CondCode, Flags
  return DAG.getNode(W65816ISD::BRCOND, DL, MVT::Other, Chain, Dest,
                     DAG.getConstant(W65CC, DL, MVT::i16), Cmp);
}

SDValue W65816TargetLowering::LowerBRCOND(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Chain = Op.getOperand(0);
  SDValue Cond = Op.getOperand(1);
  SDValue Dest = Op.getOperand(2);

  // BRCOND: br i1 %cond, label %dest
  // The condition is a boolean value (0 or 1 in i16)
  // We convert this to: compare Cond with 0, branch if not equal

  // Compare with zero
  SDValue Zero = DAG.getConstant(0, DL, MVT::i16);
  SDValue Cmp = DAG.getNode(W65816ISD::CMP, DL, MVT::Glue, Cond, Zero);

  // Branch if not equal (condition was true/non-zero)
  return DAG.getNode(W65816ISD::BRCOND, DL, MVT::Other, Chain, Dest,
                     DAG.getConstant(W65816CC::COND_NE, DL, MVT::i16), Cmp);
}

SDValue W65816TargetLowering::LowerLoad(SDValue Op, SelectionDAG &DAG) const {
  LoadSDNode *LD = cast<LoadSDNode>(Op);
  SDLoc DL(Op);
  SDValue Chain = LD->getChain();
  SDValue Addr = LD->getBasePtr();
  EVT MemVT = LD->getMemoryVT();
  EVT VT = Op.getValueType();

  // Handle boolean (i1) extending loads - treat as i8 loads
  // Booleans are stored as bytes (0 or 1)
  if (MemVT == MVT::i1 && VT == MVT::i16) {
    // Load as i8 and zero-extend (booleans are always 0 or 1)
    // Create an i8 extending load instead
    SDValue NewLoad = DAG.getExtLoad(
        ISD::ZEXTLOAD, DL, MVT::i16, Chain, Addr,
        LD->getPointerInfo(), MVT::i8, LD->getAlign(),
        LD->getMemOperand()->getFlags());
    // The result is already zero-extended to i16
    // AND with 1 to ensure it's a valid boolean (0 or 1)
    SDValue One = DAG.getConstant(1, DL, MVT::i16);
    SDValue Masked = DAG.getNode(ISD::AND, DL, MVT::i16, NewLoad, One);
    return DAG.getMergeValues({Masked, NewLoad.getValue(1)}, DL);
  }

  // Handle 8-bit extending loads (zext/sext from i8 to i16)
  if (MemVT == MVT::i8 && VT == MVT::i16) {
    // The W65816 needs mode switching for 8-bit operations
    // For now, we'll use a pseudo instruction that expands to:
    //   SEP #$20    ; switch to 8-bit accumulator
    //   LDA addr    ; load 8-bit value
    //   REP #$20    ; switch back to 16-bit accumulator
    //   AND #$00FF  ; mask to 8 bits (for zext) or sign extend
    //
    // For simplicity, we'll expand this in ISelDAGToDAG by creating
    // a custom node. For now, return SDValue() to let default handling
    // try, which will likely fail but at least lets us test the setup.
    //
    // Actually, let's try a different approach: load 16 bits and mask
    // This avoids mode switching but may read an extra byte
    // For memory-mapped I/O this could be problematic, but for general
    // RAM it's fine.
    //
    // For now, we'll just let the DAG selection handle it with patterns
    return SDValue();
  }

  // Only handle i16 loads for pointer indirection
  if (MemVT != MVT::i16)
    return SDValue(); // Let default handling take over

  // Check if this is loading from a known address type that normal
  // selection can handle (frame index, global, wrapper)
  if (isa<FrameIndexSDNode>(Addr))
    return SDValue(); // Normal selection handles this

  if (Addr.getOpcode() == W65816ISD::WRAPPER)
    return SDValue(); // Normal selection handles this

  if (Addr.getOpcode() == ISD::ADD) {
    SDValue LHS = Addr.getOperand(0);
    SDValue RHS = Addr.getOperand(1);
    // Check for indexed global access which normal selection handles
    if (LHS.getOpcode() == W65816ISD::WRAPPER ||
        RHS.getOpcode() == W65816ISD::WRAPPER)
      return SDValue();
  }

  // For loading through a pointer in a register, we need to:
  // 1. Store the pointer to a stack slot
  // 2. Use stack-relative indirect addressing LDA (offset,S),Y
  //
  // Create a new stack object to hold the pointer
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  int FI = MFI.CreateStackObject(2, Align(2), false);
  SDValue StackSlot = DAG.getFrameIndex(FI, MVT::i16);

  // Store the pointer address to the stack slot
  SDValue Store = DAG.getStore(Chain, DL, Addr, StackSlot,
                               MachinePointerInfo::getFixedStack(MF, FI));

  // Now create a load using the indirect addressing
  // For simple dereference (*ptr), the index is 0
  // For ptr[i], the index would be i*2 (byte offset)
  //
  // For now, we only handle simple dereference with index 0
  // TODO: Handle indexed access by checking if original Addr was (add ptr, idx)

  // We need to emit the indirect load using our pseudo instruction
  // Since we're in DAG lowering, we can't easily emit the pseudo directly.
  // Instead, we'll create a load from the stack slot (the pointer value)
  // and then dereference it.
  //
  // The challenge is that we need TWO loads:
  // 1. Load the pointer from the stack slot (to get the address)
  // 2. Load from that address (the actual dereference)
  //
  // But load #1 puts the pointer in a register, and we can't do load #2
  // from a register directly - that's our original problem!
  //
  // The solution is to use LDA (offset,S),Y which does both in one instruction:
  // - Reads the 16-bit pointer from (SP + offset)
  // - Adds Y to that pointer
  // - Loads from the resulting address
  //
  // But we can't easily express this in the DAG. Instead, we'll
  // emit a custom node that will be selected to use this instruction.

  // For now, just emit an error for unsupported pointer loads
  // This case requires stack-relative indirect which we've defined
  // but haven't fully integrated into selection yet.
  //
  // As a workaround, we can emit the sequence:
  // 1. Store ptr to stack
  // 2. LDY #0
  // 3. LDA (offset,S),Y
  //
  // Create the load using the frame index - this will later be
  // selected as a stack-relative indirect load

  // Actually, let's return SDValue() and let it fail cleanly,
  // then we can add special handling in ISelDAGToDAG for this pattern
  return SDValue();
}

SDValue W65816TargetLowering::LowerStore(SDValue Op, SelectionDAG &DAG) const {
  StoreSDNode *ST = cast<StoreSDNode>(Op);
  SDLoc DL(Op);
  SDValue Chain = ST->getChain();
  SDValue Value = ST->getValue();
  SDValue Addr = ST->getBasePtr();
  EVT MemVT = ST->getMemoryVT();
  EVT ValVT = Value.getValueType();

  // Handle boolean (i1) truncating stores - treat as i8 stores
  if (MemVT == MVT::i1 && ValVT == MVT::i16) {
    // Store as i8 (booleans are stored as 0 or 1 byte)
    // AND with 1 to ensure we only store 0 or 1
    SDValue One = DAG.getConstant(1, DL, MVT::i16);
    SDValue Masked = DAG.getNode(ISD::AND, DL, MVT::i16, Value, One);
    // Create an i8 truncating store
    return DAG.getTruncStore(Chain, DL, Masked, Addr, ST->getPointerInfo(),
                             MVT::i8, ST->getAlign(),
                             ST->getMemOperand()->getFlags());
  }

  // Handle truncating stores (i16 -> i8)
  if (MemVT == MVT::i8 && ValVT == MVT::i16) {
    // For truncating stores, we need to:
    //   SEP #$20    ; switch to 8-bit accumulator
    //   STA addr    ; store 8-bit value (only low byte)
    //   REP #$20    ; switch back to 16-bit accumulator
    //
    // For now, let the DAG selection handle it with patterns
    return SDValue();
  }

  // Only handle i16 stores for pointer indirection
  if (MemVT != MVT::i16)
    return SDValue(); // Let default handling take over

  // Check if this is storing to a known address type
  if (isa<FrameIndexSDNode>(Addr))
    return SDValue(); // Normal selection handles this

  if (Addr.getOpcode() == W65816ISD::WRAPPER)
    return SDValue(); // Normal selection handles this

  if (Addr.getOpcode() == ISD::ADD) {
    SDValue LHS = Addr.getOperand(0);
    SDValue RHS = Addr.getOperand(1);
    if (LHS.getOpcode() == W65816ISD::WRAPPER ||
        RHS.getOpcode() == W65816ISD::WRAPPER)
      return SDValue();
  }

  // For storing through a pointer, same issue as LowerLoad
  // Return SDValue() to let it fail, we'll add better handling later
  return SDValue();
}

MachineBasicBlock *
W65816TargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                                  MachineBasicBlock *MBB) const {
  unsigned Opc = MI.getOpcode();

  if (Opc != W65816::Select16) {
    llvm_unreachable("Unexpected instruction for custom insertion");
  }

  // Expand Select16 to a diamond control-flow pattern
  const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  // Get operands first to check condition type
  Register DstReg = MI.getOperand(0).getReg();
  Register TrueReg = MI.getOperand(1).getReg();
  Register FalseReg = MI.getOperand(2).getReg();
  W65816CC::CondCode CondCode = (W65816CC::CondCode)MI.getOperand(3).getImm();

  // Handle signed and unsigned compound conditions by emitting select pseudos
  // These pseudos are expanded later (in ExpandPseudo pass) to avoid
  // the branch folder breaking the multi-instruction sequences
  unsigned CompoundSelectOpc = 0;
  switch (CondCode) {
  case W65816CC::COND_SLT: CompoundSelectOpc = W65816::Select16_SLT; break;
  case W65816CC::COND_SGE: CompoundSelectOpc = W65816::Select16_SGE; break;
  case W65816CC::COND_SGT: CompoundSelectOpc = W65816::Select16_SGT; break;
  case W65816CC::COND_SLE: CompoundSelectOpc = W65816::Select16_SLE; break;
  case W65816CC::COND_UGT: CompoundSelectOpc = W65816::Select16_UGT; break;
  case W65816CC::COND_ULE: CompoundSelectOpc = W65816::Select16_ULE; break;
  default: break;
  }

  if (CompoundSelectOpc) {
    // For compound conditions, emit a pseudo that will be expanded later
    // Don't create the diamond pattern here - let ExpandPseudo handle it
    BuildMI(*MBB, MI, DL, TII.get(CompoundSelectOpc), DstReg)
        .addReg(TrueReg)
        .addReg(FalseReg);
    MI.eraseFromParent();
    return MBB;
  }

  // For unsigned/equality conditions, create the diamond pattern here
  // Using PHI-free approach: store values to scratch DP, load in sink block
  //
  // This avoids PHI nodes that can conflict with existing PHIs in complex
  // control flow, causing "Node emitted out of order" crashes.
  //
  // Pattern:
  //   MBB:
  //     cmp ...        (already done before Select16)
  //     bCC trueMBB
  //     bra falseMBB
  //   trueMBB:
  //     sta $FA        (store trueVal to scratch)
  //     bra sinkMBB
  //   falseMBB:
  //     sta $FA        (store falseVal to scratch)
  //     ; fall through to sinkMBB
  //   sinkMBB:
  //     lda $FA        (load result from scratch)

  // Scratch DP address for select operations ($FA-$FB)
  // This is separate from spill/reload ($FC-$FD) and ALU ($FE-$FF)
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

  // Transfer the remainder of MBB and its successors to SinkMBB
  SinkMBB->splice(SinkMBB->begin(), MBB,
                  std::next(MachineBasicBlock::iterator(MI)), MBB->end());
  SinkMBB->transferSuccessorsAndUpdatePHIs(MBB);

  // Add edges: MBB -> TrueMBB, MBB -> FalseMBB
  MBB->addSuccessor(TrueMBB);
  MBB->addSuccessor(FalseMBB);

  // Emit conditional branch to TrueMBB, unconditional to FalseMBB
  unsigned BranchOpc = getBranchOpcodeForCond(CondCode);
  BuildMI(MBB, DL, TII.get(BranchOpc)).addMBB(TrueMBB);
  BuildMI(MBB, DL, TII.get(W65816::BRA)).addMBB(FalseMBB);

  // TrueMBB: Move TrueReg to A, store to scratch, branch to SinkMBB
  TrueMBB->addSuccessor(SinkMBB);
  if (TrueReg == W65816::X) {
    BuildMI(TrueMBB, DL, TII.get(W65816::TXA));
  } else if (TrueReg == W65816::Y) {
    BuildMI(TrueMBB, DL, TII.get(W65816::TYA));
  } else if (TrueReg != W65816::A) {
    // Virtual register or imaginary register - need to load to A first
    BuildMI(TrueMBB, DL, TII.get(TargetOpcode::COPY), W65816::A).addReg(TrueReg);
  }
  BuildMI(TrueMBB, DL, TII.get(W65816::STA_dp)).addReg(W65816::A).addImm(SELECT_SCRATCH_DP);
  BuildMI(TrueMBB, DL, TII.get(W65816::BRA)).addMBB(SinkMBB);

  // FalseMBB: Move FalseReg to A, store to scratch, fall through to SinkMBB
  FalseMBB->addSuccessor(SinkMBB);
  if (FalseReg == W65816::X) {
    BuildMI(FalseMBB, DL, TII.get(W65816::TXA));
  } else if (FalseReg == W65816::Y) {
    BuildMI(FalseMBB, DL, TII.get(W65816::TYA));
  } else if (FalseReg != W65816::A) {
    BuildMI(FalseMBB, DL, TII.get(TargetOpcode::COPY), W65816::A).addReg(FalseReg);
  }
  BuildMI(FalseMBB, DL, TII.get(W65816::STA_dp)).addReg(W65816::A).addImm(SELECT_SCRATCH_DP);
  // No branch needed - falls through to SinkMBB

  // SinkMBB: Load result from scratch to A, then move to DstReg
  // Insert at the beginning of SinkMBB (before any spliced instructions)
  auto InsertPt = SinkMBB->begin();
  BuildMI(*SinkMBB, InsertPt, DL, TII.get(W65816::LDA_dp), W65816::A).addImm(SELECT_SCRATCH_DP);
  if (DstReg == W65816::X) {
    BuildMI(*SinkMBB, InsertPt, DL, TII.get(W65816::TAX));
  } else if (DstReg == W65816::Y) {
    BuildMI(*SinkMBB, InsertPt, DL, TII.get(W65816::TAY));
  } else if (DstReg != W65816::A) {
    BuildMI(*SinkMBB, InsertPt, DL, TII.get(TargetOpcode::COPY), DstReg).addReg(W65816::A);
  }

  MI.eraseFromParent();
  return SinkMBB;
}

Register W65816TargetLowering::getRegisterByName(const char *RegName, LLT VT,
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

  report_fatal_error(Twine("Invalid register name \"" + StringRef(RegName) + "\"."));
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

//===----------------------------------------------------------------------===//
// Calling Convention Implementation
//===----------------------------------------------------------------------===//

SDValue W65816TargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {

  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();

  // Analyze the arguments
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_W65816);

  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];

    if (VA.isRegLoc()) {
      // Argument passed in register
      EVT RegVT = VA.getLocVT();
      const TargetRegisterClass *RC;

      if (RegVT == MVT::i8) {
        RC = &W65816::GPR8RegClass;
      } else if (RegVT == MVT::i16) {
        RC = &W65816::GPR16RegClass;
      } else {
        llvm_unreachable("Unexpected argument type");
      }

      Register VReg = RegInfo.createVirtualRegister(RC);
      RegInfo.addLiveIn(VA.getLocReg(), VReg);
      SDValue ArgValue = DAG.getCopyFromReg(Chain, DL, VReg, RegVT);

      if (VA.getLocInfo() == CCValAssign::SExt)
        ArgValue = DAG.getNode(ISD::AssertSext, DL, RegVT, ArgValue,
                               DAG.getValueType(VA.getValVT()));
      else if (VA.getLocInfo() == CCValAssign::ZExt)
        ArgValue = DAG.getNode(ISD::AssertZext, DL, RegVT, ArgValue,
                               DAG.getValueType(VA.getValVT()));

      if (VA.getLocInfo() != CCValAssign::Full)
        ArgValue = DAG.getNode(ISD::TRUNCATE, DL, VA.getValVT(), ArgValue);

      InVals.push_back(ArgValue);
    } else {
      // Argument passed on stack
      assert(VA.isMemLoc());

      EVT ValVT = VA.getValVT();
      int FI = MFI.CreateFixedObject(ValVT.getSizeInBits() / 8,
                                     VA.getLocMemOffset(), true);
      SDValue FIN = DAG.getFrameIndex(FI, MVT::i16);
      SDValue Load =
          DAG.getLoad(ValVT, DL, Chain, FIN, MachinePointerInfo::getFixedStack(MF, FI));
      InVals.push_back(Load);
    }
  }

  // For vararg functions, record the frame index where varargs start
  if (IsVarArg) {
    W65816MachineFunctionInfo *FuncInfo = MF.getInfo<W65816MachineFunctionInfo>();
    // Varargs start after the last fixed argument on the stack
    unsigned Offset = CCInfo.getStackSize();
    int FI = MFI.CreateFixedObject(2, Offset, true);
    FuncInfo->setVarArgsFrameIndex(FI);
  }

  return Chain;
}

SDValue W65816TargetLowering::LowerReturn(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::OutputArg> &Outs,
    const SmallVectorImpl<SDValue> &OutVals, const SDLoc &DL,
    SelectionDAG &DAG) const {

  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeReturn(Outs, RetCC_W65816);

  SDValue Glue;
  SmallVector<SDValue, 4> RetOps;
  RetOps.push_back(Chain);

  // Copy return values to the physical return registers
  for (unsigned i = 0, e = RVLocs.size(); i != e; ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), OutVals[i], Glue);
    Glue = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  RetOps[0] = Chain;

  if (Glue.getNode())
    RetOps.push_back(Glue);

  return DAG.getNode(W65816ISD::RET_FLAG, DL, MVT::Other, RetOps);
}

SDValue W65816TargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                                        SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG = CLI.DAG;
  SDLoc &DL = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  CallingConv::ID CallConv = CLI.CallConv;
  bool IsVarArg = CLI.IsVarArg;

  // W65816 does not support tail call optimization
  CLI.IsTailCall = false;

  MachineFunction &MF = DAG.getMachineFunction();

  // Analyze operands of the call
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeCallOperands(Outs, CC_W65816);

  // Get a count of how many bytes are to be pushed on the stack
  unsigned NumBytes = CCInfo.getStackSize();

  // CALLSEQ_START allocates space for outgoing stack arguments
  Chain = DAG.getCALLSEQ_START(Chain, NumBytes, 0, DL);

  SDValue Glue;
  SmallVector<std::pair<unsigned, SDValue>, 4> RegsToPass;
  SmallVector<SDValue, 8> MemOpChains;

  // Walk the register/memloc assignments, inserting copies/loads
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    SDValue Arg = OutVals[i];

    if (VA.isRegLoc()) {
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
    } else {
      // Stack argument - store to the outgoing argument area
      assert(VA.isMemLoc() && "Expected memory location for stack arg");

      // Create a frame index for this argument slot.
      // The offset is relative to the outgoing argument area, which is
      // allocated by CALLSEQ_START. Use positive offset since these are
      // at the top of the outgoing area (lowest addresses, closest to SP).
      int FI = MF.getFrameInfo().CreateFixedObject(
          VA.getLocVT().getSizeInBits() / 8,
          VA.getLocMemOffset(),
          /*IsImmutable=*/false,
          /*isAliased=*/false);
      SDValue FIN = DAG.getFrameIndex(FI, MVT::i16);

      // Store the argument to the stack slot
      SDValue Store = DAG.getStore(Chain, DL, Arg, FIN,
                                   MachinePointerInfo::getFixedStack(MF, FI));
      MemOpChains.push_back(Store);
    }
  }

  // Chain all memory operations together
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, MemOpChains);

  // Copy arguments to registers in reverse order (Y, X, A).
  // This is critical because copying to X or Y requires going through A
  // (TAX/TAY), which would clobber A if we set A first. By copying to
  // Y first, then X, then A last, we ensure A has the correct value
  // at call time.
  for (auto it = RegsToPass.rbegin(); it != RegsToPass.rend(); ++it) {
    Chain = DAG.getCopyToReg(Chain, DL, it->first, it->second, Glue);
    Glue = Chain.getValue(1);
  }

  // Check if this is a far call (callee has w65816_farfunc attribute)
  bool IsFarCall = false;
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    if (const Function *F = dyn_cast<Function>(G->getGlobal())) {
      IsFarCall = F->hasFnAttribute("w65816_farfunc");
    }
  }

  // Get the target global address (always i16, JSL handles 24-bit encoding)
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee))
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), DL, MVT::i16);
  else if (ExternalSymbolSDNode *E = dyn_cast<ExternalSymbolSDNode>(Callee))
    Callee = DAG.getTargetExternalSymbol(E->getSymbol(), MVT::i16);

  // Build the call node
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  // Add argument registers to the call node
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    if (VA.isRegLoc())
      Ops.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  // Note: Register mask operand is not needed since JSR/JSL have
  // Defs = [SP, A, X, Y, P] which defines caller-saved registers

  if (Glue.getNode())
    Ops.push_back(Glue);

  // Use FAR_CALL for far functions (JSL), CALL for near functions (JSR)
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  unsigned CallOpc = IsFarCall ? W65816ISD::FAR_CALL : W65816ISD::CALL;
  Chain = DAG.getNode(CallOpc, DL, NodeTys, Ops);
  Glue = Chain.getValue(1);

  // Handle return values
  SmallVector<CCValAssign, 16> RVLocs;
  CCState RVInfo(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());
  RVInfo.AnalyzeCallResult(Ins, RetCC_W65816);

  for (unsigned i = 0, e = RVLocs.size(); i != e; ++i) {
    CCValAssign &VA = RVLocs[i];
    SDValue Val = DAG.getCopyFromReg(Chain, DL, VA.getLocReg(), VA.getLocVT(), Glue);
    Chain = Val.getValue(1);
    Glue = Val.getValue(2);
    InVals.push_back(Val);
  }

  // Clean up the stack after the call
  Chain = DAG.getCALLSEQ_END(Chain, NumBytes, 0, Glue, DL);

  return Chain;
}

bool W65816TargetLowering::CanLowerReturn(
    CallingConv::ID CallConv, MachineFunction &MF, bool IsVarArg,
    const SmallVectorImpl<ISD::OutputArg> &Outs, LLVMContext &Context,
    const Type *RetTy) const {
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, RVLocs, Context);
  return CCInfo.CheckReturn(Outs, RetCC_W65816);
}

SDValue W65816TargetLowering::LowerVASTART(SDValue Op,
                                            SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();
  W65816MachineFunctionInfo *FuncInfo = MF.getInfo<W65816MachineFunctionInfo>();

  SDLoc DL(Op);
  SDValue Chain = Op.getOperand(0);
  SDValue Ptr = Op.getOperand(1);
  EVT PtrVT = Ptr.getValueType();

  // Get the frame index of the first vararg argument
  SDValue FrameIndex = DAG.getFrameIndex(FuncInfo->getVarArgsFrameIndex(), PtrVT);

  // Get the source value for the store
  const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();

  // Store the frame index to the va_list pointer
  // This is the address where the first vararg is located
  return DAG.getStore(Chain, DL, FrameIndex, Ptr, MachinePointerInfo(SV));
}

SDValue W65816TargetLowering::LowerSIGN_EXTEND(SDValue Op,
                                                SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Val = Op.getOperand(0);
  EVT SrcVT = Val.getValueType();
  EVT DstVT = Op.getValueType();

  // Only handle i8 to i16 sign extension
  if (SrcVT != MVT::i8 || DstVT != MVT::i16) {
    // For other cases, let the legalizer handle it
    return SDValue();
  }

  // Sign extend i8 to i16 using: ((x & 0xFF) << 8) >> 8 (arithmetic shift)
  // First zero-extend, then shift-based sign extension
  SDValue ZExt = LowerZERO_EXTEND(
      DAG.getNode(ISD::ZERO_EXTEND, DL, MVT::i16, Val), DAG);
  // Shift left by 8 to put the i8 value in the high byte
  SDValue ShiftAmt = DAG.getConstant(8, DL, MVT::i16);
  SDValue Shifted = DAG.getNode(ISD::SHL, DL, MVT::i16, ZExt, ShiftAmt);
  // Arithmetic shift right by 8 to sign-extend
  SDValue Result = DAG.getNode(ISD::SRA, DL, MVT::i16, Shifted, ShiftAmt);
  return Result;
}

SDValue W65816TargetLowering::LowerZERO_EXTEND(SDValue Op,
                                                SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Val = Op.getOperand(0);
  EVT SrcVT = Val.getValueType();
  EVT DstVT = Op.getValueType();

  // Only handle i8 to i16 zero extension
  if (SrcVT != MVT::i8 || DstVT != MVT::i16) {
    // For other cases, let the legalizer handle it
    return SDValue();
  }

  // Zero extend i8 to i16: the i8 value is in the low byte of a 16-bit register
  // Just AND with 0xFF to clear the high byte
  // The register allocator will coalesce the 8-bit and 16-bit registers
  return DAG.getNode(ISD::AND, DL, MVT::i16,
                      DAG.getNode(ISD::BITCAST, DL, MVT::i16, Val),
                      DAG.getConstant(0xFF, DL, MVT::i16));
}

// Helper to promote i8 comparison operands to i16
// Returns true if promotion was needed, false if operands are already i16
static bool promoteCompareOperands(SelectionDAG &DAG,
                                   const SDLoc &DL,
                                   SDValue LHS,
                                   SDValue RHS,
                                   ISD::CondCode CC,
                                   SDValue &PromotedLHS,
                                   SDValue &PromotedRHS) {
  // If operands are already i16, no promotion needed
  if (LHS.getValueType() == MVT::i16) {
    PromotedLHS = LHS;
    PromotedRHS = RHS;
    return false;
  }

  // Use sign-extend for signed comparisons, zero-extend for unsigned
  bool IsSigned = ISD::isSignedIntSetCC(CC);

  if (IsSigned) {
    // Sign extend i8 to i16: shift left 8, then arithmetic shift right 8
    // This avoids creating SIGN_EXTEND nodes during type legalization
    SDValue Eight = DAG.getConstant(8, DL, MVT::i16);
    PromotedLHS = DAG.getNode(ISD::SHL, DL, MVT::i16,
                               DAG.getNode(ISD::ANY_EXTEND, DL, MVT::i16, LHS), Eight);
    PromotedLHS = DAG.getNode(ISD::SRA, DL, MVT::i16, PromotedLHS, Eight);
    PromotedRHS = DAG.getNode(ISD::SHL, DL, MVT::i16,
                               DAG.getNode(ISD::ANY_EXTEND, DL, MVT::i16, RHS), Eight);
    PromotedRHS = DAG.getNode(ISD::SRA, DL, MVT::i16, PromotedRHS, Eight);
  } else {
    // Zero extend: AND with 0xFF
    SDValue Mask = DAG.getConstant(0xFF, DL, MVT::i16);
    PromotedLHS = DAG.getNode(ISD::AND, DL, MVT::i16,
                               DAG.getNode(ISD::ANY_EXTEND, DL, MVT::i16, LHS), Mask);
    PromotedRHS = DAG.getNode(ISD::AND, DL, MVT::i16,
                               DAG.getNode(ISD::ANY_EXTEND, DL, MVT::i16, RHS), Mask);
  }
  return true;
}

void W65816TargetLowering::ReplaceNodeResults(SDNode *N,
                                               SmallVectorImpl<SDValue> &Results,
                                               SelectionDAG &DAG) const {
  SDLoc DL(N);

  switch (N->getOpcode()) {
  case ISD::SETCC: {
    // Handle i8 SETCC by promoting operands to i16 and lowering directly
    // to our custom SELECT_CC (same as LowerSETCC but with promoted operands)
    SDValue LHS = N->getOperand(0);
    SDValue RHS = N->getOperand(1);
    ISD::CondCode CC = cast<CondCodeSDNode>(N->getOperand(2))->get();

    // Only handle if operands need promotion (are i8)
    if (LHS.getValueType() != MVT::i8)
      break;

    SDValue PromotedLHS, PromotedRHS;
    promoteCompareOperands(DAG, DL, LHS, RHS, CC, PromotedLHS, PromotedRHS);

    // Create a comparison node that sets flags (using promoted i16 operands)
    SDValue Cmp = DAG.getNode(W65816ISD::CMP, DL, MVT::Glue, PromotedLHS, PromotedRHS);

    // Get the W65816 condition code for this ISD condition
    W65816CC::CondCode W65CC = getCondCodeForISD(CC);

    // SETCC returns 1 if true, 0 if false
    SDValue TrueVal = DAG.getConstant(1, DL, MVT::i16);
    SDValue FalseVal = DAG.getConstant(0, DL, MVT::i16);

    SDVTList VTs = DAG.getVTList(MVT::i16, MVT::Glue);
    SDValue Ops[] = {TrueVal, FalseVal, DAG.getConstant(W65CC, DL, MVT::i16), Cmp};

    SDValue Result = DAG.getNode(W65816ISD::SELECT_CC, DL, VTs, Ops);

    // Truncate result to the expected type if needed
    if (N->getValueType(0) != MVT::i16) {
      Result = DAG.getNode(ISD::TRUNCATE, DL, N->getValueType(0), Result);
    }
    Results.push_back(Result);
    return;
  }
  case ISD::BR_CC: {
    // Handle i8 BR_CC by promoting comparison operands to i16
    SDValue Chain = N->getOperand(0);
    ISD::CondCode CC = cast<CondCodeSDNode>(N->getOperand(1))->get();
    SDValue LHS = N->getOperand(2);
    SDValue RHS = N->getOperand(3);
    SDValue Dest = N->getOperand(4);

    // Only handle if operands need promotion (are i8)
    if (LHS.getValueType() != MVT::i8)
      break;

    SDValue PromotedLHS, PromotedRHS;
    promoteCompareOperands(DAG, DL, LHS, RHS, CC, PromotedLHS, PromotedRHS);

    // Create comparison and branch using our custom nodes
    SDValue Cmp = DAG.getNode(W65816ISD::CMP, DL, MVT::Glue, PromotedLHS, PromotedRHS);
    W65816CC::CondCode W65CC = getCondCodeForISD(CC);
    SDValue BrCond = DAG.getNode(W65816ISD::BRCOND, DL, MVT::Other,
                                  Chain, Dest,
                                  DAG.getConstant(W65CC, DL, MVT::i16), Cmp);
    Results.push_back(BrCond);
    return;
  }
  case ISD::SELECT_CC: {
    // Handle i8 SELECT_CC by promoting comparison operands to i16
    SDValue LHS = N->getOperand(0);
    SDValue RHS = N->getOperand(1);
    SDValue TrueV = N->getOperand(2);
    SDValue FalseV = N->getOperand(3);
    ISD::CondCode CC = cast<CondCodeSDNode>(N->getOperand(4))->get();

    // Only handle if operands need promotion (are i8)
    if (LHS.getValueType() != MVT::i8)
      break;

    SDValue PromotedLHS, PromotedRHS;
    promoteCompareOperands(DAG, DL, LHS, RHS, CC, PromotedLHS, PromotedRHS);

    // Create comparison and select using our custom nodes
    SDValue Cmp = DAG.getNode(W65816ISD::CMP, DL, MVT::Glue, PromotedLHS, PromotedRHS);
    W65816CC::CondCode W65CC = getCondCodeForISD(CC);

    SDVTList VTs = DAG.getVTList(N->getValueType(0), MVT::Glue);
    SDValue Ops[] = {TrueV, FalseV, DAG.getConstant(W65CC, DL, MVT::i16), Cmp};

    SDValue Result = DAG.getNode(W65816ISD::SELECT_CC, DL, VTs, Ops);
    Results.push_back(Result);
    return;
  }
  default:
    break;
  }

  // Default: try LowerOperation
  SDValue Res = LowerOperation(SDValue(N, 0), DAG);
  if (Res.getNode()) {
    for (unsigned I = 0, E = Res->getNumValues(); I != E; ++I)
      Results.push_back(Res.getValue(I));
  }
}

/// Check if a node is a zext/anyext load from i8 to i16
static bool isExtLoadFromI8(SDValue N, SDValue &BaseAddr, SDValue &Chain) {
  // Check for extending load from i8
  if (auto *LD = dyn_cast<LoadSDNode>(N)) {
    if (LD->getMemoryVT() == MVT::i8 &&
        LD->getExtensionType() != ISD::NON_EXTLOAD) {
      BaseAddr = LD->getBasePtr();
      Chain = LD->getChain();
      return true;
    }
  }

  // Also check for (and (load i8), 255) pattern which is equivalent to zextload
  if (N.getOpcode() == ISD::AND) {
    if (auto *C = dyn_cast<ConstantSDNode>(N.getOperand(1))) {
      if (C->getZExtValue() == 255) {
        SDValue Inner = N.getOperand(0);
        if (auto *LD = dyn_cast<LoadSDNode>(Inner)) {
          if (LD->getMemoryVT() == MVT::i8) {
            BaseAddr = LD->getBasePtr();
            Chain = LD->getChain();
            return true;
          }
        }
      }
    }
  }

  return false;
}

/// Check if an address is a simple global address (not indexed)
static bool isSimpleGlobalAddr(SDValue Addr) {
  if (Addr.getOpcode() == W65816ISD::WRAPPER) {
    SDValue Inner = Addr.getOperand(0);
    return Inner.getOpcode() == ISD::TargetGlobalAddress;
  }
  return false;
}

SDValue
W65816TargetLowering::PerformDAGCombine(SDNode *N,
                                        DAGCombinerInfo &DCI) const {
  (void)DCI; // May be used in future combines
  (void)N;   // Used in switch below

  switch (N->getOpcode()) {
  default:
    break;

  case ISD::AND:
  case ISD::OR:
  case ISD::XOR: {
    // Optimize (binop (zextload i8), (zextload i8)) pattern
    // This avoids register pressure issues on the W65816 which only has
    // A, X, Y registers and loads can only go to A.
    //
    // Instead of:
    //   lda addr1 ; zext ; spill ; lda addr2 ; zext ; reload ; and
    // We generate:
    //   sep #32 ; lda addr1 ; binop addr2 ; rep #32 ; and #255
    //
    // This uses memory-operand form and only needs one register.

    SDValue LHS = N->getOperand(0);
    SDValue RHS = N->getOperand(1);

    SDValue LHSAddr, LHSChain;
    SDValue RHSAddr, RHSChain;

    // Check if both operands are extending loads from i8
    if (!isExtLoadFromI8(LHS, LHSAddr, LHSChain))
      break;
    if (!isExtLoadFromI8(RHS, RHSAddr, RHSChain))
      break;

    // Only handle simple global addresses for now (not indexed access)
    // This is the common case for boolean variables
    if (!isSimpleGlobalAddr(LHSAddr) || !isSimpleGlobalAddr(RHSAddr))
      break;

    // Create a new sequence that loads LHS, does the binop with RHS from memory,
    // then zero-extends.
    // The result will be selected in ISelDAGToDAG to generate the optimal code.
    //
    // For now, let the default pattern matching handle it.
    // The key optimization is to ensure one operand stays as a load node.
    //
    // We can't easily restructure the DAG here because the loads may have
    // side effects (chains). Instead, we rely on instruction selection
    // to fold one load into the binop instruction.
    //
    // The actual fix is in instruction selection (ISelDAGToDAG.cpp) to
    // recognize this pattern and generate proper code using AND_abs in 8-bit mode.

    // For now, we don't transform but return early to avoid default handling
    // causing issues. The instruction selector will handle this case.
    break;
  }
  }

  return SDValue();
}
