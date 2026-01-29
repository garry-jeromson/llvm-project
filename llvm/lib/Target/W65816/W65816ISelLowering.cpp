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

  // Set up the register classes
  // Note: We only register i16 as legal. i8 values will be promoted to i16.
  // 8-bit memory operations are handled through special load/store patterns.
  // addRegisterClass(MVT::i8, &W65816::GPR8RegClass);  // Keep i8 illegal
  addRegisterClass(MVT::i16, &W65816::GPR16RegClass);

  // Compute derived properties from the register classes
  computeRegisterProperties(STI.getRegisterInfo());

  // Set the stack pointer register
  setStackPointerRegisterToSaveRestore(W65816::SP);

  // Set scheduling preference
  setSchedulingPreference(Sched::RegPressure);

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
  setOperationAction(ISD::BR_CC, MVT::i8, Custom);
  setOperationAction(ISD::BR_CC, MVT::i16, Custom);
  setOperationAction(ISD::BRCOND, MVT::Other, Expand);

  // Select operations
  setOperationAction(ISD::SELECT, MVT::i8, Expand);
  setOperationAction(ISD::SELECT, MVT::i16, Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i8, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::i16, Custom);

  // Global addresses need custom lowering
  setOperationAction(ISD::GlobalAddress, MVT::i16, Custom);

  // Set condition code actions
  setOperationAction(ISD::SETCC, MVT::i8, Expand);
  setOperationAction(ISD::SETCC, MVT::i16, Expand);

  // Count leading/trailing zeros - expand (no hardware support)
  setOperationAction(ISD::CTLZ, MVT::i16, Expand);
  setOperationAction(ISD::CTTZ, MVT::i16, Expand);
  setOperationAction(ISD::CTPOP, MVT::i16, Expand);

  // Byte swap - expand
  setOperationAction(ISD::BSWAP, MVT::i16, Expand);

  // Note: We don't use Custom for LOAD/STORE because returning SDValue()
  // for cases we don't handle doesn't work properly. Instead, we rely on
  // SelectAddr returning false for register addresses, which causes a
  // clean "Cannot select" error for unsupported pointer loads.

  // 8-bit load/store support: use extending loads and truncating stores
  // For loads: zero-extend and sign-extend 8-bit values to 16-bit
  setLoadExtAction(ISD::ZEXTLOAD, MVT::i16, MVT::i8, Custom);
  setLoadExtAction(ISD::SEXTLOAD, MVT::i16, MVT::i8, Custom);
  setLoadExtAction(ISD::EXTLOAD, MVT::i16, MVT::i8, Custom);

  // For stores: truncate 16-bit values to 8-bit
  setTruncStoreAction(MVT::i16, MVT::i8, Custom);

  // Promote i8 operations to i16 (we operate in 16-bit mode mostly)
  setOperationAction(ISD::ADD, MVT::i8, Promote);
  setOperationAction(ISD::SUB, MVT::i8, Promote);
  setOperationAction(ISD::AND, MVT::i8, Promote);
  setOperationAction(ISD::OR, MVT::i8, Promote);
  setOperationAction(ISD::XOR, MVT::i8, Promote);
  setOperationAction(ISD::SHL, MVT::i8, Promote);
  setOperationAction(ISD::SRL, MVT::i8, Promote);
  setOperationAction(ISD::SRA, MVT::i8, Promote);

  // Min/max signed and unsigned
  setMinimumJumpTableEntries(INT_MAX); // Don't use jump tables
}

const char *W65816TargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch (Opcode) {
  case W65816ISD::RET_FLAG:
    return "W65816ISD::RET_FLAG";
  case W65816ISD::CALL:
    return "W65816ISD::CALL";
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

SDValue W65816TargetLowering::LowerOperation(SDValue Op,
                                             SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  case ISD::GlobalAddress:
    return LowerGlobalAddress(Op, DAG);
  case ISD::SELECT_CC:
    return LowerSELECT_CC(Op, DAG);
  case ISD::BR_CC:
    return LowerBR_CC(Op, DAG);
  case ISD::LOAD:
    return LowerLoad(Op, DAG);
  case ISD::STORE:
    return LowerStore(Op, DAG);
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
    // After CMP, BEQ not_taken, then BCS taken
    // Simplified: use BNE after checking BCS condition
    return W65816CC::COND_NE;  // TODO: proper UGT sequence
  case ISD::SETULE:
    // For A <= B unsigned: C=0 (A < B) OR Z=1 (A == B)
    return W65816CC::COND_EQ;  // TODO: proper ULE sequence
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
  }
  llvm_unreachable("Unknown condition code");
}

SDValue W65816TargetLowering::LowerSELECT_CC(SDValue Op,
                                             SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDValue TrueVal = Op.getOperand(2);
  SDValue FalseVal = Op.getOperand(3);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(4))->get();

  // Create a comparison that sets the processor flags
  // CMP subtracts RHS from LHS and sets flags
  SDValue Cmp = DAG.getNode(W65816ISD::CMP, DL, MVT::Glue, LHS, RHS);

  // Get the W65816 condition code for this ISD condition
  W65816CC::CondCode W65CC = getCondCodeForISD(CC);

  // Create the SELECT_CC node with the condition code
  SDVTList VTs = DAG.getVTList(Op.getValueType(), MVT::Glue);
  SDValue Ops[] = {TrueVal, FalseVal, DAG.getConstant(W65CC, DL, MVT::i8), Cmp};

  return DAG.getNode(W65816ISD::SELECT_CC, DL, VTs, Ops);
}

SDValue W65816TargetLowering::LowerBR_CC(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Chain = Op.getOperand(0);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue Dest = Op.getOperand(4);

  // Create a comparison node that sets flags
  SDValue Cmp = DAG.getNode(W65816ISD::CMP, DL, MVT::Glue, LHS, RHS);

  // Get the W65816 condition code
  W65816CC::CondCode W65CC = getCondCodeForISD(CC);

  // Create conditional branch: Chain, DestBB, CondCode, Flags
  return DAG.getNode(W65816ISD::BRCOND, DL, MVT::Other, Chain, Dest,
                     DAG.getConstant(W65CC, DL, MVT::i8), Cmp);
}

SDValue W65816TargetLowering::LowerLoad(SDValue Op, SelectionDAG &DAG) const {
  LoadSDNode *LD = cast<LoadSDNode>(Op);
  SDLoc DL(Op);
  SDValue Chain = LD->getChain();
  SDValue Addr = LD->getBasePtr();
  EVT MemVT = LD->getMemoryVT();
  EVT VT = Op.getValueType();

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

  // Create the diamond pattern:
  //   MBB:
  //     cmp ...        (already done before Select16)
  //     bCC trueMBB
  //     jmp falseMBB
  //   falseMBB:
  //     ... (falseVal)
  //     jmp sinkMBB
  //   trueMBB:
  //     ... (trueVal)
  //   sinkMBB:
  //     phi result

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

  // Get operands
  Register DstReg = MI.getOperand(0).getReg();
  Register TrueReg = MI.getOperand(1).getReg();
  Register FalseReg = MI.getOperand(2).getReg();
  W65816CC::CondCode CondCode = (W65816CC::CondCode)MI.getOperand(3).getImm();

  // Handle signed conditions by emitting a signed select pseudo
  // These pseudos are expanded later (in ExpandPseudo pass) to avoid
  // the branch folder breaking the multi-instruction sequences
  unsigned SignedSelectOpc = 0;
  switch (CondCode) {
  case W65816CC::COND_SLT: SignedSelectOpc = W65816::Select16_SLT; break;
  case W65816CC::COND_SGE: SignedSelectOpc = W65816::Select16_SGE; break;
  case W65816CC::COND_SGT: SignedSelectOpc = W65816::Select16_SGT; break;
  case W65816CC::COND_SLE: SignedSelectOpc = W65816::Select16_SLE; break;
  default: break;
  }

  if (SignedSelectOpc) {
    // For signed conditions, emit a pseudo that will be expanded later
    // Don't create the diamond pattern here - let ExpandPseudo handle it
    BuildMI(*MBB, MI, DL, TII.get(SignedSelectOpc), DstReg)
        .addReg(TrueReg)
        .addReg(FalseReg);
    MI.eraseFromParent();
    return MBB;
  }

  // For unsigned/equality conditions, create the diamond pattern here
  // Add edges: MBB -> TrueMBB, MBB -> FalseMBB
  MBB->addSuccessor(TrueMBB);
  MBB->addSuccessor(FalseMBB);

  // Simple case: single branch instruction
  unsigned BranchOpc = getBranchOpcodeForCond(CondCode);
  BuildMI(MBB, DL, TII.get(BranchOpc)).addMBB(TrueMBB);
  BuildMI(MBB, DL, TII.get(W65816::BRA)).addMBB(FalseMBB);

  // TrueMBB just falls through to SinkMBB
  TrueMBB->addSuccessor(SinkMBB);

  // FalseMBB branches to SinkMBB
  FalseMBB->addSuccessor(SinkMBB);
  BuildMI(FalseMBB, DL, TII.get(W65816::BRA)).addMBB(SinkMBB);

  // SinkMBB has the PHI node
  BuildMI(*SinkMBB, SinkMBB->begin(), DL, TII.get(TargetOpcode::PHI), DstReg)
      .addReg(TrueReg)
      .addMBB(TrueMBB)
      .addReg(FalseReg)
      .addMBB(FalseMBB);

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

  MachineFunction &MF = DAG.getMachineFunction();

  // Analyze operands of the call
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeCallOperands(Outs, CC_W65816);

  // Get a count of how many bytes are to be pushed on the stack
  unsigned NumBytes = CCInfo.getStackSize();

  // Since hasReservedCallFrame() returns true, space for outgoing arguments
  // is pre-allocated in the prologue. CALLSEQ_START/END become no-ops.
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
      // Stack argument - store to the reserved outgoing argument area
      assert(VA.isMemLoc() && "Expected memory location for stack arg");

      // Create a frame index for this argument slot
      // The offset is relative to the start of the outgoing argument area
      // which is at the bottom of the stack frame (lowest address)
      int FI = MF.getFrameInfo().CreateFixedObject(
          VA.getLocVT().getSizeInBits() / 8,
          VA.getLocMemOffset(),
          /*IsImmutable=*/false);
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

  // Copy arguments to registers
  for (auto &Reg : RegsToPass) {
    Chain = DAG.getCopyToReg(Chain, DL, Reg.first, Reg.second, Glue);
    Glue = Chain.getValue(1);
  }

  // Get the target global address
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

  // Add a register mask operand to define caller-saved registers
  const TargetRegisterInfo *TRI = Subtarget.getRegisterInfo();
  const uint32_t *Mask = TRI->getCallPreservedMask(MF, CallConv);
  Ops.push_back(DAG.getRegisterMask(Mask));

  if (Glue.getNode())
    Ops.push_back(Glue);

  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  Chain = DAG.getNode(W65816ISD::CALL, DL, NodeTys, Ops);
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
