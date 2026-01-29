//===-- W65816AsmParser.cpp - Parse W65816 assembly to MCInst instructions ===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/W65816MCTargetDesc.h"
#include "TargetInfo/W65816TargetInfo.h"

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCParser/AsmLexer.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "w65816-asm-parser"

using namespace llvm;

namespace {

/// W65816Operand - Instances of this class represent a parsed W65816
/// machine instruction operand.
class W65816Operand : public MCParsedAsmOperand {
public:
  enum KindTy {
    k_Token,
    k_Register,
    k_Immediate
  };

private:
  KindTy Kind;

  struct TokOp {
    const char *Data;
    unsigned Length;
  };

  struct RegOp {
    unsigned RegNum;
  };

  struct ImmOp {
    const MCExpr *Val;
  };

  union {
    TokOp Tok;
    RegOp Reg;
    ImmOp Imm;
  };

  SMLoc StartLoc, EndLoc;

public:
  W65816Operand(KindTy K) : Kind(K) {}

  bool isToken() const override { return Kind == k_Token; }
  bool isImm() const override { return Kind == k_Immediate; }
  bool isReg() const override { return Kind == k_Register; }
  bool isMem() const override { return false; }

  MCRegister getReg() const override {
    assert(Kind == k_Register && "Invalid type access!");
    return Reg.RegNum;
  }

  StringRef getToken() const {
    assert(Kind == k_Token && "Invalid type access!");
    return StringRef(Tok.Data, Tok.Length);
  }

  const MCExpr *getImm() const {
    assert(Kind == k_Immediate && "Invalid type access!");
    return Imm.Val;
  }

  SMLoc getStartLoc() const override { return StartLoc; }
  SMLoc getEndLoc() const override { return EndLoc; }

  // Operand type checkers for the AsmMatcher
  // These return true if the operand could match the specified type

  bool isImm8() const {
    if (!isImm())
      return false;
    if (auto *CE = dyn_cast<MCConstantExpr>(getImm()))
      return isUInt<8>(CE->getValue()) || isInt<8>(CE->getValue());
    return true; // Allow symbols
  }

  bool isImm16() const {
    if (!isImm())
      return false;
    if (auto *CE = dyn_cast<MCConstantExpr>(getImm()))
      return isUInt<16>(CE->getValue()) || isInt<16>(CE->getValue());
    return true;
  }

  bool isAddr16() const { return isImm16(); }
  bool isAddr24() const { return isImm(); }
  bool isAddr8DP() const { return isImm8(); }
  bool isBrTarget8() const { return isImm(); }
  bool isBrTarget16() const { return isImm(); }

  // Indirect addressing mode checkers - for now these always return false
  // since we don't parse indirect modes yet. They can be enabled later.
  bool isIndirectAddr16() const { return false; }
  bool isIndirectXAddr16() const { return false; }
  bool isIndirectLongAddr16() const { return false; }
  bool isIndirectDP() const { return false; }
  bool isIndirectDPY() const { return false; }
  bool isIndexedIndirectDP() const { return false; }
  bool isIndirectDPLong() const { return false; }
  bool isIndirectDPLongY() const { return false; }
  bool isStackRelIndirectY() const { return false; }

  void print(raw_ostream &OS, const MCAsmInfo &MAI) const override {
    switch (Kind) {
    case k_Token:
      OS << "Token: " << getToken();
      break;
    case k_Register:
      OS << "Reg: " << getReg();
      break;
    case k_Immediate:
      OS << "Imm: ";
      MAI.printExpr(OS, *getImm());
      break;
    }
  }

  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    const MCExpr *Expr = getImm();
    if (auto *CE = dyn_cast<MCConstantExpr>(Expr))
      Inst.addOperand(MCOperand::createImm(CE->getValue()));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  static std::unique_ptr<W65816Operand> createToken(StringRef Str, SMLoc S) {
    auto Op = std::make_unique<W65816Operand>(k_Token);
    Op->Tok.Data = Str.data();
    Op->Tok.Length = Str.size();
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  static std::unique_ptr<W65816Operand> createReg(unsigned RegNo, SMLoc S,
                                                   SMLoc E) {
    auto Op = std::make_unique<W65816Operand>(k_Register);
    Op->Reg.RegNum = RegNo;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<W65816Operand> createImm(const MCExpr *Val, SMLoc S,
                                                   SMLoc E) {
    auto Op = std::make_unique<W65816Operand>(k_Immediate);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }
};

/// W65816AsmParser - Parse W65816 assembly to MCInst instructions
class W65816AsmParser : public MCTargetAsmParser {
  MCAsmParser &Parser;
  const MCRegisterInfo *MRI;

#define GET_ASSEMBLER_HEADER
#include "W65816GenAsmMatcher.inc"

  bool matchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;

  bool parseRegister(MCRegister &Reg, SMLoc &StartLoc, SMLoc &EndLoc) override;
  ParseStatus tryParseRegister(MCRegister &Reg, SMLoc &StartLoc,
                               SMLoc &EndLoc) override;

  bool parseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;

  ParseStatus parseDirective(AsmToken DirectiveID) override;

  bool parseOperand(OperandVector &Operands);
  bool parseImmediate(OperandVector &Operands);
  MCRegister matchRegisterName(StringRef Name);

  // Indirect addressing mode parsers (called by AsmMatcher)
  ParseStatus parseIndirectAddr(OperandVector &Operands);
  ParseStatus parseIndirectXAddr(OperandVector &Operands);
  ParseStatus parseIndirectLongAddr(OperandVector &Operands);
  ParseStatus parseIndirectDP(OperandVector &Operands);
  ParseStatus parseIndirectDPY(OperandVector &Operands);
  ParseStatus parseIndexedIndirectDP(OperandVector &Operands);
  ParseStatus parseIndirectDPLong(OperandVector &Operands);
  ParseStatus parseIndirectDPLongY(OperandVector &Operands);
  ParseStatus parseStackRelIndirectY(OperandVector &Operands);

public:
  W65816AsmParser(const MCSubtargetInfo &STI, MCAsmParser &Parser,
                  const MCInstrInfo &MII, const MCTargetOptions &Options)
      : MCTargetAsmParser(Options, STI, MII), Parser(Parser) {
    MCAsmParserExtension::Initialize(Parser);
    MRI = getContext().getRegisterInfo();
    setAvailableFeatures(ComputeAvailableFeatures(STI.getFeatureBits()));
  }

  MCAsmParser &getParser() const { return Parser; }
  AsmLexer &getLexer() const { return Parser.getLexer(); }
};

} // end anonymous namespace

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#include "W65816GenAsmMatcher.inc"

MCRegister W65816AsmParser::matchRegisterName(StringRef Name) {
  // Handle register names (case-insensitive)
  MCRegister Reg = StringSwitch<MCRegister>(Name.upper())
    .Case("A", W65816::A)
    .Case("X", W65816::X)
    .Case("Y", W65816::Y)
    .Case("S", W65816::SP)
    .Case("SP", W65816::SP)
    .Case("D", W65816::D)
    .Case("DBR", W65816::DBR)
    .Case("PBR", W65816::PBR)
    .Case("P", W65816::P)
    .Default(MCRegister());
  return Reg;
}

bool W65816AsmParser::parseRegister(MCRegister &Reg, SMLoc &StartLoc,
                                     SMLoc &EndLoc) {
  ParseStatus Res = tryParseRegister(Reg, StartLoc, EndLoc);
  if (!Res.isSuccess())
    return true;
  return false;
}

ParseStatus W65816AsmParser::tryParseRegister(MCRegister &Reg, SMLoc &StartLoc,
                                               SMLoc &EndLoc) {
  StartLoc = getLexer().getLoc();
  const AsmToken &Tok = getLexer().getTok();

  if (Tok.isNot(AsmToken::Identifier))
    return ParseStatus::NoMatch;

  Reg = matchRegisterName(Tok.getString());
  if (!Reg)
    return ParseStatus::NoMatch;

  EndLoc = getLexer().getLoc();
  getLexer().Lex(); // Consume register name
  return ParseStatus::Success;
}

bool W65816AsmParser::parseImmediate(OperandVector &Operands) {
  SMLoc S = getLexer().getLoc();

  // Check for '#' prefix (immediate mode indicator)
  if (getLexer().is(AsmToken::Hash)) {
    getLexer().Lex(); // Eat '#'
  }

  const MCExpr *Expr;
  if (getParser().parseExpression(Expr))
    return true;

  SMLoc E = getLexer().getLoc();
  Operands.push_back(W65816Operand::createImm(Expr, S, E));
  return false;
}

bool W65816AsmParser::parseOperand(OperandVector &Operands) {
  SMLoc S = getLexer().getLoc();

  // Try to parse as register first
  MCRegister Reg;
  SMLoc RegStart, RegEnd;
  ParseStatus RegRes = tryParseRegister(Reg, RegStart, RegEnd);
  if (RegRes.isSuccess()) {
    Operands.push_back(W65816Operand::createReg(Reg, RegStart, RegEnd));
    return false;
  }

  // Try to parse as immediate (with optional # prefix)
  if (getLexer().is(AsmToken::Hash)) {
    return parseImmediate(Operands);
  }

  // Otherwise parse as expression (address operand)
  const MCExpr *Expr;
  if (getParser().parseExpression(Expr))
    return true;

  SMLoc E = getLexer().getLoc();
  Operands.push_back(W65816Operand::createImm(Expr, S, E));
  return false;
}

bool W65816AsmParser::parseInstruction(ParseInstructionInfo &Info,
                                        StringRef Name, SMLoc NameLoc,
                                        OperandVector &Operands) {
  // Add the mnemonic as the first operand
  Operands.push_back(W65816Operand::createToken(Name, NameLoc));

  // Parse operands
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    // Parse first operand
    if (parseOperand(Operands))
      return true;

    // Parse subsequent operands separated by comma
    while (getLexer().is(AsmToken::Comma)) {
      getLexer().Lex(); // Eat comma

      if (parseOperand(Operands))
        return true;
    }
  }

  // Check for end of statement
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    return Error(getLexer().getLoc(), "unexpected token in operand");
  }

  return false;
}

bool W65816AsmParser::matchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                               OperandVector &Operands,
                                               MCStreamer &Out,
                                               uint64_t &ErrorInfo,
                                               bool MatchingInlineAsm) {
  MCInst Inst;
  unsigned MatchResult =
      MatchInstructionImpl(Operands, Inst, ErrorInfo, MatchingInlineAsm);

  switch (MatchResult) {
  case Match_Success:
    Inst.setLoc(IDLoc);
    Out.emitInstruction(Inst, getSTI());
    return false;

  case Match_MissingFeature:
    return Error(IDLoc, "instruction requires a CPU feature not currently enabled");

  case Match_InvalidOperand: {
    SMLoc ErrorLoc = IDLoc;
    if (ErrorInfo != ~0ULL) {
      if (ErrorInfo >= Operands.size())
        return Error(IDLoc, "too few operands for instruction");
      ErrorLoc = ((W65816Operand &)*Operands[ErrorInfo]).getStartLoc();
      if (ErrorLoc == SMLoc())
        ErrorLoc = IDLoc;
    }
    return Error(ErrorLoc, "invalid operand for instruction");
  }

  case Match_MnemonicFail:
    return Error(IDLoc, "unrecognized instruction mnemonic");
  }

  llvm_unreachable("Unknown match type detected!");
}

ParseStatus W65816AsmParser::parseDirective(AsmToken DirectiveID) {
  // No directives supported yet
  return ParseStatus::NoMatch;
}

//===----------------------------------------------------------------------===//
// Indirect Addressing Mode Parsers
// These are called by the generated AsmMatcher when trying to match operands
//===----------------------------------------------------------------------===//

// Parse (addr) - Indirect 16-bit address
ParseStatus W65816AsmParser::parseIndirectAddr(OperandVector &Operands) {
  // Not implemented yet - indirect modes require more complex parsing
  return ParseStatus::NoMatch;
}

// Parse (addr,x) - Indexed Indirect 16-bit address
ParseStatus W65816AsmParser::parseIndirectXAddr(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

// Parse [addr] - Indirect Long 16-bit address
ParseStatus W65816AsmParser::parseIndirectLongAddr(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

// Parse (dp) - Indirect Direct Page
ParseStatus W65816AsmParser::parseIndirectDP(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

// Parse (dp),y - Indirect Direct Page Indexed Y
ParseStatus W65816AsmParser::parseIndirectDPY(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

// Parse (dp,x) - Indexed Indirect Direct Page
ParseStatus W65816AsmParser::parseIndexedIndirectDP(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

// Parse [dp] - Indirect Long Direct Page
ParseStatus W65816AsmParser::parseIndirectDPLong(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

// Parse [dp],y - Indirect Long Direct Page Indexed Y
ParseStatus W65816AsmParser::parseIndirectDPLongY(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

// Parse (offset,s),y - Stack Relative Indirect Indexed Y
ParseStatus W65816AsmParser::parseStackRelIndirectY(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeW65816AsmParser() {
  RegisterMCAsmParser<W65816AsmParser> X(getTheW65816Target());
}
