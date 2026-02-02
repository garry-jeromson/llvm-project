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
    k_Immediate,       // Address or immediate value
    // Indirect addressing modes
    k_IndirectAddr,      // (addr) - JMP indirect
    k_IndirectXAddr,     // (addr,x) - JMP indexed indirect
    k_IndirectLongAddr,  // [addr] - JMP indirect long
    k_IndirectDP,        // (dp) - DP indirect
    k_IndirectDPY,       // (dp),y - DP indirect indexed Y
    k_IndexedIndirectDP, // (dp,x) - Indexed indirect DP
    k_IndirectDPLong,    // [dp] - DP indirect long
    k_IndirectDPLongY,   // [dp],y - DP indirect long indexed Y
    k_StackRelIndirectY  // (offset,s),y - Stack relative indirect indexed Y
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
  bool isMem() const override {
    return Kind >= k_IndirectAddr && Kind <= k_StackRelIndirectY;
  }

  MCRegister getReg() const override {
    assert(Kind == k_Register && "Invalid type access!");
    return Reg.RegNum;
  }

  StringRef getToken() const {
    assert(Kind == k_Token && "Invalid type access!");
    return StringRef(Tok.Data, Tok.Length);
  }

  const MCExpr *getImm() const {
    assert((Kind == k_Immediate || isMem()) && "Invalid type access!");
    return Imm.Val;
  }

  SMLoc getStartLoc() const override { return StartLoc; }
  SMLoc getEndLoc() const override { return EndLoc; }

  // Operand type checkers for the AsmMatcher
  // These return true if the operand could match the specified type

  // Immediate operand checkers (for #value syntax)
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

  // Address operand checkers (for absolute/direct page addressing)
  bool isAddr16() const {
    if (!isImm())
      return false;
    if (auto *CE = dyn_cast<MCConstantExpr>(getImm()))
      return isUInt<16>(CE->getValue()) || isInt<16>(CE->getValue());
    return true;
  }
  bool isAddr24() const { return isImm(); }
  bool isAddr8DP() const {
    if (!isImm())
      return false;
    if (auto *CE = dyn_cast<MCConstantExpr>(getImm()))
      return isUInt<8>(CE->getValue()) || isInt<8>(CE->getValue());
    return true;
  }
  bool isBrTarget8() const { return isImm(); }
  bool isBrTarget16() const { return isImm(); }

  // Indirect addressing mode checkers
  // Note: (expr) syntax can be either DP indirect or absolute indirect,
  // so we allow both type checkers to match. The instruction definition
  // determines which encoding is used.
  bool isIndirectAddr16() const {
    return Kind == k_IndirectAddr || Kind == k_IndirectDP;
  }
  bool isIndirectXAddr16() const {
    return Kind == k_IndirectXAddr || Kind == k_IndexedIndirectDP;
  }
  bool isIndirectLongAddr16() const {
    return Kind == k_IndirectLongAddr || Kind == k_IndirectDPLong;
  }
  bool isIndirectDP() const { return Kind == k_IndirectDP; }
  bool isIndirectDPY() const { return Kind == k_IndirectDPY; }
  bool isIndexedIndirectDP() const { return Kind == k_IndexedIndirectDP; }
  bool isIndirectDPLong() const { return Kind == k_IndirectDPLong; }
  bool isIndirectDPLongY() const { return Kind == k_IndirectDPLongY; }
  bool isStackRelIndirectY() const { return Kind == k_StackRelIndirectY; }

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
    case k_IndirectAddr:
      OS << "IndirectAddr: (";
      MAI.printExpr(OS, *getImm());
      OS << ")";
      break;
    case k_IndirectXAddr:
      OS << "IndirectXAddr: (";
      MAI.printExpr(OS, *getImm());
      OS << ",x)";
      break;
    case k_IndirectLongAddr:
      OS << "IndirectLongAddr: [";
      MAI.printExpr(OS, *getImm());
      OS << "]";
      break;
    case k_IndirectDP:
      OS << "IndirectDP: (";
      MAI.printExpr(OS, *getImm());
      OS << ")";
      break;
    case k_IndirectDPY:
      OS << "IndirectDPY: (";
      MAI.printExpr(OS, *getImm());
      OS << "),y";
      break;
    case k_IndexedIndirectDP:
      OS << "IndexedIndirectDP: (";
      MAI.printExpr(OS, *getImm());
      OS << ",x)";
      break;
    case k_IndirectDPLong:
      OS << "IndirectDPLong: [";
      MAI.printExpr(OS, *getImm());
      OS << "]";
      break;
    case k_IndirectDPLongY:
      OS << "IndirectDPLongY: [";
      MAI.printExpr(OS, *getImm());
      OS << "],y";
      break;
    case k_StackRelIndirectY:
      OS << "StackRelIndirectY: (";
      MAI.printExpr(OS, *getImm());
      OS << ",s),y";
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

  // All indirect modes store their address in Imm.Val
  void addIndirectAddr16Operands(MCInst &Inst, unsigned N) const {
    addImmOperands(Inst, N);
  }
  void addIndirectXAddr16Operands(MCInst &Inst, unsigned N) const {
    addImmOperands(Inst, N);
  }
  void addIndirectLongAddr16Operands(MCInst &Inst, unsigned N) const {
    addImmOperands(Inst, N);
  }
  void addIndirectDPOperands(MCInst &Inst, unsigned N) const {
    addImmOperands(Inst, N);
  }
  void addIndirectDPYOperands(MCInst &Inst, unsigned N) const {
    addImmOperands(Inst, N);
  }
  void addIndexedIndirectDPOperands(MCInst &Inst, unsigned N) const {
    addImmOperands(Inst, N);
  }
  void addIndirectDPLongOperands(MCInst &Inst, unsigned N) const {
    addImmOperands(Inst, N);
  }
  void addIndirectDPLongYOperands(MCInst &Inst, unsigned N) const {
    addImmOperands(Inst, N);
  }
  void addStackRelIndirectYOperands(MCInst &Inst, unsigned N) const {
    addImmOperands(Inst, N);
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


  static std::unique_ptr<W65816Operand> createIndirect(KindTy Kind,
                                                        const MCExpr *Val,
                                                        SMLoc S, SMLoc E) {
    auto Op = std::make_unique<W65816Operand>(Kind);
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
  bool parseParenExpr(OperandVector &Operands);
  bool parseBracketExpr(OperandVector &Operands);
  MCRegister matchRegisterName(StringRef Name);

  // Indirect addressing mode parsers (called by AsmMatcher for custom operands)
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
// Silence warnings about generated functions that we don't use directly.
// We have our own matchRegisterName that handles case-insensitive matching.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-function"
#include "W65816GenAsmMatcher.inc"
#pragma clang diagnostic pop

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

  // Expect '#' prefix for immediate mode - emit as a token
  if (getLexer().isNot(AsmToken::Hash))
    return true;
  Operands.push_back(W65816Operand::createToken("#", S));
  getLexer().Lex(); // Eat '#'

  // Parse the immediate value
  const MCExpr *Expr;
  if (getParser().parseExpression(Expr))
    return true;

  SMLoc E = getLexer().getLoc();
  Operands.push_back(W65816Operand::createImm(Expr, S, E));
  return false;
}

/// Parse parenthesized expression - handles various indirect modes
/// Syntax: (expr) or (expr),y or (expr,x) or (expr,s),y
bool W65816AsmParser::parseParenExpr(OperandVector &Operands) {
  SMLoc S = getLexer().getLoc();

  // Expect '('
  if (getLexer().isNot(AsmToken::LParen))
    return true;
  getLexer().Lex(); // Eat '('

  // Parse the expression
  const MCExpr *Expr;
  if (getParser().parseExpression(Expr))
    return true;

  // Check what follows the expression
  if (getLexer().is(AsmToken::Comma)) {
    getLexer().Lex(); // Eat ','

    // Check for register (x or s)
    if (getLexer().isNot(AsmToken::Identifier))
      return Error(getLexer().getLoc(), "expected register after ','");

    std::string RegNameStr = getLexer().getTok().getString().upper();
    StringRef RegName = RegNameStr;

    if (RegName == "X") {
      // (expr,x) - Indexed indirect
      getLexer().Lex(); // Eat 'x'

      if (getLexer().isNot(AsmToken::RParen))
        return Error(getLexer().getLoc(), "expected ')' after ',x'");
      getLexer().Lex(); // Eat ')'

      SMLoc E = getLexer().getLoc();
      Operands.push_back(W65816Operand::createIndirect(
          W65816Operand::k_IndexedIndirectDP, Expr, S, E));
      return false;
    }

    if (RegName == "S") {
      // (expr,s) - Stack relative, might be (expr,s),y
      getLexer().Lex(); // Eat 's'

      if (getLexer().isNot(AsmToken::RParen))
        return Error(getLexer().getLoc(), "expected ')' after ',s'");
      getLexer().Lex(); // Eat ')'

      // Check for ,y suffix
      if (getLexer().is(AsmToken::Comma)) {
        getLexer().Lex(); // Eat ','

        if (getLexer().isNot(AsmToken::Identifier))
          return Error(getLexer().getLoc(), "expected 'y' after ','");

        if (getLexer().getTok().getString().upper() != "Y")
          return Error(getLexer().getLoc(), "expected 'y' register");

        getLexer().Lex(); // Eat 'y'

        SMLoc E = getLexer().getLoc();
        Operands.push_back(W65816Operand::createIndirect(
            W65816Operand::k_StackRelIndirectY, Expr, S, E));
        return false;
      }

      // Plain (expr,s) without ,y - not a standard mode, treat as error
      return Error(getLexer().getLoc(), "stack relative indirect requires ',y' suffix");
    }

    return Error(getLexer().getLoc(), "expected 'x' or 's' register");
  }

  // Expect ')'
  if (getLexer().isNot(AsmToken::RParen))
    return Error(getLexer().getLoc(), "expected ')' or ','");
  getLexer().Lex(); // Eat ')'

  // Check for ,y suffix: (expr),y
  if (getLexer().is(AsmToken::Comma)) {
    getLexer().Lex(); // Eat ','

    if (getLexer().isNot(AsmToken::Identifier))
      return Error(getLexer().getLoc(), "expected 'y' after ','");

    if (getLexer().getTok().getString().upper() != "Y")
      return Error(getLexer().getLoc(), "expected 'y' register");

    getLexer().Lex(); // Eat 'y'

    SMLoc E = getLexer().getLoc();
    Operands.push_back(W65816Operand::createIndirect(
        W65816Operand::k_IndirectDPY, Expr, S, E));
    return false;
  }

  // Plain (expr) - could be indirect DP or indirect addr (JMP)
  // Parse as IndirectDP; the AsmMatcher validates operand size to
  // distinguish between DP indirect ($xx) and absolute indirect ($xxxx).
  SMLoc E = getLexer().getLoc();
  Operands.push_back(W65816Operand::createIndirect(
      W65816Operand::k_IndirectDP, Expr, S, E));
  return false;
}

/// Parse bracketed expression - handles indirect long modes
/// Syntax: [expr] or [expr],y
bool W65816AsmParser::parseBracketExpr(OperandVector &Operands) {
  SMLoc S = getLexer().getLoc();

  // Expect '['
  if (getLexer().isNot(AsmToken::LBrac))
    return true;
  getLexer().Lex(); // Eat '['

  // Parse the expression
  const MCExpr *Expr;
  if (getParser().parseExpression(Expr))
    return true;

  // Expect ']'
  if (getLexer().isNot(AsmToken::RBrac))
    return Error(getLexer().getLoc(), "expected ']'");
  getLexer().Lex(); // Eat ']'

  // Check for ,y suffix: [expr],y
  if (getLexer().is(AsmToken::Comma)) {
    getLexer().Lex(); // Eat ','

    if (getLexer().isNot(AsmToken::Identifier))
      return Error(getLexer().getLoc(), "expected 'y' after ','");

    if (getLexer().getTok().getString().upper() != "Y")
      return Error(getLexer().getLoc(), "expected 'y' register");

    getLexer().Lex(); // Eat 'y'

    SMLoc E = getLexer().getLoc();
    Operands.push_back(W65816Operand::createIndirect(
        W65816Operand::k_IndirectDPLongY, Expr, S, E));
    return false;
  }

  // Plain [expr] - indirect long
  SMLoc E = getLexer().getLoc();
  Operands.push_back(W65816Operand::createIndirect(
      W65816Operand::k_IndirectDPLong, Expr, S, E));
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

  // Try to parse as immediate (with # prefix)
  if (getLexer().is(AsmToken::Hash)) {
    return parseImmediate(Operands);
  }

  // Try to parse indirect addressing modes
  if (getLexer().is(AsmToken::LParen)) {
    return parseParenExpr(Operands);
  }

  // Try to parse indirect long modes
  if (getLexer().is(AsmToken::LBrac)) {
    return parseBracketExpr(Operands);
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
    // Note: for indirect modes like (dp),y the comma is already consumed
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
    // Include instruction name in error for better context
    std::string Msg = "invalid operand for instruction";
    if (!Operands.empty()) {
      StringRef Mnemonic = ((W65816Operand &)*Operands[0]).getToken();
      Msg = "invalid operand for '" + Mnemonic.str() + "' instruction";
    }
    return Error(ErrorLoc, Msg);
  }

  case Match_MnemonicFail: {
    std::string Msg = "unrecognized instruction mnemonic";
    if (!Operands.empty()) {
      StringRef Mnemonic = ((W65816Operand &)*Operands[0]).getToken();
      Msg = "unrecognized instruction mnemonic '" + Mnemonic.str() + "'";
    }
    return Error(IDLoc, Msg);
  }
  }

  llvm_unreachable("Unknown match type detected!");
}

ParseStatus W65816AsmParser::parseDirective(AsmToken DirectiveID) {
  // No directives supported yet
  return ParseStatus::NoMatch;
}

//===----------------------------------------------------------------------===//
// Custom Operand Parsers for AsmMatcher
// These are called by the generated AsmMatcher as fallback parsers
//===----------------------------------------------------------------------===//

// These parsers are called by MatchInstructionImpl when it needs to try
// parsing a specific operand type. Since we handle all indirect modes in
// parseOperand(), these can just return NoMatch to indicate the operand
// was already parsed (or doesn't match).

ParseStatus W65816AsmParser::parseIndirectAddr(OperandVector &Operands) {
  // Already handled in parseOperand()
  return ParseStatus::NoMatch;
}

ParseStatus W65816AsmParser::parseIndirectXAddr(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

ParseStatus W65816AsmParser::parseIndirectLongAddr(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

ParseStatus W65816AsmParser::parseIndirectDP(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

ParseStatus W65816AsmParser::parseIndirectDPY(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

ParseStatus W65816AsmParser::parseIndexedIndirectDP(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

ParseStatus W65816AsmParser::parseIndirectDPLong(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

ParseStatus W65816AsmParser::parseIndirectDPLongY(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

ParseStatus W65816AsmParser::parseStackRelIndirectY(OperandVector &Operands) {
  return ParseStatus::NoMatch;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeW65816AsmParser() {
  RegisterMCAsmParser<W65816AsmParser> X(getTheW65816Target());
}
