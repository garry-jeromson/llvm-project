//===-- W65816Disassembler.cpp - Disassembler for W65816 ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the W65816Disassembler class.
// The W65816 uses opcode-based encoding where the first byte fully determines
// the instruction and addressing mode. Register operands are implied by the
// opcode, not encoded in operand bits.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/W65816MCTargetDesc.h"
#include "TargetInfo/W65816TargetInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Compiler.h"

using namespace llvm;

#define DEBUG_TYPE "w65816-disassembler"

typedef MCDisassembler::DecodeStatus DecodeStatus;

namespace {

// Instruction operand patterns
enum OperandPattern {
  Pat_None,       // No operands (implied instructions)
  Pat_Imm8,       // Single 8-bit immediate
  Pat_Imm16,      // Single 16-bit immediate
  Pat_Addr8,      // Single 8-bit address (DP, branch offset)
  Pat_Addr16,     // Single 16-bit address
  Pat_Addr24,     // Single 24-bit address
  Pat_A_Imm16,    // A register (output), 16-bit immediate (input)
  Pat_A_A_Imm16,  // A register (output, input), 16-bit immediate
  Pat_A_Addr8,    // A register (output), 8-bit address
  Pat_A_A_Addr8,  // A register (output, input), 8-bit address
  Pat_A_Addr16,   // A register (output), 16-bit address
  Pat_A_A_Addr16, // A register (output, input), 16-bit address
  Pat_A_Addr24,   // A register (output), 24-bit address
  Pat_A_A_Addr24, // A register (output, input), 24-bit address
  Pat_X_Imm16,    // X register (output), 16-bit immediate
  Pat_X_Addr8,    // X register (output), 8-bit address
  Pat_X_Addr16,   // X register (output), 16-bit address
  Pat_Y_Imm16,    // Y register (output), 16-bit immediate
  Pat_Y_Addr8,    // Y register (output), 8-bit address
  Pat_Y_Addr16,   // Y register (output), 16-bit address
  Pat_STA_Addr8,  // STA: A register (input), 8-bit address
  Pat_STA_Addr16, // STA: A register (input), 16-bit address
  Pat_STA_Addr24, // STA: A register (input), 24-bit address
  Pat_STX_Addr8,  // STX: X register (input), 8-bit address
  Pat_STX_Addr16, // STX: X register (input), 16-bit address
  Pat_STY_Addr8,  // STY: Y register (input), 8-bit address
  Pat_STY_Addr16, // STY: Y register (input), 16-bit address
  Pat_CMP_Imm16,  // CMP: A register (input), 16-bit immediate
  Pat_CMP_Addr16, // CMP: A register (input), 16-bit address
  Pat_BlockMove,  // Block move: srcbank, destbank
  Pat_Invalid     // Invalid opcode
};

// Opcode to instruction info mapping
struct OpcodeInfo {
  unsigned Opcode;        // LLVM instruction opcode (0 = invalid)
  OperandPattern Pattern; // Operand pattern
};

// Get instruction size from pattern
static unsigned getInstructionSize(OperandPattern Pat) {
  switch (Pat) {
  case Pat_None:
    return 1;
  case Pat_Imm8:
  case Pat_Addr8:
  case Pat_A_Addr8:
  case Pat_A_A_Addr8:
  case Pat_X_Addr8:
  case Pat_Y_Addr8:
  case Pat_STA_Addr8:
  case Pat_STX_Addr8:
  case Pat_STY_Addr8:
    return 2;
  case Pat_Imm16:
  case Pat_Addr16:
  case Pat_A_Imm16:
  case Pat_A_A_Imm16:
  case Pat_A_Addr16:
  case Pat_A_A_Addr16:
  case Pat_X_Imm16:
  case Pat_X_Addr16:
  case Pat_Y_Imm16:
  case Pat_Y_Addr16:
  case Pat_STA_Addr16:
  case Pat_STX_Addr16:
  case Pat_STY_Addr16:
  case Pat_CMP_Imm16:
  case Pat_CMP_Addr16:
  case Pat_BlockMove:
    return 3;
  case Pat_Addr24:
  case Pat_A_Addr24:
  case Pat_A_A_Addr24:
  case Pat_STA_Addr24:
    return 4;
  case Pat_Invalid:
    return 0;
  }
  llvm_unreachable("Unknown operand pattern");
}

// Opcode table - maps W65816 opcode byte to LLVM instruction and pattern
static const OpcodeInfo OpcodeTable[256] = {
    // 0x00-0x0F
    {W65816::BRK, Pat_Imm8},            // 00: BRK
    {0, Pat_Invalid},                   // 01: ORA (dp,X) - not defined
    {W65816::COP, Pat_Imm8},            // 02: COP
    {W65816::ORA_sr, Pat_A_A_Addr8},    // 03: ORA sr,S
    {W65816::TSB_dp, Pat_Addr8},        // 04: TSB dp
    {W65816::ORA_dp, Pat_A_A_Addr8},    // 05: ORA dp
    {W65816::ASL_dp, Pat_Addr8},        // 06: ASL dp
    {0, Pat_Invalid},                   // 07: ORA [dp] - not defined
    {W65816::PHP, Pat_None},            // 08: PHP
    {W65816::ORA_imm16, Pat_A_A_Imm16}, // 09: ORA #imm
    {W65816::ASL_A, Pat_None},          // 0A: ASL A (implicit operands)
    {W65816::PHD, Pat_None},            // 0B: PHD
    {W65816::TSB_abs, Pat_Addr16},      // 0C: TSB abs
    {W65816::ORA_abs, Pat_A_A_Addr16},  // 0D: ORA abs
    {W65816::ASL_abs, Pat_Addr16},      // 0E: ASL abs
    {0, Pat_Invalid},                   // 0F: ORA long - not defined

    // 0x10-0x1F
    {W65816::BPL, Pat_Addr8},       // 10: BPL
    {0, Pat_Invalid},               // 11: ORA (dp),Y - not defined
    {0, Pat_Invalid},               // 12: ORA (dp) - not defined
    {0, Pat_Invalid},               // 13: ORA (sr,S),Y - not defined
    {W65816::TRB_dp, Pat_Addr8},    // 14: TRB dp
    {0, Pat_Invalid},               // 15: ORA dp,X - not defined
    {W65816::ASL_dpX, Pat_Addr8},   // 16: ASL dp,X
    {0, Pat_Invalid},               // 17: ORA [dp],Y - not defined
    {W65816::CLC, Pat_None},        // 18: CLC
    {0, Pat_Invalid},               // 19: ORA abs,Y - not defined
    {W65816::INC_A, Pat_None},      // 1A: INC A (implicit operands)
    {W65816::TCS, Pat_None},        // 1B: TCS
    {W65816::TRB_abs, Pat_Addr16},  // 1C: TRB abs
    {0, Pat_Invalid},               // 1D: ORA abs,X - not defined
    {W65816::ASL_absX, Pat_Addr16}, // 1E: ASL abs,X
    {0, Pat_Invalid},               // 1F: ORA long,X - not defined

    // 0x20-0x2F
    {W65816::JSR, Pat_Addr16},          // 20: JSR abs
    {0, Pat_Invalid},                   // 21: AND (dp,X) - not defined
    {W65816::JSL, Pat_Addr24},          // 22: JSL long
    {W65816::AND_sr, Pat_A_A_Addr8},    // 23: AND sr,S
    {W65816::BIT_dp, Pat_Addr8},        // 24: BIT dp
    {W65816::AND_dp, Pat_A_A_Addr8},    // 25: AND dp
    {W65816::ROL_dp, Pat_Addr8},        // 26: ROL dp
    {0, Pat_Invalid},                   // 27: AND [dp] - not defined
    {W65816::PLP, Pat_None},            // 28: PLP
    {W65816::AND_imm16, Pat_A_A_Imm16}, // 29: AND #imm
    {W65816::ROL_A, Pat_None},          // 2A: ROL A (implicit operands)
    {W65816::PLD, Pat_None},            // 2B: PLD
    {W65816::BIT_abs, Pat_Addr16},      // 2C: BIT abs
    {W65816::AND_abs, Pat_A_A_Addr16},  // 2D: AND abs
    {W65816::ROL_abs, Pat_Addr16},      // 2E: ROL abs
    {0, Pat_Invalid},                   // 2F: AND long - not defined

    // 0x30-0x3F
    {W65816::BMI, Pat_Addr8},       // 30: BMI
    {0, Pat_Invalid},               // 31: AND (dp),Y - not defined
    {0, Pat_Invalid},               // 32: AND (dp) - not defined
    {0, Pat_Invalid},               // 33: AND (sr,S),Y - not defined
    {W65816::BIT_dpX, Pat_Addr8},   // 34: BIT dp,X
    {0, Pat_Invalid},               // 35: AND dp,X - not defined
    {W65816::ROL_dpX, Pat_Addr8},   // 36: ROL dp,X
    {0, Pat_Invalid},               // 37: AND [dp],Y - not defined
    {W65816::SEC, Pat_None},        // 38: SEC
    {0, Pat_Invalid},               // 39: AND abs,Y - not defined
    {W65816::DEC_A, Pat_None},      // 3A: DEC A (implicit operands)
    {W65816::TSC, Pat_None},        // 3B: TSC
    {W65816::BIT_absX, Pat_Addr16}, // 3C: BIT abs,X
    {0, Pat_Invalid},               // 3D: AND abs,X - not defined
    {W65816::ROL_absX, Pat_Addr16}, // 3E: ROL abs,X
    {0, Pat_Invalid},               // 3F: AND long,X - not defined

    // 0x40-0x4F
    {W65816::RTI, Pat_None},            // 40: RTI
    {0, Pat_Invalid},                   // 41: EOR (dp,X) - not defined
    {W65816::WDM, Pat_Imm8},            // 42: WDM
    {W65816::EOR_sr, Pat_A_A_Addr8},    // 43: EOR sr,S
    {W65816::MVP, Pat_BlockMove},       // 44: MVP
    {W65816::EOR_dp, Pat_A_A_Addr8},    // 45: EOR dp
    {W65816::LSR_dp, Pat_Addr8},        // 46: LSR dp
    {0, Pat_Invalid},                   // 47: EOR [dp] - not defined
    {W65816::PHA, Pat_None},            // 48: PHA
    {W65816::EOR_imm16, Pat_A_A_Imm16}, // 49: EOR #imm
    {W65816::LSR_A, Pat_None},          // 4A: LSR A (implicit operands)
    {W65816::PHK, Pat_None},            // 4B: PHK
    {W65816::JMP_abs, Pat_Addr16},      // 4C: JMP abs
    {W65816::EOR_abs, Pat_A_A_Addr16},  // 4D: EOR abs
    {W65816::LSR_abs, Pat_Addr16},      // 4E: LSR abs
    {0, Pat_Invalid},                   // 4F: EOR long - not defined

    // 0x50-0x5F
    {W65816::BVC, Pat_Addr8},       // 50: BVC
    {0, Pat_Invalid},               // 51: EOR (dp),Y - not defined
    {0, Pat_Invalid},               // 52: EOR (dp) - not defined
    {0, Pat_Invalid},               // 53: EOR (sr,S),Y - not defined
    {W65816::MVN, Pat_BlockMove},   // 54: MVN
    {0, Pat_Invalid},               // 55: EOR dp,X - not defined
    {W65816::LSR_dpX, Pat_Addr8},   // 56: LSR dp,X
    {0, Pat_Invalid},               // 57: EOR [dp],Y - not defined
    {W65816::CLI, Pat_None},        // 58: CLI
    {0, Pat_Invalid},               // 59: EOR abs,Y - not defined
    {W65816::PHY, Pat_None},        // 5A: PHY
    {W65816::TCD, Pat_None},        // 5B: TCD
    {W65816::JML, Pat_Addr24},      // 5C: JML long
    {0, Pat_Invalid},               // 5D: EOR abs,X - not defined
    {W65816::LSR_absX, Pat_Addr16}, // 5E: LSR abs,X
    {0, Pat_Invalid},               // 5F: EOR long,X - not defined

    // 0x60-0x6F
    {W65816::RTS, Pat_None},            // 60: RTS
    {0, Pat_Invalid},                   // 61: ADC (dp,X) - not defined
    {W65816::PER, Pat_Addr16},          // 62: PER
    {W65816::ADC_sr, Pat_A_A_Addr8},    // 63: ADC sr,S
    {W65816::STZ_dp, Pat_Addr8},        // 64: STZ dp
    {W65816::ADC_dp, Pat_A_A_Addr8},    // 65: ADC dp
    {W65816::ROR_dp, Pat_Addr8},        // 66: ROR dp
    {0, Pat_Invalid},                   // 67: ADC [dp] - not defined
    {W65816::PLA, Pat_None},            // 68: PLA
    {W65816::ADC_imm16, Pat_A_A_Imm16}, // 69: ADC #imm
    {W65816::ROR_A, Pat_None},          // 6A: ROR A (implicit operands)
    {W65816::RTL, Pat_None},            // 6B: RTL
    {W65816::JMP_ind, Pat_Addr16},      // 6C: JMP (abs)
    {W65816::ADC_abs, Pat_A_A_Addr16},  // 6D: ADC abs
    {W65816::ROR_abs, Pat_Addr16},      // 6E: ROR abs
    {0, Pat_Invalid},                   // 6F: ADC long - not defined

    // 0x70-0x7F
    {W65816::BVS, Pat_Addr8},       // 70: BVS
    {0, Pat_Invalid},               // 71: ADC (dp),Y - not defined
    {0, Pat_Invalid},               // 72: ADC (dp) - not defined
    {0, Pat_Invalid},               // 73: ADC (sr,S),Y - not defined
    {W65816::STZ_dpX, Pat_Addr8},   // 74: STZ dp,X
    {0, Pat_Invalid},               // 75: ADC dp,X - not defined
    {W65816::ROR_dpX, Pat_Addr8},   // 76: ROR dp,X
    {0, Pat_Invalid},               // 77: ADC [dp],Y - not defined
    {W65816::SEI, Pat_None},        // 78: SEI
    {0, Pat_Invalid},               // 79: ADC abs,Y - not defined
    {W65816::PLY, Pat_None},        // 7A: PLY
    {W65816::TDC, Pat_None},        // 7B: TDC
    {W65816::JMP_indX, Pat_Addr16}, // 7C: JMP (abs,X)
    {0, Pat_Invalid},               // 7D: ADC abs,X - not defined
    {W65816::ROR_absX, Pat_Addr16}, // 7E: ROR abs,X
    {0, Pat_Invalid},               // 7F: ADC long,X - not defined

    // 0x80-0x8F
    {W65816::BRA, Pat_Addr8},               // 80: BRA
    {W65816::STA_dpXInd, Pat_STA_Addr8},    // 81: STA (dp,X)
    {W65816::BRL, Pat_Addr16},              // 82: BRL
    {W65816::STA_sr, Pat_STA_Addr8},        // 83: STA sr,S
    {W65816::STY_dp, Pat_STY_Addr8},        // 84: STY dp
    {W65816::STA_dp, Pat_STA_Addr8},        // 85: STA dp
    {W65816::STX_dp, Pat_STX_Addr8},        // 86: STX dp
    {W65816::STA_dpIndLong, Pat_STA_Addr8}, // 87: STA [dp]
    {W65816::DEY, Pat_None},                // 88: DEY
    {W65816::BIT_imm16, Pat_Imm16},         // 89: BIT #imm
    {W65816::TXA, Pat_None},                // 8A: TXA
    {W65816::PHB, Pat_None},                // 8B: PHB
    {W65816::STY_abs, Pat_STY_Addr16},      // 8C: STY abs
    {W65816::STA_abs, Pat_STA_Addr16},      // 8D: STA abs
    {W65816::STX_abs, Pat_STX_Addr16},      // 8E: STX abs
    {W65816::STA_long, Pat_STA_Addr24},     // 8F: STA long

    // 0x90-0x9F
    {W65816::BCC, Pat_Addr8},                // 90: BCC
    {W65816::STA_dpIndY, Pat_STA_Addr8},     // 91: STA (dp),Y
    {W65816::STA_dpInd, Pat_STA_Addr8},      // 92: STA (dp)
    {W65816::STA_srIndY, Pat_STA_Addr8},     // 93: STA (sr,S),Y
    {0, Pat_Invalid},                        // 94: STY dp,X - not defined
    {W65816::STA_dpX, Pat_STA_Addr8},        // 95: STA dp,X
    {0, Pat_Invalid},                        // 96: STX dp,Y - not defined
    {W65816::STA_dpIndLongY, Pat_STA_Addr8}, // 97: STA [dp],Y
    {W65816::TYA, Pat_None},                 // 98: TYA
    {W65816::STA_absY, Pat_STA_Addr16},      // 99: STA abs,Y
    {W65816::TXS, Pat_None},                 // 9A: TXS
    {W65816::TXY, Pat_None},                 // 9B: TXY
    {W65816::STZ_abs, Pat_Addr16},           // 9C: STZ abs
    {W65816::STA_absX, Pat_STA_Addr16},      // 9D: STA abs,X
    {W65816::STZ_absX, Pat_Addr16},          // 9E: STZ abs,X
    {W65816::STA_longX, Pat_STA_Addr24},     // 9F: STA long,X

    // 0xA0-0xAF
    {W65816::LDY_imm16, Pat_Y_Imm16},     // A0: LDY #imm
    {W65816::LDA_dpXInd, Pat_A_Addr8},    // A1: LDA (dp,X)
    {W65816::LDX_imm16, Pat_X_Imm16},     // A2: LDX #imm
    {W65816::LDA_sr, Pat_A_Addr8},        // A3: LDA sr,S
    {0, Pat_Invalid},                     // A4: LDY dp - not defined
    {W65816::LDA_dp, Pat_A_Addr8},        // A5: LDA dp
    {0, Pat_Invalid},                     // A6: LDX dp - not defined
    {W65816::LDA_dpIndLong, Pat_A_Addr8}, // A7: LDA [dp]
    {W65816::TAY, Pat_None},              // A8: TAY
    {W65816::LDA_imm16, Pat_A_Imm16},     // A9: LDA #imm
    {W65816::TAX, Pat_None},              // AA: TAX
    {W65816::PLB, Pat_None},              // AB: PLB
    {W65816::LDY_abs, Pat_Y_Addr16},      // AC: LDY abs
    {W65816::LDA_abs, Pat_A_Addr16},      // AD: LDA abs
    {W65816::LDX_abs, Pat_X_Addr16},      // AE: LDX abs
    {W65816::LDA_long, Pat_A_Addr24},     // AF: LDA long

    // 0xB0-0xBF
    {W65816::BCS, Pat_Addr8},              // B0: BCS
    {W65816::LDA_dpIndY, Pat_A_Addr8},     // B1: LDA (dp),Y
    {W65816::LDA_dpInd, Pat_A_Addr8},      // B2: LDA (dp)
    {W65816::LDA_srIndY, Pat_A_Addr8},     // B3: LDA (sr,S),Y
    {W65816::LDY_dpX, Pat_Y_Addr8},        // B4: LDY dp,X
    {W65816::LDA_dpX, Pat_A_Addr8},        // B5: LDA dp,X
    {W65816::LDX_dpY, Pat_X_Addr8},        // B6: LDX dp,Y
    {W65816::LDA_dpIndLongY, Pat_A_Addr8}, // B7: LDA [dp],Y
    {W65816::CLV, Pat_None},               // B8: CLV
    {W65816::LDA_absY, Pat_A_Addr16},      // B9: LDA abs,Y
    {W65816::TSX, Pat_None},               // BA: TSX
    {W65816::TYX, Pat_None},               // BB: TYX
    {0, Pat_Invalid},                      // BC: LDY abs,X - not defined
    {W65816::LDA_absX, Pat_A_Addr16},      // BD: LDA abs,X
    {0, Pat_Invalid},                      // BE: LDX abs,Y - not defined
    {W65816::LDA_longX, Pat_A_Addr24},     // BF: LDA long,X

    // 0xC0-0xCF
    {W65816::CPY_imm16, Pat_Imm16},     // C0: CPY #imm
    {0, Pat_Invalid},                   // C1: CMP (dp,X) - not defined
    {W65816::REP, Pat_Imm8},            // C2: REP
    {0, Pat_Invalid},                   // C3: CMP sr,S - not defined
    {0, Pat_Invalid},                   // C4: CPY dp - not defined
    {0, Pat_Invalid},                   // C5: CMP dp - not defined
    {W65816::DEC_dp, Pat_Addr8},        // C6: DEC dp
    {0, Pat_Invalid},                   // C7: CMP [dp] - not defined
    {W65816::INY, Pat_None},            // C8: INY
    {W65816::CMP_imm16, Pat_CMP_Imm16}, // C9: CMP #imm
    {W65816::DEX, Pat_None},            // CA: DEX
    {W65816::WAI, Pat_None},            // CB: WAI
    {0, Pat_Invalid},                   // CC: CPY abs - not defined
    {W65816::CMP_abs, Pat_CMP_Addr16},  // CD: CMP abs
    {W65816::DEC_abs, Pat_Addr16},      // CE: DEC abs
    {0, Pat_Invalid},                   // CF: CMP long - not defined

    // 0xD0-0xDF
    {W65816::BNE, Pat_Addr8},       // D0: BNE
    {0, Pat_Invalid},               // D1: CMP (dp),Y - not defined
    {0, Pat_Invalid},               // D2: CMP (dp) - not defined
    {0, Pat_Invalid},               // D3: CMP (sr,S),Y - not defined
    {W65816::PEI, Pat_Addr8},       // D4: PEI
    {0, Pat_Invalid},               // D5: CMP dp,X - not defined
    {W65816::DEC_dpX, Pat_Addr8},   // D6: DEC dp,X
    {0, Pat_Invalid},               // D7: CMP [dp],Y - not defined
    {W65816::CLD, Pat_None},        // D8: CLD
    {0, Pat_Invalid},               // D9: CMP abs,Y - not defined
    {W65816::PHX, Pat_None},        // DA: PHX
    {W65816::STP, Pat_None},        // DB: STP
    {W65816::JMP_indL, Pat_Addr16}, // DC: JML [abs]
    {0, Pat_Invalid},               // DD: CMP abs,X - not defined
    {W65816::DEC_absX, Pat_Addr16}, // DE: DEC abs,X
    {0, Pat_Invalid},               // DF: CMP long,X - not defined

    // 0xE0-0xEF
    {W65816::CPX_imm16, Pat_Imm16},     // E0: CPX #imm
    {0, Pat_Invalid},                   // E1: SBC (dp,X) - not defined
    {W65816::SEP, Pat_Imm8},            // E2: SEP
    {W65816::SBC_sr, Pat_A_A_Addr8},    // E3: SBC sr,S
    {0, Pat_Invalid},                   // E4: CPX dp - not defined
    {W65816::SBC_dp, Pat_A_A_Addr8},    // E5: SBC dp
    {W65816::INC_dp, Pat_Addr8},        // E6: INC dp
    {0, Pat_Invalid},                   // E7: SBC [dp] - not defined
    {W65816::INX, Pat_None},            // E8: INX
    {W65816::SBC_imm16, Pat_A_A_Imm16}, // E9: SBC #imm
    {W65816::NOP, Pat_None},            // EA: NOP
    {W65816::XBA, Pat_None},            // EB: XBA (implicit operands)
    {0, Pat_Invalid},                   // EC: CPX abs - not defined
    {W65816::SBC_abs, Pat_A_A_Addr16},  // ED: SBC abs
    {W65816::INC_abs, Pat_Addr16},      // EE: INC abs
    {0, Pat_Invalid},                   // EF: SBC long - not defined

    // 0xF0-0xFF
    {W65816::BEQ, Pat_Addr8},       // F0: BEQ
    {0, Pat_Invalid},               // F1: SBC (dp),Y - not defined
    {0, Pat_Invalid},               // F2: SBC (dp) - not defined
    {0, Pat_Invalid},               // F3: SBC (sr,S),Y - not defined
    {W65816::PEA, Pat_Addr16},      // F4: PEA
    {0, Pat_Invalid},               // F5: SBC dp,X - not defined
    {W65816::INC_dpX, Pat_Addr8},   // F6: INC dp,X
    {0, Pat_Invalid},               // F7: SBC [dp],Y - not defined
    {W65816::SED, Pat_None},        // F8: SED
    {0, Pat_Invalid},               // F9: SBC abs,Y - not defined
    {W65816::PLX, Pat_None},        // FA: PLX
    {W65816::XCE, Pat_None},        // FB: XCE
    {0, Pat_Invalid},               // FC: JSR (abs,X) - not defined
    {0, Pat_Invalid},               // FD: SBC abs,X - not defined
    {W65816::INC_absX, Pat_Addr16}, // FE: INC abs,X
    {0, Pat_Invalid},               // FF: SBC long,X - not defined
};

class W65816Disassembler : public MCDisassembler {
public:
  W65816Disassembler(const MCSubtargetInfo &STI, MCContext &Ctx)
      : MCDisassembler(STI, Ctx) {}

  DecodeStatus getInstruction(MCInst &MI, uint64_t &Size,
                              ArrayRef<uint8_t> Bytes, uint64_t Address,
                              raw_ostream &CStream) const override;
};

} // end anonymous namespace

static MCDisassembler *createW65816Disassembler(const Target &T,
                                                const MCSubtargetInfo &STI,
                                                MCContext &Ctx) {
  return new W65816Disassembler(STI, Ctx);
}

extern "C" LLVM_ABI LLVM_EXTERNAL_VISIBILITY void
LLVMInitializeW65816Disassembler() {
  TargetRegistry::RegisterMCDisassembler(getTheW65816Target(),
                                         createW65816Disassembler);
}

DecodeStatus W65816Disassembler::getInstruction(MCInst &MI, uint64_t &Size,
                                                ArrayRef<uint8_t> Bytes,
                                                uint64_t Address,
                                                raw_ostream &CStream) const {
  if (Bytes.empty()) {
    Size = 0;
    return MCDisassembler::Fail;
  }

  uint8_t Opcode = Bytes[0];
  const OpcodeInfo &Info = OpcodeTable[Opcode];

  if (Info.Pattern == Pat_Invalid || Info.Opcode == 0) {
    Size = 1;
    return MCDisassembler::Fail;
  }

  unsigned InsnSize = getInstructionSize(Info.Pattern);
  if (Bytes.size() < InsnSize) {
    Size = 0;
    return MCDisassembler::Fail;
  }

  MI.setOpcode(Info.Opcode);

  // Read operand values
  int64_t Imm8 = InsnSize >= 2 ? Bytes[1] : 0;
  int64_t Imm16 = InsnSize >= 3 ? (Bytes[1] | (Bytes[2] << 8)) : 0;
  int64_t Imm24 =
      InsnSize >= 4 ? (Bytes[1] | (Bytes[2] << 8) | (Bytes[3] << 16)) : 0;

  // Add operands based on pattern
  switch (Info.Pattern) {
  case Pat_None:
    // No operands
    break;

  case Pat_Imm8:
  case Pat_Addr8:
    MI.addOperand(MCOperand::createImm(Imm8));
    break;

  case Pat_Imm16:
  case Pat_Addr16:
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_Addr24:
    MI.addOperand(MCOperand::createImm(Imm24));
    break;

  case Pat_A_Imm16:
    MI.addOperand(MCOperand::createReg(W65816::A)); // dst
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_A_A_Imm16:
    MI.addOperand(MCOperand::createReg(W65816::A)); // dst
    MI.addOperand(MCOperand::createReg(W65816::A)); // src
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_A_Addr8:
    MI.addOperand(MCOperand::createReg(W65816::A)); // dst
    MI.addOperand(MCOperand::createImm(Imm8));
    break;

  case Pat_A_A_Addr8:
    MI.addOperand(MCOperand::createReg(W65816::A)); // dst
    MI.addOperand(MCOperand::createReg(W65816::A)); // src
    MI.addOperand(MCOperand::createImm(Imm8));
    break;

  case Pat_A_Addr16:
    MI.addOperand(MCOperand::createReg(W65816::A)); // dst
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_A_A_Addr16:
    MI.addOperand(MCOperand::createReg(W65816::A)); // dst
    MI.addOperand(MCOperand::createReg(W65816::A)); // src
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_A_Addr24:
    MI.addOperand(MCOperand::createReg(W65816::A)); // dst
    MI.addOperand(MCOperand::createImm(Imm24));
    break;

  case Pat_A_A_Addr24:
    MI.addOperand(MCOperand::createReg(W65816::A)); // dst
    MI.addOperand(MCOperand::createReg(W65816::A)); // src
    MI.addOperand(MCOperand::createImm(Imm24));
    break;

  case Pat_X_Imm16:
    MI.addOperand(MCOperand::createReg(W65816::X)); // dst
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_X_Addr8:
    MI.addOperand(MCOperand::createReg(W65816::X)); // dst
    MI.addOperand(MCOperand::createImm(Imm8));
    break;

  case Pat_X_Addr16:
    MI.addOperand(MCOperand::createReg(W65816::X)); // dst
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_Y_Imm16:
    MI.addOperand(MCOperand::createReg(W65816::Y)); // dst
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_Y_Addr8:
    MI.addOperand(MCOperand::createReg(W65816::Y)); // dst
    MI.addOperand(MCOperand::createImm(Imm8));
    break;

  case Pat_Y_Addr16:
    MI.addOperand(MCOperand::createReg(W65816::Y)); // dst
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_STA_Addr8:
    MI.addOperand(MCOperand::createReg(W65816::A)); // src
    MI.addOperand(MCOperand::createImm(Imm8));
    break;

  case Pat_STA_Addr16:
    MI.addOperand(MCOperand::createReg(W65816::A)); // src
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_STA_Addr24:
    MI.addOperand(MCOperand::createReg(W65816::A)); // src
    MI.addOperand(MCOperand::createImm(Imm24));
    break;

  case Pat_STX_Addr8:
    MI.addOperand(MCOperand::createReg(W65816::X)); // src
    MI.addOperand(MCOperand::createImm(Imm8));
    break;

  case Pat_STX_Addr16:
    MI.addOperand(MCOperand::createReg(W65816::X)); // src
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_STY_Addr8:
    MI.addOperand(MCOperand::createReg(W65816::Y)); // src
    MI.addOperand(MCOperand::createImm(Imm8));
    break;

  case Pat_STY_Addr16:
    MI.addOperand(MCOperand::createReg(W65816::Y)); // src
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_CMP_Imm16:
    MI.addOperand(MCOperand::createReg(W65816::A)); // src
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_CMP_Addr16:
    MI.addOperand(MCOperand::createReg(W65816::A)); // src
    MI.addOperand(MCOperand::createImm(Imm16));
    break;

  case Pat_BlockMove:
    // Block move: [opcode][destbank][srcbank]
    // Assembly syntax: MVN srcbank,destbank (order swapped)
    MI.addOperand(MCOperand::createImm(Bytes[2])); // srcbank
    MI.addOperand(MCOperand::createImm(Bytes[1])); // destbank
    break;

  case Pat_Invalid:
    Size = 1;
    return MCDisassembler::Fail;
  }

  Size = InsnSize;
  return MCDisassembler::Success;
}
