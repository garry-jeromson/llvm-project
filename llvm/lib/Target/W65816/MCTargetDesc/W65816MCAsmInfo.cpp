//===-- W65816MCAsmInfo.cpp - W65816 Asm Properties -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the W65816MCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "W65816MCAsmInfo.h"

#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/TargetParser/Triple.h"

using namespace llvm;

W65816MCAsmInfo::W65816MCAsmInfo(const Triple &TT,
                                 const MCTargetOptions &Options) {
  CodePointerSize = 2;     // 16-bit pointers (24-bit with bank)
  CalleeSaveStackSlotSize = 2;
  CommentString = ";";
  PrivateGlobalPrefix = ".L";
  PrivateLabelPrefix = ".L";
  UsesELFSectionDirectiveForBSS = true;
  UseIntegratedAssembler = true;
  SupportsDebugInformation = true;

  // W65816 is little-endian
  IsLittleEndian = true;

  // Data directives
  Data8bitsDirective = "\t.byte\t";
  Data16bitsDirective = "\t.word\t";
  Data32bitsDirective = "\t.long\t";
  Data64bitsDirective = nullptr;  // 65816 doesn't support 64-bit natively

  // Zero directive for BSS
  ZeroDirective = "\t.zero\t";

  // ASCII/ASCIZ directives
  AsciiDirective = "\t.ascii\t";
  AscizDirective = "\t.asciz\t";

  // Alignment
  AlignmentIsInBytes = false;

  // Global/weak symbol directives
  GlobalDirective = "\t.global\t";
  WeakDirective = "\t.weak\t";

  // Label suffix
  LabelSuffix = ":";

  // Maximum instruction length (4 bytes for long addressing mode)
  MaxInstLength = 4;
}
