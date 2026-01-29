//===-- W65816.td - Describe the W65816 Target Machine -----*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// This is the top level entry point for the W65816 target.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces which we are implementing
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// W65816 Subtarget Features
//===----------------------------------------------------------------------===//

def FeatureLongAddressing
    : SubtargetFeature<"long-addr", "HasLongAddressing", "true",
                       "Enable 24-bit long addressing">;

def FeatureSNES
    : SubtargetFeature<"snes", "IsSNES", "true",
                       "Target is Super Nintendo Entertainment System">;

// 8-bit mode features (M and X flags in status register)
// These allow compile-time selection of accumulator and index register width
def FeatureAcc8Bit
    : SubtargetFeature<"acc8bit", "UseAcc8Bit", "true",
                       "Use 8-bit accumulator (M=1)">;

def FeatureIdx8Bit
    : SubtargetFeature<"idx8bit", "UseIdx8Bit", "true",
                       "Use 8-bit index registers (X=1)">;

//===----------------------------------------------------------------------===//
// W65816 Processors
//===----------------------------------------------------------------------===//

class Proc<string Name, list<SubtargetFeature> Features>
    : Processor<Name, NoItineraries, Features>;

// Default: 16-bit accumulator and index registers (M=0, X=0)
def : Proc<"w65816", []>;

// 8-bit accumulator, 16-bit index (M=1, X=0)
def : Proc<"w65816-m8", [FeatureAcc8Bit]>;

// 16-bit accumulator, 8-bit index (M=0, X=1)
def : Proc<"w65816-x8", [FeatureIdx8Bit]>;

// Both 8-bit (M=1, X=1)
def : Proc<"w65816-mx8", [FeatureAcc8Bit, FeatureIdx8Bit]>;

// SNES target with long addressing
def : Proc<"snes", [FeatureSNES, FeatureLongAddressing]>;

//===----------------------------------------------------------------------===//
// Register File Description
//===----------------------------------------------------------------------===//

include "W65816RegisterInfo.td"

//===----------------------------------------------------------------------===//
// Calling Convention Description
//===----------------------------------------------------------------------===//

include "W65816CallingConv.td"

//===----------------------------------------------------------------------===//
// Instruction Descriptions
//===----------------------------------------------------------------------===//

include "W65816InstrInfo.td"

defm : RemapAllTargetPseudoPointerOperands<GPR16>;

def W65816InstrInfo : InstrInfo;

//===----------------------------------------------------------------------===//
// Assembly Printers
//===----------------------------------------------------------------------===//

def W65816AsmWriter : AsmWriter {
  string AsmWriterClassName = "InstPrinter";
  bit isMCAsmWriter = 1;
}

//===----------------------------------------------------------------------===//
// Target Declaration
//===----------------------------------------------------------------------===//

def W65816 : Target {
  let InstructionSet = W65816InstrInfo;
  let AssemblyWriters = [W65816AsmWriter];
}
