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

//===----------------------------------------------------------------------===//
// W65816 Processors
//===----------------------------------------------------------------------===//

class Proc<string Name, list<SubtargetFeature> Features>
    : Processor<Name, NoItineraries, Features>;

def : Proc<"w65816", []>;
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
