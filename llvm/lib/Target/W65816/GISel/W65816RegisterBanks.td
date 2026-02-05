//===-- W65816RegisterBanks.td - W65816 Banks ---------------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Define the W65816 register banks used for GlobalISel.
//
//===----------------------------------------------------------------------===//

/// General Purpose Register bank covering all W65816 registers.
/// Includes GPR16 (A, X, Y, RS0-RS15), ALL16 (A, X, Y, SP, D),
/// STATUS (P, DBR, PBR), and single-register classes (SPREG, DPREG).
/// All classes must be covered to prevent crashes when RegBankSelect
/// encounters implicit physical register defs/uses on instructions like JSR.
def GPRRegBank : RegisterBank<"GPR", [GPR16, ALL16, STATUS, SPREG, DPREG]>;
