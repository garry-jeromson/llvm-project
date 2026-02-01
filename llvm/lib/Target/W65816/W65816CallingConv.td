//===-- W65816CallingConv.td - Calling Conventions ---------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// This describes the calling conventions for W65816 architecture.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// W65816 Return Value Calling Convention
//===----------------------------------------------------------------------===//

def RetCC_W65816 : CallingConv<[
  // i8 values are returned in AL (low byte of accumulator)
  CCIfType<[i8], CCAssignToReg<[AL]>>,

  // i16 values are returned in A (full accumulator)
  CCIfType<[i16], CCAssignToReg<[A]>>,

  // i32 values are returned in A:X (A=low word, X=high word)
  CCIfType<[i32], CCAssignToRegWithShadow<[A, X], [A, X]>>
]>;

//===----------------------------------------------------------------------===//
// W65816 Argument Calling Conventions
//===----------------------------------------------------------------------===//

// Used for both regular and variadic functions.
// For varargs, fixed args use registers as normal; variadic args go on stack.
def CC_W65816 : CallingConv<[
  // First i8 argument goes in AL
  CCIfType<[i8], CCAssignToReg<[AL, XL, YL]>>,

  // First three i16 arguments go in A, X, Y
  CCIfType<[i16], CCAssignToReg<[A, X, Y]>>,

  // Remaining arguments are passed on the stack, 2-byte aligned
  CCIfType<[i8], CCAssignToStack<1, 1>>,
  CCIfType<[i16], CCAssignToStack<2, 2>>,
  CCIfType<[i32], CCAssignToStack<4, 2>>
]>;

//===----------------------------------------------------------------------===//
// Callee-saved register lists
//===----------------------------------------------------------------------===//

// D register is callee-saved to enable direct page optimization
// A, X, Y are caller-saved
def CSR_W65816 : CalleeSavedRegs<(add D)>;
