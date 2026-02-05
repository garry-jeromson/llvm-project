//=- W65816Combine.td - Define W65816 Combine Rules -------*- tablegen -*-=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

include "llvm/Target/GlobalISel/Combine.td"

// Fold (sub 0, (sub 0, x)) -> x (integer double negation identity)
// The generic combines include fneg_fneg_fold for floating-point but there is
// no integer equivalent. On the W65816 with only 3 registers, eliminating
// unnecessary negation pairs is important.
def double_neg_fold: GICombineRule<
  (defs root:$dst),
  (match (G_CONSTANT $zero1, 0),
         (G_SUB $inner, $zero1, $src),
         (G_CONSTANT $zero2, 0),
         (G_SUB $dst, $zero2, $inner)),
  (apply (GIReplaceReg $dst, $src))
>;

// Note: zext_trunc_fold (G_ZEXT(G_TRUNC(x)) → x when bits known zero) is
// already included in all_combines. No custom rule needed.

def W65816PreLegalizerCombiner: GICombiner<
  "W65816PreLegalizerCombinerImpl", [all_combines, double_neg_fold]> {
}

def W65816PostLegalizerCombiner: GICombiner<
  "W65816PostLegalizerCombinerImpl", [all_combines]> {
}
