//===-- W65816FixupKinds.h - W65816 Specific Fixup Entries ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816FIXUPKINDS_H
#define LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816FIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace W65816 {

/// The set of supported fixups for the W65816.
enum Fixups {
  /// A 16-bit absolute address fixup.
  fixup_w65816_16 = FirstTargetFixupKind,

  /// A 24-bit absolute long address fixup.
  fixup_w65816_24,

  /// An 8-bit PC-relative fixup for short branches.
  /// Branch range is -128 to +127 bytes from instruction.
  fixup_w65816_pcrel_8,

  /// A 16-bit PC-relative fixup for BRL (branch long).
  /// Branch range is -32768 to +32767 bytes from instruction.
  fixup_w65816_pcrel_16,

  /// An 8-bit direct page address fixup.
  fixup_w65816_dp,

  /// An 8-bit immediate value fixup.
  fixup_w65816_imm8,

  /// A 16-bit immediate value fixup.
  fixup_w65816_imm16,

  /// Marker for the end of target-specific fixups.
  LastTargetFixupKind,
  NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
};

} // namespace W65816
} // namespace llvm

#endif // LLVM_LIB_TARGET_W65816_MCTARGETDESC_W65816FIXUPKINDS_H
