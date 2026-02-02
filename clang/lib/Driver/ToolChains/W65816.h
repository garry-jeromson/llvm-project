//===--- W65816.h - W65816-specific Tool Helpers ----------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_W65816_H
#define LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_W65816_H

#include "Gnu.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/ToolChain.h"

namespace clang {
namespace driver {
namespace toolchains {

class LLVM_LIBRARY_VISIBILITY W65816ToolChain : public Generic_ELF {
public:
  W65816ToolChain(const Driver &D, const llvm::Triple &Triple,
                  const llvm::opt::ArgList &Args);

  bool isPICDefault() const override { return false; }
  bool isPIEDefault(const llvm::opt::ArgList &Args) const override {
    return false;
  }
  bool isPICDefaultForced() const override { return true; }

  UnwindLibType
  GetUnwindLibType(const llvm::opt::ArgList &Args) const override {
    return UNW_None;
  }

  void
  addClangTargetOptions(const llvm::opt::ArgList &DriverArgs,
                        llvm::opt::ArgStringList &CC1Args,
                        Action::OffloadKind DeviceOffloadKind) const override;

protected:
  Tool *buildLinker() const override;
};

} // end namespace toolchains

namespace tools {
namespace w65816 {

class LLVM_LIBRARY_VISIBILITY Linker final : public Tool {
public:
  Linker(const ToolChain &TC) : Tool("W65816::Linker", "ld.lld", TC) {}
  bool hasIntegratedCPP() const override { return false; }
  bool isLinkJob() const override { return true; }
  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output, const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override;
};

} // end namespace w65816
} // end namespace tools
} // end namespace driver
} // end namespace clang

#endif // LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_W65816_H
