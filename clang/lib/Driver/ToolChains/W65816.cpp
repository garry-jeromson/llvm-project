//===--- W65816.cpp - W65816 Helpers for Tools ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "W65816.h"
#include "clang/Basic/DiagnosticDriver.h"
#include "clang/Driver/CommonArgs.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/InputInfo.h"
#include "clang/Options/Options.h"
#include "llvm/Option/ArgList.h"

using namespace clang::driver;
using namespace clang::driver::toolchains;
using namespace clang::driver::tools;
using namespace clang;
using namespace llvm::opt;

/// W65816 Toolchain
W65816ToolChain::W65816ToolChain(const Driver &D, const llvm::Triple &Triple,
                                 const ArgList &Args)
    : Generic_ELF(D, Triple, Args) {}

void W65816ToolChain::addClangTargetOptions(
    const llvm::opt::ArgList &DriverArgs, llvm::opt::ArgStringList &CC1Args,
    Action::OffloadKind DeviceOffloadKind) const {
  // The W65816 has only 3 physical registers (A, X, Y). Code compiled without
  // optimization (-O0) may have correctness issues due to value clobbering
  // when multiple values need to flow through the accumulator.
  const Driver &D = getDriver();

  if (Arg *A = DriverArgs.getLastArg(options::OPT_O_Group)) {
    // -O0 is allowed but with a warning about potential issues
    if (A->getOption().matches(options::OPT_O0)) {
      D.Diag(diag::warn_drv_unsupported_opt_for_target)
          << "-O0" << getTriple().str();
    }
  } else {
    // No optimization flag specified - default to -O1
    CC1Args.push_back("-O1");
  }
}

Tool *W65816ToolChain::buildLinker() const {
  return new tools::w65816::Linker(*this);
}

void w65816::Linker::ConstructJob(Compilation &C, const JobAction &JA,
                                  const InputInfo &Output,
                                  const InputInfoList &Inputs,
                                  const ArgList &Args,
                                  const char *LinkingOutput) const {
  const ToolChain &ToolChain = getToolChain();
  std::string Linker = ToolChain.GetProgramPath(getShortName());
  ArgStringList CmdArgs;

  Args.AddAllArgs(CmdArgs, options::OPT_L);
  Args.AddAllArgs(CmdArgs, options::OPT_T);

  AddLinkerInputs(ToolChain, Inputs, Args, CmdArgs, JA);

  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  C.addCommand(std::make_unique<Command>(
      JA, *this, ResponseFileSupport::AtFileCurCP(), Args.MakeArgString(Linker),
      CmdArgs, Inputs, Output));
}
