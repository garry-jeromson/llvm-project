add_llvm_component_library(LLVMW65816Desc
  W65816AsmBackend.cpp
  W65816ELFObjectWriter.cpp
  W65816InstPrinter.cpp
  W65816MCAsmInfo.cpp
  W65816MCCodeEmitter.cpp
  W65816MCTargetDesc.cpp

  LINK_COMPONENTS
  MC
  W65816Info
  Support
  TargetParser

  ADD_TO_COMPONENT
  W65816
)

add_dependencies(LLVMW65816Desc W65816CommonTableGen)
