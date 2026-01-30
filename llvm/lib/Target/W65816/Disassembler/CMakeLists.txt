add_llvm_component_library(LLVMW65816Disassembler
  W65816Disassembler.cpp

  LINK_COMPONENTS
  MCDisassembler
  MC
  W65816Info
  Support

  ADD_TO_COMPONENT
  W65816
  )
