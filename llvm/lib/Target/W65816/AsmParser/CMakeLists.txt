add_llvm_component_library(LLVMW65816AsmParser
  W65816AsmParser.cpp

  LINK_COMPONENTS
  W65816Desc
  W65816Info
  MC
  MCParser
  Support

  ADD_TO_COMPONENT
  W65816
  )
