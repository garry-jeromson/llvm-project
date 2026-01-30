add_llvm_component_group(W65816)

set(LLVM_TARGET_DEFINITIONS W65816.td)

tablegen(LLVM W65816GenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM W65816GenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM W65816GenCallingConv.inc -gen-callingconv)
tablegen(LLVM W65816GenDAGISel.inc -gen-dag-isel)
tablegen(LLVM W65816GenInstrInfo.inc -gen-instr-info)
tablegen(LLVM W65816GenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM W65816GenRegisterInfo.inc -gen-register-info)
tablegen(LLVM W65816GenSDNodeInfo.inc -gen-sd-node-info)
tablegen(LLVM W65816GenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(W65816CommonTableGen)

add_llvm_target(W65816CodeGen
  W65816AsmPrinter.cpp
  W65816ExpandPseudo.cpp
  W65816FrameLowering.cpp
  W65816InstrInfo.cpp
  W65816ISelDAGToDAG.cpp
  W65816ISelLowering.cpp
  W65816MCInstLower.cpp
  W65816PeepholeOpt.cpp
  W65816RegisterInfo.cpp
  W65816Subtarget.cpp
  W65816TargetMachine.cpp
  W65816TargetObjectFile.cpp

  DEPENDS
  intrinsics_gen

  LINK_COMPONENTS
  Analysis
  AsmPrinter
  CodeGen
  CodeGenTypes
  Core
  MC
  SelectionDAG
  Support
  Target
  TargetParser
  TransformUtils
  W65816Desc
  W65816Info

  ADD_TO_COMPONENT
  W65816
  )

add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
