add_llvm_component_group(W65816)

set(LLVM_TARGET_DEFINITIONS W65816.td)

tablegen(LLVM W65816GenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM W65816GenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM W65816GenCallingConv.inc -gen-callingconv)
tablegen(LLVM W65816GenInstrInfo.inc -gen-instr-info)
tablegen(LLVM W65816GenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM W65816GenRegisterInfo.inc -gen-register-info)
tablegen(LLVM W65816GenSubtargetInfo.inc -gen-subtarget)

set(LLVM_TARGET_DEFINITIONS W65816GISel.td)
tablegen(LLVM W65816GenGlobalISel.inc -gen-global-isel)
tablegen(LLVM W65816GenRegisterBank.inc -gen-register-bank)
tablegen(LLVM W65816GenPreLegalizeGICombiner.inc -gen-global-isel-combiner
              -combiners="W65816PreLegalizerCombiner")
tablegen(LLVM W65816GenPostLegalizeGICombiner.inc -gen-global-isel-combiner
              -combiners="W65816PostLegalizerCombiner")

add_public_tablegen_target(W65816CommonTableGen)

add_llvm_target(W65816CodeGen
  GISel/W65816CallLowering.cpp
  GISel/W65816InstructionSelector.cpp
  GISel/W65816LegalizerInfo.cpp
  GISel/W65816PostLegalizerCombiner.cpp
  GISel/W65816PreLegalizerCombiner.cpp
  GISel/W65816RegisterBankInfo.cpp
  W65816AsmPrinter.cpp
  W65816ExpandPseudo.cpp
  W65816FrameLowering.cpp
  W65816InstrInfo.cpp
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
  GlobalISel
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
