# this file was made with cpp or `gcc -E/g++ -E` and c2nim, some of complex code are wriiten by hand

proc initializeAArch64TargetInfo*() {.importc: "LLVMInitializeAArch64TargetInfo".}
proc initializeAMDGPUTargetInfo*() {.importc: "LLVMInitializeAMDGPUTargetInfo".}
proc initializeARMTargetInfo*() {.importc: "LLVMInitializeARMTargetInfo".}
proc initializeBPFTargetInfo*() {.importc: "LLVMInitializeBPFTargetInfo".}
proc initializeHexagonTargetInfo*() {.importc: "LLVMInitializeHexagonTargetInfo".}
proc initializeLanaiTargetInfo*() {.importc: "LLVMInitializeLanaiTargetInfo".}
proc initializeMipsTargetInfo*() {.importc: "LLVMInitializeMipsTargetInfo".}
proc initializeMSP430TargetInfo*() {.importc: "LLVMInitializeMSP430TargetInfo".}
proc initializeNVPTXTargetInfo*() {.importc: "LLVMInitializeNVPTXTargetInfo".}
proc initializePowerPCTargetInfo*() {.importc: "LLVMInitializePowerPCTargetInfo".}
proc initializeRISCVTargetInfo*() {.importc: "LLVMInitializeRISCVTargetInfo".}
proc initializeSparcTargetInfo*() {.importc: "LLVMInitializeSparcTargetInfo".}
proc initializeSystemZTargetInfo*() {.importc: "LLVMInitializeSystemZTargetInfo".}
proc initializeWebAssemblyTargetInfo*() {.importc: "LLVMInitializeWebAssemblyTargetInfo".}
proc initializeX86TargetInfo*() {.importc: "LLVMInitializeX86TargetInfo".}
proc initializeXCoreTargetInfo*() {.importc: "LLVMInitializeXCoreTargetInfo".}
proc initializeAVRTargetInfo*() {.importc: "LLVMInitializeAVRTargetInfo".}
proc initializeAArch64Target*() {.importc: "LLVMInitializeAArch64Target".}
proc initializeAMDGPUTarget*() {.importc: "LLVMInitializeAMDGPUTarget".}
proc initializeARMTarget*() {.importc: "LLVMInitializeARMTarget".}
proc initializeBPFTarget*() {.importc: "LLVMInitializeBPFTarget".}
proc initializeHexagonTarget*() {.importc: "LLVMInitializeHexagonTarget".}
proc initializeLanaiTarget*() {.importc: "LLVMInitializeLanaiTarget".}
proc initializeMipsTarget*() {.importc: "LLVMInitializeMipsTarget".}
proc initializeMSP430Target*() {.importc: "LLVMInitializeMSP430Target".}
proc initializeNVPTXTarget*() {.importc: "LLVMInitializeNVPTXTarget".}
proc initializePowerPCTarget*() {.importc: "LLVMInitializePowerPCTarget".}
proc initializeRISCVTarget*() {.importc: "LLVMInitializeRISCVTarget".}
proc initializeSparcTarget*() {.importc: "LLVMInitializeSparcTarget".}
proc initializeSystemZTarget*() {.importc: "LLVMInitializeSystemZTarget".}
proc initializeWebAssemblyTarget*() {.importc: "LLVMInitializeWebAssemblyTarget".}
proc initializeX86Target*() {.importc: "LLVMInitializeX86Target".}
proc initializeXCoreTarget*() {.importc: "LLVMInitializeXCoreTarget".}
proc initializeAVRTarget*() {.importc: "LLVMInitializeAVRTarget".}
proc initializeAArch64TargetMC*() {.importc: "LLVMInitializeAArch64TargetMC".}
proc initializeAMDGPUTargetMC*() {.importc: "LLVMInitializeAMDGPUTargetMC".}
proc initializeARMTargetMC*() {.importc: "LLVMInitializeARMTargetMC".}
proc initializeBPFTargetMC*() {.importc: "LLVMInitializeBPFTargetMC".}
proc initializeHexagonTargetMC*() {.importc: "LLVMInitializeHexagonTargetMC".}
proc initializeLanaiTargetMC*() {.importc: "LLVMInitializeLanaiTargetMC".}
proc initializeMipsTargetMC*() {.importc: "LLVMInitializeMipsTargetMC".}
proc initializeMSP430TargetMC*() {.importc: "LLVMInitializeMSP430TargetMC".}
proc initializeNVPTXTargetMC*() {.importc: "LLVMInitializeNVPTXTargetMC".}
proc initializePowerPCTargetMC*() {.importc: "LLVMInitializePowerPCTargetMC".}
proc initializeRISCVTargetMC*() {.importc: "LLVMInitializeRISCVTargetMC".}
proc initializeSparcTargetMC*() {.importc: "LLVMInitializeSparcTargetMC".}
proc initializeSystemZTargetMC*() {.importc: "LLVMInitializeSystemZTargetMC".}
proc initializeWebAssemblyTargetMC*() {.importc: "LLVMInitializeWebAssemblyTargetMC".}
proc initializeX86TargetMC*() {.importc: "LLVMInitializeX86TargetMC".}
proc initializeXCoreTargetMC*() {.importc: "LLVMInitializeXCoreTargetMC".}
proc initializeAVRTargetMC*() {.importc: "LLVMInitializeAVRTargetMC".}
proc initializeAArch64AsmPrinter*() {.importc: "LLVMInitializeAArch64AsmPrinter".}
proc initializeAMDGPUAsmPrinter*() {.importc: "LLVMInitializeAMDGPUAsmPrinter".}
proc initializeARMAsmPrinter*() {.importc: "LLVMInitializeARMAsmPrinter".}
proc initializeBPFAsmPrinter*() {.importc: "LLVMInitializeBPFAsmPrinter".}
proc initializeHexagonAsmPrinter*() {.importc: "LLVMInitializeHexagonAsmPrinter".}
proc initializeLanaiAsmPrinter*() {.importc: "LLVMInitializeLanaiAsmPrinter".}
proc initializeMipsAsmPrinter*() {.importc: "LLVMInitializeMipsAsmPrinter".}
proc initializeMSP430AsmPrinter*() {.importc: "LLVMInitializeMSP430AsmPrinter".}
proc initializeNVPTXAsmPrinter*() {.importc: "LLVMInitializeNVPTXAsmPrinter".}
proc initializePowerPCAsmPrinter*() {.importc: "LLVMInitializePowerPCAsmPrinter".}
proc initializeRISCVAsmPrinter*() {.importc: "LLVMInitializeRISCVAsmPrinter".}
proc initializeSparcAsmPrinter*() {.importc: "LLVMInitializeSparcAsmPrinter".}
proc initializeSystemZAsmPrinter*() {.importc: "LLVMInitializeSystemZAsmPrinter".}
proc initializeWebAssemblyAsmPrinter*() {.importc: "LLVMInitializeWebAssemblyAsmPrinter".}
proc initializeX86AsmPrinter*() {.importc: "LLVMInitializeX86AsmPrinter".}
proc initializeXCoreAsmPrinter*() {.importc: "LLVMInitializeXCoreAsmPrinter".}
proc initializeAVRAsmPrinter*() {.importc: "LLVMInitializeAVRAsmPrinter".}
proc initializeAArch64AsmParser*() {.importc: "LLVMInitializeAArch64AsmParser".}
proc initializeAMDGPUAsmParser*() {.importc: "LLVMInitializeAMDGPUAsmParser".}
proc initializeARMAsmParser*() {.importc: "LLVMInitializeARMAsmParser".}
proc initializeBPFAsmParser*() {.importc: "LLVMInitializeBPFAsmParser".}
proc initializeHexagonAsmParser*() {.importc: "LLVMInitializeHexagonAsmParser".}
proc initializeLanaiAsmParser*() {.importc: "LLVMInitializeLanaiAsmParser".}
proc initializeMipsAsmParser*() {.importc: "LLVMInitializeMipsAsmParser".}
proc initializeMSP430AsmParser*() {.importc: "LLVMInitializeMSP430AsmParser".}
proc initializePowerPCAsmParser*() {.importc: "LLVMInitializePowerPCAsmParser".}
proc initializeRISCVAsmParser*() {.importc: "LLVMInitializeRISCVAsmParser".}
proc initializeSparcAsmParser*() {.importc: "LLVMInitializeSparcAsmParser".}
proc initializeSystemZAsmParser*() {.importc: "LLVMInitializeSystemZAsmParser".}
proc initializeWebAssemblyAsmParser*() {.importc: "LLVMInitializeWebAssemblyAsmParser".}
proc initializeX86AsmParser*() {.importc: "LLVMInitializeX86AsmParser".}
proc initializeAVRAsmParser*() {.importc: "LLVMInitializeAVRAsmParser".}
proc initializeAArch64Disassembler*() {.importc: "LLVMInitializeAArch64Disassembler".}
proc initializeAMDGPUDisassembler*() {.importc: "LLVMInitializeAMDGPUDisassembler".}
proc initializeARMDisassembler*() {.importc: "LLVMInitializeARMDisassembler".}
proc initializeBPFDisassembler*() {.importc: "LLVMInitializeBPFDisassembler".}
proc initializeHexagonDisassembler*() {.importc: "LLVMInitializeHexagonDisassembler".}
proc initializeLanaiDisassembler*() {.importc: "LLVMInitializeLanaiDisassembler".}
proc initializeMipsDisassembler*() {.importc: "LLVMInitializeMipsDisassembler".}
proc initializeMSP430Disassembler*() {.importc: "LLVMInitializeMSP430Disassembler".}
proc initializePowerPCDisassembler*() {.importc: "LLVMInitializePowerPCDisassembler".}
proc initializeRISCVDisassembler*() {.importc: "LLVMInitializeRISCVDisassembler".}
proc initializeSparcDisassembler*() {.importc: "LLVMInitializeSparcDisassembler".}
proc initializeSystemZDisassembler*() {.importc: "LLVMInitializeSystemZDisassembler".}
proc initializeWebAssemblyDisassembler*() {.
    importc: "LLVMInitializeWebAssemblyDisassembler".}
proc initializeX86Disassembler*() {.importc: "LLVMInitializeX86Disassembler".}
proc initializeXCoreDisassembler*() {.importc: "LLVMInitializeXCoreDisassembler".}
proc initializeAVRDisassembler*() {.importc: "LLVMInitializeAVRDisassembler".}

proc initializeAllTargetInfos*() =
     initializeAArch64TargetInfo()
     initializeAMDGPUTargetInfo()
     initializeARMTargetInfo()
     initializeBPFTargetInfo()
     initializeHexagonTargetInfo()
     initializeLanaiTargetInfo()
     initializeMipsTargetInfo()
     initializeMSP430TargetInfo()
     initializeNVPTXTargetInfo()
     initializePowerPCTargetInfo()
     initializeRISCVTargetInfo()
     initializeSparcTargetInfo()
     initializeSystemZTargetInfo()
     initializeWebAssemblyTargetInfo()
     initializeX86TargetInfo()
     initializeXCoreTargetInfo()
     initializeAVRTargetInfo()

proc initializeAllTargets( ) =
  initializeAArch64Target() 
  initializeAMDGPUTarget() 
  initializeARMTarget() 
  initializeBPFTarget() 
  initializeHexagonTarget() 
  initializeLanaiTarget() 
  initializeMipsTarget() 
  initializeMSP430Target() 
  initializeNVPTXTarget()
  initializePowerPCTarget() 
  initializeRISCVTarget() 
  initializeSparcTarget() 
  initializeSystemZTarget() 
  initializeWebAssemblyTarget() 
  initializeX86Target() 
  initializeXCoreTarget() 
  initializeAVRTarget() 

proc initializeAllTargetMCs*() =
  initializeAArch64TargetMC() 
  initializeAMDGPUTargetMC() 
  initializeARMTargetMC() 
  initializeBPFTargetMC() 
  initializeHexagonTargetMC() 
  initializeLanaiTargetMC() 
  initializeMipsTargetMC() 
  initializeMSP430TargetMC() 
  initializeNVPTXTargetMC() 
  initializePowerPCTargetMC() 
  initializeRISCVTargetMC() 
  initializeSparcTargetMC() 
  initializeSystemZTargetMC() 
  initializeWebAssemblyTargetMC() 
  initializeX86TargetMC() 
  initializeXCoreTargetMC() 
  initializeAVRTargetMC() 

proc initializeAllAsmPrinters() =
  initializeAArch64AsmPrinter() 
  initializeAMDGPUAsmPrinter() 
  initializeARMAsmPrinter() 
  initializeBPFAsmPrinter() 
  initializeHexagonAsmPrinter() 
  initializeLanaiAsmPrinter() 
  initializeMipsAsmPrinter() 
  initializeMSP430AsmPrinter() 
  initializeNVPTXAsmPrinter() 
  initializePowerPCAsmPrinter() 
  initializeRISCVAsmPrinter() 
  initializeSparcAsmPrinter() 
  initializeSystemZAsmPrinter() 
  initializeWebAssemblyAsmPrinter() 
  initializeX86AsmPrinter() 
  initializeXCoreAsmPrinter() 
  initializeAVRAsmPrinter() 

proc initializeAllAsmParsers*() =
  initializeAArch64AsmParser() 
  initializeAMDGPUAsmParser() 
  initializeARMAsmParser() 
  initializeBPFAsmParser()
  initializeHexagonAsmParser() 
  initializeLanaiAsmParser() 
  initializeMipsAsmParser() 
  initializePowerPCAsmParser() 
  initializeRISCVAsmParser() 
  initializeSparcAsmParser() 
  initializeSystemZAsmParser() 
  initializeMSP430AsmParser() 
  initializeWebAssemblyAsmParser() 
  initializeX86AsmParser() 
  initializeAVRAsmParser() 

proc LLVMInitializeAllDisassemblers*() =
  initializeAArch64Disassembler() 
  initializeAMDGPUDisassembler() 
  initializeARMDisassembler() 
  initializeBPFDisassembler() 
  initializeHexagonDisassembler() 
  initializeLanaiDisassembler() 
  initializeMipsDisassembler() 
  initializeMSP430Disassembler() 
  initializePowerPCDisassembler() 
  initializeRISCVDisassembler() 
  initializeSparcDisassembler() 
  initializeSystemZDisassembler() 
  initializeWebAssemblyDisassembler() 
  initializeX86Disassembler() 
  initializeXCoreDisassembler() 
  initializeAVRDisassembler()

proc initializeNativeTarget*() = 
  initializeX86TargetInfo()
  initializeX86Target() 
  initializeX86TargetMC()

proc initializeNativeAsmParser*() =
  initializeX86AsmParser()

proc initializeNativeAsmPrinter*() = 
  initializeX86AsmPrinter()

proc initializeNativeDisassembler*() =
  initializeX86Disassembler()

proc setModuleDataLayout*(m: ModuleRef; dl: TargetDataRef) {.
    importc: "LLVMSetModuleDataLayout".}
proc createTargetData*(stringRep: cstring): TargetDataRef {.
    importc: "LLVMCreateTargetData".}
proc disposeTargetData*(td: TargetDataRef) {.importc: "LLVMDisposeTargetData",
.}
proc addTargetLibraryInfo*(tli: TargetLibraryInfoRef; pm: PassManagerRef) {.
    importc: "LLVMAddTargetLibraryInfo".}
proc copyStringRepOfTargetData*(td: TargetDataRef): cstring {.
    importc: "LLVMCopyStringRepOfTargetData".}
proc byteOrder*(td: TargetDataRef): ByteOrdering {.importc: "LLVMByteOrder",
.}
proc pointerSize*(td: TargetDataRef): cuint {.importc: "LLVMPointerSize",
.}
proc pointerSizeForAS*(td: TargetDataRef; `as`: cuint): cuint {.
    importc: "LLVMPointerSizeForAS".}
proc intPtrType*(td: TargetDataRef): TypeRef {.importc: "LLVMIntPtrType",
.}
proc intPtrTypeForAS*(td: TargetDataRef; `as`: cuint): TypeRef {.
    importc: "LLVMIntPtrTypeForAS".}
proc intPtrTypeInContext*(c: ContextRef; td: TargetDataRef): TypeRef {.
    importc: "LLVMIntPtrTypeInContext".}
proc intPtrTypeForASInContext*(c: ContextRef; td: TargetDataRef; `as`: cuint): TypeRef {.
    importc: "LLVMIntPtrTypeForASInContext".}
proc sizeOfTypeInBits*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMSizeOfTypeInBits".}
proc storeSizeOfType*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMStoreSizeOfType".}
proc aBISizeOfType*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMABISizeOfType".}
proc aBIAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMABIAlignmentOfType".}
proc callFrameAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMCallFrameAlignmentOfType".}
proc preferredAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMPreferredAlignmentOfType".}
proc preferredAlignmentOfGlobal*(td: TargetDataRef; globalVar: ValueRef): cuint {.
    importc: "LLVMPreferredAlignmentOfGlobal".}
proc elementAtOffset*(td: TargetDataRef; structTy: TypeRef; offset: culonglong): cuint {.
    importc: "LLVMElementAtOffset".}
proc offsetOfElement*(td: TargetDataRef; structTy: TypeRef; element: cuint): culonglong {.
  importc: "LLVMOffsetOfElement".}
