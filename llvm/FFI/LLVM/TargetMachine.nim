type
  TargetRef* = ptr target
  CodeGenOptLevel* {.size: sizeof(cint).} = enum
    CodeGenLevelNone, CodeGenLevelLess, CodeGenLevelDefault, CodeGenLevelAggressive
  RelocMode* {.size: sizeof(cint).} = enum
    RelocDefault, RelocStatic, RelocPIC, RelocDynamicNoPic, RelocROPI, RelocRWPI,
    RelocROPI_RWPI
  CodeModel* {.size: sizeof(cint).} = enum
    CodeModelDefault, CodeModelJITDefault, CodeModelTiny, CodeModelSmall,
    CodeModelKernel, CodeModelMedium, CodeModelLarge
  CodeGenFileType* {.size: sizeof(cint).} = enum
    AssemblyFile, ObjectFile


proc getFirstTarget*(): TargetRef {.importc: "LLVMGetFirstTarget".}

proc getNextTarget*(t: TargetRef): TargetRef {.importc: "LLVMGetNextTarget",
    .}

proc getTargetFromName*(name: cstring): TargetRef {.
    importc: "LLVMGetTargetFromName".}

proc getTargetFromTriple*(triple: cstring; t: ptr TargetRef;
                         errorMessage: cstringArray): Bool {.
    importc: "LLVMGetTargetFromTriple".}

proc getTargetName*(t: TargetRef): cstring {.importc: "LLVMGetTargetName",
    .}

proc getTargetDescription*(t: TargetRef): cstring {.
    importc: "LLVMGetTargetDescription".}

proc targetHasJIT*(t: TargetRef): Bool {.importc: "LLVMTargetHasJIT".}

proc targetHasTargetMachine*(t: TargetRef): Bool {.
    importc: "LLVMTargetHasTargetMachine".}

proc targetHasAsmBackend*(t: TargetRef): Bool {.importc: "LLVMTargetHasAsmBackend",
    .}

proc createTargetMachine*(t: TargetRef; triple: cstring; cpu: cstring;
                         features: cstring; level: CodeGenOptLevel;
                         reloc: RelocMode; codeModel: CodeModel): TargetMachineRef {.
    importc: "LLVMCreateTargetMachine".}

proc disposeTargetMachine*(t: TargetMachineRef) {.
    importc: "LLVMDisposeTargetMachine".}

proc getTargetMachineTarget*(t: TargetMachineRef): TargetRef {.
    importc: "LLVMGetTargetMachineTarget".}

proc getTargetMachineTriple*(t: TargetMachineRef): cstring {.
    importc: "LLVMGetTargetMachineTriple".}

proc getTargetMachineCPU*(t: TargetMachineRef): cstring {.
    importc: "LLVMGetTargetMachineCPU".}

proc getTargetMachineFeatureString*(t: TargetMachineRef): cstring {.
    importc: "LLVMGetTargetMachineFeatureString".}

proc createTargetDataLayout*(t: TargetMachineRef): TargetDataRef {.
    importc: "LLVMCreateTargetDataLayout".}

proc setTargetMachineAsmVerbosity*(t: TargetMachineRef; verboseAsm: Bool) {.
    importc: "LLVMSetTargetMachineAsmVerbosity".}

proc targetMachineEmitToFile*(t: TargetMachineRef; m: ModuleRef; filename: cstring;
                             codegen: CodeGenFileType; errorMessage: cstringArray): Bool {.
    importc: "LLVMTargetMachineEmitToFile".}

proc targetMachineEmitToMemoryBuffer*(t: TargetMachineRef; m: ModuleRef;
                                     codegen: CodeGenFileType;
                                     errorMessage: cstringArray;
                                     outMemBuf: ptr MemoryBufferRef): Bool {.
    importc: "LLVMTargetMachineEmitToMemoryBuffer".}

proc getDefaultTargetTriple*(): cstring {.importc: "LLVMGetDefaultTargetTriple",
                                       .}

proc normalizeTargetTriple*(triple: cstring): cstring {.
    importc: "LLVMNormalizeTargetTriple".}

proc getHostCPUName*(): cstring {.importc: "LLVMGetHostCPUName".}

proc getHostCPUFeatures*(): cstring {.importc: "LLVMGetHostCPUFeatures",
                                   .}

proc addAnalysisPasses*(t: TargetMachineRef; pm: PassManagerRef) {.
    importc: "LLVMAddAnalysisPasses".}
