type
  OrcLLJITBuilderRef* = ptr orcOpaqueLLJITBuilder
  OrcLLJITRef* = ptr orcOpaqueLLJIT

proc orcCreateLLJITBuilder*(): OrcLLJITBuilderRef {.
    importc: "LLVMOrcCreateLLJITBuilder".}

proc orcDisposeLLJITBuilder*(builder: OrcLLJITBuilderRef) {.
    importc: "LLVMOrcDisposeLLJITBuilder".}

proc orcLLJITBuilderSetJITTargetMachineBuilder*(builder: OrcLLJITBuilderRef;
    jtmb: OrcJITTargetMachineBuilderRef) {.
    importc: "LLVMOrcLLJITBuilderSetJITTargetMachineBuilder".}

proc orcLLJITBuilderSetObjectLinkingLayerCreator*(builder: OrcLLJITBuilderRef;
    f: OrcLLJITBuilderObjectLinkingLayerCreatorFunction; ctx: pointer) {.
    importc: "LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator".}

proc orcCreateLLJIT*(result: ptr OrcLLJITRef; builder: OrcLLJITBuilderRef): ErrorRef {.
    importc: "LLVMOrcCreateLLJIT".}

proc orcDisposeLLJIT*(j: OrcLLJITRef): ErrorRef {.importc: "LLVMOrcDisposeLLJIT".}

proc orcLLJITGetExecutionSession*(j: OrcLLJITRef): OrcExecutionSessionRef {.
    importc: "LLVMOrcLLJITGetExecutionSession".}

proc orcLLJITGetMainJITDylib*(j: OrcLLJITRef): OrcJITDylibRef {.
    importc: "LLVMOrcLLJITGetMainJITDylib".}

proc orcLLJITGetTripleString*(j: OrcLLJITRef): cstring {.
    importc: "LLVMOrcLLJITGetTripleString".}


proc orcLLJITGetGlobalPrefix*(j: OrcLLJITRef): char {.
    importc: "LLVMOrcLLJITGetGlobalPrefix".}

proc orcLLJITMangleAndIntern*(j: OrcLLJITRef; unmangledName: cstring): OrcSymbolStringPoolEntryRef {.
    importc: "LLVMOrcLLJITMangleAndIntern".}

proc orcLLJITAddObjectFile*(j: OrcLLJITRef; jd: OrcJITDylibRef;
                           objBuffer: MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcLLJITAddObjectFile".}


proc orcLLJITAddObjectFileWithRT*(j: OrcLLJITRef; rt: OrcResourceTrackerRef;
                                 objBuffer: MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcLLJITAddObjectFileWithRT".}

proc orcLLJITAddLLVMIRModule*(j: OrcLLJITRef; jd: OrcJITDylibRef;
                             tsm: OrcThreadSafeModuleRef): ErrorRef {.
    importc: "LLVMOrcLLJITAddLLVMIRModule".}

proc orcLLJITAddLLVMIRModuleWithRT*(j: OrcLLJITRef; jd: OrcResourceTrackerRef;
                                   tsm: OrcThreadSafeModuleRef): ErrorRef {.
    importc: "LLVMOrcLLJITAddLLVMIRModuleWithRT".}

proc orcLLJITLookup*(j: OrcLLJITRef; result: ptr OrcExecutorAddress; name: cstring): ErrorRef {.
    importc: "LLVMOrcLLJITLookup".}

proc orcLLJITGetObjLinkingLayer*(j: OrcLLJITRef): OrcObjectLayerRef {.importc: "LLVMOrcLLJITGetObjLinkingLayer".}

proc orcLLJITGetObjTransformLayer*(j: OrcLLJITRef): OrcObjectTransformLayerRef {.
    importc: "LLVMOrcLLJITGetObjTransformLayer".}

proc orcLLJITGetIRTransformLayer*(j: OrcLLJITRef): OrcIRTransformLayerRef {.
    importc: "LLVMOrcLLJITGetIRTransformLayer".}

proc orcLLJITGetDataLayoutStr*(j: OrcLLJITRef): cstring {.
    importc: "LLVMOrcLLJITGetDataLayoutStr".}
