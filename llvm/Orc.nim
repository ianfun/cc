type
  OrcExecutorAddress* = uint64
  OrcJITTargetAddress* = uint64



type
  JITSymbolGenericFlags* {.size: sizeof(cint).} = enum
    JITSymbolGenericFlagsNone = 0, 
    JITSymbolGenericFlagsExported = 1,
    JITSymbolGenericFlagsWeak = 2,
    JITSymbolGenericFlagsCallable = 4,
    JITSymbolGenericFlagsMaterializationSideEffectsOnly = 8
  JITSymbolTargetFlags* = uint8T

  JITSymbolFlags* {.bycopy.} = object
    genericFlags*: uint8T
    targetFlags*: uint8T
  JITEvaluatedSymbol* {.bycopy.} = object
    address*: OrcExecutorAddress
    flags*: JITSymbolFlags
  OrcExecutionSessionRef* = ptr orcOpaqueExecutionSession
  OrcLLJITBuilderObjectLinkingLayerCreatorFunction = proc (ctx: pointer, ES: OrcExecutionSessionRef, Triple: cstring)
  OrcErrorReporterFunction* = proc (ctx: pointer; err: ErrorRef) {.cdecl, gcsafe.}
  OrcSymbolStringPoolRef* = ptr orcOpaqueSymbolStringPool
  OrcSymbolStringPoolEntryRef* = ptr orcOpaqueSymbolStringPoolEntry
  OrcCSymbolFlagsMapPair* {.bycopy.} = object
    name*: OrcSymbolStringPoolEntryRef
    flags*: JITSymbolFlags
  OrcCSymbolFlagsMapPairs* = ptr OrcCSymbolFlagsMapPair
  OrcCSymbolMapPair* {.bycopy.} = object
    name*: OrcSymbolStringPoolEntryRef
    sym*: JITEvaluatedSymbol
  OrcCSymbolMapPairs* = ptr OrcCSymbolMapPair

  OrcCSymbolAliasMapEntry* {.bycopy.} = object
    name*: OrcSymbolStringPoolEntryRef
    flags*: JITSymbolFlags
  OrcCSymbolAliasMapPair* {.bycopy.} = object
    name*: OrcSymbolStringPoolEntryRef
    entry*: OrcCSymbolAliasMapEntry
  OrcCSymbolAliasMapPairs* = ptr OrcCSymbolAliasMapPair
  OrcJITDylibRef* = ptr orcOpaqueJITDylib
  OrcCSymbolsList* {.bycopy.} = object
    symbols*: ptr OrcSymbolStringPoolEntryRef
    length*: csize_t
  OrcCDependenceMapPair* {.bycopy.} = object
    jd*: OrcJITDylibRef
    names*: OrcCSymbolsList
  OrcCDependenceMapPairs* = ptr OrcCDependenceMapPair
  OrcLookupKind* {.size: sizeof(cint).} = enum
    OrcLookupKindStatic, OrcLookupKindDLSym
  OrcJITDylibLookupFlags* {.size: sizeof(cint).} = enum
    OrcJITDylibLookupFlagsMatchExportedSymbolsOnly,
    OrcJITDylibLookupFlagsMatchAllSymbols
  OrcCJITDylibSearchOrderElement* {.bycopy.} = object
    jd*: OrcJITDylibRef
    jDLookupFlags*: OrcJITDylibLookupFlags
  OrcCJITDylibSearchOrder* = ptr OrcCJITDylibSearchOrderElement
  OrcSymbolLookupFlags* {.size: sizeof(cint).} = enum
    OrcSymbolLookupFlagsRequiredSymbol,
    OrcSymbolLookupFlagsWeaklyReferencedSymbol
  OrcCLookupSetElement* {.bycopy.} = object
    name*: OrcSymbolStringPoolEntryRef
    lookupFlags*: OrcSymbolLookupFlags
  OrcCLookupSet* = ptr OrcCLookupSetElement
  OrcMaterializationUnitRef* = ptr orcOpaqueMaterializationUnit
  OrcMaterializationResponsibilityRef* = ptr orcOpaqueMaterializationResponsibility
  OrcMaterializationUnitMaterializeFunction* = proc (ctx: pointer;
      mr: OrcMaterializationResponsibilityRef)
  OrcMaterializationUnitDiscardFunction* = proc (ctx: pointer; jd: OrcJITDylibRef;
      symbol: OrcSymbolStringPoolEntryRef)
  OrcMaterializationUnitDestroyFunction* = proc (ctx: pointer)
  OrcResourceTrackerRef* = ptr orcOpaqueResourceTracker
  OrcDefinitionGeneratorRef* = ptr orcOpaqueDefinitionGenerator
  OrcLookupStateRef* = ptr orcOpaqueLookupState
  OrcCAPIDefinitionGeneratorTryToGenerateFunction* = proc (
      generatorObj: OrcDefinitionGeneratorRef; ctx: pointer;
      lookupState: ptr OrcLookupStateRef; kind: OrcLookupKind; jd: OrcJITDylibRef;
      jDLookupFlags: OrcJITDylibLookupFlags; lookupSet: OrcCLookupSet;
      lookupSetSize: csize_t): ErrorRef

  OrcDisposeCAPIDefinitionGeneratorFunction* = proc (ctx: pointer)

  OrcSymbolPredicate* = proc (ctx: pointer; sym: OrcSymbolStringPoolEntryRef): cint
  OrcThreadSafeContextRef* = ptr orcOpaqueThreadSafeContext

  OrcThreadSafeModuleRef* = ptr orcOpaqueThreadSafeModule

  OrcGenericIRModuleOperationFunction* = proc (ctx: pointer; m: ModuleRef): ErrorRef


  OrcJITTargetMachineBuilderRef* = ptr orcOpaqueJITTargetMachineBuilder

  OrcObjectLayerRef* = ptr orcOpaqueObjectLayer


  OrcObjectLinkingLayerRef* = ptr orcOpaqueObjectLinkingLayer


  OrcIRTransformLayerRef* = ptr orcOpaqueIRTransformLayer

  OrcIRTransformLayerTransformFunction* = proc (ctx: pointer;
      modInOut: ptr OrcThreadSafeModuleRef; mr: OrcMaterializationResponsibilityRef): ErrorRef
  OrcObjectTransformLayerRef* = ptr orcOpaqueObjectTransformLayer

  OrcObjectTransformLayerTransformFunction* = proc (ctx: pointer;
      objInOut: ptr MemoryBufferRef): ErrorRef


  OrcIndirectStubsManagerRef* = ptr orcOpaqueIndirectStubsManager


  OrcLazyCallThroughManagerRef* = ptr orcOpaqueLazyCallThroughManager


  OrcDumpObjectsRef* = ptr orcOpaqueDumpObjects


proc orcExecutionSessionSetErrorReporter*(es: OrcExecutionSessionRef;
    reportError: OrcErrorReporterFunction; ctx: pointer) {.
    importc: "LLVMOrcExecutionSessionSetErrorReporter".}


proc orcExecutionSessionGetSymbolStringPool*(es: OrcExecutionSessionRef): OrcSymbolStringPoolRef {.
    importc: "LLVMOrcExecutionSessionGetSymbolStringPool".}


proc orcSymbolStringPoolClearDeadEntries*(ssp: OrcSymbolStringPoolRef) {.
    importc: "LLVMOrcSymbolStringPoolClearDeadEntries".}

proc orcExecutionSessionIntern*(es: OrcExecutionSessionRef; name: cstring): OrcSymbolStringPoolEntryRef {.
    importc: "LLVMOrcExecutionSessionIntern".}


type
  OrcExecutionSessionLookupHandleResultFunction* = proc (err: ErrorRef;
      result: OrcCSymbolMapPairs; numPairs: csize_t; ctx: pointer)

proc orcExecutionSessionLookup*(es: OrcExecutionSessionRef; k: OrcLookupKind;
                               searchOrder: OrcCJITDylibSearchOrder;
                               searchOrderSize: csize_t; symbols: OrcCLookupSet;
                               symbolsSize: csize_t; handleResult: OrcExecutionSessionLookupHandleResultFunction;
                               ctx: pointer) {.
    importc: "LLVMOrcExecutionSessionLookup".}

proc orcRetainSymbolStringPoolEntry*(s: OrcSymbolStringPoolEntryRef) {.
    importc: "LLVMOrcRetainSymbolStringPoolEntry".}


proc orcReleaseSymbolStringPoolEntry*(s: OrcSymbolStringPoolEntryRef) {.
    importc: "LLVMOrcReleaseSymbolStringPoolEntry".}


proc orcSymbolStringPoolEntryStr*(s: OrcSymbolStringPoolEntryRef): cstring {.
    importc: "LLVMOrcSymbolStringPoolEntryStr".}


proc orcReleaseResourceTracker*(rt: OrcResourceTrackerRef) {.
    importc: "LLVMOrcReleaseResourceTracker".}


proc orcResourceTrackerTransferTo*(srcRT: OrcResourceTrackerRef;
                                  dstRT: OrcResourceTrackerRef) {.
    importc: "LLVMOrcResourceTrackerTransferTo".}


proc orcResourceTrackerRemove*(rt: OrcResourceTrackerRef): ErrorRef {.
    importc: "LLVMOrcResourceTrackerRemove".}


proc orcDisposeDefinitionGenerator*(dg: OrcDefinitionGeneratorRef) {.
    importc: "LLVMOrcDisposeDefinitionGenerator".}

proc orcDisposeMaterializationUnit*(mu: OrcMaterializationUnitRef) {.
    importc: "LLVMOrcDisposeMaterializationUnit".}


proc orcCreateCustomMaterializationUnit*(name: cstring; ctx: pointer;
                                        syms: OrcCSymbolFlagsMapPairs;
                                        numSyms: csize_t;
                                        initSym: OrcSymbolStringPoolEntryRef;
    materialize: OrcMaterializationUnitMaterializeFunction; `discard`: OrcMaterializationUnitDiscardFunction;
    destroy: OrcMaterializationUnitDestroyFunction): OrcMaterializationUnitRef {.
    importc: "LLVMOrcCreateCustomMaterializationUnit".}

proc orcAbsoluteSymbols*(syms: OrcCSymbolMapPairs; numPairs: csize_t): OrcMaterializationUnitRef {.
    importc: "LLVMOrcAbsoluteSymbols".}

proc orcLazyReexports*(lctm: OrcLazyCallThroughManagerRef;
                      ism: OrcIndirectStubsManagerRef; sourceRef: OrcJITDylibRef;
                      callableAliases: OrcCSymbolAliasMapPairs; numPairs: csize_t): OrcMaterializationUnitRef {.
    importc: "LLVMOrcLazyReexports".}

proc orcDisposeMaterializationResponsibility*(
    mr: OrcMaterializationResponsibilityRef) {.
    importc: "LLVMOrcDisposeMaterializationResponsibility".}

proc orcMaterializationResponsibilityGetTargetDylib*(
    mr: OrcMaterializationResponsibilityRef): OrcJITDylibRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetTargetDylib".}

proc orcMaterializationResponsibilityGetExecutionSession*(
    mr: OrcMaterializationResponsibilityRef): OrcExecutionSessionRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetExecutionSession",
    .}


proc orcMaterializationResponsibilityGetSymbols*(
    mr: OrcMaterializationResponsibilityRef; numPairs: ptr csize_t): OrcCSymbolFlagsMapPairs {.
    importc: "LLVMOrcMaterializationResponsibilityGetSymbols".}

proc orcDisposeCSymbolFlagsMap*(pairs: OrcCSymbolFlagsMapPairs) {.
    importc: "LLVMOrcDisposeCSymbolFlagsMap".}

proc orcMaterializationResponsibilityGetInitializerSymbol*(
    mr: OrcMaterializationResponsibilityRef): OrcSymbolStringPoolEntryRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetInitializerSymbol",
    .}


proc orcMaterializationResponsibilityGetRequestedSymbols*(
    mr: OrcMaterializationResponsibilityRef; numSymbols: ptr csize_t): ptr OrcSymbolStringPoolEntryRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetRequestedSymbols",
    .}

proc orcDisposeSymbols*(symbols: ptr OrcSymbolStringPoolEntryRef) {.
    importc: "LLVMOrcDisposeSymbols".}

proc orcMaterializationResponsibilityNotifyResolved*(
    mr: OrcMaterializationResponsibilityRef; symbols: OrcCSymbolMapPairs;
    numPairs: csize_t): ErrorRef {.importc: "LLVMOrcMaterializationResponsibilityNotifyResolved",
                                .}


proc orcMaterializationResponsibilityNotifyEmitted*(
    mr: OrcMaterializationResponsibilityRef): ErrorRef {.
    importc: "LLVMOrcMaterializationResponsibilityNotifyEmitted".}


proc orcMaterializationResponsibilityDefineMaterializing*(
    mr: OrcMaterializationResponsibilityRef; pairs: OrcCSymbolFlagsMapPairs;
    numPairs: csize_t): ErrorRef {.importc: "LLVMOrcMaterializationResponsibilityDefineMaterializing",
                                .}


proc orcMaterializationResponsibilityFailMaterialization*(
    mr: OrcMaterializationResponsibilityRef) {.
    importc: "LLVMOrcMaterializationResponsibilityFailMaterialization",
    .}


proc orcMaterializationResponsibilityReplace*(
    mr: OrcMaterializationResponsibilityRef; mu: OrcMaterializationUnitRef): ErrorRef {.
    importc: "LLVMOrcMaterializationResponsibilityReplace".}


proc orcMaterializationResponsibilityDelegate*(
    mr: OrcMaterializationResponsibilityRef;
    symbols: ptr OrcSymbolStringPoolEntryRef; numSymbols: csize_t;
    result: ptr OrcMaterializationResponsibilityRef): ErrorRef {.
    importc: "LLVMOrcMaterializationResponsibilityDelegate".}


proc orcMaterializationResponsibilityAddDependencies*(
    mr: OrcMaterializationResponsibilityRef; name: OrcSymbolStringPoolEntryRef;
    dependencies: OrcCDependenceMapPairs; numPairs: csize_t) {.
    importc: "LLVMOrcMaterializationResponsibilityAddDependencies",
    .}


proc orcMaterializationResponsibilityAddDependenciesForAll*(
    mr: OrcMaterializationResponsibilityRef; dependencies: OrcCDependenceMapPairs;
    numPairs: csize_t) {.importc: "LLVMOrcMaterializationResponsibilityAddDependenciesForAll",
                       .}

proc orcExecutionSessionCreateBareJITDylib*(es: OrcExecutionSessionRef;
    name: cstring): OrcJITDylibRef {.importc: "LLVMOrcExecutionSessionCreateBareJITDylib",
                                  .}

proc orcExecutionSessionCreateJITDylib*(es: OrcExecutionSessionRef;
                                       result: ptr OrcJITDylibRef; name: cstring): ErrorRef {.
    importc: "LLVMOrcExecutionSessionCreateJITDylib".}


proc orcExecutionSessionGetJITDylibByName*(es: OrcExecutionSessionRef;
    name: cstring): OrcJITDylibRef {.importc: "LLVMOrcExecutionSessionGetJITDylibByName",
                                  .}


proc orcJITDylibCreateResourceTracker*(jd: OrcJITDylibRef): OrcResourceTrackerRef {.
    importc: "LLVMOrcJITDylibCreateResourceTracker".}


proc orcJITDylibGetDefaultResourceTracker*(jd: OrcJITDylibRef): OrcResourceTrackerRef {.
    importc: "LLVMOrcJITDylibGetDefaultResourceTracker".}


proc orcJITDylibDefine*(jd: OrcJITDylibRef; mu: OrcMaterializationUnitRef): ErrorRef {.
    importc: "LLVMOrcJITDylibDefine".}


proc orcJITDylibClear*(jd: OrcJITDylibRef): ErrorRef {.
    importc: "LLVMOrcJITDylibClear".}


proc orcJITDylibAddGenerator*(jd: OrcJITDylibRef; dg: OrcDefinitionGeneratorRef) {.
    importc: "LLVMOrcJITDylibAddGenerator".}

proc orcCreateCustomCAPIDefinitionGenerator*(
    f: OrcCAPIDefinitionGeneratorTryToGenerateFunction; ctx: pointer;
    dispose: OrcDisposeCAPIDefinitionGeneratorFunction): OrcDefinitionGeneratorRef {.
    importc: "LLVMOrcCreateCustomCAPIDefinitionGenerator".}


proc orcLookupStateContinueLookup*(s: OrcLookupStateRef; err: ErrorRef) {.
    importc: "LLVMOrcLookupStateContinueLookup".}


proc orcCreateDynamicLibrarySearchGeneratorForProcess*(
    result: ptr OrcDefinitionGeneratorRef; globalPrefx: char;
    filter: OrcSymbolPredicate; filterCtx: pointer): ErrorRef {.
    importc: "LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess",
    .}


proc orcCreateDynamicLibrarySearchGeneratorForPath*(
    result: ptr OrcDefinitionGeneratorRef; fileName: cstring; globalPrefix: char;
    filter: OrcSymbolPredicate; filterCtx: pointer): ErrorRef {.
    importc: "LLVMOrcCreateDynamicLibrarySearchGeneratorForPath".}

proc orcCreateStaticLibrarySearchGeneratorForPath*(
    result: ptr OrcDefinitionGeneratorRef; objLayer: OrcObjectLayerRef;
    fileName: cstring; targetTriple: cstring): ErrorRef {.
    importc: "LLVMOrcCreateStaticLibrarySearchGeneratorForPath".}

proc orcCreateNewThreadSafeContext*(): OrcThreadSafeContextRef {.
    importc: "LLVMOrcCreateNewThreadSafeContext".}

proc orcThreadSafeContextGetContext*(tSCtx: OrcThreadSafeContextRef): ContextRef {.
    importc: "LLVMOrcThreadSafeContextGetContext".}


proc orcDisposeThreadSafeContext*(tSCtx: OrcThreadSafeContextRef) {.
    importc: "LLVMOrcDisposeThreadSafeContext".}


proc orcCreateNewThreadSafeModule*(m: ModuleRef; tSCtx: OrcThreadSafeContextRef): OrcThreadSafeModuleRef {.
    importc: "LLVMOrcCreateNewThreadSafeModule".}


proc orcDisposeThreadSafeModule*(tsm: OrcThreadSafeModuleRef) {.
    importc: "LLVMOrcDisposeThreadSafeModule".}


proc orcThreadSafeModuleWithModuleDo*(tsm: OrcThreadSafeModuleRef;
                                     f: OrcGenericIRModuleOperationFunction;
                                     ctx: pointer): ErrorRef {.
    importc: "LLVMOrcThreadSafeModuleWithModuleDo".}

proc orcJITTargetMachineBuilderDetectHost*(
    result: ptr OrcJITTargetMachineBuilderRef): ErrorRef {.
    importc: "LLVMOrcJITTargetMachineBuilderDetectHost".}

proc orcJITTargetMachineBuilderCreateFromTargetMachine*(tm: TargetMachineRef): OrcJITTargetMachineBuilderRef {.
    importc: "LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine".}

proc orcDisposeJITTargetMachineBuilder*(jtmb: OrcJITTargetMachineBuilderRef) {.
    importc: "LLVMOrcDisposeJITTargetMachineBuilder".}


proc orcJITTargetMachineBuilderGetTargetTriple*(
    jtmb: OrcJITTargetMachineBuilderRef): cstring {.
    importc: "LLVMOrcJITTargetMachineBuilderGetTargetTriple".}

proc orcJITTargetMachineBuilderSetTargetTriple*(
    jtmb: OrcJITTargetMachineBuilderRef; targetTriple: cstring) {.
    importc: "LLVMOrcJITTargetMachineBuilderSetTargetTriple".}


proc orcObjectLayerAddObjectFile*(objLayer: OrcObjectLayerRef; jd: OrcJITDylibRef;
                                 objBuffer: MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcObjectLayerAddObjectFile".}

proc orcObjectLayerAddObjectFileWithRT*(objLayer: OrcObjectLayerRef;
                                       rt: OrcResourceTrackerRef;
                                       objBuffer: MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcObjectLayerAddObjectFileWithRT".}

proc orcObjectLayerEmit*(objLayer: OrcObjectLayerRef;
                        r: OrcMaterializationResponsibilityRef;
                        objBuffer: MemoryBufferRef) {.
    importc: "LLVMOrcObjectLayerEmit".}

proc orcDisposeObjectLayer*(objLayer: OrcObjectLayerRef) {.
    importc: "LLVMOrcDisposeObjectLayer".}
proc orcIRTransformLayerEmit*(iRTransformLayer: OrcIRTransformLayerRef;
                             mr: OrcMaterializationResponsibilityRef;
                             tsm: OrcThreadSafeModuleRef) {.
    importc: "LLVMOrcIRTransformLayerEmit".}


proc orcIRTransformLayerSetTransform*(iRTransformLayer: OrcIRTransformLayerRef;
    transformFunction: OrcIRTransformLayerTransformFunction; ctx: pointer) {.importc: "LLVMOrcIRTransformLayerSetTransform".}

proc orcObjectTransformLayerSetTransform*(
    objTransformLayer: OrcObjectTransformLayerRef;
    transformFunction: OrcObjectTransformLayerTransformFunction; ctx: pointer) {.
    importc: "LLVMOrcObjectTransformLayerSetTransform".}


proc orcCreateLocalIndirectStubsManager*(targetTriple: cstring): OrcIndirectStubsManagerRef {.
    importc: "LLVMOrcCreateLocalIndirectStubsManager".}


proc orcDisposeIndirectStubsManager*(ism: OrcIndirectStubsManagerRef) {.
    importc: "LLVMOrcDisposeIndirectStubsManager".}
proc orcCreateLocalLazyCallThroughManager*(targetTriple: cstring;
    es: OrcExecutionSessionRef; errorHandlerAddr: OrcJITTargetAddress;
    lctm: ptr OrcLazyCallThroughManagerRef): ErrorRef {.
    importc: "LLVMOrcCreateLocalLazyCallThroughManager".}

proc orcDisposeLazyCallThroughManager*(lctm: OrcLazyCallThroughManagerRef) {.
    importc: "LLVMOrcDisposeLazyCallThroughManager".}


proc orcCreateDumpObjects*(dumpDir: cstring; identifierOverride: cstring): OrcDumpObjectsRef {.
    importc: "LLVMOrcCreateDumpObjects".}

proc orcDisposeDumpObjects*(dumpObjects: OrcDumpObjectsRef) {.
    importc: "LLVMOrcDisposeDumpObjects".}


proc orcDumpObjectsCallOperator*(dumpObjects: OrcDumpObjectsRef;
                                objBuffer: ptr MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcDumpObjects_CallOperator".}
