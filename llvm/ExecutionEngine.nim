proc linkInInterpreter*() {.importc: "LLVMLinkInInterpreter".}
type
  GenericValueRef* = ptr opaqueGenericValue
  ExecutionEngineRef* = ptr opaqueExecutionEngine
  MCJITMemoryManagerRef* = ptr opaqueMCJITMemoryManager
  MCJITCompilerOptions* {.bycopy.} = object
    optLevel*: cuint
    codeModel*: CodeModel
    noFramePointerElim*: Bool
    enableFastISel*: Bool
    mcjmm*: MCJITMemoryManagerRef

proc createGenericValueOfInt*(ty: TypeRef; n: culonglong; isSigned: Bool): GenericValueRef {.
    importc: "LLVMCreateGenericValueOfInt".}
proc createGenericValueOfPointer*(p: pointer): GenericValueRef {.
    importc: "LLVMCreateGenericValueOfPointer".}
proc createGenericValueOfFloat*(ty: TypeRef; n: cdouble): GenericValueRef {.
    importc: "LLVMCreateGenericValueOfFloat".}
proc genericValueIntWidth*(genValRef: GenericValueRef): cuint {.
    importc: "LLVMGenericValueIntWidth".}
proc genericValueToInt*(genVal: GenericValueRef; isSigned: Bool): culonglong {.
    importc: "LLVMGenericValueToInt".}
proc genericValueToPointer*(genVal: GenericValueRef): pointer {.
    importc: "LLVMGenericValueToPointer".}
proc genericValueToFloat*(tyRef: TypeRef; genVal: GenericValueRef): cdouble {.
    importc: "LLVMGenericValueToFloat".}
proc disposeGenericValue*(genVal: GenericValueRef) {.
    importc: "LLVMDisposeGenericValue".}

proc createExecutionEngineForModule*(outEE: ptr ExecutionEngineRef; m: ModuleRef;
                                    outError: cstringArray): Bool {.
    importc: "LLVMCreateExecutionEngineForModule".}
proc createInterpreterForModule*(outInterp: ptr ExecutionEngineRef; m: ModuleRef;
                                outError: cstringArray): Bool {.
    importc: "LLVMCreateInterpreterForModule".}
proc createJITCompilerForModule*(outJIT: ptr ExecutionEngineRef; m: ModuleRef;
                                optLevel: cuint; outError: cstringArray): Bool {.
    importc: "LLVMCreateJITCompilerForModule".}
proc initializeMCJITCompilerOptions*(options: ptr MCJITCompilerOptions;
                                    sizeOfOptions: csize_t) {.
    importc: "LLVMInitializeMCJITCompilerOptions".}

proc createMCJITCompilerForModule*(outJIT: ptr ExecutionEngineRef; m: ModuleRef;
                                  options: ptr MCJITCompilerOptions;
                                  sizeOfOptions: csize_t; outError: cstringArray): Bool {.
    importc: "LLVMCreateMCJITCompilerForModule".}
proc disposeExecutionEngine*(ee: ExecutionEngineRef) {.
    importc: "LLVMDisposeExecutionEngine".}
proc runStaticConstructors*(ee: ExecutionEngineRef) {.
    importc: "LLVMRunStaticConstructors".}
proc runStaticDestructors*(ee: ExecutionEngineRef) {.
    importc: "LLVMRunStaticDestructors".}
proc runFunctionAsMain*(ee: ExecutionEngineRef; f: ValueRef; argC: cuint;
                       argV: cstringArray; envP: cstringArray): cint {.
    importc: "LLVMRunFunctionAsMain".}
proc runFunction*(ee: ExecutionEngineRef; f: ValueRef; numArgs: cuint;
                 args: ptr GenericValueRef): GenericValueRef {.
    importc: "LLVMRunFunction".}
proc freeMachineCodeForFunction*(ee: ExecutionEngineRef; f: ValueRef) {.
    importc: "LLVMFreeMachineCodeForFunction".}
proc addModule*(ee: ExecutionEngineRef; m: ModuleRef) {.importc: "LLVMAddModule".}
proc removeModule*(ee: ExecutionEngineRef; m: ModuleRef; outMod: ptr ModuleRef;
                  outError: cstringArray): Bool {.importc: "LLVMRemoveModule".}
proc findFunction*(ee: ExecutionEngineRef; name: cstring; outFn: ptr ValueRef): Bool {.
    importc: "LLVMFindFunction".}
proc recompileAndRelinkFunction*(ee: ExecutionEngineRef; fn: ValueRef): pointer {.
    importc: "LLVMRecompileAndRelinkFunction".}
proc getExecutionEngineTargetData*(ee: ExecutionEngineRef): TargetDataRef {.
    importc: "LLVMGetExecutionEngineTargetData".}
proc getExecutionEngineTargetMachine*(ee: ExecutionEngineRef): TargetMachineRef {.
    importc: "LLVMGetExecutionEngineTargetMachine".}
proc addGlobalMapping*(ee: ExecutionEngineRef; global: ValueRef; `addr`: pointer) {.
    importc: "LLVMAddGlobalMapping".}
proc getPointerToGlobal*(ee: ExecutionEngineRef; global: ValueRef): pointer {.
    importc: "LLVMGetPointerToGlobal".}
proc getGlobalValueAddress*(ee: ExecutionEngineRef; name: cstring): uint64T {.
    importc: "LLVMGetGlobalValueAddress".}
proc getFunctionAddress*(ee: ExecutionEngineRef; name: cstring): uint64T {.
    importc: "LLVMGetFunctionAddress".}

proc executionEngineGetErrMsg*(ee: ExecutionEngineRef; outError: cstringArray): Bool {.
    importc: "LLVMExecutionEngineGetErrMsg".}

type
  MemoryManagerAllocateCodeSectionCallback* = proc (opaque: pointer; size: uintptrT;
      alignment: cuint; sectionID: cuint; sectionName: cstring): ptr uint8T
  MemoryManagerAllocateDataSectionCallback* = proc (opaque: pointer; size: uintptrT;
      alignment: cuint; sectionID: cuint; sectionName: cstring; isReadOnly: Bool): ptr uint8T
  MemoryManagerFinalizeMemoryCallback* = proc (opaque: pointer; errMsg: cstringArray): Bool
  MemoryManagerDestroyCallback* = proc (opaque: pointer)

proc createSimpleMCJITMemoryManager*(opaque: pointer; allocateCodeSection: MemoryManagerAllocateCodeSectionCallback;
    allocateDataSection: MemoryManagerAllocateDataSectionCallback; finalizeMemory: MemoryManagerFinalizeMemoryCallback;
                                    destroy: MemoryManagerDestroyCallback): MCJITMemoryManagerRef {.
    importc: "LLVMCreateSimpleMCJITMemoryManager".}
proc disposeMCJITMemoryManager*(mm: MCJITMemoryManagerRef) {.
    importc: "LLVMDisposeMCJITMemoryManager".}

proc createGDBRegistrationListener*(): JITEventListenerRef {.
    importc: "LLVMCreateGDBRegistrationListener".}
proc createIntelJITEventListener*(): JITEventListenerRef {.
    importc: "LLVMCreateIntelJITEventListener".}
proc createOProfileJITEventListener*(): JITEventListenerRef {.
    importc: "LLVMCreateOProfileJITEventListener".}
proc createPerfJITEventListener*(): JITEventListenerRef {.
    importc: "LLVMCreatePerfJITEventListener".}
