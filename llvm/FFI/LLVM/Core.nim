type
  TypeKind* {.size: sizeof(cint).} = enum
    VoidTypeKind,  
    HalfTypeKind,  
    FloatTypeKind, 
    DoubleTypeKind,
    X86FP80TypeKind,   
    FP128TypeKind,     
    PPC_FP128TypeKind, 
    LabelTypeKind,     
    IntegerTypeKind,   
    FunctionTypeKind,  
    StructTypeKind,    
    ArrayTypeKind,     
    PointerTypeKind,   
    VectorTypeKind,    
    MetadataTypeKind,  
    X86MMXTypeKind,    
    TokenTypeKind,   
    ScalableVectorTypeKind, 
    BFloatTypeKind,
    X86AMXTypeKind
  Linkage* {.size: sizeof(cint).} = enum
    ExternalLinkage, 
    AvailableExternallyLinkage, LinkOnceAnyLinkage,
    LinkOnceODRLinkage,
    LinkOnceODRAutoHideLinkage,
    WeakAnyLinkage,          
    WeakODRLinkage,          
    AppendingLinkage,        
    InternalLinkage,         
    PrivateLinkage,          
    DLLImportLinkage,        
    DLLExportLinkage,        
    ExternalWeakLinkage,     
    GhostLinkage,            
    CommonLinkage,           
    LinkerPrivateLinkage,    
    LinkerPrivateWeakLinkage 
  Visibility* {.size: sizeof(cint).} = enum
    DefaultVisibility,   
    HiddenVisibility,    
    ProtectedVisibility  
  UnnamedAddr* {.size: sizeof(cint).} = enum
    NoUnnamedAddr,      
    LocalUnnamedAddr,   
    GlobalUnnamedAddr   
  DLLStorageClass* {.size: sizeof(cint).} = enum
    DefaultStorageClass = 0, DLLImportStorageClass = 1,
    DLLExportStorageClass = 2
  CallConv* {.size: sizeof(cint).} = enum
    CCallConv = 0, FastCallConv = 8, ColdCallConv = 9, GHCCallConv = 10, HiPECallConv = 11,
    WebKitJSCallConv = 12, AnyRegCallConv = 13, PreserveMostCallConv = 14,
    PreserveAllCallConv = 15, SwiftCallConv = 16, CXXFASTTLSCallConv = 17,
    X86StdcallCallConv = 64, X86FastcallCallConv = 65, ARMAPCSCallConv = 66,
    ARMAAPCSCallConv = 67, ARMAAPCSVFPCallConv = 68, MSP430INTRCallConv = 69,
    X86ThisCallCallConv = 70, PTXKernelCallConv = 71, PTXDeviceCallConv = 72,
    SPIRFUNCCallConv = 75, SPIRKERNELCallConv = 76, IntelOCLBICallConv = 77,
    X8664SysVCallConv = 78, Win64CallConv = 79, X86VectorCallCallConv = 80,
    HHVMCallConv = 81, HHVMCCallConv = 82, X86INTRCallConv = 83, AVRINTRCallConv = 84,
    AVRSIGNALCallConv = 85, AVRBUILTINCallConv = 86, AMDGPUVSCallConv = 87,
    AMDGPUGSCallConv = 88, AMDGPUPSCallConv = 89, AMDGPUCSCallConv = 90,
    AMDGPUKERNELCallConv = 91, X86RegCallCallConv = 92, AMDGPUHSCallConv = 93,
    MSP430BUILTINCallConv = 94, AMDGPULSCallConv = 95, AMDGPUESCallConv = 96
  ValueKind* {.size: sizeof(cint).} = enum
    ArgumentValueKind, BasicBlockValueKind, MemoryUseValueKind, MemoryDefValueKind,
    MemoryPhiValueKind, FunctionValueKind, GlobalAliasValueKind,
    GlobalIFuncValueKind, GlobalVariableValueKind, BlockAddressValueKind,
    ConstantExprValueKind, ConstantArrayValueKind, ConstantStructValueKind,
    ConstantVectorValueKind, UndefValueValueKind, ConstantAggregateZeroValueKind,
    ConstantDataArrayValueKind, ConstantDataVectorValueKind, ConstantIntValueKind,
    ConstantFPValueKind, ConstantPointerNullValueKind, ConstantTokenNoneValueKind,
    MetadataAsValueValueKind, InlineAsmValueKind, InstructionValueKind,
    PoisonValueValueKind
  IntPredicate* {.size: sizeof(cint).} = enum
    IntEQ = 32,
    IntNE,     
    IntUGT,    
    IntUGE,    
    IntULT,    
    IntULE,    
    IntSGT,    
    IntSGE,    
    IntSLT,    
    IntSLE     
  RealPredicate* {.size: sizeof(cint).} = enum
    RealPredicateFalse,
    RealOEQ,           
    RealOGT,           
    RealOGE,           
    RealOLT,           
    RealOLE,           
    RealONE,           
    RealORD,           
    RealUNO,           
    RealUEQ,           
    RealUGT,           
    RealUGE,           
    RealULT,           
    RealULE,           
    RealUNE,           
    RealPredicateTrue  
  LandingPadClauseTy* {.size: sizeof(cint).} = enum
    LandingPadCatch,  
    LandingPadFilter  
  ThreadLocalMode* {.size: sizeof(cint).} = enum
    NotThreadLocal = 0, GeneralDynamicTLSModel, LocalDynamicTLSModel,
    InitialExecTLSModel, LocalExecTLSModel
  AtomicOrdering* {.size: sizeof(cint).} = enum
    AtomicOrderingNotAtomic = 0,
    AtomicOrderingUnordered = 1,
    AtomicOrderingMonotonic = 2,
    AtomicOrderingAcquire = 4, 
    AtomicOrderingRelease = 5,
    AtomicOrderingAcquireRelease = 6,
    AtomicOrderingSequentiallyConsistent = 7
  AtomicRMWBinOp* {.size: sizeof(cint).} = enum
    AtomicRMWBinOpXchg,
    AtomicRMWBinOpAdd, 
    AtomicRMWBinOpSub, 
    AtomicRMWBinOpAnd, 
    AtomicRMWBinOpNand,
    AtomicRMWBinOpOr,  
    AtomicRMWBinOpXor, 
    AtomicRMWBinOpMax, 
    AtomicRMWBinOpMin, 
    AtomicRMWBinOpUMax,
    AtomicRMWBinOpUMin,
    AtomicRMWBinOpFAdd,
    AtomicRMWBinOpFSub,  
    AtomicRMWBinOpFMax, 
    AtomicRMWBinOpFMin
  DiagnosticSeverity* {.size: sizeof(cint).} = enum
    DSError, DSWarning, DSRemark, DSNote
  InlineAsmDialect* {.size: sizeof(cint).} = enum
    InlineAsmDialectATT, InlineAsmDialectIntel
  ModuleFlagBehavior* = cuint

const
  AttributeReturnIndex* = 0 
  AttributeFunctionIndex* = -1


proc initializeCore*(r: PassRegistryRef) {.importc: "LLVMInitializeCore", .}

proc shutdown*() {.importc: "LLVMShutdown".}

proc createMessage*(message: cstring): cstring {.importc: "LLVMCreateMessage".}
proc disposeMessage*(message: cstring) {.importc: "LLVMDisposeMessage".}

type
  DiagnosticHandler* = proc (a1: DiagnosticInfoRef; a2: pointer)
  YieldCallback* = proc (a1: ContextRef; a2: pointer)

proc contextCreate*(): ContextRef {.importc: "LLVMContextCreate".}


proc getGlobalContext*(): ContextRef {.importc: "LLVMGetGlobalContext",.}

proc contextSetDiagnosticHandler*(c: ContextRef; handler: DiagnosticHandler;
                                 diagnosticContext: pointer) {.importc: "LLVMContextSetDiagnosticHandler".}

proc contextGetDiagnosticHandler*(c: ContextRef): DiagnosticHandler {.
    importc: "LLVMContextGetDiagnosticHandler".}


proc contextGetDiagnosticContext*(c: ContextRef): pointer {.
    importc: "LLVMContextGetDiagnosticContext".}


proc contextSetYieldCallback*(c: ContextRef; callback: YieldCallback;
                             opaqueHandle: pointer) {.
    importc: "LLVMContextSetYieldCallback".}


proc contextShouldDiscardValueNames*(c: ContextRef): Bool {.
    importc: "LLVMContextShouldDiscardValueNames".}


proc contextSetDiscardValueNames*(c: ContextRef; `discard`: Bool) {.
    importc: "LLVMContextSetDiscardValueNames".}


proc contextSetOpaquePointers*(c: ContextRef; opaquePointers: Bool) {.
    importc: "LLVMContextSetOpaquePointers".}


proc contextDispose*(c: ContextRef) {.importc: "LLVMContextDispose".}


proc getDiagInfoDescription*(di: DiagnosticInfoRef): cstring {.
    importc: "LLVMGetDiagInfoDescription".}


proc getDiagInfoSeverity*(di: DiagnosticInfoRef): DiagnosticSeverity {.
    importc: "LLVMGetDiagInfoSeverity".}
proc getMDKindIDInContext*(c: ContextRef; name: cstring; sLen: cuint): cuint {.
    importc: "LLVMGetMDKindIDInContext".}
proc getMDKindID*(name: cstring; sLen: cuint): cuint {.importc: "LLVMGetMDKindID",
    .}

proc getEnumAttributeKindForName*(name: cstring; sLen: csize_t): cuint {.
    importc: "LLVMGetEnumAttributeKindForName".}
proc getLastEnumAttributeKind*(): cuint {.importc: "LLVMGetLastEnumAttributeKind".}

proc createEnumAttribute*(c: ContextRef; kindID: cuint; val: uint64): AttributeRef {.
    importc: "LLVMCreateEnumAttribute".}

proc getEnumAttributeKind*(a: AttributeRef): cuint {.
    importc: "LLVMGetEnumAttributeKind".}


proc getEnumAttributeValue*(a: AttributeRef): uint64 {.
    importc: "LLVMGetEnumAttributeValue".}


proc createTypeAttribute*(c: ContextRef; kindID: cuint; typeRef: TypeRef): AttributeRef {.
    importc: "LLVMCreateTypeAttribute".}


proc getTypeAttributeValue*(a: AttributeRef): TypeRef {.
    importc: "LLVMGetTypeAttributeValue".}


proc createStringAttribute*(c: ContextRef; k: cstring; kLength: cuint; v: cstring;
                           vLength: cuint): AttributeRef {.
    importc: "LLVMCreateStringAttribute".}


proc getStringAttributeKind*(a: AttributeRef; length: ptr cuint): cstring {.
    importc: "LLVMGetStringAttributeKind".}

proc getStringAttributeValue*(a: AttributeRef; length: ptr cuint): cstring {.
    importc: "LLVMGetStringAttributeValue".}

proc isEnumAttribute*(a: AttributeRef): Bool {.importc: "LLVMIsEnumAttribute",
    .}
proc isStringAttribute*(a: AttributeRef): Bool {.importc: "LLVMIsStringAttribute",
    .}
proc isTypeAttribute*(a: AttributeRef): Bool {.importc: "LLVMIsTypeAttribute",
    .}

proc getTypeByName2*(c: ContextRef; name: cstring): TypeRef {.
    importc: "LLVMGetTypeByName2".}

proc moduleCreateWithName*(moduleID: cstring): ModuleRef {.
    importc: "LLVMModuleCreateWithName".}

proc moduleCreateWithNameInContext*(moduleID: cstring; c: ContextRef): ModuleRef {.
    importc: "LLVMModuleCreateWithNameInContext".}


proc cloneModule*(m: ModuleRef): ModuleRef {.importc: "LLVMCloneModule",
    .}


proc disposeModule*(m: ModuleRef) {.importc: "LLVMDisposeModule".}

proc getModuleIdentifier*(m: ModuleRef; len: ptr csize_t): cstring {.
    importc: "LLVMGetModuleIdentifier".}


proc setModuleIdentifier*(m: ModuleRef; ident: cstring; len: csize_t) {.
    importc: "LLVMSetModuleIdentifier".}

proc getSourceFileName*(m: ModuleRef; len: ptr csize_t): cstring {.
    importc: "LLVMGetSourceFileName".}


proc setSourceFileName*(m: ModuleRef; name: cstring; len: csize_t) {.
    importc: "LLVMSetSourceFileName".}


proc getDataLayoutStr*(m: ModuleRef): cstring {.importc: "LLVMGetDataLayoutStr",
    .}
proc getDataLayout*(m: ModuleRef): cstring {.importc: "LLVMGetDataLayout",
    .}


proc setDataLayout*(m: ModuleRef; dataLayoutStr: cstring) {.
    importc: "LLVMSetDataLayout".}

proc getTarget*(m: ModuleRef): cstring {.importc: "LLVMGetTarget".}


proc setTarget*(m: ModuleRef; triple: cstring) {.importc: "LLVMSetTarget",
    .}


proc copyModuleFlagsMetadata*(m: ModuleRef; len: ptr csize_t): ptr ModuleFlagEntry {.
    importc: "LLVMCopyModuleFlagsMetadata".}

proc disposeModuleFlagsMetadata*(entries: ptr ModuleFlagEntry) {.
    importc: "LLVMDisposeModuleFlagsMetadata".}


proc moduleFlagEntriesGetFlagBehavior*(entries: ptr ModuleFlagEntry; index: cuint): ModuleFlagBehavior {.
    importc: "LLVMModuleFlagEntriesGetFlagBehavior".}


proc moduleFlagEntriesGetKey*(entries: ptr ModuleFlagEntry; index: cuint;
                             len: ptr csize_t): cstring {.
    importc: "LLVMModuleFlagEntriesGetKey".}


proc moduleFlagEntriesGetMetadata*(entries: ptr ModuleFlagEntry; index: cuint): MetadataRef {.
    importc: "LLVMModuleFlagEntriesGetMetadata".}

proc getModuleFlag*(m: ModuleRef; key: cstring; keyLen: csize_t): MetadataRef {.
    importc: "LLVMGetModuleFlag".}


proc addModuleFlag*(m: ModuleRef; behavior: ModuleFlagBehavior; key: cstring;
                   keyLen: csize_t; val: MetadataRef) {.
    importc: "LLVMAddModuleFlag".}


proc dumpModule*(m: ModuleRef) {.importc: "LLVMDumpModule".}


proc printModuleToFile*(m: ModuleRef; filename: cstring; errorMessage: cstringArray): Bool {.
    importc: "LLVMPrintModuleToFile".}


proc printModuleToString*(m: ModuleRef): cstring {.
    importc: "LLVMPrintModuleToString".}


proc getModuleInlineAsm*(m: ModuleRef; len: ptr csize_t): cstring {.
    importc: "LLVMGetModuleInlineAsm".}

proc setModuleInlineAsm2*(m: ModuleRef; `asm`: cstring; len: csize_t) {.
    importc: "LLVMSetModuleInlineAsm2".}

proc appendModuleInlineAsm*(m: ModuleRef; `asm`: cstring; len: csize_t) {.
    importc: "LLVMAppendModuleInlineAsm".}


proc getInlineAsm*(ty: TypeRef; asmString: cstring; asmStringSize: csize_t;
                  constraints: cstring; constraintsSize: csize_t;
                  hasSideEffects: Bool; isAlignStack: Bool;
                  dialect: InlineAsmDialect; canThrow: Bool): ValueRef {.
    importc: "LLVMGetInlineAsm".}


proc getModuleContext*(m: ModuleRef): ContextRef {.importc: "LLVMGetModuleContext",
    .}

proc getTypeByName*(m: ModuleRef; name: cstring): TypeRef {.
    importc: "LLVMGetTypeByName".}


proc getFirstNamedMetadata*(m: ModuleRef): NamedMDNodeRef {.
    importc: "LLVMGetFirstNamedMetadata".}


proc getLastNamedMetadata*(m: ModuleRef): NamedMDNodeRef {.
    importc: "LLVMGetLastNamedMetadata".}

proc getNextNamedMetadata*(namedMDNode: NamedMDNodeRef): NamedMDNodeRef {.
    importc: "LLVMGetNextNamedMetadata".}

proc getPreviousNamedMetadata*(namedMDNode: NamedMDNodeRef): NamedMDNodeRef {.
    importc: "LLVMGetPreviousNamedMetadata".}

proc getNamedMetadata*(m: ModuleRef; name: cstring; nameLen: csize_t): NamedMDNodeRef {.
    importc: "LLVMGetNamedMetadata".}


proc getOrInsertNamedMetadata*(m: ModuleRef; name: cstring; nameLen: csize_t): NamedMDNodeRef {.
    importc: "LLVMGetOrInsertNamedMetadata".}


proc getNamedMetadataName*(namedMD: NamedMDNodeRef; nameLen: ptr csize_t): cstring {.
    importc: "LLVMGetNamedMetadataName".}

proc getNamedMetadataNumOperands*(m: ModuleRef; name: cstring): cuint {.
    importc: "LLVMGetNamedMetadataNumOperands".}


proc getNamedMetadataOperands*(m: ModuleRef; name: cstring; dest: ptr ValueRef) {.
    importc: "LLVMGetNamedMetadataOperands".}


proc addNamedMetadataOperand*(m: ModuleRef; name: cstring; val: ValueRef) {.
    importc: "LLVMAddNamedMetadataOperand".}


proc getDebugLocDirectory*(val: ValueRef; length: ptr cuint): cstring {.
    importc: "LLVMGetDebugLocDirectory".}

proc getDebugLocFilename*(val: ValueRef; length: ptr cuint): cstring {.
    importc: "LLVMGetDebugLocFilename".}

proc getDebugLocLine*(val: ValueRef): cuint {.importc: "LLVMGetDebugLocLine",
    .}

proc getDebugLocColumn*(val: ValueRef): cuint {.importc: "LLVMGetDebugLocColumn",
    .}


proc addFunction*(m: ModuleRef; name: cstring; functionTy: TypeRef): ValueRef {.
    importc: "LLVMAddFunction".}

proc getNamedFunction*(m: ModuleRef; name: cstring): ValueRef {.
    importc: "LLVMGetNamedFunction".}


proc getFirstFunction*(m: ModuleRef): ValueRef {.importc: "LLVMGetFirstFunction",
    .}


proc getLastFunction*(m: ModuleRef): ValueRef {.importc: "LLVMGetLastFunction",
    .}

proc getNextFunction*(fn: ValueRef): ValueRef {.importc: "LLVMGetNextFunction",
    .}


proc getPreviousFunction*(fn: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousFunction".}

proc setModuleInlineAsm*(m: ModuleRef; `asm`: cstring) {.
    importc: "LLVMSetModuleInlineAsm".}


proc getTypeKind*(ty: TypeRef): TypeKind {.importc: "LLVMGetTypeKind".}

proc typeIsSized*(ty: TypeRef): Bool {.importc: "LLVMTypeIsSized".}


proc getTypeContext*(ty: TypeRef): ContextRef {.importc: "LLVMGetTypeContext",
    .}

proc dumpType*(val: TypeRef) {.importc: "LLVMDumpType".}


proc printTypeToString*(val: TypeRef): cstring {.importc: "LLVMPrintTypeToString",
    .}


proc int1TypeInContext*(c: ContextRef): TypeRef {.importc: "LLVMInt1TypeInContext",
    .}
proc int8TypeInContext*(c: ContextRef): TypeRef {.importc: "LLVMInt8TypeInContext",
    .}
proc int16TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMInt16TypeInContext".}
proc int32TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMInt32TypeInContext".}
proc int64TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMInt64TypeInContext".}
proc int128TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMInt128TypeInContext".}
proc intTypeInContext*(c: ContextRef; numBits: cuint): TypeRef {.
    importc: "LLVMIntTypeInContext".}

proc int1Type*(): TypeRef {.importc: "LLVMInt1Type".}
proc int8Type*(): TypeRef {.importc: "LLVMInt8Type".}
proc int16Type*(): TypeRef {.importc: "LLVMInt16Type".}
proc int32Type*(): TypeRef {.importc: "LLVMInt32Type".}
proc int64Type*(): TypeRef {.importc: "LLVMInt64Type".}
proc int128Type*(): TypeRef {.importc: "LLVMInt128Type".}
proc intType*(numBits: cuint): TypeRef {.importc: "LLVMIntType".}
proc getIntTypeWidth*(integerTy: TypeRef): cuint {.importc: "LLVMGetIntTypeWidth",
    .}

proc halfTypeInContext*(c: ContextRef): TypeRef {.importc: "LLVMHalfTypeInContext",
    .}

proc bFloatTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMBFloatTypeInContext".}


proc floatTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMFloatTypeInContext".}

proc doubleTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMDoubleTypeInContext".}

proc x86FP80TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMX86FP80TypeInContext".}


proc fP128TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMFP128TypeInContext".}

proc pPCFP128TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMPPCFP128TypeInContext".}

proc halfType*(): TypeRef {.importc: "LLVMHalfType".}
proc bFloatType*(): TypeRef {.importc: "LLVMBFloatType".}
proc floatType*(): TypeRef {.importc: "LLVMFloatType".}
proc doubleType*(): TypeRef {.importc: "LLVMDoubleType".}
proc x86FP80Type*(): TypeRef {.importc: "LLVMX86FP80Type".}
proc fP128Type*(): TypeRef {.importc: "LLVMFP128Type".}
proc pPCFP128Type*(): TypeRef {.importc: "LLVMPPCFP128Type".}


proc functionType*(returnType: TypeRef; paramTypes: ptr TypeRef; paramCount: cuint;
                  isVarArg: Bool): TypeRef {.importc: "LLVMFunctionType",
    .}

proc isFunctionVarArg*(functionTy: TypeRef): Bool {.importc: "LLVMIsFunctionVarArg",
    .}

proc getReturnType*(functionTy: TypeRef): TypeRef {.importc: "LLVMGetReturnType",
    .}

proc countParamTypes*(functionTy: TypeRef): cuint {.importc: "LLVMCountParamTypes",
    .}
proc getParamTypes*(functionTy: TypeRef; dest: ptr TypeRef) {.
    importc: "LLVMGetParamTypes".}


proc structTypeInContext*(c: ContextRef; elementTypes: ptr TypeRef;
                         elementCount: cuint; packed: Bool): TypeRef {.
    importc: "LLVMStructTypeInContext".}

proc structType*(elementTypes: ptr TypeRef; elementCount: cuint; packed: Bool): TypeRef {.
    importc: "LLVMStructType".}


proc structCreateNamed*(c: ContextRef; name: cstring): TypeRef {.
    importc: "LLVMStructCreateNamed".}


proc getStructName*(ty: TypeRef): cstring {.importc: "LLVMGetStructName",
                                        .}

proc structSetBody*(structTy: TypeRef; elementTypes: ptr TypeRef; elementCount: cuint;
                   packed: Bool) {.importc: "LLVMStructSetBody".}


proc countStructElementTypes*(structTy: TypeRef): cuint {.
    importc: "LLVMCountStructElementTypes".}

proc getStructElementTypes*(structTy: TypeRef; dest: ptr TypeRef) {.
    importc: "LLVMGetStructElementTypes".}


proc structGetTypeAtIndex*(structTy: TypeRef; i: cuint): TypeRef {.
    importc: "LLVMStructGetTypeAtIndex".}

proc isPackedStruct*(structTy: TypeRef): Bool {.importc: "LLVMIsPackedStruct",
    .}


proc isOpaqueStruct*(structTy: TypeRef): Bool {.importc: "LLVMIsOpaqueStruct",
    .}


proc isLiteralStruct*(structTy: TypeRef): Bool {.importc: "LLVMIsLiteralStruct",
    .}

proc getElementType*(ty: TypeRef): TypeRef {.importc: "LLVMGetElementType",
    .}


proc getSubtypes*(tp: TypeRef; arr: ptr TypeRef) {.importc: "LLVMGetSubtypes",
    .}


proc getNumContainedTypes*(tp: TypeRef): cuint {.
    importc: "LLVMGetNumContainedTypes".}


proc arrayType*(elementType: TypeRef; elementCount: cuint): TypeRef {.
    importc: "LLVMArrayType".}


proc getArrayLength*(arrayTy: TypeRef): cuint {.importc: "LLVMGetArrayLength",
    .}


proc pointerType*(elementType: TypeRef; addressSpace: cuint): TypeRef {.
    importc: "LLVMPointerType".}


proc pointerTypeIsOpaque*(ty: TypeRef): Bool {.importc: "LLVMPointerTypeIsOpaque",
    .}


proc pointerTypeInContext*(c: ContextRef; addressSpace: cuint): TypeRef {.
    importc: "LLVMPointerTypeInContext".}

proc getPointerAddressSpace*(pointerTy: TypeRef): cuint {.
    importc: "LLVMGetPointerAddressSpace".}

proc vectorType*(elementType: TypeRef; elementCount: cuint): TypeRef {.
    importc: "LLVMVectorType".}


proc scalableVectorType*(elementType: TypeRef; elementCount: cuint): TypeRef {.
    importc: "LLVMScalableVectorType".}

proc getVectorSize*(vectorTy: TypeRef): cuint {.importc: "LLVMGetVectorSize",
    .}


proc voidTypeInContext*(c: ContextRef): TypeRef {.importc: "LLVMVoidTypeInContext",
    .}

proc labelTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMLabelTypeInContext".}

proc x86MMXTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMX86MMXTypeInContext".}

proc x86AMXTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMX86AMXTypeInContext".}


proc tokenTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMTokenTypeInContext".}


proc metadataTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMMetadataTypeInContext".}

proc voidType*(): TypeRef {.importc: "LLVMVoidType".}
proc labelType*(): TypeRef {.importc: "LLVMLabelType".}
proc x86MMXType*(): TypeRef {.importc: "LLVMX86MMXType".}
proc x86AMXType*(): TypeRef {.importc: "LLVMX86AMXType".}


template for_Each_Value_Subclass*(`macro`: untyped): untyped =
  `macro`(argument)


proc getValueKind*(val: ValueRef): ValueKind {.importc: "LLVMGetValueKind",
    .}


proc getValueName2*(val: ValueRef; length: ptr csize_t): cstring {.
    importc: "LLVMGetValueName2".}


proc setValueName2*(val: ValueRef; name: cstring; nameLen: csize_t) {.
    importc: "LLVMSetValueName2".}


proc dumpValue*(val: ValueRef) {.importc: "LLVMDumpValue".}


proc printValueToString*(val: ValueRef): cstring {.
    importc: "LLVMPrintValueToString".}

proc replaceAllUsesWith*(oldVal: ValueRef; newVal: ValueRef) {.
    importc: "LLVMReplaceAllUsesWith".}

proc isConstant*(val: ValueRef): Bool {.importc: "LLVMIsConstant".}

proc isUndef*(val: ValueRef): Bool {.importc: "LLVMIsUndef".}

proc isPoison*(val: ValueRef): Bool {.importc: "LLVMIsPoison".}


template declare_Value_Cast*(name: untyped): untyped =
  valueRef


proc isAMDString*(val: ValueRef): ValueRef {.importc: "LLVMIsAMDString",
    .}

proc getValueName*(val: ValueRef): cstring {.importc: "LLVMGetValueName",
    .}

proc setValueName*(val: ValueRef; name: cstring) {.importc: "LLVMSetValueName",
    .}

proc getFirstUse*(val: ValueRef): UseRef {.importc: "LLVMGetFirstUse".}

proc getNextUse*(u: UseRef): UseRef {.importc: "LLVMGetNextUse".}


proc getUser*(u: UseRef): ValueRef {.importc: "LLVMGetUser".}

proc getUsedValue*(u: UseRef): ValueRef {.importc: "LLVMGetUsedValue".}


proc getOperand*(val: ValueRef; index: cuint): ValueRef {.importc: "LLVMGetOperand".}

proc getOperandUse*(val: ValueRef; index: cuint): UseRef {.
    importc: "LLVMGetOperandUse".}

proc setOperand*(user: ValueRef; index: cuint; val: ValueRef) {.
    importc: "LLVMSetOperand".}


proc getNumOperands*(val: ValueRef): cint {.importc: "LLVMGetNumOperands",.}


proc constNull*(ty: TypeRef): ValueRef {.importc: "LLVMConstNull".}


proc constAllOnes*(ty: TypeRef): ValueRef {.importc: "LLVMConstAllOnes",
                                        .}

proc getUndef*(ty: TypeRef): ValueRef {.importc: "LLVMGetUndef".}


proc getPoison*(ty: TypeRef): ValueRef {.importc: "LLVMGetPoison".}

proc isNull*(val: ValueRef): Bool {.importc: "LLVMIsNull".}

proc constPointerNull*(ty: TypeRef): ValueRef {.importc: "LLVMConstPointerNull".}

proc constInt*(intTy: TypeRef; n: culonglong; signExtend: Bool): ValueRef {.
    importc: "LLVMConstInt".}

proc constIntOfArbitraryPrecision*(intTy: TypeRef; numWords: cuint;
                                  words: ptr uint64): ValueRef {.
    importc: "LLVMConstIntOfArbitraryPrecision".}

proc constIntOfString*(intTy: TypeRef; text: cstring; radix: uint8T): ValueRef {.
    importc: "LLVMConstIntOfString".}

proc constIntOfStringAndSize*(intTy: TypeRef; text: cstring; sLen: cuint; radix: uint8T): ValueRef {.
    importc: "LLVMConstIntOfStringAndSize".}

proc constReal*(realTy: TypeRef; n: cdouble): ValueRef {.importc: "LLVMConstReal",
    .}

proc constRealOfString*(realTy: TypeRef; text: cstring): ValueRef {.
    importc: "LLVMConstRealOfString".}


proc constRealOfStringAndSize*(realTy: TypeRef; text: cstring; sLen: cuint): ValueRef {.
    importc: "LLVMConstRealOfStringAndSize".}


proc constIntGetZExtValue*(constantVal: ValueRef): culonglong {.
    importc: "LLVMConstIntGetZExtValue".}

proc constIntGetSExtValue*(constantVal: ValueRef): clonglong {.
    importc: "LLVMConstIntGetSExtValue".}


proc constRealGetDouble*(constantVal: ValueRef; losesInfo: ptr Bool): cdouble {.
    importc: "LLVMConstRealGetDouble".}


proc constStringInContext*(c: ContextRef; str: cstring; length: cuint;
                          dontNullTerminate: Bool): ValueRef {.
    importc: "LLVMConstStringInContext".}


proc constString*(str: cstring; length: cuint; dontNullTerminate: Bool): ValueRef {.
    importc: "LLVMConstString".}

proc isConstantString*(c: ValueRef): Bool {.importc: "LLVMIsConstantString",
                                        .}


proc getAsString*(c: ValueRef; length: ptr csize_t): cstring {.
    importc: "LLVMGetAsString".}


proc constStructInContext*(c: ContextRef; constantVals: ptr ValueRef; count: cuint;
                          packed: Bool): ValueRef {.
    importc: "LLVMConstStructInContext".}


proc constStruct*(constantVals: ptr ValueRef; count: cuint; packed: Bool): ValueRef {.
    importc: "LLVMConstStruct".}


proc constArray*(elementTy: TypeRef; constantVals: ptr ValueRef; length: cuint): ValueRef {.
    importc: "LLVMConstArray".}

proc constNamedStruct*(structTy: TypeRef; constantVals: ptr ValueRef; count: cuint): ValueRef {.
    importc: "LLVMConstNamedStruct".}


proc getAggregateElement*(c: ValueRef; idx: cuint): ValueRef {.
    importc: "LLVMGetAggregateElement".}

proc getElementAsConstant*(c: ValueRef; idx: cuint): ValueRef {.
    importc: "LLVMGetElementAsConstant".}

proc constVector*(scalarConstantVals: ptr ValueRef; size: cuint): ValueRef {.
    importc: "LLVMConstVector".}

proc getConstOpcode*(constantVal: ValueRef): Opcode {.importc: "LLVMGetConstOpcode",
    .}
proc alignOf*(ty: TypeRef): ValueRef {.importc: "LLVMAlignOf".}
proc sizeOfX*(ty: TypeRef): ValueRef {.importc: "LLVMSizeOf".}
proc constNeg*(constantVal: ValueRef): ValueRef {.importc: "LLVMConstNeg",
    .}
proc constNSWNeg*(constantVal: ValueRef): ValueRef {.importc: "LLVMConstNSWNeg",
    .}
proc constNUWNeg*(constantVal: ValueRef): ValueRef {.importc: "LLVMConstNUWNeg",
    .}
proc constFNeg*(constantVal: ValueRef): ValueRef {.importc: "LLVMConstFNeg",
    .}
proc constNot*(constantVal: ValueRef): ValueRef {.importc: "LLVMConstNot",
    .}
proc constAdd*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstAdd".}
proc constNSWAdd*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNSWAdd".}
proc constNUWAdd*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNUWAdd".}
proc constSub*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstSub".}
proc constNSWSub*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNSWSub".}
proc constNUWSub*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNUWSub".}
proc constMul*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstMul".}
proc constNSWMul*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNSWMul".}
proc constNUWMul*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNUWMul".}
proc constAnd*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstAnd".}
proc constOr*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstOr".}
proc constXor*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstXor".}
proc constICmp*(predicate: IntPredicate; lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstICmp".}
proc constFCmp*(predicate: RealPredicate; lHSConstant: ValueRef;
               rHSConstant: ValueRef): ValueRef {.importc: "LLVMConstFCmp",
    .}
proc constShl*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstShl".}
proc constLShr*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstLShr".}
proc constAShr*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstAShr".}
proc constGEP*(constantVal: ValueRef; constantIndices: ptr ValueRef; numIndices: cuint): ValueRef {.
    importc: "LLVMConstGEP".}
proc constGEP2*(ty: TypeRef; constantVal: ValueRef; constantIndices: ptr ValueRef;
               numIndices: cuint): ValueRef {.importc: "LLVMConstGEP2",
    .}
proc constInBoundsGEP*(ty: TypeRef; constantVal: ValueRef;
                       constantIndices: ptr ValueRef; numIndices: cuint): ValueRef {.
    importc: "LLVMConstInBoundsGEP".}
proc constInBoundsGEP2*(ty: TypeRef; constantVal: ValueRef;
                       constantIndices: ptr ValueRef; numIndices: cuint): ValueRef {.
    importc: "LLVMConstInBoundsGEP2".}
proc constTrunc*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstTrunc".}
proc constSExt*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstSExt".}
proc constZExt*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstZExt".}
proc constFPTrunc*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstFPTrunc".}
proc constFPExt*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstFPExt".}
proc constUIToFP*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstUIToFP".}
proc constSIToFP*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstSIToFP".}
proc constFPToUI*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstFPToUI".}
proc constFPToSI*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstFPToSI".}
proc constPtrToInt*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstPtrToInt".}
proc constIntToPtr*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstIntToPtr".}
proc constBitCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstBitCast".}
proc constAddrSpaceCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstAddrSpaceCast".}
proc constZExtOrBitCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstZExtOrBitCast".}
proc constSExtOrBitCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstSExtOrBitCast".}
proc constTruncOrBitCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstTruncOrBitCast".}
proc constPointerCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstPointerCast".}
proc constIntCast*(constantVal: ValueRef; toType: TypeRef; isSigned: Bool): ValueRef {.
    importc: "LLVMConstIntCast".}
proc constFPCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstFPCast".}
proc constSelect*(constantCondition: ValueRef; constantIfTrue: ValueRef;
                 constantIfFalse: ValueRef): ValueRef {.importc: "LLVMConstSelect",
    .}
proc constExtractElement*(vectorConstant: ValueRef; indexConstant: ValueRef): ValueRef {.
    importc: "LLVMConstExtractElement".}
proc constInsertElement*(vectorConstant: ValueRef; elementValueConstant: ValueRef;
                        indexConstant: ValueRef): ValueRef {.
    importc: "LLVMConstInsertElement".}
proc constShuffleVector*(vectorAConstant: ValueRef; vectorBConstant: ValueRef;
                        maskConstant: ValueRef): ValueRef {.
    importc: "LLVMConstShuffleVector".}
proc blockAddress*(f: ValueRef; bb: BasicBlockRef): ValueRef {.
    importc: "LLVMBlockAddress".}

proc constInlineAsm*(ty: TypeRef; asmString: cstring; constraints: cstring;
                    hasSideEffects: Bool; isAlignStack: Bool): ValueRef {.
    importc: "LLVMConstInlineAsm".}

proc getGlobalParent*(global: ValueRef): ModuleRef {.importc: "LLVMGetGlobalParent",
    .}
proc isDeclaration*(global: ValueRef): Bool {.importc: "LLVMIsDeclaration",
    .}
proc getLinkage*(global: ValueRef): Linkage {.importc: "LLVMGetLinkage",
    .}
proc setLinkage*(global: ValueRef; linkage: Linkage) {.importc: "LLVMSetLinkage",
    .}
proc getSection*(global: ValueRef): cstring {.importc: "LLVMGetSection",
    .}
proc setSection*(global: ValueRef; section: cstring) {.importc: "LLVMSetSection",
    .}
proc getVisibility*(global: ValueRef): Visibility {.importc: "LLVMGetVisibility",
    .}
proc setVisibility*(global: ValueRef; viz: Visibility) {.
    importc: "LLVMSetVisibility".}
proc getDLLStorageClass*(global: ValueRef): DLLStorageClass {.
    importc: "LLVMGetDLLStorageClass".}
proc setDLLStorageClass*(global: ValueRef; class: DLLStorageClass) {.
    importc: "LLVMSetDLLStorageClass".}
proc getUnnamedAddress*(global: ValueRef): UnnamedAddr {.
    importc: "LLVMGetUnnamedAddress".}
proc setUnnamedAddress*(global: ValueRef; unnamedAddr: UnnamedAddr) {.
    importc: "LLVMSetUnnamedAddress".}
proc globalGetValueType*(global: ValueRef): TypeRef {.
    importc: "LLVMGlobalGetValueType".}

proc hasUnnamedAddr*(global: ValueRef): Bool {.importc: "LLVMHasUnnamedAddr",
    .}

proc setUnnamedAddr*(global: ValueRef; hasUnnamedAddr: Bool) {.
    importc: "LLVMSetUnnamedAddr".}

proc getAlignment*(v: ValueRef): cuint {.importc: "LLVMGetAlignment".}


proc setAlignment*(v: ValueRef; bytes: cuint) {.importc: "LLVMSetAlignment",
    .}

proc globalSetMetadata*(global: ValueRef; kind: cuint; md: MetadataRef) {.
    importc: "LLVMGlobalSetMetadata".}

proc globalEraseMetadata*(global: ValueRef; kind: cuint) {.
    importc: "LLVMGlobalEraseMetadata".}

proc globalClearMetadata*(global: ValueRef) {.importc: "LLVMGlobalClearMetadata",
    .}

proc globalCopyAllMetadata*(value: ValueRef; numEntries: ptr csize_t): ptr ValueMetadataEntry {.
    importc: "LLVMGlobalCopyAllMetadata".}

proc disposeValueMetadataEntries*(entries: ptr ValueMetadataEntry) {.
    importc: "LLVMDisposeValueMetadataEntries".}

proc valueMetadataEntriesGetKind*(entries: ptr ValueMetadataEntry; index: cuint): cuint {.
    importc: "LLVMValueMetadataEntriesGetKind".}

proc valueMetadataEntriesGetMetadata*(entries: ptr ValueMetadataEntry; index: cuint): MetadataRef {.
    importc: "LLVMValueMetadataEntriesGetMetadata".}

proc addGlobal*(m: ModuleRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMAddGlobal".}
proc addGlobalInAddressSpace*(m: ModuleRef; ty: TypeRef; name: cstring;
                             addressSpace: cuint): ValueRef {.
    importc: "LLVMAddGlobalInAddressSpace".}
proc getNamedGlobal*(m: ModuleRef; name: cstring): ValueRef {.
    importc: "LLVMGetNamedGlobal".}
proc getFirstGlobal*(m: ModuleRef): ValueRef {.importc: "LLVMGetFirstGlobal",
    .}
proc getLastGlobal*(m: ModuleRef): ValueRef {.importc: "LLVMGetLastGlobal",
    .}
proc getNextGlobal*(globalVar: ValueRef): ValueRef {.importc: "LLVMGetNextGlobal",
    .}
proc getPreviousGlobal*(globalVar: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousGlobal".}
proc deleteGlobal*(globalVar: ValueRef) {.importc: "LLVMDeleteGlobal",
                                       .}
proc getInitializer*(globalVar: ValueRef): ValueRef {.importc: "LLVMGetInitializer",
    .}
proc setInitializer*(globalVar: ValueRef; constantVal: ValueRef) {.
    importc: "LLVMSetInitializer".}
proc isThreadLocal*(globalVar: ValueRef): Bool {.importc: "LLVMIsThreadLocal",
    .}
proc setThreadLocal*(globalVar: ValueRef; isThreadLocal: Bool) {.
    importc: "LLVMSetThreadLocal".}
proc isGlobalConstant*(globalVar: ValueRef): Bool {.importc: "LLVMIsGlobalConstant",
    .}
proc setGlobalConstant*(globalVar: ValueRef; isConstant: Bool) {.
    importc: "LLVMSetGlobalConstant".}
proc getThreadLocalMode*(globalVar: ValueRef): ThreadLocalMode {.
    importc: "LLVMGetThreadLocalMode".}
proc setThreadLocalMode*(globalVar: ValueRef; mode: ThreadLocalMode) {.
    importc: "LLVMSetThreadLocalMode".}
proc isExternallyInitialized*(globalVar: ValueRef): Bool {.
    importc: "LLVMIsExternallyInitialized".}
proc setExternallyInitialized*(globalVar: ValueRef; isExtInit: Bool) {.
    importc: "LLVMSetExternallyInitialized".}


proc addAlias*(m: ModuleRef; ty: TypeRef; aliasee: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMAddAlias".}


proc addAlias2*(m: ModuleRef; valueTy: TypeRef; addrSpace: cuint; aliasee: ValueRef;
               name: cstring): ValueRef {.importc: "LLVMAddAlias2".}

proc getNamedGlobalAlias*(m: ModuleRef; name: cstring; nameLen: csize_t): ValueRef {.
    importc: "LLVMGetNamedGlobalAlias".}


proc getFirstGlobalAlias*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetFirstGlobalAlias".}

proc getLastGlobalAlias*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetLastGlobalAlias".}


proc getNextGlobalAlias*(ga: ValueRef): ValueRef {.
    importc: "LLVMGetNextGlobalAlias".}


proc getPreviousGlobalAlias*(ga: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousGlobalAlias".}

proc aliasGetAliasee*(alias: ValueRef): ValueRef {.importc: "LLVMAliasGetAliasee",
    .}


proc aliasSetAliasee*(alias: ValueRef; aliasee: ValueRef) {.
    importc: "LLVMAliasSetAliasee".}


proc deleteFunction*(fn: ValueRef) {.importc: "LLVMDeleteFunction".}

proc hasPersonalityFn*(fn: ValueRef): Bool {.importc: "LLVMHasPersonalityFn",
    .}


proc getPersonalityFn*(fn: ValueRef): ValueRef {.importc: "LLVMGetPersonalityFn",
    .}

proc setPersonalityFn*(fn: ValueRef; personalityFn: ValueRef) {.
    importc: "LLVMSetPersonalityFn".}


proc lookupIntrinsicID*(name: cstring; nameLen: csize_t): cuint {.
    importc: "LLVMLookupIntrinsicID".}

proc getIntrinsicID*(fn: ValueRef): cuint {.importc: "LLVMGetIntrinsicID",
                                        .}


proc getIntrinsicDeclaration*(`mod`: ModuleRef; id: cuint; paramTypes: ptr TypeRef;
                             paramCount: csize_t): ValueRef {.
    importc: "LLVMGetIntrinsicDeclaration".}


proc intrinsicGetType*(ctx: ContextRef; id: cuint; paramTypes: ptr TypeRef;
                      paramCount: csize_t): TypeRef {.
    importc: "LLVMIntrinsicGetType".}


proc intrinsicGetName*(id: cuint; nameLength: ptr csize_t): cstring {.
    importc: "LLVMIntrinsicGetName".}

proc intrinsicCopyOverloadedName*(id: cuint; paramTypes: ptr TypeRef;
                                 paramCount: csize_t; nameLength: ptr csize_t): cstring {.
    importc: "LLVMIntrinsicCopyOverloadedName".}


proc intrinsicCopyOverloadedName2*(`mod`: ModuleRef; id: cuint;
                                  paramTypes: ptr TypeRef; paramCount: csize_t;
                                  nameLength: ptr csize_t): cstring {.
    importc: "LLVMIntrinsicCopyOverloadedName2".}


proc intrinsicIsOverloaded*(id: cuint): Bool {.importc: "LLVMIntrinsicIsOverloaded",
    .}


proc getFunctionCallConv*(fn: ValueRef): cuint {.importc: "LLVMGetFunctionCallConv",
    .}


proc setFunctionCallConv*(fn: ValueRef; cc: cuint) {.
    importc: "LLVMSetFunctionCallConv".}


proc getGC*(fn: ValueRef): cstring {.importc: "LLVMGetGC".}


proc setGC*(fn: ValueRef; name: cstring) {.importc: "LLVMSetGC".}


proc addAttributeAtIndex*(f: ValueRef; idx: AttributeIndex; a: AttributeRef) {.
    importc: "LLVMAddAttributeAtIndex".}
proc getAttributeCountAtIndex*(f: ValueRef; idx: AttributeIndex): cuint {.
    importc: "LLVMGetAttributeCountAtIndex".}
proc getAttributesAtIndex*(f: ValueRef; idx: AttributeIndex; attrs: ptr AttributeRef) {.
    importc: "LLVMGetAttributesAtIndex".}
proc getEnumAttributeAtIndex*(f: ValueRef; idx: AttributeIndex; kindID: cuint): AttributeRef {.
    importc: "LLVMGetEnumAttributeAtIndex".}
proc getStringAttributeAtIndex*(f: ValueRef; idx: AttributeIndex; k: cstring;
                               kLen: cuint): AttributeRef {.
    importc: "LLVMGetStringAttributeAtIndex".}
proc removeEnumAttributeAtIndex*(f: ValueRef; idx: AttributeIndex; kindID: cuint) {.
    importc: "LLVMRemoveEnumAttributeAtIndex".}
proc removeStringAttributeAtIndex*(f: ValueRef; idx: AttributeIndex; k: cstring;
                                  kLen: cuint) {.
    importc: "LLVMRemoveStringAttributeAtIndex".}


proc addTargetDependentFunctionAttr*(fn: ValueRef; a: cstring; v: cstring) {.
    importc: "LLVMAddTargetDependentFunctionAttr".}

proc countParams*(fn: ValueRef): cuint {.importc: "LLVMCountParams".}

proc getParams*(fn: ValueRef; params: ptr ValueRef) {.importc: "LLVMGetParams",
    .}


proc getParam*(fn: ValueRef; index: cuint): ValueRef {.importc: "LLVMGetParam",
    .}


proc getParamParent*(inst: ValueRef): ValueRef {.importc: "LLVMGetParamParent",
    .}


proc getFirstParam*(fn: ValueRef): ValueRef {.importc: "LLVMGetFirstParam",
    .}

proc getLastParam*(fn: ValueRef): ValueRef {.importc: "LLVMGetLastParam",
    .}


proc getNextParam*(arg: ValueRef): ValueRef {.importc: "LLVMGetNextParam",
    .}

proc getPreviousParam*(arg: ValueRef): ValueRef {.importc: "LLVMGetPreviousParam",
    .}


proc setParamAlignment*(arg: ValueRef; align: cuint) {.
    importc: "LLVMSetParamAlignment".}


proc addGlobalIFunc*(m: ModuleRef; name: cstring; nameLen: csize_t; ty: TypeRef;
                    addrSpace: cuint; resolver: ValueRef): ValueRef {.
    importc: "LLVMAddGlobalIFunc".}


proc getNamedGlobalIFunc*(m: ModuleRef; name: cstring; nameLen: csize_t): ValueRef {.
    importc: "LLVMGetNamedGlobalIFunc".}

proc getFirstGlobalIFunc*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetFirstGlobalIFunc".}


proc getLastGlobalIFunc*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetLastGlobalIFunc".}


proc getNextGlobalIFunc*(iFunc: ValueRef): ValueRef {.
    importc: "LLVMGetNextGlobalIFunc".}

proc getPreviousGlobalIFunc*(iFunc: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousGlobalIFunc".}

proc getGlobalIFuncResolver*(iFunc: ValueRef): ValueRef {.
    importc: "LLVMGetGlobalIFuncResolver".}

proc setGlobalIFuncResolver*(iFunc: ValueRef; resolver: ValueRef) {.
    importc: "LLVMSetGlobalIFuncResolver".}

proc eraseGlobalIFunc*(iFunc: ValueRef) {.importc: "LLVMEraseGlobalIFunc",.}

proc removeGlobalIFunc*(iFunc: ValueRef) {.importc: "LLVMRemoveGlobalIFunc",.}


proc mDStringInContext2*(c: ContextRef; str: cstring; sLen: csize_t): MetadataRef {.
    importc: "LLVMMDStringInContext2".}

proc mDNodeInContext2*(c: ContextRef; mDs: ptr MetadataRef; count: csize_t): MetadataRef {.
    importc: "LLVMMDNodeInContext2".}


proc metadataAsValue*(c: ContextRef; md: MetadataRef): ValueRef {.
    importc: "LLVMMetadataAsValue".}
proc valueAsMetadata*(val: ValueRef): MetadataRef {.importc: "LLVMValueAsMetadata",
    .}

proc getMDString*(v: ValueRef; length: ptr cuint): cstring {.
    importc: "LLVMGetMDString".}

proc getMDNodeNumOperands*(v: ValueRef): cuint {.
    importc: "LLVMGetMDNodeNumOperands".}

proc getMDNodeOperands*(v: ValueRef; dest: ptr ValueRef) {.
    importc: "LLVMGetMDNodeOperands".}

proc mDStringInContext*(c: ContextRef; str: cstring; sLen: cuint): ValueRef {.
    importc: "LLVMMDStringInContext".}

proc mDString*(str: cstring; sLen: cuint): ValueRef {.importc: "LLVMMDString",
    .}

proc mDNodeInContext*(c: ContextRef; vals: ptr ValueRef; count: cuint): ValueRef {.
    importc: "LLVMMDNodeInContext".}

proc mDNode*(vals: ptr ValueRef; count: cuint): ValueRef {.importc: "LLVMMDNode", .}

proc basicBlockAsValue*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMBasicBlockAsValue".}

proc valueIsBasicBlock*(val: ValueRef): Bool {.importc: "LLVMValueIsBasicBlock",
    .}

proc valueAsBasicBlock*(val: ValueRef): BasicBlockRef {.
    importc: "LLVMValueAsBasicBlock".}

proc getBasicBlockName*(bb: BasicBlockRef): cstring {.
    importc: "LLVMGetBasicBlockName".}

proc getBasicBlockParent*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetBasicBlockParent".}

proc getBasicBlockTerminator*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetBasicBlockTerminator".}

proc countBasicBlocks*(fn: ValueRef): cuint {.importc: "LLVMCountBasicBlocks",
    .}

proc getBasicBlocks*(fn: ValueRef; basicBlocks: ptr BasicBlockRef) {.
    importc: "LLVMGetBasicBlocks".}

proc getFirstBasicBlock*(fn: ValueRef): BasicBlockRef {.
    importc: "LLVMGetFirstBasicBlock".}

proc getLastBasicBlock*(fn: ValueRef): BasicBlockRef {.
    importc: "LLVMGetLastBasicBlock".}

proc getNextBasicBlock*(bb: BasicBlockRef): BasicBlockRef {.
    importc: "LLVMGetNextBasicBlock".}

proc getPreviousBasicBlock*(bb: BasicBlockRef): BasicBlockRef {.
    importc: "LLVMGetPreviousBasicBlock".}
proc getEntryBasicBlock*(fn: ValueRef): BasicBlockRef {.
    importc: "LLVMGetEntryBasicBlock".}

proc insertExistingBasicBlockAfterInsertBlock*(builder: BuilderRef;
    bb: BasicBlockRef) {.importc: "LLVMInsertExistingBasicBlockAfterInsertBlock".}

proc appendExistingBasicBlock*(fn: ValueRef; bb: BasicBlockRef) {.
    importc: "LLVMAppendExistingBasicBlock".}

proc createBasicBlockInContext*(c: ContextRef; name: cstring): BasicBlockRef {.
    importc: "LLVMCreateBasicBlockInContext".}

proc appendBasicBlockInContext*(c: ContextRef; fn: ValueRef; name: cstring): BasicBlockRef {.
    importc: "LLVMAppendBasicBlockInContext".}


proc appendBasicBlock*(fn: ValueRef; name: cstring): BasicBlockRef {.
    importc: "LLVMAppendBasicBlock".}

proc insertBasicBlockInContext*(c: ContextRef; bb: BasicBlockRef; name: cstring): BasicBlockRef {.
    importc: "LLVMInsertBasicBlockInContext".}

proc insertBasicBlock*(insertBeforeBB: BasicBlockRef; name: cstring): BasicBlockRef {.
    importc: "LLVMInsertBasicBlock".}


proc deleteBasicBlock*(bb: BasicBlockRef) {.importc: "LLVMDeleteBasicBlock",
    .}

proc removeBasicBlockFromParent*(bb: BasicBlockRef) {.
    importc: "LLVMRemoveBasicBlockFromParent".}


proc moveBasicBlockBefore*(bb: BasicBlockRef; movePos: BasicBlockRef) {.
    importc: "LLVMMoveBasicBlockBefore".}


proc moveBasicBlockAfter*(bb: BasicBlockRef; movePos: BasicBlockRef) {.
    importc: "LLVMMoveBasicBlockAfter".}

proc getFirstInstruction*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetFirstInstruction".}

proc getLastInstruction*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetLastInstruction".}
proc hasMetadata*(val: ValueRef): cint {.importc: "LLVMHasMetadata".}


proc getMetadata*(val: ValueRef; kindID: cuint): ValueRef {.
    importc: "LLVMGetMetadata".}

proc setMetadata*(val: ValueRef; kindID: cuint; node: ValueRef) {.
    importc: "LLVMSetMetadata".}

proc instructionGetAllMetadataOtherThanDebugLoc*(instr: ValueRef;
    numEntries: ptr csize_t): ptr ValueMetadataEntry {.
    importc: "LLVMInstructionGetAllMetadataOtherThanDebugLoc".}


proc getInstructionParent*(inst: ValueRef): BasicBlockRef {.
    importc: "LLVMGetInstructionParent".}


proc getNextInstruction*(inst: ValueRef): ValueRef {.
    importc: "LLVMGetNextInstruction".}


proc getPreviousInstruction*(inst: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousInstruction".}

proc instructionRemoveFromParent*(inst: ValueRef) {.
    importc: "LLVMInstructionRemoveFromParent".}

proc instructionEraseFromParent*(inst: ValueRef) {.
    importc: "LLVMInstructionEraseFromParent".}

proc deleteInstruction*(inst: ValueRef) {.importc: "LLVMDeleteInstruction".}

proc getInstructionOpcode*(inst: ValueRef): Opcode {.
    importc: "LLVMGetInstructionOpcode".}


proc getICmpPredicate*(inst: ValueRef): IntPredicate {.
    importc: "LLVMGetICmpPredicate".}

proc getFCmpPredicate*(inst: ValueRef): RealPredicate {.
    importc: "LLVMGetFCmpPredicate".}


proc instructionClone*(inst: ValueRef): ValueRef {.importc: "LLVMInstructionClone",
    .}


proc isATerminatorInst*(inst: ValueRef): ValueRef {.
    importc: "LLVMIsATerminatorInst".}

proc getNumArgOperands*(instr: ValueRef): cuint {.importc: "LLVMGetNumArgOperands",
    .}

proc setInstructionCallConv*(instr: ValueRef; cc: cuint) {.
    importc: "LLVMSetInstructionCallConv".}

proc getInstructionCallConv*(instr: ValueRef): cuint {.
    importc: "LLVMGetInstructionCallConv".}
proc setInstrParamAlignment*(instr: ValueRef; idx: AttributeIndex; align: cuint) {.
    importc: "LLVMSetInstrParamAlignment".}
proc addCallSiteAttribute*(c: ValueRef; idx: AttributeIndex; a: AttributeRef) {.
    importc: "LLVMAddCallSiteAttribute".}
proc getCallSiteAttributeCount*(c: ValueRef; idx: AttributeIndex): cuint {.
    importc: "LLVMGetCallSiteAttributeCount".}
proc getCallSiteAttributes*(c: ValueRef; idx: AttributeIndex; attrs: ptr AttributeRef) {.
    importc: "LLVMGetCallSiteAttributes".}
proc getCallSiteEnumAttribute*(c: ValueRef; idx: AttributeIndex; kindID: cuint): AttributeRef {.
    importc: "LLVMGetCallSiteEnumAttribute".}
proc getCallSiteStringAttribute*(c: ValueRef; idx: AttributeIndex; k: cstring;
                                kLen: cuint): AttributeRef {.
    importc: "LLVMGetCallSiteStringAttribute".}
proc removeCallSiteEnumAttribute*(c: ValueRef; idx: AttributeIndex; kindID: cuint) {.
    importc: "LLVMRemoveCallSiteEnumAttribute".}
proc removeCallSiteStringAttribute*(c: ValueRef; idx: AttributeIndex; k: cstring;
                                   kLen: cuint) {.
    importc: "LLVMRemoveCallSiteStringAttribute".}

proc getCalledFunctionType*(c: ValueRef): TypeRef {.
    importc: "LLVMGetCalledFunctionType".}

proc getCalledValue*(instr: ValueRef): ValueRef {.importc: "LLVMGetCalledValue",
    .}

proc isTailCall*(callInst: ValueRef): Bool {.importc: "LLVMIsTailCall",
    .}


proc setTailCall*(callInst: ValueRef; isTailCall: Bool) {.importc: "LLVMSetTailCall",
    .}

proc getNormalDest*(invokeInst: ValueRef): BasicBlockRef {.
    importc: "LLVMGetNormalDest".}


proc getUnwindDest*(invokeInst: ValueRef): BasicBlockRef {.
    importc: "LLVMGetUnwindDest".}


proc setNormalDest*(invokeInst: ValueRef; b: BasicBlockRef) {.
    importc: "LLVMSetNormalDest".}


proc setUnwindDest*(invokeInst: ValueRef; b: BasicBlockRef) {.
    importc: "LLVMSetUnwindDest".}

proc getNumSuccessors*(term: ValueRef): cuint {.importc: "LLVMGetNumSuccessors",
    .}


proc getSuccessor*(term: ValueRef; i: cuint): BasicBlockRef {.
    importc: "LLVMGetSuccessor".}


proc setSuccessor*(term: ValueRef; i: cuint; `block`: BasicBlockRef) {.importc: "LLVMSetSuccessor".}

proc isConditional*(branch: ValueRef): Bool {.importc: "LLVMIsConditional",.}

proc getCondition*(branch: ValueRef): ValueRef {.importc: "LLVMGetCondition",.}

proc setCondition*(branch: ValueRef; cond: ValueRef) {.importc: "LLVMSetCondition".}

proc getSwitchDefaultDest*(switchInstr: ValueRef): BasicBlockRef {.
    importc: "LLVMGetSwitchDefaultDest".}

proc getAllocatedType*(alloca: ValueRef): TypeRef {.importc: "LLVMGetAllocatedType".}

proc isInBounds*(gep: ValueRef): Bool {.importc: "LLVMIsInBounds".}

proc setIsInBounds*(gep: ValueRef; inBounds: Bool) {.importc: "LLVMSetIsInBounds".}

proc getGEPSourceElementType*(gep: ValueRef): TypeRef {.
    importc: "LLVMGetGEPSourceElementType".}

proc addIncoming*(phiNode: ValueRef; incomingValues: ptr ValueRef;
                 incomingBlocks: ptr BasicBlockRef; count: cuint) {.importc: "LLVMAddIncoming".}

proc countIncoming*(phiNode: ValueRef): cuint {.importc: "LLVMCountIncoming",.}


proc getIncomingValue*(phiNode: ValueRef; index: cuint): ValueRef {.importc: "LLVMGetIncomingValue".}


proc getIncomingBlock*(phiNode: ValueRef; index: cuint): BasicBlockRef {.importc: "LLVMGetIncomingBlock".}


proc getNumIndices*(inst: ValueRef): cuint {.importc: "LLVMGetNumIndices".}


proc getIndices*(inst: ValueRef): ptr cuint {.importc: "LLVMGetIndices".}

proc createBuilderInContext*(c: ContextRef): BuilderRef {.
    importc: "LLVMCreateBuilderInContext".}
proc createBuilder*(): BuilderRef {.importc: "LLVMCreateBuilder".}
proc positionBuilder*(builder: BuilderRef; `block`: BasicBlockRef; instr: ValueRef) {.
    importc: "LLVMPositionBuilder".}
proc positionBuilderBefore*(builder: BuilderRef; instr: ValueRef) {.
    importc: "LLVMPositionBuilderBefore".}
proc positionBuilderAtEnd*(builder: BuilderRef; `block`: BasicBlockRef) {.
    importc: "LLVMPositionBuilderAtEnd".}
proc getInsertBlock*(builder: BuilderRef): BasicBlockRef {.
    importc: "LLVMGetInsertBlock".}
proc clearInsertionPosition*(builder: BuilderRef) {.
    importc: "LLVMClearInsertionPosition".}
proc insertIntoBuilder*(builder: BuilderRef; instr: ValueRef) {.
    importc: "LLVMInsertIntoBuilder".}
proc insertIntoBuilderWithName*(builder: BuilderRef; instr: ValueRef; name: cstring) {.
    importc: "LLVMInsertIntoBuilderWithName".}
proc disposeBuilder*(builder: BuilderRef) {.importc: "LLVMDisposeBuilder",.}

proc getCurrentDebugLocation2*(builder: BuilderRef): MetadataRef {.
    importc: "LLVMGetCurrentDebugLocation2".}

proc setCurrentDebugLocation2*(builder: BuilderRef; loc: MetadataRef) {.
    importc: "LLVMSetCurrentDebugLocation2".}

proc setInstDebugLocation*(builder: BuilderRef; inst: ValueRef) {.
    importc: "LLVMSetInstDebugLocation".}

proc addMetadataToInst*(builder: BuilderRef; inst: ValueRef) {.
    importc: "LLVMAddMetadataToInst".}

proc builderGetDefaultFPMathTag*(builder: BuilderRef): MetadataRef {.
    importc: "LLVMBuilderGetDefaultFPMathTag".}

proc builderSetDefaultFPMathTag*(builder: BuilderRef; fPMathTag: MetadataRef) {.
    importc: "LLVMBuilderSetDefaultFPMathTag".}

proc setCurrentDebugLocation*(builder: BuilderRef; L: ValueRef) {.
    importc: "LLVMSetCurrentDebugLocation".}

proc getCurrentDebugLocation*(builder: BuilderRef): ValueRef {.
    importc: "LLVMGetCurrentDebugLocation".}

proc buildRetVoid*(a1: BuilderRef): ValueRef {.importc: "LLVMBuildRetVoid",
    .}
proc buildRet*(a1: BuilderRef; v: ValueRef): ValueRef {.importc: "LLVMBuildRet",
    .}
proc buildAggregateRet*(a1: BuilderRef; retVals: ptr ValueRef; n: cuint): ValueRef {.
    importc: "LLVMBuildAggregateRet".}
proc buildBr*(a1: BuilderRef; dest: BasicBlockRef): ValueRef {.importc: "LLVMBuildBr",
    .}
proc buildCondBr*(a1: BuilderRef; `if`: ValueRef; then: BasicBlockRef;
                 `else`: BasicBlockRef): ValueRef {.importc: "LLVMBuildCondBr",
    .}
proc buildSwitch*(a1: BuilderRef; v: ValueRef; `else`: BasicBlockRef; numCases: cuint): ValueRef {.
    importc: "LLVMBuildSwitch".}
proc buildIndirectBr*(b: BuilderRef; `addr`: ValueRef; numDests: cuint): ValueRef {.
    importc: "LLVMBuildIndirectBr".}
proc buildInvoke2*(a1: BuilderRef; ty: TypeRef; fn: ValueRef; args: ptr ValueRef;
                  numArgs: cuint; then: BasicBlockRef; catch: BasicBlockRef;
                  name: cstring): ValueRef {.importc: "LLVMBuildInvoke2",
    .}
proc buildUnreachable*(a1: BuilderRef): ValueRef {.importc: "LLVMBuildUnreachable",
    .}

proc buildResume*(b: BuilderRef; exn: ValueRef): ValueRef {.
    importc: "LLVMBuildResume".}
proc buildLandingPad*(b: BuilderRef; ty: TypeRef; persFn: ValueRef; numClauses: cuint;
                     name: cstring): ValueRef {.importc: "LLVMBuildLandingPad",
    .}
proc buildCleanupRet*(b: BuilderRef; catchPad: ValueRef; bb: BasicBlockRef): ValueRef {.
    importc: "LLVMBuildCleanupRet".}
proc buildCatchRet*(b: BuilderRef; catchPad: ValueRef; bb: BasicBlockRef): ValueRef {.
    importc: "LLVMBuildCatchRet".}
proc buildCatchPad*(b: BuilderRef; parentPad: ValueRef; args: ptr ValueRef;
                   numArgs: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildCatchPad".}
proc buildCleanupPad*(b: BuilderRef; parentPad: ValueRef; args: ptr ValueRef;
                     numArgs: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildCleanupPad".}
proc buildCatchSwitch*(b: BuilderRef; parentPad: ValueRef; unwindBB: BasicBlockRef;
                      numHandlers: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildCatchSwitch".}

proc addCase*(switch: ValueRef; onVal: ValueRef; dest: BasicBlockRef) {.
    importc: "LLVMAddCase".}

proc addDestination*(indirectBr: ValueRef; dest: BasicBlockRef) {.
    importc: "LLVMAddDestination".}

proc getNumClauses*(landingPad: ValueRef): cuint {.importc: "LLVMGetNumClauses",
    .}

proc getClause*(landingPad: ValueRef; idx: cuint): ValueRef {.
    importc: "LLVMGetClause".}

proc addClause*(landingPad: ValueRef; clauseVal: ValueRef) {.
    importc: "LLVMAddClause".}

proc isCleanup*(landingPad: ValueRef): Bool {.importc: "LLVMIsCleanup",
    .}

proc setCleanup*(landingPad: ValueRef; val: Bool) {.importc: "LLVMSetCleanup",
    .}

proc addHandler*(catchSwitch: ValueRef; dest: BasicBlockRef) {.
    importc: "LLVMAddHandler".}

proc getNumHandlers*(catchSwitch: ValueRef): cuint {.importc: "LLVMGetNumHandlers",
    .}

proc getHandlers*(catchSwitch: ValueRef; handlers: ptr BasicBlockRef) {.
    importc: "LLVMGetHandlers".}

proc getArgOperand*(funclet: ValueRef; i: cuint): ValueRef {.
    importc: "LLVMGetArgOperand".}

proc setArgOperand*(funclet: ValueRef; i: cuint; value: ValueRef) {.
    importc: "LLVMSetArgOperand".}

proc getParentCatchSwitch*(catchPad: ValueRef): ValueRef {.
    importc: "LLVMGetParentCatchSwitch".}

proc setParentCatchSwitch*(catchPad: ValueRef; catchSwitch: ValueRef) {.
    importc: "LLVMSetParentCatchSwitch".}

proc buildAdd*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildAdd".}
proc buildNSWAdd*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNSWAdd".}
proc buildNUWAdd*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNUWAdd".}
proc buildFAdd*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFAdd".}
proc buildSub*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSub".}
proc buildNSWSub*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNSWSub".}
proc buildNUWSub*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNUWSub".}
proc buildFSub*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFSub".}
proc buildMul*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildMul".}
proc buildNSWMul*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNSWMul".}
proc buildNUWMul*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNUWMul".}
proc buildFMul*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFMul".}
proc buildUDiv*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildUDiv".}
proc buildExactUDiv*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildExactUDiv".}
proc buildSDiv*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSDiv".}
proc buildExactSDiv*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildExactSDiv".}
proc buildFDiv*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFDiv".}
proc buildURem*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildURem".}
proc buildSRem*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSRem".}
proc buildFRem*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFRem".}
proc buildShl*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildShl".}
proc buildLShr*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildLShr".}
proc buildAShr*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildAShr".}
proc buildAnd*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildAnd".}
proc buildOr*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildOr".}
proc buildXor*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildXor".}
proc buildBinOp*(b: BuilderRef; op: Opcode; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildBinOp".}
proc buildNeg*(a1: BuilderRef; v: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNeg".}
proc buildNSWNeg*(b: BuilderRef; v: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNSWNeg".}
proc buildNUWNeg*(b: BuilderRef; v: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNUWNeg".}
proc buildFNeg*(a1: BuilderRef; v: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFNeg".}
proc buildNot*(a1: BuilderRef; v: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNot".}

proc buildMalloc*(a1: BuilderRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildMalloc".}
proc buildArrayMalloc*(a1: BuilderRef; ty: TypeRef; val: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildArrayMalloc".}

proc buildMemSet*(b: BuilderRef; `ptr`: ValueRef; val: ValueRef; len: ValueRef;
                 align: cuint): ValueRef {.importc: "LLVMBuildMemSet",
                                        .}

proc buildMemCpy*(b: BuilderRef; dst: ValueRef; dstAlign: cuint; src: ValueRef;
                 srcAlign: cuint; size: ValueRef): ValueRef {.
    importc: "LLVMBuildMemCpy".}
proc buildMemMove*(b: BuilderRef; dst: ValueRef; dstAlign: cuint; src: ValueRef;
                  srcAlign: cuint; size: ValueRef): ValueRef {.
    importc: "LLVMBuildMemMove".}
proc buildAlloca*(a1: BuilderRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildAlloca".}
proc buildArrayAlloca*(a1: BuilderRef; ty: TypeRef; val: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildArrayAlloca".}
proc buildFree*(a1: BuilderRef; pointerVal: ValueRef): ValueRef {.
    importc: "LLVMBuildFree".}

proc buildLoad*(a1: BuilderRef; pointerVal: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildLoad".}

proc buildLoad2*(a1: BuilderRef; ty: TypeRef; pointerVal: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildLoad2".}
proc buildStore*(a1: BuilderRef; val: ValueRef; `ptr`: ValueRef): ValueRef {.
    importc: "LLVMBuildStore".}

proc buildGEP2*(b: BuilderRef; ty: TypeRef; pointer: ValueRef; indices: ptr ValueRef;
               numIndices: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildGEP2".}
proc buildInBoundsGEP2*(b: BuilderRef; ty: TypeRef; pointer: ValueRef;
                       indices: ptr ValueRef; numIndices: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildInBoundsGEP2".}
proc buildStructGEP2*(b: BuilderRef; ty: TypeRef; pointer: ValueRef; idx: cuint;
                     name: cstring): ValueRef {.importc: "LLVMBuildStructGEP2",
    .}
proc buildGlobalString*(b: BuilderRef; str: cstring; name: cstring): ValueRef {.
    importc: "LLVMBuildGlobalString".}
proc buildGlobalStringPtr*(b: BuilderRef; str: cstring; name: cstring): ValueRef {.
    importc: "LLVMBuildGlobalStringPtr".}
proc getVolatile*(memoryAccessInst: ValueRef): Bool {.importc: "LLVMGetVolatile",
    .}
proc setVolatile*(memoryAccessInst: ValueRef; isVolatile: Bool) {.
    importc: "LLVMSetVolatile".}
proc getWeak*(cmpXchgInst: ValueRef): Bool {.importc: "LLVMGetWeak".}
proc setWeak*(cmpXchgInst: ValueRef; isWeak: Bool) {.importc: "LLVMSetWeak",
    .}
proc getOrdering*(memoryAccessInst: ValueRef): AtomicOrdering {.
    importc: "LLVMGetOrdering".}
proc setOrdering*(memoryAccessInst: ValueRef; ordering: AtomicOrdering) {.
    importc: "LLVMSetOrdering".}
proc getAtomicRMWBinOp*(atomicRMWInst: ValueRef): AtomicRMWBinOp {.
    importc: "LLVMGetAtomicRMWBinOp".}
proc setAtomicRMWBinOp*(atomicRMWInst: ValueRef; binOp: AtomicRMWBinOp) {.
    importc: "LLVMSetAtomicRMWBinOp".}

proc buildTrunc*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildTrunc".}
proc buildZExt*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildZExt".}
proc buildSExt*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSExt".}
proc buildFPToUI*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFPToUI".}
proc buildFPToSI*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFPToSI".}
proc buildUIToFP*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildUIToFP".}
proc buildSIToFP*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSIToFP".}
proc buildFPTrunc*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFPTrunc".}
proc buildFPExt*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFPExt".}
proc buildPtrToInt*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildPtrToInt".}
proc buildIntToPtr*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildIntToPtr".}
proc buildBitCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildBitCast".}
proc buildAddrSpaceCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildAddrSpaceCast".}
proc buildZExtOrBitCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildZExtOrBitCast".}
proc buildSExtOrBitCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSExtOrBitCast".}
proc buildTruncOrBitCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildTruncOrBitCast".}
proc buildCast*(b: BuilderRef; op: Opcode; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildCast".}
proc buildPointerCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildPointerCast".}
proc buildIntCast2*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; isSigned: Bool;
                   name: cstring): ValueRef {.importc: "LLVMBuildIntCast2",
    .}
proc buildFPCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFPCast".}

proc buildIntCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildIntCast".}

proc getCastOpcode*(src: ValueRef; srcIsSigned: Bool; destTy: TypeRef;
                   destIsSigned: Bool): Opcode {.importc: "LLVMGetCastOpcode",
    .}

proc buildICmp*(a1: BuilderRef; op: IntPredicate; lhs: ValueRef; rhs: ValueRef;
               name: cstring): ValueRef {.importc: "LLVMBuildICmp".}
proc buildFCmp*(a1: BuilderRef; op: RealPredicate; lhs: ValueRef; rhs: ValueRef;
               name: cstring): ValueRef {.importc: "LLVMBuildFCmp".}

proc buildPhi*(a1: BuilderRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildPhi".}

proc buildCall2*(a1: BuilderRef; a2: TypeRef; fn: ValueRef; args: ptr ValueRef;
                numArgs: cuint; name: cstring): ValueRef {.importc: "LLVMBuildCall2",
    .}
proc buildSelect*(a1: BuilderRef; `if`: ValueRef; then: ValueRef; `else`: ValueRef;
                 name: cstring): ValueRef {.importc: "LLVMBuildSelect",
    .}
proc buildVAArg*(a1: BuilderRef; list: ValueRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildVAArg".}
proc buildExtractElement*(a1: BuilderRef; vecVal: ValueRef; index: ValueRef;
                         name: cstring): ValueRef {.
    importc: "LLVMBuildExtractElement".}
proc buildInsertElement*(a1: BuilderRef; vecVal: ValueRef; eltVal: ValueRef;
                        index: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildInsertElement".}
proc buildShuffleVector*(a1: BuilderRef; v1: ValueRef; v2: ValueRef; mask: ValueRef;
                        name: cstring): ValueRef {.
    importc: "LLVMBuildShuffleVector".}
proc buildExtractValue*(a1: BuilderRef; aggVal: ValueRef; index: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildExtractValue".}
proc buildInsertValue*(a1: BuilderRef; aggVal: ValueRef; eltVal: ValueRef;
                      index: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildInsertValue".}
proc buildFreeze*(a1: BuilderRef; val: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFreeze".}
proc buildIsNull*(a1: BuilderRef; val: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildIsNull".}
proc buildIsNotNull*(a1: BuilderRef; val: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildIsNotNull".}

proc buildPtrDiff2*(a1: BuilderRef; elemTy: TypeRef; lhs: ValueRef; rhs: ValueRef;
                   name: cstring): ValueRef {.importc: "LLVMBuildPtrDiff2",
    .}
proc buildFence*(b: BuilderRef; ordering: AtomicOrdering; singleThread: Bool;
                name: cstring): ValueRef {.importc: "LLVMBuildFence".}
proc buildAtomicRMW*(b: BuilderRef; op: AtomicRMWBinOp; `ptr`: ValueRef; val: ValueRef;
                    ordering: AtomicOrdering; singleThread: Bool): ValueRef {.
    importc: "LLVMBuildAtomicRMW".}
proc buildAtomicCmpXchg*(b: BuilderRef; `ptr`: ValueRef; cmp: ValueRef; new: ValueRef;
                        successOrdering: AtomicOrdering;
                        failureOrdering: AtomicOrdering; singleThread: Bool): ValueRef {.
    importc: "LLVMBuildAtomicCmpXchg".}

proc getNumMaskElements*(shuffleVectorInst: ValueRef): cuint {.
    importc: "LLVMGetNumMaskElements".}

proc getUndefMaskElem*(): cint {.importc: "LLVMGetUndefMaskElem".}

proc getMaskValue*(shuffleVectorInst: ValueRef; elt: cuint): cint {.
    importc: "LLVMGetMaskValue".}
proc isAtomicSingleThread*(atomicInst: ValueRef): Bool {.
    importc: "LLVMIsAtomicSingleThread".}
proc setAtomicSingleThread*(atomicInst: ValueRef; singleThread: Bool) {.
    importc: "LLVMSetAtomicSingleThread".}
proc getCmpXchgSuccessOrdering*(cmpXchgInst: ValueRef): AtomicOrdering {.
    importc: "LLVMGetCmpXchgSuccessOrdering".}
proc setCmpXchgSuccessOrdering*(cmpXchgInst: ValueRef; ordering: AtomicOrdering) {.
    importc: "LLVMSetCmpXchgSuccessOrdering".}
proc getCmpXchgFailureOrdering*(cmpXchgInst: ValueRef): AtomicOrdering {.
    importc: "LLVMGetCmpXchgFailureOrdering".}
proc setCmpXchgFailureOrdering*(cmpXchgInst: ValueRef; ordering: AtomicOrdering) {.
    importc: "LLVMSetCmpXchgFailureOrdering".}

proc createModuleProviderForExistingModule*(m: ModuleRef): ModuleProviderRef {.
    importc: "LLVMCreateModuleProviderForExistingModule".}

proc disposeModuleProvider*(m: ModuleProviderRef) {.
    importc: "LLVMDisposeModuleProvider".}

proc createMemoryBufferWithContentsOfFile*(path: cstring;
    outMemBuf: ptr MemoryBufferRef; outMessage: cstringArray): Bool {.
    importc: "LLVMCreateMemoryBufferWithContentsOfFile".}
proc createMemoryBufferWithSTDIN*(outMemBuf: ptr MemoryBufferRef;
                                 outMessage: cstringArray): Bool {.
    importc: "LLVMCreateMemoryBufferWithSTDIN".}
proc createMemoryBufferWithMemoryRange*(inputData: cstring;
                                       inputDataLength: csize_t;
                                       bufferName: cstring;
                                       requiresNullTerminator: Bool): MemoryBufferRef {.
    importc: "LLVMCreateMemoryBufferWithMemoryRange".}
proc createMemoryBufferWithMemoryRangeCopy*(inputData: cstring;
    inputDataLength: csize_t; bufferName: cstring): MemoryBufferRef {.
    importc: "LLVMCreateMemoryBufferWithMemoryRangeCopy".}
proc getBufferStart*(memBuf: MemoryBufferRef): cstring {.
    importc: "LLVMGetBufferStart".}
proc getBufferSize*(memBuf: MemoryBufferRef): csize_t {.
    importc: "LLVMGetBufferSize".}
proc disposeMemoryBuffer*(memBuf: MemoryBufferRef) {.
    importc: "LLVMDisposeMemoryBuffer".}

proc getGlobalPassRegistry*(): PassRegistryRef {.
    importc: "LLVMGetGlobalPassRegistry".}

proc createPassManager*(): PassManagerRef {.importc: "LLVMCreatePassManager",
    .}

proc createFunctionPassManagerForModule*(m: ModuleRef): PassManagerRef {.
    importc: "LLVMCreateFunctionPassManagerForModule".}

proc createFunctionPassManager*(mp: ModuleProviderRef): PassManagerRef {.
    importc: "LLVMCreateFunctionPassManager".}

proc runPassManager*(pm: PassManagerRef; m: ModuleRef): Bool {.
    importc: "LLVMRunPassManager".}

proc initializeFunctionPassManager*(fpm: PassManagerRef): Bool {.
    importc: "LLVMInitializeFunctionPassManager".}

proc runFunctionPassManager*(fpm: PassManagerRef; f: ValueRef): Bool {.
    importc: "LLVMRunFunctionPassManager".}

proc finalizeFunctionPassManager*(fpm: PassManagerRef): Bool {.
    importc: "LLVMFinalizeFunctionPassManager".}

proc disposePassManager*(pm: PassManagerRef) {.importc: "LLVMDisposePassManager",
    .}

proc startMultithreaded*(): Bool {.importc: "LLVMStartMultithreaded".}

proc stopMultithreaded*() {.importc: "LLVMStopMultithreaded".}

proc isMultithreaded*(): Bool {.importc: "LLVMIsMultithreaded".}
