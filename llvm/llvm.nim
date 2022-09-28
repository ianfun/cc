## LLVM Backend
##
## LLVM-C API

type
  OpaqueMemoryBuffer*{.importc: "void".} = object
  OpaqueAttributeRef*{.importc: "void".} = object
  OpaqueContext*{.importc: "void".} = object
  OpaqueModule*{.importc: "void".} = object
  OpaqueType*{.importc: "void".} = object
  OpaqueValue*{.importc: "void".} = object
  OpaqueBasicBlock*{.importc: "void".} = object
  OpaqueBuilder*{.importc: "void".} = object
  OpaqueModuleProvider*{.importc: "void".} = object
  OpaquePassManager*{.importc: "void".} = object
  OpaquePassRegistry*{.importc: "void".} = object
  OpaqueUse*{.importc: "void".} = object
  OpaqueDiagnosticInfo*{.importc: "void".} = object
  OpaqueTargetMachine*{.importc: "void".} = object
  orcOpaqueLLJITBuilder*{.importc: "void".} = object
  orcOpaqueLLJIT*{.importc: "void".} = object
  orcOpaqueSymbolStringPool*{.importc: "void".} = object
  orcOpaqueSymbolStringPoolEntry*{.importc: "void".} = object
  orcOpaqueJITDylib*{.importc: "void".} = object
  orcOpaqueJITTargetMachineBuilder*{.importc: "void".} = object
  orcOpaqueMaterializationUnit*{.importc: "void".} = object
  orcOpaqueMaterializationResponsibility*{.importc: "void".} = object
  orcOpaqueResourceTracker*{.importc: "void".} = object
  orcOpaqueDefinitionGenerator*{.importc: "void".} = object
  orcOpaqueLookupState{.importc: "void".} = object
  orcOpaqueThreadSafeContext{.importc: "void".} = object
  orcOpaqueObjectTransformLayer*{.importc: "void".} = object
  orcOpaqueExecutionSession*{.importc: "void".} = object
  orcOpaqueIRTransformLayer*{.importc: "void".} = object
  opaqueError*{.importc: "void".} = object
  orcOpaqueObjectLayer*{.importc: "void".} = object
  orcOpaqueObjectLinkingLayer*{.importc: "void".} = object
  orcOpaqueIndirectStubsManager*{.importc: "void".} = object
  orcOpaqueLazyCallThroughManager*{.importc: "void".} = object
  orcOpaqueDumpObjects*{.importc: "void".} = object
  ErrorRef* = pointer
  orcOpaqueThreadSafeModule*{.importc: "void".} = object
  OpaquePassManagerBuilder*{.importc: "void".} = object
  OpaqueMetaData{.importc: "void".} = object
  OpaqueDIBuilder{.importc: "void".} = object
  target{.importc: "void".} = object
  OpaqueJITEventListener{.importc: "void".} = object
  OpaqueNamedMDNode{.importc: "void".} = object
  opaqueValueMetadataEntry{.importc: "void".} = object
  comdat{.importc: "void".} = object
  opaqueModuleFlagEntry{.importc: "void".} = object
  OpaqueBinary{.importc: "void".} = object
  int64T = int64
  uint64T = uint64
  uint8T = uint8
  int32T = int32
  uint32T = uint32
  Bool* = cint
  AttributeIndex* = cuint
  opaqueTargetData = object
  TargetDataRef* = ptr opaqueTargetData
  TargetLibraryInfoRef* = pointer
  Opcode* = cint
  DIFlags* = cint
  DWARFTypeEncoding* = cuint
  MetadataKind* = cuint
  ByteOrdering* {.size: sizeof(cint).} = enum
    BigEndian, LittleEndian
  TargetMachineRef* = pointer
  PassManagerBuilderRef* = pointer
  VerifierFailureAction* {.size: sizeof(cint), pure.} = enum
    AbortProcessAction, PrintMessageAction, ReturnStatusAction

const
  False*: Bool = 0
  True*: Bool = 1

include triple
include LLVMInstrinsics
include Attribute
include DWARF
include FFI/LLVM/Types
include FFI/LLVM/Support
include FFI/LLVM/Error
include FFI/LLVM/Core
include FFI/LLVM/Target
include FFI/LLVM/BitReader
include FFI/LLVM/IRReader
include FFI/LLVM/BitWriter
include FFI/LLVM/Linker
include FFI/LLVM/Analysis
include FFI/LLVM/TargetMachine
include FFI/LLVM/Transforms/PassManagerBuilder
include FFI/LLVM/Orc
include FFI/LLVM/LLJIT
include FFI/LLVM/DebugInfo

proc typeOfX*(val: ValueRef): TypeRef {.importc: "LLVMTypeOf".}

proc `$`*(v: ValueRef): string =
  let m = v.printValueToString()
  result = $m
  disposeMessage(m)

proc `$`*(v: TypeRef): string =
  let m = v.printTypeToString()
  result = $m
  disposeMessage(m)

proc `$`*(err: ErrorRef): string =
  var m = getErrorMessage(err)
  result = $m
  disposeErrorMessage(m)

proc constInt*(ty: TypeRef, a: culonglong): ValueRef =
  constInt(ty, a, False)

const
   LLVMRet*            = 1.Opcode
   LLVMBr*             = 2.Opcode
   LLVMSwitch*         = 3.Opcode
   LLVMIndirectBr*     = 4.Opcode
   LLVMInvoke*         = 5.Opcode
   LLVMUnreachable*    = 7.Opcode
   LLVMCallBr*         = 67.Opcode
   LLVMFNeg*           = 66.Opcode
   LLVMAdd*            = 8.Opcode
   LLVMFAdd*           = 9.Opcode
   LLVMSub*            = 10.Opcode
   LLVMFSub*           = 11.Opcode
   LLVMMul*            = 12.Opcode
   LLVMFMul*           = 13.Opcode
   LLVMUDiv*           = 14.Opcode
   LLVMSDiv*           = 15.Opcode
   LLVMFDiv*           = 16.Opcode
   LLVMURem*           = 17.Opcode
   LLVMSRem*           = 18.Opcode
   LLVMFRem*           = 19.Opcode
   LLVMShl*            = 20.Opcode
   LLVMLShr*           = 21.Opcode
   LLVMAShr*           = 22.Opcode
   LLVMAnd*            = 23.Opcode
   LLVMOr*             = 24.Opcode
   LLVMXor*            = 25.Opcode
   LLVMAlloca*         = 26.Opcode
   LLVMLoad*           = 27.Opcode
   LLVMStore*          = 28.Opcode
   LLVMGetElementPtr*  = 29.Opcode
   LLVMTrunc*          = 30.Opcode
   LLVMZExt*           = 31.Opcode
   LLVMSExt*           = 32.Opcode
   LLVMFPToUI*         = 33.Opcode
   LLVMFPToSI*         = 34.Opcode
   LLVMUIToFP*         = 35.Opcode
   LLVMSIToFP*         = 36.Opcode
   LLVMFPTrunc*        = 37.Opcode
   LLVMFPExt*          = 38.Opcode
   LLVMPtrToInt*       = 39.Opcode
   LLVMIntToPtr*       = 40.Opcode
   LLVMBitCast*        = 41.Opcode
   LLVMAddrSpaceCast*  = 60.Opcode
   LLVMICmp*           = 42.Opcode
   LLVMFCmp*           = 43.Opcode
   LLVMPHI*            = 44.Opcode
   LLVMCall*           = 45.Opcode
   LLVMSelect*         = 46.Opcode
   LLVMUserOp1*        = 47.Opcode
   LLVMUserOp2*        = 48.Opcode
   LLVMVAArg*          = 49.Opcode
   LLVMExtractElement* = 50.Opcode
   LLVMInsertElement*  = 51.Opcode
   LLVMShuffleVector*  = 52.Opcode
   LLVMExtractValue*   = 53.Opcode
   LLVMInsertValue*    = 54.Opcode
   LLVMFreeze*         = 68.Opcode
   LLVMFence*          = 55.Opcode
   LLVMAtomicCmpXchg*  = 56.Opcode
   LLVMAtomicRMW*      = 57.Opcode
   LLVMResume*         = 58.Opcode
   LLVMLandingPad*     = 59.Opcode
   LLVMCleanupRet*     = 61.Opcode
   LLVMCatchRet*       = 62.Opcode
   LLVMCatchPad*       = 63.Opcode
   LLVMCleanupPad*     = 64.Opcode
   LLVMCatchSwitch*    = 65.Opcode

type
  Value* = ValueRef ## LLVM Value
  Type* = TypeRef ## LLVM Type
  Label* = BasicBlockRef ## LLVM block
  DIScope* = MetadataRef ## LLVM DIScope
  DIType* = MetadataRef ## Debug Type

# wrappers from llvmAPI.cpp

proc nimLLVMSetDSOLocal*(Global: ValueRef) {.importc: "LLVMNimSetDSOLocal".}

proc nimLLVMOptModule*(m: ModuleRef) {.importc: "LLVMNimOptModule".}

proc nimLLVMGetAllocaArraySize*(Alloca: ValueRef): ValueRef {.importc: "LLVMNimGetAllocaArraySize".}

proc nimLLVMGetIntrinsicForMSBuiltin*(Prefix, BuiltinName: cstring): cuint {.importc: "LLVMNimGetIntrinsicForMSBuiltin".}

proc nimLLVMGetIntrinsicForClangBuiltin*(Prefix, BuiltinName: cstring): cuint {.importc: "LLVMNimGetIntrinsicForClangBuiltin".}

proc nimLLVMConfigureTarget*(tripleStr: cstring, Target: ptr TargetRef, Machine: ptr TargetMachineRef, TD: ptr TargetDataRef, theTriple: ptr tripleRef, f: ptr uint32): cstring {.importc: "LLVMNimConfigureTarget".}

proc nimGetArch*(t: tripleRef): ArchType {.importc: "LLVMNimGetArch".}

proc nimGetOS*(t: tripleRef): OSType {.importc: "LLVMNimGetOS".}

proc nimGetEnv*(t: tripleRef): EnvironmentType {.importc: "LLVMNimGetEnv".}

proc nimGetArchName*(t: tripleRef): cstring {.importc: "LLVMNimGetArchName".}

proc nimdIBuilderGetOrCreateSubrange*(b: DIBuilderRef, count: MetadataRef): MetadataRef {.importc: "LLVMNimdIBuilderGetOrCreateSubrange".}

proc nimGlobalAddDebugInfo*(g: ValueRef, m: MetadataRef) {.importc: "LLVMNimGlobalAddDebugInfo".}

proc nimAddLabel*(Builder: DIBuilderRef, Scope: MetadataRef, BB: BasicBlockRef, name: cstring, nameLen: csize_t, file:MetadataRef, lineno: cuint, loc: MetadataRef) {.importc: "LLVMNimAddLabel".}

# end wrappers
