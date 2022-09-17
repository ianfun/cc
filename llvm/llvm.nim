## LLVM Backend
##
## LLVM-C API

type
  OpaqueMemoryBuffer*{.pure, final.} = object
  OpaqueAttributeRef*{.pure, final.} = object
  OpaqueContext*{.pure, final.} = object
  OpaqueModule*{.pure, final.} = object
  OpaqueType*{.pure, final.} = object
  OpaqueValue*{.pure, final.} = object
  OpaqueBasicBlock*{.pure, final.} = object
  OpaqueBuilder*{.pure, final.} = object
  OpaqueModuleProvider*{.pure, final.} = object
  OpaquePassManager*{.pure, final.} = object
  OpaquePassRegistry*{.pure, final.} = object
  OpaqueUse*{.pure, final.} = object
  OpaqueDiagnosticInfo*{.pure, final.} = object
  OpaqueTargetMachine*{.pure, final.} = object
  orcOpaqueLLJITBuilder*{.pure, final.} = object
  orcOpaqueLLJIT*{.pure, final.} = object
  orcOpaqueSymbolStringPool*{.pure, final.} = object
  orcOpaqueSymbolStringPoolEntry*{.pure, final.} = object
  orcOpaqueJITDylib*{.pure, final.} = object
  orcOpaqueJITTargetMachineBuilder*{.pure, final.} = object
  orcOpaqueMaterializationUnit*{.pure, final.} = object
  orcOpaqueMaterializationResponsibility*{.pure, final.} = object
  orcOpaqueResourceTracker*{.pure, final.} = object
  orcOpaqueDefinitionGenerator*{.pure, final.} = object
  orcOpaqueLookupState{.pure, final.} = object
  orcOpaqueThreadSafeContext{.pure, final.} = object
  orcOpaqueObjectTransformLayer*{.pure, final.} = object
  orcOpaqueExecutionSession*{.pure, final.} = object
  orcOpaqueIRTransformLayer*{.pure, final.} = object
  opaqueError*{.pure, final.} = object
  orcOpaqueObjectLayer*{.pure, final.} = object
  orcOpaqueObjectLinkingLayer*{.pure, final.} = object
  orcOpaqueIndirectStubsManager*{.pure, final.} = object
  orcOpaqueLazyCallThroughManager*{.pure, final.} = object
  orcOpaqueDumpObjects*{.pure, final.} = object
  ErrorRef* = pointer
  orcOpaqueThreadSafeModule*{.pure, final.} = object
  OpaquePassManagerBuilder*{.pure, final.} = object
  OpaqueMetaData{.pure, final.} = object
  OpaqueDIBuilder{.pure, final.} = object
  target{.pure, final.} = object
  OpaqueJITEventListener{.pure, final.} = object
  OpaqueNamedMDNode{.pure, final.} = object
  opaqueValueMetadataEntry{.pure, final.} = object
  comdat{.pure, final.} = object
  opaqueModuleFlagEntry{.pure, final.} = object
  OpaqueBinary{.pure, final.} = object
  int64T = int64
  uint64T = uint64
  uint8T = uint8
  int32T = int32
  uint32T = uint32
  Bool* = cint
  AttributeIndex* = cuint
  opaqueTargetData{.pure, final.} = object
  TargetDataRef* = ptr opaqueTargetData
  TargetLibraryInfoRef* = distinct pointer
  Opcode* = cint
  DIFlags* = cint
  DWARFTypeEncoding* = cuint
  MetadataKind* = cuint
  ByteOrdering* {.size: sizeof(cint).} = enum
    BigEndian, LittleEndian
  TargetMachineRef* = distinct pointer
  PassManagerBuilderRef* = distinct pointer
  VerifierFailureAction* {.size: sizeof(cint), pure.} = enum
    AbortProcessAction, PrintMessageAction, ReturnStatusAction

const
  False*: Bool = 0
  True*: Bool = 1

include LLVMInstrinsics
include LLVMAttribute
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

# wrappers from llvmAPI.cpp

proc nimLLVMSetDSOLocal*(Global: ValueRef) {.importc: "LLVMNimSetDSOLocal".}

proc nimLLVMOptModule*(m: ModuleRef) {.importc: "LLVMNimOptModule".}

proc nimLLVMGetAllocaArraySize*(Alloca: ValueRef): ValueRef {.importc: "LLVMNimGetAllocaArraySize".}

proc nimLLVMGetIntrinsicForMSBuiltin*(Prefix, BuiltinName: cstring): cuint {.importc: "LLVMNimGetIntrinsicForMSBuiltin".}

proc nimLLVMGetIntrinsicForClangBuiltin*(Prefix, BuiltinName: cstring): cuint {.importc: "LLVMNimGetIntrinsicForClangBuiltin".}

proc nimLLVMConfigureTarget*(tripleStr: cstring, Target: ptr TargetRef, Machine: ptr TargetMachineRef, TD: ptr TargetDataRef): cstring {.importc: "LLVMNimConfigureTarget".}

# end wrappers
