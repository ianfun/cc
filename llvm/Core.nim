## ===-- llvm-c/Core.h - Core Library C Interface ------------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMCore.a, which implements    *|
## |* the LLVM intermediate representation.                                      *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_CORE_H [NewLine] # LLVM_C_CORE_H [NewLine] # llvm-c/Deprecated.h [NewLine] # llvm-c/ErrorHandling.h [NewLine] # llvm-c/ExternC.h [NewLine] # llvm-c/Types.h [NewLine] LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMC LLVM-C: C interface to LLVM
##
##  This module exposes parts of the LLVM library as a C API.
##
##  @{
##  *
##  @defgroup LLVMCTransforms Transforms
##  *
##  @defgroup LLVMCCore Core
##
##  This modules provide an interface to libLLVMCore, which implements
##  the LLVM intermediate representation as well as other related types
##  and utilities.
##
##  Many exotic languages can interoperate with C code but have a harder time
##  with C++ due to name mangling. So in addition to C, this interface enables
##  tools written in such languages.
##
##  @{
##  *
##  @defgroup LLVMCCoreTypes Types and Enumerations
##
##  @{
##  / External users depend on the following values being stable. It is not safe
## / to reorder them. typedef enum {  Terminator Instructions LLVMRet = 1 , LLVMBr = 2 , LLVMSwitch = 3 , LLVMIndirectBr = 4 , LLVMInvoke = 5 ,  removed 6 due to API changes LLVMUnreachable = 7 , LLVMCallBr = 67 ,  Standard Unary Operators LLVMFNeg = 66 ,  Standard Binary Operators LLVMAdd = 8 , LLVMFAdd = 9 , LLVMSub = 10 , LLVMFSub = 11 , LLVMMul = 12 , LLVMFMul = 13 , LLVMUDiv = 14 , LLVMSDiv = 15 , LLVMFDiv = 16 , LLVMURem = 17 , LLVMSRem = 18 , LLVMFRem = 19 ,  Logical Operators LLVMShl = 20 , LLVMLShr = 21 , LLVMAShr = 22 , LLVMAnd = 23 , LLVMOr = 24 , LLVMXor = 25 ,  Memory Operators LLVMAlloca = 26 , LLVMLoad = 27 , LLVMStore = 28 , LLVMGetElementPtr = 29 ,  Cast Operators LLVMTrunc = 30 , LLVMZExt = 31 , LLVMSExt = 32 , LLVMFPToUI = 33 , LLVMFPToSI = 34 , LLVMUIToFP = 35 , LLVMSIToFP = 36 , LLVMFPTrunc = 37 , LLVMFPExt = 38 , LLVMPtrToInt = 39 , LLVMIntToPtr = 40 , LLVMBitCast = 41 , LLVMAddrSpaceCast = 60 ,  Other Operators LLVMICmp = 42 , LLVMFCmp = 43 , LLVMPHI = 44 , LLVMCall = 45 , LLVMSelect = 46 , LLVMUserOp1 = 47 , LLVMUserOp2 = 48 , LLVMVAArg = 49 , LLVMExtractElement = 50 , LLVMInsertElement = 51 , LLVMShuffleVector = 52 , LLVMExtractValue = 53 , LLVMInsertValue = 54 , LLVMFreeze = 68 ,  Atomic operators LLVMFence = 55 , LLVMAtomicCmpXchg = 56 , LLVMAtomicRMW = 57 ,  Exception Handling Operators LLVMResume = 58 , LLVMLandingPad = 59 , LLVMCleanupRet = 61 , LLVMCatchRet = 62 , LLVMCatchPad = 63 , LLVMCleanupPad = 64 , LLVMCatchSwitch = 65 } LLVMOpcode ;
## Error: expected ';'!!!

type ## *
    ##  Emits an error if two values disagree, otherwise the resulting value is
    ##  that of the operands.
    ##
    ##  @see Module::ModFlagBehavior::Error
    ##
  TypeKind* {.size: sizeof(cint).} = enum
    VoidTypeKind,             ## *< type with no size
    HalfTypeKind,             ## *< 16 bit floating point type
    FloatTypeKind,            ## *< 32 bit floating point type
    DoubleTypeKind,           ## *< 64 bit floating point type
    X86FP80TypeKind,          ## *< 80 bit floating point type (X87)
    FP128TypeKind,            ## *< 128 bit floating point type (112-bit mantissa)
    PPC_FP128TypeKind,        ## *< 128 bit floating point type (two 64-bits)
    LabelTypeKind,            ## *< Labels
    IntegerTypeKind,          ## *< Arbitrary bit width integers
    FunctionTypeKind,         ## *< Functions
    StructTypeKind,           ## *< Structures
    ArrayTypeKind,            ## *< Arrays
    PointerTypeKind,          ## *< Pointers
    VectorTypeKind,           ## *< Fixed width SIMD vector type
    MetadataTypeKind,         ## *< Metadata
    X86MMXTypeKind,           ## *< X86 MMX
    TokenTypeKind,            ## *< Tokens
    ScalableVectorTypeKind,   ## *< Scalable SIMD vector type
    BFloatTypeKind,           ## *< 16 bit brain floating point type
    X86AMXTypeKind            ## *< X86 AMX
  Linkage* {.size: sizeof(cint).} = enum
    ExternalLinkage,          ## *< Externally visible function
    AvailableExternallyLinkage, LinkOnceAnyLinkage, ## *< Keep one copy of function when linking (inline)
    LinkOnceODRLinkage,       ## *< Same, but only replaced by something
                       ##                             equivalent.
    LinkOnceODRAutoHideLinkage, ## *< Obsolete
    WeakAnyLinkage,           ## *< Keep one copy of function when linking (weak)
    WeakODRLinkage,           ## *< Same, but only replaced by something
                   ##                             equivalent.
    AppendingLinkage,         ## *< Special purpose, only applies to global arrays
    InternalLinkage,          ## *< Rename collisions when linking (static
                    ##                                functions)
    PrivateLinkage,           ## *< Like Internal, but omit from symbol table
    DLLImportLinkage,         ## *< Obsolete
    DLLExportLinkage,         ## *< Obsolete
    ExternalWeakLinkage,      ## *< ExternalWeak linkage description
    GhostLinkage,             ## *< Obsolete
    CommonLinkage,            ## *< Tentative definitions
    LinkerPrivateLinkage,     ## *< Like Private, but linker removes.
    LinkerPrivateWeakLinkage  ## *< Like LinkerPrivate, but is weak.
  Visibility* {.size: sizeof(cint).} = enum
    DefaultVisibility,        ## *< The GV is visible
    HiddenVisibility,         ## *< The GV is hidden
    ProtectedVisibility       ## *< The GV is protected
  UnnamedAddr* {.size: sizeof(cint).} = enum
    NoUnnamedAddr,            ## *< Address of the GV is significant.
    LocalUnnamedAddr,         ## *< Address of the GV is locally insignificant.
    GlobalUnnamedAddr         ## *< Address of the GV is globally insignificant.
  DLLStorageClass* {.size: sizeof(cint).} = enum
    DefaultStorageClass = 0, DLLImportStorageClass = 1, ## *< Function to be imported from DLL.
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
    IntEQ = 32,                 ## *< equal
    IntNE,                    ## *< not equal
    IntUGT,                   ## *< unsigned greater than
    IntUGE,                   ## *< unsigned greater or equal
    IntULT,                   ## *< unsigned less than
    IntULE,                   ## *< unsigned less or equal
    IntSGT,                   ## *< signed greater than
    IntSGE,                   ## *< signed greater or equal
    IntSLT,                   ## *< signed less than
    IntSLE                    ## *< signed less or equal
  RealPredicate* {.size: sizeof(cint).} = enum
    RealPredicateFalse,       ## *< Always false (always folded)
    RealOEQ,                  ## *< True if ordered and equal
    RealOGT,                  ## *< True if ordered and greater than
    RealOGE,                  ## *< True if ordered and greater than or equal
    RealOLT,                  ## *< True if ordered and less than
    RealOLE,                  ## *< True if ordered and less than or equal
    RealONE,                  ## *< True if ordered and operands are unequal
    RealORD,                  ## *< True if ordered (no nans)
    RealUNO,                  ## *< True if unordered: isnan(X) | isnan(Y)
    RealUEQ,                  ## *< True if unordered or equal
    RealUGT,                  ## *< True if unordered or greater than
    RealUGE,                  ## *< True if unordered, greater than, or equal
    RealULT,                  ## *< True if unordered or less than
    RealULE,                  ## *< True if unordered, less than, or equal
    RealUNE,                  ## *< True if unordered or not equal
    RealPredicateTrue         ## *< Always true (always folded)
  LandingPadClauseTy* {.size: sizeof(cint).} = enum
    LandingPadCatch,          ## *< A catch clause
    LandingPadFilter          ## *< A filter clause
  ThreadLocalMode* {.size: sizeof(cint).} = enum
    NotThreadLocal = 0, GeneralDynamicTLSModel, LocalDynamicTLSModel,
    InitialExecTLSModel, LocalExecTLSModel
  AtomicOrdering* {.size: sizeof(cint).} = enum
    AtomicOrderingNotAtomic = 0, ## *< A load or store which is not atomic
    AtomicOrderingUnordered = 1, ## *< Lowest level of atomicity, guarantees
                              ##                                      somewhat sane results, lock free.
    AtomicOrderingMonotonic = 2, ## *< guarantees that if you take all the
                              ##                                      operations affecting a specific address,
                              ##                                      a consistent ordering exists
    AtomicOrderingAcquire = 4, ## *< Acquire provides a barrier of the sort
                            ##                                    necessary to acquire a lock to access other
                            ##                                    memory with normal loads and stores.
    AtomicOrderingRelease = 5, ## *< Release is similar to Acquire, but with
                            ##                                    a barrier of the sort necessary to release
                            ##                                    a lock.
    AtomicOrderingAcquireRelease = 6, ## *< provides both an Acquire and a
                                   ##                                           Release barrier (for fences and
                                   ##                                           operations which both read and write
                                   ##                                            memory).
    AtomicOrderingSequentiallyConsistent = 7
  AtomicRMWBinOp* {.size: sizeof(cint).} = enum
    AtomicRMWBinOpXchg,       ## *< Set the new value and return the one old
    AtomicRMWBinOpAdd,        ## *< Add a value and return the old one
    AtomicRMWBinOpSub,        ## *< Subtract a value and return the old one
    AtomicRMWBinOpAnd,        ## *< And a value and return the old one
    AtomicRMWBinOpNand,       ## *< Not-And a value and return the old one
    AtomicRMWBinOpOr,         ## *< OR a value and return the old one
    AtomicRMWBinOpXor,        ## *< Xor a value and return the old one
    AtomicRMWBinOpMax, ## *< Sets the value if it's greater than the
                      ##                              original using a signed comparison and return
                      ##                              the old one
    AtomicRMWBinOpMin, ## *< Sets the value if it's Smaller than the
                      ##                              original using a signed comparison and return
                      ##                              the old one
    AtomicRMWBinOpUMax, ## *< Sets the value if it's greater than the
                       ##                              original using an unsigned comparison and return
                       ##                              the old one
    AtomicRMWBinOpUMin, ## *< Sets the value if it's greater than the
                       ##                               original using an unsigned comparison and return
                       ##                               the old one
    AtomicRMWBinOpFAdd,       ## *< Add a floating point value and return the
                       ##                               old one
    AtomicRMWBinOpFSub,       ## *< Subtract a floating point value and return the
                       ##                             old one
    AtomicRMWBinOpFMax, ## *< Sets the value if it's greater than the
                       ##                              original using an floating point comparison and
                       ##                              return the old one
    AtomicRMWBinOpFMin ## *< Sets the value if it's smaller than the
                      ##                              original using an floating point comparison and
                      ##                              return the old one
  DiagnosticSeverity* {.size: sizeof(cint).} = enum
    DSError, DSWarning, DSRemark, DSNote
  InlineAsmDialect* {.size: sizeof(cint).} = enum
    InlineAsmDialectATT, InlineAsmDialectIntel
  ModuleFlagBehavior* {.size: sizeof(cint).} = enum
    ModuleFlagBehaviorError, ## *
                            ##  Emits a warning if two values disagree. The result value will be the
                            ##  operand for the flag from the first module being linked.
                            ##
                            ##  @see Module::ModFlagBehavior::Warning
                            ##
    ModuleFlagBehaviorWarning, ## *
                              ##  Adds a requirement that another module flag be present and have a
                              ##  specified value after linking is performed. The value must be a metadata
                              ##  pair, where the first element of the pair is the ID of the module flag
                              ##  to be restricted, and the second element of the pair is the value the
                              ##  module flag should be restricted to. This behavior can be used to
                              ##  restrict the allowable results (via triggering of an error) of linking
                              ##  IDs with the **Override** behavior.
                              ##
                              ##  @see Module::ModFlagBehavior::Require
                              ##
    ModuleFlagBehaviorRequire, ## *
                              ##  Uses the specified value, regardless of the behavior or value of the
                              ##  other module. If both modules specify **Override**, but the values
                              ##  differ, an error will be emitted.
                              ##
                              ##  @see Module::ModFlagBehavior::Override
                              ##
    ModuleFlagBehaviorOverride, ## *
                               ##  Appends the two values, which are required to be metadata nodes.
                               ##
                               ##  @see Module::ModFlagBehavior::Append
                               ##
    ModuleFlagBehaviorAppend, ## *
                             ##  Appends the two values, which are required to be metadata
                             ##  nodes. However, duplicate entries in the second list are dropped
                             ##  during the append operation.
                             ##
                             ##  @see Module::ModFlagBehavior::AppendUnique
                             ##
    ModuleFlagBehaviorAppendUnique

















## *
##  Attribute index are either LLVMAttributeReturnIndex,
##  LLVMAttributeFunctionIndex or a parameter number from 1 to N.
##

const
  AttributeReturnIndex* = 0 ##  ISO C restricts enumerator values to range of 'int'
                              ##  (4294967295 is too large)
                              ##  LLVMAttributeFunctionIndex = ~0U,
  AttributeFunctionIndex* = -1

## !!!Ignored construct:  typedef unsigned LLVMAttributeIndex ;
## Error: identifier expected, but got: ;!!!

## *
##  @}
##

proc initializeCore*(r: PassRegistryRef) {.importc: "LLVMInitializeCore",
                                        .}
## * Deallocate and destroy all ManagedStatic variables.
##     @see llvm::llvm_shutdown
##     @see ManagedStatic

proc shutdown*() {.importc: "LLVMShutdown".}
## ===-- Error handling ----------------------------------------------------===

proc createMessage*(message: cstring): cstring {.importc: "LLVMCreateMessage",
    .}
proc disposeMessage*(message: cstring) {.importc: "LLVMDisposeMessage",
                                      .}
## *
##  @defgroup LLVMCCoreContext Contexts
##
##  Contexts are execution states for the core LLVM IR system.
##
##  Most types are tied to a context instance. Multiple contexts can
##  exist simultaneously. A single context is not thread safe. However,
##  different contexts can execute on different threads simultaneously.
##
##  @{
##

type
  DiagnosticHandler* = proc (a1: DiagnosticInfoRef; a2: pointer)
  YieldCallback* = proc (a1: ContextRef; a2: pointer)

## *
##  Create a new context.
##
##  Every call to this function should be paired with a call to
##  LLVMContextDispose() or the context will leak memory.
##

proc contextCreate*(): ContextRef {.importc: "LLVMContextCreate".}
## *
##  Obtain the global context instance.
##

proc getGlobalContext*(): ContextRef {.importc: "LLVMGetGlobalContext",
                                    .}
## *
##  Set the diagnostic handler for this context.
##

proc contextSetDiagnosticHandler*(c: ContextRef; handler: DiagnosticHandler;
                                 diagnosticContext: pointer) {.
    importc: "LLVMContextSetDiagnosticHandler".}
## *
##  Get the diagnostic handler of this context.
##

proc contextGetDiagnosticHandler*(c: ContextRef): DiagnosticHandler {.
    importc: "LLVMContextGetDiagnosticHandler".}
## *
##  Get the diagnostic context of this context.
##

proc contextGetDiagnosticContext*(c: ContextRef): pointer {.
    importc: "LLVMContextGetDiagnosticContext".}
## *
##  Set the yield callback function for this context.
##
##  @see LLVMContext::setYieldCallback()
##

proc contextSetYieldCallback*(c: ContextRef; callback: YieldCallback;
                             opaqueHandle: pointer) {.
    importc: "LLVMContextSetYieldCallback".}
## *
##  Retrieve whether the given context is set to discard all value names.
##
##  @see LLVMContext::shouldDiscardValueNames()
##

proc contextShouldDiscardValueNames*(c: ContextRef): Bool {.
    importc: "LLVMContextShouldDiscardValueNames".}
## *
##  Set whether the given context discards all value names.
##
##  If true, only the names of GlobalValue objects will be available in the IR.
##  This can be used to save memory and runtime, especially in release mode.
##
##  @see LLVMContext::setDiscardValueNames()
##

proc contextSetDiscardValueNames*(c: ContextRef; `discard`: Bool) {.
    importc: "LLVMContextSetDiscardValueNames".}
## *
##  Set whether the given context is in opaque pointer mode.
##
##  @see LLVMContext::setOpaquePointers()
##

proc contextSetOpaquePointers*(c: ContextRef; opaquePointers: Bool) {.
    importc: "LLVMContextSetOpaquePointers".}
## *
##  Destroy a context instance.
##
##  This should be called for every call to LLVMContextCreate() or memory
##  will be leaked.
##

proc contextDispose*(c: ContextRef) {.importc: "LLVMContextDispose".}
## *
##  Return a string representation of the DiagnosticInfo. Use
##  LLVMDisposeMessage to free the string.
##
##  @see DiagnosticInfo::print()
##

proc getDiagInfoDescription*(di: DiagnosticInfoRef): cstring {.
    importc: "LLVMGetDiagInfoDescription".}
## *
##  Return an enum LLVMDiagnosticSeverity.
##
##  @see DiagnosticInfo::getSeverity()
##

proc getDiagInfoSeverity*(di: DiagnosticInfoRef): DiagnosticSeverity {.
    importc: "LLVMGetDiagInfoSeverity".}
proc getMDKindIDInContext*(c: ContextRef; name: cstring; sLen: cuint): cuint {.
    importc: "LLVMGetMDKindIDInContext".}
proc getMDKindID*(name: cstring; sLen: cuint): cuint {.importc: "LLVMGetMDKindID",
    .}
## *
##  Return an unique id given the name of a enum attribute,
##  or 0 if no attribute by that name exists.
##
##  See http://llvm.org/docs/LangRef.html#parameter-attributes
##  and http://llvm.org/docs/LangRef.html#function-attributes
##  for the list of available attributes.
##
##  NB: Attribute names and/or id are subject to change without
##  going through the C API deprecation cycle.
##

proc getEnumAttributeKindForName*(name: cstring; sLen: csize_t): cuint {.
    importc: "LLVMGetEnumAttributeKindForName".}
proc getLastEnumAttributeKind*(): cuint {.importc: "LLVMGetLastEnumAttributeKind",
                                       .}
## *
##  Create an enum attribute.
##

proc createEnumAttribute*(c: ContextRef; kindID: cuint; val: uint64): AttributeRef {.
    importc: "LLVMCreateEnumAttribute".}
## *
##  Get the unique id corresponding to the enum attribute
##  passed as argument.
##

proc getEnumAttributeKind*(a: AttributeRef): cuint {.
    importc: "LLVMGetEnumAttributeKind".}
## *
##  Get the enum attribute's value. 0 is returned if none exists.
##

proc getEnumAttributeValue*(a: AttributeRef): uint64 {.
    importc: "LLVMGetEnumAttributeValue".}
## *
##  Create a type attribute
##

proc createTypeAttribute*(c: ContextRef; kindID: cuint; typeRef: TypeRef): AttributeRef {.
    importc: "LLVMCreateTypeAttribute".}
## *
##  Get the type attribute's value.
##

proc getTypeAttributeValue*(a: AttributeRef): TypeRef {.
    importc: "LLVMGetTypeAttributeValue".}
## *
##  Create a string attribute.
##

proc createStringAttribute*(c: ContextRef; k: cstring; kLength: cuint; v: cstring;
                           vLength: cuint): AttributeRef {.
    importc: "LLVMCreateStringAttribute".}
## *
##  Get the string attribute's kind.
##

proc getStringAttributeKind*(a: AttributeRef; length: ptr cuint): cstring {.
    importc: "LLVMGetStringAttributeKind".}
## *
##  Get the string attribute's value.
##

proc getStringAttributeValue*(a: AttributeRef; length: ptr cuint): cstring {.
    importc: "LLVMGetStringAttributeValue".}
## *
##  Check for the different types of attributes.
##

proc isEnumAttribute*(a: AttributeRef): Bool {.importc: "LLVMIsEnumAttribute",
    .}
proc isStringAttribute*(a: AttributeRef): Bool {.importc: "LLVMIsStringAttribute",
    .}
proc isTypeAttribute*(a: AttributeRef): Bool {.importc: "LLVMIsTypeAttribute",
    .}
## *
##  Obtain a Type from a context by its registered name.
##

proc getTypeByName2*(c: ContextRef; name: cstring): TypeRef {.
    importc: "LLVMGetTypeByName2".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreModule Modules
##
##  Modules represent the top-level structure in an LLVM program. An LLVM
##  module is effectively a translation unit or a collection of
##  translation units merged together.
##
##  @{
##
## *
##  Create a new, empty module in the global context.
##
##  This is equivalent to calling LLVMModuleCreateWithNameInContext with
##  LLVMGetGlobalContext() as the context parameter.
##
##  Every invocation should be paired with LLVMDisposeModule() or memory
##  will be leaked.
##

proc moduleCreateWithName*(moduleID: cstring): ModuleRef {.
    importc: "LLVMModuleCreateWithName".}
## *
##  Create a new, empty module in a specific context.
##
##  Every invocation should be paired with LLVMDisposeModule() or memory
##  will be leaked.
##

proc moduleCreateWithNameInContext*(moduleID: cstring; c: ContextRef): ModuleRef {.
    importc: "LLVMModuleCreateWithNameInContext".}
## *
##  Return an exact copy of the specified module.
##

proc cloneModule*(m: ModuleRef): ModuleRef {.importc: "LLVMCloneModule",
    .}
## *
##  Destroy a module instance.
##
##  This must be called for every created module or memory will be
##  leaked.
##

proc disposeModule*(m: ModuleRef) {.importc: "LLVMDisposeModule".}
## *
##  Obtain the identifier of a module.
##
##  @param M Module to obtain identifier of
##  @param Len Out parameter which holds the length of the returned string.
##  @return The identifier of M.
##  @see Module::getModuleIdentifier()
##

proc getModuleIdentifier*(m: ModuleRef; len: ptr csize_t): cstring {.
    importc: "LLVMGetModuleIdentifier".}
## *
##  Set the identifier of a module to a string Ident with length Len.
##
##  @param M The module to set identifier
##  @param Ident The string to set M's identifier to
##  @param Len Length of Ident
##  @see Module::setModuleIdentifier()
##

proc setModuleIdentifier*(m: ModuleRef; ident: cstring; len: csize_t) {.
    importc: "LLVMSetModuleIdentifier".}
## *
##  Obtain the module's original source file name.
##
##  @param M Module to obtain the name of
##  @param Len Out parameter which holds the length of the returned string
##  @return The original source file name of M
##  @see Module::getSourceFileName()
##

proc getSourceFileName*(m: ModuleRef; len: ptr csize_t): cstring {.
    importc: "LLVMGetSourceFileName".}
## *
##  Set the original source file name of a module to a string Name with length
##  Len.
##
##  @param M The module to set the source file name of
##  @param Name The string to set M's source file name to
##  @param Len Length of Name
##  @see Module::setSourceFileName()
##

proc setSourceFileName*(m: ModuleRef; name: cstring; len: csize_t) {.
    importc: "LLVMSetSourceFileName".}
## *
##  Obtain the data layout for a module.
##
##  @see Module::getDataLayoutStr()
##
##  LLVMGetDataLayout is DEPRECATED, as the name is not only incorrect,
##  but match the name of another method on the module. Prefer the use
##  of LLVMGetDataLayoutStr, which is not ambiguous.
##

proc getDataLayoutStr*(m: ModuleRef): cstring {.importc: "LLVMGetDataLayoutStr",
    .}
proc getDataLayout*(m: ModuleRef): cstring {.importc: "LLVMGetDataLayout",
    .}
## *
##  Set the data layout for a module.
##
##  @see Module::setDataLayout()
##

proc setDataLayout*(m: ModuleRef; dataLayoutStr: cstring) {.
    importc: "LLVMSetDataLayout".}
## *
##  Obtain the target triple for a module.
##
##  @see Module::getTargetTriple()
##

proc getTarget*(m: ModuleRef): cstring {.importc: "LLVMGetTarget".}
## *
##  Set the target triple for a module.
##
##  @see Module::setTargetTriple()
##

proc setTarget*(m: ModuleRef; triple: cstring) {.importc: "LLVMSetTarget",
    .}
## *
##  Returns the module flags as an array of flag-key-value triples.  The caller
##  is responsible for freeing this array by calling
##  \c LLVMDisposeModuleFlagsMetadata.
##
##  @see Module::getModuleFlagsMetadata()
##

proc copyModuleFlagsMetadata*(m: ModuleRef; len: ptr csize_t): ptr ModuleFlagEntry {.
    importc: "LLVMCopyModuleFlagsMetadata".}
## *
##  Destroys module flags metadata entries.
##

proc disposeModuleFlagsMetadata*(entries: ptr ModuleFlagEntry) {.
    importc: "LLVMDisposeModuleFlagsMetadata".}
## *
##  Returns the flag behavior for a module flag entry at a specific index.
##
##  @see Module::ModuleFlagEntry::Behavior
##

proc moduleFlagEntriesGetFlagBehavior*(entries: ptr ModuleFlagEntry; index: cuint): ModuleFlagBehavior {.
    importc: "LLVMModuleFlagEntriesGetFlagBehavior".}
## *
##  Returns the key for a module flag entry at a specific index.
##
##  @see Module::ModuleFlagEntry::Key
##

proc moduleFlagEntriesGetKey*(entries: ptr ModuleFlagEntry; index: cuint;
                             len: ptr csize_t): cstring {.
    importc: "LLVMModuleFlagEntriesGetKey".}
## *
##  Returns the metadata for a module flag entry at a specific index.
##
##  @see Module::ModuleFlagEntry::Val
##

proc moduleFlagEntriesGetMetadata*(entries: ptr ModuleFlagEntry; index: cuint): MetadataRef {.
    importc: "LLVMModuleFlagEntriesGetMetadata".}
## *
##  Add a module-level flag to the module-level flags metadata if it doesn't
##  already exist.
##
##  @see Module::getModuleFlag()
##

proc getModuleFlag*(m: ModuleRef; key: cstring; keyLen: csize_t): MetadataRef {.
    importc: "LLVMGetModuleFlag".}
## *
##  Add a module-level flag to the module-level flags metadata if it doesn't
##  already exist.
##
##  @see Module::addModuleFlag()
##

proc addModuleFlag*(m: ModuleRef; behavior: ModuleFlagBehavior; key: cstring;
                   keyLen: csize_t; val: MetadataRef) {.
    importc: "LLVMAddModuleFlag".}
## *
##  Dump a representation of a module to stderr.
##
##  @see Module::dump()
##

proc dumpModule*(m: ModuleRef) {.importc: "LLVMDumpModule".}
## *
##  Print a representation of a module to a file. The ErrorMessage needs to be
##  disposed with LLVMDisposeMessage. Returns 0 on success, 1 otherwise.
##
##  @see Module::print()
##

proc printModuleToFile*(m: ModuleRef; filename: cstring; errorMessage: cstringArray): Bool {.
    importc: "LLVMPrintModuleToFile".}
## *
##  Return a string representation of the module. Use
##  LLVMDisposeMessage to free the string.
##
##  @see Module::print()
##

proc printModuleToString*(m: ModuleRef): cstring {.
    importc: "LLVMPrintModuleToString".}
## *
##  Get inline assembly for a module.
##
##  @see Module::getModuleInlineAsm()
##

proc getModuleInlineAsm*(m: ModuleRef; len: ptr csize_t): cstring {.
    importc: "LLVMGetModuleInlineAsm".}
## *
##  Set inline assembly for a module.
##
##  @see Module::setModuleInlineAsm()
##

proc setModuleInlineAsm2*(m: ModuleRef; `asm`: cstring; len: csize_t) {.
    importc: "LLVMSetModuleInlineAsm2".}
## *
##  Append inline assembly to a module.
##
##  @see Module::appendModuleInlineAsm()
##

proc appendModuleInlineAsm*(m: ModuleRef; `asm`: cstring; len: csize_t) {.
    importc: "LLVMAppendModuleInlineAsm".}
## *
##  Create the specified uniqued inline asm string.
##
##  @see InlineAsm::get()
##

proc getInlineAsm*(ty: TypeRef; asmString: cstring; asmStringSize: csize_t;
                  constraints: cstring; constraintsSize: csize_t;
                  hasSideEffects: Bool; isAlignStack: Bool;
                  dialect: InlineAsmDialect; canThrow: Bool): ValueRef {.
    importc: "LLVMGetInlineAsm".}
## *
##  Obtain the context to which this module is associated.
##
##  @see Module::getContext()
##

proc getModuleContext*(m: ModuleRef): ContextRef {.importc: "LLVMGetModuleContext",
    .}
## * Deprecated: Use LLVMGetTypeByName2 instead.

proc getTypeByName*(m: ModuleRef; name: cstring): TypeRef {.
    importc: "LLVMGetTypeByName".}
## *
##  Obtain an iterator to the first NamedMDNode in a Module.
##
##  @see llvm::Module::named_metadata_begin()
##

proc getFirstNamedMetadata*(m: ModuleRef): NamedMDNodeRef {.
    importc: "LLVMGetFirstNamedMetadata".}
## *
##  Obtain an iterator to the last NamedMDNode in a Module.
##
##  @see llvm::Module::named_metadata_end()
##

proc getLastNamedMetadata*(m: ModuleRef): NamedMDNodeRef {.
    importc: "LLVMGetLastNamedMetadata".}
## *
##  Advance a NamedMDNode iterator to the next NamedMDNode.
##
##  Returns NULL if the iterator was already at the end and there are no more
##  named metadata nodes.
##

proc getNextNamedMetadata*(namedMDNode: NamedMDNodeRef): NamedMDNodeRef {.
    importc: "LLVMGetNextNamedMetadata".}
## *
##  Decrement a NamedMDNode iterator to the previous NamedMDNode.
##
##  Returns NULL if the iterator was already at the beginning and there are
##  no previous named metadata nodes.
##

proc getPreviousNamedMetadata*(namedMDNode: NamedMDNodeRef): NamedMDNodeRef {.
    importc: "LLVMGetPreviousNamedMetadata".}
## *
##  Retrieve a NamedMDNode with the given name, returning NULL if no such
##  node exists.
##
##  @see llvm::Module::getNamedMetadata()
##

proc getNamedMetadata*(m: ModuleRef; name: cstring; nameLen: csize_t): NamedMDNodeRef {.
    importc: "LLVMGetNamedMetadata".}
## *
##  Retrieve a NamedMDNode with the given name, creating a new node if no such
##  node exists.
##
##  @see llvm::Module::getOrInsertNamedMetadata()
##

proc getOrInsertNamedMetadata*(m: ModuleRef; name: cstring; nameLen: csize_t): NamedMDNodeRef {.
    importc: "LLVMGetOrInsertNamedMetadata".}
## *
##  Retrieve the name of a NamedMDNode.
##
##  @see llvm::NamedMDNode::getName()
##

proc getNamedMetadataName*(namedMD: NamedMDNodeRef; nameLen: ptr csize_t): cstring {.
    importc: "LLVMGetNamedMetadataName".}
## *
##  Obtain the number of operands for named metadata in a module.
##
##  @see llvm::Module::getNamedMetadata()
##

proc getNamedMetadataNumOperands*(m: ModuleRef; name: cstring): cuint {.
    importc: "LLVMGetNamedMetadataNumOperands".}
## *
##  Obtain the named metadata operands for a module.
##
##  The passed LLVMValueRef pointer should refer to an array of
##  LLVMValueRef at least LLVMGetNamedMetadataNumOperands long. This
##  array will be populated with the LLVMValueRef instances. Each
##  instance corresponds to a llvm::MDNode.
##
##  @see llvm::Module::getNamedMetadata()
##  @see llvm::MDNode::getOperand()
##

proc getNamedMetadataOperands*(m: ModuleRef; name: cstring; dest: ptr ValueRef) {.
    importc: "LLVMGetNamedMetadataOperands".}
## *
##  Add an operand to named metadata.
##
##  @see llvm::Module::getNamedMetadata()
##  @see llvm::MDNode::addOperand()
##

proc addNamedMetadataOperand*(m: ModuleRef; name: cstring; val: ValueRef) {.
    importc: "LLVMAddNamedMetadataOperand".}
## *
##  Return the directory of the debug location for this value, which must be
##  an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
##
##  @see llvm::Instruction::getDebugLoc()
##  @see llvm::GlobalVariable::getDebugInfo()
##  @see llvm::Function::getSubprogram()
##

proc getDebugLocDirectory*(val: ValueRef; length: ptr cuint): cstring {.
    importc: "LLVMGetDebugLocDirectory".}
## *
##  Return the filename of the debug location for this value, which must be
##  an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
##
##  @see llvm::Instruction::getDebugLoc()
##  @see llvm::GlobalVariable::getDebugInfo()
##  @see llvm::Function::getSubprogram()
##

proc getDebugLocFilename*(val: ValueRef; length: ptr cuint): cstring {.
    importc: "LLVMGetDebugLocFilename".}
## *
##  Return the line number of the debug location for this value, which must be
##  an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
##
##  @see llvm::Instruction::getDebugLoc()
##  @see llvm::GlobalVariable::getDebugInfo()
##  @see llvm::Function::getSubprogram()
##

proc getDebugLocLine*(val: ValueRef): cuint {.importc: "LLVMGetDebugLocLine",
    .}
## *
##  Return the column number of the debug location for this value, which must be
##  an llvm::Instruction.
##
##  @see llvm::Instruction::getDebugLoc()
##

proc getDebugLocColumn*(val: ValueRef): cuint {.importc: "LLVMGetDebugLocColumn",
    .}
## *
##  Add a function to a module under a specified name.
##
##  @see llvm::Function::Create()
##

proc addFunction*(m: ModuleRef; name: cstring; functionTy: TypeRef): ValueRef {.
    importc: "LLVMAddFunction".}
## *
##  Obtain a Function value from a Module by its name.
##
##  The returned value corresponds to a llvm::Function value.
##
##  @see llvm::Module::getFunction()
##

proc getNamedFunction*(m: ModuleRef; name: cstring): ValueRef {.
    importc: "LLVMGetNamedFunction".}
## *
##  Obtain an iterator to the first Function in a Module.
##
##  @see llvm::Module::begin()
##

proc getFirstFunction*(m: ModuleRef): ValueRef {.importc: "LLVMGetFirstFunction",
    .}
## *
##  Obtain an iterator to the last Function in a Module.
##
##  @see llvm::Module::end()
##

proc getLastFunction*(m: ModuleRef): ValueRef {.importc: "LLVMGetLastFunction",
    .}
## *
##  Advance a Function iterator to the next Function.
##
##  Returns NULL if the iterator was already at the end and there are no more
##  functions.
##

proc getNextFunction*(fn: ValueRef): ValueRef {.importc: "LLVMGetNextFunction",
    .}
## *
##  Decrement a Function iterator to the previous Function.
##
##  Returns NULL if the iterator was already at the beginning and there are
##  no previous functions.
##

proc getPreviousFunction*(fn: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousFunction".}
## * Deprecated: Use LLVMSetModuleInlineAsm2 instead.

proc setModuleInlineAsm*(m: ModuleRef; `asm`: cstring) {.
    importc: "LLVMSetModuleInlineAsm".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreType Types
##
##  Types represent the type of a value.
##
##  Types are associated with a context instance. The context internally
##  deduplicates types so there is only 1 instance of a specific type
##  alive at a time. In other words, a unique type is shared among all
##  consumers within a context.
##
##  A Type in the C API corresponds to llvm::Type.
##
##  Types have the following hierarchy:
##
##    types:
##      integer type
##      real type
##      function type
##      sequence types:
##        array type
##        pointer type
##        vector type
##      void type
##      label type
##      opaque type
##
##  @{
##
## *
##  Obtain the enumerated type of a Type instance.
##
##  @see llvm::Type:getTypeID()
##

proc getTypeKind*(ty: TypeRef): TypeKind {.importc: "LLVMGetTypeKind".}
## *
##  Whether the type has a known size.
##
##  Things that don't have a size are abstract types, labels, and void.a
##
##  @see llvm::Type::isSized()
##

proc typeIsSized*(ty: TypeRef): Bool {.importc: "LLVMTypeIsSized".}
## *
##  Obtain the context to which this type instance is associated.
##
##  @see llvm::Type::getContext()
##

proc getTypeContext*(ty: TypeRef): ContextRef {.importc: "LLVMGetTypeContext",
    .}
## *
##  Dump a representation of a type to stderr.
##
##  @see llvm::Type::dump()
##

proc dumpType*(val: TypeRef) {.importc: "LLVMDumpType".}
## *
##  Return a string representation of the type. Use
##  LLVMDisposeMessage to free the string.
##
##  @see llvm::Type::print()
##

proc printTypeToString*(val: TypeRef): cstring {.importc: "LLVMPrintTypeToString",
    .}
## *
##  @defgroup LLVMCCoreTypeInt Integer Types
##
##  Functions in this section operate on integer types.
##
##  @{
##
## *
##  Obtain an integer type from a context with specified bit width.
##

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
## *
##  Obtain an integer type from the global context with a specified bit
##  width.
##

proc int1Type*(): TypeRef {.importc: "LLVMInt1Type".}
proc int8Type*(): TypeRef {.importc: "LLVMInt8Type".}
proc int16Type*(): TypeRef {.importc: "LLVMInt16Type".}
proc int32Type*(): TypeRef {.importc: "LLVMInt32Type".}
proc int64Type*(): TypeRef {.importc: "LLVMInt64Type".}
proc int128Type*(): TypeRef {.importc: "LLVMInt128Type".}
proc intType*(numBits: cuint): TypeRef {.importc: "LLVMIntType".}
proc getIntTypeWidth*(integerTy: TypeRef): cuint {.importc: "LLVMGetIntTypeWidth",
    .}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreTypeFloat Floating Point Types
##
##  @{
##
## *
##  Obtain a 16-bit floating point type from a context.
##

proc halfTypeInContext*(c: ContextRef): TypeRef {.importc: "LLVMHalfTypeInContext",
    .}
## *
##  Obtain a 16-bit brain floating point type from a context.
##

proc bFloatTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMBFloatTypeInContext".}
## *
##  Obtain a 32-bit floating point type from a context.
##

proc floatTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMFloatTypeInContext".}
## *
##  Obtain a 64-bit floating point type from a context.
##

proc doubleTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMDoubleTypeInContext".}
## *
##  Obtain a 80-bit floating point type (X87) from a context.
##

proc x86FP80TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMX86FP80TypeInContext".}
## *
##  Obtain a 128-bit floating point type (112-bit mantissa) from a
##  context.
##

proc fP128TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMFP128TypeInContext".}
## *
##  Obtain a 128-bit floating point type (two 64-bits) from a context.
##

proc pPCFP128TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMPPCFP128TypeInContext".}
## *
##  Obtain a floating point type from the global context.
##
##  These map to the functions in this group of the same name.
##

proc halfType*(): TypeRef {.importc: "LLVMHalfType".}
proc bFloatType*(): TypeRef {.importc: "LLVMBFloatType".}
proc floatType*(): TypeRef {.importc: "LLVMFloatType".}
proc doubleType*(): TypeRef {.importc: "LLVMDoubleType".}
proc x86FP80Type*(): TypeRef {.importc: "LLVMX86FP80Type".}
proc fP128Type*(): TypeRef {.importc: "LLVMFP128Type".}
proc pPCFP128Type*(): TypeRef {.importc: "LLVMPPCFP128Type".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreTypeFunction Function Types
##
##  @{
##
## *
##  Obtain a function type consisting of a specified signature.
##
##  The function is defined as a tuple of a return Type, a list of
##  parameter types, and whether the function is variadic.
##

proc functionType*(returnType: TypeRef; paramTypes: ptr TypeRef; paramCount: cuint;
                  isVarArg: Bool): TypeRef {.importc: "LLVMFunctionType",
    .}
## *
##  Returns whether a function type is variadic.
##

proc isFunctionVarArg*(functionTy: TypeRef): Bool {.importc: "LLVMIsFunctionVarArg",
    .}
## *
##  Obtain the Type this function Type returns.
##

proc getReturnType*(functionTy: TypeRef): TypeRef {.importc: "LLVMGetReturnType",
    .}
## *
##  Obtain the number of parameters this function accepts.
##

proc countParamTypes*(functionTy: TypeRef): cuint {.importc: "LLVMCountParamTypes",
    .}
## *
##  Obtain the types of a function's parameters.
##
##  The Dest parameter should point to a pre-allocated array of
##  LLVMTypeRef at least LLVMCountParamTypes() large. On return, the
##  first LLVMCountParamTypes() entries in the array will be populated
##  with LLVMTypeRef instances.
##
##  @param FunctionTy The function type to operate on.
##  @param Dest Memory address of an array to be filled with result.
##

proc getParamTypes*(functionTy: TypeRef; dest: ptr TypeRef) {.
    importc: "LLVMGetParamTypes".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreTypeStruct Structure Types
##
##  These functions relate to LLVMTypeRef instances.
##
##  @see llvm::StructType
##
##  @{
##
## *
##  Create a new structure type in a context.
##
##  A structure is specified by a list of inner elements/types and
##  whether these can be packed together.
##
##  @see llvm::StructType::create()
##

proc structTypeInContext*(c: ContextRef; elementTypes: ptr TypeRef;
                         elementCount: cuint; packed: Bool): TypeRef {.
    importc: "LLVMStructTypeInContext".}
## *
##  Create a new structure type in the global context.
##
##  @see llvm::StructType::create()
##

proc structType*(elementTypes: ptr TypeRef; elementCount: cuint; packed: Bool): TypeRef {.
    importc: "LLVMStructType".}
## *
##  Create an empty structure in a context having a specified name.
##
##  @see llvm::StructType::create()
##

proc structCreateNamed*(c: ContextRef; name: cstring): TypeRef {.
    importc: "LLVMStructCreateNamed".}
## *
##  Obtain the name of a structure.
##
##  @see llvm::StructType::getName()
##

proc getStructName*(ty: TypeRef): cstring {.importc: "LLVMGetStructName",
                                        .}
## *
##  Set the contents of a structure type.
##
##  @see llvm::StructType::setBody()
##

proc structSetBody*(structTy: TypeRef; elementTypes: ptr TypeRef; elementCount: cuint;
                   packed: Bool) {.importc: "LLVMStructSetBody".}
## *
##  Get the number of elements defined inside the structure.
##
##  @see llvm::StructType::getNumElements()
##

proc countStructElementTypes*(structTy: TypeRef): cuint {.
    importc: "LLVMCountStructElementTypes".}
## *
##  Get the elements within a structure.
##
##  The function is passed the address of a pre-allocated array of
##  LLVMTypeRef at least LLVMCountStructElementTypes() long. After
##  invocation, this array will be populated with the structure's
##  elements. The objects in the destination array will have a lifetime
##  of the structure type itself, which is the lifetime of the context it
##  is contained in.
##

proc getStructElementTypes*(structTy: TypeRef; dest: ptr TypeRef) {.
    importc: "LLVMGetStructElementTypes".}
## *
##  Get the type of the element at a given index in the structure.
##
##  @see llvm::StructType::getTypeAtIndex()
##

proc structGetTypeAtIndex*(structTy: TypeRef; i: cuint): TypeRef {.
    importc: "LLVMStructGetTypeAtIndex".}
## *
##  Determine whether a structure is packed.
##
##  @see llvm::StructType::isPacked()
##

proc isPackedStruct*(structTy: TypeRef): Bool {.importc: "LLVMIsPackedStruct",
    .}
## *
##  Determine whether a structure is opaque.
##
##  @see llvm::StructType::isOpaque()
##

proc isOpaqueStruct*(structTy: TypeRef): Bool {.importc: "LLVMIsOpaqueStruct",
    .}
## *
##  Determine whether a structure is literal.
##
##  @see llvm::StructType::isLiteral()
##

proc isLiteralStruct*(structTy: TypeRef): Bool {.importc: "LLVMIsLiteralStruct",
    .}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreTypeSequential Sequential Types
##
##  Sequential types represents "arrays" of types. This is a super class
##  for array, vector, and pointer types.
##
##  @{
##
## *
##  Obtain the element type of an array or vector type.
##
##  This currently also works for pointer types, but this usage is deprecated.
##
##  @see llvm::SequentialType::getElementType()
##

proc getElementType*(ty: TypeRef): TypeRef {.importc: "LLVMGetElementType",
    .}
## *
##  Returns type's subtypes
##
##  @see llvm::Type::subtypes()
##

proc getSubtypes*(tp: TypeRef; arr: ptr TypeRef) {.importc: "LLVMGetSubtypes",
    .}
## *
##   Return the number of types in the derived type.
##
##  @see llvm::Type::getNumContainedTypes()
##

proc getNumContainedTypes*(tp: TypeRef): cuint {.
    importc: "LLVMGetNumContainedTypes".}
## *
##  Create a fixed size array type that refers to a specific type.
##
##  The created type will exist in the context that its element type
##  exists in.
##
##  @see llvm::ArrayType::get()
##

proc arrayType*(elementType: TypeRef; elementCount: cuint): TypeRef {.
    importc: "LLVMArrayType".}
## *
##  Obtain the length of an array type.
##
##  This only works on types that represent arrays.
##
##  @see llvm::ArrayType::getNumElements()
##

proc getArrayLength*(arrayTy: TypeRef): cuint {.importc: "LLVMGetArrayLength",
    .}
## *
##  Create a pointer type that points to a defined type.
##
##  The created type will exist in the context that its pointee type
##  exists in.
##
##  @see llvm::PointerType::get()
##

proc pointerType*(elementType: TypeRef; addressSpace: cuint): TypeRef {.
    importc: "LLVMPointerType".}
## *
##  Determine whether a pointer is opaque.
##
##  True if this is an instance of an opaque PointerType.
##
##  @see llvm::Type::isOpaquePointerTy()
##

proc pointerTypeIsOpaque*(ty: TypeRef): Bool {.importc: "LLVMPointerTypeIsOpaque",
    .}
## *
##  Create an opaque pointer type in a context.
##
##  @see llvm::PointerType::get()
##

proc pointerTypeInContext*(c: ContextRef; addressSpace: cuint): TypeRef {.
    importc: "LLVMPointerTypeInContext".}
## *
##  Obtain the address space of a pointer type.
##
##  This only works on types that represent pointers.
##
##  @see llvm::PointerType::getAddressSpace()
##

proc getPointerAddressSpace*(pointerTy: TypeRef): cuint {.
    importc: "LLVMGetPointerAddressSpace".}
## *
##  Create a vector type that contains a defined type and has a specific
##  number of elements.
##
##  The created type will exist in the context thats its element type
##  exists in.
##
##  @see llvm::VectorType::get()
##

proc vectorType*(elementType: TypeRef; elementCount: cuint): TypeRef {.
    importc: "LLVMVectorType".}
## *
##  Create a vector type that contains a defined type and has a scalable
##  number of elements.
##
##  The created type will exist in the context thats its element type
##  exists in.
##
##  @see llvm::ScalableVectorType::get()
##

proc scalableVectorType*(elementType: TypeRef; elementCount: cuint): TypeRef {.
    importc: "LLVMScalableVectorType".}
## *
##  Obtain the (possibly scalable) number of elements in a vector type.
##
##  This only works on types that represent vectors (fixed or scalable).
##
##  @see llvm::VectorType::getNumElements()
##

proc getVectorSize*(vectorTy: TypeRef): cuint {.importc: "LLVMGetVectorSize",
    .}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreTypeOther Other Types
##
##  @{
##
## *
##  Create a void type in a context.
##

proc voidTypeInContext*(c: ContextRef): TypeRef {.importc: "LLVMVoidTypeInContext",
    .}
## *
##  Create a label type in a context.
##

proc labelTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMLabelTypeInContext".}
## *
##  Create a X86 MMX type in a context.
##

proc x86MMXTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMX86MMXTypeInContext".}
## *
##  Create a X86 AMX type in a context.
##

proc x86AMXTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMX86AMXTypeInContext".}
## *
##  Create a token type in a context.
##

proc tokenTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMTokenTypeInContext".}
## *
##  Create a metadata type in a context.
##

proc metadataTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMMetadataTypeInContext".}
## *
##  These are similar to the above functions except they operate on the
##  global context.
##

proc voidType*(): TypeRef {.importc: "LLVMVoidType".}
proc labelType*(): TypeRef {.importc: "LLVMLabelType".}
proc x86MMXType*(): TypeRef {.importc: "LLVMX86MMXType".}
proc x86AMXType*(): TypeRef {.importc: "LLVMX86AMXType".}
## *
##  @}
##
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValues Values
##
##  The bulk of LLVM's object model consists of values, which comprise a very
##  rich type hierarchy.
##
##  LLVMValueRef essentially represents llvm::Value. There is a rich
##  hierarchy of classes within this type. Depending on the instance
##  obtained, not all APIs are available.
##
##  Callers can determine the type of an LLVMValueRef by calling the
##  LLVMIsA* family of functions (e.g. LLVMIsAArgument()). These
##  functions are defined by a macro, so it isn't obvious which are
##  available by looking at the Doxygen source code. Instead, look at the
##  source definition of LLVM_FOR_EACH_VALUE_SUBCLASS and note the list
##  of value names given. These value names also correspond to classes in
##  the llvm::Value hierarchy.
##
##  @{
##

template for_Each_Value_Subclass*(`macro`: untyped): untyped =
  `macro`(argument)

## !!!Ignored construct:  macro ( BasicBlock ) macro ( InlineAsm ) macro ( User ) macro ( Constant ) macro ( BlockAddress ) macro ( ConstantAggregateZero ) macro ( ConstantArray ) macro ( ConstantDataSequential ) macro ( ConstantDataArray ) macro ( ConstantDataVector ) macro ( ConstantExpr ) macro ( ConstantFP ) macro ( ConstantInt ) macro ( ConstantPointerNull ) macro ( ConstantStruct ) macro ( ConstantTokenNone ) macro ( ConstantVector ) macro ( GlobalValue ) macro ( GlobalAlias ) macro ( GlobalObject ) macro ( Function ) macro ( GlobalVariable ) macro ( GlobalIFunc ) macro ( UndefValue ) macro ( PoisonValue ) macro ( Instruction ) macro ( UnaryOperator ) macro ( BinaryOperator ) macro ( CallInst ) macro ( IntrinsicInst ) macro ( DbgInfoIntrinsic ) macro ( DbgVariableIntrinsic ) macro ( DbgDeclareInst ) macro ( DbgLabelInst ) macro ( MemIntrinsic ) macro ( MemCpyInst ) macro ( MemMoveInst ) macro ( MemSetInst ) macro ( CmpInst ) macro ( FCmpInst ) macro ( ICmpInst ) macro ( ExtractElementInst ) macro ( GetElementPtrInst ) macro ( InsertElementInst ) macro ( InsertValueInst ) macro ( LandingPadInst ) macro ( PHINode ) macro ( SelectInst ) macro ( ShuffleVectorInst ) macro ( StoreInst ) macro ( BranchInst ) macro ( IndirectBrInst ) macro ( InvokeInst ) macro ( ReturnInst ) macro ( SwitchInst ) macro ( UnreachableInst ) macro ( ResumeInst ) macro ( CleanupReturnInst ) macro ( CatchReturnInst ) macro ( CatchSwitchInst ) macro ( CallBrInst ) macro ( FuncletPadInst ) macro ( CatchPadInst ) macro ( CleanupPadInst ) macro ( UnaryInstruction ) macro ( AllocaInst ) macro ( CastInst ) macro ( AddrSpaceCastInst ) macro ( BitCastInst ) macro ( FPExtInst ) macro ( FPToSIInst ) macro ( FPToUIInst ) macro ( FPTruncInst ) macro ( IntToPtrInst ) macro ( PtrToIntInst ) macro ( SExtInst ) macro ( SIToFPInst ) macro ( TruncInst ) macro ( UIToFPInst ) macro ( ZExtInst ) macro ( ExtractValueInst ) macro ( LoadInst ) macro ( VAArgInst ) macro ( FreezeInst ) macro ( AtomicCmpXchgInst ) macro ( AtomicRMWInst ) macro ( FenceInst ) [NewLine] *
##  @defgroup LLVMCCoreValueGeneral General APIs
##
##  Functions in this section work on all LLVMValueRef instances,
##  regardless of their sub-type. They correspond to functions available
##  on llvm::Value.
##
##  @{
##  *
##  Obtain the type of a value.
##
##  @see llvm::Value::getType()
##  LLVMTypeRef LLVMTypeOf ( LLVMValueRef Val ) ;
## Error: expected ';'!!!

## *
##  Obtain the enumerated type of a Value instance.
##
##  @see llvm::Value::getValueID()
##

proc getValueKind*(val: ValueRef): ValueKind {.importc: "LLVMGetValueKind",
    .}
## *
##  Obtain the string name of a value.
##
##  @see llvm::Value::getName()
##

proc getValueName2*(val: ValueRef; length: ptr csize_t): cstring {.
    importc: "LLVMGetValueName2".}
## *
##  Set the string name of a value.
##
##  @see llvm::Value::setName()
##

proc setValueName2*(val: ValueRef; name: cstring; nameLen: csize_t) {.
    importc: "LLVMSetValueName2".}
## *
##  Dump a representation of a value to stderr.
##
##  @see llvm::Value::dump()
##

proc dumpValue*(val: ValueRef) {.importc: "LLVMDumpValue".}
## *
##  Return a string representation of the value. Use
##  LLVMDisposeMessage to free the string.
##
##  @see llvm::Value::print()
##

proc printValueToString*(val: ValueRef): cstring {.
    importc: "LLVMPrintValueToString".}
## *
##  Replace all uses of a value with another one.
##
##  @see llvm::Value::replaceAllUsesWith()
##

proc replaceAllUsesWith*(oldVal: ValueRef; newVal: ValueRef) {.
    importc: "LLVMReplaceAllUsesWith".}
## *
##  Determine whether the specified value instance is constant.
##

proc isConstant*(val: ValueRef): Bool {.importc: "LLVMIsConstant".}
## *
##  Determine whether a value instance is undefined.
##

proc isUndef*(val: ValueRef): Bool {.importc: "LLVMIsUndef".}
## *
##  Determine whether a value instance is poisonous.
##

proc isPoison*(val: ValueRef): Bool {.importc: "LLVMIsPoison".}
## *
##  Convert value instances between types.
##
##  Internally, an LLVMValueRef is "pinned" to a specific type. This
##  series of functions allows you to cast an instance to a specific
##  type.
##
##  If the cast is not valid for the specified type, NULL is returned.
##
##  @see llvm::dyn_cast_or_null<>
##

template declare_Value_Cast*(name: untyped): untyped =
  valueRef

## !!!Ignored construct:  LLVMIsA ## name ( LLVMValueRef Val ) ;
## Error: expected ';'!!!

## !!!Ignored construct:  [NewLine] LLVM_FOR_EACH_VALUE_SUBCLASS ( LLVM_DECLARE_VALUE_CAST ) LLVMValueRef LLVMIsAMDNode ( LLVMValueRef Val ) ;
## Error: did not expect [NewLine]!!!

proc isAMDString*(val: ValueRef): ValueRef {.importc: "LLVMIsAMDString",
    .}
## * Deprecated: Use LLVMGetValueName2 instead.

proc getValueName*(val: ValueRef): cstring {.importc: "LLVMGetValueName",
    .}
## * Deprecated: Use LLVMSetValueName2 instead.

proc setValueName*(val: ValueRef; name: cstring) {.importc: "LLVMSetValueName",
    .}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueUses Usage
##
##  This module defines functions that allow you to inspect the uses of a
##  LLVMValueRef.
##
##  It is possible to obtain an LLVMUseRef for any LLVMValueRef instance.
##  Each LLVMUseRef (which corresponds to a llvm::Use instance) holds a
##  llvm::User and llvm::Value.
##
##  @{
##
## *
##  Obtain the first use of a value.
##
##  Uses are obtained in an iterator fashion. First, call this function
##  to obtain a reference to the first use. Then, call LLVMGetNextUse()
##  on that instance and all subsequently obtained instances until
##  LLVMGetNextUse() returns NULL.
##
##  @see llvm::Value::use_begin()
##

proc getFirstUse*(val: ValueRef): UseRef {.importc: "LLVMGetFirstUse".}
## *
##  Obtain the next use of a value.
##
##  This effectively advances the iterator. It returns NULL if you are on
##  the final use and no more are available.
##

proc getNextUse*(u: UseRef): UseRef {.importc: "LLVMGetNextUse".}
## *
##  Obtain the user value for a user.
##
##  The returned value corresponds to a llvm::User type.
##
##  @see llvm::Use::getUser()
##

proc getUser*(u: UseRef): ValueRef {.importc: "LLVMGetUser".}
## *
##  Obtain the value this use corresponds to.
##
##  @see llvm::Use::get().
##

proc getUsedValue*(u: UseRef): ValueRef {.importc: "LLVMGetUsedValue".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueUser User value
##
##  Function in this group pertain to LLVMValueRef instances that descent
##  from llvm::User. This includes constants, instructions, and
##  operators.
##
##  @{
##
## *
##  Obtain an operand at a specific index in a llvm::User value.
##
##  @see llvm::User::getOperand()
##

proc getOperand*(val: ValueRef; index: cuint): ValueRef {.importc: "LLVMGetOperand",
    .}
## *
##  Obtain the use of an operand at a specific index in a llvm::User value.
##
##  @see llvm::User::getOperandUse()
##

proc getOperandUse*(val: ValueRef; index: cuint): UseRef {.
    importc: "LLVMGetOperandUse".}
## *
##  Set an operand at a specific index in a llvm::User value.
##
##  @see llvm::User::setOperand()
##

proc setOperand*(user: ValueRef; index: cuint; val: ValueRef) {.
    importc: "LLVMSetOperand".}
## *
##  Obtain the number of operands in a llvm::User value.
##
##  @see llvm::User::getNumOperands()
##

proc getNumOperands*(val: ValueRef): cint {.importc: "LLVMGetNumOperands",
                                        .}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueConstant Constants
##
##  This section contains APIs for interacting with LLVMValueRef that
##  correspond to llvm::Constant instances.
##
##  These functions will work for any LLVMValueRef in the llvm::Constant
##  class hierarchy.
##
##  @{
##
## *
##  Obtain a constant value referring to the null instance of a type.
##
##  @see llvm::Constant::getNullValue()
##

proc constNull*(ty: TypeRef): ValueRef {.importc: "LLVMConstNull".}
##  all zeroes
## *
##  Obtain a constant value referring to the instance of a type
##  consisting of all ones.
##
##  This is only valid for integer types.
##
##  @see llvm::Constant::getAllOnesValue()
##

proc constAllOnes*(ty: TypeRef): ValueRef {.importc: "LLVMConstAllOnes",
                                        .}
## *
##  Obtain a constant value referring to an undefined value of a type.
##
##  @see llvm::UndefValue::get()
##

proc getUndef*(ty: TypeRef): ValueRef {.importc: "LLVMGetUndef".}
## *
##  Obtain a constant value referring to a poison value of a type.
##
##  @see llvm::PoisonValue::get()
##

proc getPoison*(ty: TypeRef): ValueRef {.importc: "LLVMGetPoison".}
## *
##  Determine whether a value instance is null.
##
##  @see llvm::Constant::isNullValue()
##

proc isNull*(val: ValueRef): Bool {.importc: "LLVMIsNull".}
## *
##  Obtain a constant that is a constant pointer pointing to NULL for a
##  specified type.
##

proc constPointerNull*(ty: TypeRef): ValueRef {.importc: "LLVMConstPointerNull",
    .}
## *
##  @defgroup LLVMCCoreValueConstantScalar Scalar constants
##
##  Functions in this group model LLVMValueRef instances that correspond
##  to constants referring to scalar types.
##
##  For integer types, the LLVMTypeRef parameter should correspond to a
##  llvm::IntegerType instance and the returned LLVMValueRef will
##  correspond to a llvm::ConstantInt.
##
##  For floating point types, the LLVMTypeRef returned corresponds to a
##  llvm::ConstantFP.
##
##  @{
##
## *
##  Obtain a constant value for an integer type.
##
##  The returned value corresponds to a llvm::ConstantInt.
##
##  @see llvm::ConstantInt::get()
##
##  @param IntTy Integer type to obtain value of.
##  @param N The value the returned instance should refer to.
##  @param SignExtend Whether to sign extend the produced value.
##

proc constInt*(intTy: TypeRef; n: culonglong; signExtend: Bool): ValueRef {.
    importc: "LLVMConstInt".}
## *
##  Obtain a constant value for an integer of arbitrary precision.
##
##  @see llvm::ConstantInt::get()
##

proc constIntOfArbitraryPrecision*(intTy: TypeRef; numWords: cuint;
                                  words: ptr uint64): ValueRef {.
    importc: "LLVMConstIntOfArbitraryPrecision".}
## *
##  Obtain a constant value for an integer parsed from a string.
##
##  A similar API, LLVMConstIntOfStringAndSize is also available. If the
##  string's length is available, it is preferred to call that function
##  instead.
##
##  @see llvm::ConstantInt::get()
##

proc constIntOfString*(intTy: TypeRef; text: cstring; radix: uint8T): ValueRef {.
    importc: "LLVMConstIntOfString".}
## *
##  Obtain a constant value for an integer parsed from a string with
##  specified length.
##
##  @see llvm::ConstantInt::get()
##

proc constIntOfStringAndSize*(intTy: TypeRef; text: cstring; sLen: cuint; radix: uint8T): ValueRef {.
    importc: "LLVMConstIntOfStringAndSize".}
## *
##  Obtain a constant value referring to a double floating point value.
##

proc constReal*(realTy: TypeRef; n: cdouble): ValueRef {.importc: "LLVMConstReal",
    .}
## *
##  Obtain a constant for a floating point value parsed from a string.
##
##  A similar API, LLVMConstRealOfStringAndSize is also available. It
##  should be used if the input string's length is known.
##

proc constRealOfString*(realTy: TypeRef; text: cstring): ValueRef {.
    importc: "LLVMConstRealOfString".}
## *
##  Obtain a constant for a floating point value parsed from a string.
##

proc constRealOfStringAndSize*(realTy: TypeRef; text: cstring; sLen: cuint): ValueRef {.
    importc: "LLVMConstRealOfStringAndSize".}
## *
##  Obtain the zero extended value for an integer constant value.
##
##  @see llvm::ConstantInt::getZExtValue()
##

proc constIntGetZExtValue*(constantVal: ValueRef): culonglong {.
    importc: "LLVMConstIntGetZExtValue".}
## *
##  Obtain the sign extended value for an integer constant value.
##
##  @see llvm::ConstantInt::getSExtValue()
##

proc constIntGetSExtValue*(constantVal: ValueRef): clonglong {.
    importc: "LLVMConstIntGetSExtValue".}
## *
##  Obtain the double value for an floating point constant value.
##  losesInfo indicates if some precision was lost in the conversion.
##
##  @see llvm::ConstantFP::getDoubleValue
##

proc constRealGetDouble*(constantVal: ValueRef; losesInfo: ptr Bool): cdouble {.
    importc: "LLVMConstRealGetDouble".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueConstantComposite Composite Constants
##
##  Functions in this group operate on composite constants.
##
##  @{
##
## *
##  Create a ConstantDataSequential and initialize it with a string.
##
##  @see llvm::ConstantDataArray::getString()
##

proc constStringInContext*(c: ContextRef; str: cstring; length: cuint;
                          dontNullTerminate: Bool): ValueRef {.
    importc: "LLVMConstStringInContext".}
## *
##  Create a ConstantDataSequential with string content in the global context.
##
##  This is the same as LLVMConstStringInContext except it operates on the
##  global context.
##
##  @see LLVMConstStringInContext()
##  @see llvm::ConstantDataArray::getString()
##

proc constString*(str: cstring; length: cuint; dontNullTerminate: Bool): ValueRef {.
    importc: "LLVMConstString".}
## *
##  Returns true if the specified constant is an array of i8.
##
##  @see ConstantDataSequential::getAsString()
##

proc isConstantString*(c: ValueRef): Bool {.importc: "LLVMIsConstantString",
                                        .}
## *
##  Get the given constant data sequential as a string.
##
##  @see ConstantDataSequential::getAsString()
##

proc getAsString*(c: ValueRef; length: ptr csize_t): cstring {.
    importc: "LLVMGetAsString".}
## *
##  Create an anonymous ConstantStruct with the specified values.
##
##  @see llvm::ConstantStruct::getAnon()
##

proc constStructInContext*(c: ContextRef; constantVals: ptr ValueRef; count: cuint;
                          packed: Bool): ValueRef {.
    importc: "LLVMConstStructInContext".}
## *
##  Create a ConstantStruct in the global Context.
##
##  This is the same as LLVMConstStructInContext except it operates on the
##  global Context.
##
##  @see LLVMConstStructInContext()
##

proc constStruct*(constantVals: ptr ValueRef; count: cuint; packed: Bool): ValueRef {.
    importc: "LLVMConstStruct".}
## *
##  Create a ConstantArray from values.
##
##  @see llvm::ConstantArray::get()
##

proc constArray*(elementTy: TypeRef; constantVals: ptr ValueRef; length: cuint): ValueRef {.
    importc: "LLVMConstArray".}
## *
##  Create a non-anonymous ConstantStruct from values.
##
##  @see llvm::ConstantStruct::get()
##

proc constNamedStruct*(structTy: TypeRef; constantVals: ptr ValueRef; count: cuint): ValueRef {.
    importc: "LLVMConstNamedStruct".}
## *
##  Get element of a constant aggregate (struct, array or vector) at the
##  specified index. Returns null if the index is out of range, or it's not
##  possible to determine the element (e.g., because the constant is a
##  constant expression.)
##
##  @see llvm::Constant::getAggregateElement()
##

proc getAggregateElement*(c: ValueRef; idx: cuint): ValueRef {.
    importc: "LLVMGetAggregateElement".}
## *
##  Get an element at specified index as a constant.
##
##  @see ConstantDataSequential::getElementAsConstant()
##

proc getElementAsConstant*(c: ValueRef; idx: cuint): ValueRef {.
    importc: "LLVMGetElementAsConstant".}
## *
##  Create a ConstantVector from values.
##
##  @see llvm::ConstantVector::get()
##

proc constVector*(scalarConstantVals: ptr ValueRef; size: cuint): ValueRef {.
    importc: "LLVMConstVector".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueConstantExpressions Constant Expressions
##
##  Functions in this group correspond to APIs on llvm::ConstantExpr.
##
##  @see llvm::ConstantExpr.
##
##  @{
##

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
## * Deprecated: Use LLVMGetInlineAsm instead.

proc constInlineAsm*(ty: TypeRef; asmString: cstring; constraints: cstring;
                    hasSideEffects: Bool; isAlignStack: Bool): ValueRef {.
    importc: "LLVMConstInlineAsm".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueConstantGlobals Global Values
##
##  This group contains functions that operate on global values. Functions in
##  this group relate to functions in the llvm::GlobalValue class tree.
##
##  @see llvm::GlobalValue
##
##  @{
##

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
## *
##  Returns the "value type" of a global value.  This differs from the formal
##  type of a global value which is always a pointer type.
##
##  @see llvm::GlobalValue::getValueType()
##

proc globalGetValueType*(global: ValueRef): TypeRef {.
    importc: "LLVMGlobalGetValueType".}
## * Deprecated: Use LLVMGetUnnamedAddress instead.

proc hasUnnamedAddr*(global: ValueRef): Bool {.importc: "LLVMHasUnnamedAddr",
    .}
## * Deprecated: Use LLVMSetUnnamedAddress instead.

proc setUnnamedAddr*(global: ValueRef; hasUnnamedAddr: Bool) {.
    importc: "LLVMSetUnnamedAddr".}
## *
##  @defgroup LLVMCCoreValueWithAlignment Values with alignment
##
##  Functions in this group only apply to values with alignment, i.e.
##  global variables, load and store instructions.
##
## *
##  Obtain the preferred alignment of the value.
##  @see llvm::AllocaInst::getAlignment()
##  @see llvm::LoadInst::getAlignment()
##  @see llvm::StoreInst::getAlignment()
##  @see llvm::AtomicRMWInst::setAlignment()
##  @see llvm::AtomicCmpXchgInst::setAlignment()
##  @see llvm::GlobalValue::getAlignment()
##

proc getAlignment*(v: ValueRef): cuint {.importc: "LLVMGetAlignment".}
## *
##  Set the preferred alignment of the value.
##  @see llvm::AllocaInst::setAlignment()
##  @see llvm::LoadInst::setAlignment()
##  @see llvm::StoreInst::setAlignment()
##  @see llvm::AtomicRMWInst::setAlignment()
##  @see llvm::AtomicCmpXchgInst::setAlignment()
##  @see llvm::GlobalValue::setAlignment()
##

proc setAlignment*(v: ValueRef; bytes: cuint) {.importc: "LLVMSetAlignment",
    .}
## *
##  Sets a metadata attachment, erasing the existing metadata attachment if
##  it already exists for the given kind.
##
##  @see llvm::GlobalObject::setMetadata()
##

proc globalSetMetadata*(global: ValueRef; kind: cuint; md: MetadataRef) {.
    importc: "LLVMGlobalSetMetadata".}
## *
##  Erases a metadata attachment of the given kind if it exists.
##
##  @see llvm::GlobalObject::eraseMetadata()
##

proc globalEraseMetadata*(global: ValueRef; kind: cuint) {.
    importc: "LLVMGlobalEraseMetadata".}
## *
##  Removes all metadata attachments from this value.
##
##  @see llvm::GlobalObject::clearMetadata()
##

proc globalClearMetadata*(global: ValueRef) {.importc: "LLVMGlobalClearMetadata",
    .}
## *
##  Retrieves an array of metadata entries representing the metadata attached to
##  this value. The caller is responsible for freeing this array by calling
##  \c LLVMDisposeValueMetadataEntries.
##
##  @see llvm::GlobalObject::getAllMetadata()
##

proc globalCopyAllMetadata*(value: ValueRef; numEntries: ptr csize_t): ptr ValueMetadataEntry {.
    importc: "LLVMGlobalCopyAllMetadata".}
## *
##  Destroys value metadata entries.
##

proc disposeValueMetadataEntries*(entries: ptr ValueMetadataEntry) {.
    importc: "LLVMDisposeValueMetadataEntries".}
## *
##  Returns the kind of a value metadata entry at a specific index.
##

proc valueMetadataEntriesGetKind*(entries: ptr ValueMetadataEntry; index: cuint): cuint {.
    importc: "LLVMValueMetadataEntriesGetKind".}
## *
##  Returns the underlying metadata node of a value metadata entry at a
##  specific index.
##

proc valueMetadataEntriesGetMetadata*(entries: ptr ValueMetadataEntry; index: cuint): MetadataRef {.
    importc: "LLVMValueMetadataEntriesGetMetadata".}
## *
##  @}
##
## *
##  @defgroup LLVMCoreValueConstantGlobalVariable Global Variables
##
##  This group contains functions that operate on global variable values.
##
##  @see llvm::GlobalVariable
##
##  @{
##

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
## *
##  @}
##
## *
##  @defgroup LLVMCoreValueConstantGlobalAlias Global Aliases
##
##  This group contains function that operate on global alias values.
##
##  @see llvm::GlobalAlias
##
##  @{
##

proc addAlias*(m: ModuleRef; ty: TypeRef; aliasee: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMAddAlias".}
## *
##  Add a GlobalAlias with the given value type, address space and aliasee.
##
##  @see llvm::GlobalAlias::create()
##

proc addAlias2*(m: ModuleRef; valueTy: TypeRef; addrSpace: cuint; aliasee: ValueRef;
               name: cstring): ValueRef {.importc: "LLVMAddAlias2".}
## *
##  Obtain a GlobalAlias value from a Module by its name.
##
##  The returned value corresponds to a llvm::GlobalAlias value.
##
##  @see llvm::Module::getNamedAlias()
##

proc getNamedGlobalAlias*(m: ModuleRef; name: cstring; nameLen: csize_t): ValueRef {.
    importc: "LLVMGetNamedGlobalAlias".}
## *
##  Obtain an iterator to the first GlobalAlias in a Module.
##
##  @see llvm::Module::alias_begin()
##

proc getFirstGlobalAlias*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetFirstGlobalAlias".}
## *
##  Obtain an iterator to the last GlobalAlias in a Module.
##
##  @see llvm::Module::alias_end()
##

proc getLastGlobalAlias*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetLastGlobalAlias".}
## *
##  Advance a GlobalAlias iterator to the next GlobalAlias.
##
##  Returns NULL if the iterator was already at the end and there are no more
##  global aliases.
##

proc getNextGlobalAlias*(ga: ValueRef): ValueRef {.
    importc: "LLVMGetNextGlobalAlias".}
## *
##  Decrement a GlobalAlias iterator to the previous GlobalAlias.
##
##  Returns NULL if the iterator was already at the beginning and there are
##  no previous global aliases.
##

proc getPreviousGlobalAlias*(ga: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousGlobalAlias".}
## *
##  Retrieve the target value of an alias.
##

proc aliasGetAliasee*(alias: ValueRef): ValueRef {.importc: "LLVMAliasGetAliasee",
    .}
## *
##  Set the target value of an alias.
##

proc aliasSetAliasee*(alias: ValueRef; aliasee: ValueRef) {.
    importc: "LLVMAliasSetAliasee".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueFunction Function values
##
##  Functions in this group operate on LLVMValueRef instances that
##  correspond to llvm::Function instances.
##
##  @see llvm::Function
##
##  @{
##
## *
##  Remove a function from its containing module and deletes it.
##
##  @see llvm::Function::eraseFromParent()
##

proc deleteFunction*(fn: ValueRef) {.importc: "LLVMDeleteFunction".}
## *
##  Check whether the given function has a personality function.
##
##  @see llvm::Function::hasPersonalityFn()
##

proc hasPersonalityFn*(fn: ValueRef): Bool {.importc: "LLVMHasPersonalityFn",
    .}
## *
##  Obtain the personality function attached to the function.
##
##  @see llvm::Function::getPersonalityFn()
##

proc getPersonalityFn*(fn: ValueRef): ValueRef {.importc: "LLVMGetPersonalityFn",
    .}
## *
##  Set the personality function attached to the function.
##
##  @see llvm::Function::setPersonalityFn()
##

proc setPersonalityFn*(fn: ValueRef; personalityFn: ValueRef) {.
    importc: "LLVMSetPersonalityFn".}
## *
##  Obtain the intrinsic ID number which matches the given function name.
##
##  @see llvm::Function::lookupIntrinsicID()
##

proc lookupIntrinsicID*(name: cstring; nameLen: csize_t): cuint {.
    importc: "LLVMLookupIntrinsicID".}
## *
##  Obtain the ID number from a function instance.
##
##  @see llvm::Function::getIntrinsicID()
##

proc getIntrinsicID*(fn: ValueRef): cuint {.importc: "LLVMGetIntrinsicID",
                                        .}
## *
##  Create or insert the declaration of an intrinsic.  For overloaded intrinsics,
##  parameter types must be provided to uniquely identify an overload.
##
##  @see llvm::Intrinsic::getDeclaration()
##

proc getIntrinsicDeclaration*(`mod`: ModuleRef; id: cuint; paramTypes: ptr TypeRef;
                             paramCount: csize_t): ValueRef {.
    importc: "LLVMGetIntrinsicDeclaration".}
## *
##  Retrieves the type of an intrinsic.  For overloaded intrinsics, parameter
##  types must be provided to uniquely identify an overload.
##
##  @see llvm::Intrinsic::getType()
##

proc intrinsicGetType*(ctx: ContextRef; id: cuint; paramTypes: ptr TypeRef;
                      paramCount: csize_t): TypeRef {.
    importc: "LLVMIntrinsicGetType".}
## *
##  Retrieves the name of an intrinsic.
##
##  @see llvm::Intrinsic::getName()
##

proc intrinsicGetName*(id: cuint; nameLength: ptr csize_t): cstring {.
    importc: "LLVMIntrinsicGetName".}
## * Deprecated: Use LLVMIntrinsicCopyOverloadedName2 instead.

proc intrinsicCopyOverloadedName*(id: cuint; paramTypes: ptr TypeRef;
                                 paramCount: csize_t; nameLength: ptr csize_t): cstring {.
    importc: "LLVMIntrinsicCopyOverloadedName".}
## *
##  Copies the name of an overloaded intrinsic identified by a given list of
##  parameter types.
##
##  Unlike LLVMIntrinsicGetName, the caller is responsible for freeing the
##  returned string.
##
##  This version also supports unnamed types.
##
##  @see llvm::Intrinsic::getName()
##

proc intrinsicCopyOverloadedName2*(`mod`: ModuleRef; id: cuint;
                                  paramTypes: ptr TypeRef; paramCount: csize_t;
                                  nameLength: ptr csize_t): cstring {.
    importc: "LLVMIntrinsicCopyOverloadedName2".}
## *
##  Obtain if the intrinsic identified by the given ID is overloaded.
##
##  @see llvm::Intrinsic::isOverloaded()
##

proc intrinsicIsOverloaded*(id: cuint): Bool {.importc: "LLVMIntrinsicIsOverloaded",
    .}
## *
##  Obtain the calling function of a function.
##
##  The returned value corresponds to the LLVMCallConv enumeration.
##
##  @see llvm::Function::getCallingConv()
##

proc getFunctionCallConv*(fn: ValueRef): cuint {.importc: "LLVMGetFunctionCallConv",
    .}
## *
##  Set the calling convention of a function.
##
##  @see llvm::Function::setCallingConv()
##
##  @param Fn Function to operate on
##  @param CC LLVMCallConv to set calling convention to
##

proc setFunctionCallConv*(fn: ValueRef; cc: cuint) {.
    importc: "LLVMSetFunctionCallConv".}
## *
##  Obtain the name of the garbage collector to use during code
##  generation.
##
##  @see llvm::Function::getGC()
##

proc getGC*(fn: ValueRef): cstring {.importc: "LLVMGetGC".}
## *
##  Define the garbage collector to use during code generation.
##
##  @see llvm::Function::setGC()
##

proc setGC*(fn: ValueRef; name: cstring) {.importc: "LLVMSetGC".}
## *
##  Add an attribute to a function.
##
##  @see llvm::Function::addAttribute()
##

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
## *
##  Add a target-dependent attribute to a function
##  @see llvm::AttrBuilder::addAttribute()
##

proc addTargetDependentFunctionAttr*(fn: ValueRef; a: cstring; v: cstring) {.
    importc: "LLVMAddTargetDependentFunctionAttr".}
## *
##  @defgroup LLVMCCoreValueFunctionParameters Function Parameters
##
##  Functions in this group relate to arguments/parameters on functions.
##
##  Functions in this group expect LLVMValueRef instances that correspond
##  to llvm::Function instances.
##
##  @{
##
## *
##  Obtain the number of parameters in a function.
##
##  @see llvm::Function::arg_size()
##

proc countParams*(fn: ValueRef): cuint {.importc: "LLVMCountParams".}
## *
##  Obtain the parameters in a function.
##
##  The takes a pointer to a pre-allocated array of LLVMValueRef that is
##  at least LLVMCountParams() long. This array will be filled with
##  LLVMValueRef instances which correspond to the parameters the
##  function receives. Each LLVMValueRef corresponds to a llvm::Argument
##  instance.
##
##  @see llvm::Function::arg_begin()
##

proc getParams*(fn: ValueRef; params: ptr ValueRef) {.importc: "LLVMGetParams",
    .}
## *
##  Obtain the parameter at the specified index.
##
##  Parameters are indexed from 0.
##
##  @see llvm::Function::arg_begin()
##

proc getParam*(fn: ValueRef; index: cuint): ValueRef {.importc: "LLVMGetParam",
    .}
## *
##  Obtain the function to which this argument belongs.
##
##  Unlike other functions in this group, this one takes an LLVMValueRef
##  that corresponds to a llvm::Attribute.
##
##  The returned LLVMValueRef is the llvm::Function to which this
##  argument belongs.
##

proc getParamParent*(inst: ValueRef): ValueRef {.importc: "LLVMGetParamParent",
    .}
## *
##  Obtain the first parameter to a function.
##
##  @see llvm::Function::arg_begin()
##

proc getFirstParam*(fn: ValueRef): ValueRef {.importc: "LLVMGetFirstParam",
    .}
## *
##  Obtain the last parameter to a function.
##
##  @see llvm::Function::arg_end()
##

proc getLastParam*(fn: ValueRef): ValueRef {.importc: "LLVMGetLastParam",
    .}
## *
##  Obtain the next parameter to a function.
##
##  This takes an LLVMValueRef obtained from LLVMGetFirstParam() (which is
##  actually a wrapped iterator) and obtains the next parameter from the
##  underlying iterator.
##

proc getNextParam*(arg: ValueRef): ValueRef {.importc: "LLVMGetNextParam",
    .}
## *
##  Obtain the previous parameter to a function.
##
##  This is the opposite of LLVMGetNextParam().
##

proc getPreviousParam*(arg: ValueRef): ValueRef {.importc: "LLVMGetPreviousParam",
    .}
## *
##  Set the alignment for a function parameter.
##
##  @see llvm::Argument::addAttr()
##  @see llvm::AttrBuilder::addAlignmentAttr()
##

proc setParamAlignment*(arg: ValueRef; align: cuint) {.
    importc: "LLVMSetParamAlignment".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueGlobalIFunc IFuncs
##
##  Functions in this group relate to indirect functions.
##
##  Functions in this group expect LLVMValueRef instances that correspond
##  to llvm::GlobalIFunc instances.
##
##  @{
##
## *
##  Add a global indirect function to a module under a specified name.
##
##  @see llvm::GlobalIFunc::create()
##

proc addGlobalIFunc*(m: ModuleRef; name: cstring; nameLen: csize_t; ty: TypeRef;
                    addrSpace: cuint; resolver: ValueRef): ValueRef {.
    importc: "LLVMAddGlobalIFunc".}
## *
##  Obtain a GlobalIFunc value from a Module by its name.
##
##  The returned value corresponds to a llvm::GlobalIFunc value.
##
##  @see llvm::Module::getNamedIFunc()
##

proc getNamedGlobalIFunc*(m: ModuleRef; name: cstring; nameLen: csize_t): ValueRef {.
    importc: "LLVMGetNamedGlobalIFunc".}
## *
##  Obtain an iterator to the first GlobalIFunc in a Module.
##
##  @see llvm::Module::ifunc_begin()
##

proc getFirstGlobalIFunc*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetFirstGlobalIFunc".}
## *
##  Obtain an iterator to the last GlobalIFunc in a Module.
##
##  @see llvm::Module::ifunc_end()
##

proc getLastGlobalIFunc*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetLastGlobalIFunc".}
## *
##  Advance a GlobalIFunc iterator to the next GlobalIFunc.
##
##  Returns NULL if the iterator was already at the end and there are no more
##  global aliases.
##

proc getNextGlobalIFunc*(iFunc: ValueRef): ValueRef {.
    importc: "LLVMGetNextGlobalIFunc".}
## *
##  Decrement a GlobalIFunc iterator to the previous GlobalIFunc.
##
##  Returns NULL if the iterator was already at the beginning and there are
##  no previous global aliases.
##

proc getPreviousGlobalIFunc*(iFunc: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousGlobalIFunc".}
## *
##  Retrieves the resolver function associated with this indirect function, or
##  NULL if it doesn't not exist.
##
##  @see llvm::GlobalIFunc::getResolver()
##

proc getGlobalIFuncResolver*(iFunc: ValueRef): ValueRef {.
    importc: "LLVMGetGlobalIFuncResolver".}
## *
##  Sets the resolver function associated with this indirect function.
##
##  @see llvm::GlobalIFunc::setResolver()
##

proc setGlobalIFuncResolver*(iFunc: ValueRef; resolver: ValueRef) {.
    importc: "LLVMSetGlobalIFuncResolver".}
## *
##  Remove a global indirect function from its parent module and delete it.
##
##  @see llvm::GlobalIFunc::eraseFromParent()
##

proc eraseGlobalIFunc*(iFunc: ValueRef) {.importc: "LLVMEraseGlobalIFunc",
                                       .}
## *
##  Remove a global indirect function from its parent module.
##
##  This unlinks the global indirect function from its containing module but
##  keeps it alive.
##
##  @see llvm::GlobalIFunc::removeFromParent()
##

proc removeGlobalIFunc*(iFunc: ValueRef) {.importc: "LLVMRemoveGlobalIFunc",
                                        .}
## *
##  @}
##
## *
##  @}
##
## *
##  @}
##
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueMetadata Metadata
##
##  @{
##
## *
##  Create an MDString value from a given string value.
##
##  The MDString value does not take ownership of the given string, it remains
##  the responsibility of the caller to free it.
##
##  @see llvm::MDString::get()
##

proc mDStringInContext2*(c: ContextRef; str: cstring; sLen: csize_t): MetadataRef {.
    importc: "LLVMMDStringInContext2".}
## *
##  Create an MDNode value with the given array of operands.
##
##  @see llvm::MDNode::get()
##

proc mDNodeInContext2*(c: ContextRef; mDs: ptr MetadataRef; count: csize_t): MetadataRef {.
    importc: "LLVMMDNodeInContext2".}
## *
##  Obtain a Metadata as a Value.
##

proc metadataAsValue*(c: ContextRef; md: MetadataRef): ValueRef {.
    importc: "LLVMMetadataAsValue".}
## *
##  Obtain a Value as a Metadata.
##

proc valueAsMetadata*(val: ValueRef): MetadataRef {.importc: "LLVMValueAsMetadata",
    .}
## *
##  Obtain the underlying string from a MDString value.
##
##  @param V Instance to obtain string from.
##  @param Length Memory address which will hold length of returned string.
##  @return String data in MDString.
##

proc getMDString*(v: ValueRef; length: ptr cuint): cstring {.
    importc: "LLVMGetMDString".}
## *
##  Obtain the number of operands from an MDNode value.
##
##  @param V MDNode to get number of operands from.
##  @return Number of operands of the MDNode.
##

proc getMDNodeNumOperands*(v: ValueRef): cuint {.
    importc: "LLVMGetMDNodeNumOperands".}
## *
##  Obtain the given MDNode's operands.
##
##  The passed LLVMValueRef pointer should point to enough memory to hold all of
##  the operands of the given MDNode (see LLVMGetMDNodeNumOperands) as
##  LLVMValueRefs. This memory will be populated with the LLVMValueRefs of the
##  MDNode's operands.
##
##  @param V MDNode to get the operands from.
##  @param Dest Destination array for operands.
##

proc getMDNodeOperands*(v: ValueRef; dest: ptr ValueRef) {.
    importc: "LLVMGetMDNodeOperands".}
## * Deprecated: Use LLVMMDStringInContext2 instead.

proc mDStringInContext*(c: ContextRef; str: cstring; sLen: cuint): ValueRef {.
    importc: "LLVMMDStringInContext".}
## * Deprecated: Use LLVMMDStringInContext2 instead.

proc mDString*(str: cstring; sLen: cuint): ValueRef {.importc: "LLVMMDString",
    .}
## * Deprecated: Use LLVMMDNodeInContext2 instead.

proc mDNodeInContext*(c: ContextRef; vals: ptr ValueRef; count: cuint): ValueRef {.
    importc: "LLVMMDNodeInContext".}
## * Deprecated: Use LLVMMDNodeInContext2 instead.

proc mDNode*(vals: ptr ValueRef; count: cuint): ValueRef {.importc: "LLVMMDNode",
    .}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueBasicBlock Basic Block
##
##  A basic block represents a single entry single exit section of code.
##  Basic blocks contain a list of instructions which form the body of
##  the block.
##
##  Basic blocks belong to functions. They have the type of label.
##
##  Basic blocks are themselves values. However, the C API models them as
##  LLVMBasicBlockRef.
##
##  @see llvm::BasicBlock
##
##  @{
##
## *
##  Convert a basic block instance to a value type.
##

proc basicBlockAsValue*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMBasicBlockAsValue".}
## *
##  Determine whether an LLVMValueRef is itself a basic block.
##

proc valueIsBasicBlock*(val: ValueRef): Bool {.importc: "LLVMValueIsBasicBlock",
    .}
## *
##  Convert an LLVMValueRef to an LLVMBasicBlockRef instance.
##

proc valueAsBasicBlock*(val: ValueRef): BasicBlockRef {.
    importc: "LLVMValueAsBasicBlock".}
## *
##  Obtain the string name of a basic block.
##

proc getBasicBlockName*(bb: BasicBlockRef): cstring {.
    importc: "LLVMGetBasicBlockName".}
## *
##  Obtain the function to which a basic block belongs.
##
##  @see llvm::BasicBlock::getParent()
##

proc getBasicBlockParent*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetBasicBlockParent".}
## *
##  Obtain the terminator instruction for a basic block.
##
##  If the basic block does not have a terminator (it is not well-formed
##  if it doesn't), then NULL is returned.
##
##  The returned LLVMValueRef corresponds to an llvm::Instruction.
##
##  @see llvm::BasicBlock::getTerminator()
##

proc getBasicBlockTerminator*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetBasicBlockTerminator".}
## *
##  Obtain the number of basic blocks in a function.
##
##  @param Fn Function value to operate on.
##

proc countBasicBlocks*(fn: ValueRef): cuint {.importc: "LLVMCountBasicBlocks",
    .}
## *
##  Obtain all of the basic blocks in a function.
##
##  This operates on a function value. The BasicBlocks parameter is a
##  pointer to a pre-allocated array of LLVMBasicBlockRef of at least
##  LLVMCountBasicBlocks() in length. This array is populated with
##  LLVMBasicBlockRef instances.
##

proc getBasicBlocks*(fn: ValueRef; basicBlocks: ptr BasicBlockRef) {.
    importc: "LLVMGetBasicBlocks".}
## *
##  Obtain the first basic block in a function.
##
##  The returned basic block can be used as an iterator. You will likely
##  eventually call into LLVMGetNextBasicBlock() with it.
##
##  @see llvm::Function::begin()
##

proc getFirstBasicBlock*(fn: ValueRef): BasicBlockRef {.
    importc: "LLVMGetFirstBasicBlock".}
## *
##  Obtain the last basic block in a function.
##
##  @see llvm::Function::end()
##

proc getLastBasicBlock*(fn: ValueRef): BasicBlockRef {.
    importc: "LLVMGetLastBasicBlock".}
## *
##  Advance a basic block iterator.
##

proc getNextBasicBlock*(bb: BasicBlockRef): BasicBlockRef {.
    importc: "LLVMGetNextBasicBlock".}
## *
##  Go backwards in a basic block iterator.
##

proc getPreviousBasicBlock*(bb: BasicBlockRef): BasicBlockRef {.
    importc: "LLVMGetPreviousBasicBlock".}
## *
##  Obtain the basic block that corresponds to the entry point of a
##  function.
##
##  @see llvm::Function::getEntryBlock()
##

proc getEntryBasicBlock*(fn: ValueRef): BasicBlockRef {.
    importc: "LLVMGetEntryBasicBlock".}
## *
##  Insert the given basic block after the insertion point of the given builder.
##
##  The insertion point must be valid.
##
##  @see llvm::Function::BasicBlockListType::insertAfter()
##

proc insertExistingBasicBlockAfterInsertBlock*(builder: BuilderRef;
    bb: BasicBlockRef) {.importc: "LLVMInsertExistingBasicBlockAfterInsertBlock",
                       .}
## *
##  Append the given basic block to the basic block list of the given function.
##
##  @see llvm::Function::BasicBlockListType::push_back()
##

proc appendExistingBasicBlock*(fn: ValueRef; bb: BasicBlockRef) {.
    importc: "LLVMAppendExistingBasicBlock".}
## *
##  Create a new basic block without inserting it into a function.
##
##  @see llvm::BasicBlock::Create()
##

proc createBasicBlockInContext*(c: ContextRef; name: cstring): BasicBlockRef {.
    importc: "LLVMCreateBasicBlockInContext".}
## *
##  Append a basic block to the end of a function.
##
##  @see llvm::BasicBlock::Create()
##

proc appendBasicBlockInContext*(c: ContextRef; fn: ValueRef; name: cstring): BasicBlockRef {.
    importc: "LLVMAppendBasicBlockInContext".}
## *
##  Append a basic block to the end of a function using the global
##  context.
##
##  @see llvm::BasicBlock::Create()
##

proc appendBasicBlock*(fn: ValueRef; name: cstring): BasicBlockRef {.
    importc: "LLVMAppendBasicBlock".}
## *
##  Insert a basic block in a function before another basic block.
##
##  The function to add to is determined by the function of the
##  passed basic block.
##
##  @see llvm::BasicBlock::Create()
##

proc insertBasicBlockInContext*(c: ContextRef; bb: BasicBlockRef; name: cstring): BasicBlockRef {.
    importc: "LLVMInsertBasicBlockInContext".}
## *
##  Insert a basic block in a function using the global context.
##
##  @see llvm::BasicBlock::Create()
##

proc insertBasicBlock*(insertBeforeBB: BasicBlockRef; name: cstring): BasicBlockRef {.
    importc: "LLVMInsertBasicBlock".}
## *
##  Remove a basic block from a function and delete it.
##
##  This deletes the basic block from its containing function and deletes
##  the basic block itself.
##
##  @see llvm::BasicBlock::eraseFromParent()
##

proc deleteBasicBlock*(bb: BasicBlockRef) {.importc: "LLVMDeleteBasicBlock",
    .}
## *
##  Remove a basic block from a function.
##
##  This deletes the basic block from its containing function but keep
##  the basic block alive.
##
##  @see llvm::BasicBlock::removeFromParent()
##

proc removeBasicBlockFromParent*(bb: BasicBlockRef) {.
    importc: "LLVMRemoveBasicBlockFromParent".}
## *
##  Move a basic block to before another one.
##
##  @see llvm::BasicBlock::moveBefore()
##

proc moveBasicBlockBefore*(bb: BasicBlockRef; movePos: BasicBlockRef) {.
    importc: "LLVMMoveBasicBlockBefore".}
## *
##  Move a basic block to after another one.
##
##  @see llvm::BasicBlock::moveAfter()
##

proc moveBasicBlockAfter*(bb: BasicBlockRef; movePos: BasicBlockRef) {.
    importc: "LLVMMoveBasicBlockAfter".}
## *
##  Obtain the first instruction in a basic block.
##
##  The returned LLVMValueRef corresponds to a llvm::Instruction
##  instance.
##

proc getFirstInstruction*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetFirstInstruction".}
## *
##  Obtain the last instruction in a basic block.
##
##  The returned LLVMValueRef corresponds to an LLVM:Instruction.
##

proc getLastInstruction*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetLastInstruction".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstruction Instructions
##
##  Functions in this group relate to the inspection and manipulation of
##  individual instructions.
##
##  In the C++ API, an instruction is modeled by llvm::Instruction. This
##  class has a large number of descendents. llvm::Instruction is a
##  llvm::Value and in the C API, instructions are modeled by
##  LLVMValueRef.
##
##  This group also contains sub-groups which operate on specific
##  llvm::Instruction types, e.g. llvm::CallInst.
##
##  @{
##
## *
##  Determine whether an instruction has any metadata attached.
##

proc hasMetadata*(val: ValueRef): cint {.importc: "LLVMHasMetadata".}
## *
##  Return metadata associated with an instruction value.
##

proc getMetadata*(val: ValueRef; kindID: cuint): ValueRef {.
    importc: "LLVMGetMetadata".}
## *
##  Set metadata associated with an instruction value.
##

proc setMetadata*(val: ValueRef; kindID: cuint; node: ValueRef) {.
    importc: "LLVMSetMetadata".}
## *
##  Returns the metadata associated with an instruction value, but filters out
##  all the debug locations.
##
##  @see llvm::Instruction::getAllMetadataOtherThanDebugLoc()
##

proc instructionGetAllMetadataOtherThanDebugLoc*(instr: ValueRef;
    numEntries: ptr csize_t): ptr ValueMetadataEntry {.
    importc: "LLVMInstructionGetAllMetadataOtherThanDebugLoc".}
## *
##  Obtain the basic block to which an instruction belongs.
##
##  @see llvm::Instruction::getParent()
##

proc getInstructionParent*(inst: ValueRef): BasicBlockRef {.
    importc: "LLVMGetInstructionParent".}
## *
##  Obtain the instruction that occurs after the one specified.
##
##  The next instruction will be from the same basic block.
##
##  If this is the last instruction in a basic block, NULL will be
##  returned.
##

proc getNextInstruction*(inst: ValueRef): ValueRef {.
    importc: "LLVMGetNextInstruction".}
## *
##  Obtain the instruction that occurred before this one.
##
##  If the instruction is the first instruction in a basic block, NULL
##  will be returned.
##

proc getPreviousInstruction*(inst: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousInstruction".}
## *
##  Remove an instruction.
##
##  The instruction specified is removed from its containing building
##  block but is kept alive.
##
##  @see llvm::Instruction::removeFromParent()
##

proc instructionRemoveFromParent*(inst: ValueRef) {.
    importc: "LLVMInstructionRemoveFromParent".}
## *
##  Remove and delete an instruction.
##
##  The instruction specified is removed from its containing building
##  block and then deleted.
##
##  @see llvm::Instruction::eraseFromParent()
##

proc instructionEraseFromParent*(inst: ValueRef) {.
    importc: "LLVMInstructionEraseFromParent".}
## *
##  Delete an instruction.
##
##  The instruction specified is deleted. It must have previously been
##  removed from its containing building block.
##
##  @see llvm::Value::deleteValue()
##

proc deleteInstruction*(inst: ValueRef) {.importc: "LLVMDeleteInstruction",
                                       .}
## *
##  Obtain the code opcode for an individual instruction.
##
##  @see llvm::Instruction::getOpCode()
##

proc getInstructionOpcode*(inst: ValueRef): Opcode {.
    importc: "LLVMGetInstructionOpcode".}
## *
##  Obtain the predicate of an instruction.
##
##  This is only valid for instructions that correspond to llvm::ICmpInst
##  or llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp.
##
##  @see llvm::ICmpInst::getPredicate()
##

proc getICmpPredicate*(inst: ValueRef): IntPredicate {.
    importc: "LLVMGetICmpPredicate".}
## *
##  Obtain the float predicate of an instruction.
##
##  This is only valid for instructions that correspond to llvm::FCmpInst
##  or llvm::ConstantExpr whose opcode is llvm::Instruction::FCmp.
##
##  @see llvm::FCmpInst::getPredicate()
##

proc getFCmpPredicate*(inst: ValueRef): RealPredicate {.
    importc: "LLVMGetFCmpPredicate".}
## *
##  Create a copy of 'this' instruction that is identical in all ways
##  except the following:
##    * The instruction has no parent
##    * The instruction has no name
##
##  @see llvm::Instruction::clone()
##

proc instructionClone*(inst: ValueRef): ValueRef {.importc: "LLVMInstructionClone",
    .}
## *
##  Determine whether an instruction is a terminator. This routine is named to
##  be compatible with historical functions that did this by querying the
##  underlying C++ type.
##
##  @see llvm::Instruction::isTerminator()
##

proc isATerminatorInst*(inst: ValueRef): ValueRef {.
    importc: "LLVMIsATerminatorInst".}
## *
##  @defgroup LLVMCCoreValueInstructionCall Call Sites and Invocations
##
##  Functions in this group apply to instructions that refer to call
##  sites and invocations. These correspond to C++ types in the
##  llvm::CallInst class tree.
##
##  @{
##
## *
##  Obtain the argument count for a call instruction.
##
##  This expects an LLVMValueRef that corresponds to a llvm::CallInst,
##  llvm::InvokeInst, or llvm:FuncletPadInst.
##
##  @see llvm::CallInst::getNumArgOperands()
##  @see llvm::InvokeInst::getNumArgOperands()
##  @see llvm::FuncletPadInst::getNumArgOperands()
##

proc getNumArgOperands*(instr: ValueRef): cuint {.importc: "LLVMGetNumArgOperands",
    .}
## *
##  Set the calling convention for a call instruction.
##
##  This expects an LLVMValueRef that corresponds to a llvm::CallInst or
##  llvm::InvokeInst.
##
##  @see llvm::CallInst::setCallingConv()
##  @see llvm::InvokeInst::setCallingConv()
##

proc setInstructionCallConv*(instr: ValueRef; cc: cuint) {.
    importc: "LLVMSetInstructionCallConv".}
## *
##  Obtain the calling convention for a call instruction.
##
##  This is the opposite of LLVMSetInstructionCallConv(). Reads its
##  usage.
##
##  @see LLVMSetInstructionCallConv()
##

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
## *
##  Obtain the function type called by this instruction.
##
##  @see llvm::CallBase::getFunctionType()
##

proc getCalledFunctionType*(c: ValueRef): TypeRef {.
    importc: "LLVMGetCalledFunctionType".}
## *
##  Obtain the pointer to the function invoked by this instruction.
##
##  This expects an LLVMValueRef that corresponds to a llvm::CallInst or
##  llvm::InvokeInst.
##
##  @see llvm::CallInst::getCalledOperand()
##  @see llvm::InvokeInst::getCalledOperand()
##

proc getCalledValue*(instr: ValueRef): ValueRef {.importc: "LLVMGetCalledValue",
    .}
## *
##  Obtain whether a call instruction is a tail call.
##
##  This only works on llvm::CallInst instructions.
##
##  @see llvm::CallInst::isTailCall()
##

proc isTailCall*(callInst: ValueRef): Bool {.importc: "LLVMIsTailCall",
    .}
## *
##  Set whether a call instruction is a tail call.
##
##  This only works on llvm::CallInst instructions.
##
##  @see llvm::CallInst::setTailCall()
##

proc setTailCall*(callInst: ValueRef; isTailCall: Bool) {.importc: "LLVMSetTailCall",
    .}
## *
##  Return the normal destination basic block.
##
##  This only works on llvm::InvokeInst instructions.
##
##  @see llvm::InvokeInst::getNormalDest()
##

proc getNormalDest*(invokeInst: ValueRef): BasicBlockRef {.
    importc: "LLVMGetNormalDest".}
## *
##  Return the unwind destination basic block.
##
##  Works on llvm::InvokeInst, llvm::CleanupReturnInst, and
##  llvm::CatchSwitchInst instructions.
##
##  @see llvm::InvokeInst::getUnwindDest()
##  @see llvm::CleanupReturnInst::getUnwindDest()
##  @see llvm::CatchSwitchInst::getUnwindDest()
##

proc getUnwindDest*(invokeInst: ValueRef): BasicBlockRef {.
    importc: "LLVMGetUnwindDest".}
## *
##  Set the normal destination basic block.
##
##  This only works on llvm::InvokeInst instructions.
##
##  @see llvm::InvokeInst::setNormalDest()
##

proc setNormalDest*(invokeInst: ValueRef; b: BasicBlockRef) {.
    importc: "LLVMSetNormalDest".}
## *
##  Set the unwind destination basic block.
##
##  Works on llvm::InvokeInst, llvm::CleanupReturnInst, and
##  llvm::CatchSwitchInst instructions.
##
##  @see llvm::InvokeInst::setUnwindDest()
##  @see llvm::CleanupReturnInst::setUnwindDest()
##  @see llvm::CatchSwitchInst::setUnwindDest()
##

proc setUnwindDest*(invokeInst: ValueRef; b: BasicBlockRef) {.
    importc: "LLVMSetUnwindDest".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstructionTerminator Terminators
##
##  Functions in this group only apply to instructions for which
##  LLVMIsATerminatorInst returns true.
##
##  @{
##
## *
##  Return the number of successors that this terminator has.
##
##  @see llvm::Instruction::getNumSuccessors
##

proc getNumSuccessors*(term: ValueRef): cuint {.importc: "LLVMGetNumSuccessors",
    .}
## *
##  Return the specified successor.
##
##  @see llvm::Instruction::getSuccessor
##

proc getSuccessor*(term: ValueRef; i: cuint): BasicBlockRef {.
    importc: "LLVMGetSuccessor".}
## *
##  Update the specified successor to point at the provided block.
##
##  @see llvm::Instruction::setSuccessor
##

proc setSuccessor*(term: ValueRef; i: cuint; `block`: BasicBlockRef) {.
    importc: "LLVMSetSuccessor".}
## *
##  Return if a branch is conditional.
##
##  This only works on llvm::BranchInst instructions.
##
##  @see llvm::BranchInst::isConditional
##

proc isConditional*(branch: ValueRef): Bool {.importc: "LLVMIsConditional",
    .}
## *
##  Return the condition of a branch instruction.
##
##  This only works on llvm::BranchInst instructions.
##
##  @see llvm::BranchInst::getCondition
##

proc getCondition*(branch: ValueRef): ValueRef {.importc: "LLVMGetCondition",
    .}
## *
##  Set the condition of a branch instruction.
##
##  This only works on llvm::BranchInst instructions.
##
##  @see llvm::BranchInst::setCondition
##

proc setCondition*(branch: ValueRef; cond: ValueRef) {.importc: "LLVMSetCondition",
    .}
## *
##  Obtain the default destination basic block of a switch instruction.
##
##  This only works on llvm::SwitchInst instructions.
##
##  @see llvm::SwitchInst::getDefaultDest()
##

proc getSwitchDefaultDest*(switchInstr: ValueRef): BasicBlockRef {.
    importc: "LLVMGetSwitchDefaultDest".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstructionAlloca Allocas
##
##  Functions in this group only apply to instructions that map to
##  llvm::AllocaInst instances.
##
##  @{
##
## *
##  Obtain the type that is being allocated by the alloca instruction.
##

proc getAllocatedType*(alloca: ValueRef): TypeRef {.importc: "LLVMGetAllocatedType",
    .}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstructionGetElementPointer GEPs
##
##  Functions in this group only apply to instructions that map to
##  llvm::GetElementPtrInst instances.
##
##  @{
##
## *
##  Check whether the given GEP operator is inbounds.
##

proc isInBounds*(gep: ValueRef): Bool {.importc: "LLVMIsInBounds".}
## *
##  Set the given GEP instruction to be inbounds or not.
##

proc setIsInBounds*(gep: ValueRef; inBounds: Bool) {.importc: "LLVMSetIsInBounds",
    .}
## *
##  Get the source element type of the given GEP operator.
##

proc getGEPSourceElementType*(gep: ValueRef): TypeRef {.
    importc: "LLVMGetGEPSourceElementType".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstructionPHINode PHI Nodes
##
##  Functions in this group only apply to instructions that map to
##  llvm::PHINode instances.
##
##  @{
##
## *
##  Add an incoming value to the end of a PHI list.
##

proc addIncoming*(phiNode: ValueRef; incomingValues: ptr ValueRef;
                 incomingBlocks: ptr BasicBlockRef; count: cuint) {.
    importc: "LLVMAddIncoming".}
## *
##  Obtain the number of incoming basic blocks to a PHI node.
##

proc countIncoming*(phiNode: ValueRef): cuint {.importc: "LLVMCountIncoming",
    .}
## *
##  Obtain an incoming value to a PHI node as an LLVMValueRef.
##

proc getIncomingValue*(phiNode: ValueRef; index: cuint): ValueRef {.
    importc: "LLVMGetIncomingValue".}
## *
##  Obtain an incoming value to a PHI node as an LLVMBasicBlockRef.
##

proc getIncomingBlock*(phiNode: ValueRef; index: cuint): BasicBlockRef {.
    importc: "LLVMGetIncomingBlock".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstructionExtractValue ExtractValue
##  @defgroup LLVMCCoreValueInstructionInsertValue InsertValue
##
##  Functions in this group only apply to instructions that map to
##  llvm::ExtractValue and llvm::InsertValue instances.
##
##  @{
##
## *
##  Obtain the number of indices.
##  NB: This also works on GEP operators.
##

proc getNumIndices*(inst: ValueRef): cuint {.importc: "LLVMGetNumIndices",
    .}
## *
##  Obtain the indices as an array.
##

proc getIndices*(inst: ValueRef): ptr cuint {.importc: "LLVMGetIndices",
    .}
## *
##  @}
##
## *
##  @}
##
## *
##  @}
##
## *
##  @defgroup LLVMCCoreInstructionBuilder Instruction Builders
##
##  An instruction builder represents a point within a basic block and is
##  the exclusive means of building instructions using the C interface.
##
##  @{
##

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
proc disposeBuilder*(builder: BuilderRef) {.importc: "LLVMDisposeBuilder",
    .}
##  Metadata
## *
##  Get location information used by debugging information.
##
##  @see llvm::IRBuilder::getCurrentDebugLocation()
##

proc getCurrentDebugLocation2*(builder: BuilderRef): MetadataRef {.
    importc: "LLVMGetCurrentDebugLocation2".}
## *
##  Set location information used by debugging information.
##
##  To clear the location metadata of the given instruction, pass NULL to \p Loc.
##
##  @see llvm::IRBuilder::SetCurrentDebugLocation()
##

proc setCurrentDebugLocation2*(builder: BuilderRef; loc: MetadataRef) {.
    importc: "LLVMSetCurrentDebugLocation2".}
## *
##  Attempts to set the debug location for the given instruction using the
##  current debug location for the given builder.  If the builder has no current
##  debug location, this function is a no-op.
##
##  @deprecated LLVMSetInstDebugLocation is deprecated in favor of the more general
##              LLVMAddMetadataToInst.
##
##  @see llvm::IRBuilder::SetInstDebugLocation()
##

proc setInstDebugLocation*(builder: BuilderRef; inst: ValueRef) {.
    importc: "LLVMSetInstDebugLocation".}
## *
##  Adds the metadata registered with the given builder to the given instruction.
##
##  @see llvm::IRBuilder::AddMetadataToInst()
##

proc addMetadataToInst*(builder: BuilderRef; inst: ValueRef) {.
    importc: "LLVMAddMetadataToInst".}
## *
##  Get the dafult floating-point math metadata for a given builder.
##
##  @see llvm::IRBuilder::getDefaultFPMathTag()
##

proc builderGetDefaultFPMathTag*(builder: BuilderRef): MetadataRef {.
    importc: "LLVMBuilderGetDefaultFPMathTag".}
## *
##  Set the default floating-point math metadata for the given builder.
##
##  To clear the metadata, pass NULL to \p FPMathTag.
##
##  @see llvm::IRBuilder::setDefaultFPMathTag()
##

proc builderSetDefaultFPMathTag*(builder: BuilderRef; fPMathTag: MetadataRef) {.
    importc: "LLVMBuilderSetDefaultFPMathTag".}
## *
##  Deprecated: Passing the NULL location will crash.
##  Use LLVMGetCurrentDebugLocation2 instead.
##

proc setCurrentDebugLocation*(builder: BuilderRef; L: ValueRef) {.
    importc: "LLVMSetCurrentDebugLocation".}
## *
##  Deprecated: Returning the NULL location will crash.
##  Use LLVMGetCurrentDebugLocation2 instead.
##

proc getCurrentDebugLocation*(builder: BuilderRef): ValueRef {.
    importc: "LLVMGetCurrentDebugLocation".}
##  Terminators

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
##  Exception Handling

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
##  Add a case to the switch instruction

proc addCase*(switch: ValueRef; onVal: ValueRef; dest: BasicBlockRef) {.
    importc: "LLVMAddCase".}
##  Add a destination to the indirectbr instruction

proc addDestination*(indirectBr: ValueRef; dest: BasicBlockRef) {.
    importc: "LLVMAddDestination".}
##  Get the number of clauses on the landingpad instruction

proc getNumClauses*(landingPad: ValueRef): cuint {.importc: "LLVMGetNumClauses",
    .}
##  Get the value of the clause at index Idx on the landingpad instruction

proc getClause*(landingPad: ValueRef; idx: cuint): ValueRef {.
    importc: "LLVMGetClause".}
##  Add a catch or filter clause to the landingpad instruction

proc addClause*(landingPad: ValueRef; clauseVal: ValueRef) {.
    importc: "LLVMAddClause".}
##  Get the 'cleanup' flag in the landingpad instruction

proc isCleanup*(landingPad: ValueRef): Bool {.importc: "LLVMIsCleanup",
    .}
##  Set the 'cleanup' flag in the landingpad instruction

proc setCleanup*(landingPad: ValueRef; val: Bool) {.importc: "LLVMSetCleanup",
    .}
##  Add a destination to the catchswitch instruction

proc addHandler*(catchSwitch: ValueRef; dest: BasicBlockRef) {.
    importc: "LLVMAddHandler".}
##  Get the number of handlers on the catchswitch instruction

proc getNumHandlers*(catchSwitch: ValueRef): cuint {.importc: "LLVMGetNumHandlers",
    .}
## *
##  Obtain the basic blocks acting as handlers for a catchswitch instruction.
##
##  The Handlers parameter should point to a pre-allocated array of
##  LLVMBasicBlockRefs at least LLVMGetNumHandlers() large. On return, the
##  first LLVMGetNumHandlers() entries in the array will be populated
##  with LLVMBasicBlockRef instances.
##
##  @param CatchSwitch The catchswitch instruction to operate on.
##  @param Handlers Memory address of an array to be filled with basic blocks.
##

proc getHandlers*(catchSwitch: ValueRef; handlers: ptr BasicBlockRef) {.
    importc: "LLVMGetHandlers".}
##  Funclets
##  Get the number of funcletpad arguments.

proc getArgOperand*(funclet: ValueRef; i: cuint): ValueRef {.
    importc: "LLVMGetArgOperand".}
##  Set a funcletpad argument at the given index.

proc setArgOperand*(funclet: ValueRef; i: cuint; value: ValueRef) {.
    importc: "LLVMSetArgOperand".}
## *
##  Get the parent catchswitch instruction of a catchpad instruction.
##
##  This only works on llvm::CatchPadInst instructions.
##
##  @see llvm::CatchPadInst::getCatchSwitch()
##

proc getParentCatchSwitch*(catchPad: ValueRef): ValueRef {.
    importc: "LLVMGetParentCatchSwitch".}
## *
##  Set the parent catchswitch instruction of a catchpad instruction.
##
##  This only works on llvm::CatchPadInst instructions.
##
##  @see llvm::CatchPadInst::setCatchSwitch()
##

proc setParentCatchSwitch*(catchPad: ValueRef; catchSwitch: ValueRef) {.
    importc: "LLVMSetParentCatchSwitch".}
##  Arithmetic

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
##  Memory

proc buildMalloc*(a1: BuilderRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildMalloc".}
proc buildArrayMalloc*(a1: BuilderRef; ty: TypeRef; val: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildArrayMalloc".}
## *
##  Creates and inserts a memset to the specified pointer and the
##  specified value.
##
##  @see llvm::IRRBuilder::CreateMemSet()
##

proc buildMemSet*(b: BuilderRef; `ptr`: ValueRef; val: ValueRef; len: ValueRef;
                 align: cuint): ValueRef {.importc: "LLVMBuildMemSet",
                                        .}
## *
##  Creates and inserts a memcpy between the specified pointers.
##
##  @see llvm::IRRBuilder::CreateMemCpy()
##

proc buildMemCpy*(b: BuilderRef; dst: ValueRef; dstAlign: cuint; src: ValueRef;
                 srcAlign: cuint; size: ValueRef): ValueRef {.
    importc: "LLVMBuildMemCpy".}
## *
##  Creates and inserts a memmove between the specified pointers.
##
##  @see llvm::IRRBuilder::CreateMemMove()
##

proc buildMemMove*(b: BuilderRef; dst: ValueRef; dstAlign: cuint; src: ValueRef;
                  srcAlign: cuint; size: ValueRef): ValueRef {.
    importc: "LLVMBuildMemMove".}
proc buildAlloca*(a1: BuilderRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildAlloca".}
proc buildArrayAlloca*(a1: BuilderRef; ty: TypeRef; val: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildArrayAlloca".}
proc buildFree*(a1: BuilderRef; pointerVal: ValueRef): ValueRef {.
    importc: "LLVMBuildFree".}

proc buildLoad*(b: BuilderRef, pointerVal: ValueRef; name: cstring): ValueRef {.
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
##  Casts

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
## * Deprecated: This cast is always signed. Use LLVMBuildIntCast2 instead.

proc buildIntCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildIntCast".}
  ## Signed cast!
proc getCastOpcode*(src: ValueRef; srcIsSigned: Bool; destTy: TypeRef;
                   destIsSigned: Bool): Opcode {.importc: "LLVMGetCastOpcode",
    .}
##  Comparisons

proc buildICmp*(a1: BuilderRef; op: IntPredicate; lhs: ValueRef; rhs: ValueRef;
               name: cstring): ValueRef {.importc: "LLVMBuildICmp".}
proc buildFCmp*(a1: BuilderRef; op: RealPredicate; lhs: ValueRef; rhs: ValueRef;
               name: cstring): ValueRef {.importc: "LLVMBuildFCmp".}
##  Miscellaneous instructions

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
## *
##  Get the number of elements in the mask of a ShuffleVector instruction.
##

proc getNumMaskElements*(shuffleVectorInst: ValueRef): cuint {.
    importc: "LLVMGetNumMaskElements".}
## *
##  \returns a constant that specifies that the result of a \c ShuffleVectorInst
##  is undefined.
##

proc getUndefMaskElem*(): cint {.importc: "LLVMGetUndefMaskElem".}
## *
##  Get the mask value at position Elt in the mask of a ShuffleVector
##  instruction.
##
##  \Returns the result of \c LLVMGetUndefMaskElem() if the mask value is undef
##  at that position.
##

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
## *
##  @}
##
## *
##  @defgroup LLVMCCoreModuleProvider Module Providers
##
##  @{
##
## *
##  Changes the type of M so it can be passed to FunctionPassManagers and the
##  JIT.  They take ModuleProviders for historical reasons.
##

proc createModuleProviderForExistingModule*(m: ModuleRef): ModuleProviderRef {.
    importc: "LLVMCreateModuleProviderForExistingModule".}
## *
##  Destroys the module M.
##

proc disposeModuleProvider*(m: ModuleProviderRef) {.
    importc: "LLVMDisposeModuleProvider".}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreMemoryBuffers Memory Buffers
##
##  @{
##

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
## *
##  @}
##
## *
##  @defgroup LLVMCCorePassRegistry Pass Registry
##  @ingroup LLVMCCore
##
##  @{
##
## * Return the global pass registry, for use with initialization functions.
##     @see llvm::PassRegistry::getPassRegistry

proc getGlobalPassRegistry*(): PassRegistryRef {.
    importc: "LLVMGetGlobalPassRegistry".}
## *
##  @}
##
## *
##  @defgroup LLVMCCorePassManagers Pass Managers
##  @ingroup LLVMCCore
##
##  @{
##
## * Constructs a new whole-module pass pipeline. This type of pipeline is
##     suitable for link-time optimization and whole-module transformations.
##     @see llvm::PassManager::PassManager

proc createPassManager*(): PassManagerRef {.importc: "LLVMCreatePassManager",
    .}
## * Constructs a new function-by-function pass pipeline over the module
##     provider. It does not take ownership of the module provider. This type of
##     pipeline is suitable for code generation and JIT compilation tasks.
##     @see llvm::FunctionPassManager::FunctionPassManager

proc createFunctionPassManagerForModule*(m: ModuleRef): PassManagerRef {.
    importc: "LLVMCreateFunctionPassManagerForModule".}
## * Deprecated: Use LLVMCreateFunctionPassManagerForModule instead.

proc createFunctionPassManager*(mp: ModuleProviderRef): PassManagerRef {.
    importc: "LLVMCreateFunctionPassManager".}
## * Initializes, executes on the provided module, and finalizes all of the
##     passes scheduled in the pass manager. Returns 1 if any of the passes
##     modified the module, 0 otherwise.
##     @see llvm::PassManager::run(Module&)

proc runPassManager*(pm: PassManagerRef; m: ModuleRef): Bool {.
    importc: "LLVMRunPassManager".}
## * Initializes all of the function passes scheduled in the function pass
##     manager. Returns 1 if any of the passes modified the module, 0 otherwise.
##     @see llvm::FunctionPassManager::doInitialization

proc initializeFunctionPassManager*(fpm: PassManagerRef): Bool {.
    importc: "LLVMInitializeFunctionPassManager".}
## * Executes all of the function passes scheduled in the function pass manager
##     on the provided function. Returns 1 if any of the passes modified the
##     function, false otherwise.
##     @see llvm::FunctionPassManager::run(Function&)

proc runFunctionPassManager*(fpm: PassManagerRef; f: ValueRef): Bool {.
    importc: "LLVMRunFunctionPassManager".}
## * Finalizes all of the function passes scheduled in the function pass
##     manager. Returns 1 if any of the passes modified the module, 0 otherwise.
##     @see llvm::FunctionPassManager::doFinalization

proc finalizeFunctionPassManager*(fpm: PassManagerRef): Bool {.
    importc: "LLVMFinalizeFunctionPassManager".}
## * Frees the memory of a pass pipeline. For function pipelines, does not free
##     the module provider.
##     @see llvm::PassManagerBase::~PassManagerBase.

proc disposePassManager*(pm: PassManagerRef) {.importc: "LLVMDisposePassManager",
    .}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreThreading Threading
##
##  Handle the structures needed to make LLVM safe for multithreading.
##
##  @{
##
## * Deprecated: Multi-threading can only be enabled/disabled with the compile
##     time define LLVM_ENABLE_THREADS.  This function always returns
##     LLVMIsMultithreaded().

proc startMultithreaded*(): Bool {.importc: "LLVMStartMultithreaded".}
## * Deprecated: Multi-threading can only be enabled/disabled with the compile
##     time define LLVM_ENABLE_THREADS.

proc stopMultithreaded*() {.importc: "LLVMStopMultithreaded".}
## * Check whether LLVM is executing in thread-safe mode or not.
##     @see llvm::llvm_is_multithreaded

proc isMultithreaded*(): Bool {.importc: "LLVMIsMultithreaded".}
## *
##  @}
##
## *
##  @}
##
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END #  LLVM_C_CORE_H [NewLine]
## Error: expected ';'!!!
