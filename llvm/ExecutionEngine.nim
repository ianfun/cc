## ===-- llvm-c/ExecutionEngine.h - ExecutionEngine Lib C Iface --*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMExecutionEngine.o, which    *|
## |* implements various analyses of the LLVM IR.                                *|
## |*                                                                            *|
## |* Many exotic languages can interoperate with C code but have a harder time  *|
## |* with C++ due to name mangling. So in addition to C, this interface enables *|
## |* tools written in such languages.                                           *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_EXECUTIONENGINE_H [NewLine] # LLVM_C_EXECUTIONENGINE_H [NewLine] # llvm-c/ExternC.h [NewLine] # llvm-c/Target.h [NewLine] # llvm-c/TargetMachine.h [NewLine] # llvm-c/Types.h [NewLine] LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMCExecutionEngine Execution Engine
##  @ingroup LLVMC
##
##  @{
##  void LLVMLinkInMCJIT ( void ) ;
## Error: expected ';'!!!

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


## ===-- Operations on generic values --------------------------------------===

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
## ===-- Operations on execution engines -----------------------------------===

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
## *
##  Create an MCJIT execution engine for a module, with the given options. It is
##  the responsibility of the caller to ensure that all fields in Options up to
##  the given SizeOfOptions are initialized. It is correct to pass a smaller
##  value of SizeOfOptions that omits some fields. The canonical way of using
##  this is:
##
##  LLVMMCJITCompilerOptions options;
##  LLVMInitializeMCJITCompilerOptions(&options, sizeof(options));
##  ... fill in those options you care about
##  LLVMCreateMCJITCompilerForModule(&jit, mod, &options, sizeof(options),
##                                   &error);
##
##  Note that this is also correct, though possibly suboptimal:
##
##  LLVMCreateMCJITCompilerForModule(&jit, mod, 0, 0, &error);
##

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
## / Returns true on error, false on success. If true is returned then the error
## / message is copied to OutStr and cleared in the ExecutionEngine instance.

proc executionEngineGetErrMsg*(ee: ExecutionEngineRef; outError: cstringArray): Bool {.
    importc: "LLVMExecutionEngineGetErrMsg".}
## ===-- Operations on memory managers -------------------------------------===

type
  MemoryManagerAllocateCodeSectionCallback* = proc (opaque: pointer; size: uintptrT;
      alignment: cuint; sectionID: cuint; sectionName: cstring): ptr uint8T
  MemoryManagerAllocateDataSectionCallback* = proc (opaque: pointer; size: uintptrT;
      alignment: cuint; sectionID: cuint; sectionName: cstring; isReadOnly: Bool): ptr uint8T
  MemoryManagerFinalizeMemoryCallback* = proc (opaque: pointer; errMsg: cstringArray): Bool
  MemoryManagerDestroyCallback* = proc (opaque: pointer)

## *
##  Create a simple custom MCJIT memory manager. This memory manager can
##  intercept allocations in a module-oblivious way. This will return NULL
##  if any of the passed functions are NULL.
##
##  @param Opaque An opaque client object to pass back to the callbacks.
##  @param AllocateCodeSection Allocate a block of memory for executable code.
##  @param AllocateDataSection Allocate a block of memory for data.
##  @param FinalizeMemory Set page permissions and flush cache. Return 0 on
##    success, 1 on error.
##

proc createSimpleMCJITMemoryManager*(opaque: pointer; allocateCodeSection: MemoryManagerAllocateCodeSectionCallback;
    allocateDataSection: MemoryManagerAllocateDataSectionCallback; finalizeMemory: MemoryManagerFinalizeMemoryCallback;
                                    destroy: MemoryManagerDestroyCallback): MCJITMemoryManagerRef {.
    importc: "LLVMCreateSimpleMCJITMemoryManager".}
proc disposeMCJITMemoryManager*(mm: MCJITMemoryManagerRef) {.
    importc: "LLVMDisposeMCJITMemoryManager".}
## ===-- JIT Event Listener functions -------------------------------------===

proc createGDBRegistrationListener*(): JITEventListenerRef {.
    importc: "LLVMCreateGDBRegistrationListener".}
proc createIntelJITEventListener*(): JITEventListenerRef {.
    importc: "LLVMCreateIntelJITEventListener".}
proc createOProfileJITEventListener*(): JITEventListenerRef {.
    importc: "LLVMCreateOProfileJITEventListener".}
proc createPerfJITEventListener*(): JITEventListenerRef {.
    importc: "LLVMCreatePerfJITEventListener".}
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END # [NewLine]
## Error: expected ';'!!!
