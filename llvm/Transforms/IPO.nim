## ===-- IPO.h - Interprocedural Transformations C Interface -----*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMIPO.a, which implements     *|
## |* various interprocedural transformations of the LLVM IR.                    *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_TRANSFORMS_IPO_H [NewLine] # LLVM_C_TRANSFORMS_IPO_H [NewLine] # llvm-c/ExternC.h [NewLine] # llvm-c/Types.h [NewLine] LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMCTransformsIPO Interprocedural transformations
##  @ingroup LLVMCTransforms
##
##  @{
##  * See llvm::createConstantMergePass function. void LLVMAddConstantMergePass ( LLVMPassManagerRef PM ) ;
## Error: expected ';'!!!

## * See llvm::createMergeFunctionsPass function.

proc addMergeFunctionsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddMergeFunctionsPass".}
## * See llvm::createCalledValuePropagationPass function.

proc addCalledValuePropagationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddCalledValuePropagationPass".}
## * See llvm::createDeadArgEliminationPass function.

proc addDeadArgEliminationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddDeadArgEliminationPass".}
## * See llvm::createFunctionAttrsPass function.

proc addFunctionAttrsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddFunctionAttrsPass".}
## * See llvm::createFunctionInliningPass function.

proc addFunctionInliningPass*(pm: PassManagerRef) {.
    importc: "LLVMAddFunctionInliningPass".}
## * See llvm::createAlwaysInlinerPass function.

proc addAlwaysInlinerPass*(pm: PassManagerRef) {.
    importc: "LLVMAddAlwaysInlinerPass".}
## * See llvm::createGlobalDCEPass function.

proc addGlobalDCEPass*(pm: PassManagerRef) {.importc: "LLVMAddGlobalDCEPass",
    .}
## * See llvm::createGlobalOptimizerPass function.

proc addGlobalOptimizerPass*(pm: PassManagerRef) {.
    importc: "LLVMAddGlobalOptimizerPass".}
## * See llvm::createPruneEHPass function.

proc addPruneEHPass*(pm: PassManagerRef) {.importc: "LLVMAddPruneEHPass",
                                        .}
## * See llvm::createIPSCCPPass function.

proc addIPSCCPPass*(pm: PassManagerRef) {.importc: "LLVMAddIPSCCPPass",
                                       .}
## * See llvm::createInternalizePass function.

proc addInternalizePass*(a1: PassManagerRef; allButMain: cuint) {.
    importc: "LLVMAddInternalizePass".}
## *
##  Create and add the internalize pass to the given pass manager with the
##  provided preservation callback.
##
##  The context parameter is forwarded to the callback on each invocation.
##  As such, it is the responsibility of the caller to extend its lifetime
##  until execution of this pass has finished.
##
##  @see llvm::createInternalizePass function.
##

proc addInternalizePassWithMustPreservePredicate*(pm: PassManagerRef;
    context: pointer; mustPreserve: proc (a1: ValueRef; a2: pointer): Bool) {.
    importc: "LLVMAddInternalizePassWithMustPreservePredicate".}
## * See llvm::createStripDeadPrototypesPass function.

proc addStripDeadPrototypesPass*(pm: PassManagerRef) {.
    importc: "LLVMAddStripDeadPrototypesPass".}
## * See llvm::createStripSymbolsPass function.

proc addStripSymbolsPass*(pm: PassManagerRef) {.importc: "LLVMAddStripSymbolsPass",
    .}
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END # [NewLine]
## Error: expected ';'!!!
