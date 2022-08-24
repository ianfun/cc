## ===-- Scalar.h - Scalar Transformation Library C Interface ----*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMScalarOpts.a, which         *|
## |* implements various scalar transformations of the LLVM IR.                  *|
## |*                                                                            *|
## |* Many exotic languages can interoperate with C code but have a harder time  *|
## |* with C++ due to name mangling. So in addition to C, this interface enables *|
## |* tools written in such languages.                                           *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_TRANSFORMS_SCALAR_H [NewLine] # LLVM_C_TRANSFORMS_SCALAR_H [NewLine] # llvm-c/ExternC.h [NewLine] # llvm-c/Types.h [NewLine] LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMCTransformsScalar Scalar transformations
##  @ingroup LLVMCTransforms
##
##  @{
##  * See llvm::createAggressiveDCEPass function. void LLVMAddAggressiveDCEPass ( LLVMPassManagerRef PM ) ;
## Error: expected ';'!!!

## * See llvm::createDeadCodeEliminationPass function.

proc addDCEPass*(pm: PassManagerRef) {.importc: "LLVMAddDCEPass".}
## * See llvm::createBitTrackingDCEPass function.

proc addBitTrackingDCEPass*(pm: PassManagerRef) {.
    importc: "LLVMAddBitTrackingDCEPass".}
## * See llvm::createAlignmentFromAssumptionsPass function.

proc addAlignmentFromAssumptionsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddAlignmentFromAssumptionsPass".}
## * See llvm::createCFGSimplificationPass function.

proc addCFGSimplificationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddCFGSimplificationPass".}
## * See llvm::createDeadStoreEliminationPass function.

proc addDeadStoreEliminationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddDeadStoreEliminationPass".}
## * See llvm::createScalarizerPass function.

proc addScalarizerPass*(pm: PassManagerRef) {.importc: "LLVMAddScalarizerPass",
    .}
## * See llvm::createMergedLoadStoreMotionPass function.

proc addMergedLoadStoreMotionPass*(pm: PassManagerRef) {.
    importc: "LLVMAddMergedLoadStoreMotionPass".}
## * See llvm::createGVNPass function.

proc addGVNPass*(pm: PassManagerRef) {.importc: "LLVMAddGVNPass".}
## * See llvm::createGVNPass function.

proc addNewGVNPass*(pm: PassManagerRef) {.importc: "LLVMAddNewGVNPass",
                                       .}
## * See llvm::createIndVarSimplifyPass function.

proc addIndVarSimplifyPass*(pm: PassManagerRef) {.
    importc: "LLVMAddIndVarSimplifyPass".}
## * See llvm::createInstructionCombiningPass function.

proc addInstructionCombiningPass*(pm: PassManagerRef) {.
    importc: "LLVMAddInstructionCombiningPass".}
## * See llvm::createInstSimplifyLegacyPass function.

proc addInstructionSimplifyPass*(pm: PassManagerRef) {.
    importc: "LLVMAddInstructionSimplifyPass".}
## * See llvm::createJumpThreadingPass function.

proc addJumpThreadingPass*(pm: PassManagerRef) {.
    importc: "LLVMAddJumpThreadingPass".}
## * See llvm::createLICMPass function.

proc addLICMPass*(pm: PassManagerRef) {.importc: "LLVMAddLICMPass".}
## * See llvm::createLoopDeletionPass function.

proc addLoopDeletionPass*(pm: PassManagerRef) {.importc: "LLVMAddLoopDeletionPass",
    .}
## * See llvm::createLoopIdiomPass function

proc addLoopIdiomPass*(pm: PassManagerRef) {.importc: "LLVMAddLoopIdiomPass",
    .}
## * See llvm::createLoopRotatePass function.

proc addLoopRotatePass*(pm: PassManagerRef) {.importc: "LLVMAddLoopRotatePass",
    .}
## * See llvm::createLoopRerollPass function.

proc addLoopRerollPass*(pm: PassManagerRef) {.importc: "LLVMAddLoopRerollPass",
    .}
## * See llvm::createLoopUnrollPass function.

proc addLoopUnrollPass*(pm: PassManagerRef) {.importc: "LLVMAddLoopUnrollPass",
    .}
## * See llvm::createLoopUnrollAndJamPass function.

proc addLoopUnrollAndJamPass*(pm: PassManagerRef) {.
    importc: "LLVMAddLoopUnrollAndJamPass".}
## * See llvm::createLowerAtomicPass function.

proc addLowerAtomicPass*(pm: PassManagerRef) {.importc: "LLVMAddLowerAtomicPass",
    .}
## * See llvm::createMemCpyOptPass function.

proc addMemCpyOptPass*(pm: PassManagerRef) {.importc: "LLVMAddMemCpyOptPass",
    .}
## * See llvm::createPartiallyInlineLibCallsPass function.

proc addPartiallyInlineLibCallsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddPartiallyInlineLibCallsPass".}
## * See llvm::createReassociatePass function.

proc addReassociatePass*(pm: PassManagerRef) {.importc: "LLVMAddReassociatePass",
    .}
## * See llvm::createSCCPPass function.

proc addSCCPPass*(pm: PassManagerRef) {.importc: "LLVMAddSCCPPass".}
## * See llvm::createSROAPass function.

proc addScalarReplAggregatesPass*(pm: PassManagerRef) {.
    importc: "LLVMAddScalarReplAggregatesPass".}
## * See llvm::createSROAPass function.

proc addScalarReplAggregatesPassSSA*(pm: PassManagerRef) {.
    importc: "LLVMAddScalarReplAggregatesPassSSA".}
## * See llvm::createSROAPass function.

proc addScalarReplAggregatesPassWithThreshold*(pm: PassManagerRef; threshold: cint) {.
    importc: "LLVMAddScalarReplAggregatesPassWithThreshold".}
## * See llvm::createSimplifyLibCallsPass function.

proc addSimplifyLibCallsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddSimplifyLibCallsPass".}
## * See llvm::createTailCallEliminationPass function.

proc addTailCallEliminationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddTailCallEliminationPass".}
## * See llvm::demotePromoteMemoryToRegisterPass function.

proc addDemoteMemoryToRegisterPass*(pm: PassManagerRef) {.
    importc: "LLVMAddDemoteMemoryToRegisterPass".}
## * See llvm::createVerifierPass function.

proc addVerifierPass*(pm: PassManagerRef) {.importc: "LLVMAddVerifierPass",
    .}
## * See llvm::createCorrelatedValuePropagationPass function

proc addCorrelatedValuePropagationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddCorrelatedValuePropagationPass".}
## * See llvm::createEarlyCSEPass function

proc addEarlyCSEPass*(pm: PassManagerRef) {.importc: "LLVMAddEarlyCSEPass",
    .}
## * See llvm::createEarlyCSEPass function

proc addEarlyCSEMemSSAPass*(pm: PassManagerRef) {.
    importc: "LLVMAddEarlyCSEMemSSAPass".}
## * See llvm::createLowerExpectIntrinsicPass function

proc addLowerExpectIntrinsicPass*(pm: PassManagerRef) {.
    importc: "LLVMAddLowerExpectIntrinsicPass".}
## * See llvm::createLowerConstantIntrinsicsPass function

proc addLowerConstantIntrinsicsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddLowerConstantIntrinsicsPass".}
## * See llvm::createTypeBasedAliasAnalysisPass function

proc addTypeBasedAliasAnalysisPass*(pm: PassManagerRef) {.
    importc: "LLVMAddTypeBasedAliasAnalysisPass".}
## * See llvm::createScopedNoAliasAAPass function

proc addScopedNoAliasAAPass*(pm: PassManagerRef) {.
    importc: "LLVMAddScopedNoAliasAAPass".}
## * See llvm::createBasicAliasAnalysisPass function

proc addBasicAliasAnalysisPass*(pm: PassManagerRef) {.
    importc: "LLVMAddBasicAliasAnalysisPass".}
## * See llvm::createUnifyFunctionExitNodesPass function

proc addUnifyFunctionExitNodesPass*(pm: PassManagerRef) {.
    importc: "LLVMAddUnifyFunctionExitNodesPass".}
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END # [NewLine]
## Error: expected ';'!!!
