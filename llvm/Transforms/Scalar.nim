proc addDCEPass*(pm: PassManagerRef) {.importc: "LLVMAddDCEPass".}

proc addBitTrackingDCEPass*(pm: PassManagerRef) {.
    importc: "LLVMAddBitTrackingDCEPass".}

proc addAlignmentFromAssumptionsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddAlignmentFromAssumptionsPass".}

proc addCFGSimplificationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddCFGSimplificationPass".}

proc addDeadStoreEliminationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddDeadStoreEliminationPass".}

proc addScalarizerPass*(pm: PassManagerRef) {.importc: "LLVMAddScalarizerPass",
    .}

proc addMergedLoadStoreMotionPass*(pm: PassManagerRef) {.
    importc: "LLVMAddMergedLoadStoreMotionPass".}

proc addGVNPass*(pm: PassManagerRef) {.importc: "LLVMAddGVNPass".}

proc addNewGVNPass*(pm: PassManagerRef) {.importc: "LLVMAddNewGVNPass",
                                       .}

proc addIndVarSimplifyPass*(pm: PassManagerRef) {.
    importc: "LLVMAddIndVarSimplifyPass".}

proc addInstructionCombiningPass*(pm: PassManagerRef) {.
    importc: "LLVMAddInstructionCombiningPass".}

proc addInstructionSimplifyPass*(pm: PassManagerRef) {.
    importc: "LLVMAddInstructionSimplifyPass".}

proc addJumpThreadingPass*(pm: PassManagerRef) {.
    importc: "LLVMAddJumpThreadingPass".}

proc addLICMPass*(pm: PassManagerRef) {.importc: "LLVMAddLICMPass".}

proc addLoopDeletionPass*(pm: PassManagerRef) {.importc: "LLVMAddLoopDeletionPass",
    .}

proc addLoopIdiomPass*(pm: PassManagerRef) {.importc: "LLVMAddLoopIdiomPass",
    .}

proc addLoopRotatePass*(pm: PassManagerRef) {.importc: "LLVMAddLoopRotatePass",
    .}

proc addLoopRerollPass*(pm: PassManagerRef) {.importc: "LLVMAddLoopRerollPass",
    .}

proc addLoopUnrollPass*(pm: PassManagerRef) {.importc: "LLVMAddLoopUnrollPass",
    .}

proc addLoopUnrollAndJamPass*(pm: PassManagerRef) {.
    importc: "LLVMAddLoopUnrollAndJamPass".}

proc addLowerAtomicPass*(pm: PassManagerRef) {.importc: "LLVMAddLowerAtomicPass",
    .}

proc addMemCpyOptPass*(pm: PassManagerRef) {.importc: "LLVMAddMemCpyOptPass",
    .}

proc addPartiallyInlineLibCallsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddPartiallyInlineLibCallsPass".}

proc addReassociatePass*(pm: PassManagerRef) {.importc: "LLVMAddReassociatePass",
    .}

proc addSCCPPass*(pm: PassManagerRef) {.importc: "LLVMAddSCCPPass".}

proc addScalarReplAggregatesPass*(pm: PassManagerRef) {.
    importc: "LLVMAddScalarReplAggregatesPass".}

proc addScalarReplAggregatesPassSSA*(pm: PassManagerRef) {.
    importc: "LLVMAddScalarReplAggregatesPassSSA".}

proc addScalarReplAggregatesPassWithThreshold*(pm: PassManagerRef; threshold: cint) {.
    importc: "LLVMAddScalarReplAggregatesPassWithThreshold".}

proc addSimplifyLibCallsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddSimplifyLibCallsPass".}

proc addTailCallEliminationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddTailCallEliminationPass".}

proc addDemoteMemoryToRegisterPass*(pm: PassManagerRef) {.
    importc: "LLVMAddDemoteMemoryToRegisterPass".}

proc addVerifierPass*(pm: PassManagerRef) {.importc: "LLVMAddVerifierPass",
    .}

proc addCorrelatedValuePropagationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddCorrelatedValuePropagationPass".}

proc addEarlyCSEPass*(pm: PassManagerRef) {.importc: "LLVMAddEarlyCSEPass",
    .}

proc addEarlyCSEMemSSAPass*(pm: PassManagerRef) {.
    importc: "LLVMAddEarlyCSEMemSSAPass".}

proc addLowerExpectIntrinsicPass*(pm: PassManagerRef) {.
    importc: "LLVMAddLowerExpectIntrinsicPass".}

proc addLowerConstantIntrinsicsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddLowerConstantIntrinsicsPass".}

proc addTypeBasedAliasAnalysisPass*(pm: PassManagerRef) {.
    importc: "LLVMAddTypeBasedAliasAnalysisPass".}

proc addScopedNoAliasAAPass*(pm: PassManagerRef) {.
    importc: "LLVMAddScopedNoAliasAAPass".}

proc addBasicAliasAnalysisPass*(pm: PassManagerRef) {.
    importc: "LLVMAddBasicAliasAnalysisPass".}

proc addUnifyFunctionExitNodesPass*(pm: PassManagerRef) {.
    importc: "LLVMAddUnifyFunctionExitNodesPass".}
