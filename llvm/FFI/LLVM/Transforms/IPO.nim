proc addMergeFunctionsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddMergeFunctionsPass".}

proc addCalledValuePropagationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddCalledValuePropagationPass".}

proc addDeadArgEliminationPass*(pm: PassManagerRef) {.
    importc: "LLVMAddDeadArgEliminationPass".}

proc addFunctionAttrsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddFunctionAttrsPass".}

proc addFunctionInliningPass*(pm: PassManagerRef) {.
    importc: "LLVMAddFunctionInliningPass".}

proc addAlwaysInlinerPass*(pm: PassManagerRef) {.
    importc: "LLVMAddAlwaysInlinerPass".}

proc addGlobalDCEPass*(pm: PassManagerRef) {.importc: "LLVMAddGlobalDCEPass".}

proc addGlobalOptimizerPass*(pm: PassManagerRef) {.
    importc: "LLVMAddGlobalOptimizerPass".}

proc addPruneEHPass*(pm: PassManagerRef) {.importc: "LLVMAddPruneEHPass".}

proc addIPSCCPPass*(pm: PassManagerRef) {.importc: "LLVMAddIPSCCPPass".}

proc addInternalizePass*(a1: PassManagerRef; allButMain: cuint) {.
    importc: "LLVMAddInternalizePass".}

proc addInternalizePassWithMustPreservePredicate*(pm: PassManagerRef;
    context: pointer; mustPreserve: proc (a1: ValueRef; a2: pointer): Bool) {.
    importc: "LLVMAddInternalizePassWithMustPreservePredicate".}

proc addStripDeadPrototypesPass*(pm: PassManagerRef) {.
    importc: "LLVMAddStripDeadPrototypesPass".}

proc addStripSymbolsPass*(pm: PassManagerRef) {.importc: "LLVMAddStripSymbolsPass".}
