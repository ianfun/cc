proc passManagerBuilderCreate*(): PassManagerBuilderRef {.importc: "LLVMPassManagerBuilderCreate".}

proc passManagerBuilderDispose*(pmb: PassManagerBuilderRef) {.importc: "LLVMPassManagerBuilderDispose".}

proc passManagerBuilderSetOptLevel*(pmb: PassManagerBuilderRef; optLevel: cuint) {.importc: "LLVMPassManagerBuilderSetOptLevel".}

proc passManagerBuilderSetSizeLevel*(pmb: PassManagerBuilderRef; sizeLevel: cuint) {.importc: "LLVMPassManagerBuilderSetSizeLevel".}

proc passManagerBuilderSetDisableUnitAtATime*(pmb: PassManagerBuilderRef; value: Bool) {.importc: "LLVMPassManagerBuilderSetDisableUnitAtATime".}

proc passManagerBuilderSetDisableUnrollLoops*(pmb: PassManagerBuilderRef; value: Bool) {.importc: "LLVMPassManagerBuilderSetDisableUnrollLoops",.}

proc passManagerBuilderSetDisableSimplifyLibCalls*(pmb: PassManagerBuilderRef; value: Bool) {.importc: "LLVMPassManagerBuilderSetDisableSimplifyLibCalls".}

proc passManagerBuilderUseInlinerWithThreshold*(pmb: PassManagerBuilderRef; threshold: cuint) {.importc: "LLVMPassManagerBuilderUseInlinerWithThreshold",.}

proc passManagerBuilderPopulateFunctionPassManager*(pmb: PassManagerBuilderRef; pm: PassManagerRef) {.importc: "LLVMPassManagerBuilderPopulateFunctionPassManager",.}

proc passManagerBuilderPopulateModulePassManager*(pmb: PassManagerBuilderRef; pm: PassManagerRef) {.importc: "LLVMPassManagerBuilderPopulateModulePassManager".}
