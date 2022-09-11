proc runPasses*(m: ModuleRef; passes: cstring; tm: TargetMachineRef;
               options: PassBuilderOptionsRef): ErrorRef {.
    importc: "LLVMRunPasses".}


proc createPassBuilderOptions*(): PassBuilderOptionsRef {.
    importc: "LLVMCreatePassBuilderOptions".}


proc passBuilderOptionsSetVerifyEach*(options: PassBuilderOptionsRef;
                                     verifyEach: Bool) {.
    importc: "LLVMPassBuilderOptionsSetVerifyEach".}


proc passBuilderOptionsSetDebugLogging*(options: PassBuilderOptionsRef;
                                       debugLogging: Bool) {.
    importc: "LLVMPassBuilderOptionsSetDebugLogging".}
proc passBuilderOptionsSetLoopInterleaving*(options: PassBuilderOptionsRef;
    loopInterleaving: Bool) {.importc: "LLVMPassBuilderOptionsSetLoopInterleaving",
                            .}
proc passBuilderOptionsSetLoopVectorization*(options: PassBuilderOptionsRef;
    loopVectorization: Bool) {.importc: "LLVMPassBuilderOptionsSetLoopVectorization",
                             .}
proc passBuilderOptionsSetSLPVectorization*(options: PassBuilderOptionsRef;
    sLPVectorization: Bool) {.importc: "LLVMPassBuilderOptionsSetSLPVectorization",
                            .}
proc passBuilderOptionsSetLoopUnrolling*(options: PassBuilderOptionsRef;
                                        loopUnrolling: Bool) {.
    importc: "LLVMPassBuilderOptionsSetLoopUnrolling".}
proc passBuilderOptionsSetForgetAllSCEVInLoopUnroll*(
    options: PassBuilderOptionsRef; forgetAllSCEVInLoopUnroll: Bool) {.
    importc: "LLVMPassBuilderOptionsSetForgetAllSCEVInLoopUnroll".}
proc passBuilderOptionsSetLicmMssaOptCap*(options: PassBuilderOptionsRef;
    licmMssaOptCap: cuint) {.importc: "LLVMPassBuilderOptionsSetLicmMssaOptCap",
                           .}
proc passBuilderOptionsSetLicmMssaNoAccForPromotionCap*(
    options: PassBuilderOptionsRef; licmMssaNoAccForPromotionCap: cuint) {.
    importc: "LLVMPassBuilderOptionsSetLicmMssaNoAccForPromotionCap",
    .}
proc passBuilderOptionsSetCallGraphProfile*(options: PassBuilderOptionsRef;
    callGraphProfile: Bool) {.importc: "LLVMPassBuilderOptionsSetCallGraphProfile",
                            .}
proc passBuilderOptionsSetMergeFunctions*(options: PassBuilderOptionsRef;
    mergeFunctions: Bool) {.importc: "LLVMPassBuilderOptionsSetMergeFunctions",
                          .}


proc disposePassBuilderOptions*(options: PassBuilderOptionsRef) {.
    importc: "LLVMDisposePassBuilderOptions".}
