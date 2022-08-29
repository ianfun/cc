proc verifyModule*(m: ModuleRef; action: VerifierFailureAction;
                  outMessage: cstringArray): Bool {.importc: "LLVMVerifyModule".}

proc verifyFunction*(fn: ValueRef; action: VerifierFailureAction): Bool {.
    importc: "LLVMVerifyFunction".}

proc viewFunctionCFG*(fn: ValueRef) {.importc: "LLVMViewFunctionCFG".}
proc viewFunctionCFGOnly*(fn: ValueRef) {.importc: "LLVMViewFunctionCFGOnly".}
