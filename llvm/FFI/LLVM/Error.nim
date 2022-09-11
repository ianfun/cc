type
  ErrorTypeId* = pointer

proc getErrorTypeId*(err: ErrorRef): ErrorTypeId {.importc: "LLVMGetErrorTypeId".}

proc consumeError*(err: ErrorRef) {.importc: "LLVMConsumeError".}

proc getErrorMessage*(err: ErrorRef): cstring {.importc: "LLVMGetErrorMessage".}

proc disposeErrorMessage*(errMsg: cstring) {.importc: "LLVMDisposeErrorMessage".}

proc getStringErrorTypeId*(): ErrorTypeId {.importc: "LLVMGetStringErrorTypeId".}

proc createStringError*(errMsg: cstring): ErrorRef {.
    importc: "LLVMCreateStringError".}
