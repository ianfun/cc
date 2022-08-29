proc installFatalErrorHandler*(handler: FatalErrorHandler) {.
    importc: "LLVMInstallFatalErrorHandler".}

proc resetFatalErrorHandler*() {.importc: "LLVMResetFatalErrorHandler".}


proc enablePrettyStackTrace*() {.importc: "LLVMEnablePrettyStackTrace".}
