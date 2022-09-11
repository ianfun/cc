proc writeBitcodeToFD*(m: ModuleRef; fd: cint; shouldClose: cint; unbuffered: cint): cint {.
    importc: "LLVMWriteBitcodeToFD".}

proc writeBitcodeToFileHandle*(m: ModuleRef; handle: cint): cint {.
    importc: "LLVMWriteBitcodeToFileHandle".}

proc writeBitcodeToFile*(m: ModuleRef; Path: cstring): cint {.
    importc: "LLVMWriteBitcodeToFile".}

proc writeBitcodeToMemoryBuffer*(m: ModuleRef): MemoryBufferRef {.
    importc: "LLVMWriteBitcodeToMemoryBuffer".}
