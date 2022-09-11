proc parseBitcode2*(memBuf: MemoryBufferRef; outModule: ptr ModuleRef): Bool {.
    importc: "LLVMParseBitcode2".}

proc parseBitcodeInContext*(contextRef: ContextRef; memBuf: MemoryBufferRef;
                           outModule: ptr ModuleRef; outMessage: cstringArray): Bool {.
    importc: "LLVMParseBitcodeInContext".}
proc parseBitcodeInContext2*(contextRef: ContextRef; memBuf: MemoryBufferRef;
                            outModule: ptr ModuleRef): Bool {.
    importc: "LLVMParseBitcodeInContext2".}

proc getBitcodeModuleInContext*(contextRef: ContextRef; memBuf: MemoryBufferRef;
                               outM: ptr ModuleRef; outMessage: cstringArray): Bool {.
    importc: "LLVMGetBitcodeModuleInContext".}


proc getBitcodeModuleInContext2*(contextRef: ContextRef; memBuf: MemoryBufferRef;
                                outM: ptr ModuleRef): Bool {.
    importc: "LLVMGetBitcodeModuleInContext2".}

proc getBitcodeModule*(memBuf: MemoryBufferRef; outM: ptr ModuleRef;
                      outMessage: cstringArray): Bool {.
    importc: "LLVMGetBitcodeModule".}
proc getBitcodeModule2*(memBuf: MemoryBufferRef; outM: ptr ModuleRef): Bool {.
    importc: "LLVMGetBitcodeModule2".}
