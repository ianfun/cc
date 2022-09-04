proc parseIRInContext(contextRef: ContextRef, membuf: MemoryBufferRef, outM: ptr ModuleRef, outMessage: cstringArray): Bool {.importc: "LLVMParseIRInContext".}
