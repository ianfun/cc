proc parseCommandLineOptions*(argc: cint; argv: cstringArray; overview: cstring) {.
    importc: "LLVMParseCommandLineOptions".}

proc searchForAddressOfSymbol*(symbolName: cstring): pointer {.
    importc: "LLVMSearchForAddressOfSymbol".}

proc addSymbol*(symbolName: cstring; symbolValue: pointer) {.
    importc: "LLVMAddSymbol".}
