proc addPromoteMemoryToRegisterPass*(pm: PassManagerRef) {.
    importc: "LLVMAddPromoteMemoryToRegisterPass".}

proc addAddDiscriminatorsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddAddDiscriminatorsPass".}
