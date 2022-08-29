proc initializeTransformUtils*(r: PassRegistryRef) {.
    importc: "LLVMInitializeTransformUtils".}
proc initializeScalarOpts*(r: PassRegistryRef) {.
    importc: "LLVMInitializeScalarOpts".}
proc initializeObjCARCOpts*(r: PassRegistryRef) {.
    importc: "LLVMInitializeObjCARCOpts".}
proc initializeVectorization*(r: PassRegistryRef) {.
    importc: "LLVMInitializeVectorization".}
proc initializeInstCombine*(r: PassRegistryRef) {.
    importc: "LLVMInitializeInstCombine".}
proc initializeAggressiveInstCombiner*(r: PassRegistryRef) {.
    importc: "LLVMInitializeAggressiveInstCombiner".}
proc initializeIPO*(r: PassRegistryRef) {.importc: "LLVMInitializeIPO".}
proc initializeInstrumentation*(r: PassRegistryRef) {.
    importc: "LLVMInitializeInstrumentation".}
proc initializeAnalysis*(r: PassRegistryRef) {.importc: "LLVMInitializeAnalysis".}
proc initializeIPA*(r: PassRegistryRef) {.importc: "LLVMInitializeIPA".}
proc initializeCodeGen*(r: PassRegistryRef) {.importc: "LLVMInitializeCodeGen".}
proc initializeTarget*(r: PassRegistryRef) {.importc: "LLVMInitializeTarget".}
