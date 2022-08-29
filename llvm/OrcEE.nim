proc orcRTDyldObjectLinkingLayerRegisterJITEventListener*(
    rTDyldObjLinkingLayer: OrcObjectLayerRef; listener: JITEventListenerRef) {.
    importc: "LLVMOrcRTDyldObjectLinkingLayerRegisterJITEventListener",
    .}
