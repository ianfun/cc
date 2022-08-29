proc createDisasmCPU*(triple: cstring; cpu: cstring; disInfo: pointer; tagType: cint;
                     getOpInfo: OpInfoCallback; symbolLookUp: SymbolLookupCallback): DisasmContextRef {.
    importc: "LLVMCreateDisasmCPU".}


proc createDisasmCPUFeatures*(triple: cstring; cpu: cstring; features: cstring;
                             disInfo: pointer; tagType: cint;
                             getOpInfo: OpInfoCallback;
                             symbolLookUp: SymbolLookupCallback): DisasmContextRef {.
    importc: "LLVMCreateDisasmCPUFeatures".}


proc setDisasmOptions*(dc: DisasmContextRef; options: uint64T): cint {.
    importc: "LLVMSetDisasmOptions".}

  DisassemblerOptionUseMarkup* = 1
  DisassemblerOptionPrintImmHex* = 2
  DisassemblerOptionAsmPrinterVariant* = 4
  DisassemblerOptionSetInstrComments* = 8
  DisassemblerOptionPrintLatency* = 16


proc disasmDispose*(dc: DisasmContextRef) {.importc: "LLVMDisasmDispose".}


proc disasmInstruction*(dc: DisasmContextRef; bytes: ptr uint8T; bytesSize: uint64T;
                       pc: uint64T; outString: cstring; outStringSize: csize_t): csize_t {.
    importc: "LLVMDisasmInstruction".}
