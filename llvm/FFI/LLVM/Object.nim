type
  SymbolIteratorRef* = ptr opaqueSymbolIterator
  RelocationIteratorRef* = ptr opaqueRelocationIterator
  BinaryType* {.size: sizeof(cint).} = enum
    BinaryTypeArchive,        
    BinaryTypeMachOUniversalBinary,
    BinaryTypeCOFFImportFile,
    BinaryTypeIR,            
    BinaryTypeWinRes,        
    BinaryTypeCOFF,          
    BinaryTypeELF32L,        
    BinaryTypeELF32B,        
    BinaryTypeELF64L,        
    BinaryTypeELF64B,        
    BinaryTypeMachO32L,      
    BinaryTypeMachO32B,      
    BinaryTypeMachO64L,      
    BinaryTypeMachO64B,      
    BinaryTypeWasm,          
    BinaryTypeOffload        


proc createBinary*(memBuf: MemoryBufferRef; context: ContextRef;
                  errorMessage: cstringArray): BinaryRef {.
    importc: "LLVMCreateBinary".}


proc disposeBinary*(br: BinaryRef) {.importc: "LLVMDisposeBinary".}


proc binaryCopyMemoryBuffer*(br: BinaryRef): MemoryBufferRef {.
    importc: "LLVMBinaryCopyMemoryBuffer".}

proc binaryGetType*(br: BinaryRef): BinaryType {.importc: "LLVMBinaryGetType".}

proc machOUniversalBinaryCopyObjectForArch*(br: BinaryRef; arch: cstring;
    archLen: csize_t; errorMessage: cstringArray): BinaryRef {.
    importc: "LLVMMachOUniversalBinaryCopyObjectForArch".}


proc objectFileCopySectionIterator*(br: BinaryRef): SectionIteratorRef {.
    importc: "LLVMObjectFileCopySectionIterator".}


proc objectFileIsSectionIteratorAtEnd*(br: BinaryRef; si: SectionIteratorRef): Bool {.
    importc: "LLVMObjectFileIsSectionIteratorAtEnd".}


proc objectFileCopySymbolIterator*(br: BinaryRef): SymbolIteratorRef {.
    importc: "LLVMObjectFileCopySymbolIterator".}

proc objectFileIsSymbolIteratorAtEnd*(br: BinaryRef; si: SymbolIteratorRef): Bool {.
    importc: "LLVMObjectFileIsSymbolIteratorAtEnd".}
proc disposeSectionIterator*(si: SectionIteratorRef) {.
    importc: "LLVMDisposeSectionIterator".}
proc moveToNextSection*(si: SectionIteratorRef) {.importc: "LLVMMoveToNextSection",
    .}
proc moveToContainingSection*(sect: SectionIteratorRef; sym: SymbolIteratorRef) {.
    importc: "LLVMMoveToContainingSection".}

proc disposeSymbolIterator*(si: SymbolIteratorRef) {.
    importc: "LLVMDisposeSymbolIterator".}
proc moveToNextSymbol*(si: SymbolIteratorRef) {.importc: "LLVMMoveToNextSymbol",
    .}

proc getSectionName*(si: SectionIteratorRef): cstring {.
    importc: "LLVMGetSectionName".}
proc getSectionSize*(si: SectionIteratorRef): uint64T {.
    importc: "LLVMGetSectionSize".}
proc getSectionContents*(si: SectionIteratorRef): cstring {.
    importc: "LLVMGetSectionContents".}
proc getSectionAddress*(si: SectionIteratorRef): uint64T {.
    importc: "LLVMGetSectionAddress".}
proc getSectionContainsSymbol*(si: SectionIteratorRef; sym: SymbolIteratorRef): Bool {.
    importc: "LLVMGetSectionContainsSymbol".}

proc getRelocations*(section: SectionIteratorRef): RelocationIteratorRef {.
    importc: "LLVMGetRelocations".}
proc disposeRelocationIterator*(ri: RelocationIteratorRef) {.
    importc: "LLVMDisposeRelocationIterator".}
proc isRelocationIteratorAtEnd*(section: SectionIteratorRef;
                               ri: RelocationIteratorRef): Bool {.
    importc: "LLVMIsRelocationIteratorAtEnd".}
proc moveToNextRelocation*(ri: RelocationIteratorRef) {.
    importc: "LLVMMoveToNextRelocation".}

proc getSymbolName*(si: SymbolIteratorRef): cstring {.importc: "LLVMGetSymbolName",
    .}
proc getSymbolAddress*(si: SymbolIteratorRef): uint64T {.
    importc: "LLVMGetSymbolAddress".}
proc getSymbolSize*(si: SymbolIteratorRef): uint64T {.importc: "LLVMGetSymbolSize",
    .}

proc getRelocationOffset*(ri: RelocationIteratorRef): uint64T {.
    importc: "LLVMGetRelocationOffset".}
proc getRelocationSymbol*(ri: RelocationIteratorRef): SymbolIteratorRef {.
    importc: "LLVMGetRelocationSymbol".}
proc getRelocationType*(ri: RelocationIteratorRef): uint64T {.
    importc: "LLVMGetRelocationType".}

proc getRelocationTypeName*(ri: RelocationIteratorRef): cstring {.
    importc: "LLVMGetRelocationTypeName".}
proc getRelocationValueString*(ri: RelocationIteratorRef): cstring {.
    importc: "LLVMGetRelocationValueString".}

type
  ObjectFileRef* = ptr opaqueObjectFile


proc createObjectFile*(memBuf: MemoryBufferRef): ObjectFileRef {.
    importc: "LLVMCreateObjectFile".}

proc disposeObjectFile*(objectFile: ObjectFileRef) {.
    importc: "LLVMDisposeObjectFile".}

proc getSections*(objectFile: ObjectFileRef): SectionIteratorRef {.
    importc: "LLVMGetSections".}

proc isSectionIteratorAtEnd*(objectFile: ObjectFileRef; si: SectionIteratorRef): Bool {.
    importc: "LLVMIsSectionIteratorAtEnd".}

proc getSymbols*(objectFile: ObjectFileRef): SymbolIteratorRef {.
    importc: "LLVMGetSymbols".}

proc isSymbolIteratorAtEnd*(objectFile: ObjectFileRef; si: SymbolIteratorRef): Bool {.
    importc: "LLVMIsSymbolIteratorAtEnd".}
