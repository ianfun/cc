type
  TargetDataRef* = ptr OpaqueTargetData
  TargetLibraryInfoRef* = ptr OpaqueTargetLibraryInfotData

proc setModuleDataLayout*(m: ModuleRef; dl: TargetDataRef) {.
    importc: "LLVMSetModuleDataLayout".}

proc createTargetData*(stringRep: cstring): TargetDataRef {.
    importc: "LLVMCreateTargetData".}

proc disposeTargetData*(td: TargetDataRef) {.importc: "LLVMDisposeTargetData",
    .}

proc addTargetLibraryInfo*(tli: TargetLibraryInfoRef; pm: PassManagerRef) {.
    importc: "LLVMAddTargetLibraryInfo".}

proc copyStringRepOfTargetData*(td: TargetDataRef): cstring {.
    importc: "LLVMCopyStringRepOfTargetData".}

proc byteOrder*(td: TargetDataRef): ByteOrdering {.importc: "LLVMByteOrder",
    .}

proc pointerSize*(td: TargetDataRef): cuint {.importc: "LLVMPointerSize",
    .}

proc pointerSizeForAS*(td: TargetDataRef; `as`: cuint): cuint {.
    importc: "LLVMPointerSizeForAS".}

proc intPtrType*(td: TargetDataRef): TypeRef {.importc: "LLVMIntPtrType",
    .}

proc intPtrTypeForAS*(td: TargetDataRef; `as`: cuint): TypeRef {.
    importc: "LLVMIntPtrTypeForAS".}

proc intPtrTypeInContext*(c: ContextRef; td: TargetDataRef): TypeRef {.
    importc: "LLVMIntPtrTypeInContext".}

proc intPtrTypeForASInContext*(c: ContextRef; td: TargetDataRef; `as`: cuint): TypeRef {.
    importc: "LLVMIntPtrTypeForASInContext".}

proc sizeOfTypeInBits*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMSizeOfTypeInBits".}

proc storeSizeOfType*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMStoreSizeOfType".}

proc aBISizeOfType*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMABISizeOfType".}

proc aBIAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMABIAlignmentOfType".}

proc callFrameAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMCallFrameAlignmentOfType".}

proc preferredAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMPreferredAlignmentOfType".}

proc preferredAlignmentOfGlobal*(td: TargetDataRef; globalVar: ValueRef): cuint {.
    importc: "LLVMPreferredAlignmentOfGlobal".}

proc elementAtOffset*(td: TargetDataRef; structTy: TypeRef; offset: culonglong): cuint {.
    importc: "LLVMElementAtOffset".}

proc offsetOfElement*(td: TargetDataRef; structTy: TypeRef; element: cuint): culonglong {.
    importc: "LLVMOffsetOfElement".}
