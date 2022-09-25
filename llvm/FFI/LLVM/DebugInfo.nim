
type
  DWARFSourceLanguage* {.size: sizeof(cint).} = enum
    DWARFSourceLanguageC89, DWARFSourceLanguageC, DWARFSourceLanguageAda83,
    DWARFSourceLanguageC_plusPlus, DWARFSourceLanguageCobol74,
    DWARFSourceLanguageCobol85, DWARFSourceLanguageFortran77,
    DWARFSourceLanguageFortran90, DWARFSourceLanguagePascal83, DWARFSourceLanguageModula2,
    DWARFSourceLanguageJava, DWARFSourceLanguageC99, DWARFSourceLanguageAda95,
    DWARFSourceLanguageFortran95, DWARFSourceLanguagePLI, DWARFSourceLanguageObjC,
    DWARFSourceLanguageObjC_plusPlus, DWARFSourceLanguageUPC, DWARFSourceLanguageD,
    DWARFSourceLanguagePython,
    DWARFSourceLanguageOpenCL, DWARFSourceLanguageGo, DWARFSourceLanguageModula3,
    DWARFSourceLanguageHaskell, DWARFSourceLanguageC_plusPlus03,
    DWARFSourceLanguageC_plusPlus11, DWARFSourceLanguageOCaml,
    DWARFSourceLanguageRust, DWARFSourceLanguageC11, DWARFSourceLanguageSwift,
    DWARFSourceLanguageJulia, DWARFSourceLanguageDylan,
    DWARFSourceLanguageC_plusPlus14, DWARFSourceLanguageFortran03,
    DWARFSourceLanguageFortran08, DWARFSourceLanguageRenderScript, DWARFSourceLanguageBLISS,
    DWARFSourceLanguageMipsAssembler, DWARFSourceLanguageGOOGLE_RenderScript,
    DWARFSourceLanguageBORLAND_Delphi


type
  DWARFEmissionKind* {.size: sizeof(cint).} = enum
    DWARFEmissionNone = 0, DWARFEmissionFull, DWARFEmissionLineTablesOnly

const
  MDStringMetadataKind* = 0
  ConstantAsMetadataMetadataKind* = 1
  LocalAsMetadataMetadataKind* = 2
  DistinctMDOperandPlaceholderMetadataKind* = 3
  MDTupleMetadataKind* = 4
  DILocationMetadataKind* = 5
  DIExpressionMetadataKind* = 6
  DIGlobalVariableExpressionMetadataKind* = 7
  GenericDINodeMetadataKind* = 8
  DISubrangeMetadataKind* = 9
  DIEnumeratorMetadataKind* = 10
  DIBasicTypeMetadataKind* = 11
  DIDerivedTypeMetadataKind* = 12
  DICompositeTypeMetadataKind* = 13
  DISubroutineTypeMetadataKind* = 14
  DIFileMetadataKind* = 15
  DICompileUnitMetadataKind* = 16
  DISubprogramMetadataKind* = 17
  DILexicalBlockMetadataKind* = 18
  DILexicalBlockFileMetadataKind* = 19
  DINamespaceMetadataKind* = 20
  DIModuleMetadataKind* = 21
  DITemplateTypeParameterMetadataKind* = 22
  DITemplateValueParameterMetadataKind* = 23
  DIGlobalVariableMetadataKind* = 24
  DILocalVariableMetadataKind* = 25
  DILabelMetadataKind* = 26
  DIObjCPropertyMetadataKind* = 27
  DIImportedEntityMetadataKind* = 28
  DIMacroMetadataKind* = 29
  DIMacroFileMetadataKind* = 30
  DICommonBlockMetadataKind* = 31
  DIStringTypeMetadataKind* = 32
  DIGenericSubrangeMetadataKind* = 33
  DIArgListMetadataKind* = 34


type
  DWARFMacinfoRecordType* {.size: sizeof(cint).} = enum
    DWARFMacinfoRecordTypeDefine = 0x01, DWARFMacinfoRecordTypeMacro = 0x02,
    DWARFMacinfoRecordTypeStartFile = 0x03, DWARFMacinfoRecordTypeEndFile = 0x04,
    DWARFMacinfoRecordTypeVendorExt = 0xff


proc debugMetadataVersion*(): cuint {.importc: "LLVMDebugMetadataVersion".}


proc getModuleDebugMetadataVersion*(module: ModuleRef): cuint {.
    importc: "LLVMGetModuleDebugMetadataVersion".}


proc stripModuleDebugInfo*(module: ModuleRef): Bool {.
    importc: "LLVMStripModuleDebugInfo".}

proc createDIBuilderDisallowUnresolved*(m: ModuleRef): DIBuilderRef {.
    importc: "LLVMCreateDIBuilderDisallowUnresolved".}


proc createDIBuilder*(m: ModuleRef): DIBuilderRef {.importc: "LLVMCreateDIBuilder",
    .}


proc disposeDIBuilder*(builder: DIBuilderRef) {.importc: "LLVMDisposeDIBuilder",
    .}

proc dIBuilderFinalize*(builder: DIBuilderRef) {.importc: "LLVMDIBuilderFinalize",
    .}

proc dIBuilderFinalizeSubprogram*(builder: DIBuilderRef; subprogram: MetadataRef) {.
    importc: "LLVMDIBuilderFinalizeSubprogram".}

proc dIBuilderCreateCompileUnit*(builder: DIBuilderRef; lang: DWARFSourceLanguage;
                                fileRef: MetadataRef; producer: cstring;
                                producerLen: csize_t; isOptimized: Bool;
                                flags: cstring; flagsLen: csize_t;
                                runtimeVer: cuint; splitName: cstring;
                                splitNameLen: csize_t; kind: DWARFEmissionKind;
                                dWOId: cuint; splitDebugInlining: Bool;
                                debugInfoForProfiling: Bool; sysRoot: cstring;
                                sysRootLen: csize_t; sdk: cstring; sDKLen: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreateCompileUnit".}

proc dIBuilderCreateFile*(builder: DIBuilderRef; filename: cstring;
                         filenameLen: csize_t; directory: cstring;
                         directoryLen: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreateFile".}

proc dIBuilderCreateModule*(builder: DIBuilderRef; parentScope: MetadataRef;
                           name: cstring; nameLen: csize_t; configMacros: cstring;
                           configMacrosLen: csize_t; includePath: cstring;
                           includePathLen: csize_t; aPINotesFile: cstring;
                           aPINotesFileLen: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreateModule".}


proc dIBuilderCreateNameSpace*(builder: DIBuilderRef; parentScope: MetadataRef;
                              name: cstring; nameLen: csize_t; exportSymbols: Bool): MetadataRef {.
    importc: "LLVMDIBuilderCreateNameSpace".}

proc dIBuilderCreateFunction*(builder: DIBuilderRef; scope: MetadataRef;
                             name: cstring; nameLen: csize_t; linkageName: cstring;
                             linkageNameLen: csize_t; file: MetadataRef;
                             lineNo: cuint; ty: MetadataRef; isLocalToUnit: Bool;
                             isDefinition: Bool; scopeLine: cuint; flags: DIFlags;
                             isOptimized: Bool): MetadataRef {.
    importc: "LLVMDIBuilderCreateFunction".}

proc dIBuilderCreateLexicalBlock*(builder: DIBuilderRef; scope: MetadataRef;
                                 file: MetadataRef; line: cuint; column: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateLexicalBlock".}

proc dIBuilderCreateLexicalBlockFile*(builder: DIBuilderRef; scope: MetadataRef;
                                     file: MetadataRef; discriminator: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateLexicalBlockFile".}

proc dIBuilderCreateImportedModuleFromNamespace*(builder: DIBuilderRef;
    scope: MetadataRef; ns: MetadataRef; file: MetadataRef; line: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateImportedModuleFromNamespace".}

proc dIBuilderCreateImportedModuleFromAlias*(builder: DIBuilderRef;
    scope: MetadataRef; importedEntity: MetadataRef; file: MetadataRef; line: cuint;
    elements: ptr MetadataRef; numElements: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateImportedModuleFromAlias".}

proc dIBuilderCreateImportedModuleFromModule*(builder: DIBuilderRef;
    scope: MetadataRef; m: MetadataRef; file: MetadataRef; line: cuint;
    elements: ptr MetadataRef; numElements: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateImportedModuleFromModule".}

proc dIBuilderCreateImportedDeclaration*(builder: DIBuilderRef; scope: MetadataRef;
                                        decl: MetadataRef; file: MetadataRef;
                                        line: cuint; name: cstring;
                                        nameLen: csize_t;
                                        elements: ptr MetadataRef;
                                        numElements: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateImportedDeclaration".}

proc dIBuilderCreateDebugLocation*(ctx: ContextRef; line: cuint; column: cuint;
                                  scope: MetadataRef; inlinedAt: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateDebugLocation".}


proc dILocationGetLine*(location: MetadataRef): cuint {.
    importc: "LLVMDILocationGetLine".}

proc dILocationGetColumn*(location: MetadataRef): cuint {.
    importc: "LLVMDILocationGetColumn".}


proc dILocationGetScope*(location: MetadataRef): MetadataRef {.
    importc: "LLVMDILocationGetScope".}


proc dILocationGetInlinedAt*(location: MetadataRef): MetadataRef {.
    importc: "LLVMDILocationGetInlinedAt".}


proc dIScopeGetFile*(scope: MetadataRef): MetadataRef {.
    importc: "LLVMDIScopeGetFile".}

proc dIFileGetDirectory*(file: MetadataRef; len: ptr cuint): cstring {.
    importc: "LLVMDIFileGetDirectory".}

proc dIFileGetFilename*(file: MetadataRef; len: ptr cuint): cstring {.
    importc: "LLVMDIFileGetFilename".}


proc dIFileGetSource*(file: MetadataRef; len: ptr cuint): cstring {.
    importc: "LLVMDIFileGetSource".}

proc dIBuilderGetOrCreateTypeArray*(builder: DIBuilderRef; data: ptr MetadataRef;
                                   numElements: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderGetOrCreateTypeArray".}

proc dIBuilderCreateSubroutineType*(builder: DIBuilderRef; file: MetadataRef;
                                   parameterTypes: ptr MetadataRef;
                                   numParameterTypes: cuint; flags: DIFlags): MetadataRef {.
    importc: "LLVMDIBuilderCreateSubroutineType".}

proc dIBuilderCreateMacro*(builder: DIBuilderRef; parentMacroFile: MetadataRef;
                          line: cuint; recordType: DWARFMacinfoRecordType;
                          name: cstring; nameLen: csize_t; value: cstring;
                          valueLen: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreateMacro".}


proc dIBuilderCreateTempMacroFile*(builder: DIBuilderRef;
                                  parentMacroFile: MetadataRef; line: cuint;
                                  file: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateTempMacroFile".}


proc dIBuilderCreateEnumerator*(builder: DIBuilderRef; name: cstring;
                               nameLen: csize_t; value: int64T; isUnsigned: Bool): MetadataRef {.
    importc: "LLVMDIBuilderCreateEnumerator".}

proc dIBuilderCreateEnumerationType*(builder: DIBuilderRef; scope: MetadataRef;
                                    name: cstring; nameLen: csize_t;
                                    file: MetadataRef; lineNumber: cuint;
                                    sizeInBits: uint64T; alignInBits: uint32;
                                    elements: ptr MetadataRef; numElements: cuint;
                                    classTy: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateEnumerationType".}


proc dIBuilderCreateUnionType*(builder: DIBuilderRef; scope: MetadataRef;
                              name: cstring; nameLen: csize_t; file: MetadataRef;
                              lineNumber: cuint; sizeInBits: uint64T;
                              alignInBits: uint64; flags: DIFlags;
                              elements: ptr MetadataRef; numElements: cuint;
                              runTimeLang: cuint; uniqueId: cstring;
                              uniqueIdLen: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreateUnionType".}


proc dIBuilderCreateArrayType*(builder: DIBuilderRef; size: uint64T;
                              alignInBits: uint32; ty: MetadataRef;
                              subscripts: ptr MetadataRef; numSubscripts: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateArrayType".}

proc dIBuilderCreateVectorType*(builder: DIBuilderRef; size: uint64T;
                               alignInBits: uint32; ty: MetadataRef;
                               subscripts: ptr MetadataRef; numSubscripts: cuint): MetadataRef {.
    importc: "LLVMDIBuilderCreateVectorType".}


proc dIBuilderCreateUnspecifiedType*(builder: DIBuilderRef; name: cstring;
                                    nameLen: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreateUnspecifiedType".}


proc dIBuilderCreateBasicType*(builder: DIBuilderRef; name: cstring;
                              nameLen: csize_t; sizeInBits: uint64T;
                              encoding: DWARFTypeEncoding; flags: DIFlags): MetadataRef {.
    importc: "LLVMDIBuilderCreateBasicType".}

proc dIBuilderCreatePointerType*(builder: DIBuilderRef; pointeeTy: MetadataRef;
                                sizeInBits: uint64T; alignInBits: uint32;
                                addressSpace: cuint; name: cstring; nameLen: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreatePointerType".}


proc dIBuilderCreateStructType*(builder: DIBuilderRef; scope: MetadataRef;
                               name: cstring; nameLen: csize_t; file: MetadataRef;
                               lineNumber: cuint; sizeInBits: uint64T;
                               alignInBits: uint32; flags: DIFlags;
                               derivedFrom: MetadataRef;
                               elements: ptr MetadataRef; numElements: cuint;
                               runTimeLang: cuint; vTableHolder: MetadataRef;
                               uniqueId: cstring; uniqueIdLen: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreateStructType".}


proc dIBuilderCreateMemberType*(builder: DIBuilderRef; scope: MetadataRef;
                               name: cstring; nameLen: csize_t; file: MetadataRef;
                               lineNo: cuint; sizeInBits: uint64T;
                               alignInBits: uint32; offsetInBits: uint64T;
                               flags: DIFlags; ty: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateMemberType".}


proc dIBuilderCreateStaticMemberType*(builder: DIBuilderRef; scope: MetadataRef;
                                     name: cstring; nameLen: csize_t;
                                     file: MetadataRef; lineNumber: cuint;
                                     `type`: MetadataRef; flags: DIFlags;
                                     constantVal: ValueRef; alignInBits: uint32): MetadataRef {.
    importc: "LLVMDIBuilderCreateStaticMemberType".}

proc dIBuilderCreateMemberPointerType*(builder: DIBuilderRef;
                                      pointeeType: MetadataRef;
                                      classType: MetadataRef; sizeInBits: uint64T;
                                      alignInBits: uint32; flags: DIFlags): MetadataRef {.
    importc: "LLVMDIBuilderCreateMemberPointerType".}


proc dIBuilderCreateObjCIVar*(builder: DIBuilderRef; name: cstring; nameLen: csize_t;
                             file: MetadataRef; lineNo: cuint; sizeInBits: uint64T;
                             alignInBits: uint32; offsetInBits: uint64T;
                             flags: DIFlags; ty: MetadataRef;
                             propertyNode: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateObjCIVar".}


proc dIBuilderCreateObjCProperty*(builder: DIBuilderRef; name: cstring;
                                 nameLen: csize_t; file: MetadataRef; lineNo: cuint;
                                 getterName: cstring; getterNameLen: csize_t;
                                 setterName: cstring; setterNameLen: csize_t;
                                 propertyAttributes: cuint; ty: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateObjCProperty".}


proc dIBuilderCreateObjectPointerType*(builder: DIBuilderRef; `type`: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateObjectPointerType".}


proc dIBuilderCreateQualifiedType*(builder: DIBuilderRef; tag: cuint;
                                  `type`: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateQualifiedType".}

proc dIBuilderCreateReferenceType*(builder: DIBuilderRef; tag: cuint;
                                  `type`: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateReferenceType".}


proc dIBuilderCreateNullPtrType*(builder: DIBuilderRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateNullPtrType".}

proc dIBuilderCreateTypedef*(builder: DIBuilderRef; `type`: MetadataRef;
                            name: cstring; nameLen: csize_t; file: MetadataRef;
                            lineNo: cuint; scope: MetadataRef; alignInBits: uint32): MetadataRef {.
    importc: "LLVMDIBuilderCreateTypedef".}
proc dIBuilderCreateInheritance*(builder: DIBuilderRef; ty: MetadataRef;
                                baseTy: MetadataRef; baseOffset: uint64T;
                                vBPtrOffset: uint32; flags: DIFlags): MetadataRef {.
    importc: "LLVMDIBuilderCreateInheritance".}

proc dIBuilderCreateForwardDecl*(builder: DIBuilderRef; tag: cuint; name: cstring;
                                nameLen: csize_t; scope: MetadataRef;
                                file: MetadataRef; line: cuint; runtimeLang: cuint;
                                sizeInBits: uint64T; alignInBits: uint32;
                                uniqueIdentifier: cstring;
                                uniqueIdentifierLen: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreateForwardDecl".}


proc dIBuilderCreateReplaceableCompositeType*(builder: DIBuilderRef; tag: cuint;
    name: cstring; nameLen: csize_t; scope: MetadataRef; file: MetadataRef; line: cuint;
    runtimeLang: cuint; sizeInBits: uint64T; alignInBits: uint32; flags: DIFlags;
    uniqueIdentifier: cstring; uniqueIdentifierLen: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreateReplaceableCompositeType".}

proc dIBuilderCreateBitFieldMemberType*(builder: DIBuilderRef; scope: MetadataRef;
                                       name: cstring; nameLen: csize_t;
                                       file: MetadataRef; lineNumber: cuint;
                                       sizeInBits: uint64T; offsetInBits: uint64T;
                                       storageOffsetInBits: uint64T;
                                       flags: DIFlags; `type`: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateBitFieldMemberType".}


proc dIBuilderCreateClassType*(builder: DIBuilderRef; scope: MetadataRef;
                              name: cstring; nameLen: csize_t; file: MetadataRef;
                              lineNumber: cuint; sizeInBits: uint64T;
                              alignInBits: uint32; offsetInBits: uint64T;
                              flags: DIFlags; derivedFrom: MetadataRef;
                              elements: ptr MetadataRef; numElements: cuint;
                              vTableHolder: MetadataRef;
                              templateParamsNode: MetadataRef;
                              uniqueIdentifier: cstring;
                              uniqueIdentifierLen: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreateClassType".}


proc dIBuilderCreateArtificialType*(builder: DIBuilderRef; `type`: MetadataRef): MetadataRef {.
    importc: "LLVMDIBuilderCreateArtificialType".}


proc dITypeGetName*(dType: MetadataRef; length: ptr csize_t): cstring {.
    importc: "LLVMDITypeGetName".}


proc dITypeGetSizeInBits*(dType: MetadataRef): uint64T {.
    importc: "LLVMDITypeGetSizeInBits".}


proc dITypeGetOffsetInBits*(dType: MetadataRef): uint64T {.
    importc: "LLVMDITypeGetOffsetInBits".}

proc dITypeGetAlignInBits*(dType: MetadataRef): uint32 {.
    importc: "LLVMDITypeGetAlignInBits".}

proc dITypeGetLine*(dType: MetadataRef): cuint {.importc: "LLVMDITypeGetLine",
    .}


proc dITypeGetFlags*(dType: MetadataRef): DIFlags {.importc: "LLVMDITypeGetFlags",
    .}


proc dIBuilderGetOrCreateSubrange*(builder: DIBuilderRef; lowerBound: int64T;
                                  count: int64T): MetadataRef {.
    importc: "LLVMDIBuilderGetOrCreateSubrange".}

proc dIBuilderGetOrCreateArray*(builder: DIBuilderRef; data: ptr MetadataRef;
                               numElements: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderGetOrCreateArray".}


proc dIBuilderCreateExpression*(builder: DIBuilderRef; `addr`: ptr uint64T;
                               length: csize_t): MetadataRef {.
    importc: "LLVMDIBuilderCreateExpression".}


proc dIBuilderCreateConstantValueExpression*(builder: DIBuilderRef; value: uint64T): MetadataRef {.
    importc: "LLVMDIBuilderCreateConstantValueExpression".}

proc dIBuilderCreateGlobalVariableExpression*(builder: DIBuilderRef;
    scope: MetadataRef; name: cstring; nameLen: csize_t; linkage: cstring;
    linkLen: csize_t; file: MetadataRef; lineNo: cuint; ty: MetadataRef;
    localToUnit: Bool; expr: MetadataRef; decl: MetadataRef; alignInBits: uint32): MetadataRef {.
    importc: "LLVMDIBuilderCreateGlobalVariableExpression".}


proc dIGlobalVariableExpressionGetVariable*(gve: MetadataRef): MetadataRef {.
    importc: "LLVMDIGlobalVariableExpressionGetVariable".}


proc dIGlobalVariableExpressionGetExpression*(gve: MetadataRef): MetadataRef {.
    importc: "LLVMDIGlobalVariableExpressionGetExpression".}


proc dIVariableGetFile*(`var`: MetadataRef): MetadataRef {.
    importc: "LLVMDIVariableGetFile".}


proc dIVariableGetScope*(`var`: MetadataRef): MetadataRef {.
    importc: "LLVMDIVariableGetScope".}


proc dIVariableGetLine*(`var`: MetadataRef): cuint {.
    importc: "LLVMDIVariableGetLine".}

proc temporaryMDNode*(ctx: ContextRef; data: ptr MetadataRef; numElements: csize_t): MetadataRef {.
    importc: "LLVMTemporaryMDNode".}


proc disposeTemporaryMDNode*(tempNode: MetadataRef) {.
    importc: "LLVMDisposeTemporaryMDNode".}


proc metadataReplaceAllUsesWith*(tempTargetMetadata: MetadataRef;
                                replacement: MetadataRef) {.
    importc: "LLVMMetadataReplaceAllUsesWith".}


proc dIBuilderCreateTempGlobalVariableFwdDecl*(builder: DIBuilderRef;
    scope: MetadataRef; name: cstring; nameLen: csize_t; linkage: cstring;
    lnkLen: csize_t; file: MetadataRef; lineNo: cuint; ty: MetadataRef;
    localToUnit: Bool; decl: MetadataRef; alignInBits: uint32): MetadataRef {.
    importc: "LLVMDIBuilderCreateTempGlobalVariableFwdDecl".}


proc dIBuilderInsertDeclareBefore*(builder: DIBuilderRef; storage: ValueRef;
                                  varInfo: MetadataRef; expr: MetadataRef;
                                  debugLoc: MetadataRef; instr: ValueRef): ValueRef {.
    importc: "LLVMDIBuilderInsertDeclareBefore".}


proc dIBuilderInsertDeclareAtEnd*(builder: DIBuilderRef; storage: ValueRef;
                                 varInfo: MetadataRef; expr: MetadataRef;
                                 debugLoc: MetadataRef; `block`: BasicBlockRef): ValueRef {.
    importc: "LLVMDIBuilderInsertDeclareAtEnd".}


proc dIBuilderInsertDbgValueBefore*(builder: DIBuilderRef; val: ValueRef;
                                   varInfo: MetadataRef; expr: MetadataRef;
                                   debugLoc: MetadataRef; instr: ValueRef): ValueRef {.
    importc: "LLVMDIBuilderInsertDbgValueBefore".}


proc dIBuilderInsertDbgValueAtEnd*(builder: DIBuilderRef; val: ValueRef;
                                  varInfo: MetadataRef; expr: MetadataRef;
                                  debugLoc: MetadataRef; `block`: BasicBlockRef): ValueRef {.
    importc: "LLVMDIBuilderInsertDbgValueAtEnd".}


proc dIBuilderCreateAutoVariable*(builder: DIBuilderRef; scope: MetadataRef;
                                 name: cstring; nameLen: csize_t; file: MetadataRef;
                                 lineNo: cuint; ty: MetadataRef;
                                 alwaysPreserve: Bool; flags: DIFlags;
                                 alignInBits: uint32): MetadataRef {.
    importc: "LLVMDIBuilderCreateAutoVariable".}


proc dIBuilderCreateParameterVariable*(builder: DIBuilderRef; scope: MetadataRef;
                                      name: cstring; nameLen: csize_t; argNo: cuint;
                                      file: MetadataRef; lineNo: cuint;
                                      ty: MetadataRef; alwaysPreserve: Bool;
                                      flags: DIFlags): MetadataRef {.
    importc: "LLVMDIBuilderCreateParameterVariable".}


proc getSubprogram*(`func`: ValueRef): MetadataRef {.importc: "LLVMGetSubprogram",
    .}


proc setSubprogram*(`func`: ValueRef; sp: MetadataRef) {.
    importc: "LLVMSetSubprogram".}

proc dISubprogramGetLine*(subprogram: MetadataRef): cuint {.
    importc: "LLVMDISubprogramGetLine".}


proc instructionGetDebugLoc*(inst: ValueRef): MetadataRef {.
    importc: "LLVMInstructionGetDebugLoc".}


proc instructionSetDebugLoc*(inst: ValueRef; loc: MetadataRef) {.
    importc: "LLVMInstructionSetDebugLoc".}


proc getMetadataKind*(metadata: MetadataRef): MetadataKind {.
    importc: "LLVMGetMetadataKind".}

