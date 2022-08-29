type
  LtoBoolT* = cuchar
  LtoBoolT* = bool



const
  LTO_API_VERSION* = 29


type
  LtoSymbolAttributes* {.size: sizeof(cint).} = enum
    LTO_SYMBOL_ALIGNMENT_MASK = 0x0000001F,
    LTO_SYMBOL_PERMISSIONS_RODATA = 0x00000080,
    LTO_SYMBOL_PERMISSIONS_CODE = 0x000000A0,
    LTO_SYMBOL_PERMISSIONS_DATA = 0x000000C0,
    LTO_SYMBOL_PERMISSIONS_MASK = 0x000000E0,
    LTO_SYMBOL_DEFINITION_REGULAR = 0x00000100,
    LTO_SYMBOL_DEFINITION_TENTATIVE = 0x00000200,
    LTO_SYMBOL_DEFINITION_WEAK = 0x00000300,
    LTO_SYMBOL_DEFINITION_UNDEFINED = 0x00000400,
    LTO_SYMBOL_DEFINITION_WEAKUNDEF = 0x00000500,
    LTO_SYMBOL_DEFINITION_MASK = 0x00000700,
    LTO_SYMBOL_SCOPE_INTERNAL = 0x00000800, LTO_SYMBOL_SCOPE_HIDDEN = 0x00001000,
    LTO_SYMBOL_SCOPE_DEFAULT = 0x00001800, LTO_SYMBOL_SCOPE_PROTECTED = 0x00002000,
    LTO_SYMBOL_SCOPE_DEFAULT_CAN_BE_HIDDEN = 0x00002800,
    LTO_SYMBOL_SCOPE_MASK = 0x00003800, LTO_SYMBOL_COMDAT = 0x00004000,
    LTO_SYMBOL_ALIAS = 0x00008000

type
  LtoDebugModel* {.size: sizeof(cint).} = enum
    LTO_DEBUG_MODEL_NONE = 0, LTO_DEBUG_MODEL_DWARF = 1



type
  LtoCodegenModel* {.size: sizeof(cint).} = enum
    LTO_CODEGEN_PIC_MODEL_STATIC = 0, LTO_CODEGEN_PIC_MODEL_DYNAMIC = 1,
    LTO_CODEGEN_PIC_MODEL_DYNAMIC_NO_PIC = 2, LTO_CODEGEN_PIC_MODEL_DEFAULT = 3


type
  LtoModuleT* = ptr opaqueLTOModule

type
  LtoCodeGenT* = ptr opaqueLTOCodeGenerator

type
  ThinltoCodeGenT* = ptr opaqueThinLTOCodeGenerator


proc ltoGetErrorMessage*(): cstring {.importc: "lto_get_error_message".}

proc ltoModuleIsObjectFile*(path: cstring): LtoBoolT {.
    importc: "lto_module_is_object_file".}

proc ltoModuleIsObjectFileForTarget*(path: cstring; targetTriplePrefix: cstring): LtoBoolT {.
    importc: "lto_module_is_object_file_for_target".}


proc ltoModuleHasObjcCategory*(mem: pointer; length: csize_t): LtoBoolT {.
    importc: "lto_module_has_objc_category".}

proc ltoModuleIsObjectFileInMemory*(mem: pointer; length: csize_t): LtoBoolT {.
    importc: "lto_module_is_object_file_in_memory".}


proc ltoModuleIsObjectFileInMemoryForTarget*(mem: pointer; length: csize_t;
    targetTriplePrefix: cstring): LtoBoolT {.
    importc: "lto_module_is_object_file_in_memory_for_target".}


proc ltoModuleCreate*(path: cstring): LtoModuleT {.importc: "lto_module_create".}


proc ltoModuleCreateFromMemory*(mem: pointer; length: csize_t): LtoModuleT {.
    importc: "lto_module_create_from_memory".}

proc ltoModuleCreateFromMemoryWithPath*(mem: pointer; length: csize_t; path: cstring): LtoModuleT {.
    importc: "lto_module_create_from_memory_with_path".}


proc ltoModuleCreateInLocalContext*(mem: pointer; length: csize_t; path: cstring): LtoModuleT {.
    importc: "lto_module_create_in_local_context".}

proc ltoModuleCreateInCodegenContext*(mem: pointer; length: csize_t; path: cstring;
                                     cg: LtoCodeGenT): LtoModuleT {.
    importc: "lto_module_create_in_codegen_context".}


proc ltoModuleCreateFromFd*(fd: cint; path: cstring; fileSize: csize_t): LtoModuleT {.
    importc: "lto_module_create_from_fd".}

proc ltoModuleCreateFromFdAtOffset*(fd: cint; path: cstring; fileSize: csize_t;
                                   mapSize: csize_t; offset: OffT): LtoModuleT {.
    importc: "lto_module_create_from_fd_at_offset".}


proc ltoModuleDispose*(`mod`: LtoModuleT) {.importc: "lto_module_dispose",
    .}


proc ltoModuleGetTargetTriple*(`mod`: LtoModuleT): cstring {.
    importc: "lto_module_get_target_triple".}


proc ltoModuleSetTargetTriple*(`mod`: LtoModuleT; triple: cstring) {.
    importc: "lto_module_set_target_triple".}


proc ltoModuleGetNumSymbols*(`mod`: LtoModuleT): cuint {.
    importc: "lto_module_get_num_symbols".}

proc ltoModuleGetSymbolName*(`mod`: LtoModuleT; index: cuint): cstring {.
    importc: "lto_module_get_symbol_name".}

proc ltoModuleGetSymbolAttribute*(`mod`: LtoModuleT; index: cuint): LtoSymbolAttributes {.
    importc: "lto_module_get_symbol_attribute".}


proc ltoModuleGetLinkeropts*(`mod`: LtoModuleT): cstring {.
    importc: "lto_module_get_linkeropts".}


proc ltoModuleGetMachoCputype*(`mod`: LtoModuleT; outCputype: ptr cuint;
                              outCpusubtype: ptr cuint): LtoBoolT {.
    importc: "lto_module_get_macho_cputype".}


proc ltoModuleHasCtorDtor*(`mod`: LtoModuleT): LtoBoolT {.
    importc: "lto_module_has_ctor_dtor".}


type
  LtoCodegenDiagnosticSeverityT* {.size: sizeof(cint).} = enum
    LTO_DS_ERROR = 0, LTO_DS_WARNING = 1, LTO_DS_NOTE = 2, LTO_DS_REMARK = 3


type
  LtoDiagnosticHandlerT* = proc (severity: LtoCodegenDiagnosticSeverityT;
                              diag: cstring; ctxt: pointer)


proc ltoCodegenSetDiagnosticHandler*(a1: LtoCodeGenT; a2: LtoDiagnosticHandlerT;
                                    a3: pointer) {.
    importc: "lto_codegen_set_diagnostic_handler".}

proc ltoCodegenCreate*(): LtoCodeGenT {.importc: "lto_codegen_create",
                                     .}


proc ltoCodegenCreateInLocalContext*(): LtoCodeGenT {.
    importc: "lto_codegen_create_in_local_context".}


proc ltoCodegenDispose*(a1: LtoCodeGenT) {.importc: "lto_codegen_dispose",
                                        .}
proc ltoCodegenAddModule*(cg: LtoCodeGenT; `mod`: LtoModuleT): LtoBoolT {.
    importc: "lto_codegen_add_module".}


proc ltoCodegenSetModule*(cg: LtoCodeGenT; `mod`: LtoModuleT) {.
    importc: "lto_codegen_set_module".}

proc ltoCodegenSetDebugModel*(cg: LtoCodeGenT; a2: LtoDebugModel): LtoBoolT {.
    importc: "lto_codegen_set_debug_model".}


proc ltoCodegenSetPicModel*(cg: LtoCodeGenT; a2: LtoCodegenModel): LtoBoolT {.
    importc: "lto_codegen_set_pic_model".}


proc ltoCodegenSetCpu*(cg: LtoCodeGenT; cpu: cstring) {.
    importc: "lto_codegen_set_cpu".}


proc ltoCodegenSetAssemblerPath*(cg: LtoCodeGenT; path: cstring) {.
    importc: "lto_codegen_set_assembler_path".}

proc ltoCodegenSetAssemblerArgs*(cg: LtoCodeGenT; args: cstringArray; nargs: cint) {.
    importc: "lto_codegen_set_assembler_args".}


proc ltoCodegenAddMustPreserveSymbol*(cg: LtoCodeGenT; symbol: cstring) {.
    importc: "lto_codegen_add_must_preserve_symbol".}


proc ltoCodegenWriteMergedModules*(cg: LtoCodeGenT; path: cstring): LtoBoolT {.
    importc: "lto_codegen_write_merged_modules".}


proc ltoCodegenCompile*(cg: LtoCodeGenT; length: ptr csize_t): pointer {.
    importc: "lto_codegen_compile".}

proc ltoCodegenCompileToFile*(cg: LtoCodeGenT; name: cstringArray): LtoBoolT {.
    importc: "lto_codegen_compile_to_file".}


proc ltoCodegenOptimize*(cg: LtoCodeGenT): LtoBoolT {.
    importc: "lto_codegen_optimize".}

proc ltoCodegenCompileOptimized*(cg: LtoCodeGenT; length: ptr csize_t): pointer {.
    importc: "lto_codegen_compile_optimized".}


proc ltoApiVersion*(): cuint {.importc: "lto_api_version".}

proc ltoSetDebugOptions*(options: cstringArray; number: cint) {.
    importc: "lto_set_debug_options".}

proc ltoCodegenDebugOptions*(cg: LtoCodeGenT; a2: cstring) {.
    importc: "lto_codegen_debug_options".}


proc ltoCodegenDebugOptionsArray*(cg: LtoCodeGenT; a2: cstringArray; number: cint) {.
    importc: "lto_codegen_debug_options_array".}


proc ltoInitializeDisassembler*() {.importc: "lto_initialize_disassembler",
                                  .}


proc ltoCodegenSetShouldInternalize*(cg: LtoCodeGenT; shouldInternalize: LtoBoolT) {.
    importc: "lto_codegen_set_should_internalize".}


proc ltoCodegenSetShouldEmbedUselists*(cg: LtoCodeGenT;
                                      shouldEmbedUselists: LtoBoolT) {.
    importc: "lto_codegen_set_should_embed_uselists".}

type
  LtoInputT* = ptr opaqueLTOInput


proc ltoInputCreate*(buffer: pointer; bufferSize: csize_t; path: cstring): LtoInputT {.
    importc: "lto_input_create".}


proc ltoInputDispose*(input: LtoInputT) {.importc: "lto_input_dispose",
                                       .}


proc ltoInputGetNumDependentLibraries*(input: LtoInputT): cuint {.
    importc: "lto_input_get_num_dependent_libraries".}


proc ltoInputGetDependentLibrary*(input: LtoInputT; index: csize_t; size: ptr csize_t): cstring {.
    importc: "lto_input_get_dependent_library".}


proc ltoRuntimeLibSymbolsList*(size: ptr csize_t): cstringArray {.
    importc: "lto_runtime_lib_symbols_list".}

type
  LTOObjectBuffer* {.bycopy.} = object
    buffer*: cstring
    size*: csize_t


proc thinltoCreateCodegen*(): ThinltoCodeGenT {.importc: "thinlto_create_codegen",
    .}


proc thinltoCodegenDispose*(cg: ThinltoCodeGenT) {.
    importc: "thinlto_codegen_dispose".}

proc thinltoCodegenAddModule*(cg: ThinltoCodeGenT; identifier: cstring;
                             data: cstring; length: cint) {.
    importc: "thinlto_codegen_add_module".}


proc thinltoCodegenProcess*(cg: ThinltoCodeGenT) {.
    importc: "thinlto_codegen_process".}

proc thinltoModuleGetNumObjects*(cg: ThinltoCodeGenT): cuint {.
    importc: "thinlto_module_get_num_objects".}

proc thinltoModuleGetObject*(cg: ThinltoCodeGenT; index: cuint): LTOObjectBuffer {.
    importc: "thinlto_module_get_object".}

proc thinltoModuleGetNumObjectFiles*(cg: ThinltoCodeGenT): cuint {.
    importc: "thinlto_module_get_num_object_files".}


proc thinltoModuleGetObjectFile*(cg: ThinltoCodeGenT; index: cuint): cstring {.
    importc: "thinlto_module_get_object_file".}

proc thinltoCodegenSetPicModel*(cg: ThinltoCodeGenT; a2: LtoCodegenModel): LtoBoolT {.
    importc: "thinlto_codegen_set_pic_model".}


proc thinltoCodegenSetSavetempsDir*(cg: ThinltoCodeGenT; saveTempsDir: cstring) {.
    importc: "thinlto_codegen_set_savetemps_dir".}


proc thinltoSetGeneratedObjectsDir*(cg: ThinltoCodeGenT; saveTempsDir: cstring) {.
    importc: "thinlto_set_generated_objects_dir".}

proc thinltoCodegenSetCpu*(cg: ThinltoCodeGenT; cpu: cstring) {.
    importc: "thinlto_codegen_set_cpu".}


proc thinltoCodegenDisableCodegen*(cg: ThinltoCodeGenT; disable: LtoBoolT) {.
    importc: "thinlto_codegen_disable_codegen".}


proc thinltoCodegenSetCodegenOnly*(cg: ThinltoCodeGenT; codegenOnly: LtoBoolT) {.
    importc: "thinlto_codegen_set_codegen_only".}

proc thinltoDebugOptions*(options: cstringArray; number: cint) {.
    importc: "thinlto_debug_options".}


proc ltoModuleIsThinlto*(`mod`: LtoModuleT): LtoBoolT {.
    importc: "lto_module_is_thinlto".}

proc thinltoCodegenAddMustPreserveSymbol*(cg: ThinltoCodeGenT; name: cstring;
    length: cint) {.importc: "thinlto_codegen_add_must_preserve_symbol",
                  .}


proc thinltoCodegenAddCrossReferencedSymbol*(cg: ThinltoCodeGenT; name: cstring;
    length: cint) {.importc: "thinlto_codegen_add_cross_referenced_symbol",
                  .}

proc thinltoCodegenSetCacheDir*(cg: ThinltoCodeGenT; cacheDir: cstring) {.
    importc: "thinlto_codegen_set_cache_dir".}

proc thinltoCodegenSetCachePruningInterval*(cg: ThinltoCodeGenT; interval: cint) {.
    importc: "thinlto_codegen_set_cache_pruning_interval".}

proc thinltoCodegenSetFinalCacheSizeRelativeToAvailableSpace*(
    cg: ThinltoCodeGenT; percentage: cuint) {.importc: "thinlto_codegen_set_final_cache_size_relative_to_available_space",
    .}


proc thinltoCodegenSetCacheEntryExpiration*(cg: ThinltoCodeGenT; expiration: cuint) {.
    importc: "thinlto_codegen_set_cache_entry_expiration".}

proc thinltoCodegenSetCacheSizeBytes*(cg: ThinltoCodeGenT; maxSizeBytes: cuint) {.
    importc: "thinlto_codegen_set_cache_size_bytes".}


proc thinltoCodegenSetCacheSizeMegabytes*(cg: ThinltoCodeGenT;
    maxSizeMegabytes: cuint) {.importc: "thinlto_codegen_set_cache_size_megabytes",.}

proc thinltoCodegenSetCacheSizeFiles*(cg: ThinltoCodeGenT; maxSizeFiles: cuint) {.
    importc: "thinlto_codegen_set_cache_size_files".}
