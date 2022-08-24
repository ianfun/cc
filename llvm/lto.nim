## ===-- llvm-c/lto.h - LTO Public C Interface ---------------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header provides public interface to an abstract link time optimization*|
## |* library.  LLVM provides an implementation of this interface for use with   *|
## |* llvm bitcode files.                                                        *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_LTO_H [NewLine] # LLVM_C_LTO_H [NewLine] # llvm-c/ExternC.h [NewLine] # __cplusplus [NewLine] # < cstddef > [NewLine] # [NewLine] # < stddef . h > [NewLine] # [NewLine] # < sys / types . h > [NewLine] # __cplusplus [NewLine] # ! defined ( _MSC_VER ) [NewLine] # < stdbool . h > [NewLine] typedef bool lto_bool_t ;
## Error: expected ';'!!!

##  MSVC in particular does not have anything like _Bool or bool in C, but we can
##    at least make sure the type is the same size.  The implementation side will
##    use C++ bool.

type
  LtoBoolT* = cuchar
  LtoBoolT* = bool

## *
##  @defgroup LLVMCLTO LTO
##  @ingroup LLVMC
##
##  @{
##

const
  LTO_API_VERSION* = 29

## *
##  \since prior to LTO_API_VERSION=3
##

type
  LtoSymbolAttributes* {.size: sizeof(cint).} = enum
    LTO_SYMBOL_ALIGNMENT_MASK = 0x0000001F, ##  log2 of alignment
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


## *
##  \since prior to LTO_API_VERSION=3
##

type
  LtoDebugModel* {.size: sizeof(cint).} = enum
    LTO_DEBUG_MODEL_NONE = 0, LTO_DEBUG_MODEL_DWARF = 1


## *
##  \since prior to LTO_API_VERSION=3
##

type
  LtoCodegenModel* {.size: sizeof(cint).} = enum
    LTO_CODEGEN_PIC_MODEL_STATIC = 0, LTO_CODEGEN_PIC_MODEL_DYNAMIC = 1,
    LTO_CODEGEN_PIC_MODEL_DYNAMIC_NO_PIC = 2, LTO_CODEGEN_PIC_MODEL_DEFAULT = 3


## * opaque reference to a loaded object module

type
  LtoModuleT* = ptr opaqueLTOModule

## * opaque reference to a code generator

type
  LtoCodeGenT* = ptr opaqueLTOCodeGenerator

## * opaque reference to a thin code generator

type
  ThinltoCodeGenT* = ptr opaqueThinLTOCodeGenerator

## !!!Ignored construct:  LLVM_C_EXTERN_C_BEGIN *
##  Returns a printable string.
##
##  \since prior to LTO_API_VERSION=3
##  extern const char * lto_get_version ( void ) ;
## Error: expected ';'!!!

## *
##  Returns the last error string or NULL if last operation was successful.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoGetErrorMessage*(): cstring {.importc: "lto_get_error_message".}
## *
##  Checks if a file is a loadable object file.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoModuleIsObjectFile*(path: cstring): LtoBoolT {.
    importc: "lto_module_is_object_file".}
## *
##  Checks if a file is a loadable object compiled for requested target.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoModuleIsObjectFileForTarget*(path: cstring; targetTriplePrefix: cstring): LtoBoolT {.
    importc: "lto_module_is_object_file_for_target".}
## *
##  Return true if \p Buffer contains a bitcode file with ObjC code (category
##  or class) in it.
##
##  \since LTO_API_VERSION=20
##

proc ltoModuleHasObjcCategory*(mem: pointer; length: csize_t): LtoBoolT {.
    importc: "lto_module_has_objc_category".}
## *
##  Checks if a buffer is a loadable object file.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoModuleIsObjectFileInMemory*(mem: pointer; length: csize_t): LtoBoolT {.
    importc: "lto_module_is_object_file_in_memory".}
## *
##  Checks if a buffer is a loadable object compiled for requested target.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoModuleIsObjectFileInMemoryForTarget*(mem: pointer; length: csize_t;
    targetTriplePrefix: cstring): LtoBoolT {.
    importc: "lto_module_is_object_file_in_memory_for_target".}
## *
##  Loads an object file from disk.
##  Returns NULL on error (check lto_get_error_message() for details).
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoModuleCreate*(path: cstring): LtoModuleT {.importc: "lto_module_create".}
## *
##  Loads an object file from memory.
##  Returns NULL on error (check lto_get_error_message() for details).
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoModuleCreateFromMemory*(mem: pointer; length: csize_t): LtoModuleT {.
    importc: "lto_module_create_from_memory".}
## *
##  Loads an object file from memory with an extra path argument.
##  Returns NULL on error (check lto_get_error_message() for details).
##
##  \since LTO_API_VERSION=9
##

proc ltoModuleCreateFromMemoryWithPath*(mem: pointer; length: csize_t; path: cstring): LtoModuleT {.
    importc: "lto_module_create_from_memory_with_path".}
## *
##  Loads an object file in its own context.
##
##  Loads an object file in its own LLVMContext.  This function call is
##  thread-safe.  However, modules created this way should not be merged into an
##  lto_code_gen_t using \a lto_codegen_add_module().
##
##  Returns NULL on error (check lto_get_error_message() for details).
##
##  \since LTO_API_VERSION=11
##

proc ltoModuleCreateInLocalContext*(mem: pointer; length: csize_t; path: cstring): LtoModuleT {.
    importc: "lto_module_create_in_local_context".}
## *
##  Loads an object file in the codegen context.
##
##  Loads an object file into the same context as \c cg.  The module is safe to
##  add using \a lto_codegen_add_module().
##
##  Returns NULL on error (check lto_get_error_message() for details).
##
##  \since LTO_API_VERSION=11
##

proc ltoModuleCreateInCodegenContext*(mem: pointer; length: csize_t; path: cstring;
                                     cg: LtoCodeGenT): LtoModuleT {.
    importc: "lto_module_create_in_codegen_context".}
## *
##  Loads an object file from disk. The seek point of fd is not preserved.
##  Returns NULL on error (check lto_get_error_message() for details).
##
##  \since LTO_API_VERSION=5
##

proc ltoModuleCreateFromFd*(fd: cint; path: cstring; fileSize: csize_t): LtoModuleT {.
    importc: "lto_module_create_from_fd".}
## *
##  Loads an object file from disk. The seek point of fd is not preserved.
##  Returns NULL on error (check lto_get_error_message() for details).
##
##  \since LTO_API_VERSION=5
##

proc ltoModuleCreateFromFdAtOffset*(fd: cint; path: cstring; fileSize: csize_t;
                                   mapSize: csize_t; offset: OffT): LtoModuleT {.
    importc: "lto_module_create_from_fd_at_offset".}
## *
##  Frees all memory internally allocated by the module.
##  Upon return the lto_module_t is no longer valid.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoModuleDispose*(`mod`: LtoModuleT) {.importc: "lto_module_dispose",
    .}
## *
##  Returns triple string which the object module was compiled under.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoModuleGetTargetTriple*(`mod`: LtoModuleT): cstring {.
    importc: "lto_module_get_target_triple".}
## *
##  Sets triple string with which the object will be codegened.
##
##  \since LTO_API_VERSION=4
##

proc ltoModuleSetTargetTriple*(`mod`: LtoModuleT; triple: cstring) {.
    importc: "lto_module_set_target_triple".}
## *
##  Returns the number of symbols in the object module.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoModuleGetNumSymbols*(`mod`: LtoModuleT): cuint {.
    importc: "lto_module_get_num_symbols".}
## *
##  Returns the name of the ith symbol in the object module.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoModuleGetSymbolName*(`mod`: LtoModuleT; index: cuint): cstring {.
    importc: "lto_module_get_symbol_name".}
## *
##  Returns the attributes of the ith symbol in the object module.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoModuleGetSymbolAttribute*(`mod`: LtoModuleT; index: cuint): LtoSymbolAttributes {.
    importc: "lto_module_get_symbol_attribute".}
## *
##  Returns the module's linker options.
##
##  The linker options may consist of multiple flags. It is the linker's
##  responsibility to split the flags using a platform-specific mechanism.
##
##  \since LTO_API_VERSION=16
##

proc ltoModuleGetLinkeropts*(`mod`: LtoModuleT): cstring {.
    importc: "lto_module_get_linkeropts".}
## *
##  If targeting mach-o on darwin, this function gets the CPU type and subtype
##  that will end up being encoded in the mach-o header. These are the values
##  that can be found in mach/machine.h.
##
##  \p out_cputype and \p out_cpusubtype must be non-NULL.
##
##  Returns true on error (check lto_get_error_message() for details).
##
##  \since LTO_API_VERSION=27
##

proc ltoModuleGetMachoCputype*(`mod`: LtoModuleT; outCputype: ptr cuint;
                              outCpusubtype: ptr cuint): LtoBoolT {.
    importc: "lto_module_get_macho_cputype".}
## *
##  This function can be used by the linker to check if a given module has
##  any constructor or destructor functions.
##
##  Returns true if the module has either the @llvm.global_ctors or the
##  @llvm.global_dtors symbol. Otherwise returns false.
##
##  \since LTO_API_VERSION=29
##

proc ltoModuleHasCtorDtor*(`mod`: LtoModuleT): LtoBoolT {.
    importc: "lto_module_has_ctor_dtor".}
## *
##  Diagnostic severity.
##
##  \since LTO_API_VERSION=7
##

type
  LtoCodegenDiagnosticSeverityT* {.size: sizeof(cint).} = enum
    LTO_DS_ERROR = 0, LTO_DS_WARNING = 1, LTO_DS_NOTE = 2, LTO_DS_REMARK = 3 ##  Added in LTO_API_VERSION=10.


## *
##  Diagnostic handler type.
##  \p severity defines the severity.
##  \p diag is the actual diagnostic.
##  The diagnostic is not prefixed by any of severity keyword, e.g., 'error: '.
##  \p ctxt is used to pass the context set with the diagnostic handler.
##
##  \since LTO_API_VERSION=7
##

type
  LtoDiagnosticHandlerT* = proc (severity: LtoCodegenDiagnosticSeverityT;
                              diag: cstring; ctxt: pointer)

## *
##  Set a diagnostic handler and the related context (void *).
##  This is more general than lto_get_error_message, as the diagnostic handler
##  can be called at anytime within lto.
##
##  \since LTO_API_VERSION=7
##

proc ltoCodegenSetDiagnosticHandler*(a1: LtoCodeGenT; a2: LtoDiagnosticHandlerT;
                                    a3: pointer) {.
    importc: "lto_codegen_set_diagnostic_handler".}
## *
##  Instantiates a code generator.
##  Returns NULL on error (check lto_get_error_message() for details).
##
##  All modules added using \a lto_codegen_add_module() must have been created
##  in the same context as the codegen.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoCodegenCreate*(): LtoCodeGenT {.importc: "lto_codegen_create",
                                     .}
## *
##  Instantiate a code generator in its own context.
##
##  Instantiates a code generator in its own context.  Modules added via \a
##  lto_codegen_add_module() must have all been created in the same context,
##  using \a lto_module_create_in_codegen_context().
##
##  \since LTO_API_VERSION=11
##

proc ltoCodegenCreateInLocalContext*(): LtoCodeGenT {.
    importc: "lto_codegen_create_in_local_context".}
## *
##  Frees all code generator and all memory it internally allocated.
##  Upon return the lto_code_gen_t is no longer valid.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoCodegenDispose*(a1: LtoCodeGenT) {.importc: "lto_codegen_dispose",
                                        .}
## *
##  Add an object module to the set of modules for which code will be generated.
##  Returns true on error (check lto_get_error_message() for details).
##
##  \c cg and \c mod must both be in the same context.  See \a
##  lto_codegen_create_in_local_context() and \a
##  lto_module_create_in_codegen_context().
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoCodegenAddModule*(cg: LtoCodeGenT; `mod`: LtoModuleT): LtoBoolT {.
    importc: "lto_codegen_add_module".}
## *
##  Sets the object module for code generation. This will transfer the ownership
##  of the module to the code generator.
##
##  \c cg and \c mod must both be in the same context.
##
##  \since LTO_API_VERSION=13
##

proc ltoCodegenSetModule*(cg: LtoCodeGenT; `mod`: LtoModuleT) {.
    importc: "lto_codegen_set_module".}
## *
##  Sets if debug info should be generated.
##  Returns true on error (check lto_get_error_message() for details).
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoCodegenSetDebugModel*(cg: LtoCodeGenT; a2: LtoDebugModel): LtoBoolT {.
    importc: "lto_codegen_set_debug_model".}
## *
##  Sets which PIC code model to generated.
##  Returns true on error (check lto_get_error_message() for details).
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoCodegenSetPicModel*(cg: LtoCodeGenT; a2: LtoCodegenModel): LtoBoolT {.
    importc: "lto_codegen_set_pic_model".}
## *
##  Sets the cpu to generate code for.
##
##  \since LTO_API_VERSION=4
##

proc ltoCodegenSetCpu*(cg: LtoCodeGenT; cpu: cstring) {.
    importc: "lto_codegen_set_cpu".}
## *
##  Sets the location of the assembler tool to run. If not set, libLTO
##  will use gcc to invoke the assembler.
##
##  \since LTO_API_VERSION=3
##

proc ltoCodegenSetAssemblerPath*(cg: LtoCodeGenT; path: cstring) {.
    importc: "lto_codegen_set_assembler_path".}
## *
##  Sets extra arguments that libLTO should pass to the assembler.
##
##  \since LTO_API_VERSION=4
##

proc ltoCodegenSetAssemblerArgs*(cg: LtoCodeGenT; args: cstringArray; nargs: cint) {.
    importc: "lto_codegen_set_assembler_args".}
## *
##  Adds to a list of all global symbols that must exist in the final generated
##  code. If a function is not listed there, it might be inlined into every usage
##  and optimized away.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoCodegenAddMustPreserveSymbol*(cg: LtoCodeGenT; symbol: cstring) {.
    importc: "lto_codegen_add_must_preserve_symbol".}
## *
##  Writes a new object file at the specified path that contains the
##  merged contents of all modules added so far.
##  Returns true on error (check lto_get_error_message() for details).
##
##  \since LTO_API_VERSION=5
##

proc ltoCodegenWriteMergedModules*(cg: LtoCodeGenT; path: cstring): LtoBoolT {.
    importc: "lto_codegen_write_merged_modules".}
## *
##  Generates code for all added modules into one native object file.
##  This calls lto_codegen_optimize then lto_codegen_compile_optimized.
##
##  On success returns a pointer to a generated mach-o/ELF buffer and
##  length set to the buffer size.  The buffer is owned by the
##  lto_code_gen_t and will be freed when lto_codegen_dispose()
##  is called, or lto_codegen_compile() is called again.
##  On failure, returns NULL (check lto_get_error_message() for details).
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoCodegenCompile*(cg: LtoCodeGenT; length: ptr csize_t): pointer {.
    importc: "lto_codegen_compile".}
## *
##  Generates code for all added modules into one native object file.
##  This calls lto_codegen_optimize then lto_codegen_compile_optimized (instead
##  of returning a generated mach-o/ELF buffer, it writes to a file).
##
##  The name of the file is written to name. Returns true on error.
##
##  \since LTO_API_VERSION=5
##

proc ltoCodegenCompileToFile*(cg: LtoCodeGenT; name: cstringArray): LtoBoolT {.
    importc: "lto_codegen_compile_to_file".}
## *
##  Runs optimization for the merged module. Returns true on error.
##
##  \since LTO_API_VERSION=12
##

proc ltoCodegenOptimize*(cg: LtoCodeGenT): LtoBoolT {.
    importc: "lto_codegen_optimize".}
## *
##  Generates code for the optimized merged module into one native object file.
##  It will not run any IR optimizations on the merged module.
##
##  On success returns a pointer to a generated mach-o/ELF buffer and length set
##  to the buffer size.  The buffer is owned by the lto_code_gen_t and will be
##  freed when lto_codegen_dispose() is called, or
##  lto_codegen_compile_optimized() is called again. On failure, returns NULL
##  (check lto_get_error_message() for details).
##
##  \since LTO_API_VERSION=12
##

proc ltoCodegenCompileOptimized*(cg: LtoCodeGenT; length: ptr csize_t): pointer {.
    importc: "lto_codegen_compile_optimized".}
## *
##  Returns the runtime API version.
##
##  \since LTO_API_VERSION=12
##

proc ltoApiVersion*(): cuint {.importc: "lto_api_version".}
## *
##  Parses options immediately, making them available as early as possible. For
##  example during executing codegen::InitTargetOptionsFromCodeGenFlags. Since
##  parsing shud only happen once, only one of lto_codegen_debug_options or
##  lto_set_debug_options should be called.
##
##  This function takes one or more options separated by spaces.
##  Warning: passing file paths through this function may confuse the argument
##  parser if the paths contain spaces.
##
##  \since LTO_API_VERSION=28
##

proc ltoSetDebugOptions*(options: cstringArray; number: cint) {.
    importc: "lto_set_debug_options".}
## *
##  Sets options to help debug codegen bugs. Since parsing shud only happen once,
##  only one of lto_codegen_debug_options or lto_set_debug_options
##  should be called.
##
##  This function takes one or more options separated by spaces.
##  Warning: passing file paths through this function may confuse the argument
##  parser if the paths contain spaces.
##
##  \since prior to LTO_API_VERSION=3
##

proc ltoCodegenDebugOptions*(cg: LtoCodeGenT; a2: cstring) {.
    importc: "lto_codegen_debug_options".}
## *
##  Same as the previous function, but takes every option separately through an
##  array.
##
##  \since prior to LTO_API_VERSION=26
##

proc ltoCodegenDebugOptionsArray*(cg: LtoCodeGenT; a2: cstringArray; number: cint) {.
    importc: "lto_codegen_debug_options_array".}
## *
##  Initializes LLVM disassemblers.
##  FIXME: This doesn't really belong here.
##
##  \since LTO_API_VERSION=5
##

proc ltoInitializeDisassembler*() {.importc: "lto_initialize_disassembler",
                                  .}
## *
##  Sets if we should run internalize pass during optimization and code
##  generation.
##
##  \since LTO_API_VERSION=14
##

proc ltoCodegenSetShouldInternalize*(cg: LtoCodeGenT; shouldInternalize: LtoBoolT) {.
    importc: "lto_codegen_set_should_internalize".}
## *
##  Set whether to embed uselists in bitcode.
##
##  Sets whether \a lto_codegen_write_merged_modules() should embed uselists in
##  output bitcode.  This should be turned on for all -save-temps output.
##
##  \since LTO_API_VERSION=15
##

proc ltoCodegenSetShouldEmbedUselists*(cg: LtoCodeGenT;
                                      shouldEmbedUselists: LtoBoolT) {.
    importc: "lto_codegen_set_should_embed_uselists".}
## * Opaque reference to an LTO input file

type
  LtoInputT* = ptr opaqueLTOInput

## *
##  Creates an LTO input file from a buffer. The path
##  argument is used for diagnotics as this function
##  otherwise does not know which file the given buffer
##  is associated with.
##
##  \since LTO_API_VERSION=24
##

proc ltoInputCreate*(buffer: pointer; bufferSize: csize_t; path: cstring): LtoInputT {.
    importc: "lto_input_create".}
## *
##  Frees all memory internally allocated by the LTO input file.
##  Upon return the lto_module_t is no longer valid.
##
##  \since LTO_API_VERSION=24
##

proc ltoInputDispose*(input: LtoInputT) {.importc: "lto_input_dispose",
                                       .}
## *
##  Returns the number of dependent library specifiers
##  for the given LTO input file.
##
##  \since LTO_API_VERSION=24
##

proc ltoInputGetNumDependentLibraries*(input: LtoInputT): cuint {.
    importc: "lto_input_get_num_dependent_libraries".}
## *
##  Returns the ith dependent library specifier
##  for the given LTO input file. The returned
##  string is not null-terminated.
##
##  \since LTO_API_VERSION=24
##

proc ltoInputGetDependentLibrary*(input: LtoInputT; index: csize_t; size: ptr csize_t): cstring {.
    importc: "lto_input_get_dependent_library".}
## *
##  Returns the list of libcall symbols that can be generated by LTO
##  that might not be visible from the symbol table of bitcode files.
##
##  \since prior to LTO_API_VERSION=25
##

proc ltoRuntimeLibSymbolsList*(size: ptr csize_t): cstringArray {.
    importc: "lto_runtime_lib_symbols_list".}
## *
##  @} // endgoup LLVMCLTO
##  @defgroup LLVMCTLTO ThinLTO
##  @ingroup LLVMC
##
##  @{
##
## *
##  Type to wrap a single object returned by ThinLTO.
##
##  \since LTO_API_VERSION=18
##

type
  LTOObjectBuffer* {.bycopy.} = object
    buffer*: cstring
    size*: csize_t


## *
##  Instantiates a ThinLTO code generator.
##  Returns NULL on error (check lto_get_error_message() for details).
##
##
##  The ThinLTOCodeGenerator is not intended to be reuse for multiple
##  compilation: the model is that the client adds modules to the generator and
##  ask to perform the ThinLTO optimizations / codegen, and finally destroys the
##  codegenerator.
##
##  \since LTO_API_VERSION=18
##

proc thinltoCreateCodegen*(): ThinltoCodeGenT {.importc: "thinlto_create_codegen",
    .}
## *
##  Frees the generator and all memory it internally allocated.
##  Upon return the thinlto_code_gen_t is no longer valid.
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenDispose*(cg: ThinltoCodeGenT) {.
    importc: "thinlto_codegen_dispose".}
## *
##  Add a module to a ThinLTO code generator. Identifier has to be unique among
##  all the modules in a code generator. The data buffer stays owned by the
##  client, and is expected to be available for the entire lifetime of the
##  thinlto_code_gen_t it is added to.
##
##  On failure, returns NULL (check lto_get_error_message() for details).
##
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenAddModule*(cg: ThinltoCodeGenT; identifier: cstring;
                             data: cstring; length: cint) {.
    importc: "thinlto_codegen_add_module".}
## *
##  Optimize and codegen all the modules added to the codegenerator using
##  ThinLTO. Resulting objects are accessible using thinlto_module_get_object().
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenProcess*(cg: ThinltoCodeGenT) {.
    importc: "thinlto_codegen_process".}
## *
##  Returns the number of object files produced by the ThinLTO CodeGenerator.
##
##  It usually matches the number of input files, but this is not a guarantee of
##  the API and may change in future implementation, so the client should not
##  assume it.
##
##  \since LTO_API_VERSION=18
##

proc thinltoModuleGetNumObjects*(cg: ThinltoCodeGenT): cuint {.
    importc: "thinlto_module_get_num_objects".}
## *
##  Returns a reference to the ith object file produced by the ThinLTO
##  CodeGenerator.
##
##  Client should use \p thinlto_module_get_num_objects() to get the number of
##  available objects.
##
##  \since LTO_API_VERSION=18
##

proc thinltoModuleGetObject*(cg: ThinltoCodeGenT; index: cuint): LTOObjectBuffer {.
    importc: "thinlto_module_get_object".}
## *
##  Returns the number of object files produced by the ThinLTO CodeGenerator.
##
##  It usually matches the number of input files, but this is not a guarantee of
##  the API and may change in future implementation, so the client should not
##  assume it.
##
##  \since LTO_API_VERSION=21
##

proc thinltoModuleGetNumObjectFiles*(cg: ThinltoCodeGenT): cuint {.
    importc: "thinlto_module_get_num_object_files".}
## *
##  Returns the path to the ith object file produced by the ThinLTO
##  CodeGenerator.
##
##  Client should use \p thinlto_module_get_num_object_files() to get the number
##  of available objects.
##
##  \since LTO_API_VERSION=21
##

proc thinltoModuleGetObjectFile*(cg: ThinltoCodeGenT; index: cuint): cstring {.
    importc: "thinlto_module_get_object_file".}
## *
##  Sets which PIC code model to generate.
##  Returns true on error (check lto_get_error_message() for details).
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenSetPicModel*(cg: ThinltoCodeGenT; a2: LtoCodegenModel): LtoBoolT {.
    importc: "thinlto_codegen_set_pic_model".}
## *
##  Sets the path to a directory to use as a storage for temporary bitcode files.
##  The intention is to make the bitcode files available for debugging at various
##  stage of the pipeline.
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenSetSavetempsDir*(cg: ThinltoCodeGenT; saveTempsDir: cstring) {.
    importc: "thinlto_codegen_set_savetemps_dir".}
## *
##  Set the path to a directory where to save generated object files. This
##  path can be used by a linker to request on-disk files instead of in-memory
##  buffers. When set, results are available through
##  thinlto_module_get_object_file() instead of thinlto_module_get_object().
##
##  \since LTO_API_VERSION=21
##

proc thinltoSetGeneratedObjectsDir*(cg: ThinltoCodeGenT; saveTempsDir: cstring) {.
    importc: "thinlto_set_generated_objects_dir".}
## *
##  Sets the cpu to generate code for.
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenSetCpu*(cg: ThinltoCodeGenT; cpu: cstring) {.
    importc: "thinlto_codegen_set_cpu".}
## *
##  Disable CodeGen, only run the stages till codegen and stop. The output will
##  be bitcode.
##
##  \since LTO_API_VERSION=19
##

proc thinltoCodegenDisableCodegen*(cg: ThinltoCodeGenT; disable: LtoBoolT) {.
    importc: "thinlto_codegen_disable_codegen".}
## *
##  Perform CodeGen only: disable all other stages.
##
##  \since LTO_API_VERSION=19
##

proc thinltoCodegenSetCodegenOnly*(cg: ThinltoCodeGenT; codegenOnly: LtoBoolT) {.
    importc: "thinlto_codegen_set_codegen_only".}
## *
##  Parse -mllvm style debug options.
##
##  \since LTO_API_VERSION=18
##

proc thinltoDebugOptions*(options: cstringArray; number: cint) {.
    importc: "thinlto_debug_options".}
## *
##  Test if a module has support for ThinLTO linking.
##
##  \since LTO_API_VERSION=18
##

proc ltoModuleIsThinlto*(`mod`: LtoModuleT): LtoBoolT {.
    importc: "lto_module_is_thinlto".}
## *
##  Adds a symbol to the list of global symbols that must exist in the final
##  generated code. If a function is not listed there, it might be inlined into
##  every usage and optimized away. For every single module, the functions
##  referenced from code outside of the ThinLTO modules need to be added here.
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenAddMustPreserveSymbol*(cg: ThinltoCodeGenT; name: cstring;
    length: cint) {.importc: "thinlto_codegen_add_must_preserve_symbol",
                  .}
## *
##  Adds a symbol to the list of global symbols that are cross-referenced between
##  ThinLTO files. If the ThinLTO CodeGenerator can ensure that every
##  references from a ThinLTO module to this symbol is optimized away, then
##  the symbol can be discarded.
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenAddCrossReferencedSymbol*(cg: ThinltoCodeGenT; name: cstring;
    length: cint) {.importc: "thinlto_codegen_add_cross_referenced_symbol",
                  .}
## *
##  @} // endgoup LLVMCTLTO
##  @defgroup LLVMCTLTO_CACHING ThinLTO Cache Control
##  @ingroup LLVMCTLTO
##
##  These entry points control the ThinLTO cache. The cache is intended to
##  support incremental builds, and thus needs to be persistent across builds.
##  The client enables the cache by supplying a path to an existing directory.
##  The code generator will use this to store objects files that may be reused
##  during a subsequent build.
##  To avoid filling the disk space, a few knobs are provided:
##   - The pruning interval limits the frequency at which the garbage collector
##     will try to scan the cache directory to prune expired entries.
##     Setting to a negative number disables the pruning.
##   - The pruning expiration time indicates to the garbage collector how old an
##     entry needs to be to be removed.
##   - Finally, the garbage collector can be instructed to prune the cache until
##     the occupied space goes below a threshold.
##  @{
##
## *
##  Sets the path to a directory to use as a cache storage for incremental build.
##  Setting this activates caching.
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenSetCacheDir*(cg: ThinltoCodeGenT; cacheDir: cstring) {.
    importc: "thinlto_codegen_set_cache_dir".}
## *
##  Sets the cache pruning interval (in seconds). A negative value disables the
##  pruning. An unspecified default value will be applied, and a value of 0 will
##  force prunning to occur.
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenSetCachePruningInterval*(cg: ThinltoCodeGenT; interval: cint) {.
    importc: "thinlto_codegen_set_cache_pruning_interval".}
## *
##  Sets the maximum cache size that can be persistent across build, in terms of
##  percentage of the available space on the disk. Set to 100 to indicate
##  no limit, 50 to indicate that the cache size will not be left over half the
##  available space. A value over 100 will be reduced to 100, a value of 0 will
##  be ignored. An unspecified default value will be applied.
##
##  The formula looks like:
##   AvailableSpace = FreeSpace + ExistingCacheSize
##   NewCacheSize = AvailableSpace * P/100
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenSetFinalCacheSizeRelativeToAvailableSpace*(
    cg: ThinltoCodeGenT; percentage: cuint) {.importc: "thinlto_codegen_set_final_cache_size_relative_to_available_space",
    .}
## *
##  Sets the expiration (in seconds) for an entry in the cache. An unspecified
##  default value will be applied. A value of 0 will be ignored.
##
##  \since LTO_API_VERSION=18
##

proc thinltoCodegenSetCacheEntryExpiration*(cg: ThinltoCodeGenT; expiration: cuint) {.
    importc: "thinlto_codegen_set_cache_entry_expiration".}
## *
##  Sets the maximum size of the cache directory (in bytes). A value over the
##  amount of available space on the disk will be reduced to the amount of
##  available space. An unspecified default value will be applied. A value of 0
##  will be ignored.
##
##  \since LTO_API_VERSION=22
##

proc thinltoCodegenSetCacheSizeBytes*(cg: ThinltoCodeGenT; maxSizeBytes: cuint) {.
    importc: "thinlto_codegen_set_cache_size_bytes".}
## *
##  Same as thinlto_codegen_set_cache_size_bytes, except the maximum size is in
##  megabytes (2^20 bytes).
##
##  \since LTO_API_VERSION=23
##

proc thinltoCodegenSetCacheSizeMegabytes*(cg: ThinltoCodeGenT;
    maxSizeMegabytes: cuint) {.importc: "thinlto_codegen_set_cache_size_megabytes",
                             .}
## *
##  Sets the maximum number of files in the cache directory. An unspecified
##  default value will be applied. A value of 0 will be ignored.
##
##  \since LTO_API_VERSION=22
##

proc thinltoCodegenSetCacheSizeFiles*(cg: ThinltoCodeGenT; maxSizeFiles: cuint) {.
    importc: "thinlto_codegen_set_cache_size_files".}
## *
##  @} // endgroup LLVMCTLTO_CACHING
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END #  LLVM_C_LTO_H [NewLine]
## Error: expected ';'!!!
