## ===-- llvm-c/Object.h - Object Lib C Iface --------------------*- C++ -*-===
##
##  Part of the LLVM Project, under the Apache License v2.0 with LLVM
##  Exceptions.
##  See https://llvm.org/LICENSE.txt for license information.
##  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
##
## ===----------------------------------------------------------------------===
##
##  This header declares the C interface to libLLVMObject.a, which
##  implements object file reading and writing.
##
##  Many exotic languages can interoperate with C code but have a harder time
##  with C++ due to name mangling. So in addition to C, this interface enables
##  tools written in such languages.
##
## ===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_OBJECT_H [NewLine] # LLVM_C_OBJECT_H [NewLine] # llvm-c/ExternC.h [NewLine] # llvm-c/Types.h [NewLine] # llvm/Config/llvm-config.h [NewLine] LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMCObject Object file reading and writing
##  @ingroup LLVMC
##
##  @{
##   Opaque type wrappers typedef struct LLVMOpaqueSectionIterator * LLVMSectionIteratorRef ;
## Error: expected ';'!!!

type
  SymbolIteratorRef* = ptr opaqueSymbolIterator
  RelocationIteratorRef* = ptr opaqueRelocationIterator
  BinaryType* {.size: sizeof(cint).} = enum
    BinaryTypeArchive,        ## *< Archive file.
    BinaryTypeMachOUniversalBinary, ## *< Mach-O Universal Binary file.
    BinaryTypeCOFFImportFile, ## *< COFF Import file.
    BinaryTypeIR,             ## *< LLVM IR.
    BinaryTypeWinRes,         ## *< Windows resource (.res) file.
    BinaryTypeCOFF,           ## *< COFF Object file.
    BinaryTypeELF32L,         ## *< ELF 32-bit, little endian.
    BinaryTypeELF32B,         ## *< ELF 32-bit, big endian.
    BinaryTypeELF64L,         ## *< ELF 64-bit, little endian.
    BinaryTypeELF64B,         ## *< ELF 64-bit, big endian.
    BinaryTypeMachO32L,       ## *< MachO 32-bit, little endian.
    BinaryTypeMachO32B,       ## *< MachO 32-bit, big endian.
    BinaryTypeMachO64L,       ## *< MachO 64-bit, little endian.
    BinaryTypeMachO64B,       ## *< MachO 64-bit, big endian.
    BinaryTypeWasm,           ## *< Web Assembly.
    BinaryTypeOffload         ## *< Offloading fatbinary.


## *
##  Create a binary file from the given memory buffer.
##
##  The exact type of the binary file will be inferred automatically, and the
##  appropriate implementation selected.  The context may be NULL except if
##  the resulting file is an LLVM IR file.
##
##  The memory buffer is not consumed by this function.  It is the responsibilty
##  of the caller to free it with \c LLVMDisposeMemoryBuffer.
##
##  If NULL is returned, the \p ErrorMessage parameter is populated with the
##  error's description.  It is then the caller's responsibility to free this
##  message by calling \c LLVMDisposeMessage.
##
##  @see llvm::object::createBinary
##

proc createBinary*(memBuf: MemoryBufferRef; context: ContextRef;
                  errorMessage: cstringArray): BinaryRef {.
    importc: "LLVMCreateBinary".}
## *
##  Dispose of a binary file.
##
##  The binary file does not own its backing buffer.  It is the responsibilty
##  of the caller to free it with \c LLVMDisposeMemoryBuffer.
##

proc disposeBinary*(br: BinaryRef) {.importc: "LLVMDisposeBinary".}
## *
##  Retrieves a copy of the memory buffer associated with this object file.
##
##  The returned buffer is merely a shallow copy and does not own the actual
##  backing buffer of the binary. Nevertheless, it is the responsibility of the
##  caller to free it with \c LLVMDisposeMemoryBuffer.
##
##  @see llvm::object::getMemoryBufferRef
##

proc binaryCopyMemoryBuffer*(br: BinaryRef): MemoryBufferRef {.
    importc: "LLVMBinaryCopyMemoryBuffer".}
## *
##  Retrieve the specific type of a binary.
##
##  @see llvm::object::Binary::getType
##

proc binaryGetType*(br: BinaryRef): BinaryType {.importc: "LLVMBinaryGetType",
    .}
##
##  For a Mach-O universal binary file, retrieves the object file corresponding
##  to the given architecture if it is present as a slice.
##
##  If NULL is returned, the \p ErrorMessage parameter is populated with the
##  error's description.  It is then the caller's responsibility to free this
##  message by calling \c LLVMDisposeMessage.
##
##  It is the responsiblity of the caller to free the returned object file by
##  calling \c LLVMDisposeBinary.
##

proc machOUniversalBinaryCopyObjectForArch*(br: BinaryRef; arch: cstring;
    archLen: csize_t; errorMessage: cstringArray): BinaryRef {.
    importc: "LLVMMachOUniversalBinaryCopyObjectForArch".}
## *
##  Retrieve a copy of the section iterator for this object file.
##
##  If there are no sections, the result is NULL.
##
##  The returned iterator is merely a shallow copy. Nevertheless, it is
##  the responsibility of the caller to free it with
##  \c LLVMDisposeSectionIterator.
##
##  @see llvm::object::sections()
##

proc objectFileCopySectionIterator*(br: BinaryRef): SectionIteratorRef {.
    importc: "LLVMObjectFileCopySectionIterator".}
## *
##  Returns whether the given section iterator is at the end.
##
##  @see llvm::object::section_end
##

proc objectFileIsSectionIteratorAtEnd*(br: BinaryRef; si: SectionIteratorRef): Bool {.
    importc: "LLVMObjectFileIsSectionIteratorAtEnd".}
## *
##  Retrieve a copy of the symbol iterator for this object file.
##
##  If there are no symbols, the result is NULL.
##
##  The returned iterator is merely a shallow copy. Nevertheless, it is
##  the responsibility of the caller to free it with
##  \c LLVMDisposeSymbolIterator.
##
##  @see llvm::object::symbols()
##

proc objectFileCopySymbolIterator*(br: BinaryRef): SymbolIteratorRef {.
    importc: "LLVMObjectFileCopySymbolIterator".}
## *
##  Returns whether the given symbol iterator is at the end.
##
##  @see llvm::object::symbol_end
##

proc objectFileIsSymbolIteratorAtEnd*(br: BinaryRef; si: SymbolIteratorRef): Bool {.
    importc: "LLVMObjectFileIsSymbolIteratorAtEnd".}
proc disposeSectionIterator*(si: SectionIteratorRef) {.
    importc: "LLVMDisposeSectionIterator".}
proc moveToNextSection*(si: SectionIteratorRef) {.importc: "LLVMMoveToNextSection",
    .}
proc moveToContainingSection*(sect: SectionIteratorRef; sym: SymbolIteratorRef) {.
    importc: "LLVMMoveToContainingSection".}
##  ObjectFile Symbol iterators

proc disposeSymbolIterator*(si: SymbolIteratorRef) {.
    importc: "LLVMDisposeSymbolIterator".}
proc moveToNextSymbol*(si: SymbolIteratorRef) {.importc: "LLVMMoveToNextSymbol",
    .}
##  SectionRef accessors

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
##  Section Relocation iterators

proc getRelocations*(section: SectionIteratorRef): RelocationIteratorRef {.
    importc: "LLVMGetRelocations".}
proc disposeRelocationIterator*(ri: RelocationIteratorRef) {.
    importc: "LLVMDisposeRelocationIterator".}
proc isRelocationIteratorAtEnd*(section: SectionIteratorRef;
                               ri: RelocationIteratorRef): Bool {.
    importc: "LLVMIsRelocationIteratorAtEnd".}
proc moveToNextRelocation*(ri: RelocationIteratorRef) {.
    importc: "LLVMMoveToNextRelocation".}
##  SymbolRef accessors

proc getSymbolName*(si: SymbolIteratorRef): cstring {.importc: "LLVMGetSymbolName",
    .}
proc getSymbolAddress*(si: SymbolIteratorRef): uint64T {.
    importc: "LLVMGetSymbolAddress".}
proc getSymbolSize*(si: SymbolIteratorRef): uint64T {.importc: "LLVMGetSymbolSize",
    .}
##  RelocationRef accessors

proc getRelocationOffset*(ri: RelocationIteratorRef): uint64T {.
    importc: "LLVMGetRelocationOffset".}
proc getRelocationSymbol*(ri: RelocationIteratorRef): SymbolIteratorRef {.
    importc: "LLVMGetRelocationSymbol".}
proc getRelocationType*(ri: RelocationIteratorRef): uint64T {.
    importc: "LLVMGetRelocationType".}
##  NOTE: Caller takes ownership of returned string of the two
##  following functions.

proc getRelocationTypeName*(ri: RelocationIteratorRef): cstring {.
    importc: "LLVMGetRelocationTypeName".}
proc getRelocationValueString*(ri: RelocationIteratorRef): cstring {.
    importc: "LLVMGetRelocationValueString".}
## * Deprecated: Use LLVMBinaryRef instead.

type
  ObjectFileRef* = ptr opaqueObjectFile

## * Deprecated: Use LLVMCreateBinary instead.

proc createObjectFile*(memBuf: MemoryBufferRef): ObjectFileRef {.
    importc: "LLVMCreateObjectFile".}
## * Deprecated: Use LLVMDisposeBinary instead.

proc disposeObjectFile*(objectFile: ObjectFileRef) {.
    importc: "LLVMDisposeObjectFile".}
## * Deprecated: Use LLVMObjectFileCopySectionIterator instead.

proc getSections*(objectFile: ObjectFileRef): SectionIteratorRef {.
    importc: "LLVMGetSections".}
## * Deprecated: Use LLVMObjectFileIsSectionIteratorAtEnd instead.

proc isSectionIteratorAtEnd*(objectFile: ObjectFileRef; si: SectionIteratorRef): Bool {.
    importc: "LLVMIsSectionIteratorAtEnd".}
## * Deprecated: Use LLVMObjectFileCopySymbolIterator instead.

proc getSymbols*(objectFile: ObjectFileRef): SymbolIteratorRef {.
    importc: "LLVMGetSymbols".}
## * Deprecated: Use LLVMObjectFileIsSymbolIteratorAtEnd instead.

proc isSymbolIteratorAtEnd*(objectFile: ObjectFileRef; si: SymbolIteratorRef): Bool {.
    importc: "LLVMIsSymbolIteratorAtEnd".}
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END # [NewLine]
## Error: expected ';'!!!