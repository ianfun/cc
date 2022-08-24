## ===-- llvm-c/Disassembler.h - Disassembler Public C Interface ---*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header provides a public interface to a disassembler library.         *|
## |* LLVM provides an implementation of this interface.                         *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_DISASSEMBLER_H [NewLine] # LLVM_C_DISASSEMBLER_H [NewLine] # llvm-c/DisassemblerTypes.h [NewLine] # llvm-c/ExternC.h [NewLine] *
##  @defgroup LLVMCDisassembler Disassembler
##  @ingroup LLVMC
##
##  @{
##  LLVM_C_EXTERN_C_BEGIN *
##  Create a disassembler for the TripleName.  Symbolic disassembly is supported
##  by passing a block of information in the DisInfo parameter and specifying the
##  TagType and callback functions as described above.  These can all be passed
##  as NULL.  If successful, this returns a disassembler context.  If not, it
##  returns NULL. This function is equivalent to calling
##  LLVMCreateDisasmCPUFeatures() with an empty CPU name and feature set.
##  LLVMDisasmContextRef LLVMCreateDisasm ( const char * TripleName , void * DisInfo , int TagType , LLVMOpInfoCallback GetOpInfo , LLVMSymbolLookupCallback SymbolLookUp ) ;
## Error: expected ';'!!!

## *
##  Create a disassembler for the TripleName and a specific CPU.  Symbolic
##  disassembly is supported by passing a block of information in the DisInfo
##  parameter and specifying the TagType and callback functions as described
##  above.  These can all be passed * as NULL.  If successful, this returns a
##  disassembler context.  If not, it returns NULL. This function is equivalent
##  to calling LLVMCreateDisasmCPUFeatures() with an empty feature set.
##

proc createDisasmCPU*(triple: cstring; cpu: cstring; disInfo: pointer; tagType: cint;
                     getOpInfo: OpInfoCallback; symbolLookUp: SymbolLookupCallback): DisasmContextRef {.
    importc: "LLVMCreateDisasmCPU".}
## *
##  Create a disassembler for the TripleName, a specific CPU and specific feature
##  string.  Symbolic disassembly is supported by passing a block of information
##  in the DisInfo parameter and specifying the TagType and callback functions as
##  described above.  These can all be passed * as NULL.  If successful, this
##  returns a disassembler context.  If not, it returns NULL.
##

proc createDisasmCPUFeatures*(triple: cstring; cpu: cstring; features: cstring;
                             disInfo: pointer; tagType: cint;
                             getOpInfo: OpInfoCallback;
                             symbolLookUp: SymbolLookupCallback): DisasmContextRef {.
    importc: "LLVMCreateDisasmCPUFeatures".}
## *
##  Set the disassembler's options.  Returns 1 if it can set the Options and 0
##  otherwise.
##

proc setDisasmOptions*(dc: DisasmContextRef; options: uint64T): cint {.
    importc: "LLVMSetDisasmOptions".}
##  The option to produce marked up assembly.

const
  DisassemblerOptionUseMarkup* = 1

##  The option to print immediates as hex.

const
  DisassemblerOptionPrintImmHex* = 2

##  The option use the other assembler printer variant

const
  DisassemblerOptionAsmPrinterVariant* = 4

##  The option to set comment on instructions

const
  DisassemblerOptionSetInstrComments* = 8

##  The option to print latency information alongside instructions

const
  DisassemblerOptionPrintLatency* = 16

## *
##  Dispose of a disassembler context.
##

proc disasmDispose*(dc: DisasmContextRef) {.importc: "LLVMDisasmDispose".}
## *
##  Disassemble a single instruction using the disassembler context specified in
##  the parameter DC.  The bytes of the instruction are specified in the
##  parameter Bytes, and contains at least BytesSize number of bytes.  The
##  instruction is at the address specified by the PC parameter.  If a valid
##  instruction can be disassembled, its string is returned indirectly in
##  OutString whose size is specified in the parameter OutStringSize.  This
##  function returns the number of bytes in the instruction or zero if there was
##  no valid instruction.
##

proc disasmInstruction*(dc: DisasmContextRef; bytes: ptr uint8T; bytesSize: uint64T;
                       pc: uint64T; outString: cstring; outStringSize: csize_t): csize_t {.
    importc: "LLVMDisasmInstruction".}
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END #  LLVM_C_DISASSEMBLER_H [NewLine]
## Error: expected ';'!!!
