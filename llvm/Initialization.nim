## ===-- llvm-c/Initialization.h - Initialization C Interface ------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to LLVM initialization routines,      *|
## |* which must be called before you can use the functionality provided by      *|
## |* the corresponding LLVM library.                                            *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_INITIALIZATION_H [NewLine] # LLVM_C_INITIALIZATION_H [NewLine] # llvm-c/ExternC.h [NewLine] # llvm-c/Types.h [NewLine] LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMCInitialization Initialization Routines
##  @ingroup LLVMC
##
##  This module contains routines used to initialize the LLVM system.
##
##  @{
##  void LLVMInitializeCore ( LLVMPassRegistryRef R ) ;
## Error: expected ';'!!!

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
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END # [NewLine]
## Error: expected ';'!!!
