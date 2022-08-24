## ===-- Utils.h - Transformation Utils Library C Interface ------*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMTransformUtils.a, which     *|
## |* implements various transformation utilities of the LLVM IR.                *|
## |*                                                                            *|
## |* Many exotic languages can interoperate with C code but have a harder time  *|
## |* with C++ due to name mangling. So in addition to C, this interface enables *|
## |* tools written in such languages.                                           *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_TRANSFORMS_UTILS_H [NewLine] # LLVM_C_TRANSFORMS_UTILS_H [NewLine] # llvm-c/ExternC.h [NewLine] # llvm-c/Types.h [NewLine] LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMCTransformsUtils Transformation Utilities
##  @ingroup LLVMCTransforms
##
##  @{
##  * See llvm::createLowerSwitchPass function. void LLVMAddLowerSwitchPass ( LLVMPassManagerRef PM ) ;
## Error: expected ';'!!!

## * See llvm::createPromoteMemoryToRegisterPass function.

proc addPromoteMemoryToRegisterPass*(pm: PassManagerRef) {.
    importc: "LLVMAddPromoteMemoryToRegisterPass".}
## * See llvm::createAddDiscriminatorsPass function.

proc addAddDiscriminatorsPass*(pm: PassManagerRef) {.
    importc: "LLVMAddAddDiscriminatorsPass".}
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END # [NewLine]
## Error: expected ';'!!!
