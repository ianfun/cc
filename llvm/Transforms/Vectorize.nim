## ===---------------------------Vectorize.h --------------------- -*- C -*-===*\
## |*===----------- Vectorization Transformation Library C Interface ---------===*|
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMVectorize.a, which          *|
## |* implements various vectorization transformations of the LLVM IR.           *|
## |*                                                                            *|
## |* Many exotic languages can interoperate with C code but have a harder time  *|
## |* with C++ due to name mangling. So in addition to C, this interface enables *|
## |* tools written in such languages.                                           *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_TRANSFORMS_VECTORIZE_H [NewLine] # LLVM_C_TRANSFORMS_VECTORIZE_H [NewLine] # llvm-c/ExternC.h [NewLine] # llvm-c/Types.h [NewLine] LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMCTransformsVectorize Vectorization transformations
##  @ingroup LLVMCTransforms
##
##  @{
##  * See llvm::createLoopVectorizePass function. void LLVMAddLoopVectorizePass ( LLVMPassManagerRef PM ) ;
## Error: expected ';'!!!

## * See llvm::createSLPVectorizerPass function.

proc addSLPVectorizePass*(pm: PassManagerRef) {.importc: "LLVMAddSLPVectorizePass",
    .}
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END # [NewLine]
## Error: expected ';'!!!
