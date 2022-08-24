## ===- llvm-c/ExternC.h - Wrapper for 'extern "C"' ----------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This file defines an 'extern "C"' wrapper                                  *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_EXTERNC_H [NewLine] # LLVM_C_EXTERNC_H [NewLine] # __clang__ [NewLine] # LLVM_C_STRICT_PROTOTYPES_BEGIN _Pragma ( clang diagnostic push ) _Pragma ( clang diagnostic error "-Wstrict-prototypes" ) [NewLine] # LLVM_C_STRICT_PROTOTYPES_END _Pragma ( clang diagnostic pop ) [NewLine] # [NewLine] # LLVM_C_STRICT_PROTOTYPES_BEGIN [NewLine] # LLVM_C_STRICT_PROTOTYPES_END [NewLine] # [NewLine] # __cplusplus [NewLine] # LLVM_C_EXTERN_C_BEGIN extern C { LLVM_C_STRICT_PROTOTYPES_BEGIN [NewLine] # LLVM_C_EXTERN_C_END LLVM_C_STRICT_PROTOTYPES_END } [NewLine] # [NewLine] # LLVM_C_EXTERN_C_BEGIN LLVM_C_STRICT_PROTOTYPES_BEGIN [NewLine] # LLVM_C_EXTERN_C_END LLVM_C_STRICT_PROTOTYPES_END [NewLine] # [NewLine] # [NewLine]
## Error: expected ';'!!!
