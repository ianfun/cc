## ===-- llvm-c/Deprecated.h - Deprecation macro -------------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares LLVM_ATTRIBUTE_C_DEPRECATED() macro, which can be     *|
## |* used to deprecate functions in the C interface.                            *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_DEPRECATED_H [NewLine] # LLVM_C_DEPRECATED_H [NewLine] # __has_feature [NewLine] # __has_feature ( x ) 0 [NewLine] # [NewLine]  This is a variant of LLVM_ATTRIBUTE_DEPRECATED() that is compatible with
##  C compilers. # __has_feature ( attribute_deprecated_with_message ) [NewLine] # LLVM_ATTRIBUTE_C_DEPRECATED ( decl , message ) decl __attribute__ ( ( deprecated ( message ) ) ) [NewLine] # defined ( __GNUC__ ) [NewLine] # LLVM_ATTRIBUTE_C_DEPRECATED ( decl , message ) decl __attribute__ ( ( deprecated ) ) [NewLine] # defined ( _MSC_VER ) [NewLine] # LLVM_ATTRIBUTE_C_DEPRECATED ( decl , message ) __declspec ( deprecated ( message ) ) decl [NewLine] # [NewLine] # LLVM_ATTRIBUTE_C_DEPRECATED ( decl , message ) decl [NewLine] # [NewLine] #  LLVM_C_DEPRECATED_H [NewLine]
## Error: expected ';'!!!
