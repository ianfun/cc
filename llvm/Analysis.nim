## ===-- llvm-c/Analysis.h - Analysis Library C Interface --------*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMAnalysis.a, which           *|
## |* implements various analyses of the LLVM IR.                                *|
## |*                                                                            *|
## |* Many exotic languages can interoperate with C code but have a harder time  *|
## |* with C++ due to name mangling. So in addition to C, this interface enables *|
## |* tools written in such languages.                                           *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_ANALYSIS_H [NewLine] # LLVM_C_ANALYSIS_H [NewLine] # llvm-c/ExternC.h [NewLine] # llvm-c/Types.h [NewLine] LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMCAnalysis Analysis
##  @ingroup LLVMC
##
##  @{
##  typedef enum { LLVMAbortProcessAction ,  verifier will print to stderr and abort() LLVMPrintMessageAction ,  verifier will print to stderr and return 1 LLVMReturnStatusAction  verifier will just return 1 } LLVMVerifierFailureAction ;
## Error: expected ';'!!!

##  Verifies that a module is valid, taking the specified action if not.
##    Optionally returns a human-readable description of any invalid constructs.
##    OutMessage must be disposed with LLVMDisposeMessage.

proc verifyModule*(m: ModuleRef; action: VerifierFailureAction;
                  outMessage: cstringArray): Bool {.importc: "LLVMVerifyModule".}
##  Verifies that a single function is valid, taking the specified action. Useful
##    for debugging.

proc verifyFunction*(fn: ValueRef; action: VerifierFailureAction): Bool {.
    importc: "LLVMVerifyFunction".}
##  Open up a ghostview window that displays the CFG of the current function.
##    Useful for debugging.

proc viewFunctionCFG*(fn: ValueRef) {.importc: "LLVMViewFunctionCFG".}
proc viewFunctionCFGOnly*(fn: ValueRef) {.importc: "LLVMViewFunctionCFGOnly".}
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END # [NewLine]
## Error: expected ';'!!!
