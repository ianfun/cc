/*
g++ llvm.c -I/usr/lib/llvm-10/include -fno-exceptions -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -L/usr/lib/llvm-10/lib -lLLVM-10
*/
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>

#include <stdio.h>
#include <stdlib.h>

int main() {
    LLVMInitializeAllTargets();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllTargetInfos();
    LLVMPassRegistryRef passreg = LLVMGetGlobalPassRegistry();
    LLVMInitializeCore(passreg);
    
    LLVMModuleRef mod = LLVMModuleCreateWithName("my_module");
    LLVMTypeRef param_types[] = { LLVMInt32Type(), LLVMInt32Type() };
    LLVMTypeRef ret_type = LLVMFunctionType(LLVMInt32Type(), param_types, 2, 0);

    LLVMValueRef sum = LLVMAddFunction(mod, "sum", ret_type);
    LLVMBasicBlockRef entry = LLVMAppendBasicBlock(sum, "entry");
    LLVMBuilderRef builder = LLVMCreateBuilder();
    LLVMPositionBuilderAtEnd(builder, entry);
    LLVMValueRef tmp = LLVMBuildAdd(builder, LLVMGetParam(sum, 0), LLVMGetParam(sum, 1), "tmp");
    LLVMBuildRet(builder, tmp);

    LLVMValueRef mainf = LLVMAddFunction(mod, "main", ret_type);
    LLVMBasicBlockRef entry2 = LLVMAppendBasicBlock(mainf, "entry");
    LLVMPositionBuilderAtEnd(builder, entry2);
    LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 0, 0));

    char *error = NULL;
    LLVMVerifyModule(mod, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);
    if (LLVMWriteBitcodeToFile(mod, "sum.bc") != 0) {
        fprintf(stderr, "error writing bitcode to file, skipping\n");
    }
    LLVMDisposeBuilder(builder);
}
