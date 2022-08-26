struct Foo
{
  int d;
  char s;
};
struct Foo a;

/*#include "llvm-c/Core.h"
#include "llvm-c/Error.h"
#include "llvm-c/Initialization.h"
#include "llvm-c/LLJIT.h"
#include "llvm-c/Support.h"
#include "llvm-c/Target.h"
#include <stdio.h>

int main() {
  LLVMInitializeCore(LLVMGetGlobalPassRegistry());
  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMModuleRef M = LLVMModuleCreateWithName("demo");
  LLVMTypeRef ParamTypes[] = {};
  LLVMTypeRef SumFunctionType =
      LLVMFunctionType(LLVMInt32Type(), ParamTypes, 0, 0);
  LLVMValueRef SumFunction = LLVMAddFunction(M, "sum", SumFunctionType);
  LLVMBasicBlockRef EntryBB = LLVMAppendBasicBlock(SumFunction, "entry");
  LLVMBuilderRef Builder = LLVMCreateBuilder();
  LLVMPositionBuilderAtEnd(Builder, EntryBB);

  LLVMValueRef var = LLVMBuildAlloca(Builder, LLVMInt32Type(), "");


  LLVMValueRef Result = LLVMBuildLoad(Builder, var, "");

  LLVMBuildRet(Builder, Result);

  LLVMPrintModuleToFile(M, "main.ll", NULL);

  printf("%s\n", "OK!");
}*/

