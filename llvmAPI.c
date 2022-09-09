/*
llvmAPI.c - helper functions
*/
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include <llvm-c/Types.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/TargetMachine.h>

void LLVMNimOptFunction(LLVMValueRef f){
	LLVMBasicBlockRef bb = LLVMGetFirstBasicBlock(f);
	for(;bb;){
	LLVMValueRef Inst = LLVMGetFirstInstruction(bb);
		_Bool next = 0;
		for(;Inst;){
			if(next == 0){
			LLVMValueRef rm = LLVMIsATerminatorInst(Inst);
				if(rm)
				{
					next = 1;
				}
				Inst = LLVMGetNextInstruction(Inst);
			}else{
				LLVMValueRef n = LLVMGetNextInstruction(Inst);
				LLVMInstructionEraseFromParent(Inst);
				Inst = n;
			}
		}
		bb = LLVMGetNextBasicBlock(bb);
	}
}
extern void LLVMNimOptModule(LLVMModuleRef M){
	for(LLVMValueRef f = LLVMGetFirstFunction(M);f;f=LLVMGetNextFunction(f)){
		LLVMNimOptFunction(f);
	}
}

extern void LLVMNimInit(){
	//LLVMInitializeCore(LLVMGetGlobalPassRegistry());
	LLVMInitializeNativeTarget();
	LLVMInitializeNativeAsmPrinter();
	LLVMInitializeNativeAsmParser();
	//LLVMInitializeNativeDisassembler();
}
extern void LLVMNimFinalizeModule(LLVMModuleRef M, LLVMContextRef ctx){
	const char *idents = "cc: A C Compiler(https://ianfun.github.io/cc.git)";
	LLVMValueRef ident = LLVMMDStringInContext(ctx, idents, strlen(idents)); 
	LLVMAddNamedMetadataOperand(M, "llvm.ident", ident);
	LLVMTypeRef i32 = LLVMInt32TypeInContext(ctx);
	const char * wchars = "short_wchar";
	LLVMValueRef short_wchars[] = {LLVMConstInt(i32, 1, 0), LLVMMDStringInContext(ctx, wchars, strlen(wchars)), LLVMConstInt(i32, 1, 0)};
	LLVMValueRef short_wchar = LLVMMDNodeInContext(ctx, short_wchars, 3);
	const char * enums = "short_enum";
	LLVMValueRef short_enums[] = {LLVMConstInt(i32, 1, 0), LLVMMDStringInContext(ctx, enums, strlen(enums)), LLVMConstInt(i32, 1, 0)};
	LLVMValueRef short_enum = LLVMMDNodeInContext(ctx, short_enums, 3);
	LLVMValueRef flags[2] = {short_wchar, short_enum};
	LLVMAddNamedMetadataOperand(M, "llvm.module.flags", short_enum);
	LLVMAddNamedMetadataOperand(M, "llvm.module.flags", short_wchar);
}