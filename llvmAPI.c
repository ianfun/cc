//#include "llvm-c/Core.h"
//#include "llvm/IR/Constants.h"
//#include "llvm/Support/TargetSelect.h"
//
//using namespace llvm;
//
//extern "C" {
//
//LLVMBool LLVMConstIntIsZero(LLVMValueRef ConstantVal){
//	return unwrap<ConstantInt>(ConstantVal)->isZero();
//}
//
//}
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
