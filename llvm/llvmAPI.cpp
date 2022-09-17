/*
llvmAPI.cpp - helper functions
*/
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include <llvm-c/Core.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/TargetMachine.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>

using namespace llvm;

static void opt(Function *f){
	for(Function::iterator bb = f->begin();bb != f->end();++bb){
		bool next = false;
		for(BasicBlock::iterator Inst = bb->begin();Inst != bb->end();){
			if(!next){
				next = Inst->isTerminator();
				++Inst;
			}else{
				(Inst++)->eraseFromParent();
			}
		}
	}
}
extern "C" {
void LLVMNimSetDSOLocal(LLVMValueRef Global){
	GlobalValue *GV = unwrap<GlobalValue>(Global);
	GV->setDSOLocal(true);
}
LLVMValueRef LLVMNimGetAllocaArraySize(LLVMValueRef Alloca){
	return wrap(unwrap<AllocaInst>(Alloca)->getArraySize());
}
void LLVMNimOptModule(LLVMModuleRef M){
	Module *Mod = unwrap(M);
	for(Module::iterator I = Mod->begin();I != Mod->end();++I){
		opt(& *I);
	}
	//for(LLVMValueRef f = LLVMGetFirstFunction(M);f;f=LLVMGetNextFunction(f)){
	//	LLVMNimOptFunction(f);
	//}
}
void LLVMNimInit(){
	//LLVMInitializeCore(LLVMGetGlobalPassRegistry());
	llvm::InitializeNativeTarget();
	llvm::InitializeNativeTargetAsmPrinter();
	llvm::InitializeNativeTargetAsmParser();
	//LLVMInitializeNativeDisassembler();
}
void LLVMNimInitAll(){
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();
	llvm::InitializeAllTargetInfos();
}
}
