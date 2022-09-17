/*
llvmAPI.cpp - helper functions
*/
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include <string>

#include <llvm-c/Core.h>
#include <llvm-c/TargetMachine.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/Host.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

using namespace llvm;

static inline void myInitTarget(){
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();
}
static inline void myInitAllTargets(){
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmParsers();
	InitializeAllAsmPrinters();
	InitializeAllTargetInfos();
}

/*
copied from https://llvm.org/doxygen/TargetMachineC_8cpp_source.html
*/
static TargetMachine *unwrap(LLVMTargetMachineRef P) {
  return reinterpret_cast<TargetMachine *>(P);
}
static Target *unwrap(LLVMTargetRef P) {
  return reinterpret_cast<Target*>(P);
}
static LLVMTargetMachineRef wrap(const TargetMachine *P) {
  return reinterpret_cast<LLVMTargetMachineRef>(const_cast<TargetMachine *>(P));
}
static LLVMTargetRef wrap(const Target * P) {
  return reinterpret_cast<LLVMTargetRef>(const_cast<Target*>(P));
}

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
char* LLVMNimConfigureTarget(const char* tripleStr, LLVMTargetRef *Target, LLVMTargetMachineRef *Machine, LLVMTargetDataRef *TD){
	std::string Error;
	std::string triple = tripleStr;
	if (triple.empty()){
		myInitTarget();
		triple = sys::getDefaultTargetTriple();
	}else{
		myInitAllTargets();
	}
	auto T = TargetRegistry::lookupTarget(triple, Error);
	if (!T)
	{
		return strdup(Error.c_str());
	}
	auto CPU = "generic";
	auto Features = "";

	TargetOptions opt;
	// TODO: opt
	auto TargetMachine = T->createTargetMachine(triple, CPU, Features, opt, Reloc::PIC_, None, CodeGenOpt::Aggressive);
	auto layout = new DataLayout(TargetMachine->createDataLayout());

	*Machine = wrap(TargetMachine);
	*TD = wrap(layout);
	*Target = wrap(T);

	return NULL;
}
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
}
unsigned LLVMNimGetIntrinsicForMSBuiltin(const char* Prefix, const char* BuiltinName){
	return Intrinsic::getIntrinsicForMSBuiltin(Prefix, StringRef(BuiltinName));
}
unsigned LLVMNimGetIntrinsicForClangBuiltin(const char* Prefix, const char* BuiltinName){
	return Intrinsic::getIntrinsicForClangBuiltin(Prefix, StringRef(BuiltinName));
}
}
