/*
llvmAPI.cpp - helper functions

Compile in Windows: 
$ clang++ -std=c++14 -fno-exceptions -fno-rtti -D_FILE_OFFSET_BITS=64 -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS llvmAPI.cpp -c -o llvmAPI.o -O3
Compile in Linux: 
$ g++ `llvm-config --cxxflags` llvmAPI.cpp -c -o llvmAPI.o -O3

*/

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include <string>
#include <cstdint>

#include <llvm-c/Core.h>
#include <llvm-c/DebugInfo.h>
#include <llvm-c/TargetMachine.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/Host.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/Metadata.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Support/Casting.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

using namespace llvm;


constexpr const uint32_t 
  FNone = 0,
  FMinGW = 1,
  F32Bit = 2,
  F64Bit = 4;

static inline void myInitTarget(){
	InitializeNativeTarget(); // initialize Target, MC and Info
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();
}
static inline void myInitAllTargets(){
#ifdef _SMALL
	LLVMInitializeARMTargetInfo();
	LLVMInitializeX86TargetInfo();

	LLVMInitializeX86Target();
	LLVMInitializeARMTarget();

	LLVMInitializeX86AsmPrinter();
	LLVMInitializeARMAsmPrinter();

	LLVMInitializeX86AsmParser();
	LLVMInitializeARMAsmParser();

	LLVMInitializeX86TargetMC();
	LLVMInitializeARMTargetMC();
#else
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmParsers();
	InitializeAllAsmPrinters();
#endif
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
	// this is needed for some basic block may have mutiple terminators(and you may see llvm says `error: label expected to be numbered 'xx'`)
	// remove all code after first terminator
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
int LLVMNimGetArch(Triple* t){
	return static_cast<int>(t->getArch());
}
int LLVMNimGetOS(Triple* t){
	return static_cast<int>(t->getOS());
}
int LLVMNimGetEnv(Triple* t){
	return static_cast<int>(t->getEnvironment());
}
LLVMBool LLVMNimisArch32Bit(Triple* t){
	return static_cast<LLVMBool>(t->isArch32Bit());
}
LLVMBool LLVMNimisArch64Bit(Triple* t){
	return static_cast<LLVMBool>(t->isArch64Bit());
}
const char* LLVMNimGetArchName(Triple* t){
	/*
	  the lifetime of triple arch name is store in Triple's std::string, so we can return it
	*/
	return t->getArchName().begin();
}
char* LLVMNimConfigureTarget(const char* tripleStr, LLVMTargetRef *Target, LLVMTargetMachineRef *Machine, LLVMTargetDataRef *TD, Triple** theTriple, uint32_t* f){
	if (tripleStr == NULL)
	{
		myInitAllTargets();
		return NULL;
	}
	std::string Error;
	std::string triple {tripleStr};
	if (*f){
		myInitTarget();
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
	*theTriple = new Triple(Twine(triple));
	*f = FNone;
	if ((*theTriple)->isArch64Bit())
	{
		*f |= F64Bit;
	}
	else if ((*theTriple)->isArch32Bit())
	{
		*f |= F32Bit;
	}
	if ((*theTriple)->isOSCygMing()){
		*f |= FMinGW;
	}
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
LLVMMetadataRef LLVMNimdIBuilderGetOrCreateSubrange(LLVMDIBuilderRef Builder, LLVMMetadataRef count){
	return wrap(unwrap(Builder)->getOrCreateSubrange(0, reinterpret_cast<Metadata*>(count)));
}
void LLVMNimGlobalAddDebugInfo(LLVMValueRef Global, LLVMMetadataRef GVE){
	GlobalVariable *G = unwrap<GlobalVariable>(Global);
	G->addDebugInfo(reinterpret_cast<DIGlobalVariableExpression*>(GVE));
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
void LLVMNimAddLabel(LLVMDIBuilderRef Builder, LLVMMetadataRef Scope, LLVMBasicBlockRef BB, const char* name, size_t nameLen, LLVMMetadataRef file, unsigned lineno, LLVMMetadataRef loc){
	auto DIBuilder = unwrap(Builder);
	DILabel *LabelInfo = DIBuilder->createLabel(reinterpret_cast<DIScope*>(Scope), StringRef(name, nameLen), reinterpret_cast<DIFile*>(file), lineno);
	DIBuilder->insertLabel(LabelInfo, reinterpret_cast<DILocation*>(loc), reinterpret_cast<BasicBlock*>(BB));
}
unsigned LLVMNimGetIntrinsicForMSBuiltin(const char* Prefix, const char* BuiltinName){
	return Intrinsic::getIntrinsicForMSBuiltin(Prefix, StringRef(BuiltinName));
}
unsigned LLVMNimGetIntrinsicForClangBuiltin(const char* Prefix, const char* BuiltinName){
	return Intrinsic::getIntrinsicForClangBuiltin(Prefix, StringRef(BuiltinName));
}
}
