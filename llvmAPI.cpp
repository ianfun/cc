/*

llvmAPI.cpp - C++ helper API

function for some specific API has no LLVM-C API provided in LLVM libary

*/

#include "llvm-c/Core.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/TargetSelect.h"

using namespace llvm;

extern "C" {

LLVMBool LLVMConstIntIsZero(LLVMValueRef ConstantVal){
	return unwrap<ConstantInt>(ConstantVal)->isZero();
}

}