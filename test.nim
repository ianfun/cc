import 
  llvm/llvm

initializeAllAsmPrinters()
initializeAllTargets()
initializeAllTargetInfos()
initializeAllTargetMCs()

var module = moduleCreateWithName("nim-gen")


var target: llvm.TargetRef
let tr = getDefaultTargetTriple()
  
discard getTargetFromTriple(tr, addr target, nil)
  
var machine = createTargetMachine(target, tr, "", "", CodeGenLevelDefault, RelocDefault, CodeModelDefault)
var layout = createTargetDataLayout(machine)
setModuleDataLayout(module, layout)
setTarget(module, tr)


var param_types = [int32Type(), int32Type()]
var ret_type = functionType(int32Type(), param_types)

var sum = addFunction(module, "sum", ret_type)
var entry = appendBasicBlock(sum, "entry")
var builder = createBuilder()
positionBuilderAtEnd(builder, entry)
var tmp = buildAdd(builder, getParam(sum, 0), getParam(sum, 1), "tmp")
var tmp2 = buildAdd(builder, tmp, tmp, "tmp2")
# var arr = constArray(int32Type(), addr tmp2, 1)
# setGlobalConstant(arr, llvm.True)
var tmp3 = buildAdd(builder, tmp, tmp2, "tmp3")
discard buildRet(builder, tmp3)

#var mt = structCreateNamed(getGlobalContext(), "mystruct")
#var ty = int16Type()
#structSetBody(mt, addr ty, 1, 0)

var ret_type2 = functionType(int32Type(), param_types)

var maxf = addFunction(module, "max", ret_type2)
var entry2 = appendBasicBlock(maxf, "entry")
positionBuilderAtEnd(builder, entry2)

var testexpr = getParam(maxf, 0)

var ifelseret = buildAlloca(builder, int32Type(), "if.ret")

var cond = buildICmp(builder, IntNE, constInt(int32Type(), 0, 1), testexpr, "cmd")

var iftrue = appendBasicBlock(maxf, "if.true")

var iffalse = appendBasicBlock(maxf, "if.false")

var ifend = appendBasicBlock(maxf, "if.end")

discard buildCondBr(builder, cond, iftrue, iffalse)

positionBuilderAtEnd(builder, iftrue)
var ifstmt = buildSub(builder, constInt(int32Type(), 1234, 1), constInt(int32Type(), 6789, 1), "if_stmt")
discard buildStore(builder, ifstmt, ifelseret)
discard buildBr(builder, ifend)

positionBuilderAtEnd(builder, iffalse)
var elsestmt = buildAdd(builder, constInt(int32Type(), 100, 1), constInt(int32Type(), 100, 1), "else_stmt")
discard buildStore(builder, elsestmt, ifelseret)
discard buildBr(builder, ifend)

positionBuilderAtEnd(builder, ifend)
discard buildRet(builder, buildLoad(builder, ifelseret, "load.ret"))

#var c = constInt(int16Type(), 10, 0)
#var s = constNamedStruct(mt, addr c, 1)
#discard buildRet(builder, s)

var puts = addFunction(module, "puts", functionType(int32Type(), [pointerType(int8Type())]))
setLinkage(puts, ExternalLinkage)

var mainf = addFunction(module, "main", functionType(int32Type(), [int32Type()]))
var entry3 = appendBasicBlock(mainf, "entry")
positionBuilderAtEnd(builder, entry3)

var arg = buildGlobalStringPtr(builder, "Hello world!", "hello.str")
var puts_ret = buildCall(builder, puts, addr arg, 1, "puts.ret")
discard buildRet(builder, puts_ret)

var err: cstring

discard verifyModule(module, PrintMessageAction, cast[cstringArray](addr err))

discard writeBitcodeToFile(module, "sum.bc")

discard printModuleToFile(module, "sum.ll", cast[cstringArray](addr err))
if err != nil:
  echo err
  err = nil
discard targetMachineEmitToFile(machine, module, "sum.o", ObjectFile, cast[cstringArray](addr err))
discard targetMachineEmitToFile(machine, module, "sum.S", AssemblyFile, cast[cstringArray](addr err))

disposeBuilder(builder)

echo "OK"
