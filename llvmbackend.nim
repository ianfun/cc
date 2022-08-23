import llvm/llvm
import token

type
  Value* = ValueRef
  Type = TypeRef

var
  module*: ModuleRef
  builder*: BuilderRef
  machine*: TargetMachineRef
  currentfunction*: Value

proc name(n: cstring): cstring = n

proc init_backend*() =
  initializeAllAsmPrinters()
  initializeAllTargets()
  initializeAllTargetInfos()
  initializeAllTargetMCs()
  module = moduleCreateWithName("main")
  var target: llvm.TargetRef
  let tr = getDefaultTargetTriple()
  discard getTargetFromTriple(tr, addr target, nil)
  var machine = createTargetMachine(target, tr, "", "", CodeGenLevelDefault, RelocDefault, CodeModelDefault)
  var layout = createTargetDataLayout(machine)
  setModuleDataLayout(module, layout)
  setTarget(module, tr)
  builder = createBuilder()

proc verify*() =
  var err: cstring
  discard verifyModule(module, PrintMessageAction, cast[cstringArray](addr err))

proc writeLLVMBitcodeToFile*(path: string) =
  discard writeBitcodeToFile(module, path)

proc writeModuleToFile*(path: string) =
  var err: cstring
  if printModuleToFile(module, path, cast[cstringArray](addr err)) != True:
    error($err)

proc writeObjectFile*(path: string) =
  var err: cstring
  if targetMachineEmitToFile(machine, module, path, ObjectFile, cast[cstringArray](addr err)) != True:
    error($err)

proc writeAssemblyFile*(path: string) =
  var err: cstring
  if targetMachineEmitToFile(machine, module, path, AssemblyFile, cast[cstringArray](addr err)) != True:
    error($err)

proc close_backend*() =
  disposeBuilder(builder)

proc gen_bool*(val: bool): Value =
  constInt(int1Type(), if val: 1 else: 0, False)

proc gen_int*(i: int, tags: ITag): Value =
  type C = culonglong
  case tags:
  of Iint:
    if TYINT == TYINT32:
      constInt(int32Type(), C(i), True)
    else:
      constInt(int64Type(), C(i), True)
  of Ilong:
    if TYLONG == TYINT32:
      constInt(int32Type(), C(i), True)
    else:
      constInt(int64Type(), C(i), True)
  of Iulonglong:
     constInt(int64Type(), C(i), False)
  of Ilonglong:
     constInt(int64Type(), C(i), True)
  of Iuint:
    if TYINT == TYINT32:
      constInt(int32Type(), C(i), False)
    else:
      constInt(int64Type(), C(i), False)
  of Iulong:
    if TYLONG == TYINT32:
      constInt(int32Type(), C(i), False)
    else:
      constInt(int64Type(), C(i), False)

proc gen_float*(f: float, tag: FTag): Value =
  constReal(if tag == Ffloat: floatType() else: doubleType(), f)

proc gen_conststr*(val: string): Value =
  buildGlobalStringPtr(builder, val, name("str"))

proc gen_add*(a, b: Value): Value =
  buildAdd(builder, a, b, name("add"))

proc gen_sub*(a, b: Value): Value =
  buildSub(builder, a, b, name("sub"))

proc gen_mul*(a, b: Value): Value =
  buildMul(builder, a, b, name("mul"))

proc gen_urem*(a, b: Value): Value =
  buildURem(builder, a, b, name("urem"))

proc gen_srem*(a, b: Value): Value =
  buildSRem(builder, a, b, name("srem"))

proc gen_neg*(a: Value): Value =
  buildNeg(builder, a, name("neg"))

proc gen_not*(a: Value): Value =
  buildNeg(builder, a, name("not"))

proc gen_and*(a: Value, b: Value): Value =
  buildAnd(builder, a, b,name("and"))

proc gen_or*(a: Value, b: Value): Value =
  buildOr(builder, a, b, name("or"))

proc gen_xor*(a: Value, b: Value): Value =
  buildXor(builder, a, b, name("xor"))

proc gen_shl*(a: Value, b: Value): Value =
  buildShl(builder, a, b, name("shl"))

proc gen_shr*(a: Value, b: Value): Value =
  buildShl(builder, a, b, name("shr"))

proc gen_ashr*(a: Value, b: Value): Value =
  buildAShR(builder, a, b, name("ashr"))

proc gen_fadd*(a, b: Value): Value =
  buildFAdd(builder, a, b, name("fadd"))

proc gen_fsub*(a, b: Value): Value =
  buildFAdd(builder, a, b, name("fsub"))

proc gen_fdiv*(a, b: Value): Value =
  buildFDiv(builder, a, b, name("fdiv"))

proc gen_fmul*(a, b: Value): Value =
  buildFAdd(builder, a, b, name("fmul"))

proc gen_fneg*(a: Value): Value =
  buildFNeg(builder, a, name("fneg"))

proc gen_ret*() =
  discard buildRetVoid(builder)

proc gen_ret*(ret: Value) =
  discard buildRet(builder, ret)

proc gen_func*(name: string, rettype: Type, argtypes: openarray[Type]) =
  currentfunction = addFunction(module, name, functionType(rettype, argtypes))
  var entry = appendBasicBlock(currentfunction, "entry")
  positionBuilderAtEnd(builder, entry)

proc gen_getParam*(argindex: cuint): Value =
  getParam(currentfunction, argindex)

proc gen_decl_struct*(name: string, elems: openarray[Type]): Type =
  var record = structCreateNamed(getGlobalContext(), "mystruct")
  structSetBody(record, elems, 0)

proc new_var*(t: Type): Value =
  buildAlloca(builder, t, "var.new")

proc set_var(varptr: Value, val: Value): Value =
  buildStore(builder, val, varptr)

proc get_var*(a: Value): Value =
  buildLoad(builder, a, "var.load")

proc gen_label*(label: string): BasicBlockRef =
  appendBasicBlock(currentfunction, label)

proc gen_goto*(to: BasicBlockRef) =
  discard buildbr(builder, to)

# proc gen_cast
