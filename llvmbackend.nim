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

proc writeBitcodeToFile*(path: string) =
  discard writeBitcodeToFile(module, path)

proc llvm_error(msg: string) =
  stderr.writeLine("llvm error: " & msg)

proc llvm_error(msg: cstring) =
  stderr.write(msg)

proc writeModuleToFile*(path: string) =
  var err: cstring = ""
  if printModuleToFile(module, path, cast[cstringArray](addr err)) == True:
    llvm_error("LLVMPrintModuleToFile")
    llvm_error(err)

proc writeObjectFile*(path: string) =
  var err: cstring = ""
  if targetMachineEmitToFile(machine, module, path, ObjectFile, cast[cstringArray](addr err)) == True:
    llvm_error("LLVMTargetMachineEmitToFile")
    llvm_error(err)

proc writeAssemblyFile*(path: string) =
  var err: cstring = ""
  if targetMachineEmitToFile(machine, module, path, AssemblyFile, cast[cstringArray](addr err)) == True:
    llvm_error("LLVMTargetMachineEmitToFile")
    llvm_error(err)


proc close_backend*() =
  disposeBuilder(builder)

proc gen*(e: Expr): Value

proc gen*(s: Stmt)

proc gen_cond*(a: Expr): Value =
  ## generate bool(conditional) expression
  discard

proc gen_bool*(val: bool): Value =
  constInt(int1Type(), if val: 1 else: 0, False)

proc gen_true*(): Value =
  constInt(int1Type(), 1, False)

proc gen_false*(): Value =
  constInt(int1Type(), 0, False)

proc gen_int*(i: culonglong, tags: uint32): Value =
    if (tags and TYBOOL) != 0:
        constInt(int1Type(), i, llvm.False)
    elif (tags and (TYINT8 or TYUINT8)) != 0:
        constInt(int8Type(), i, llvm.False)
    elif (tags and (TYINT16 or TYUINT16)) != 0:
        constInt(int16Type(), i, llvm.False)
    elif (tags and (TYINT32 or TYUINT32)) != 0:
        constInt(int32Type(), i, llvm.False)
    elif (tags and (TYINT64 or TYUINT64)) != 0:
        constInt(int64Type(), i, llvm.False)
    else:
        assert false
        nil


proc gen_float*(f: float, tag: uint32): Value =
  constReal(if (tag and TYFLOAT) != 0: floatType() else: doubleType(), f)

proc gen_struct*(t: Type, values: seq[Value]): Value =
  var l = len(values)
  var buf = create(Value, l or 1)
  var arr = cast[ptr UncheckedArray[Value]](buf)
  for i in 0 ..< l:
    arr[i] = values[i]
  dealloc(buf)
  constNamedStruct(t, buf, cuint(l))

proc gen_union*(t: Type, values: seq[Value]): Value =
  gen_struct(t, values)

proc gen_str*(val: string): Value =
  buildGlobalStringPtr(builder, val, name("str"))

proc gen_add*(a, b: Value): Value =
  buildAdd(builder, a, b, name("add"))

proc gen_getParam*(argindex: cuint): Value =
  getParam(currentfunction, argindex)

proc gen_decl_struct*(name: string, elems: openarray[Type]): Type =
  var record = structCreateNamed(getGlobalContext(), "mystruct")
  structSetBody(record, elems, 0)

proc new_var*(t: Type): Value =
  buildAlloca(builder, t, "var.new")

proc set_var*(varptr: Value, val: Value): Value =
  buildStore(builder, val, varptr)

proc get_var*(a: Value): Value =
  buildLoad(builder, a, "var.load")

proc gen_label*(label: string): BasicBlockRef =
  appendBasicBlock(currentfunction, label)

proc gen_goto*(to: BasicBlockRef) =
  discard buildbr(builder, to)

proc toBackendType*(ty: CType): Type =
  case ty.spec:
  of TYPRIM:
    return (
      if (ty.tags and TYVOID) != 0:
          voidTypeInContext(getGlobalContext())
      elif (ty.tags and TYBOOL) != 0:
          int1Type()
      elif (ty.tags and (TYINT8 or TYUINT8)) != 0:
          int8Type()
      elif (ty.tags and (TYINT16 or TYUINT16)) != 0:
          int16Type()
      elif (ty.tags and (TYINT32 or TYUINT32)) != 0:
          int32Type()
      elif (ty.tags and (TYINT64 or TYUINT64)) != 0:
          int64Type()
      elif (ty.tags and TYFLOAT) != 0:
          floatType()
      elif (ty.tags and TYDOUBLE) != 0:
          doubleType()
      else:
        assert false
        nil
      )
  of TYPOINTER:
    return pointerType(toBackendType(ty.p), 0)
  # TODO: table get, decl => here
  of TYSTRUCT, TYUNION:
    let l = len(ty.selems)
    var buf = create(Type, l or 1)
    var arr = cast[ptr UncheckedArray[Type]](buf)
    for i in 0 ..< l:
      arr[i] = toBackendType(ty.selems[i][1])
    result = structCreateNamed(getGlobalContext(), cstring(ty.sname))
    structSetBody(result, buf, cuint(l), False)
    dealloc(buf)
    return result
  of TYFUNCTION:
    let l = len(ty.params)
    var buf = create(Type, l or 1)
    var arr = cast[ptr UncheckedArray[Type]](buf)
    var ivarargs = false
    for i in 0 ..< l:
      if ty.params[i][1] == nil:
        ivarargs = true
        break
      if ty.params[i][1].spec == TYBITFIELD:
        arr[i] = toBackendType(ty.params[i][1].bittype)
      else:
        arr[i] = toBackendType(ty.params[i][1])
    result = functionType(toBackendType(ty.ret), buf, cuint(l), if ivarargs: True else: False)
    dealloc(buf)
    return result
  of TYARRAY:
    return arrayType(toBackendType(ty.arrtype), cuint(ty.arrsize))
  of TYENUM:
    if TYINT == TYINT32:
      return int32Type()
    else:
      return int64Type()
  of TYBITFIELD:
    assert false
    return nil

proc getZero*(ty: CType): Value =
  constNull(toBackendType(ty))

proc gen_condition*(test: Expr, lhs: Expr, rhs: Expr): Value =
  ## build a `cond ? lhs : rhs` expression
  var ret = buildAlloca(builder, toBackendType(lhs.ty), "cond.alloc")
  var iftrue = appendBasicBlock(currentfunction, "cond.true")
  var iffalse = appendBasicBlock(currentfunction, "cond.false")
  var ifend = appendBasicBlock(currentfunction, "cond.end")
  discard buildCondBr(builder, gen_cond(test), iftrue, iffalse)

  positionBuilderAtEnd(builder, iftrue)
  discard buildStore(builder, gen(lhs), ret)
  discard buildBr(builder, ifend)

  positionBuilderAtEnd(builder, iffalse)
  discard buildStore(builder, gen(rhs), ret)
  discard buildBr(builder, ifend)

  positionBuilderAtEnd(builder, ifend)
  return buildLoad(builder, ret, "cond.load")

proc gen_condition*(test: Expr, lhs: Value, rhs: Expr): Value =
  ## build a `cond ? lhs : rhs` expression
  var ret = buildAlloca(builder, typeOfX(lhs), "cond.alloc")
  var iftrue = appendBasicBlock(currentfunction, "cond.true")
  var iffalse = appendBasicBlock(currentfunction, "cond.false")
  var ifend = appendBasicBlock(currentfunction, "cond.end")
  discard buildCondBr(builder, gen_cond(test), iftrue, iffalse)

  positionBuilderAtEnd(builder, iftrue)
  discard buildStore(builder, (lhs), ret)
  discard buildBr(builder, ifend)

  positionBuilderAtEnd(builder, iffalse)
  discard buildStore(builder, gen(rhs), ret)
  discard buildBr(builder, ifend)

  positionBuilderAtEnd(builder, ifend)
  return buildLoad(builder, ret, "cond.load")

proc gen_condition*(test: Expr, lhs: Expr, rhs: Value): Value =
  ## build a `cond ? lhs : rhs` expression
  var ret = buildAlloca(builder, typeOfX(rhs), "cond.alloc")
  var iftrue = appendBasicBlock(currentfunction, "cond.true")
  var iffalse = appendBasicBlock(currentfunction, "cond.false")
  var ifend = appendBasicBlock(currentfunction, "cond.end")
  discard buildCondBr(builder, gen_cond(test), iftrue, iffalse)

  positionBuilderAtEnd(builder, iftrue)
  discard buildStore(builder, gen(lhs), ret)
  discard buildBr(builder, ifend)

  positionBuilderAtEnd(builder, iffalse)
  discard buildStore(builder, (rhs), ret)
  discard buildBr(builder, ifend)

  positionBuilderAtEnd(builder, ifend)
  return buildLoad(builder, ret, "cond.load")


proc gen_logicAnd*(a, b: Expr): Value =
  ## alias for `a ? b : false`
  gen_condition(a, b, gen_false())

proc gen_logicOr*(a, b: Expr): Value =
  ## alias for `a ? true : b `
  gen_condition(a, gen_true(), b)

proc gen_if*(test: Expr, body: Stmt) =
  var iftrue = appendBasicBlock(currentfunction, "if.true")
  var ifend = appendBasicBlock(currentfunction, "if.end")
  discard buildCondBr(builder, gen_cond(test), iftrue, ifend)

  positionBuilderAtEnd(builder, iftrue)
  gen(body)
  discard buildBr(builder, ifend)

  positionBuilderAtEnd(builder, ifend)

proc gen_if*(test: Expr, body: Stmt, elsebody: Stmt) =
  var iftrue = appendBasicBlock(currentfunction, "if.true")
  var iffalse = appendBasicBlock(currentfunction, "if.false")
  var ifend = appendBasicBlock(currentfunction, "if.end")
  discard buildCondBr(builder, gen_cond(test), iftrue, iffalse)

  positionBuilderAtEnd(builder, iftrue)
  gen(body)
  discard buildBr(builder, ifend)

  positionBuilderAtEnd(builder, iffalse)
  gen(elsebody)
  discard buildBr(builder, ifend)

  positionBuilderAtEnd(builder, ifend)

proc gen_while*(test: Expr, body: Stmt) =
  var whilecmp = appendBasicBlock(currentfunction, "while.cmp")
  var whilebody = appendBasicBlock(currentfunction, "while.body")
  var whileleave = appendBasicBlock(currentfunction, "while.leave")
  discard buildBr(builder, whilecmp)

  positionBuilderAtEnd(builder, whilecmp)
  var cond = gen_cond(test)
  discard buildCondBr(builder, cond, whilebody, whileleave)

  positionBuilderAtEnd(builder, whilebody)
  gen(body)

  discard buildBr(builder, whilecmp)

  positionBuilderAtEnd(builder, whileleave)

proc gen_dowhile*(test: Expr, body: Stmt) =
  var dowhilecmp = appendBasicBlock(currentfunction, "dowhile.cmp")
  var dowhilebody = appendBasicBlock(currentfunction, "dowhile.body")
  var dowhileleave = appendBasicBlock(currentfunction, "dowhile.leave")

  positionBuilderAtEnd(builder, dowhilebody)
  gen(body)

  positionBuilderAtEnd(builder, dowhilecmp)
  var cond = gen_cond(test)
  discard buildCondBr(builder, cond, dowhilebody, dowhileleave)

  positionBuilderAtEnd(builder, dowhileleave)

# the compiler may generate a table(array), so require `O(1)` time indexing
proc getOp(a: BinOP): Opcode =
  case a:
  of token.Add: llvm.Add
  of token.FAdd: llvm.FAdd
  of token.Sub: llvm.Sub
  of token.FSub: llvm.FSub
  of token.Mul: llvm.Mul
  of token.FMul: llvm.FMul
  of token.UDiv: llvm.UDiv
  of token.SDiv: llvm.SDiv
  of token.FDiv: llvm.FDiv
  of token.URem: llvm.URem
  of token.SRem: llvm.SRem
  of token.FRem: llvm.FRem
  of token.Shr: llvm.LShr
  of token.AShr: llvm.AShr
  of token.Shl: llvm.Shl
  of token.And: llvm.And
  of token.Xor: llvm.Xor
  of token.Or: llvm.Or
  else: assert(false);cast[Opcode](0)

proc getICmpOp(a: BinOP): IntPredicate =
  case a:
  of EQ: IntEQ
  of NE: IntNE
  of UGE: IntUGE
  of UGT: IntUGT
  of ULE: IntULE
  of ULT: IntULT
  of SGE: IntSGE
  of SGT: IntSGT
  of SLT: IntSLT
  of SLE: IntSLE 
  else: assert(false);cast[IntPredicate](0)

proc getFCmpOp(a: BinOP): RealPredicate =
  case a:
  of FEQ: RealOEQ
  of FNE: RealONE
  of FGT: RealOGT
  of FGE: RealOGE
  of FLT: RealOLT
  of FLE: RealOLE
  else: assert(false);cast[RealPredicate](0)


proc gen*(s: Stmt) =
  case s.k:
  of SCompound:
    # TODO: Block
    for i in s.stmts:
      gen(i)
  of SExpr:
      discard gen(s.exprbody)
  of SFunction:
      assert s.functy.spec == TYFUNCTION
      currentfunction = addFunction(module, cstring(s.funcname), toBackendType(s.functy))
      var entry = appendBasicBlock(currentfunction, "entry")
      positionBuilderAtEnd(builder, entry)
      gen(s.funcbody)
  of SReturn:
      if s.exprbody != nil:
        discard buildRet(builder, gen(s.exprbody))
      else:
        discard buildRetVoid(builder)
  of SIf:
      if s.elsebody == nil:
        gen_if(s.iftest, s.ifbody)
      else:
        gen_if(s.iftest, s.ifbody, s.elsebody)
  of SWhile:
    gen_while(s.test, s.body)
  of SDoWhile:
    gen_dowhile(s.test, s.body)
  of SStaticAssertDecl:
    discard
  else:
    assert false

proc gen_cast*(e: Expr, to: CType): Value =
  discard

proc gen*(e: Expr): Value =
  case e.k:
  of EBin:
    case e.bop:
    of EQ..SLE:
      buildICmp(builder, getICmpOp(e.bop), gen(e.lhs), gen(e.rhs), name("icmp"))
    of FEQ..FLE:
      buildFCmp(builder, getFCmpOp(e.bop), gen(e.lhs), gen(e.rhs), name("fcmp"))
    of Comma:
      nil
    else:
      buildBinOp(builder, getOp(e.bop), gen(e.lhs), gen(e.rhs), name("bop"))
  of EIntLit:
    gen_int(e.ival.culonglong, e.ty.tags)
  of EFloatLit:
    gen_float(e.fval, e.ty.tags)
  of EUnary:
    case e.uop:
    of token.Pos: gen(e.uoperand)
    of token.Neg: buildNeg(builder, gen(e.uoperand), "neg")
    of token.FNeg: buildFNeg(builder, gen(e.uoperand), "fneg")
    of token.Not: buildNot(builder, gen(e.uoperand), "not")
    of token.AddressOf: nil
    of token.PrefixIncrement: nil
    of token.PrefixDecrement: nil
    of token.Dereference: buildLoad(builder, gen(e.uoperand), "deref")
    of token.LogicalNot: buildICmp(builder, IntEQ, gen(e.uoperand), getZero(e.ty), "logicnot")
    of UNop: assert false; nil
  of EPostFix:
    nil
  of EVar:
    nil
  of ECondition:
    nil
  of ECast:
    return gen_cast(e.castval, (e.ty))
  of ECall:
    nil
  of ESubscript:
    nil
  of EArray:
    nil


