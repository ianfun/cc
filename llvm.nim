import tables

type
  OpaqueMemoryBuffer = object
  OpaqueAttributeRef{.pure, final.} = object
  OpaqueContext{.pure, final.} = object
  OpaqueModule{.pure, final.} = object
  OpaqueType{.pure, final.} = object
  OpaqueValue{.pure, final.} = object
  OpaqueBasicBlock{.pure, final.} = object
  OpaqueBuilder{.pure, final.} = object
  OpaqueModuleProvider{.pure, final.} = object
  OpaquePassManager{.pure, final.} = object
  OpaquePassRegistry{.pure, final.} = object
  OpaqueUse{.pure, final.} = object
  OpaqueDiagnosticInfo{.pure, final.} = object
  OpaqueTargetMachine{.pure, final.} = object
  orcOpaqueLLJITBuilder*{.pure, final.} = object
  orcOpaqueLLJIT*{.pure, final.} = object
  orcOpaqueSymbolStringPool*{.pure, final.} = object
  orcOpaqueSymbolStringPoolEntry*{.pure, final.} = object
  orcOpaqueJITDylib*{.pure, final.} = object
  orcOpaqueJITTargetMachineBuilder*{.pure, final.} = object
  orcOpaqueMaterializationUnit*{.pure, final.} = object
  orcOpaqueMaterializationResponsibility*{.pure, final.} = object
  orcOpaqueResourceTracker*{.pure, final.} = object
  orcOpaqueDefinitionGenerator*{.pure, final.} = object
  orcOpaqueLookupState{.pure, final.} = object
  orcOpaqueThreadSafeContext{.pure, final.} = object
  orcOpaqueObjectTransformLayer*{.pure, final.} = object
  orcOpaqueExecutionSession*{.pure, final.} = object
  orcOpaqueIRTransformLayer*{.pure, final.} = object
  opaqueError*{.pure, final.} = object
  orcOpaqueObjectLayer*{.pure, final.} = object
  orcOpaqueObjectLinkingLayer*{.pure, final.} = object
  orcOpaqueIndirectStubsManager*{.pure, final.} = object
  orcOpaqueLazyCallThroughManager*{.pure, final.} = object
  orcOpaqueDumpObjects*{.pure, final.} = object
  ErrorRef*{.pure, final.} = ptr opaqueError
  orcOpaqueThreadSafeModule*{.pure, final.} = object
  OpaquePassManagerBuilder*{.pure, final.} = object
  OpaqueMetaData{.pure, final.} = object
  OpaqueDIBuilder{.pure, final.} = object
  target{.pure, final.} = object
  OpaqueJITEventListener{.pure, final.} = object
  OpaqueNamedMDNode{.pure, final.} = object
  opaqueValueMetadataEntry{.pure, final.} = object
  comdat{.pure, final.} = object
  opaqueModuleFlagEntry{.pure, final.} = object
  OpaqueBinary{.pure, final.} = object
  int64T = int64
  uint64T = uint64
  uint8T = uint8
  int32T = int32
  uint32T = uint32
  Bool* = cint
  AttributeIndex* = cuint
  OpaqueTargetData{.pure, final.} = object
  TargetDataRef* = ptr OpaqueTargetData
  OpaqueTargetLibraryInfotData{.pure, final.} = object
  TargetLibraryInfoRef* = ptr OpaqueTargetLibraryInfotData
  Opcode {.pure, size: sizeof(cint).} = cint
  DIFlags* = cint
  DWARFTypeEncoding* = cuint
  MetadataKind* = cuint
  ByteOrdering* {.size: sizeof(cint).} = enum
    BigEndian, LittleEndian
  TargetMachineRef* = ptr OpaqueTargetMachine
  PassManagerBuilderRef* = ptr OpaquePassManagerBuilder
  VerifierFailureAction {.size: sizeof(cint), pure.} = enum
    AbortProcessAction, PrintMessageAction, ReturnStatusAction

const
  False*: Bool = 0
  True*: Bool = 1

# llvm core
include llvm/Types
include llvm/Support
include llvm/Error
include llvm/Core

# writing LLVM IR
include llvm/BitWriter

# verifyModule
include llvm/Analysis

# target machine
include llvm/TargetMachine

# optmize modules
include llvm/Transforms/PassManagerBuilder
include llvm/Transforms/Scalar

# jit headers
include llvm/Orc
include llvm/LLJIT

#  initializeNativeTarget
#  initializeNativeAsmPrinter
include LLVMTarget

proc typeOfX*(val: ValueRef): TypeRef {.importc: "LLVMTypeOf".}

proc `$`*(v: ValueRef): string =
  let tmp = v.printValueToString()
  result = $tmp
  disposeMessage(tmp)

proc `$`*(v: TypeRef): string =
  let tmp = v.printTypeToString()
  result = $tmp
  disposeMessage(tmp)

const
   LLVMRet*            = 1.Opcode
   LLVMBr*             = 2.Opcode
   LLVMSwitch*         = 3.Opcode
   LLVMIndirectBr*     = 4.Opcode
   LLVMInvoke*         = 5.Opcode
   LLVMUnreachable*    = 7.Opcode
   LLVMCallBr*         = 67.Opcode
   LLVMFNeg*           = 66.Opcode
   LLVMAdd*            = 8.Opcode
   LLVMFAdd*           = 9.Opcode
   LLVMSub*            = 10.Opcode
   LLVMFSub*           = 11.Opcode
   LLVMMul*            = 12.Opcode
   LLVMFMul*           = 13.Opcode
   LLVMUDiv*           = 14.Opcode
   LLVMSDiv*           = 15.Opcode
   LLVMFDiv*           = 16.Opcode
   LLVMURem*           = 17.Opcode
   LLVMSRem*           = 18.Opcode
   LLVMFRem*           = 19.Opcode
   LLVMShl*            = 20.Opcode
   LLVMLShr*           = 21.Opcode
   LLVMAShr*           = 22.Opcode
   LLVMAnd*            = 23.Opcode
   LLVMOr*             = 24.Opcode
   LLVMXor*            = 25.Opcode
   LLVMAlloca*         = 26.Opcode
   LLVMLoad*           = 27.Opcode
   LLVMStore*          = 28.Opcode
   LLVMGetElementPtr*  = 29.Opcode
   LLVMTrunc*          = 30.Opcode
   LLVMZExt*           = 31.Opcode
   LLVMSExt*           = 32.Opcode
   LLVMFPToUI*         = 33.Opcode
   LLVMFPToSI*         = 34.Opcode
   LLVMUIToFP*         = 35.Opcode
   LLVMSIToFP*         = 36.Opcode
   LLVMFPTrunc*        = 37.Opcode
   LLVMFPExt*          = 38.Opcode
   LLVMPtrToInt*       = 39.Opcode
   LLVMIntToPtr*       = 40.Opcode
   LLVMBitCast*        = 41.Opcode
   LLVMAddrSpaceCast*  = 60.Opcode
   LLVMICmp*           = 42.Opcode
   LLVMFCmp*           = 43.Opcode
   LLVMPHI*            = 44.Opcode
   LLVMCall*           = 45.Opcode
   LLVMSelect*         = 46.Opcode
   LLVMUserOp1*        = 47.Opcode
   LLVMUserOp2*        = 48.Opcode
   LLVMVAArg*          = 49.Opcode
   LLVMExtractElement* = 50.Opcode
   LLVMInsertElement*  = 51.Opcode
   LLVMShuffleVector*  = 52.Opcode
   LLVMExtractValue*   = 53.Opcode
   LLVMInsertValue*    = 54.Opcode
   LLVMFreeze*         = 68.Opcode
   LLVMFence*          = 55.Opcode
   LLVMAtomicCmpXchg*  = 56.Opcode
   LLVMAtomicRMW*      = 57.Opcode
   LLVMResume*         = 58.Opcode
   LLVMLandingPad*     = 59.Opcode
   LLVMCleanupRet*     = 61.Opcode
   LLVMCatchRet*       = 62.Opcode
   LLVMCatchPad*       = 63.Opcode
   LLVMCleanupPad*     = 64.Opcode
   LLVMCatchSwitch*    = 65.Opcode


import token

type
  Value* = ValueRef
  Type = TypeRef

type
    Backend* = ref object
      # jump labels
      lables*: seq[TableRef[string, Value]]
      # struct/union
      tags*: seq[TableRef[string, Type]]
      # variable and enum constants
      vars*: seq[TableRef[string, Value]]
      # module
      m*: ModuleRef
      module*: ModuleRef
      builder*: BuilderRef
      machine*: TargetMachineRef
      currentfunction*: Value
      tsCtx*: OrcThreadSafeContextRef
      ctx*: ContextRef
      layout*: TargetDataRef

var b*: Backend

proc llvm_error*(msg: string) =
  stderr.writeLine("llvm error: " & msg)

proc llvm_error*(msg: cstring) =
  stderr.write(msg)

proc newBackend*(module_name="main"): bool =
  initializeCore(getGlobalPassRegistry())
  initializeNativeTarget()
  initializeNativeAsmPrinter()
  b = Backend(
    tsCtx: orcCreateNewThreadSafeContext(),
    builder: createBuilder()
  )
  b.ctx =  orcThreadSafeContextGetContext(b.tsCtx)
  b.module = moduleCreateWithNameInContext(module_name, b.ctx)
  var target: TargetRef
  let tr = getDefaultTargetTriple()
  if getTargetFromTriple(tr, addr target, nil) == True:
    llvm_error("LLVMGetTargetFromTriple failed")
    return false
  b.machine = createTargetMachine(target, tr, "", "", CodeGenLevelDefault, RelocDefault, CodeModelDefault)
  b.layout = createTargetDataLayout(b.machine)
  setModuleDataLayout(b.module, b.layout)
  setTarget(b.module, tr)
  return true

proc enterScope*() =
  ## not token.enterBlock
  b.lables.add(newTable[string, Value]())
  b.tags.add(newTable[string, Type]())
  b.vars.add(newTable[string, Value]())

proc leaveScope*() =
  ## not token.leaveBlock
  discard b.tags.pop()
  discard b.lables.pop()
  discard b.vars.pop()

proc getVar*(name: string): Value =
  for i in countdown(len(b.vars)-1, 0):
    result = b.vars[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc putVar*(name: string, val: Value) =
  b.vars[^1][name] = val

proc getLabel*(name: string): Value =
  for i in countdown(len(b.lables)-1, 0):
    result = b.lables[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc putLabel*(name: string, label: Value) =
  b.lables[^1][name] = label

proc getTags*(name: string): Type =
  for i in countdown(len(b.tags)-1, 0):
    result = b.tags[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc putTags*(name: string, t: Type) =
  b.tags[^1][name] = t

proc shutdown_backend*() =
  shutdown()

proc writeBitcodeToFile*(path: string) =
  discard writeBitcodeToFile(b.module, path)

proc optimize*() =
  var passM = createPassManager()

  addInstructionCombiningPass(passM)
  addReassociatePass(passM)
  addGVNPass(passM)
  addCFGSimplificationPass(passM)

  discard runPassManager(passM, b.module)

  disposePassManager(passM)

proc verify*() =
  var err: cstring
  discard verifyModule(b.module, PrintMessageAction, cast[cstringArray](addr err))

proc writeModuleToFile*(path: string) =
  var err: cstring = ""
  if printModuleToFile(b.module, path, cast[cstringArray](addr err)) == True:
    llvm_error("LLVMPrintModuleToFile")
    llvm_error(err)

proc writeObjectFile*(path: string) =
  var err: cstring = ""
  if targetMachineEmitToFile(b.machine, b.module, path, ObjectFile, cast[cstringArray](addr err)) == True:
    llvm_error("LLVMTargetMachineEmitToFile")
    llvm_error(err)

proc writeAssemblyFile*(path: string) =
  var err: cstring = ""
  if targetMachineEmitToFile(b.machine, b.module, path, AssemblyFile, cast[cstringArray](addr err)) == True:
    llvm_error("LLVMTargetMachineEmitToFile")
    llvm_error(err)


proc close_backend*() =
  disposeBuilder(b.builder)

proc gen*(e: Expr): Value

proc gen*(s: Stmt)

proc gen_cond*(a: Expr): Value =
  ## generate bool(conditional) expression
  gen(a)

proc gen_bool*(val: bool): Value =
  constInt(int1TypeInContext(b.ctx), if val: 1 else: 0, False)

proc gen_true*(): Value =
  constInt(int1TypeInContext(b.ctx), 1, False)

proc gen_false*(): Value =
  constInt(int1TypeInContext(b.ctx), 0, False)

proc gen_int*(i: culonglong, tags: uint32): Value =
    if (tags and TYBOOL) != 0:
        constInt(int1TypeInContext(b.ctx), i, False)
    elif (tags and (TYINT8 or TYUINT8)) != 0:
        constInt(int8TypeInContext(b.ctx), i, False)
    elif (tags and (TYINT16 or TYUINT16)) != 0:
        constInt(int16TypeInContext(b.ctx), i, False)
    elif (tags and (TYINT32 or TYUINT32)) != 0:
        constInt(int32TypeInContext(b.ctx), i, False)
    elif (tags and (TYINT64 or TYUINT64)) != 0:
        constInt(int64TypeInContext(b.ctx), i, False)
    else:
        assert false
        nil

proc gen_float*(f: float, tag: uint32): Value =
  constReal(if (tag and TYFLOAT) != 0: floatTypeInContext(b.ctx) else: doubleTypeInContext(b.ctx), f)

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
  buildGlobalStringPtr(b.builder, val, "str")

proc gen_getParam*(argindex: cuint): Value =
  getParam(b.currentfunction, argindex)

#proc gen_decl_struct*(name: string, elems: openarray[Type]): Type =
#  var record = structCreateNamed(getGlobalContext(), "mystruct")
#  structSetBody(record, elems, 0)

proc new_var*(t: Type): Value =
  buildAlloca(b.builder, t, "var.new")

proc set_var*(varptr: Value, val: Value): Value =
  buildStore(b.builder, val, varptr)

proc get_var*(a: Value): Value =
  buildLoad(b.builder, a, "var.load")

proc gen_label*(label: string): BasicBlockRef =
  appendBasicBlockInContext(b.ctx, b.currentfunction, label)

proc gen_goto*(to: BasicBlockRef) =
  discard buildbr(b.builder, to)

proc backendint*(): Type =
    if TYINT == TYINT32:
      int32TypeInContext(b.ctx)
    else:
      int64TypeInContext(b.ctx)

proc toBackendType*(ty: CType): Type =
  case ty.spec:
  of TYPRIM:
    return (
      if (ty.tags and TYBOOL) != 0:
          int1TypeInContext(b.ctx)
      elif (ty.tags and (TYINT8 or TYUINT8)) != 0:
          int8TypeInContext(b.ctx)
      elif (ty.tags and (TYINT16 or TYUINT16)) != 0:
          int16TypeInContext(b.ctx)
      elif (ty.tags and (TYINT32 or TYUINT32)) != 0:
          int32TypeInContext(b.ctx)
      elif (ty.tags and (TYINT64 or TYUINT64)) != 0:
          int64TypeInContext(b.ctx)
      elif (ty.tags and TYFLOAT) != 0:
          floatTypeInContext(b.ctx)
      elif (ty.tags and TYDOUBLE) != 0:
          doubleTypeInContext(b.ctx)
      else:
        assert false
        nil
      )
  of TYPOINTER:
    if (ty.p.tags and TYVOID) != 0:
      # `void*` is i8 in llvm
      return pointerType(int8TypeInContext(b.ctx), 0)
    return pointerType(toBackendType(ty.p), 0)
  of TYSTRUCT, TYUNION:
    if len(ty.sname) > 0:
      var old = getTags(ty.sname)
      if old != nil:
        return old
      let l = len(ty.selems)
      var buf = create(Type, l or 1)
      var arr = cast[ptr UncheckedArray[Type]](buf)
      for i in 0 ..< l:
        arr[i] = toBackendType(ty.selems[i][1])
      result = structCreateNamed(b.ctx, cstring(ty.sname))
      structSetBody(result, buf, cuint(l), False)
      dealloc(buf)
      putTags(ty.sname, result)
      return result

    let l = len(ty.selems)
    var buf = create(Type, l or 1)
    var arr = cast[ptr UncheckedArray[Type]](buf)
    for i in 0 ..< l:
      arr[i] = toBackendType(ty.selems[i][1])
    result = structTypeInContext(b.ctx, buf, cuint(l), False)

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
    if len(ty.ename) > 0:
      let s = getTags(ty.ename)
      if s == nil:
        for (name, val) in ty.eelems:
          putVar(name, constInt(backendint(), val.culonglong, False))
        putTags(ty.ename, backendint())
    return backendint()
  of TYBITFIELD:
    assert false
    return nil

proc getZero*(ty: CType): Value =
  constNull(toBackendType(ty))

proc getOne*(ty: CType): Value =
  assert ty.spec == TYPRIM
  constInt(toBackendType(ty), 1, False)

proc gen_condition*(test: Expr, lhs: Expr, rhs: Expr): Value =
  ## build a `cond ? lhs : rhs` expression
  var ty = toBackendType(lhs.ty)
  var iftrue = appendBasicBlockInContext(b.ctx, b.currentfunction, "cond.true")
  var iffalse = appendBasicBlockInContext(b.ctx, b.currentfunction, "cond.false")
  var ifend = appendBasicBlockInContext(b.ctx, b.currentfunction, "cond.end")
  discard buildCondBr(b.builder, gen_cond(test), iftrue, iffalse)
  
  positionBuilderAtEnd(b.builder, iftrue)
  var left = gen(lhs)
  discard buildBr(b.builder, ifend)
  
  positionBuilderAtEnd(b.builder, iffalse)
  var right =gen (rhs)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, ifend)
  var phi = buildPhi(b.builder, ty, "cond.phi")

  var blocks = [iftrue, iffalse]
  var values = [left, right]
  addIncoming(phi, addr values[0], addr blocks[0], 2)
  return phi

proc gen_condition*(test: Expr, lhs: Value, rhs: Expr): Value =
  ## build a `cond ? lhs : rhs` expression
  var ty = toBackendType(rhs.ty)
  var iftrue = appendBasicBlockInContext(b.ctx, b.currentfunction, "cond.true")
  var iffalse = appendBasicBlockInContext(b.ctx, b.currentfunction, "cond.false")
  var ifend = appendBasicBlockInContext(b.ctx, b.currentfunction, "cond.end")
  discard buildCondBr(b.builder, gen_cond(test), iftrue, iffalse)
  
  positionBuilderAtEnd(b.builder, iftrue)
  var left = (lhs)
  discard buildBr(b.builder, ifend)
  
  positionBuilderAtEnd(b.builder, iffalse)
  var right = gen(rhs)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, ifend)
  var phi = buildPhi(b.builder, ty, "cond.phi")

  var blocks = [iftrue, iffalse]
  var values = [left, right]
  addIncoming(phi, addr values[0], addr blocks[0], 2)
  return phi

proc gen_condition*(test: Expr, lhs: Expr, rhs: Value): Value =
  ## build a `cond ? lhs : rhs` expression
  var ty = toBackendType(lhs.ty)
  var iftrue = appendBasicBlockInContext(b.ctx, b.currentfunction, "cond.true")
  var iffalse = appendBasicBlockInContext(b.ctx, b.currentfunction, "cond.false")
  var ifend = appendBasicBlockInContext(b.ctx, b.currentfunction, "cond.end")
  discard buildCondBr(b.builder, gen_cond(test), iftrue, iffalse)
  
  positionBuilderAtEnd(b.builder, iftrue)
  var left = gen(lhs)
  discard buildBr(b.builder, ifend)
  
  positionBuilderAtEnd(b.builder, iffalse)
  var right = (rhs)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, ifend)
  var phi = buildPhi(b.builder, ty, "cond.phi")

  var blocks = [iftrue, iffalse]
  var values = [left, right]
  addIncoming(phi, addr values[0], addr blocks[0], 2)
  return phi


proc gen_if*(test: Expr, body: Stmt) =
  var iftrue = appendBasicBlockInContext(b.ctx, b.currentfunction, "if.true")
  var ifend = appendBasicBlockInContext(b.ctx, b.currentfunction, "if.end")
  discard buildCondBr(b.builder, gen_cond(test), iftrue, ifend)

  positionBuilderAtEnd(b.builder, iftrue)
  gen(body)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, ifend)

proc gen_if*(test: Expr, body: Stmt, elsebody: Stmt) =
  var iftrue = appendBasicBlockInContext(b.ctx, b.currentfunction, "if.true")
  var iffalse = appendBasicBlockInContext(b.ctx, b.currentfunction, "if.false")
  var ifend = appendBasicBlockInContext(b.ctx, b.currentfunction, "if.end")
  discard buildCondBr(b.builder, gen_cond(test), iftrue, iffalse)

  positionBuilderAtEnd(b.builder, iftrue)
  gen(body)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, iffalse)
  gen(elsebody)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, ifend)

proc gen_while*(test: Expr, body: Stmt) =
  var whilecmp = appendBasicBlockInContext(b.ctx, b.currentfunction, "while.cmp")
  var whilebody = appendBasicBlockInContext(b.ctx, b.currentfunction, "while.body")
  var whileleave = appendBasicBlockInContext(b.ctx, b.currentfunction, "while.leave")
  discard buildBr(b.builder, whilecmp)

  positionBuilderAtEnd(b.builder, whilecmp)
  var cond = gen_cond(test)
  discard buildCondBr(b.builder, cond, whilebody, whileleave)

  positionBuilderAtEnd(b.builder, whilebody)
  gen(body)

  discard buildBr(b.builder, whilecmp)

  positionBuilderAtEnd(b.builder, whileleave)

proc gen_dowhile*(test: Expr, body: Stmt) =
  var dowhilecmp = appendBasicBlockInContext(b.ctx, b.currentfunction, "dowhile.cmp")
  var dowhilebody = appendBasicBlockInContext(b.ctx, b.currentfunction, "dowhile.body")
  var dowhileleave = appendBasicBlockInContext(b.ctx, b.currentfunction, "dowhile.leave")

  positionBuilderAtEnd(b.builder, dowhilebody)
  gen(body)

  positionBuilderAtEnd(b.builder, dowhilecmp)
  var cond = gen_cond(test)
  discard buildCondBr(b.builder, cond, dowhilebody, dowhileleave)

  positionBuilderAtEnd(b.builder, dowhileleave)

# the compiler may generate a table(array), so require `O(1)` time indexing
proc getOp*(a: BinOP): Opcode =
  case a:
  of token.Add: LLVMAdd
  of token.FAdd: LLVMFAdd
  of token.Sub: LLVMSub
  of token.FSub: LLVMFSub
  of token.Mul: LLVMMul
  of token.FMul: LLVMFMul
  of token.UDiv: LLVMUDiv
  of token.SDiv: LLVMSDiv
  of token.FDiv: LLVMFDiv
  of token.URem: LLVMURem
  of token.SRem: LLVMSRem
  of token.FRem: LLVMFRem
  of token.Shr: LLVMLShr
  of token.AShr: LLVMAShr
  of token.Shl: LLVMShl
  of token.And: LLVMAnd
  of token.Xor: LLVMXor
  of token.Or: LLVMOr
  else: assert(false);cast[Opcode](0)

proc getICmpOp*(a: BinOP): IntPredicate =
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

proc getFCmpOp*(a: BinOP): RealPredicate =
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
    enterScope()
    for i in s.stmts:
      gen(i)
    leaveScope()
  of SExpr:
      discard gen(s.exprbody)
  of SFunction:
      assert s.functy.spec == TYFUNCTION
      var ty = toBackendType(s.functy)
      b.currentfunction = addFunction(b.module, cstring(s.funcname), ty)
      var entry = appendBasicBlockInContext(b.ctx, b.currentfunction, "entry")
      positionBuilderAtEnd(b.builder, entry)
      gen(s.funcbody)
  of SReturn:
      if s.exprbody != nil:
        discard buildRet(b.builder, gen(s.exprbody))
      else:
        discard buildRetVoid(b.builder)
  of SIf:
      if s.elsebody == nil:
        gen_if(s.iftest, s.ifbody)
      else:
        gen_if(s.iftest, s.ifbody, s.elsebody)
  of SWhile:
    gen_while(s.test, s.body)
  of SDoWhile:
    gen_dowhile(s.test, s.body)
  of SDeclOnly:
    discard toBackendType(s.decl)
  of SSemicolon:
    discard
  else:
    assert false

proc gen_cast*(e: Expr, to: CType): Value =
  discard

proc neZero*(a: Expr): Expr =
  Expr(k: EBin, lhs: a, rhs: Expr(k: EBackend, p: cast[pointer](getZero(a.ty))), bop: NE)

proc eqZero*(a: Expr): Expr =
  Expr(k: EBin, lhs: a, rhs: Expr(k: EBackend, p: cast[pointer](getZero(a.ty))), bop: EQ)

proc gen*(e: Expr): Value =
  case e.k:
  of EBackend:
    cast[Value](e.p)
  of EBin:
    case e.bop:
    of EQ..SLE:
      buildICmp(b.builder, getICmpOp(e.bop), gen(e.lhs), gen(e.rhs), "icmp")
    of FEQ..FLE:
      buildFCmp(b.builder, getFCmpOp(e.bop), gen(e.lhs), gen(e.rhs), "fcmp")
    of LogicalAnd:
      let x = buildICmp(b.builder, IntNE, getZero(e.lhs.ty), gen(e.lhs), "land.cmp")
      let y = buildICmp(b.builder, IntNE, getZero(e.lhs.ty), gen(e.rhs), "land.cmp")
      let c = buildAnd(b.builder, x, y, "land.and")
      buildZExt(b.builder, c, backendint(), "lor.zext")
    of LogicalOr:
      let t = buildOr(b.builder, gen(e.lhs), gen(e.rhs), "lor.or")
      let nz = buildICmp(b.builder, IntNE, getZero(e.lhs.ty), t, "lor.cmp")
      buildZExt(b.builder, nz, backendint(), "lor.zext")
    of Comma:
      discard gen(e.lhs)
      gen(e.rhs)
    else:
      buildBinOp(b.builder, getOp(e.bop), gen(e.lhs), gen(e.rhs), "bop")
  of EIntLit:
    gen_int(e.ival.culonglong, e.ty.tags)
  of EFloatLit:
    gen_float(e.fval, e.ty.tags)
  of EUnary:
    case e.uop:
    of token.Pos: gen(e.uoperand)
    of token.Neg: buildNeg(b.builder, gen(e.uoperand), "neg")
    of token.FNeg: buildFNeg(b.builder, gen(e.uoperand), "fneg")
    of token.Not: buildNot(b.builder, gen(e.uoperand), "not")
    of token.AddressOf: nil
    of token.PrefixIncrement: nil
    of token.PrefixDecrement: nil
    of token.Dereference: buildLoad(b.builder, gen(e.uoperand), "deref")
    of token.LogicalNot: buildICmp(b.builder, IntEQ, gen(e.uoperand), getZero(e.ty), "logicnot")
    of UNop: assert false; nil
  of EPostFix:
    nil
  of EVar:
    getVar(e.sval)
  of ECondition:
    gen_condition(e.cond, e.cleft, e.cright)
  of ECast:
    return gen_cast(e.castval, (e.ty))
  of ECall:
    nil
  of ESubscript:
    nil
  of EArray:
    nil

proc jit_error*(msg: string) =
  stderr.writeLine("llvm jit error: " & msg)

proc jit_error*(msg: cstring) =
  stderr.write("llvm jit error: ")
  stderr.writeLine(msg)

proc jit_error*(err: ErrorRef) =
  var msg = getErrorMessage(err)
  stdout.writeLine(msg)
  disposeErrorMessage(msg)

proc orc_error_report*(ctx: pointer; err: ErrorRef) {.cdecl, gcsafe.} =
  jit_error(err)

proc getThreadSafeModule*(): OrcThreadSafeModuleRef =
    result = orcCreateNewThreadSafeModule(b.module, b.tsCtx)
    orcDisposeThreadSafeContext(b.tsCtx)

proc runjit*() =
    var thread_safe_mod = getThreadSafeModule()
    var jit: OrcLLJITRef 
    
    var err = orcCreateLLJIT(addr jit, nil)
    if err != nil:
      jit_error(err)
      return

    orcExecutionSessionSetErrorReporter(orcLLJITGetExecutionSession(jit), orc_error_report, nil)

    var prefix = orcLLJITGetGlobalPrefix(jit)

    var gen1: OrcDefinitionGeneratorRef
    var gen2: OrcDefinitionGeneratorRef
    err = orcCreateDynamicLibrarySearchGeneratorForProcess(addr gen1, prefix, nil, nil)

    err = orcCreateDynamicLibrarySearchGeneratorForPath(addr gen2, "/lib/x86_64-linux-gnu/libc.so.6", prefix, nil, nil)
    if err != nil:
      jit_error(err)
      return

    var jd = orcLLJITGetMainJITDylib(jit)
    orcJITDylibAddGenerator(jd, gen1)
    orcJITDylibAddGenerator(jd, gen2)

    err = orcLLJITAddLLVMIRModule(jit, jd, thread_safe_mod)
    if err != nil:
      jit_error(err)
      return

    var main: OrcExecutorAddress = 0
    
    err = orcLLJITLookup(jit, addr main, "main")
    if err != nil:
      jit_error(err)
      return

    if main == 0:
      jit_error("cannot find main function")
      return

    type MainTY = proc (argc: cint, argv: ptr cstring): cint {.cdecl, gcsafe.}

    let fmain = cast[MainTY](main)

    var realargs = @["main", "--version"]
    var argslen = len(realargs)
    var mainargs = create(cstring, argslen or 1)
    var arr = cast[ptr UncheckedArray[cstring]](mainargs)

    for i in 0..<argslen:
      arr[i] = cstring(realargs[i])

    let ret = fmain(cint(argslen), mainargs)

    dealloc(mainargs)

    verbose("main() return: " & $ret)

    err = orcDisposeLLJIT(jit)
    if err != nil:
      jit_error(err)
      return

