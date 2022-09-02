## LLVM Backend
##
## LLVM-15 C API
##
## https://llvm.org/docs/OpaquePointers.html
##
## since `sizeof` is contant expression (operand cannot be VLA), so `LLVMSizeOfTypeInBits()` and `LLVMStoreSizeOfType()`
## should called when eval constant expression or let compiler give it a assumption(for example, `_Bool` and `char` is 1 bytes, `int32` is 4 bytes ...)
##
## in LLVM, interger type has bits, so it is not necessarily to call LLVM API, but struct and bool type may has align bits or storage size  for a LLVM target
##
## the main export function is `gen()`
##
## application should call setBackend to initialize backend, call `app.getSizeof()` to get sizeof CType, and call `gen()` to build whole *translation-unit* to single LLVM Module
##
## when there no need for calling `gen()` function, call `shutdownBackend()` to shutdown LLVM
##
## `runjit()` will run module.

import std/[tables, exitprocs]
import core, parser


type
  OpaqueMemoryBuffer*{.pure, final.} = object
  OpaqueAttributeRef*{.pure, final.} = object
  OpaqueContext*{.pure, final.} = object
  OpaqueModule*{.pure, final.} = object
  OpaqueType*{.pure, final.} = object
  OpaqueValue*{.pure, final.} = object
  OpaqueBasicBlock*{.pure, final.} = object
  OpaqueBuilder*{.pure, final.} = object
  OpaqueModuleProvider*{.pure, final.} = object
  OpaquePassManager*{.pure, final.} = object
  OpaquePassRegistry*{.pure, final.} = object
  OpaqueUse*{.pure, final.} = object
  OpaqueDiagnosticInfo*{.pure, final.} = object
  OpaqueTargetMachine*{.pure, final.} = object
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
  ErrorRef* = pointer
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
  TargetDataRef* = distinct pointer
  TargetLibraryInfoRef* = distinct pointer
  Opcode {.pure, size: sizeof(cint).} = cint
  DIFlags* = cint
  DWARFTypeEncoding* = cuint
  MetadataKind* = cuint
  ByteOrdering* {.size: sizeof(cint).} = enum
    BigEndian, LittleEndian
  TargetMachineRef* = distinct pointer
  PassManagerBuilderRef* = distinct pointer
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
# include llvm/Transforms/Scalar

# jit headers
include llvm/Orc
include llvm/LLJIT

#  initializeNativeTarget
#  initializeNativeAsmPrinter
include LLVMTarget

proc typeOfX*(val: ValueRef): TypeRef {.importc: "LLVMTypeOf".}

proc constIntIsZero*(constantVal: ValueRef): Bool {.importc: "LLVMConstIntIsZero".}

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

type
  Value* = ValueRef ## LLVM Value
  Type* = TypeRef ## LLVM Type
  Label* = BasicBlockRef ## LLVM block

type
    Backend* = ref object
      # jump labels
      labels*: seq[TableRef[string, Label]]
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
      topBreak*: Label
      topContinue*: Label
      target*: TargetRef

var b*: Backend

proc llvm_error*(msg: string) =
  stderr.writeLine("llvm error: " & msg)

proc llvm_error*(msg: cstring) =
  stderr.write(msg)

proc wrap*(ty: CType): Type

proc wrap2*(ty: CType): Type

proc llvmGetAlignOf*(ty: CType): culonglong =
  aBIAlignmentOfType(b.layout, wrap2(ty))

proc llvmGetsizeof*(ty: CType): culonglong =
  storeSizeOfType(b.layout, wrap2(ty))

proc llvmGetOffsetof*(ty: CType, idx: int): culonglong =
  offsetOfElement(b.layout, wrap(ty), cuint(idx))

proc newBackend*(module_name: string = "main", source_file: string = "main") =
  initializeCore(getGlobalPassRegistry())
  initializeNativeTarget()
  initializeNativeAsmParser()
  initializeNativeAsmPrinter()
  initializeAllAsmParsers()
  b = Backend(
    tsCtx: orcCreateNewThreadSafeContext(),
    builder: createBuilder()
  )
  b.ctx = orcThreadSafeContextGetContext(b.tsCtx)
  contextSetDiscardValueNames(b.ctx, True)
  contextSetOpaquePointers(b.ctx, if app.opaquePointerEnabled: True else: False)
  b.module = moduleCreateWithNameInContext(module_name, b.ctx)
  setSourceFileName(b.module, source_file, source_file.len.csize_t)
  

proc initTarget*() =
  var err: cstring
  if app.triple.len == 0:
    app.triple.add(getDefaultTargetTriple())
  if getTargetFromTriple(app.triple.cstring, addr b.target, cast[cstringArray](err)) == True:
    llvm_error(err)
    app.triple = $getDefaultTargetTriple()
    discard getTargetFromTriple(app.triple.cstring, addr b.target, cast[cstringArray](err))
  b.machine = createTargetMachine(b.target, app.triple.cstring, "", "", CodeGenLevelAggressive, RelocPIC, CodeModelDefault)
  b.layout = createTargetDataLayout(b.machine)
  setModuleDataLayout(b.module, b.layout)
  setTarget(b.module, app.triple.cstring)
  app.pointersize = pointerSize(b.layout)

proc dumpVersionInfo*() =
  var arr = [cstring("llvm"), cstring("--version")]
  parseCommandLineOptions(2, cast[cstringArray](addr arr[0]), nil)

proc handle_asm(s: string) =
  if b.currentfunction == nil:
    # module level asm
    appendModuleInlineAsm(b.module, cstring(s), len(s).csize_t)
  else:
    var asmtype = functionType(voidTypeInContext(b.ctx), nil, 0, False)
    var f = getInlineAsm(asmtype, cstring(s), len(s).csize_t, nil, 0, True, False, InlineAsmDialectATT, False)
    var c = buildCall2(b.builder, asmtype, f, nil, 0, "") 
    setTailCall(c, True)

proc setBackend*() =
  app.getSizeof = llvmGetsizeof
  app.getoffsetof = llvmGetOffsetof
  app.getAlignOf = llvmGetAlignOf
  newBackend()

proc enterScope*() =
  ## not token.enterBlock
  b.labels.add(newTable[string, Label]())
  b.tags.add(newTable[string, Type]())
  b.vars.add(newTable[string, Value]())

proc leaveScope*() =
  ## not token.leaveBlock
  discard b.tags.pop()
  discard b.labels.pop()
  discard b.vars.pop()

proc getVar*(name: string): Value =
  for i in countdown(len(b.vars)-1, 0):
    result = b.vars[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc putVar*(name: string, val: Value) =
  b.vars[^1][name] = val

proc getLabel*(name: string): Label =
  for i in countdown(len(b.labels)-1, 0):
    result = b.labels[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc putLabel*(name: string, label: Label) =
  b.labels[^1][name] = label

proc getTags*(name: string): Type =
  for i in countdown(len(b.tags)-1, 0):
    result = b.tags[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc putTags*(name: string, t: Type) =
  b.tags[^1][name] = t

proc shutdownBackend*() =
  shutdown()

proc optimize*() =
  var passM = createPassManager()
  var pb = passManagerBuilderCreate()
  # addInstructionCombiningPass(passM)
  # addReassociatePass(passM)
  # addGVNPass(passM)
  # addCFGSimplificationPass(passM)
  passManagerBuilderSetSizeLevel(pb, app.sizeLevel)
  passManagerBuilderSetOptLevel(pb, app.optLevel)
  if app.inlineThreshold != 0:
    passManagerBuilderUseInlinerWithThreshold(pb, app.inlineThreshold)
  
  passManagerBuilderPopulateModulePassManager(pb, passM)
  discard runPassManager(passM, b.module)

  passManagerBuilderDispose(pb)
  disposePassManager(passM)

proc verify*() =
  var err: cstring
  discard verifyModule(b.module, PrintMessageAction, cast[cstringArray](addr err))

proc writeModuleToFile*(path: string) =
  var err: cstring = ""
  if printModuleToFile(b.module, path, cast[cstringArray](addr err)) == True:
    llvm_error("LLVMPrintModuleToFile")
    llvm_error(err)

proc writeBitcodeToFile*(path: string) =
  if writeBitcodeToFile(b.module, path) != 0:
    llvm_error("LLVMWriteBitcodeToFile")

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
  if bool(a.ty.tags and TYBOOL):
    gen(a)
  else:
    let lhs = gen(a)
    let rhs = constNull(wrap(a.ty))
    buildICmp(b.builder, IntNE, lhs, rhs, "")

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
        unreachable()
        nil

proc gen_float*(f: float, tag: uint32): Value =
  constReal(if (tag and TYFLOAT) != 0: floatTypeInContext(b.ctx) else: doubleTypeInContext(b.ctx), f)

proc gen_str*(val: string, ty: var Type): Value =
  var gstr = constStringInContext(b.ctx, cstring(val), len(val).cuint, False)
  ty = typeOfX(gstr)
  result = addGlobal(b.module, ty, "")
  setGlobalConstant(result, True)
  setLinkage(result, PrivateLinkage)
  setInitializer(result, gstr)

proc gen_str_ptr*(val: string): Value =
  var ty: Type
  var s = gen_str(val, ty)
  var zero = constInt(int32Type(), 0, False)
  var indices = [zero, zero]
  buildInBoundsGEP2(b.builder, pointerType(ty, 0), s, addr indices[0], 2, "")

proc backendint*(): Type =
    ## note: we do not used `intTypeInContext()` for int
    if TYINT == TYINT32:
      int32TypeInContext(b.ctx)
    else:
      int64TypeInContext(b.ctx)

proc load*(p: Value, t: Type): Value =
  buildLoad2(b.builder, t, p, "load")

proc store*(p: Value, v: Value) =
  discard buildStore(b.builder, v, p)

proc wrap2*(ty: CType): Type =
  case ty.spec:
  of TYPRIM:
    return (
      if (ty.tags and (TYINT8 or TYUINT8)) != 0:
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
      elif (ty.tags and TYVOID) != 0:
          voidTypeInContext(b.ctx)
      else:
        unreachable()
        nil
      )
  of TYPOINTER:
    return pointerTypeInContext(b.ctx, 0)
  of TYSTRUCT, TYUNION:
    let l = len(ty.selems)
    var buf = create(Type, l or 1)
    var arr = cast[ptr UncheckedArray[Type]](buf)
    for i in 0 ..< l:
      if ty.selems[i][1].spec == TYBITFIELD:
        arr[i] = intTypeInContext(b.ctx, cuint(ty.selems[i][1].bitsize))
      else:
        arr[i] = wrap2(ty.selems[i][1])
    return structTypeInContext(b.ctx, buf, cuint(l), False)
  of TYARRAY:
    return arrayType(wrap(ty.arrtype), cuint(ty.arrsize))
  of TYINCOMPLETE, TYBITFIELD, TYENUM, TYFUNCTION:
    unreachable()

proc wrap*(ty: CType): Type =
  ## wrap a CType to LLVM Type
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
      elif (ty.tags and TYVOID) != 0:
          voidTypeInContext(b.ctx)
      else:
        unreachable()
        nil
      )
  of TYPOINTER:
    if app.opaquePointerEnabled:
      return pointerTypeInContext(b.ctx, 0)
    else:
      # `return pointerType(voidTypeInContext(b.ctx), 0)`
      # clang use `i8*` for `void*` and `char*`
      return pointerType(int8TypeInContext(b.ctx), 0)
  of TYSTRUCT, TYUNION:
    if len(ty.sname) > 0:
      # try to find old definition or declaration
      result = getTags(ty.sname)
      if result != nil:
        if isOpaqueStruct(result) == False:
          # the struct has call setBody, do nothing
          return result
      else:
        # the struct name is not created
        # create a opaque struct
        result = structCreateNamed(b.ctx, cstring(ty.sname))
      let l = len(ty.selems)
      var buf = create(Type, l or 1)
      var arr = cast[ptr UncheckedArray[Type]](buf)
      for i in 0 ..< l:
        if ty.selems[i][1].spec == TYBITFIELD:
          arr[i] = intTypeInContext(b.ctx, cuint(ty.selems[i][1].bitsize))
        else:
          arr[i] = wrap(ty.selems[i][1])
      # set struct body
      structSetBody(result, buf, cuint(l), False)
      dealloc(buf)
      # record it
      putTags(ty.sname, result)
      return result

    let l = len(ty.selems)
    var buf = create(Type, l or 1)
    var arr = cast[ptr UncheckedArray[Type]](buf)
    for i in 0 ..< l:
      arr[i] = wrap(ty.selems[i][1])
    result = structTypeInContext(b.ctx, buf, cuint(l), False)
  of TYFUNCTION:
    let l = len(ty.params)
    var buf = create(Type, l or 1)
    var arr = cast[ptr UncheckedArray[Type]](buf)
    var ivarargs = false
    var i = 0.cuint
    while true:
      if i == len(ty.params).cuint:
        break
      if ty.params[i][1] == nil:
        ivarargs = true
        break
      if ty.params[i][1].spec == TYBITFIELD:
        arr[i] = wrap(ty.params[i][1].bittype)
      else:
        arr[i] = wrap(ty.params[i][1])
      inc i
    result = functionType(wrap(ty.ret), buf, i, if ivarargs: True else: False)
    dealloc(buf)  
    return result
  of TYARRAY:
    return arrayType(wrap(ty.arrtype), cuint(ty.arrsize))
  of TYENUM:
    return backendint()
  of TYINCOMPLETE:
    case ty.tag:
    of TYSTRUCT:
      result = getTags(ty.sname)
      if result != nil:
        return result
      return structCreateNamed(b.ctx, cstring(ty.sname))
    of TYUNION:
      result = getTags(ty.sname)
      if result != nil:
        return result
      return structCreateNamed(b.ctx, cstring(ty.sname))
    of TYENUM:
      return backendint()
    else:
      unreachable()
  of TYBITFIELD:
    unreachable()
    return nil

proc getZero*(ty: CType): Value =
  constNull(wrap(ty))

proc getOne*(ty: CType): Value =
  assert ty.spec == TYPRIM
  constInt(wrap(ty), 1, False)

proc gen_condition*(test: Expr, lhs: Expr, rhs: Expr): Value =
  ## build a `cond ? lhs : rhs` expression
  var ty = wrap(lhs.ty)
  var iftrue = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var iffalse = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var ifend = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  discard buildCondBr(b.builder, gen_cond(test), iftrue, iffalse)
  
  positionBuilderAtEnd(b.builder, iftrue)
  var left = gen(lhs)
  discard buildBr(b.builder, ifend)
  
  positionBuilderAtEnd(b.builder, iffalse)
  var right =gen (rhs)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, ifend)
  var phi = buildPhi(b.builder, ty, "")

  var blocks = [iftrue, iffalse]
  var values = [left, right]
  addIncoming(phi, addr values[0], addr blocks[0], 2)
  return phi

proc gen_condition*(test: Expr, lhs: Value, rhs: Expr): Value =
  ## build a `cond ? lhs : rhs` expression
  var ty = wrap(rhs.ty)
  var iftrue = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var iffalse = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var ifend = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  discard buildCondBr(b.builder, gen_cond(test), iftrue, iffalse)
  
  positionBuilderAtEnd(b.builder, iftrue)
  var left = (lhs)
  discard buildBr(b.builder, ifend)
  
  positionBuilderAtEnd(b.builder, iffalse)
  var right = gen(rhs)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, ifend)
  var phi = buildPhi(b.builder, ty, "")

  var blocks = [iftrue, iffalse]
  var values = [left, right]
  addIncoming(phi, addr values[0], addr blocks[0], 2)
  return phi

proc gen_condition*(test: Expr, lhs: Expr, rhs: Value): Value =
  ## build a `cond ? lhs : rhs` expression
  var ty = wrap(lhs.ty)
  var iftrue = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var iffalse = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var ifend = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  discard buildCondBr(b.builder, gen_cond(test), iftrue, iffalse)
  
  positionBuilderAtEnd(b.builder, iftrue)
  var left = gen(lhs)
  discard buildBr(b.builder, ifend)
  
  positionBuilderAtEnd(b.builder, iffalse)
  var right = (rhs)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, ifend)
  var phi = buildPhi(b.builder, ty, "")

  var blocks = [iftrue, iffalse]
  var values = [left, right]
  addIncoming(phi, addr values[0], addr blocks[0], 2)
  return phi


proc gen_if*(test: Expr, body: Stmt) =
  var iftrue = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var ifend = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  discard buildCondBr(b.builder, gen_cond(test), iftrue, ifend)

  positionBuilderAtEnd(b.builder, iftrue)
  gen(body)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, ifend)

proc gen_if*(test: Expr, body: Stmt, elsebody: Stmt) =
  var iftrue = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var iffalse = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var ifend = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  discard buildCondBr(b.builder, gen_cond(test), iftrue, iffalse)

  positionBuilderAtEnd(b.builder, iftrue)
  gen(body)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, iffalse)
  gen(elsebody)
  discard buildBr(b.builder, ifend)

  positionBuilderAtEnd(b.builder, ifend)

proc gen_while*(test: Expr, body: Stmt) =
  var old_break = b.topBreak
  var old_continue = b.topContinue

  var whilecmp = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var whilebody = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var whileleave = appendBasicBlockInContext(b.ctx, b.currentfunction, "")

  b.topBreak = whileleave
  b.topContinue = whilecmp

  discard buildBr(b.builder, whilecmp)

  positionBuilderAtEnd(b.builder, whilecmp)
  var cond = gen_cond(test)
  discard buildCondBr(b.builder, cond, whilebody, whileleave)

  positionBuilderAtEnd(b.builder, whilebody)
  gen(body)

  discard buildBr(b.builder, whilecmp)

  positionBuilderAtEnd(b.builder, whileleave)

  b.topBreak = old_break
  b.topContinue = old_continue

proc gen_for*(test: Expr, body: Stmt, sforinit: Stmt, eforincl: Expr) =
  var old_break = b.topBreak
  var old_continue = b.topContinue

  if sforinit != nil:
    gen(sforinit)
  enterScope()
  var forcmp = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var forbody = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var forleave = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var forincl = appendBasicBlockInContext(b.ctx, b.currentfunction, "")

  b.topBreak = forleave
  b.topContinue = forincl

  discard buildBr(b.builder, forcmp)

  # for.cmp
  positionBuilderAtEnd(b.builder, forcmp)
  var cond = gen_cond(test)
  discard buildCondBr(b.builder, cond, forbody, forleave)

  # for.body
  positionBuilderAtEnd(b.builder, forbody)
  gen(body)
  discard buildBr(b.builder, forincl)

  # for.incl
  positionBuilderAtEnd(b.builder, forincl)
  if eforincl != nil:
    discard gen(eforincl)
  discard buildBr(b.builder, forcmp)
  positionBuilderAtEnd(b.builder, forleave)
  leaveScope()

  b.topBreak = old_break
  b.topContinue = old_continue

proc gen_dowhile*(test: Expr, body: Stmt) =
  var old_break = b.topBreak
  var old_continue = b.topContinue

  var dowhilecmp = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var dowhilebody = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var dowhileleave = appendBasicBlockInContext(b.ctx, b.currentfunction, "")

  b.topBreak = dowhileleave
  b.topContinue = dowhilecmp

  positionBuilderAtEnd(b.builder, dowhilebody)
  gen(body)

  positionBuilderAtEnd(b.builder, dowhilecmp)
  var cond = gen_cond(test)
  discard buildCondBr(b.builder, cond, dowhilebody, dowhileleave)

  positionBuilderAtEnd(b.builder, dowhileleave)
  b.topBreak = old_break
  b.topContinue = old_continue

# the compiler may generate a table(array), so require `O(1)` time indexing
proc getOp*(a: BinOP): Opcode =
  case a:
  of UAdd: LLVMAdd
  of FAdd: LLVMFAdd
  of USub: LLVMSub
  of FSub: LLVMFSub
  of UMul: LLVMMul
  of FMul: LLVMFMul
  of UDiv: LLVMUDiv
  of SDiv: LLVMSDiv
  of FDiv: LLVMFDiv
  of URem: LLVMURem
  of SRem: LLVMSRem
  of FRem: LLVMFRem
  of Shr: LLVMLShr
  of AShr: LLVMAShr
  of Shl: LLVMShl
  of And: LLVMAnd
  of Xor: LLVMXor
  of Or: LLVMOr
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

proc getCastOp*(a: CastOp): Opcode =
  case a:
  of Trunc:
    LLVMTrunc
  of ZExt:
    LLVMZExt
  of SExt:
    LLVMSExt
  of FPToUI:
    LLVMFPToUI
  of FPToSI:
    LLVMFPToSI
  of UIToFP:
    LLVMUIToFP
  of SIToFP:
    LLVMSIToFP
  of FPTrunc:
    LLVMFPTrunc
  of FPExt:
    LLVMFPExt
  of PtrToInt:
    LLVMPtrToInt
  of IntToPtr:
    LLVMIntToPtr
  of BitCast:
    LLVMBitCast

proc newFunction*(varty: CType, name: string): Value =
    result = b.vars[0].getOrDefault(name, nil)
    if result != nil:
      return result
    var fty = wrap(varty)
    result = addFunction(b.module, name.cstring, fty)
    b.vars[0][name] = result
    for i in 0 ..< len(varty.params):
      if varty.params[i][1] == nil:
        break
      var pa = getParam(result, i.cuint)
      setValueName2(pa, cstring(varty.params[i][0]), varty.params[i][0].len.csize_t)

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
      enterScope()
      assert s.functy.spec == TYFUNCTION
      var ty = wrap(s.functy)
      b.currentfunction = newFunction(s.functy, s.funcname)
      var entry = appendBasicBlockInContext(b.ctx, b.currentfunction, "entry")
      positionBuilderAtEnd(b.builder, entry)
      var paramLen = countParamTypes(ty)
      if paramLen > 0:
        var fparamsTypes = create(Type, paramLen)
        var typesarr = cast[ptr UncheckedArray[Type]](fparamsTypes)
        getParamTypes(ty, fparamsTypes)
        var i = 0
        var iter = getFirstParam(b.currentfunction)
        while iter != nil:
          var p = buildAlloca(b.builder, typesarr[i], cstring(s.functy.params[i][0]))
          store(p, iter)
          putVar(s.functy.params[i][0], p)
          iter = getNextParam(iter)
          inc i
        dealloc(fparamsTypes)
      for i in s.funcbody.stmts:
        gen(i)
      leaveScope()
  of SReturn:
      if s.exprbody != nil:
        discard buildRet(b.builder, gen(s.exprbody))
        # ret void if gen(s.exprbody) == nil
      else:
        discard buildRetVoid(b.builder)
  of SIf:
      if s.elsebody == nil:
        gen_if(s.iftest, s.ifbody)
      else:
        gen_if(s.iftest, s.ifbody, s.elsebody)
  of SWhile:
    enterScope()
    gen_while(s.test, s.body)
    leaveScope()
  of SDoWhile:
    enterScope()
    gen_dowhile(s.test, s.body)
    leaveScope()
  of SFor:
    enterScope()
    gen_for(s.forcond, s.forbody, s.forinit, s.forincl)
    leaveScope()
  of SDeclOnly:
    discard wrap(s.decl)
  of SLabled:
    var ib = appendBasicBlockInContext(b.ctx, b.currentfunction, cstring(s.label))
    positionBuilderAtEnd(b.builder, ib)
    putLabel(s.label, ib)
    gen(s.labledstmt)
  of SGoto:
    let loc = getLabel(s.location)
    if loc == nil:
      error("cannot find position for label: " & s.location)
    else:
      discard buildbr(b.builder, loc)
  of SSemicolon:
    discard
  of SContinue:
    discard buildBr(b.builder, b.topContinue)
  of SBreak:
    discard buildBr(b.builder, b.topBreak)
  of SAsm:
    handle_asm(s.asms)
  of SSwitch:
    unreachable()
  of SDefault:
    unreachable()
  of SCase:
    unreachable()
  of SVarDecl:
    # TODO: top-level fuction decl and def
    # TODO: global, extern handling
    for (name, varty, init) in s.vars:
      # https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html#function-code-generation
      # code generation for function prototypes
      if varty.spec == TYFUNCTION:
        discard newFunction(varty, name)
      else:
        var ty = wrap(varty)
        if b.currentfunction == nil:
          var g = addGlobal(b.module, ty, cstring(name))
          if init == nil:
            setInitializer(g, constNull(ty))
          else:
            var i = gen(init)
            if isConstant(i) == False:
              llvm_error("global initializer is not constant")
            setInitializer(g, i)
          if (varty.tags and TYTHREAD_LOCAL) != 0:
            # LLVMSetThreadLocalMode ?
            setThreadLocal(g, 1)
          if (varty.tags and TYSTATIC) != 0:
            setLinkage(g, InternalLinkage)
          elif (varty.tags and TYEXTERN) != 0:
            setExternallyInitialized(g, True)
            setLinkage(g, ExternalLinkage)
          else:
            if (varty.tags and TYREGISTER) != 0:
              warning("register is not supported in LLVM backend")
          # setLinkage(g, CommonLinkage)
          putVar(name, g)
        else:
          var val = buildAlloca(b.builder, ty, cstring(name))
          if init != nil:
            let initv = gen(init)
            store(val, initv)
          putVar(name, val)
  of SVarDecl1:
    unreachable()

proc gen_cast*(e: Expr, to: CType, op: CastOp): Value

proc neZero*(a: Expr): Expr =
  Expr(k: EBin, lhs: a, rhs: Expr(k: EBackend, p: cast[pointer](getZero(a.ty))), bop: NE)

proc eqZero*(a: Expr): Expr =
  Expr(k: EBin, lhs: a, rhs: Expr(k: EBackend, p: cast[pointer](getZero(a.ty))), bop: EQ)

proc incl*(p: Value, t: Type) =
  var l = load(p, t)
  var l2 = buildAdd(b.builder, l, constInt(t, 1.culonglong, False), "")
  store(p, l2)

proc decl*(p: Value, t: Type) =
  var l = load(p, t)
  var l2 = buildSub(b.builder, l, constInt(t, 1.culonglong, False), "")
  store(p, l2)

proc getAddress*(e: Expr): Value =
  # return address
  case e.k:
  of EVar:
    getVar(e.sval)
  of EPointerMemberAccess:
    var basep = gen(e.obj)
    var r = constInt(int32Type(), e.idx.culonglong, False)
    buildInBoundsGEP2(b.builder, pointerType(wrap(e.ty), 0), basep, addr r, 1, "")
  of EMemberAccess:
    var basep = getAddress(e.obj)
    var r = constInt(int32Type(), e.idx.culonglong, False)
    buildInBoundsGEP2(b.builder, pointerType(wrap(e.ty), 0), basep, addr r, 1, "")
  of EPostFix:
    case e.pop:
    of PostfixIncrement:
      var basep = getAddress(e.poperand)
      incl(basep, wrap(e.ty))
      basep
    of PostfixDecrement:
      var basep = getAddress(e.poperand)
      decl(basep, wrap(e.ty))
      basep
  of ESubscript:
    assert e.left.ty.spec == TYPOINTER # the left must be a pointer
    var ty = wrap(e.left.ty.p) # get pointer element type
    var v = getAddress(e.left) # lookup lvalue
    var basep = load(v, ty) # load address in lvalue
    var r = gen(e.right) # get index
    buildInBoundsGEP2(b.builder, pointerType(ty, 0), basep, addr r, 1, "")
  else:
    unreachable()
    nil

proc getPointerElementType*(t: Type): Type =
  getElementType(t)

proc gen*(e: Expr): Value =
  case e.k:
  of EUndef:
    getUndef(wrap(e.ty))
  of ArrToAddress:
    var arr = getAddress(e.voidexpr)
    var ty = wrap(e.ty.p)
    var idx = [constInt(int32Type(), 0, False), constInt(int32Type(), 0, False)]
    buildInBoundsGEP2(b.builder, ty, arr, addr idx[0], 2, "")
  of ESubscript:
    assert e.left.ty.spec == TYPOINTER # the left must be a pointer
    var ty = wrap(e.left.ty.p) # get pointer element type
    var v = getAddress(e.left) # lookup lvalue
    var basep = load(v, ty) # load address in lvalue
    var r = gen(e.right) # get index
    var gaddr = buildInBoundsGEP2(b.builder, pointerType(ty, 0), basep, addr r, 1, "")
    load(gaddr, ty) # return lvalue
  of EMemberAccess:
    buildExtractValue(b.builder, gen(e.obj), e.idx.cuint, "")
  of EPointerMemberAccess:
    load(getAddress(e), wrap(e.ty))
  of EString:
    gen_str_ptr(e.str)
  of EBackend:
    cast[Value](e.p)
  of EBin:
    case e.bop:
    of Assign:
      var v = gen(e.rhs)
      let basep = getAddress(e.lhs)
      store(basep, v)
      load(basep, wrap(e.ty))
    of SAdd:
      var l = gen(e.lhs)
      var r = gen(e.rhs)
      buildNSWAdd(b.builder, l, r, "")
    of SSub:
      var l = gen(e.lhs)
      var r = gen(e.rhs)
      buildNSWSub(b.builder, l, r, "")
    of SMul:
      var l = gen(e.lhs)
      var r = gen(e.rhs)
      buildNSWSub(b.builder, l, r, "")
    of SAddP:
      var l = gen(e.lhs)
      var r = gen(e.rhs)
      buildInBoundsGEP2(b.builder, wrap(e.ty), l, addr r, 1, "")
    of EQ..SLE:
      buildICmp(b.builder, getICmpOp(e.bop), gen(e.lhs), gen(e.rhs), "")
    of FEQ..FLE:
      buildFCmp(b.builder, getFCmpOp(e.bop), gen(e.lhs), gen(e.rhs), "")
    of LogicalAnd:
      let x = buildICmp(b.builder, IntNE, getZero(e.lhs.ty), gen(e.lhs), "")
      let y = buildICmp(b.builder, IntNE, getZero(e.lhs.ty), gen(e.rhs), "")
      let c = buildAnd(b.builder, x, y, "")
      buildZExt(b.builder, c, backendint(), "")
    of LogicalOr:
      let t = buildOr(b.builder, gen(e.lhs), gen(e.rhs), "")
      let nz = buildICmp(b.builder, IntNE, getZero(e.lhs.ty), t, "")
      buildZExt(b.builder, nz, backendint(), "")
    of Comma:
      discard gen(e.lhs)
      gen(e.rhs)
    else:
      buildBinOp(b.builder, getOp(e.bop), gen(e.lhs), gen(e.rhs), "")
  of EIntLit:
    gen_int(e.ival.culonglong, e.ty.tags)
  of EFloatLit:
    gen_float(e.fval, e.ty.tags)
  of EVoid:
    gen(e.voidexpr)
  of EUnary:
    case e.uop:
    of Pos: gen(e.uoperand)
    of SNeg: buildNSWNeg(b.builder, gen(e.uoperand), "")
    of UNeg: buildNeg(b.builder, gen(e.uoperand), "")
    of FNeg: buildFNeg(b.builder, gen(e.uoperand), "")
    of Not: buildNot(b.builder, gen(e.uoperand), "")
    of AddressOf: getAddress(e.uoperand)
    of PrefixIncrement: 
      var ty = wrap(e.ty)
      let basep = getAddress(e.uoperand)
      var l = load(basep, ty)
      var l2 = buildAdd(b.builder, l, constInt(ty, 1.culonglong, False), "")
      store(basep, l2)
      load(basep, ty)
    of PrefixDecrement:
      var ty = wrap(e.ty)
      let basep = getAddress(e.uoperand)
      var l = load(basep, ty)
      var l2 = buildSub(b.builder, l, constInt(ty, 1.culonglong, False), "")
      store(basep, l2)
      load(basep, ty)
    of Dereference: buildLoad2(b.builder, wrap(e.ty), gen(e.uoperand), "")
    of LogicalNot: buildICmp(b.builder, IntEQ, gen(e.uoperand), getZero(e.ty), "")
  of EPostFix:
    case e.pop:
    of PostfixIncrement:
      var basep = getAddress(e.poperand)
      incl(basep, wrap(e.ty))
      load(basep, wrap(e.ty))
    of PostfixDecrement:
      var basep = getAddress(e.poperand)
      decl(basep, wrap(e.ty))
      load(basep, wrap(e.ty))
  of EVar:
    var pvar = getVar(e.sval)
    assert pvar != nil
    load(pvar, wrap(e.ty))
  of ECondition:
    gen_condition(e.cond, e.cleft, e.cright)
  of ECast:
    gen_cast(e.castval, e.ty, e.castop)
  of EDefault:
    if (e.ty.tags and TYVOID) != 0:
      nil
    else:
      constNull(wrap(e.ty))
  of ECall:
    var ty = wrap(e.callfunc.ty)
    var f = getAddress(e.callfunc)
    var l = len(e.callargs)
    var args = create(Value, l or 1)
    var arr = cast[ptr UncheckedArray[Value]](args)
    for i in 0 ..< l:
      arr[i] = gen(e.callargs[i]) # eval argument from left to right
    var res = buildCall2(b.builder, ty, f, args, l.cuint, "")
    dealloc(args)
    setTailCall(res, True)
    res
  of EStruct:
    var ty = wrap(e.ty)
    if len(e.arr) == 0:
      constNull(ty)
    else:
      # TODO: bit field, padding
      var l = len(e.arr)
      var inits = create(Value, l)
      var arr = cast[ptr UncheckedArray[Value]](inits)
      for i in 0 ..< l:
        arr[i] = gen(e.arr[i])
      var ret = constNamedStruct(ty, inits, cuint(l))
      dealloc(inits)
      ret
  of EArray:
    assert e.ty.arrsize == len(e.arr)
    var elemTy = wrap(e.ty.arrtype)
    var ty = arrayType(elemTy, cuint(e.ty.arrsize))
    if len(e.arr) == 0:
      constNull(ty)
    else:
        var l = len(e.arr)
        var inits = create(Value, l)
        var arr = cast[ptr UncheckedArray[Value]](inits)
        for i in 0 ..< l:
          arr[i] = gen(e.arr[i])
        var ret = constArray(elemTy, inits, cuint(l))
        dealloc(inits)
        ret

proc gen_cast*(e: Expr, to: CType, op: CastOp): Value =
  var c = gen(e)
  buildCast(b.builder, getCastOp(op), c, wrap(to), "")

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
    if targetHasJIT(b.target) == False:
      llvm_error("this target has no JIT!")
      return
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

    when defined(windows):
      const crtpath = r"C:\Windows\System32\kernel32.dll"
    else:
      const crtpath = "/lib/x86_64-linux-gnu/libc.so.6"
    err = orcCreateDynamicLibrarySearchGeneratorForPath(addr gen2, crtpath, prefix, nil, nil)
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

    var realargs = @["main"]
    # TODO: use command line args from CLI options
    var argslen = len(realargs)
    var mainargs = create(cstring, argslen or 1)
    var arr = cast[ptr UncheckedArray[cstring]](mainargs)

    for i in 0..<argslen:
      arr[i] = cstring(realargs[i])

    let ret = fmain(cint(argslen), mainargs)

    dealloc(mainargs)

    exitprocs.setProgramResult(ret)

    verbose("main() return: " & $ret)

    err = orcDisposeLLJIT(jit)
    if err != nil:
      jit_error(err)
      return
