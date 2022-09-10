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
import config, core, parser, builtins, ast, operators, types

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

import LLVMInstrinsics
import LLVMAttribute

const
  False*: Bool = 0
  True*: Bool = 1

# LLVM Core
include llvm/Types
include llvm/Support
include llvm/Error
include llvm/Core

#LLVMParseBitcodeInContext
include llvm/BitReader

# LLVMParseIRInContext
include llvm/IRReader

# writing LLVM IR
# writeBitcodeToFile
include llvm/BitWriter

# LLVMLinkModules2
include llvm/Linker

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
# include LLVMTarget

proc setModuleDataLayout*(m: ModuleRef; dl: TargetDataRef) {.
    importc: "LLVMSetModuleDataLayout".}
proc createTargetData*(stringRep: cstring): TargetDataRef {.
    importc: "LLVMCreateTargetData".}
proc disposeTargetData*(td: TargetDataRef) {.importc: "LLVMDisposeTargetData",
.}
proc addTargetLibraryInfo*(tli: TargetLibraryInfoRef; pm: PassManagerRef) {.
    importc: "LLVMAddTargetLibraryInfo".}
proc copyStringRepOfTargetData*(td: TargetDataRef): cstring {.
    importc: "LLVMCopyStringRepOfTargetData".}
proc byteOrder*(td: TargetDataRef): ByteOrdering {.importc: "LLVMByteOrder",
.}
proc pointerSize*(td: TargetDataRef): cuint {.importc: "LLVMPointerSize",
.}
proc pointerSizeForAS*(td: TargetDataRef; `as`: cuint): cuint {.
    importc: "LLVMPointerSizeForAS".}
proc intPtrType*(td: TargetDataRef): TypeRef {.importc: "LLVMIntPtrType",
.}
proc intPtrTypeForAS*(td: TargetDataRef; `as`: cuint): TypeRef {.
    importc: "LLVMIntPtrTypeForAS".}
proc intPtrTypeInContext*(c: ContextRef; td: TargetDataRef): TypeRef {.
    importc: "LLVMIntPtrTypeInContext".}
proc intPtrTypeForASInContext*(c: ContextRef; td: TargetDataRef; `as`: cuint): TypeRef {.
    importc: "LLVMIntPtrTypeForASInContext".}
proc sizeOfTypeInBits*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMSizeOfTypeInBits".}
proc storeSizeOfType*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMStoreSizeOfType".}
proc aBISizeOfType*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMABISizeOfType".}
proc aBIAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMABIAlignmentOfType".}
proc callFrameAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMCallFrameAlignmentOfType".}
proc preferredAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMPreferredAlignmentOfType".}
proc preferredAlignmentOfGlobal*(td: TargetDataRef; globalVar: ValueRef): cuint {.
    importc: "LLVMPreferredAlignmentOfGlobal".}
proc elementAtOffset*(td: TargetDataRef; structTy: TypeRef; offset: culonglong): cuint {.
    importc: "LLVMElementAtOffset".}
proc offsetOfElement*(td: TargetDataRef; structTy: TypeRef; element: cuint): culonglong {.
  importc: "LLVMOffsetOfElement".}


# ** begin wrapper from llvmAPI.cpp ***

proc nimLLVMinit*() {.importc: "LLVMNimInit".}

proc nimLLVMinitAll*() {.importc: "LLVMNimInitAll".}

proc nimLLVMSetDSOLocal*(Global: ValueRef) {.importc: "LLVMNimSetDSOLocal".}

proc nimLLVMOptModule*(m: ModuleRef) {.importc: "LLVMNimOptModule".}

# *** end wrapper ***

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
    Backend* = object
      ## jump labels
      labels*: seq[TableRef[string, Label]]
      ## struct/union
      tags*: seq[TableRef[string, Type]]
      ## variable and enum constants
      vars*: seq[TableRef[string, Value]]
      ## module
      m*: ModuleRef
      module*: ModuleRef
      builder*: BuilderRef
      machine*: TargetMachineRef
      currentfunction*: Value
      tsCtx*: OrcThreadSafeContextRef
      ctx*: ContextRef
      layout*: TargetDataRef
      ## jump labels
      topBreak*: Label
      topTest*: Value
      topdefaultCase*: Label
      topSwitch*: Label
      topContinue*: Label
      target*: TargetRef
      topCase*: Label
      ## LLVM Types
      i1, i8*, i16*, i32*, i64*, ffloat*, fdouble*, voidty*, ptrty*: Type
      ## intptr
      intPtr*: Type
      ## `i32 1/0` constant
      i32_1*, i32_0*: Value
      ## LLVM false/true constant
      i1_0, i1_1*: Value


var b*: ptr Backend

proc llvm_error*(msg: string) =
  stderr.writeLine("LLVM ERROR: " & msg)

proc llvm_error*(msg: cstring) =
  if msg != nil:
    stderr.write("LLVM ERROR: ")
    stderr.write(msg)
    stderr.write('\n')

proc wrap*(ty: CType): Type

proc wrap2*(ty: CType): Type

proc llvmGetAlignOf*(ty: CType): culonglong =
  aBIAlignmentOfType(b.layout, wrap2(ty))

proc llvmGetsizeof*(ty: CType): culonglong =
  storeSizeOfType(b.layout, wrap2(ty))

proc llvmGetOffsetof*(ty: CType, idx: int): culonglong =
  offsetOfElement(b.layout, wrap(ty), cuint(idx))

proc initModule() =
  const idents = "cc: A C Compiler(https://ianfun.github.io/cc.git)"
  var ident = mDStringInContext(b.ctx, idents, len(idents))
  addNamedMetadataOperand(b.module, "llvm.ident", ident)
  
  const wchars = "short_wchar"
  var short_wchars = [b.i32_1, mDStringInContext(b.ctx, wchars, len(wchars)), b.i32_1]
  var short_wchar = mDNodeInContext(b.ctx, addr short_wchars[0], 3)
  addNamedMetadataOperand(b.module, "llvm.module.flags", short_wchar)

  const enums = "short_enum"
  var short_enums = [b.i32_1, mDStringInContext(b.ctx, enums, len(enums)), b.i32_1]
  var short_enum = mDNodeInContext(b.ctx, addr short_enums[0], 3)
  addNamedMetadataOperand(b.module, "llvm.module.flags", short_enum)

  const wcharsizes = "wchar_size"
  var wcharsize_arr = [b.i32_1, mDStringInContext(b.ctx, wcharsizes, len(wcharsizes)), constInt(b.i32, 4, False)]
  var wcharsizenode = mDNodeInContext(b.ctx, addr wcharsize_arr[0], 3)
  addNamedMetadataOperand(b.module, "llvm.module.flags", wcharsizenode)

proc newBackend*(module_name, source_file: string) =
  #initializeCore(getGlobalPassRegistry())
  #initializeNativeTarget()
  #initializeNativeAsmParser()
  #initializeNativeAsmPrinter()
  #initializeAllAsmParsers()
  nimLLVMinit()
  b = create(Backend)
  b.tsCtx = orcCreateNewThreadSafeContext()
  b.ctx = orcThreadSafeContextGetContext(b.tsCtx)
  b.builder = createBuilderInContext(b.ctx)
  if app.input == InputC:
    contextSetDiscardValueNames(b.ctx, True)
  b.module = moduleCreateWithNameInContext(module_name, b.ctx)
  setSourceFileName(b.module, source_file, source_file.len.csize_t)
  b.voidty = voidTypeInContext(b.ctx)
  b.ptrty = pointerTypeInContext(b.ctx, 0)
  b.i1 = int1TypeInContext(b.ctx)
  b.i8 = int8TypeInContext(b.ctx)
  b.i16 = int16TypeInContext(b.ctx)
  b.i32 = int32TypeInContext(b.ctx)
  b.i64 = int64TypeInContext(b.ctx)
  b.ffloat = floatTypeInContext(b.ctx)
  b.fdouble = doubleTypeInContext(b.ctx)
  b.i32_1 = constInt(b.i32, 1, False)
  b.i32_0 = constInt(b.i32, 0, False)
  b.i1_0 = constInt(b.i1, 0, False)
  b.i1_1 = constInt(b.i1, 1, False)
  initModule()

#proc lookupIntrinsicID(name: string): cuint =
#  lookupIntrinsicID(name, name.len.csize_t)

proc initTarget*() =
  var err: cstring
  if app.triple.len == 0:
    app.triple.add(getDefaultTargetTriple())
  if getTargetFromTriple(app.triple.cstring, addr b.target, cast[cstringArray](addr err)) == True:
    llvm_error(err)
    app.triple = $getDefaultTargetTriple()
    discard getTargetFromTriple(app.triple.cstring, addr b.target, cast[cstringArray](addr err))
  b.machine = createTargetMachine(b.target, app.triple.cstring, "", "", CodeGenLevelAggressive, RelocPIC, CodeModelDefault)
  b.layout = createTargetDataLayout(b.machine)
  setModuleDataLayout(b.module, b.layout)
  setTarget(b.module, app.triple.cstring)
  app.pointersize = pointerSize(b.layout)
  contextSetOpaquePointers(b.ctx, if app.opaquePointerEnabled: True else: False)
  b.intPtr = intPtrTypeInContext(b.ctx, b.layout)
  #b.i_setjmp = lookupIntrinsicID("llvm.eh.sjlj.longjmp") # 69
  #b.i_longjmp = lookupIntrinsicID("llvm.eh.sjlj.setjmp") # 71


proc dumpVersionInfo*() =
  var arr = [cstring("llvm"), cstring("--version")]
  parseCommandLineOptions(2, cast[cstringArray](addr arr[0]), nil)

proc handle_asm(s: string) =
  if b.currentfunction == nil:
    # module level asm
    appendModuleInlineAsm(b.module, cstring(s), len(s).csize_t)
  else:
    var asmtype = functionType(b.voidty, nil, 0, False)
    var f = getInlineAsm(asmtype, cstring(s), len(s).csize_t, nil, 0, True, False, InlineAsmDialectATT, False)
    var c = buildCall2(b.builder, asmtype, f, nil, 0, "") 
    setTailCall(c, True)

proc setBackend*() =
  app.getSizeof = llvmGetsizeof
  app.getoffsetof = llvmGetOffsetof
  app.getAlignOf = llvmGetAlignOf
  initTarget()

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
  if b != nil:
     dealloc(b)
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

proc link*(dest, src: ModuleRef): bool =
  ## return true when error
  bool(linkModules2(dest, src))

proc readBitcodeToModule*(path: cstring): ModuleRef =
  var mem: MemoryBufferRef
  var err: cstring
  if createMemoryBufferWithContentsOfFile(path, addr mem, cast[cstringArray](addr err)) == True:
    llvm_error(err)
  if parseBitcodeInContext(b.ctx, mem, addr result, cast[cstringArray](addr err)) == True:
    llvm_error(err)

proc readIRToModule*(path: cstring): ModuleRef =
  var mem: MemoryBufferRef
  var err: cstring
  if createMemoryBufferWithContentsOfFile(path, addr mem, cast[cstringArray](addr err)) == True:
    llvm_error(err)
  if parseIRInContext(b.ctx, mem, addr result, cast[cstringArray](addr err)) == True:
    llvm_error(err)

proc writeModuleToFile*(path: string, m: ModuleRef) =
  var err: cstring = ""
  if printModuleToFile(m, path, cast[cstringArray](addr err)) == True:
    llvm_error(err)

proc writeModuleToFile*(path: string) =
  writeModuleToFile(path, b.module)

proc writeBitcodeToFile*(path: string, m: ModuleRef) =
  if writeBitcodeToFile(m, path) != 0:
    llvm_error("LLVMWriteBitcodeToFile")

proc writeBitcodeToFile*(path: string) =
  writeBitcodeToFile(path, b.module)

proc writeObjectFile*(path: string, m: ModuleRef) =
  var err: cstring
  if targetMachineEmitToFile(b.machine, m, path, ObjectFile, cast[cstringArray](addr err)) == True:
    llvm_error(err)

proc writeObjectFile*(path: string) =
  writeObjectFile(path, b.module)

proc writeAssemblyFile*(path: string, m: ModuleRef) =
  var err: cstring
  if targetMachineEmitToFile(b.machine, m, path, AssemblyFile, cast[cstringArray](addr err)) == True:
    llvm_error(err)

proc writeAssemblyFile*(path: string) =
  writeAssemblyFile(path, b.module)

proc close_backend*() =
  disposeBuilder(b.builder)

proc gen*(e: Expr): Value

proc gen*(s: Stmt)

proc gen_cond*(a: Expr): Value =
  ## generate bool(conditional) expression
  if bool(a.ty.tags and TYBOOL):
    gen(a)
  else:
    buildIsNotNull(b.builder, gen(a), "")

proc gen_bool*(val: bool): Value =
  if val: b.i1_1 else: b.i1_0

proc gen_true*(): Value =
  b.i1_1

proc gen_false*(): Value =
  b.i1_0

proc gen_int*(i: culonglong, tags: uint32): Value =
    if (tags and TYBOOL) != 0:
        if i > 0: b.i1_1 else: b.i1_0
    elif (tags and (TYINT8 or TYUINT8)) != 0:
        constInt(b.i8, i, False)
    elif (tags and (TYINT16 or TYUINT16)) != 0:
        constInt(b.i16, i, False)
    elif (tags and (TYINT32 or TYUINT32)) != 0:
        constInt(b.i32, i, False)
    elif (tags and (TYINT64 or TYUINT64)) != 0:
        constInt(b.i64, i, False)
    else:
        unreachable()
        nil

proc gen_float*(f: float, tag: uint32): Value =
  constReal(if (tag and TYFLOAT) != 0: b.ffloat else: b.fdouble, f)

proc gen_str*(val: string, ty: var Type): Value =
  var gstr = constStringInContext(b.ctx, cstring(val), len(val).cuint, False)
  ty = typeOfX(gstr)
  result = addGlobal(b.module, ty, "")
  # setGlobalConstant(result, True)
  # we use array instead of constant string!
  setLinkage(result, PrivateLinkage)
  setInitializer(result, gstr)
  setAlignment(result, 1)
  setUnnamedAddr(result, 1)

proc gen_str_ptr*(val: string): Value =
  var ty: Type
  var s = gen_str(val, ty)
  var indices = [b.i32_0, b.i32_0]
  buildInBoundsGEP2(b.builder, pointerType(ty, 0), s, addr indices[0], 2, "")

proc backendint*(): Type =
    if TYINT == TYINT32:
      b.i32
    else:
      b.i64

proc load*(p: Value, t: Type, align: uint32 = 0): Value =
  assert p != nil
  assert t != nil
  result = buildLoad2(b.builder, t, p, "")
  if align != 0:
    setAlignment(result, align.cuint)

proc store*(p: Value, v: Value, align: uint32 = 0) =
  var s = buildStore(b.builder, v, p)
  if align != 0:
    setAlignment(s, align.cuint)

proc wrap2*(ty: CType): Type =
  case ty.spec:
  of TYPRIM:
    return (
      if (ty.tags and (TYBOOL)) != 0: b.i1
      elif (ty.tags and (TYINT8 or TYUINT8)) != 0: b.i8
      elif (ty.tags and (TYINT16 or TYUINT16)) != 0: b.i16
      elif (ty.tags and (TYINT32 or TYUINT32)) != 0: b.i32
      elif (ty.tags and (TYINT64 or TYUINT64)) != 0: b.i64
      elif (ty.tags and TYFLOAT) != 0: b.ffloat
      elif (ty.tags and TYDOUBLE) != 0: b.fdouble
      elif (ty.tags and TYVOID) != 0: b.voidty
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
  of TYENUM:
    return backendint()
  else:
    unreachable()

proc wrap*(ty: CType): Type =
  ## wrap a CType to LLVM Type
  case ty.spec:
  of TYPRIM:
    return (
      if (ty.tags and TYBOOL) != 0: b.i1
      elif (ty.tags and (TYINT8 or TYUINT8)) != 0: b.i8
      elif (ty.tags and (TYINT16 or TYUINT16)) != 0: b.i16
      elif (ty.tags and (TYINT32 or TYUINT32)) != 0: b.i32
      elif (ty.tags and (TYINT64 or TYUINT64)) != 0: b.i64
      elif (ty.tags and TYFLOAT) != 0: b.ffloat
      elif (ty.tags and TYDOUBLE) != 0: b.fdouble
      elif (ty.tags and TYVOID) != 0: b.voidty
      else:
        unreachable()
        nil
      )
  of TYPOINTER:
    if app.opaquePointerEnabled:
      return b.ptrty
    else:
      # `return pointerType(voidTypeInContext(b.ctx), 0)`
      # clang use `i8*` for `void*` and `char*`
      return pointerType(b.i8, 0)
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
    if ty.ename.len != 0:
      result = getTags(ty.ename)
      if result != nil:
        return result
    result = backendint()
    if ty.ename.len != 0:
        putTags(ty.ename, result)
    for (name, v) in ty.eelems:
        var init = constInt(result, v.culonglong, False)
        var g = addGlobal(b.module, result, name.cstring)
        setInitializer(g, init)
        setLinkage(g, PrivateLinkage)
        setGlobalConstant(g, True)
        putVar(name, g)
    return result
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

proc gen_logical*(lhs: Expr, rhs: Expr, isand = true): Value =
  var cond2 = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var phib = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  var a = buildAlloca(b.builder, b.i1, "")
  var left = gen_cond(lhs)
  store(a, left)
  if isand:
    discard buildCondBr(b.builder, left, cond2, phib)
  else:
    discard buildCondBr(b.builder, left, phib, cond2)

  positionBuilderAtEnd(b.builder, cond2)
  var right = gen_cond(rhs)
  store(a, right)
  discard buildBr(b.builder, phib)

  positionBuilderAtEnd(b.builder, phib)

  return load(a, b.i1)

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

proc gen_switch*(test: Expr, body: Stmt) =
  var old_switch = b.topSwitch
  var old_break = b.topBreak
  var old_test = b.topTest
  var old_case = b.topCase
  var old_default = b.topdefaultCase

  b.topTest = gen(test)
  b.topBreak = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  b.topSwitch = getInsertBlock(b.builder)
  b.topCase = nil
  b.topdefaultCase = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  gen(body)
  if b.topCase != nil:
    discard buildBr(b.builder, b.topBreak)
  positionBuilderAtEnd(b.builder, b.topSwitch)
  discard buildBr(b.builder, b.topdefaultCase)
  positionBuilderAtEnd(b.builder, b.topBreak)

  b.topdefaultCase = old_default
  b.topCase = old_case
  b.topTest = old_test
  b.topBreak = old_break
  b.topSwitch = old_switch

proc gen_case*(test: Expr, body: Stmt) =
  var thiscase = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  if b.topCase != nil:
    discard buildBr(b.builder, thiscase)
  b.topCase = thiscase
  positionBuilderAtEnd(b.builder, b.topSwitch)
  let lhs = gen(test)
  let rhs = b.topTest
  let cond = buildICmp(b.builder, IntEQ, lhs, rhs, "")
  b.topSwitch = appendBasicBlockInContext(b.ctx, b.currentfunction, "")
  discard buildCondBr(b.builder, cond, thiscase, b.topSwitch)
  positionBuilderAtEnd(b.builder, thiscase)
  gen(body)

proc gen_default*(body: Stmt) =
  if b.topCase != nil:
    discard buildBr(b.builder, b.topdefaultCase)
  b.topCase = b.topdefaultCase
  positionBuilderAtEnd(b.builder, b.topdefaultCase)
  gen(body)

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
  else: unreachable();cast[IntPredicate](0)

proc getFCmpOp*(a: BinOP): RealPredicate =
  case a:
  of FEQ: RealOEQ
  of FNE: RealONE
  of FGT: RealOGT
  of FGE: RealOGE
  of FLT: RealOLT
  of FLE: RealOLE
  else: unreachable();cast[RealPredicate](0)

proc getCastOp*(a: CastOp): Opcode =
  case a:
  of CastOp.Trunc:
    LLVMTrunc
  of CastOp.ZExt:
    LLVMZExt
  of CastOp.SExt:
    LLVMSExt
  of CastOp.FPToUI:
    LLVMFPToUI
  of CastOp.FPToSI:
    LLVMFPToSI
  of CastOp.UIToFP:
    LLVMUIToFP
  of CastOp.SIToFP:
    LLVMSIToFP
  of CastOp.FPTrunc:
    LLVMFPTrunc
  of CastOp.FPExt:
    LLVMFPExt
  of CastOp.PtrToInt:
    LLVMPtrToInt
  of CastOp.IntToPtr:
    LLVMIntToPtr
  of CastOp.BitCast:
    LLVMBitCast

proc addAttribute(f: Value, attrID: cuint) =
    var attr = createEnumAttribute(b.ctx, attrID, 0)
    addAttributeAtIndex(f, cast[AttributeIndex](AttributeFunctionIndex), attr)

proc newFunction*(varty: CType, name: string): Value =
    result = b.vars[0].getOrDefault(name, nil)
    if result != nil:
      return result
    var fty = wrap(varty)
    result = addFunction(b.module, name.cstring, fty)
    nimLLVMSetDSOLocal(result)
    addAttribute(result, NoUnwind)
    addAttribute(result, OptimizeForSize)
    if bool(varty.ret.tags and TYSTATIC):
      setLinkage(result, InternalLinkage)
    if bool(varty.ret.tags and TYNORETURN):
      addAttribute(result, NoReturn)
    if bool(varty.ret.tags and TYINLINE):
      addAttribute(result, InlineHint)
    b.vars[0][name] = result
    #for i in 0 ..< len(varty.params):
    #  if varty.params[i][1] == nil:
    #    break
      #var pa = getParam(result, i.cuint)
      #setValueName2(pa, cstring(varty.params[i][0]), varty.params[i][0].len.csize_t)

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
    gen_switch(s.test, s.body)
  of SDefault:
    gen_default(s.default_stmt)
  of SCase:
    gen_case(s.case_expr, s.case_stmt)
  of SVarDecl:
    for (name, varty, init) in s.vars:
      var align = varty.align
      if bool(varty.tags and TYTYPEDEF):
        discard
      elif varty.spec == TYFUNCTION:
        if not is_builtin_name(name):
          discard newFunction(varty, name)
      else:
        var ty = wrap(varty)
        if b.currentfunction == nil:
          var ginit = if init == nil: constNull(ty) else: gen(init)
          var g = addGlobal(b.module, typeOfX(ginit), cstring(name))
          nimLLVMSetDSOLocal(g)
          if align != 0:
            setAlignment(g, align)
          if isConstant(ginit) == False:
            llvm_error("global initializer is not constant")
            return
          setInitializer(g, ginit)
          if (varty.tags and TYTHREAD_LOCAL) != 0:
            # LLVMSetThreadLocalMode ?
            setThreadLocal(g, 1)
          if (varty.tags and TYSTATIC) != 0:
            setLinkage(g, InternalLinkage)
          elif (varty.tags and TYEXTERN) != 0:
            setExternallyInitialized(g, True)
            setLinkage(g, ExternalLinkage)
          else:
            setLinkage(g, CommonLinkage)
            if (varty.tags and TYREGISTER) != 0:
              warning("register variables is ignored in LLVM backend")
          # setLinkage(g, CommonLinkage)
          putVar(name, g)
        else:
          if align != 0:
            var val = buildAlloca(b.builder, ty, "")
            setAlignment(val, align)
            if init != nil:
              let initv = gen(init)
              store(val, initv, align)
            putVar(name, val)
          else:
            var val = buildAlloca(b.builder, ty, "")
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

proc incl*(p: Value, t: Type): Value =
  var l = load(p, t)
  var l2 = buildAdd(b.builder, l, constInt(t, 1.culonglong, False), "")
  store(p, l2)
  return l

proc incl*(p: Value, t: Type, align: uint32): Value =
  var l = load(p, t, align)
  var l2 = buildAdd(b.builder, l, constInt(t, 1.culonglong, False), "")
  store(p, l2, align)
  return l

proc decl*(p: Value, t: Type, align: uint32): Value =
  var l = load(p, t, align)
  var l2 = buildSub(b.builder, l, constInt(t, 1.culonglong, False), "")
  store(p, l2, align)
  return l

proc decl*(p: Value, t: Type): Value =
  var l = load(p, t)
  var l2 = buildSub(b.builder, l, constInt(t, 1.culonglong, False), "")
  store(p, l2)
  return l

proc getArray(e: Expr): Value =
    var elemTy = wrap(e.ty.arrtype)
    if len(e.arr) != e.ty.arrsize:
      var l = len(e.arr)
      var inits = create(Value, l + 1)
      var arr = cast[ptr UncheckedArray[Value]](inits)
      for i in 0 ..< l:
        arr[i] = gen(e.arr[i])
      var rem = e.ty.arrsize - len(e.arr)
      arr[l] = constNull(vectorType(elemTy, cuint(rem)))
      var initStruct = constStruct(inits, cuint(l) + 1, True)
      dealloc(inits)
      initStruct
    else:
      var l = len(e.arr)
      var inits = create(Value, l or 1)
      var arr = cast[ptr UncheckedArray[Value]](inits)
      for i in 0 ..< l:
        arr[i] = gen(e.arr[i])
      var ret = constArray(elemTy, inits, l.cuint)
      dealloc(inits)
      ret

proc getAddress*(e: Expr): Value =
  # return address
  case e.k:
  of EVar:
    getVar(e.sval)
  of EPointerMemberAccess, EMemberAccess:
    var basep = if e.k == EMemberAccess: getAddress(e.obj) else: gen(e.obj)
    var r = [b.i32_0, constInt(b.i32, e.idx.culonglong, False)]
    var ty = wrap(e.obj.ty)
    buildInBoundsGEP2(b.builder, ty, basep, addr r[0], 2, "")
  of EUnary:
    case e.uop:
    of Dereference:
      gen(e.uoperand)
    else:
      unreachable()
      nil
  of EPostFix:
    case e.pop:
    of PostfixIncrement, PostfixDecrement:
      var basep = getAddress(e.poperand)
      if e.ty.spec == TYPOINTER:
        var ty = wrap(e.poperand.ty)
        var i = b.i32_1
        if e.pop == PostfixDecrement:
          i = constNeg(i)
        var l = load(basep, wrap(e.poperand.ty))
        var g = buildInBoundsGEP2(b.builder, ty, l, addr i, 1, "")
        store(basep, g)
      else:
        var a = e.poperand.ty.align
        var ty = wrap(e.ty)
        if e.pop == PostfixDecrement:
          discard decl(basep, ty, a)
        else:
          discard incl(basep, ty, a)
      basep
  of ESubscript:
    assert e.left.ty.spec == TYPOINTER # the left must be a pointer
    var ty = wrap(e.left.ty.p)
    var v = gen(e.left) # a pointer
    var r = [gen(e.right)] # get index
    buildInBoundsGEP2(b.builder, ty, v, addr r[0], 1, "")
  of ArrToAddress:
    var arr = getAddress(e.voidexpr)
    buildBitCast(b.builder, arr, wrap(e.ty), "")
  of EString:
    gen_str_ptr(e.str)
  of EArray:
    var v = getArray(e)
    if b.currentfunction == nil:
      var g = addGlobal(b.module, typeOfX(v), "")
      setLinkage(g, InternalLinkage)
      setInitializer(g, v)
      g
    else:
      var local = buildAlloca(b.builder, typeOfX(v), "")
      discard buildStore(b.builder, v, local)
      local
  else:
    unreachable()
    nil

proc pointerBitCast*(v: Value, fromTy: Type, to: TypeRef): Value =
  if b.currentfunction == nil:
    var g = addGlobal(b.module, fromTy, "")
    setLinkage(g, InternalLinkage)
    setInitializer(g, v)
    g
  else:
    var local = buildAlloca(b.builder, fromTy, "")
    discard buildStore(b.builder, v, local)
    local

proc gen*(e: Expr): Value =
  case e.k:
  of EUndef:
    getUndef(wrap(e.ty))
  of ArrToAddress:
    var arr = getAddress(e.voidexpr)
    buildBitCast(b.builder, arr, wrap(e.ty), "")
  of ESubscript:
    assert e.left.ty.spec == TYPOINTER # the left must be a pointer
    var ty = wrap(e.left.ty.p)
    var v = gen(e.left) # a pointer
    var r = [gen(e.right)] # get index
    var gaddr = buildInBoundsGEP2(b.builder, ty, v, addr r[0], 1, "")
    load(gaddr, ty) # return lvalue
  of EMemberAccess, EPointerMemberAccess:
    var base = gen(e.obj)
    if e.k == EPointerMemberAccess:
      base = load(base, wrap(e.obj.ty), e.obj.ty.align)
    buildExtractValue(b.builder, base, e.idx.cuint, "")
  of EString:
    gen_str_ptr(e.str)
  of EBackend:
    cast[Value](e.p)
  of EBin:
    case e.bop:
    of Assign:
      var v = gen(e.rhs)
      let basep = getAddress(e.lhs)
      store(basep, v, e.lhs.ty.align)
      load(basep, wrap(e.ty), e.lhs.ty.align)
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
      buildNSWMul(b.builder, l, r, "")
    of PtrDiff:
      var l = gen(e.lhs)
      var r = gen(e.rhs)
      var sub = buildSub(b.builder, l, r, "")
      var t = e.lhs.castval.ty.p
      let s = getsizeof(t)
      if s != 1:
        var c = constInt(b.intPtr, s.culonglong, False)
        buildExactSDiv(b.builder, sub, c, "")
      else:
        sub
    of SAddP:
      var l = gen(e.lhs)
      var r = gen(e.rhs)
      buildInBoundsGEP2(b.builder, wrap(e.ty), l, addr r, 1, "")
    of EQ..SLE:
      buildICmp(b.builder, getICmpOp(e.bop), gen(e.lhs), gen(e.rhs), "")
    of FEQ..FLE:
      buildFCmp(b.builder, getFCmpOp(e.bop), gen(e.lhs), gen(e.rhs), "")
    of LogicalAnd:
      # a && b
      # => a ? b : 0
      gen_logical(e.lhs, e.rhs, isand=true)
    of LogicalOr:
      # a || b
      # => a ? 1 : b
      gen_logical(e.rhs, e.lhs, isand=false)
    of Comma:
      discard gen(e.lhs)
      gen(e.rhs)
    else:
      var op = getOp(e.bop)
      var lhs = gen(e.lhs)
      var rhs = gen(e.rhs)
      buildBinOp(b.builder, op, lhs, rhs, "")
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
    of PrefixIncrement, PrefixDecrement:
      var ty = wrap(e.ty)
      var i = b.i32_1
      if e.uop == PrefixDecrement:
        i = constNeg(i)
      let basep = getAddress(e.uoperand)
      assert basep != nil
      if e.ty.spec == TYPOINTER:
        var l = load(basep, wrap(e.uoperand.ty))
        var g = buildInBoundsGEP2(b.builder, ty, l, addr i, 1, "")
        store(basep, g)
        g
      else:
        var l = load(basep, ty)
        var l2 = buildAdd(b.builder, l, i, "")
        store(basep, l2)
        load(basep, ty)
    of Dereference: load(gen(e.uoperand), wrap(e.ty))
    of LogicalNot: buildisNull(b.builder, gen(e.uoperand), "")
  of EPostFix:
    case e.pop:
    of PostfixIncrement, PostfixDecrement:
      var basep = getAddress(e.poperand)
      if e.ty.spec == TYPOINTER:
        var ty = wrap(e.poperand.ty)
        var i = b.i32_1
        if e.pop == PostfixDecrement:
          i = constNot(i)
        var l = load(basep, wrap(e.poperand.ty))
        var g = buildInBoundsGEP2(b.builder, ty, l, addr i, 1, "")
        store(basep, g)
        l
      else:
        var a = e.poperand.ty.align
        var ty = wrap(e.ty)
        if e.pop == PostfixDecrement:
          decl(basep, ty, a)
        else:
          incl(basep, ty, a)
  of EVar:
    var pvar = getVar(e.sval)
    assert pvar != nil, e.sval
    load(pvar, wrap(e.ty), e.ty.align)
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
    if f == nil:
      llvm_error("connot find function")
      nil
    else:
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
    getArray(e)

proc gen_cast*(e: Expr, to: CType, op: CastOp): Value =
  var c = gen(e)
  buildCast(b.builder, getCastOp(op), c, wrap(to), "")

proc jit_error*(msg: string) =
  stderr.writeLine("LLVM JIT ERROR: " & msg)

proc jit_error*(msg: cstring) =
  stderr.write("LLVM JIT ERROR: ")
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

    # TODO: use command line args from CLI options
    var o = @[appFileName]
    o &= options
    var argslen = len(o)
    var mainargs = create(cstring, argslen or 1)
    var arr = cast[ptr UncheckedArray[cstring]](mainargs)

    for i in 0..<argslen:
      arr[i] = cstring(o[i])

    let ret = fmain(cint(argslen), mainargs)

    dealloc(mainargs)

    exitprocs.setProgramResult(ret)

    verbose("main() return: " & $ret)

    err = orcDisposeLLJIT(jit)
    if err != nil:
      jit_error(err)
      return
