import ast, config, stream, token, location
import std/[os, tables, sets]

type
  TranslationUnit* = object
    currentfunctionRet*, currentInitTy*, currentCase*: CType
    pfunc*: string
    currentAlign*: uint32
    fstack*: seq[Stream]
    filenamestack*, pathstack*: seq[string]
    locstack*: seq[Location]
    want_expr*: bool
    filename*, path*: string
    macros*: Table[string, PPMacro]
    ppstack*: seq[uint8]
    ok*: bool
    onces*, expansion_list*: HashSet[string]
    lables*: seq[TableRef[string, uint8]]
    tags*: seq[TableRef[string, Info]]
    typedefs*: seq[TableRef[string, Info]]
    counter*: int
    retTy*: CType
    type_error*: bool
    eval_error*: bool
    parse_error*: bool
    bad_error*: bool

const
  LBL_UNDEFINED* = 0'u8
  LBL_FORWARD* = 1'u8
  LBL_DECLARED* = 2'u8
  LBL_OK* = 4'u8

var t* = TranslationUnit(ok: true,
  bad_error: false, eval_error: false, parse_error: false, want_expr: false, counter: 0,
  filename: "<built-in>", path: "<built-in>"
  )


proc isTopLevel*(): bool =
    t.typedefs.len == 1

var options* = commandLineParams()
var appFileName* = getAppFilename()

type 
  VerboseLevel* = enum
    WError, WWarning, WNote, WVerbose
  Linker* = enum LLD, GCCLD

proc perror*(str: cstring) {.importc: "perror", header: "stdio.h".}

type
    Input* = enum
      InputC, InputIR, InputBC, InputObject, InputBF, InputAsm
    Output* = enum
      OutputLink,
      OutputLLVMAssembly, OutputBitcode,
      OutputObjectFile, OutputAssembly,
      OutputCheck
    CC*{.final.} = object
      ## command line options
      input*: Input
      mode*: Output
      runJit*: bool
      optLevel*: cuint ## 0 = -O0, 1 = -O1, 2 = -O2, 3 = -O3
      sizeLevel*: cuint ## 0 = none, 1 = -Os, 2 = -Oz
      inlineThreshold*: cuint
      output*: string
      verboseLevel*: VerboseLevel
      opaquePointerEnabled*: bool
      linker*: Linker
      triple*: string
      ## input files
      inputs*: seq[string]

      ## backend
      pointersize*: culonglong
      getSizeof*: proc (ty: CType): culonglong
      getoffsetof*: proc (ty: CType, idx: int): culonglong
      getAlignOf*: proc (ty: CType): culonglong

      ## lexer
      lex*: proc ()

      ## C preprocessor
      cpp*: proc ()

      ## constant evaluator
      eval_const_expression*: proc (e: Expr): intmax_t

      ## pragma handler
      pragma*: proc (tokens: seq[TokenV])
      pragmas*: proc (p: string)

var app* = CC(
    optLevel: 0.cuint, 
    sizeLevel: 0.cuint, 
    inlineThreshold: 0, 
    verboseLevel: WNote,
    opaquePointerEnabled: true,
    mode: OutputLink,
    input: InputC,
    output: "",
    triple: "",
    runJit: false,
    linker: GCCLD
)

proc warningPlain*(msg: string) =
  if ord(app.verboseLevel) >= ord(WWarning):
    fstderr << "cc: \e[33m" & "warning: " & msg & "\e[0m"

proc error*() =
  fstderr << "cc: \e[31merror\e[0m: "

proc verbose*(msg: string) =
  if ord(app.verboseLevel) >= ord(WVerbose):
    fstderr << "cc: "
    fstderr << msg

proc getPtrDiff_t*(): CType = get(if app.pointersize == 4: TYINT32 else: TYINT64)

proc getIntPtr_t*(): CType = getPtrDiff_t()

