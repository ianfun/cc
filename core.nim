import config, types, stream, ast, token
import std/[os]

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
    cstderr << "cc: \e[33m" & "warning: " & msg & "\e[0m"

proc error*() =
  cstderr << "cc: \e[31merror\e[0m: "

proc verbose*(msg: string) =
  if ord(app.verboseLevel) >= ord(WVerbose):
    cstderr << "cc: "
    cstderr << msg

proc getPtrDiff_t*(): CType = make_primitive(if app.pointersize == 4: TYINT32 else: TYINT64)

proc getIntPtr_t*(): CType = getPtrDiff_t()

