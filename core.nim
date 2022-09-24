import std/[exitprocs]

type
  intmax_t* = int64
  uintmax_t* = uint64
  Codepoint* = uint32

proc builtin_unreachable() {.importc: "__builtin_unreachable", nodecl.}

proc unreachable*() =
  # https://learn.microsoft.com/en-us/cpp/intrinsics/assume?view=msvc-170
  # https://clang.llvm.org/docs/LanguageExtensions.html#builtin-assume
  # https://stackoverflow.com/questions/63493968/reproducing-clangs-builtin-assume-for-gcc
  when defined(debug):
    assert false, "INTERNAL ERROR: unreachable executed!"
  else:
    builtin_unreachable()

proc set_exit_code*(code: auto) =
  exitprocs.setProgramResult(code)

proc cc_exit*(c: auto) =
  quit c

proc unsafe_utf8_codepoint*(s: cstring): (Codepoint, int) =
    if 0xf0 == (0xf8 and s[0].Codepoint): # 4 byte
      (
        ((0x07 and s[0].Codepoint) shl 18) or 
        ((0x3f and s[1].Codepoint) shl 12) or
        ((0x3f and s[2].Codepoint) shl 6) or
         (0x3f and s[3].Codepoint),
      4)
    elif 0xe0 == (0xf0 and s[0].Codepoint): # 3 byte
      ( 
        ((0x0f and s[0].Codepoint) shl 12) or 
        ((0x3f and s[1].Codepoint) shl 6) or 
         (0x3f and s[2].Codepoint),
      3)
    elif 0xc0 == (0xe0 and s[0].Codepoint): # 2 byte
      (
        ((0x1f and s[0].Codepoint) shl 6) or 
         (0x3f and s[1].Codepoint),
      2)
    else: # 1 byte
      (s[0].Codepoint, 1)

proc writeUTF8toUTF32*(s: string): seq[Codepoint] =
  # TODO: reserve 3 more bytes to prevent bad utf8 terminate access overflow
  var i = 0
  while true:
    if i >= len(s):
      break
    let (codepoint, length) = unsafe_utf8_codepoint(cast[cstring](cast[int](cstring(s)) + i))
    result.add(codepoint)
    i += length

proc writeUTF8toUTF16*(s: string): seq[uint16] =
  let u32 = writeUTF8toUTF32(s)
  result = newSeqOfCap[uint16](len(u32) div 2)
  for codepoint in u32:
    var c = codepoint
    if c <= 0xFFFF:
      result.add(uint16(c))
    else:
      c -= 0x10000
      result.add(uint16(0xD800 + (c shr 10)))
      result.add(uint16(0xDC00 + (c and 0x3FF)))

proc show*(c: char): string =
  let n = int(c)
  if n >= 33 and n <= 126:
    result = "'"
    result.add(c)
    result.add('\'')
  else:
    result = "<"
    result.addInt(n)
    result.add('>')

type 
    VerboseLevel* = enum
      VError, VWarning, VNote, VVerbose
    Linker* = enum LLD, GCCLD
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

      pointersize*: culonglong

var app* = CC(
    optLevel: 0.cuint, 
    sizeLevel: 0.cuint, 
    inlineThreshold: 0, 
    verboseLevel: VNote,
    opaquePointerEnabled: true,
    mode: OutputLink,
    input: InputC,
    output: "",
    triple: "",
    runJit: false,
    linker: GCCLD
)

