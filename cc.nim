# cc - C Compiler
#
# Copyright(c) ianfun 2022

include "config.nim"

# llvm-config --ldflags --system-libs --libs all
when defined(windows):
    when CC_NO_RAEADLINE:
        {.passL: "./llvm/llvmAPI.o libLLVM-15.dll".}
    else:
        {.passL: "./llvm/llvmAPI.o libLLVM-15.dll -lreadline -ltinfo".}
else:
    when CC_NO_RAEADLINE:
        {.passL: "-L/usr/lib/llvm-15/lib -lLLVM-15 ./llvm/llvmAPI.o".}
    else:
        {.passL: "-L/usr/lib/llvm-15/lib -lLLVM-15 ./llvm/llvmAPI.o -lreadline -ltinfo".}

import std/[math, sets, tables, editdistance, heapqueue, sequtils, os, times, unicode, macrocache, strutils, exitprocs]
import llvm/llvm

when not CC_NO_RAEADLINE:
    import readline/[readline, history]

type
  Location = object
    fd: uint32
    line: uint32
    col: uint32
  intmax_t = int64
  uintmax_t = uint64
  Codepoint = uint32

when USE_SetConsoleTextAttribute:
    import wincolor
else:
    proc isatty(fd: cint): cint {.importc: "isatty", header: "<unistd.h>".}

type 
    VerboseLevel = enum
      VError, VWarning, VNote, VVerbose
    Linker = enum LLD, GCCLD
    Output = enum
      OutputLink,
      OutputLLVMAssembly, OutputBitcode,
      OutputObjectFile, OutputAssembly,
      OutputCheck
    CC{.final.} = object
      ## command line options
      mode: Output
      runJit: bool
      dir: string
      optLevel: cuint ## 0 = -O0, 1 = -O1, 2 = -O2, 3 = -O3
      sizeLevel: cuint ## 0 = none, 1 = -Os, 2 = -Oz
      inlineThreshold: cuint
      output: string
      verboseLevel: VerboseLevel
      linker: Linker
      triple: string
      ## input files
      pointersize: culonglong
      isAttyStderr: bool
      ## create debug output
      g: bool
      strbuf: string
      libtrace: bool
      when defined(windows):
        hStderr: HANDLE
        hStderrInfo: CONSOLE_SCREEN_BUFFER_INFO

type
    Fd* = cint
    cssize_t{.importc: "ssize_t", nodecl, header: "stdio.h".} = clonglong
    off_t {.importc: "off_t ", nodecl, header: "unistd.h".} = clonglong
    BinaryBuffer = pointer | cstring | ptr

proc perror(str: cstring) {.importc: "perror", header: "stdio.h".}

proc read(fd: Fd, buf: BinaryBuffer, count: csize_t): cssize_t {.importc: "read", header: "unistd.h".}

proc write(fd: Fd, buf: BinaryBuffer, count: csize_t): cssize_t {.importc: "write", header: "unistd.h".}

proc open(pathname: cstring, flags: cint): cint {.importc: "open", nodecl, header: "fcntl.h".}

proc close(fd: cint): cint {.importc: "close", nodecl, header: "unistd.h".}

# proc lseek(fd: Fd, offset: off_t , whence: int): off_t {.importc: "lseek", nodecl, header: "unistd.h".}

let O_RDONLY = cint(0)

const
  fstderr: Fd = 2

var app = CC(
    optLevel: 0.cuint, 
    sizeLevel: 0.cuint, 
    inlineThreshold: 0, 
    verboseLevel: VNote,
    mode: OutputLink,
    output: "",
    triple: "",
    runJit: false,
    linker: GCCLD,
    strbuf: newStringOfCap(150),
    g: false,
    dir: getCurrentDir(),
    libtrace: false
)

when defined(windows):
    var hStderrInfo: CONSOLE_SCREEN_BUFFER_INFO
    app.hStderr = GetStdHandle(STD_ERROR_HANDLE)
    app.isAttyStderr = GetConsoleScreenBufferInfo(app.hStderr, addr hStderrInfo) != 0
else:
    app.isAttyStderr = bool(isatty(fstderr))

type
    StringRef = object
      len: int
      str: cstring
    StreamType = enum
      FileStream, StringStream, StdinStream
    Stream = ref object
      case k: StreamType
      of FileStream:
        buf: pointer
        pos: uint
        p: uint
        fd: Fd
      of StringStream, StdinStream:
        s: string
        i: int

proc `<<`(stream: Fd, msg: string) =
    discard write(stream, cstring(msg), msg.len.csize_t)

proc newStringStream(s: string): Stream {.raises: [].} =
    result = Stream(k: StringStream, s: s, i: 0)

proc newFileStream(path: string): Stream {.raises: [].} =
    let fd = open(path, O_RDONLY)
    if fd < 0:
        return nil
    result = Stream(k: FileStream, fd: fd, buf: alloc(STREAM_BUFFER_SIZE))

proc newStdinStream(): Stream {.raises: [].} =
    result = Stream(k: StdinStream)

proc readChar(s: Stream): char {.raises: [].}

proc close(s: Stream) {.raises: [].} =
    case s.k:
    of FileStream:
        if s.buf != nil:
            dealloc(s.buf)
        discard close(s.fd)
    of StringStream, StdinStream:
        discard


type Token = enum
  TNul=0, TNewLine=int('\n'),
#    ' '             !      "
  TSpace=int(' '), TNot, TDoubleQ,
#    #               $        %        &         '           (          )        *    +      ,       -      .     /
  TBash=int('#'), TDolor, TPercent, TBitAnd, TSignleQ,   TLbracket, TRbracket, TMul, TAdd, TComma, TDash, TDot, TSlash,
#    :                 ;        <      =      >         ?          @ 
  TColon=int(':'), TSemicolon, TLt, TAssign, TGt, TQuestionMark, TMouse, 
#       [                       \               ]            ^        _           `
  TLSquareBrackets=int('['), TBackslash, TRSquareBrackets, TXor, TUnderscore, TGraveAccent,
#         {                  |           }            ~
  TLcurlyBracket=int('{'), TBitOr, TRcurlyBracket, TBitNot, 

  T255=255,

  Kauto="auto", Kfor="for",
  Kgoto="goto", Kif="if", Kinline="inline", 
  Kint="int", Kchar="char", Klong="long", 
  Kregister="register", Krestrict="restrict", 
  Kreturn="return", Kshort="short", Kelse="else", 
  Kenum="enum", Kextern="extern", Kdouble="double", 
  Kdo="do", Kdefault="default", Kfloat="float",
  Ksigned="signed", Ksizeof="sizeof", Kstatic="static", 
  Kstruct="struct", Kswitch="switch", Kcase="case",
  Ktypedef="typedef", Kunion="union", Kcontinue="continue",
  Kunsigned="unsigned", Kvoid="void", Kvolatile="volatile", 
  Kwhile="while", Kconst="const", Kbreak="break",
  K_Alignas="_Alignas", K_Alignof="_Alignof", K_Atomic="_Atomic", 
  K_Bool="_Bool", K_Complex="Complex", K_asm="__asm__"
  K_Decimal128="_Decimal128", K_Decimal32="_Decimal32", 
  K_Decimal64="_Decimal64", K_Generic="_Generic", 
  K_Imaginary="_Imaginary", K_Noreturn="_Noreturn", K_Static_assert="_Static_assert",
  K_Thread_local="_Thread_local"

  TAddAdd="++", TSubSub="--", TArrow="->",
  Tshl="<<", Tshr=">>",
  TGe=">=", TLe="<=",
  TEq="==", TNe="!=",
  TLogicalOr="||",
  TLogicalAnd="&&",
  TAsignAdd="+=",
  TAsignSub="-=",
  TAsignMul="*=",
  TAsignDiv="/=",
  TAsignRem="%=",
  TAsignShl="<<=",
  TAsignShr=">>=",
  TAsignBitAnd="&=",
  TAsignBitOr="|=",
  TAsignBitXor="^=",
  
  TEllipsis2="..",
  TEllipsis="...", # varargs
  TNumberLit="<number>",
  TFloatLit="<float>",
  TCharLit="<char>",
  TIdentifier="<identifier>",
  CPPIdent="<CPPident>",
  TStringLit="<string>",
  TPPNumber="<pp-number>", # pp-token => pp-number, used by preprocessor
  PPPlaceholder="<placeholder>",
  PPSharp="#",
  PPSharpSharp="##",
  PPMacroPop="<pop>",
  TEOF="<EOF>"

type
    PPMacroFlags = enum
      MOBJ, MFUNC, MBuiltin
    PPMacro = ref object
      tokens: seq[TokenV]
      case flags: PPMacroFlags # if a object like macro?
      of MFUNC:
        params: seq[string]
        ivarargs: bool
      of MBuiltin:
        fn: proc ()
      of MOBJ:
        discard
    PPFlags = enum
      PFNormal=1, PFPP=2
    TokenVTags = enum
      TVNormal, TVSVal, TVIVal, TVFval, TVStr
    # integer tags
    ITag = enum
      Iint,   Ilong,   Iulong,  Ilonglong, Iulonglong, Iuint
    # also for chars
    # char,   char16,  char32,  wchar
    #  u8       u        U       L
    # float tags
    FTag = enum
      Fdobule, Ffloat
    TokenV = ref object
      tok: Token
      case tags: TokenVTags
      of TVNormal:
        discard
      of TVSVal:
        s: string
      of TVFval:
        f: float
        ftag: FTag
      of TVIVal:
        i: uintmax_t
        itag: ITag
      of TVStr:
        str: string
        enc: uint8

const
  CSkip = {' ', '\t', '\f', '\v'} # space, tab, new line, form feed
  KwStart = Kauto
  KwEnd = K_Thread_local

var
  gkeywordtable = initTable[string, Token](int(KwEnd) - int(KwStart) + 1)

proc isKeyword(a: string): Token =
  gkeywordtable.getOrDefault(a, TNul)

for k in int(KwStart)..int(KwEnd):
  gkeywordtable[$cast[Token](k)] = cast[Token](k)

const type_specifier_set = {
    Kchar, Kint, Kshort, Ksigned, 
    Kunsigned, 
    Klong, Kdouble, Kfloat,
    K_Atomic,
    K_Complex, Kvoid, K_Bool
} ## primitive types

const function_specifier_set = {
    Kinline, K_Noreturn
} ## function specfiers

const storage_class_specifier_set = {
    Ktypedef, Kextern, Kstatic, 
    K_Thread_local, Kauto, Kregister
} ## storage specfiers

const type_qualifier_set = {
     Kvolatile, Krestrict, Kconst
} ## type qualifiers

const declaration_specifier_set = 
    type_specifier_set +
    storage_class_specifier_set +
    type_qualifier_set +
    function_specifier_set ## declaration specfiers

const hexs: cstring = "0123456789ABCDEF"

proc isprint(a: cint): cint {.importc: "isprint", nodecl, header: "ctype.h".}

proc reverse(a: var string) =
    var l = len(a) - 1
    for i in 0 ..< (len(a) div 2):
        let c = a[i]
        a[i] = a[l - i]
        a[l - i] = c

proc hex(a: uint): string =
    var c = a
    while true:
        result &= hexs[a mod 16]
        c = c shr 4
        if c == 0:
            break
    reverse(result)

proc stringizing(a: char): string =
    return '\'' & (
        case a:
        of '\a':
            "\\a"
        of '\b':
            "\\b"
        of '\f':
            "\\f"
        of '\n':
            "\\n"
        of '\r':
            "\\r"
        of '\t':
            "\\t"
        of '\v':
            "\\v"
        of '\e':
            "\\e"
        else:
            if isprint(cint(a)) != 0:
                $a
            else:
                "\\x" & hex(uint(a))
    ) & '\''

proc stringizing(a: string): string =
    result.add('"')
    for v in a:
        result.add(stringizing(v))
    result.add('"')

proc stringizing(a: TokenV): string =
    case a.tok:
        of PPPlaceholder:
            discard
        of TEllipsis:
            result.add("...")
        of TEllipsis2:
            result.add("..")
        of TSpace:
            result.add(' ')
        of TIdentifier, CPPIdent, TPPNumber:
            result.add(a.s)
        of TStringLit:
            result.add(stringizing(a.s))
        of PPSharp:
            result.add('#')
        of PPSharpSharp:
            result.add("##")
        of TCharLit:
            result.add(stringizing(char(a.i)))
        else:
            if uint(a.tok) < 255:
                result.add(char(a.tok))
            else:
                result.add($a.tok)

proc stringizing(a: seq[TokenV]): string =
    for t in a:
        result.add(stringizing(t))

proc unreachable() =
  assert false, "INTERNAL ERROR: unreachable executed!"

proc set_exit_code(code: auto) =
  exitprocs.setProgramResult(code)

proc cc_exit(c: auto) =
  quit c

proc unsafe_utf8_codepoint(s: cstring): (Codepoint, int) =
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

proc writeUTF8toUTF32(s: string): seq[Codepoint] =
  # TODO: reserve 3 more bytes to prevent bad utf8 terminate access overflow
  var i = 0
  while true:
    if i >= len(s):
      break
    let (codepoint, length) = unsafe_utf8_codepoint(cast[cstring](cast[int](cstring(s)) + i))
    result.add(codepoint)
    i += length

proc writeUTF8toUTF16(s: string): seq[uint16] =
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

proc show(c: char): string =
  return repr(c)

type
    PostfixOP = enum
      PostfixIncrement="++", PostfixDecrement="--"
    UnaryOP = enum
      Pos, # it like nop, but it will do integer promotion
      UNeg, SNeg, FNeg,
      Not, AddressOf,
      PrefixIncrement, PrefixDecrement, Dereference, LogicalNot
    BinOP = enum
      Nop=0,
      # Arithmetic operators
      UAdd, SAdd, FAdd, 
      USub, SSub, FSub,
      UMul, SMul, FMul, # no SMul!
      UDiv, SDiv, FDiv, 
      URem, SRem, FRem,
      Shr, AShr, Shl, # or sar in GNU Assembly

      And, Xor, Or,

      LogicalAnd, LogicalOr, 
      Assign,
      SAddP,
      PtrDiff,
      Comma,

      # compare operators
      EQ, NE, 
      UGT, UGE, ULT, ULE, 
      SGT, SGE, SLT, SLE,
      # ordered float compare
      FEQ, FNE, 
      FGT, FGE, FLT, FLE
    CastOp = enum
      Trunc,
      ZExt,
      SExt,
      FPToUI, 
      FPToSI, 
      UIToFP, 
      SIToFP, 
      FPTrunc,
      FPExt,
      PtrToInt, 
      IntToPtr, 
      BitCast
    CTypeSpec = enum
      TYPRIM,
      TYPOINTER,
      TYSTRUCT,
      TYUNION,
      TYENUM,
      TYBITFIELD,
      TYARRAY,
      TYFUNCTION,
      TYINCOMPLETE
    CType = ref object
      tags: uint32
      align: uint32 # zero if use default (not specified)
      case spec: CTypeSpec
      of TYPRIM:
        discard
      of TYPOINTER:
        p: CType
      of TYSTRUCT, TYUNION:
        sname: string
        selems: seq[(string, CType)]
        packed: bool
      of TYENUM:
        ename: string
        enumerators: seq[(string, uintmax_t)] # only valid when app.g is true
      of TYBITFIELD:
        bittype: CType
        bitsize: uintmax_t
      of TYFUNCTION:
        ret: CType
        params: seq[(string, CType)]
      of TYARRAY:
        arrsize: uintmax_t
        arrtype: CType
        hassize: bool
        vla: Expr
      of TYINCOMPLETE:
        tag: CTypeSpec
        name: string
    StmtKind = enum
      SSemicolon, SCompound, SGoto, SContinue, SBreak, SReturn, SExpr, SLabled, SIf, 
      SDoWhile, SWhile, SFor, SSwitch, SDeclOnly, SAsm
      SVarDecl, SDefault, SCase, SFunction, SVarDecl1
    StmtList = seq[Stmt] # compound_stmt, { body }
    Stmt = ref object
      loc: Location
      case k: StmtKind
      of SFunction:
        funcname: string
        functy: CType
        funcbody: Stmt
        labels: HashSet[string]
      of SAsm:
        asms: string
      of SCompound:
        stmts: StmtList
      of SDefault:
        default_stmt: Stmt
      of Scase:
        case_expr: Expr
        case_stmt: Stmt
      of SBreak, SContinue, SSemicolon:
        discard
      of SExpr, SReturn:
        exprbody: Expr
      of SGoto:
        location: string
      of SLabled:
        label: string
        labledstmt: Stmt
      of SIf:
        iftest: Expr
        ifbody: Stmt
        elsebody: Stmt
      of SDoWhile, SWhile, SSwitch:
        test: Expr
        body: Stmt
      of SFor:
        forinit: Stmt
        forcond, forincl: Expr
        forbody: Stmt
      of SVarDecl1:
        var1name: string
        var1type: CType
      of SVarDecl:
        vars: seq[(string, CType, Expr)]
        # (name, type, init<opt>, align<opt>)
      of SDeclOnly:
        decl: CType

    ExprKind = enum
      EBin, EUnary, EPostFix, EIntLit, EFloatLit, EVoid,
      EVar, ECondition, ECast, ECall, ESubscript, EDefault,
      EArray, EStruct, EBackend, EString, EUndef, EVLAGetSize
      EPointerMemberAccess, EMemberAccess, ArrToAddress
    Expr = ref object
      ty: CType
      loc: Location
      case k: ExprKind
      of EVoid, ArrToAddress:
        voidexpr: Expr 
      of EVLAGetSize:
        vla: Expr
      of EBin:
        lhs, rhs: Expr
        bop: BinOP
      of EUnary:
        uop: UnaryOP
        uoperand: Expr
      of EPostFix:
        pop: PostfixOP
        poperand: Expr
      of EIntLit:
        ival: uintmax_t
      of EFloatLit:
        fval: float
      of EString:
        str: string
        is_constant: bool
      of EVar:
        sval: string
      of EArray, EStruct:
        arr: seq[Expr]
      of ECondition:
        cond: Expr
        cleft, cright: Expr
      of ECast:
        castop: CastOp
        castval: Expr
      of ECall:
        callfunc: Expr
        callargs: seq[Expr]
      of ESubscript:
        left, right: Expr
      of EPointerMemberAccess, EMemberAccess:
        obj: Expr
        idx: int
      of EBackend:
        p: pointer
      of EDefault, EUndef:
        discard

proc `$`(a: Stmt, level=0): string

proc `$`(e: Expr): string

proc `$`(a: CType, level=0): string

proc joinShow6(a: seq[(string, CType, Expr)]): string =
    result = ""
    for i in 0..<len(a):
        result.add(a[i][0])
        result.add(": ")
        result.add($a[i][1])
        if a[i][2] != nil:
          result.add('=')
          result.add($a[i][2])
        if i < (len(a)-1):
          result.add(", ")

proc joinShow5(a: seq[Stmt], level=1): string =
    var padding = "  ".repeat(level)
    result = "{\n"
    for i in a:
        result.add(padding & '\t')
        result.add(`$`(i, level + 1))
        result.add('\n')
    if level > 2:
      result.add("  ".repeat(level - 2))
    result.add("}")

proc joinShow[T](a: seq[T], c: string = " "): string =
    if len(a) == 0:
        return ""
    for i in 0..<(len(a)-1):
        result.add($a[i])
        result.add(c)
    result.add($a[^1])


proc `$`(a: Stmt, level=0): string =
  if a == nil:
    return "<nil>"
  case a.k:
  of SAsm:
    "__asm__(" & a.asms & ')'
  of SFunction:
    "Function " & a.funcname & " :\n" & $a.functy & `$`(a.funcbody, level + 1)
  of SVarDecl1:
    "<SVarDecl1>"
  of SVarDecl:
    joinShow6(a.vars)
  of SCompound:
    joinShow5(a.stmts, level+1)
  of SDefault:
    "default: " & $a.default_stmt
  of Scase:
    "case " & $a.case_expr & ':' & $a.case_stmt
  of SBreak:
    "break;"
  of SContinue:
    "continue;"
  of SSemicolon:
    ";"
  of SExpr:
    $a.exprbody & ';'
  of SReturn:
    if a.exprbody == nil:
      "return;"
    else:
      "return " & $a.exprbody & ';'
  of SGoto:
    "goto " & a.location & ';'
  of SLabled:
    a.label & ':' & $a.labledstmt
  of SIf:
    "if (" & $a.iftest & ") " & `$`(a.ifbody, level + 1) & (if a.elsebody==nil: "" else: "else " & `$`(a.elsebody, level + 1))
  of SWhile:
    "while (" & $a.test & ") " & `$`(a.body, level + 1)
  of SSwitch:
    "switch (" & $a.test & ") " & `$`(a.body, level + 1)
  of SDoWhile:
    "do " & `$`(a.body, level + 1) & " while (" & $a.test & ");"
  of SFor:
    "for (" & (if a.forinit==nil: ";" else: $a.forinit) & 
    (if a.forcond==nil: "" else: $a.forcond) & ';' &
    (if a.forincl==nil: "" else: $a.forincl) & ')' &
    `$`(a.forbody, level+1)
  of SDeclOnly:
    $(a.decl)

proc `$`(o: UnaryOP): string =
  case o:
  of Pos:
    "+"
  of UNeg, SNeg, FNeg:
    "-"
  of Not:
    "!"
  of AddressOf:
    "&&"
  of PrefixIncrement:
    "++"
  of PrefixDecrement:
    "--"
  of Dereference:
    ""
  of LogicalNot:
    "~"

proc `$`(o: BinOP): string =
  case o:
  of Nop: "<nop>"
  of UAdd, SAdd, FAdd, SAddP: "+"
  of USub, SSub, FSub, PtrDiff: "-"
  of UMul, SMul, FMul: ""
  of UDiv, SDiv, FDiv: "/"
  of URem, SRem, FRem: "%"
  of Shr, AShr: ">>"
  of Shl: "<<"
  of And: "&"
  of Xor: "^"
  of Or: "|"
  of LogicalAnd: "&&"
  of LogicalOr: "||"
  of Assign: "="
  of Comma: ","
  of EQ, FEQ: "=="
  of NE, FNE: "!="
  of UGT, SGT, FGT: ">"
  of UGE, SGE, FGE: ">="
  of ULT, SLT, FLT: "<"
  of ULE, SLE, FLE: "<="

proc `$`(e: Expr): string =
  if e == nil:
    return "<nil>"
  case e.k:
  of EVLAGetSize:
    "sizeof(" & $e.vla & ')'
  of EUndef:
    "<undefined-value>"
  of EVoid:
    "(void)" & $e.voidexpr
  of ArrToAddress:
    "&(" & $e.voidexpr & ")"
  of EDefault:
    "<zero>"
  of EStruct:
    "<struct-constant>"
  of EMemberAccess:
    $e.obj & '.' & $e.idx
  of EPointerMemberAccess:
    $e.obj & "->" & $e.idx
  of EString:
    repr(e.str)
  of EBin:
    '(' & $e.lhs & ' ' & $e.bop & ' ' & $e.rhs & ')'
  of EPostFix:
    $e.poperand & $e.pop
  of EUnary:
    $e.uop & $e.uoperand
  of EIntLit:
    $e.ival
  of EFloatLit:
    $e.fval
  of EVar:
    e.sval
  of ECondition:
    $e.cond & '?' & $e.cleft & ':' & $e.cright
  of ECast:
    '(' & $e.ty & ')' & $e.castval
  of ESubscript:
    $e.left & '[' & $e.right & ']'
  of ECall:
    $e.callfunc & '(' & $joinShow(e.callargs, ", ") & ')'
  of EArray:
    "{" & $e.arr & "}"
  of EBackend:
    "<backend>"

proc isConstant(e: Expr): bool =
    ## check whether a expression is a constant-expression
    case e.k:
    of EVoid, EVLAGetSize:
        false
    of ArrToAddress:
        true
    of EBin:
        isConstant(e.lhs) and isConstant(e.rhs)
    of EUnary:
        isConstant(e.uoperand)
    of EPostFix:
        false
    of EIntLit, EFloatLit, EString, EVar, EArray, EStruct:
        true
    of ECondition:
        isConstant(e.cond) and isConstant(e.cleft) and isConstant(e.cright)
    of ECast:
        true
    of ECall:
        false
    of ESubscript:
        false
    of EMemberAccess:
        isConstant(e.obj)
    of EPointerMemberAccess:
        false
    of EBackend:
        false
    of EDefault, EUndef:
        true


const tyCounter = CacheCounter"tyCounter"

proc make_ty(): uint32 =
  result = 1'u32 shl tyCounter.value
  tyCounter.inc()

const ## optional type tags
  TYINVALID = 0'u32
  # TYAUTO = make_ty()
  TYCONST = make_ty()
  TYRESTRICT = make_ty()
  TYVOLATILE = make_ty()
  TYATOMIC = make_ty()
  TYINLINE = make_ty()
  TYSTATIC = make_ty()
  TYNORETURN = make_ty()
  TYALIGNAS = make_ty()
  TYEXTERN = make_ty()
  TYREGISTER = make_ty()
  TYTHREAD_LOCAL = make_ty()
  TYTYPEDEF = make_ty()
  TYLVALUE = make_ty()
  TYPARAM = make_ty()

const ## basic types
  TYVOID = make_ty()
  TYBOOL = make_ty()
  TYCOMPLEX = make_ty()
  TYINT8 = make_ty()
  TYINT16 = make_ty()
  TYINT32 = make_ty()
  TYINT64 = make_ty()
  TYUINT8 = make_ty()
  TYUINT16 = make_ty()
  TYUINT32 = make_ty()
  TYUINT64 = make_ty()
  TYFLOAT = make_ty()
  TYDOUBLE = make_ty()

const ## type alias
  TYCHAR = TYINT8
  TYSHORT = TYINT16
  TYINT = TYINT32
  TYLONG = when CC_LONG64: TYINT64 else: TYINT32
  TYLONGLONG = TYINT64
  TYUCHAR = TYUINT8
  TYUSHORT = TYUINT16
  TYUINT = TYUINT32
  TYULONG = TYUINT64 ## 32 bit in MSVC, 64 bit in GCC/JVM
  TYULONGLONG = TYUINT64
  TYLONGDOUBLE = TYDOUBLE

const 
  prim = TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
    TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64 or 
    TYFLOAT or TYDOUBLE or
    TYBOOL
  unsigned = TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64

proc addTag2(a: uint32, dst: var string) =
  if bool(a and TYCONST): dst.add("const ")
  if bool(a and TYVOLATILE): dst.add("volatile ")
  if bool(a and TYRESTRICT): dst.add("restrict ")

proc noralizeType(ty: var CType) =
    const mask = TYTYPEDEF or TYEXTERN or TYSTATIC or TYTHREAD_LOCAL or TYREGISTER
    case ty.spec:
    of TYFUNCTION:
        var h = ty.ret.tags and mask
        ty.ret.tags = ty.ret.tags and (not mask)
        ty.tags = ty.tags or h
    of TYARRAY:
        var h = ty.arrtype.tags and mask
        ty.arrtype.tags = ty.arrtype.tags and (not mask)
        ty.tags = ty.tags or h
    of TYPOINTER:
        var h = ty.p.tags and mask
        ty.p.tags = ty.p.tags and (not mask)
        ty.tags = ty.tags or h
    else:
        discard

proc addTag(a: uint32, dst: var string) =
  addTag2(a, dst)
  if bool(a and TYATOMIC): dst.add("_Atomic ")
  if bool(a and TYINLINE): dst.add("inline ")
  if bool(a and TYSTATIC): dst.add("static ")
  if bool(a and TYNORETURN): dst.add("_Noreturn ")
  if bool(a and TYALIGNAS): dst.add("_Alignas ")
  if bool(a and TYEXTERN): dst.add("extern ")
  if bool(a and TYREGISTER): dst.add("register ")
  if bool(a and TYTHREAD_LOCAL): dst.add("_Thread_local ")
  if bool(a and TYTYPEDEF): dst.add("typedef ")

proc joinShow2[A, B](a: seq[(A, B)]): string =
    if len(a) == 0:
        return ""
    for i in 0..<(len(a)-1):
        result.add($a[i][1])
        result.add(' ')
        result.add($a[i][0])
        result.add(", ")
    result.add($a[^1][1] & ' ' & $a[^1][0])

proc `$`(a: CType, level=0): string =
  if a == nil:
    return "<nil>"
  result = ""
  case a.spec:
  of TYINCOMPLETE:
    case a.tag:
    of TYSTRUCT:
        result.add("struct ")
    of TYUNION:
        result.add("union ")
    of TYENUM:
        result.add("enum ")
    else:
        unreachable()
    result.add(a.name)
  of TYPRIM:
      addTag(a.tags, result)
      let s = (
        if bool(a.tags and TYVOID): "void"
        elif bool(a.tags and TYBOOL): "_Bool"
        elif bool(a.tags and TYCOMPLEX): "_Complex"
        elif bool(a.tags and TYINT8): "char"
        elif bool(a.tags and TYINT16): "short"
        elif bool(a.tags and TYINT32): "int"
        elif bool(a.tags and TYINT64): "long long"
        elif bool(a.tags and TYUINT8): "unsigned char"
        elif bool(a.tags and TYUINT16): "unsigned short"
        elif bool(a.tags and TYUINT32): "unsigned int"
        elif bool(a.tags and TYUINT64): "unsigned long long"
        elif bool(a.tags and TYFLOAT): "float"
        elif bool(a.tags and TYDOUBLE): "double"
        else: ""
      )
      if s.len > 0:
        result.add(s)
  of TYENUM:
    addTag(a.tags, result)
    result.add("enum " & a.ename)
  of TYFUNCTION:
    result.add($a.ret & " (*)(" & joinShow2(a.params) & ')')
  of TYSTRUCT:
    addTag(a.tags, result)
    result.add("struct " & a.sname)
  of TYUNION:
    addTag(a.tags, result)
    result.add("union " & a.sname)
  of TYBITFIELD:
    result.add($a.bittype & " : " &  $a.bitsize)
  of TYPOINTER:
    result.add('(')
    addTag2(a.tags, result)
    result.add($a.p)
    result.add("*)")
  of TYARRAY:
    result.add($a.arrtype & " [" & (if a.vla != nil: $a.vla else: (if a.hassize: $a.arrsize else: "")) & "]")

proc make_primitive(tag: uint32): CType =
  CType(tags: tag, spec: TYPRIM)

type TypeCache = object
    b, v, i8, u8, i16, u16, i32, u32, i64, u64, ffloatty, fdoublety, strty: CType

var tycache = TypeCache(
  b: make_primitive(TYBOOL),
  v: make_primitive(TYVOID),
  i8: make_primitive(TYINT8),
  u8: make_primitive(TYUINT8),
  i16: make_primitive(TYINT16),
  u16: make_primitive(TYINT16),
  i32: make_primitive(TYINT32),
  u32: make_primitive(TYUINT32),
  i64: make_primitive(TYINT64),
  u64: make_primitive(TYUINT64),
  ffloatty: make_primitive(TYFLOAT),
  fdoublety: make_primitive(TYDOUBLE),
  strty: CType(spec: TYPOINTER, tags: TYINVALID, p: make_primitive(TYINT8))
)

proc iintty(self: TypeCache): CType =
    when TYINT == TYINT32:
        self.i32
    else:
        self.i64

proc uintty(self: TypeCache): CType =
    when TYUINT == TYUINT32:
        self.u32
    else:
        self.u64

proc ilongty(self: TypeCache): CType =
    when CC_LONG64:
        self.i64
    else:
        self.i32

proc ulongty(self: TypeCache): CType =
    when CC_LONG64:
        self.u64
    else:
        self.u32

proc iwcharty(self: TypeCache): CType =
    when CC_WCHAR32:
        self.i32
    else:
        self.i16

proc ilonglongty(self: TypeCache): CType =
    self.i64

proc ulonglongty(self: TypeCache): CType =
    self.u64

proc ushortty(self: TypeCache): CType =
    self.u16

proc iintptrty(self: TypeCache): CType =
    if app.pointersize == 4:
        self.i32
    else:
        self.i64

proc iptrdiff_tty(self: TypeCache): CType =
    self.iintptrty

proc isize_tty(self: TypeCache): CType =
    self.iintptrty

proc getPointerType(base: CType): CType = CType(tags: TYINVALID, spec: TYPOINTER, p: base)

proc compatible(p, expected: CType): bool =
    ## https://en.cppreference.com/w/c/language/type
    if p.spec != expected.spec:
        return false
    else:
        case p.spec:
        of TYPRIM:
            return (p.tags and prim) == (expected.tags and prim)
        of TYFUNCTION:
            if not compatible(p.ret, expected.ret):
                return false
            for i in 0..<len(p.params):
                if p.params[i][1] == nil:                    
                    break
                if expected.params[i][1] == nil:
                    break
                if not compatible(p.params[i][1], expected.params[i][1]):
                    return false
            return true
        of TYSTRUCT, TYENUM, TYUNION:
            return cast[pointer](p) == cast[pointer](expected)
        of TYPOINTER:
            const mask = TYRESTRICT or TYCONST or TYVOLATILE
            return bool(p.p.tags and TYVOID) or bool(expected.p.tags and TYVOID) or (((p.tags and mask) == (expected.tags and mask)) and compatible(p.p, expected.p))
        of TYINCOMPLETE:
            return p.tag == expected.tag and p.name == expected.name
        of TYBITFIELD:
            unreachable()
        of TYARRAY:
            return compatible(p.arrtype, expected.arrtype)

when TYINT == TYINT32:
    const sizeofint = 4.culonglong
else:
    const sizeofint = 8.culonglong

proc constStr(s: cstring | string): StringRef = StringRef(len: len(s), str: s)

proc `$`(self: StringRef): string = $self.str

proc `&`(s: string, c: StringRef): string =
    s & $c.str

proc `[]`(self: StringRef, idx: auto): char =
    assert idx < self.len, "index too large for const string!"
    self.str[idx]

proc len(self: StringRef): int = self.len

proc `==`(a: StringRef, b: string): bool =
    if len(a) != len(b):
        return false
    for i in 0 ..< len(a):
        if a.str[i] != b[i]:
            return false
    return true

proc `==`(a: string, b: StringRef): bool =
    if len(a) != len(b):
        return false
    for i in 0 ..< len(a):
        if a[i] != b.str[i]:
            return false
    return true

proc `==`(a, b: StringRef): bool =
    if a.len != b.len:
        return false
    for i in 0 ..< a.len:
        if a.str[i] != b.str[i]:
            return false
    return true

type
  Info = ref object
    tag: uint32
    loc: Location
    ty: CType
  MessageType = enum
    MFlush, MRaw, MRawLine, MNote, MWarning, MVerbose, MError, MTypeError, MEvalError, MParseError, Mperror
  MessageWriter = proc (s: StringRef, t: MessageType) {.raises: [], locks: 0, cdecl.}
  Preprocessor {.final.} = object ## The C Preprocessor
    counter: uint ## `__COUNTER__`
    onces: HashSet[string] ## `#pragma once`
    ok: bool ## preprocessor condition is false: `#if 0`
    macros: Table[string, PPMacro] ## defined macros
    ppstack: seq[uint8] ## preprocessor condition stack
    want_expr: bool ## true if parsing `#if`
    expansion_list: HashSet[string] ## current expanding macros
    eval_error: bool ## error during evalate constant expressions
    eval_error_msg: string ## eval error message
    flags: PPFlags ## preprocessor flags
  BScope {.final.} = object
    tags: Table[string, Info] ## struct/union/enums
    typedefs: Table[string, Info] ## typedef, variables
    enums: Table[string, uintmax_t] ## enum constants
  Sema {.final.} = object ## Semantics processor
    scopes: seq[BScope] # block scope
    currentfunctionRet, currentInitTy, currentCase: CType
    pfunc: string ## current function name: `__func__`
    retTy: CType ## current function return type
    currentAlign: uint32 ## current align(bytes)
    type_error: bool ## type error
    lables: seq[Table[string, uint8]] ## lable scope
  Lexer {.final.} = object
    tok: TokenV
    line: uint32 ## current line
    col: uint32 ## current column
    c: char ## current char
    lastc: uint16
  Parser {.final.} = object ## Parser can parse a translation unit.A translation unit is many input files, including `#include <xxx>`
    tokenq: seq[TokenV]
    l: Lexer ## current Lexer
    pp: Preprocessor
    sema: Sema
    w: MessageWriter
    fstack: seq[Stream] ## input files
    filenamestack, pathstack: seq[string]
    locstack: seq[Location] 
    filename, path: string
    parse_error: bool ## parse error
    bad_error: bool ## other error
  BasicTypeIndex = enum
    DIvoidty, DIi1, DIi8, DIi16, DIi32, DIi64, DIffloat, DIfdouble, DIintPtr, DIptr
  BasicType = object
    ty: Type
    di: DIType
  Backend = object
    types: array[BasicTypeIndex, BasicType]
    ## other signed types
    di8, di16, di32, di64: DIType
    ## jump labels
    labels: seq[Table[string, Label]]
    ## struct/union
    tags: seq[Table[string, Type]]
    ## variables
    vars: seq[Table[string, Value]]
    dtags: seq[Table[string, DIType]]
    ## module
    m: ModuleRef
    module: ModuleRef
    builder: BuilderRef
    di: DIBuilderRef
    lexBlocks: seq[DIScope]
    machine: TargetMachineRef
    currentfunction: Value
    tsCtx: OrcThreadSafeContextRef
    ctx: ContextRef
    layout: TargetDataRef
    triple: tripleRef
    archName: cstring
    arch: ArchType
    os: OSType
    env: EnvironmentType
    f: uint32 ## some flags
    ## jump labels
    topBreak: Label
    topTest: Value
    topdefaultCase: Label
    topSwitch: Label
    topContinue: Label
    target: TargetRef
    topCase: Label
    ## `i32 1/0` constant
    i32_1, i32_0: Value
    ## LLVM false/true constant
    i1_0, i1_1: Value
    ## true when gen expression statement
    expr_stmt: bool
    ## libtrace support
    lt_callframe, lt_call_ty: Type
    lt_begin_call, lt_end_call: Value
    lt_now: Value

var b = Backend()

# clang use i8 for C's void types(LLVM's void type void type only allowed for function results)
# for the code
#   `extern void foo;`
# will produce 
#   `@foo = external global i8, align 1`
# and `void*` is `i8*` or `ptr` in clang

proc getBasicTypeIndex(a: uint32): BasicTypeIndex =
    if (a and TYBOOL) != 0: DIi1
    elif (a and (TYINT8 or TYUINT8 or TYVOID)) != 0: DIi8
    elif (a and (TYINT16 or TYUINT16)) != 0: DIi16
    elif (a and (TYINT32 or TYUINT32)) != 0: DIi32
    elif (a and (TYINT64 or TYUINT64)) != 0: DIi64
    elif (a and TYFLOAT) != 0: DIffloat
    elif (a and TYDOUBLE) != 0: DIfdouble
    else:
      unreachable()
      DIi8

proc `$`(loc: Location): string =
  $loc.line & ':' & $loc.col

proc conditional_expression(): Expr

proc type_name(): (CType, bool)

proc expression(): Expr

proc primary_expression(): Expr

proc postfix_expression(): Expr

proc unary_expression(): Expr

proc cast_expression(): Expr

proc multiplicative_expression(): Expr

proc shift_expression(): Expr

proc relational_expression(): Expr

proc equality_expression(): Expr

proc AND_expression(): Expr

proc exclusive_OR_expression(): Expr

proc inclusive_OR_expression(): Expr

proc logical_AND_expression(): Expr

proc logical_OR_expression(): Expr

proc conditional_expression(start: Expr): Expr

proc assignment_expression(): Expr

proc initializer_list(): Expr

proc parameter_type_list(): (bool, seq[(string, CType)])

proc declaration(): Stmt

proc type_qualifier_list(ty: var CType)

proc merge_types(ts: seq[Token]): Ctype

proc declaration_specifiers(): CType

proc specifier_qualifier_list(): CType

proc constant_expression(): Expr =
    conditional_expression()

proc warn_if_bad_cast(e: Expr, ty: CType, msg: string): bool

proc consoleColoredWritter(s: StringRef, ty: MessageType) {.raises: [], locks: 0, cdecl.}

var t = Parser(
        pp: Preprocessor(
            counter: 0,
            onces: initHashSet[string](),
            ok: true,
            macros: initTable[string, PPMacro](),
            ppstack: newSeqOfCap[uint8](5),
            want_expr: false,
            eval_error: false,
            flags: PFNormal
        ),
        sema: Sema(),
        w: consoleColoredWritter,
        l: Lexer(
            line: 1,
            col: 1,
            c: ' ',
            lastc: 256,
            tok: TokenV(tok: TNul, tags: TVNormal)
        ),
        parse_error: false,
        bad_error: false
    )

proc getLoc(): Location {.inline.} =
    Location(line: t.l.line, col: t.l.col, fd: 0)

proc write(s: StringRef, ty: MessageType) {.inline.} =
    t.w(s, ty)

proc write(s: cstring | string, ty: MessageType) {.inline.} =
    t.w(StringRef(str: s, len: len(s)), ty)

proc consoleColoredWritter(s: StringRef, ty: MessageType) {.raises: [], locks: 0, cdecl.} =
    if ty == MRaw or ty == MVerbose:
        app.strbuf.setLen 0
        app.strbuf.add(s.str)
        fstderr << app.strbuf
        return
    elif ty == MFlush:
        fstderr << app.strbuf
        return
    elif ty == Mperror:
        app.strbuf.add("cc: ")
        perror(s.str)
        return
    elif ty == MRawLine:
        app.strbuf.setLen 0
        app.strbuf.add(s.str)
        app.strbuf.add('\n')
        fstderr << app.strbuf
        return
    app.strbuf.setLen 0
    when USE_SetConsoleTextAttribute:
        fstderr << "cc: "
    else:
        app.strbuf.add("cc: ")
    if app.isAttyStderr:
        if ty == MError or ty == MTypeError:
            # red
            when USE_SetConsoleTextAttribute:
                discard SetConsoleTextAttribute(app.hStderr, FOREGROUND_RED)
            else:
                app.strbuf.add("\e[31m")
        elif ty == MNote:
            # green
            when USE_SetConsoleTextAttribute:
                discard SetConsoleTextAttribute(app.hStderr, FOREGROUND_GREEN)
            else:
                app.strbuf.add("\e[32m")
        elif ty == MWarning:
            # purple
            when USE_SetConsoleTextAttribute:
                discard SetConsoleTextAttribute(app.hStderr, FOREGROUND_BLUE or FOREGROUND_RED)
            else:
                app.strbuf.add("\e[35m")
        elif ty == MParseError:
            # blue
            when USE_SetConsoleTextAttribute:
                discard SetConsoleTextAttribute(app.hStderr, FOREGROUND_BLUE)
            else:
                app.strbuf.add("\e[34m")
        elif ty == MEvalError:
            # blue
            when USE_SetConsoleTextAttribute:
                discard SetConsoleTextAttribute(app.hStderr, FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_INTENSITY)
            else:
                app.strbuf.add("\e[36m")
    if ty == MNote:
        app.strbuf.add("note")
    elif ty == MWarning:
        app.strbuf.add("warning")
    elif ty == MError:
        app.strbuf.add("error")
    elif ty == MEvalError:
        app.strbuf.add("eval error")
    elif ty == MTypeError:
        app.strbuf.add("type error")
    elif ty == MParseError:
        app.strbuf.add("parse error")
    when USE_SetConsoleTextAttribute:
        fstderr << app.strbuf    
        if app.isAttyStderr:
            discard SetConsoleTextAttribute(app.hStderr, hStderrInfo.wAttributes)
        app.strbuf.setLen 0
        app.strbuf.add(": ")
        app.strbuf.add(s.str)
        app.strbuf.add('\n')
        fstderr << app.strbuf
    else:
        app.strbuf.add("\e[0m")
        app.strbuf.add(": ")
        app.strbuf.add(s.str)
        app.strbuf.add('\n')
        fstderr << app.strbuf

proc showToken(): string =
  case t.l.tok.tok:
  of TNumberLit: '\'' & $t.l.tok.i & '\''
  of TPPNumber: '\'' & t.l.tok.s & '\''
  of TCharLit: show(char(t.l.tok.i))
  of TIdentifier, CPPIdent: '\'' & t.l.tok.s & '\''
  of TSpace: "'space'"
  of PPSharp: "'#'"
  of PPSharpSharp: "'##'"
  of TFloatLit: '\'' & $t.l.tok.f & '\''
  of TEllipsis2: "'..'"
  of TEllipsis: "'...'"
  of TStringLit: '"' & t.l.tok.s & '"'
  of TEOF: "<EOF>"
  of TNul: "<null>"
  else:
    if t.l.tok.tok < T255:
       show(char(int(t.l.tok.tok)))
    else:
      "'keyword " & $t.l.tok.tok & '\''

proc make_tok(op: Token) {.inline.} =
    t.l.tok = TokenV(tok: op, tags: TVNormal)

proc make_ch_lit(ch: char) {.inline.} =
    t.l.tok.i = uintmax_t(ch)

template error(msg: untyped) =
    write(msg, MError)

template raw_message(msg: untyped) =
    write(msg, MRaw)

template raw_message_line(msg: untyped) =
    write(msg, MRawLine)

template myperror(msg: untyped) =
    write(msg, Mperror)

template type_error(msg: untyped) =
    write(msg, MTypeError)
    t.sema.type_error = true

template eval_error(msg: untyped) =
    write(msg, MEvalError)
    t.sema.type_error = true

proc error_incomplete(ty: CType) =
  let s = if ty.tag == TYSTRUCT:  "struct" else: (if ty.tag == TYUNION: "union" else: "enum")
  type_error("use of incomplete type '" & s & " " & ty.name & '\'')

template parse_error(msg: untyped) =
    if t.parse_error == false and t.sema.type_error == false:
      write(msg, MParseError)
      t.parse_error = true

template warning(msg: untyped) =
    if ord(app.verboseLevel) >= ord(VWarning):
        write(msg, MWarning)

template flush() =
    write(app.strbuf, MFlush)

template expect(msg: untyped) =
    ## emit `expect ...` error message
    parse_error("expect " & msg & ", got " & showToken())

template expectExpression() =
    expect("expression")

template expectStatement() =
    expect("statement")

template expectLB() =
    expect("'('")

template expectRB() =
    expect("')'")

template verbose(msg: untyped) =
    if ord(app.verboseLevel) >= ord(VVerbose):
        write(msg, MVerbose)

template note(msg: untyped) =
    if ord(app.verboseLevel) >= ord(VNote):
        write(msg, MNote)

proc putToken() = 
    t.tokenq.add(t.l.tok)

proc addFile(filename: string) =
    if filename in t.pp.onces:
        return
    let s = newFileStream(filename)
    if s == nil:
        error(filename)
        return
    t.fstack.add(s)
    t.filenamestack.add(t.filename)
    t.pathstack.add(t.path)
    t.locstack.add(Location(line: t.l.line, col: t.l.col))
    t.filename = filename
    t.path = filename
    t.l.line = 1
    t.l.col = 1

proc setStdin() =
  t.fstack.add(newStdinStream())
  t.filenamestack.add(t.filename)
  t.pathstack.add(t.path)
  t.locstack.add(Location(line: t.l.line, col: t.l.col))
  t.filename = "<stdin>"
  t.path = "/dev/stdin"
  t.l.line = 1
  t.l.col = 1
  app.output = "stdin"

proc readChar(s: Stream): char {.raises: [].} =
    ## try to read from stream, if any error happens, return '\0'(EOF)
    case s.k:
    of FileStream:
        if s.pos == 0:
            let status = read(s.fd, s.buf, STREAM_BUFFER_SIZE)
            if status <= 0:
                return '\0'
            s.pos = uint(status)
            s.p = 0
        let str = cast[cstring](s.buf)
        let c = str[s.p]
        inc s.p
        dec s.pos
        if c == '\0':
            warning("null character(s) ignored")
            return readChar(s)
        return c
    of StringStream:
        if s.i >= len(s.s):
            return '\0'
        result = s.s[s.i]
        inc s.i
    of StdinStream:
        if s.i >= len(s.s):
            when CC_NO_RAEADLINE:
                fstderr << STDIN_PROMPT
                try:
                    s.s = stdin.readLine()
                except EOFError, IOError:
                    return '\0'
            else:
                var line = readLine(STDIN_PROMPT)
                if line == nil:
                    return '\0'
                s.s = $line
                add_history(line)
                free(line)
                s.s.add('\n')
            s.i = 0
        result = s.s[s.i]
        inc s.i

proc my_UNEG(a: uintmax_t): uintmax_t {.importc: "myopneg", nodecl, header: "myop.h".}

proc my_SNEG(a: intmax_t): uintmax_t {.importc: "myopneg", nodecl, header: "myop.h".}

proc `&&`(a, b: uintmax_t): uintmax_t {.importc: "myopand", nodecl, header: "myop.h".}

proc `||`(a, b: uintmax_t): uintmax_t {.importc: "myopor", nodecl, header: "myop.h".}

proc `!`(a: uintmax_t): uintmax_t {.importc: "myopnot", nodecl, header: "myop.h".}

proc eval_error2(msg: string): uintmax_t =
  t.pp.eval_error = true
  t.pp.eval_error_msg = msg
  return 0

proc write_eval_msg() = 
    eval_error("error when evaluating constant-expression: " & t.pp.eval_error_msg)

proc evali(e: Expr): uintmax_t

proc casti(e: Expr): uintmax_t =
    if bool(e.ty.tags and TYBOOL):
        return evali(e.castval) and 1
    if bool(e.ty.tags and (TYINT8 or TYUINT8)):
        return evali(e.castval) and high(uint8)
    if bool(e.ty.tags and (TYINT16 or TYUINT16)):
        return evali(e.castval) and high(uint16)
    if bool(e.ty.tags and (TYINT32 or TYUINT32)):
        return evali(e.castval) and high(uint32)
    else:
        return evali(e.castval)

proc evali(e: Expr): uintmax_t =
  ## run the constant expression
  ##
  ## if error, setting eval_error
  case e.k:
  of EBin:
    case e.bop:
    of SAdd, UAdd:
      evali(e.lhs) + evali(e.rhs)
    of SSub, USub:
      evali(e.lhs) - evali(e.rhs)
    of SMul, UMul:
      evali(e.lhs) * evali(e.rhs)
    of UDiv:
      evali(e.lhs) div evali(e.rhs)
    of SDiv:
      uintmax_t(intmax_t(evali(e.lhs)) div intmax_t(evali(e.rhs)))
    of URem:
      evali(e.lhs) mod evali(e.rhs)
    of SRem:
      uintmax_t(intmax_t(evali(e.lhs)) mod intmax_t(evali(e.rhs)))
    of Shr:
      evali(e.lhs) shr evali(e.rhs)
    of AShr:
      uintmax_t(ashr(intmax_t(evali(e.lhs)), evali(e.rhs)))
    of EQ:
      if evali(e.lhs) == evali(e.rhs): 1 else: 0
    of NE:
      if evali(e.lhs) != evali(e.rhs): 1 else: 0
    of UGE:
      if evali(e.lhs) >= evali(e.rhs): 1 else: 0
    of UGT:
      if evali(e.lhs) > evali(e.rhs): 1 else: 0
    of ULE:
      if evali(e.lhs) <= evali(e.rhs): 1 else: 0
    of ULT:
      if evali(e.lhs) < evali(e.rhs): 1 else: 0
    of SGE:
      if intmax_t(evali(e.lhs)) >= intmax_t(evali(e.rhs)): 1 else: 0
    of SGT:
      if intmax_t(evali(e.lhs)) > intmax_t(evali(e.rhs)): 1 else: 0
    of SLE:
      if intmax_t(evali(e.lhs)) <= intmax_t(evali(e.rhs)): 1 else: 0
    of SLT:
      if intmax_t(evali(e.lhs)) < intmax_t(evali(e.rhs)): 1 else: 0
    of And:
      evali(e.lhs) and evali(e.rhs)
    of Xor:
      evali(e.lhs) xor evali(e.rhs)
    of Or:
      evali(e.lhs) or evali(e.rhs)
    of LogicalAnd:
      evali(e.lhs) && evali(e.rhs)
    of LogicalOr:
      evali(e.lhs) || evali(e.rhs)
    else:
      eval_error2($e)
  of EUnary:
    case e.uop:
    of Pos:
        evali(e.uoperand)
    of UNeg:
      my_UNEG(evali(e.uoperand))
    of SNeg:
        my_SNEG(intmax_t(evali(e.uoperand)))
    of LogicalNot:
        ! evali(e.uoperand)
    of Not:
        not evali(e.uoperand)
    else:
        eval_error2($e)
  of ECast:
    case e.castop:
    of ZExt, SExt:
        evali(e.castval)
    of Trunc:
        casti(e)
    else:
        eval_error2("cannot cast to " & $e.ty)
  of EIntLit:
    e.ival
  of EFloatLit:
    eval_error2("floating constant in constant-expression")
  of ECondition:
    if evali(e.cond) != 0: evali(e.cleft) else: evali(e.cright)
  else:
    eval_error2($e)

proc getToken()

proc nextTok()

proc isalnum(c: char): bool =
    return c in {'A'..'Z', 'a'..'z', '0'..'9'}

proc resetLine() =
    t.l.col = 1
    inc t.l.line

proc stream_read() =
    if t.l.lastc <= 0xFF:
        t.l.c = char(t.l.lastc)
        t.l.lastc = 256
        return
    if t.fstack.len == 0:
        t.l.c = '\0'
    else:
        let s = t.fstack[^1]
        t.l.c = s.readChar()
        inc t.l.col
        if t.l.c == '\0':
            let fd = t.fstack.pop()
            t.filename = t.filenamestack.pop()
            t.path = t.pathstack.pop()
            let loc = t.locstack.pop()
            t.l.line = loc.line
            t.l.col = loc.col
            fd.close()
            stream_read()
        elif t.l.c == '\n':
          resetLine()
        elif t.l.c == '\r':
          resetLine()
          t.l.c = '\n'
          let c = s.readChar()
          if c != '\n':
            t.l.lastc = uint16(c)

proc eat() =
    stream_read()
    if t.l.c == '/':
        stream_read()
        if t.l.c == '*':
            while true:
                stream_read()
                if t.l.c == '*':
                    stream_read()
                    if t.l.c == '/':
                        t.l.c = ' ' # make a space: GNU CPP do it
                        break
                elif t.l.c == '\0':
                    parse_error("expect '/' before EOF")
                    return
        elif t.l.c == '/':
            while true:
                stream_read()
                if t.l.c == '\n' or t.l.c == '\0':
                    break
        else:
            t.l.lastc = uint16(t.l.c)
            t.l.c = '/'
    elif t.l.c == '\\':
        let c = t.l.c
        stream_read()
        if t.l.c == '\n':
            eat()
        else:
            t.l.lastc = uint16(t.l.c)
            t.l.c = c

proc readHexChar(): Codepoint =
    var n = Codepoint(0)
    while true:
        if t.l.c in {'0'..'9'}:
            n = (n shl 4.Codepoint) or (Codepoint(t.l.c) - '0'.Codepoint)
        elif t.l.c in {'a'..'f'}:
            n = (n shl 4.Codepoint) or (Codepoint(t.l.c) - 'a'.Codepoint + 10.Codepoint)
        elif t.l.c in {'A'..'F'}:
            n = (n shl 4.Codepoint) or (Codepoint(t.l.c) - 'A'.Codepoint + 10.Codepoint)
        else:
            return n
        eat()

proc readFloatSuffix() =
    if t.l.c == 'F' or t.l.c == 'f':
        t.l.tok.ftag = Ffloat

proc readSuffix() =
    if t.l.c == 'U' or t.l.c == 'u':
        case t.l.tok.itag:
        of Iint:
            t.l.tok.itag = Iuint
        of Ilong:
            t.l.tok.itag = Iulong
        of Ilonglong:
            t.l.tok.itag = Iulonglong
        else:
            parse_error("double 'u' suffix in integer constant")
            return
    else:
        case t.l.tok.itag:
        of Iint:
            t.l.tok.itag = Ilong
        of Iuint:
            t.l.tok.itag = Iulong
        of Ilong:
            t.l.tok.itag = Ilonglong
        of Iulong:
            t.l.tok.itag = Iulonglong
        of Ilonglong, Iulonglong:
            parse_error("more than 2 'l' suffix in integer constant")
            return

proc validUCN(codepoint: Codepoint) =
    if codepoint > 0x10FFFF:
        warning("codepoint too large")
    if codepoint >= 0xD800 and codepoint <= 0xDFFF:
        warning("universal character in surrogate range")

proc validUCNName(codepoint: Codepoint) =
    if codepoint <= 0x009F'u32 and codepoint != Codepoint('`') and codepoint != Codepoint('$') and codepoint != Codepoint('@'):
        warning("codepoint " & $codepoint &  " cannot be a universal character name")

proc readUChar(count: int): Codepoint =
    var n = 0'u32
    var i = 0
    while true:
        inc i
        if t.l.c in {'0'..'9'}:
            n = (n shl 4) or (Codepoint(t.l.c) - '0'.Codepoint)
        elif t.l.c in {'a'..'f'}:
            n = (n shl 4) or (Codepoint(t.l.c) - 'a'.Codepoint + 10'u32)
        elif t.l.c in {'A'..'F'}:
            n = (n shl 4) or (Codepoint(t.l.c) - 'A'.Codepoint + 10'u32)
        else:
            warning("expect " & $count & " hex digits for universal character")
            break
        eat()
        if i == count:
            break
    validUCN(n)
    return n

proc readEscape(): Codepoint =
    eat()
    case t.l.c:
    of 'a':
        eat()
        return Codepoint(7)
    of 'b':
        eat()
        return Codepoint(8)
    of 'f':
        eat()
        return Codepoint(12)
    of 'n':
        eat()
        return Codepoint(10)
    of 'r':
        eat()
        return Codepoint(13)
    of 't':
        eat()
        return Codepoint(9)
    of 'v':
        eat()
        return Codepoint(11)
    of 'e', 'E':
        eat()
        return Codepoint(27)
    of 'x':
        eat()
        readHexChar()
    of 'u', 'U':
        let n = if t.l.c == 'U': 8 else: 4
        eat()
        return readUChar(n)
    of '0' .. '7':
        const octalChs = {'0'..'7'}
        var n = Codepoint(t.l.c) - '0'.Codepoint
        eat() # eat first
        if t.l.c in octalChs:
            n = (n shl 3.Codepoint) or (Codepoint(t.l.c) - '0'.Codepoint)
            eat() # eat second
            if t.l.c in octalChs:
                n = (n shl 3.Codepoint) or (Codepoint(t.l.c) - '0'.Codepoint)
                eat() # eat third
        return n
    else:
        let c = t.l.c.Codepoint
        eat()
        return c

proc readCharLit(tag: ITag = Iint) =
    t.l.tok = TokenV(tok: TCharLit, tags: TVIVal, itag: tag)
    eat() # eat '
    if t.l.c == '\\':
        let codepoint = readEscape()
        if codepoint > 0xFF:
            warning("character constant too large")
        make_ch_lit(char(codepoint and 0xFF))
    else:
        make_ch_lit(t.l.c)
        eat()
    if t.l.c != '\'':
        parse_error("expect ' , got " & show(t.l.c))
    eat()

proc readIdentLit() =
    while true:
      if t.l.c == '\\':
        eat()
        if t.l.c == 'U':
            eat()
            let codepoint = readUChar(8)
            validUCNName(codepoint)
            t.l.tok.s.add(Rune(codepoint).toUTF8)
        elif t.l.c == 'u':
            eat()
            let codepoint = readUChar(4)
            validUCNName(codepoint)
            t.l.tok.s.add(Rune(codepoint).toUTF8)
        else:
            parse_error("invalid escape in identifier")
            return
      else:
        if not (isalnum(t.l.c) or (uint8(t.l.c) and 0x80'u8) != 0 or t.l.c == '$' or t.l.c == '_'):
            break
        t.l.tok.s.add(t.l.c)
        eat()

proc decimaltoInt(s: string): uintmax_t =
    for c in s:
        result = (result * 10) + (uintmax_t(c) - '0'.uintmax_t)

proc decimal16toInt(s: string): uintmax_t =
    for c in s:
        if c in {'0'..'9'}:
            result = (result shl 4) or (uintmax_t(c) - '0'.uintmax_t)
        elif c in {'a'..'f'}:
            result = (result shl 4) or (uintmax_t(c) - 'a'.uintmax_t + 10)
        else:
            result = (result shl 4) or (uintmax_t(c) - 'A'.uintmax_t + 10)

proc decimal2toInt(s: string): uintmax_t =
    for c in s:
        if c == '1':
            result = (result shl 1) or 1
        else:
            result = result shl 1

proc pow10(a: uintmax_t): uintmax_t =
    result = 1
    for i in 0..<a:
        result = 10

proc readHexFloatLit(intPart: float = 0.0) =
    # read a float start from '.'
    t.l.tok = TokenV(tok: TFloatLit, tags: TVFVal, ftag: Fdobule)
    t.l.tok.f = intPart
    var e = 1.0
    while true:
        e /= 16.0
        eat()
        if t.l.c in {'0'..'9'}:
          t.l.tok.f += float(int(t.l.c) - '0'.int) * e
        elif t.l.c in {'a'..'f'}:
          t.l.tok.f += float(int(t.l.c) - 'a'.int + 10) * e
        elif t.l.c in {'A'..'F'}:
          t.l.tok.f += float(int(t.l.c) - 'A'.int + 10) * e
        else:
            if t.l.c in {'L', 'l', 'F', 'f'}:
                readFloatSuffix()
                eat()
                break
            if t.l.c != 'P' and t.l.c != 'p':
                parse_error("expect p or P in hex floating constant, got " & show(t.l.c))
                break
            eat()
            var negate = false
            if t.l.c == '-':
                negate = true
                eat()
            elif t.l.c == '+':
                eat()
            elif t.l.c notin {'0'..'9'}:
                parse_error("expect exponent digits")
            var exps: string
            while true:
                exps.add(t.l.c)
                eat()
                if t.l.c notin {'0'..'9'}:
                    break
            t.l.tok.f = pow(2, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
            if t.l.c in {'L', 'l', 'F', 'f'}:
                readFloatSuffix()
                eat()
            break

# read a float start from '.'
proc readFloatLit(intPart: float = 0.0) =
    t.l.tok = TokenV(tok: TFloatLit, tags: TVFval, ftag: Fdobule)
    t.l.tok.f = intPart
    var e = 0
    while true:
        inc e
        if t.l.c notin {'0'..'9'}:
            if t.l.c in {'L', 'l', 'F', 'f'}:
                readFloatSuffix()
                eat()
                break
            if t.l.c != 'E' and t.l.c != 'e':
                break
            eat()
            var negate = false
            if t.l.c == '-':
                negate = true
                eat()
            elif t.l.c == '+':
                eat()
            elif t.l.c notin {'0'..'9'}:
                parse_error("expect exponent digits")
                return
            var exps: string
            while true:
                exps.add(t.l.c)
                eat()
                if t.l.c notin {'0'..'9'}:
                    break
            t.l.tok.f = pow(10, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
            if t.l.c in {'L', 'l', 'F', 'f'}:
                readFloatSuffix()
                eat()
            break
        t.l.tok.f += float(int(t.l.c) - '0'.int) / pow(10.0, float(e))
        eat()

proc readNumberLit() =
    t.l.tok = TokenV(tok: TNumberLit, tags: TVIVal, itag: Iint)
    t.l.tok.i = 0
    if t.l.c == '0':
        eat()
        case t.l.c:
        of '0'..'7': # octal
          discard
        of 'x', 'X': # hex
          eat()
          if t.l.c notin {'0'..'9', 'A'..'F', 'a'..'f'}:
            parse_error("invalid hex literal: " & show(t.l.c))
            return
          while true:
            if t.l.c in {'0'..'9'}:
              t.l.tok.i = (t.l.tok.i shl 4) or (uintmax_t(t.l.c) - uintmax_t('0'))
            elif t.l.c in {'a'..'f'}:
              t.l.tok.i = (t.l.tok.i shl 4) or (uintmax_t(t.l.c) -  uintmax_t('a') + 10)
            elif t.l.c in {'A'..'F'}:
              t.l.tok.i = (t.l.tok.i shl 4) or (uintmax_t(t.l.c) - uintmax_t('A') + 10)
            elif t.l.c in {'L', 'l', 'U', 'u'}:
              while t.l.c in {'L', 'l', 'U', 'u'}:
                eat()
              return
            elif t.l.c == '.':
                readHexFloatLit(float(t.l.tok.i))
                return
            else:
              return
            eat()
        of 'b', 'B': # binary
          eat()
          if t.l.c != '0' and t.l.c != '1':
            parse_error("invalid binary literal: expect 0 or 1, got " & show(t.l.c))
            return
          while true:
            case t.l.c:
            of '0':
              t.l.tok.i = t.l.tok.i shl 1
            of '1':
              t.l.tok.i = (t.l.tok.i shl 1) or 1
            else:
              if t.l.c in {'0'..'9'}:
                warning("invalid decimal digit in binary literal")
              return
            eat()
        of '.': # float
          eat()
          readFloatLit()
          return
        else:
          return # zero
    else: # decimal
      while true:
        t.l.tok.i = (10 * t.l.tok.i) + (uintmax_t(t.l.c) - uintmax_t('0'))
        eat()
        if t.l.c == '.':
            eat()
            readFloatLit(float(t.l.tok.i))
            break
        if t.l.c in {'L', 'l', 'U', 'u'}:
            while t.l.c in {'L', 'l', 'U', 'u'}:
                readSuffix()
                eat()
            break
        elif t.l.c notin {'0'..'9'}:
            if t.l.c in {'a'..'z', 'A'..'Z'}: # User-defined literals?
              warning("invalid decimal suffix " & show(t.l.c))
              note("user-defined literals is a C++ feature")
            break

proc readStringLit(enc: uint8) =
    t.l.tok = TokenV(tok: TStringLit, tags: TVStr)
    while true:
        if t.l.c == '\\':
            t.l.tok.str.add(Rune(readEscape()).toUTF8)
        elif t.l.c == '"':
            eat()
            break
        else:
            if 0xF0 == (0xF8 and Codepoint(t.l.c)): # 4 byte
              t.l.tok.str.add(t.l.c)
              eat()
              t.l.tok.str.add(t.l.c)
              eat()
              t.l.tok.str.add(t.l.c)
              eat()
              t.l.tok.str.add(t.l.c)
            elif 0xE0 == (0xF0 and Codepoint(t.l.c)): # 3 byte
              t.l.tok.str.add(t.l.c)
              eat()
              t.l.tok.str.add(t.l.c)
              eat()
              t.l.tok.str.add(t.l.c)
            elif 0xC0 == (0xE0 and Codepoint(t.l.c)): # 2 byte
              t.l.tok.str.add(t.l.c)
              eat()
              t.l.tok.str.add(t.l.c)
            else: # 1 byte
              if t.l.c == '\n':
                warning("missing terminating '\"' character, read newline as \\n")
              elif t.l.c == '\0':
                parse_error("unexpected EOF")
                return
              t.l.tok.str.add(t.l.c)
            eat()
    t.l.tok.enc = enc

proc readPPNumberAfterDot() =
    t.l.tok.s.add('.')
    while t.l.c in {'0'..'9'}:
        t.l.tok.s.add(t.l.c)
        eat()

proc readPPNumber() =
    t.l.tok = TokenV(tok: TPPNumber, tags: TVSVal)
    if t.l.c == '0':
        eat()
        case t.l.c:
        of 'x', 'X':
            t.l.tok.s = "0x"
            while true:
                eat()
                if t.l.c notin {'0'..'9', 'a'..'f', 'A'..'F'}:
                    break
                t.l.tok.s.add(t.l.c)
        of 'B', 'b':
            t.l.tok.s = "0b"
            while true:
                eat()
                if t.l.c notin {'0', '1'}:
                    break
                t.l.tok.s.add(t.l.c)
        of '0' .. '7':
            t.l.tok.s = "0"
            t.l.tok.s.add(t.l.c)
            while true:
                eat()
                if t.l.c notin {'0'..'7'}:
                    break
                t.l.tok.s.add(t.l.c)
        of '.':
            eat()
            readPPNumberAfterDot()
        else:
            t.l.tok.s = "0"
            return
    else:
        while true:
            t.l.tok.s.add(t.l.c)
            eat()
            if t.l.c notin {'0'..'9'}:
                break
        if t.l.c == '.':
            eat()
            readPPNumberAfterDot()
    if t.l.c in {'e', 'p', 'E', 'P'}:
        t.l.tok.s.add(t.l.c)
        eat()
        if t.l.c == '+' or t.l.c == '-':
            t.l.tok.s.add(t.l.c)
            eat()
        while t.l.c in {'0'..'9'}:
            t.l.tok.s.add(t.l.c)
            eat()
    # nodigit
    elif t.l.c in {'a'..'z', 'A'..'Z', '_'}:
        while true:
            t.l.tok.s.add(t.l.c)
            eat()
            if t.l.c notin {'a'..'z', 'A'..'Z', '_'}:
                break
    # universal-character-name
    elif t.l.c == '\\':
        eat()
        let n = if t.l.c == 'U': 8 else: 4
        eat()
        let c = readUChar(n)
        t.l.tok.s.add(Rune(c).toUTF8)

proc skipLine() =
    while t.l.c != '\n' and t.l.c != '\0':
        eat()
    t.pp.flags = PFNormal

proc tokensEq(a, b: seq[TokenV]): bool =
  if a.len != b.len:
    return false
  for i in 0..<len(a):
    let x = a[i]
    let y = b[i]
    if x.tok != y.tok:
      return false
    case x.tok:
      of TStringLit, TPPNumber, TIdentifier:
        if x.s != y.s:
          return false
      else:
        continue
  return true

proc ppMacroEq(a, b: PPMacro): bool =
  result = (a.flags == b.flags) and
  (if a.flags == MFUNC: (a.ivarargs == b.ivarargs) else: true) and
  tokensEq(a.tokens, b.tokens)  

proc nextTok() =
    # Tokenize
    while true:
        if t.l.c in CSkip:
            while true:
                eat()
                if t.l.c notin CSkip:
                    break
            if t.pp.flags == PFPP and t.pp.want_expr == false:
                make_tok(TSpace)
                return
            continue
        if t.l.c == '#':
            if t.pp.flags == PFPP:
                eat()
                if t.l.c == '#':
                    make_tok(PPSharpSharp)
                    eat()
                else:
                    make_tok(PPSharp) 
                return
            eat()
            t.pp.flags = PFPP
            nextTok() # directive
            if t.l.tok.tok != CPPIdent:
                t.pp.flags = PFNormal
                parse_error("invalid preprocessing directive: expect an identifier")
                return
            case t.l.tok.s:
            of "define":
                nextTok() # name
                while t.l.tok.tok == TSpace:
                    nextTok()
                if t.l.tok.tok != CPPIdent:
                    t.pp.flags = PFNormal
                    parse_error("macro name should be a identifier, got " & showToken())
                    note("the syntax is:\n\t#define identifier replacement-list\n\t#define identifier(identifier-list) replacement-list")
                    return
                var name = t.l.tok.s
                nextTok()
                var m = if t.l.tok.tok == TLbracket: PPMacro(flags: MFUNC, ivarargs: false) else: PPMacro(flags: MOBJ)
                if m.flags == MFUNC:
                    while true:
                        nextTok()
                        if t.l.tok.tok == CPPIdent:
                            m.params.add(t.l.tok.s)
                            nextTok()
                            if t.l.tok.tok == TRbracket:
                                break
                            elif t.l.tok.tok == TComma:
                                continue
                            else:
                                parse_error("')' or ',' expected")
                                t.pp.flags = PFNormal
                                return
                        elif t.l.tok.tok == TEllipsis:
                            nextTok()
                            if t.l.tok.tok != TEllipsis:
                                parse_error("'.' expected")
                                t.pp.flags = PFNormal
                                return
                            nextTok()
                            if t.l.tok.tok != TEllipsis:
                                parse_error("'.' expected")
                                t.pp.flags = PFNormal
                                return
                            m.ivarargs = true
                            nextTok()
                            if t.l.tok.tok != TRbracket:
                                parse_error("')' expected")
                                t.pp.flags = PFNormal
                                return
                            break
                        elif t.l.tok.tok == CPPIdent and t.l.tok.s == "__VA_ARGS__":
                            m.ivarargs = true
                            nextTok()
                            if t.l.tok.tok != TRbracket:
                                parse_error("')' expected")
                                t.pp.flags = PFNormal
                                return
                            break
                        elif t.l.tok.tok == TRbracket:
                            break
                    nextTok()
                while t.l.tok.tok == TSpace:
                    nextTok()
                while true:
                    if t.pp.flags != PFPP:
                        break
                    m.tokens.add(t.l.tok)
                    nextTok()
                if m.tokens.len > 0:
                    while m.tokens[^1].tok == TSpace:
                        discard m.tokens.pop()
                var ok = true
                if len(m.tokens) >= 1:
                    if m.tokens[0].tok == PPSharpSharp:
                        parse_error("'##' cannot appear at start of macro expansion")
                        ok = false
                    if len(m.tokens) >= 2:
                        if m.tokens[^1].tok == PPSharpSharp:
                            parse_error("'##' cannot appear at end of macro expansion")
                            ok = false
                if ok:
                  let pr = t.pp.macros.getOrDefault(name, nil)
                  if pr != nil:
                    if not ppMacroEq(pr, m):
                      warning("macro " & name & " redefined")
                  t.pp.macros[name] = m
            of "if":
                nextTok() # if
                while t.l.tok.tok == TSpace:
                    nextTok()
                var ok: bool
                t.pp.want_expr = true
                let e = constant_expression()
                t.pp.want_expr = false
                if e == nil:
                    parse_error("expect constant_expression")
                    ok = false
                else:
                    ok = evali(e) != 0
                    if t.pp.eval_error:
                        write_eval_msg()
                t.pp.ppstack.add(if ok: 1 else: 0)
                t.pp.ok = ok
                skipLine()
            of "ifdef", "ifndef":
                let ndef = t.l.tok.s == "ifndef"
                nextTok()
                while t.l.tok.tok == TSpace:
                    nextTok()
                if t.l.tok.tok != CPPIdent:
                    t.pp.flags = PFNormal
                    parse_error("expect identifier")
                    note("the syntax is:\n\t#ifdef identifier\n\t#ifndef identifier")
                    return
                let name = t.l.tok.s # no copy
                let v = if ndef: not t.pp.macros.contains(name) else: t.pp.macros.contains(name)
                t.pp.ppstack.add(if v: 1 else: 0)
                t.pp.ok = v
                skipLine()
            of "else":
                if t.pp.ppstack.len == 0:
                    parse_error("no matching #if")
                    return
                if (t.pp.ppstack[^1] and 2) != 0:
                    parse_error("#else after #else")
                    return
                t.pp.ppstack[^1] = t.pp.ppstack[^1] or 2
                t.pp.ok = not t.pp.ok
                skipLine()
            of "elif":
                nextTok() # elif
                while t.l.tok.tok == TSpace:
                    nextTok()
                if t.pp.ppstack.len == 0:
                    parse_error("no matching #if")
                    return
                if (t.pp.ppstack[^1] and 2) != 0:
                    parse_error("#elif after #else")
                    return
                if t.pp.ok == false:
                    var ok: bool
                    t.pp.want_expr = true
                    let e = constant_expression()
                    t.pp.want_expr = false
                    if e == nil:
                        parse_error("expect constant_expression")
                        ok = false
                    else:
                        ok = evali(e) != 0
                    t.pp.ok = ok
                else:
                    t.pp.ok = false
                skipLine()
            of "endif":
                if t.pp.ppstack.len == 0:
                    parse_error("no matching #if")
                    skipLine()
                    return
                discard t.pp.ppstack.pop() # t.pp.ppstack.del(t.pp.ppstack.len - 1)
                if t.pp.ppstack.len == 0:
                    t.pp.ok = true
                else:
                    t.pp.ok = (t.pp.ppstack[^1] or 1) != 0
                skipLine()
            of "include":
                while t.l.c in CSkip:
                    eat()
                var path: string
                case t.l.c:
                of '"':
                    while true:
                        eat()
                        if t.l.c == '"':
                            eat()
                            break
                        if t.l.c == '\0' or t.l.c == '\n':
                            t.pp.flags = PFNormal
                            parse_error("unexpected EOF, expect path or '\"'")
                            return
                        path.add(t.l.c)
                    addFile(path)
                of '<':
                    while true:
                        eat()
                        if t.l.c == '>':
                            eat()
                            break
                        if t.l.c == '\0' or t.l.c == '\n':
                            t.pp.flags = PFNormal
                            parse_error("unexpected EOF, expect path or '>'")
                            return
                        path.add(t.l.c)
                    addFile(path)
                else:
                    parse_error("expect \"FILENAME\" or <FILENAME>")
                    note("the syntax is:\n\t#include <path>\n\t#include \"path\"")
                    return
                skipLine()
            of "line":
                while t.l.c in CSkip:
                    eat()
                if t.l.c notin {'0'..'9'}:
                    parse_error("expect digits (positive line number)")
                var s: string
                while true:
                    s.add(t.l.c)
                    eat()
                    if t.l.c in {'0'..'9'}:
                        continue
                    break
                var i = decimaltoInt(s)
                if i > high(uint32):
                    warning("overflow in line number")
                t.l.line = cast[uint32](i)
                while t.l.c in CSkip:
                    eat()
                if t.l.c == '\0' or t.l.c == '\n':
                    discard
                else:
                    if t.l.c != '"':
                        parse_error("expect \"FILENAME\"")
                        return
                    var f: string
                    while true:
                        eat()
                        if t.l.c == '\n' or t.l.c == '"' or t.l.c == '\\' or t.l.c == '\0':
                            break
                        f.add(t.l.c)
                    if t.l.c != '"':
                        parse_error("'\"' expected")
                        return
                    eat()
                    t.filename = f
                    skipLine()
            of "undef":
                nextTok()
                while t.l.tok.tok == TSpace:
                    nextTok()
                if t.l.tok.tok != CPPIdent:
                    t.pp.flags = PFNormal
                    parse_error("macro name should be a identifier")
                    note("the syntax is: #undef identifier")
                    return
                t.pp.macros.del(t.l.tok.s)
                skipLine()
            of "pragma":
                nextTok() # eat pragma
                var pragmas: seq[TokenV]
                while true:
                    if t.pp.flags != PFPP:
                        break
                    if t.l.tok.tok != TSpace:
                        pragmas.add(t.l.tok)
                    nextTok()
                # TODO: pragma
                skipLine()
            of "error", "warning":
                var s: string
                let iswarning = t.l.tok.s == "warning"
                while t.l.c == ' ':
                    eat()
                while true:
                    if t.l.c == '\n' or t.l.c == '\0':
                        break
                    s.add(t.l.c)
                    eat()
                if iswarning:
                    warning("#warning: " & s)
                else:
                    parse_error("#error: " & s)
                skipLine()
            else:
                parse_error("invalid directive: " & t.l.tok.s)
                skipLine()
            eat()
            continue
        if t.pp.flags == PFNormal and t.pp.ok == false:
            while t.l.c != '\n' and t.l.c != '\0':
                eat()
            if t.l.c == '\n':
                eat()
            elif t.l.c == '\0':
                t.pp.flags = PFNormal
                t.l.tok = TokenV(tok: TEOF, tags: TVNormal)
                return
            continue
        if t.l.c == '\n':
            if t.pp.flags == PFPP:
                t.pp.flags = PFNormal
                t.l.tok = TokenV(tok: TNewLine, tags: TVNormal)
                return
            eat()
            continue
        if t.l.c in {'0'..'9'}:
            if t.pp.flags == PFPP:
                readPPNumber()
            else:
                readNumberLit()
            return
        case t.l.c:
        of 'u':
            eat()
            if t.l.c == '"':
                eat()
                readStringLit(16)
            elif t.l.c == '\'':
                readCharLit(Ilong)
            elif t.l.c == '8':
                eat()
                if t.l.c != '"':
                    if t.l.c == '\'':
                        readCharLit()
                    else:
                        t.l.tok = TokenV(tok: CPPIdent, tags: TVSVal, s: "u8")
                        readIdentLit()
                else:
                    eat()
                    readStringLit(8)
            else:
                t.l.tok = TokenV(tok: CPPIdent, tags: TVSVal, s: "u")
                readIdentLit()
            return
        of 'U':
          eat()
          if t.l.c != '"':
            if t.l.c == '\'':
                readCharLit(Iulong)
            else:
                t.l.tok = TokenV(tok: CPPIdent, tags: TVSVal, s: "U")
                readIdentLit()
          else:
            eat()
            readStringLit(32)
          return
        of 'L':
            eat()
            if t.l.c != '"':
                if t.l.c == '\'':
                    readCharLit(Ilonglong)
                else:
                    t.l.tok = TokenV(tok: CPPIdent, tags: TVSVal, s: "L")
                    readIdentLit()
            else:
                eat()
                readStringLit(16)
            return
        else:
            discard

        if t.l.c in {'a'..'z', 'A'..'Z', '\x80' .. '\xFD', '_', '$', '\\'}:
            t.l.tok = TokenV(tok: CPPIdent, tags: TVSVal, s: "")
            readIdentLit()
            return
        case t.l.c:
        of '.':
            if t.pp.flags == PFNormal:
                eat() # first
                if t.l.c == '.':
                    eat() # second
                    if t.l.c != '.':
                        make_tok(TEllipsis2)
                        return
                    eat() # third
                    make_tok(TEllipsis)
                elif t.l.c in {'0'..'9'}:
                    readFloatLit()
                else:
                    make_tok(TDot)
            else:
                eat() # first '.'
                if t.l.c == '.':
                    make_tok(TDot)
                else:
                    t.l.tok = TokenV(tok: TPPNumber, tags: TVSVal)
                    readPPNumberAfterDot()
            return
        of '\0':
            if t.pp.flags == PFPP:
                t.pp.flags = PFNormal
            t.l.tok = TokenV(tok: TEOF, tags: TVNormal)
            return
        of '(', ')', '~', '?', '{', '}', ',', '[', ']', ';', '@':
            make_tok(cast[Token](t.l.c))
            eat()
            return
        of '"':
            eat()
            readStringLit(8)
            return
        of ':':
            eat()
            if t.l.c == '>':
                make_tok(TRSquareBrackets)
                eat()
            else:
                make_tok(TColon)
            return
        of '-':
            eat()
            if t.l.c == '-':
                make_tok(TSubSub)
                eat()
            elif t.l.c == '=':
                make_tok(TAsignSub)
                eat()
            elif t.l.c == '>':
                make_tok(TArrow)
                eat()
            else:
                make_tok(TDash)
            return
        of '+':
            eat()
            if t.l.c == '+':
                make_tok(TAddAdd)
                eat()
            elif t.l.c == '=':
                make_tok(TAsignAdd)
                eat()
            else:
                make_tok(TAdd)
            return
        of '\'':
            readCharLit()
            return
        of '>':
            eat()
            if t.l.c == '=':
                make_tok(TGe)
                eat()
            elif t.l.c == '>':
                make_tok(Tshr)
                eat()
            else:
                make_tok(TGt)
            return
        of '<':
            eat()
            case t.l.c:
            of '<':
                make_tok(Tshl)
                eat()
            of '=':
                make_tok(TLe)
                eat()
            of ':':
                make_tok(TLSquareBrackets)
                eat()
            of '%':
                make_tok(TLcurlyBracket)
                eat()
            else:
                make_tok(TLt)
            return
        of '%':
            eat()
            if t.l.c == '=':
                make_tok(TAsignRem)
                eat()
            elif t.l.c == '>':
                make_tok(TRcurlyBracket)
                eat()
            elif t.l.c == ':':
                make_tok(TBash)
                eat()
            else:
                make_tok(TPercent)
            return
        of '*':
            eat()
            if t.l.c == '=':
                make_tok(TAsignMul)
                eat()
            else:
                make_tok(TMul)
            return
        of '=':
            eat()
            if t.l.c == '=':
                make_tok(TEq)
                eat()
            else:
                make_tok(TAssign)
            return  
        of '&':
            eat()
            if t.l.c == '=':
                make_tok(TAsignBitAnd)
                eat()
            elif t.l.c == '&':
                make_tok(TLogicalAnd)
                eat()
            else:
                make_tok(TBitAnd)
            return
        of '|':
            eat()
            if t.l.c == '=':
                make_tok(TAsignBitOr)
                eat()
            elif t.l.c == '|':
                make_tok(TLogicalOr)
                eat()
            else:
                make_tok(TBitOr)
            return
        of '^':
            eat()
            if t.l.c == '=':
                make_tok(TAsignBitXor)
                eat()
            else:
                make_tok(TXor)
            return
        of '/':
            eat()
            if t.l.c == '=':
                make_tok(TAsignDiv)
                eat()
                return
            else:
                make_tok(TSlash)
                return
        of '!':
            eat()
            if t.l.c == '=':
                make_tok(TNe)
                eat()
            else:
                make_tok(TNot)
            return
        else:
            discard
        warning("stray " & show(t.l.c) & " in program")
        eat()

proc builtin_Pragma() =
  getToken()
  if t.l.tok.tok != TLbracket:
      expectLB()
      note("the syntax is:\n\t_Pragma(<string>)")
  else:
      getToken()
      if t.l.tok.tok != TStringLit:
          expect("expect string literal")
          note("the syntax is:\n\t_Pragma(<string>)")
      else:
          var pra = t.l.tok.s
          discard pra
          getToken()
          if t.l.tok.tok != TRbracket:
              expectRB()
          else:
              getToken()

proc getMacro(name: string): PPMacro =
  case name:
  of "__COUNTER__":
    result = PPMacro(tokens: @[TokenV(tok: TPPNumber, tags: TVSVal, s: $t.pp.counter)], flags: MOBJ)
    inc t.pp.counter
  of "__LINE__":
    result = PPMacro(tokens: @[TokenV(tok: TPPNumber, tags: TVSVal, s: $t.l.line)], flags: MOBJ)
  of "__FILE__":
    result = PPMacro(tokens: @[TokenV(tok: TStringLit, tags: TVSVal, s: t.filename)], flags: MOBJ)
  of "__DATE__":
    let n = now()
    result = PPMacro(tokens: @[TokenV(tok: TStringLit, tags: TVSVal, s: n.format(initTimeFormat("MMM dd yyyy")))], flags: MOBJ)
  of "__TIME__":
    let n = now()
    result = PPMacro(tokens: @[TokenV(tok: TStringLit, tags: TVSVal, s: n.format(initTimeFormat("hh:mm:ss")))], flags: MOBJ)
  of "_Pragma":
    result = PPMacro(flags: MBuiltin, fn: builtin_Pragma)
  else:
    result = t.pp.macros.getOrDefault(name, nil)

proc beginExpandMacro(a: string) =
  t.pp.expansion_list.incl a

proc endExpandMacro(a: string) =
  t.pp.expansion_list.excl a

proc isMacroInUse(a: string): bool =
  t.pp.expansion_list.contains(a)

proc checkMacro()

proc getToken() =
    ## CPP layer(Preprocessing)
    if len(t.tokenq) == 0:
        nextTok()
    else:
        t.l.tok = t.tokenq.pop()
    if t.l.tok.tok == CPPIdent:
        let o = t.pp.flags
        t.pp.flags = PFPP
        checkMacro()
        t.pp.flags = o
        if t.l.tok.tok == TNewLine:
            getToken()
    elif t.l.tok.tok == PPMacroPop:
        endExpandMacro(t.l.tok.s)
        getToken()

proc paste_token(a, b: TokenV): TokenV =
    var oldl = t.l
    t.l = Lexer(
            line: 1,
            col: 1,
            c: ' ',
            lastc: 256,
            tok: TokenV(tok: TNul, tags: TVNormal)
    )
    let s = stringizing(a) & stringizing(b)
    var oldlen = t.fstack.len
    t.fstack.add(newStringStream(s))
    nextTok()
    if t.l.tok.tok == TSpace:
        nextTok()
    if t.fstack.len != oldlen:
        discard # more then one tokens? invalid
    elif t.l.tok.tok == TNul:
        discard # parse failed!
    else:
        result = t.l.tok
    t.l = oldl
    if result.tok == TNul:
        parse_error('\'' & s & "' is an invalid preprocessing token")
        note("after join '##'")

proc concatenation(a: var seq[TokenV]): bool =
    if len(a) <= 2:
        return false
    var i = len(a)
    while true:
        dec i
        if a[i].tok == PPSharpSharp:
            break
        if i == 0:
            return false
    if i == 0 or (i+1) == len(a):
        return false
    var start = i - 1
    var last = i + 1
    let r = paste_token(a[last], a[start])
    if r.tok != TNul:
        # (start) (i) (last)
        a.del(start)
        # (i-1) (last-1)
        a.del(i-1)
        # (last-2)
        a[last - 2] = r
        return true
    return false

proc removeSpace(a: var seq[TokenV]) =
    if len(a) > 0:
        while a[^1].tok == TSpace:
            discard a.pop()
            if len(a) == 0:
                return
        while a[0].tok == TSpace:
            a.del(0)
            if len(a) == 0:
                return

proc macroCheck(m: PPMacro, args: var seq[seq[TokenV]], name: string): bool =
    for i in mitems(args):
        removeSpace(i)
    if len(args[0]) == 0:
        args.del(0)
    if m.ivarargs:
        if len(args) < len(m.params):
            parse_error("function-like macro " & name & " expect at least " & $len(m.params) & " arguments, but " & $len(args) & " provided")
            return false
    else:
        if len(args) != len(m.params):
            parse_error("function-like macro " & name & " expect " & $len(m.params) & " arguments, but got " & $len(args) & " provided")
            return false
    return true

proc checkMacro() =
    var name = t.l.tok.s
    let m = getMacro(name)
    if m != nil:
        case m.flags:
        of MOBJ:
            if len(m.tokens) > 0:
                beginExpandMacro(name)
                t.tokenq.add(TokenV(tok: PPMacroPop, tags: TVSVal, s: name))
                var cp = m.tokens                
                while concatenation(cp):
                    discard
                for i in countdown(len(cp)-1, 0):
                    let tok = cp[i]
                    if tok.tok != TSpace:
                        if tok.tok == CPPIdent:
                            if isMacroInUse(tok.s):
                                note("self-referential macro '" & tok.s & "' skipped")
                                tok.tok = TIdentifier
                        t.tokenq.add(tok)
            getToken()
        of MFUNC:
            var my = t.l.tok
            getToken()
            if t.l.tok.tok == TLbracket:
                getToken()
                var args: seq[seq[TokenV]]
                args.add(default(seq[TokenV]))
                while true:
                    if t.l.tok.tok == TEOF:
                        parse_error("unexpected EOF while parsing function-like macro arguments")
                        return
                    if t.l.tok.tok == TRbracket:
                        break
                    elif t.l.tok.tok == TComma:
                        args.add(default(seq[TokenV]))
                    elif t.l.tok.tok == TNewLine: # $6.10.3-10 new-line is considered a normal white-space character
                        args[^1].add(TokenV(tok: TSpace, tags: TVNormal))
                        t.pp.flags = PFPP
                        eat()
                    else:
                        args[^1].add(t.l.tok)
                    getToken()
                if macroCheck(m, args, name) == false:
                    return
                if len(m.tokens) > 0:
                    var cp: seq[TokenV]
                    for i in m.tokens:
                        if i.tok != TSpace:
                            cp.add(i)
                    var i = len(cp)
                    var s: seq[TokenV]
                    beginExpandMacro(name)
                    while true:
                        dec i
                        let t = cp[i]
                        case t.tok:
                        of TSpace:
                            discard
                        of CPPIdent:
                            let k = m.params.find(t.s)
                            if k != -1:
                                if i > 0 and cp[i-1].tok == PPSharp:
                                    s.add(TokenV(tok: TStringLit, tags: TVSVal, s: stringizing(args[k])))
                                    dec i
                                else:
                                    for j in args[k]:
                                        s.add(j)
                            else:
                                if isMacroInUse(t.s):
                                    note("self-referential macro '" & t.s & "' skipped")
                                    t.tok = TIdentifier
                                s.add(t)
                        else:
                            s.add(t)
                        if i == 0:
                            break
                    while concatenation(s):
                        discard
                    
                    t.tokenq.add(TokenV(tok: PPMacroPop, tags: TVSVal, s: name))
                    for i in s:
                        t.tokenq.add(i)
                getToken()
            else:
                if t.l.tok.tok != TNewLine:
                    putToken()
                t.l.tok = my
                t.l.tok.tok = TIdentifier
        of MBuiltin:
            m.fn()
    else:
        t.l.tok.tok = TIdentifier

var cli_idx = 0
let options = commandLineParams()

proc hasNext(): bool =
  return cli_idx < len(options)

proc get(): string =
  result = options[cli_idx]
  inc cli_idx

proc system(command: cstring): cint {.importc: "system", header: "stdio.h".}

proc runLD(input, path: string) =
  var cmd = "gcc libtrace/libtrace.o \"" & $input & "\" -o \"" & $path & '"'
  let status = system(cmd.cstring)
  if status != 0:
    myperror("gcc returned " & $status & " exit status")
    set_exit_code(1)

proc runLLD(input, output: string) =
  var cmd = "ld.lld --hash-style=gnu --no-add-needed  --build-id --eh-frame-hdr -dynamic-linker /lib/x86_64-linux-gnu/libc.so.6 /usr/lib64/ld-linux-x86-64.so.2 "
  cmd &= input
  cmd &= " -o "
  cmd &= output
  let status = system(cmd.cstring)
  if status != 0:
    error("ld.lld returned " & $status & " exit status")
    set_exit_code(1)

proc dumpVersionInfo() =
  var arr = [cstring("llvm"), cstring("--version")]
  parseCommandLineOptions(2, cast[cstringArray](addr arr[0]), nil)

proc showVersion() =
  raw_message_line "CC: C Compiler"
  raw_message_line "Homepage: https://github.com/ianfun/cc.git"
  raw_message_line "Bug report: https://github.com/ianfun/cc/issues"
  dumpVersionInfo()

proc help()

proc hash1(s: string): uint64 =
    # https://stackoverflow.com/questions/7666509/hash-function-for-string
    # https://stackoverflow.com/questions/2624192/good-hash-function-for-strings
    result = 5381
    for c in s:
        result = ((result shl 5) + result) + uint64(c)

proc hash2(s: string): uint64 =
    # https://stackoverflow.com/questions/7666509/hash-function-for-string
    # https://stackoverflow.com/questions/2624192/good-hash-function-for-strings
    result = 5381
    for i in 1..<len(s):
        result = ((result shl 5) + result) + uint64(s[i])

proc h(s: string): (StringRef, uint64) = (constStr(s), hash1(s))

proc initBackend() =
  # you must initTarget before you call this
  b.tsCtx = orcCreateNewThreadSafeContext()
  b.ctx = orcThreadSafeContextGetContext(b.tsCtx)
  contextSetDiscardValueNames(b.ctx, True)
  b.builder = createBuilderInContext(b.ctx)
  b.types[DIintPtr].ty = intPtrTypeInContext(b.ctx, b.layout)

  b.types[DIvoidty].ty = voidTypeInContext(b.ctx)
  b.types[DIptr].ty = pointerTypeInContext(b.ctx, 0)
  b.types[DIi1].ty = int1TypeInContext(b.ctx)
  b.types[DIi8].ty = int8TypeInContext(b.ctx)
  b.types[DIi16].ty = int16TypeInContext(b.ctx)
  b.types[DIi32].ty = int32TypeInContext(b.ctx)
  b.types[DIi64].ty = int64TypeInContext(b.ctx)
  b.types[DIffloat].ty = floatTypeInContext(b.ctx)
  b.types[DIfdouble].ty = doubleTypeInContext(b.ctx)

  b.i32_1 = constInt(b.types[DIi32].ty, 1)
  b.i32_0 = constInt(b.types[DIi32].ty, 0)
  b.i1_0 = constInt(b.types[DIi1].ty, 0)
  b.i1_1 = constInt(b.types[DIi1].ty, 1)

proc initTarget2() =
  discard nimLLVMConfigureTarget(nil, nil, nil, nil, nil, nil)

proc listTargets() =
  initTarget2()
  # app.strbuf.setLen 0
  app.strbuf.add("  Registered Targets:\n")
  var t = getFirstTarget()
  while t != nil:
    app.strbuf.add("    ")
    var n = getTargetName(t)
    var l = 15 - len(n)
    app.strbuf.add(n)
    while l > 0:
      app.strbuf.add(' ')
      dec l
    app.strbuf.add(" - ")
    app.strbuf.add(getTargetDescription(t))
    app.strbuf.add('\n')
    t = getNextTarget(t)
  flush()
  cc_exit(1)

type P = proc () {.nimcall.}

var cliOptions = [
  ("v".h, 0, cast[P](showVersion), "print version info"),
  ("g".h, 0, proc () = app.g = true, "create debug information in output"),
  ("libtrace".h, 0, proc () = app.libtrace = true, "Crate libtrace stack trace"),
  ("version".h, 0, cast[P](showVersion), "print version info"),
  ("help".h, 0, help, "help"),
  ("-help".h, 0, help, "help"),
  ("print-targets".h, 0, listTargets, "print registered targets"),
  ("stdin".h, 0, setStdin, "add stdin to files"),
  ("jit".h, 0, proc () = app.runJit = true, "run `main` function in LLVM JIT"),
  ("target".h, 1, cast[P](proc (s: string) = app.triple = s), "set target triple: e.g: x86_64-pc-linux-gnu, x86_64-pc-windows-msvc"),
  ("verbose".h, 0, proc () = app.verboseLevel = VVerbose, "enable verbose message"),
  ("note".h, 0, proc () = app.verboseLevel = VNote, "enable note message"),
  ("warning".h, 0, proc () = app.verboseLevel = VWarning, "enable warning message"),
  ("Wall".h, 0, proc () = app.verboseLevel = VWarning, "enable warning message"),
  ("?".h, 0, help, "help"),
  ("o".h, 1, cast[P](proc (s: string) = app.output = s), "set output file path"),
  ("emit-llvm".h, 0, cast[P](proc () = app.mode = OutputLLVMAssembly), "output LLVM Assembly"),
  ("emit-bitcode".h, 0, cast[P](proc () = app.mode = OutputBitcode), "output LLVM bitcode"),
  ("c".h, 0, cast[P](proc () = app.mode = OutputObjectFile), "output object file"),
  ("ld".h, 0, cast[P](proc () = app.linker = GCCLD), "use ld, The GNU linker"),
  ("lld".h, 0, cast[P](proc () = app.linker = LLD), "use LLD, The LLVM linker"),
  ("s".h, 0, cast[P](proc () = app.mode = OutputAssembly),  "output assembly"),
  ("fsyntax-only".h, 0, cast[P](proc () = app.mode = OutputCheck), "parse input file, type checking, emit warning and messages.Do not output a file"),
  ("O0".h, 0, proc () = app.optLevel = 0, "no optimization(default)"),
  ("O1".h, 0, proc () = app.optLevel = 1, "Somewhere between -O0 and -O2"),
  ("O2".h, 0, proc () = app.optLevel = 2, "enables most optimizations"),
  ("O3".h, 0, proc () = app.optLevel = 3, "enables optimizations that take longer to perform or that may generate larger code(for example, loop unrolling)"),
  ("O4".h, 0, proc () = app.optLevel = 3, " = O3"),
  ("Os".h, 0, proc () = app.sizeLevel = 1, "reduce code size"),
  ("Oz".h, 0, proc () = app.sizeLevel = 2, "reduce code size further"),
]

proc help() =
  # app.strbuf.setLen 0
  app.strbuf.add("command line options\nOption                         Description\n")
  for i in cliOptions:
    app.strbuf.add('-')
    app.strbuf.add(i[0][0].str)
    var l = 30 - len(i[0][0].str)
    while l > 0:
      app.strbuf.add(' ')
      dec l
    app.strbuf.add(i[3])
    app.strbuf.add('\n')
  app.strbuf.add('\n')
  flush()
  showVersion()

type FixName = object
    priority: int
    name: string

proc `<`(a, b: FixName): bool = a.priority < b.priority

proc fix(name: string) =
    var fixList = initHeapQueue[FixName]()
    for i in 0..<len(cliOptions):
        var v = $ cliOptions[i][0][0]
        var d = editDistance(name, v)
        fixList.push(FixName(priority: d, name: v))
    var msg: string
    while len(fixList) > 0:
        var f = fixList.pop()
        if f.priority < 3:
            msg.add("Perhaps you meant: '-" & f.name & "'\n")
    if msg.len > 1:
        fstderr << msg

proc parseCLI(): bool =
  var inputs = false
  var name: int
  while hasNext():
    let one = get()
    if one[0] == '-':
      var o = hash2(one)
      var has = false
      for i in cliOptions:
        if i[0][1] == o and i[0][0] == one[1..^1]:
          has = true
          if i[1] == 1:
            if hasNext() == false:
              error("command " & one & " expect one argument")
              cc_exit(1)
            var s = get()
            cast[proc (s: string){.nimcall.}](i[2])(s)
          else:
            i[2]()
      if has == false:
        error("unrecognized command line option '" & one & '\'')
        fix(one[1..^1])
    else:
      if inputs == false:
        inputs = true
        name = cli_idx
      addFile(one)
  if t.fstack.len == 0:
    error("no input files")
    return false
  if app.mode == OutputLink:
    if app.output.len == 0:
        when defined(windows):
            app.output = "a.exe"
        else:
            app.output = "a.out"
    else:
        when defined(windows):
            if '.' notin app.output:
                # if the output has no extension, add .exe
                app.output.add(".exe")
  else:
    if app.output.len == 0:
        cli_idx = name - 1
        app.output.add(get())
        case app.mode:
        of OutputLLVMAssembly:
            app.output.add(".ll")
        of OutputBitcode:
            app.output.add(".bc")
        of OutputObjectFile:
            app.output.add(".o")
        of OutputAssembly:
            app.output.add(".s")
        else:
            discard
  return true

const
 FNone = 0'u32
 FMinGW = 1'u32
 F32Bit = 2'u32
 F64Bit = 4'u32

proc llvm_error(msg: string) =
  raw_message_line "LLVM ERROR: " & msg

proc llvm_error(msg: cstring) =
  if msg != nil:
    raw_message_line("LLVM ERROR: " & $msg)

proc wrap(ty: CType): Type

proc wrap2(ty: CType): Type

proc wrap3(ty: CType): DIType

proc getLexScope(): DIScope =
    b.lexBlocks[^1]

var file: MetadataRef

proc getFile(): DIScope =
    if file == nil:
      file = dIBuilderCreateFile(b.di, "foobar", 6, app.dir.cstring, app.dir.len.csize_t)
    return file
    # b.lexBlocks[0]

proc createDebugFuctionType(ty: CType): DIType =
    let l = len(ty.params)
    var buf = create(DIType, l + 1)
    var arr = cast[ptr UncheckedArray[DIType]](buf)
    var ivarargs = false
    var i = 1.cuint
    arr[0] = wrap3(ty.ret)
    var m = len(ty.params).cuint
    while true:
      if i == m:
        break
      if ty.params[i-1][1] == nil:
        ivarargs = true
        break
      arr[i] = wrap3(ty.params[i-1][1])
      inc i
    dIBuilderCreateSubroutineType(b.di, getFile(), buf, i, DIFlagZero)

proc llvmGetAlignof(ty: CType): culonglong =
  aBIAlignmentOfType(b.layout, wrap2(ty))

proc llvmGetsizeof(ty: CType): culonglong =
  storeSizeOfType(b.layout, wrap2(ty))

proc gen_str_ptr(val: string, is_constant: bool): Value

proc getsizeof(ty: CType): culonglong =
    if bool(ty.tags and TYVOID):
        return 1
    if ty.spec == TYINCOMPLETE:
        error_incomplete(ty)
        return 0
    elif ty.spec == TYFUNCTION:
        return 1
    if ty.spec == TYENUM:
        return sizeofint
    if ty.spec == TYPOINTER:
        return app.pointersize
    return llvmGetSizeof(ty)

proc getsizeof(e: Expr): culonglong =
    getsizeof(e.ty)

proc getAlignof(ty: CType): culonglong =
    llvmGetAlignOf(ty)

proc getAlignof(e: Expr): culonglong =
    getAlignof(e.ty)

proc emitDebugLocation() =
    setCurrentDebugLocation2(b.builder, nil)

proc wrap(loc: Location): DIScope =
    dIBuilderCreateDebugLocation(b.ctx, loc.line.cuint, loc.col.cuint, getLexScope(), nil)

proc emitDebugLocation(e: Expr | Stmt) =
    var loc = wrap(e.loc)
    setCurrentDebugLocation2(b.builder, loc)

proc addlibtraceSupport() =
  var arr = [
    b.types[DIi32].ty, # line
    b.types[DIptr].ty, # func
    b.types[DIptr].ty, # file
    b.types[DIptr].ty  # next
  ]
  b.lt_callframe = structCreateNamed(b.ctx, "CallFrame")
  structSetBody(b.lt_callframe, addr arr[0], len(arr).cuint, False)
  b.lt_now = addGlobal(b.module, b.types[DIptr].ty, "__libtrace_now")
  setLinkage(b.lt_now, ExternalLinkage)
  var ty = functionType(b.types[DIvoidty].ty, nil, 0, False)
  b.lt_call_ty = ty
  b.lt_begin_call = addFunction(b.module, "__libtrace_begin_call", ty)
  b.lt_end_call = addFunction(b.module, "__libtrace_end_call", ty)

proc load(p: Value, t: Type, align: uint32 = 0): Value =
  assert p != nil
  assert t != nil
  result = buildLoad2(b.builder, t, p, "")
  if align != 0:
    setAlignment(result, align.cuint)

proc store(p: Value, v: Value, align: uint32 = 0) =
  var s = buildStore(b.builder, v, p)
  if align != 0:
    setAlignment(s, align.cuint)

proc addLLVMModule(source_file: string) =
  b.module = moduleCreateWithNameInContext(source_file.cstring, b.ctx)
  setSourceFileName(b.module, cstring(source_file), source_file.len.csize_t)
  setModuleDataLayout(b.module, b.layout)
  setTarget(b.module, app.triple.cstring)

  var ident = mDStringInContext(b.ctx, cstring(cc_version_full), len(cc_version_full))
  addNamedMetadataOperand(b.module, "llvm.ident", ident)

  const wchars: cstring = "short_wchar"
  var short_wchars = [b.i32_1, mDStringInContext(b.ctx, wchars, len(wchars)), b.i32_1]
  var short_wchar = mDNodeInContext(b.ctx, addr short_wchars[0], 3)
  addNamedMetadataOperand(b.module, "llvm.module.flags", short_wchar)

  const enums: cstring = "short_enum"
  var short_enums = [b.i32_1, mDStringInContext(b.ctx, enums, len(enums)), b.i32_1]
  var short_enum = mDNodeInContext(b.ctx, addr short_enums[0], 3)
  addNamedMetadataOperand(b.module, "llvm.module.flags", short_enum)

  const wcharsizes: cstring = "wchar_size"
  var wcharsize_arr = [b.i32_1, mDStringInContext(b.ctx, wcharsizes, len(wcharsizes)), constInt(b.types[DIi32].ty, 4)]
  var wcharsizenode = mDNodeInContext(b.ctx, addr wcharsize_arr[0], 3)
  addNamedMetadataOperand(b.module, "llvm.module.flags", wcharsizenode)

  if app.libtrace:
    addlibtraceSupport()
  if app.g:
    b.di = createDIBuilder(b.module)
    var file = dIBuilderCreateFile(b.di, source_file.cstring, source_file.len.csize_t, app.dir.cstring, app.dir.len.csize_t)
    b.lexBlocks.add(dIBuilderCreateCompileUnit(
        b.di,
        DWARFSourceLanguageC99,
        file,
        cstring(cc_version_full),
        cc_version_full.len.csize_t,
        isOptimized=False,
        flags="",
        flagsLen=0,
        runtimeVer=0,
        splitName=nil,
        splitNameLen=0,
        kind=DWARFEmissionFull,
        dWOId=0,
        splitDebugInlining=False,
        debugInfoForProfiling=False,
        sysRoot=nil,
        sysRootLen=0,
        sdk=nil,
        sDKLen=0
    ))
    proc dbgType(v: BasicTypeIndex, name: string, encoding: cuint) =
        b.types[v].di = dIBuilderCreateBasicType(b.di, name, len(name).csize_t, sizeOfTypeInBits(b.layout, b.types[v].ty), encoding, DIFlagZero)

    proc dbgType2(name: string, encoding: cuint, sizeBits: uint64): DIType =
        dIBuilderCreateBasicType(b.di, name, len(name).csize_t, sizeBits, encoding, DIFlagZero)

    b.types[DIptr].di = dIBuilderCreatePointerType(b.di, nil, sizeOfTypeInBits(b.layout, b.types[DIintPtr].ty), 0, 0, "void*", 5)
    # void is always nil in DIType!
    dbgType(DIi1, "_Bool", DW_ATE_boolean)
    dbgType(DIi8, "unsiged char", DW_ATE_unsigned)
    dbgType(DIi16, "unsigned short", DW_ATE_unsigned)
    dbgType(DIi32, "unsigned", DW_ATE_unsigned)
    dbgType(DIi64, "unsigned long long", DW_ATE_unsigned)
    dbgType(DIffloat, "float", DW_ATE_decimal_float)
    dbgType(DIfdouble, "double", DW_ATE_decimal_float)
    b.di8 = dbgType2("char", DW_ATE_signed_char, sizeOfTypeInBits(b.layout, b.types[DIi8].ty))
    b.di16 = dbgType2("short", DW_ATE_signed, sizeOfTypeInBits(b.layout, b.types[DIi16].ty))
    b.di32 = dbgType2("int", DW_ATE_signed, sizeOfTypeInBits(b.layout, b.types[DIi32].ty))
    b.di64 = dbgType2("long long", DW_ATE_signed, sizeOfTypeInBits(b.layout, b.types[DIi64].ty))
    const dwarf_version = "Dwarf Version"
    addModuleFlag(b.module, 6, dwarf_version, dwarf_version.len.csize_t, valueAsMetadata(constInt(b.types[DIi32].ty, DWARF_VERSION)))
    const debug_info = "Debug Info Version"
    addModuleFlag(b.module, 1, debug_info, debug_info.len.csize_t, valueAsMetadata(constInt(b.types[DIi32].ty, debugMetadataVersion())))

proc setInsertPoint(loc: Label) {.inline.} =
  positionBuilderAtEnd(b.builder, loc)

proc addBlock(): Label {.inline.} =
  appendBasicBlockInContext(b.ctx, b.currentfunction, "")

proc addBlock(name: cstring): Label {.inline.} =
  appendBasicBlockInContext(b.ctx, b.currentfunction, name)

proc condBr(cond: Value, iftrue, iffalse: Label) =
  discard buildCondBr(b.builder, cond, iftrue, iffalse)

proc br(loc: Label) {.inline.} =
  discard buildBr(b.builder, loc)

proc getTerminator(): Value =
    getBasicBlockTerminator(getInsertBlock(b.builder))

proc gep(ty: Type, p: Value, indices: var Value): Value {.inline.} =
  buildInBoundsGEP2(b.builder, ty, p, addr indices, 1, "")

proc gep(ty: Type, p: Value, indices: ptr Value, num: cuint): Value {.inline.} =
  buildInBoundsGEP2(b.builder, ty, p, indices, num, "")

proc gep(ty: Type, p: Value, indices: var openarray[Value]): Value {.inline.} =
  gep(ty, p, addr indices[0], indices.len.cuint)

proc buildlibtraceBeginCall(funcstr: string, line: uint32, file: string): ValueRef =
  discard buildCall2(b.builder, b.lt_call_ty, b.lt_begin_call, nil, 0, "")
  var frame = buildAlloca(b.builder, b.lt_callframe, "")
  var vals = [constInt(b.types[DIi32].ty, line.culonglong), gen_str_ptr(funcstr, true), gen_str_ptr(file, true), constNull(b.types[DIptr].ty)]
  store(frame, constNamedStruct(b.lt_callframe, addr vals[0], vals.len.cuint))
  var old = buildAlloca(b.builder, b.types[DIptr].ty, "")
  var o = load(b.lt_now, b.types[DIptr].ty)
  store(old, o)
  var p = buildStructGEP2(b.builder, b.lt_callframe, o, 3, "")
  store(p, frame)
  store(b.lt_now, frame)
  return old

proc buildlibtraceAfterCall(old: ValueRef) =
  store(b.lt_now, load(old, b.types[DIptr].ty))
  var p = buildStructGEP2(b.builder, b.lt_callframe, load(b.lt_now, b.types[DIptr].ty), 3, "")
  store(p, constNull(b.types[DIptr].ty))
  discard buildCall2(b.builder, b.lt_call_ty, b.lt_end_call, nil, 0, "")

proc initTarget(all = false): bool =
  b.f = FNone
  if app.triple.len == 0:
    app.triple = $getDefaultTargetTriple()
    b.f = 1
  var err = nimLLVMConfigureTarget(app.triple.cstring, addr b.target, addr b.machine, addr b.layout, addr b.triple, addr b.f)
  if err != nil or b.triple == nil:
    llvm_error(err)
    return false
  app.pointersize = pointerSize(b.layout)
  b.arch = nimGetArch(b.triple)
  b.archName = nimGetArchName(b.triple)
  b.os = nimGetOS(b.triple)
  b.env = nimGetEnv(b.triple)
  return true

proc handle_asm(s: string) =
  if b.currentfunction == nil:
    # module level asm
    appendModuleInlineAsm(b.module, cstring(s), len(s).csize_t)
  else:
    var asmtype = functionType(b.types[DIvoidty].ty, nil, 0, False)
    var f = getInlineAsm(asmtype, cstring(s), len(s).csize_t, nil, 0, True, False, InlineAsmDialectATT, False)
    discard buildCall2(b.builder, asmtype, f, nil, 0, "")

proc enterScope() =
  b.tags.add(initTable[string, Type]())
  b.vars.add(initTable[string, Value]())
  if app.g:
      b.dtags.add(initTable[string, DIType]())

proc leaveScope() =
  discard b.tags.pop()
  discard b.vars.pop()
  if app.g:
    discard b.dtags.pop()

proc getVar(name: string): Value =
  for i in countdown(len(b.vars)-1, 0):
    result = b.vars[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc putVar(name: string, val: Value) =
  b.vars[^1][name] = val

proc getDIEnum(name: string): DIType =
  for i in countdown(len(b.tags)-1, 0):
    result = b.dtags[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc getTags(name: string): Type =
  for i in countdown(len(b.tags)-1, 0):
    result = b.tags[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc putTags(name: string, t: Type) =
  b.tags[^1][name] = t

proc putDIEnum(name: string, t: DIType) =
    b.dtags[^1][name] = t

proc optimize() =
  var passM = createPassManager()
  var pb = passManagerBuilderCreate()
  passManagerBuilderSetSizeLevel(pb, app.sizeLevel)
  passManagerBuilderSetOptLevel(pb, app.optLevel)
  if app.inlineThreshold != 0:
    passManagerBuilderUseInlinerWithThreshold(pb, app.inlineThreshold)
  
  passManagerBuilderPopulateModulePassManager(pb, passM)
  discard runPassManager(passM, b.module)

  passManagerBuilderDispose(pb)
  disposePassManager(passM)

proc writeModuleToFile(path: string) =
  var err: cstring
  if printModuleToFile(b.module, path, cast[cstringArray](addr err)) == True:
    llvm_error(err)

proc writeBitcodeToFile(path: string) =
  if writeBitcodeToFile(b.module, path) != 0:
    llvm_error("LLVMWriteBitcodeToFile")

proc writeObjectFile(path: string) =
  var err: cstring
  if targetMachineEmitToFile(b.machine, b.module, path, ObjectFile, cast[cstringArray](addr err)) == True:
    llvm_error(err)

proc writeAssemblyFile(path: string) =
  var err: cstring
  if targetMachineEmitToFile(b.machine, b.module, path, AssemblyFile, cast[cstringArray](addr err)) == True:
    llvm_error(err)

proc gen(e: Expr): Value

proc gen(s: Stmt)

proc gen_cond(a: Expr): Value =
  ## generate bool(conditional) expression
  if bool(a.ty.tags and TYBOOL):
    gen(a)
  else:
    buildIsNotNull(b.builder, gen(a), "")

proc gen_int(i: culonglong, tags: uint32): Value =
    return constInt(b.types[getBasicTypeIndex(tags)].ty, i)

proc gen_float(f: float, tag: uint32): Value =
  constReal(if (tag and TYFLOAT) != 0: b.types[DIffloat].ty else: b.types[DIfdouble].ty, f)

proc gen_str_ptr(val: string, is_constant: bool): Value =
  var gstr = constStringInContext(b.ctx, cstring(val), len(val).cuint, False)
  var ty = typeOfX(gstr)
  result = addGlobal(b.module, ty, ".str")
  setLinkage(result, PrivateLinkage)
  setInitializer(result, gstr)
  setAlignment(result, 1)
  setUnnamedAddr(result, 1)
  if is_constant:
    setGlobalConstant(result, True)

proc wrap2(ty: CType): Type =
  case ty.spec:
  of TYPRIM:
    return b.types[getBasicTypeIndex(ty.tags)].ty
  of TYPOINTER:
    return b.types[DIptr].ty
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
    return b.types[DIi32].ty
  else:
    unreachable()

proc createEnum(ty: CType): DIType =
    var buf = create(DIType, ty.enumerators.len or 1)
    var arr = cast[ptr UncheckedArray[DIType]](buf)
    for i in 0 ..< ty.enumerators.len:
        let (name, val) = ty.enumerators[i]
        arr[i] = dIBuilderCreateEnumerator(
            b.di, 
            cstring(name), name.len.csize_t, 
            val.int64, isUnsigned=False
        )
    result = dIBuilderCreateEnumerationType(
        b.di, 
        getLexScope(), 
        cstring(ty.ename), ty.ename.len.csize_t, 
        getFile(), lineNumber=0,
        sizeInBits=sizeOfTypeInBits(b.layout, b.types[DIi32].ty), alignInBits=0, 
        elements=buf, numElements=ty.enumerators.len.cuint, 
        classTy=b.di32)
    dealloc(buf)

proc wrap3Noqualified(ty: CType): DIType =
  case ty.spec:
  of TYPRIM:
    if bool(ty.tags and TYINT8):
        return b.di8
    if bool(ty.tags and TYINT16):
        return b.di16
    if bool(ty.tags and TYINT32):
        return b.di32
    if bool(ty.tags and TYINT64):
        return b.di64
    return b.types[getBasicTypeIndex(ty.tags)].di
  of TYPOINTER:
    if bool(ty.tags and TYVOID):
        return b.types[DIptr].di
    var p = wrap3(ty.p)
    var s = $ty
    return dIBuilderCreatePointerType(b.di, p, sizeOfTypeInBits(b.layout, b.types[DIptr].ty), 0, 0, cstring(s), s.len.csize_t)
  of TYSTRUCT, TYUNION:
    if len(ty.sname) > 0:
        result = getDIEnum(ty.sname)
        if result != nil:
            return result
    var t = wrap2(ty)
    let l = len(ty.selems)
    var buf = create(DIType, l or 1)
    var arr = cast[ptr UncheckedArray[DIType]](buf)
    for i in 0 ..< l:
        var m = wrap3(ty.selems[i][1])
        arr[i] = dIBuilderCreateMemberType(
            b.di, getLexScope(), 
            cstring(ty.selems[i][0]), ty.selems[i][0].len.csize_t,
            getFile(), lineNo=0, sizeInBits=getsizeof(ty.selems[i][1]),
            alignInBits=0, offsetInBits=offsetOfElement(b.layout, t, i.cuint) * 8,
            flags=DIFlagZero, ty=m
        )
    if ty.spec == TYSTRUCT: 
        result = dIBuilderCreateStructType(
            b.di, getLexScope(), 
            cstring(ty.sname), ty.sname.len.csize_t,
            getFile(), lineNumber=0,
            sizeInBits=sizeOfTypeInBits(b.layout, t), 
            alignInBits=0, 
            flags=DIFlagZero,
            derivedFrom=nil,
            elements=buf,
            numElements=l.cuint,
            runTimeLang=0,
            vTableHolder=nil,
            uniqueId=nil,
            uniqueIdLen=0
        )
    else:
        result = dIBuilderCreateUnionType(
            b.di, getLexScope(),
            cstring(ty.sname), ty.sname.len.csize_t, 
            getFile(), lineNumber=0,
            sizeInBits=sizeOfTypeInBits(b.layout, t),
            alignInBits=0, 
            flags=DIFlagZero,
            elements=buf,
            numElements=l.cuint,
            runTimeLang=0,
            uniqueId=nil,
            uniqueIdLen=0
        )
    dealloc(buf)
    if ty.sname.len > 0:
        putDIEnum(ty.sname, result)
  of TYARRAY:
    var size = getsizeof(ty)
    var subscripts: MetadataRef = nil
    if ty.hassize:
        subscripts = dIBuilderGetOrCreateSubrange(b.di, 0, size.int64)
    else:
        subscripts = nimdIBuilderGetOrCreateSubrange(b.di, nil)
    return dIBuilderCreateArrayType(b.di, size, getAlignof(ty).uint32, wrap3(ty.arrtype), addr subscripts, 1)
  of TYFUNCTION:
    return createDebugFuctionType(ty)
  of TYENUM:
    if ty.ename.len != 0:
      result = getDIEnum(ty.ename)
      if result != nil:
        return result
    result = createEnum(ty)
    if ty.ename.len != 0:
        putDIEnum(ty.ename, result)
    return result
  else:
    return nil

proc wrap3(ty: CType): DIType = 
    result = wrap3Noqualified(ty)
    if result != nil:
        if bool(ty.tags and TYCONST):
            result = dIBuilderCreateQualifiedType(b.di, DW_TAG_const_type, result)
        if bool(ty.tags and TYVOLATILE):
            result = dIBuilderCreateQualifiedType(b.di, DW_TAG_volatile_type, result)
        if bool(ty.tags and TYRESTRICT):
            result = dIBuilderCreateQualifiedType(b.di, DW_TAG_restrict_type, result)


proc wrap(ty: CType): Type =
  ## wrap a CType to LLVM Type
  case ty.spec:
  of TYPRIM:
    return b.types[getBasicTypeIndex(ty.tags)].ty
  of TYPOINTER:
    return b.types[DIptr].ty
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
        var name = if ty.spec == TYUNION: "union." else: "struct."
        name.add(ty.sname)
        result = structCreateNamed(b.ctx, cstring(name))
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
      arr[i] = wrap(ty.params[i][1])
      inc i
    var ret: Type
    if bool(ty.ret.tags and TYVOID):
      ret = b.types[DIvoidty].ty
    else:
      ret = wrap(ty.ret)
    result = functionType(ret, buf, i, if ivarargs: True else: False)
    dealloc(buf)
    return result
  of TYARRAY:
    return arrayType(wrap(ty.arrtype), cuint(ty.arrsize))
  of TYENUM:
    if ty.ename.len != 0:
      result = getTags(ty.ename)
      if result != nil:
        return result
    result = b.types[DIi32].ty
    if ty.ename.len != 0:
        putTags(ty.ename, result)
    return result
  of TYINCOMPLETE:
    case ty.tag:
    of TYSTRUCT:
      result = getTags(ty.name)
      if result != nil:
        return result
      return structCreateNamed(b.ctx, cstring(ty.name))
    of TYUNION:
      result = getTags(ty.sname)
      if result != nil:
        return result
      return structCreateNamed(b.ctx, cstring(ty.sname))
    of TYENUM:
      return b.types[DIi32].ty
    else:
      unreachable()
  of TYBITFIELD:
    unreachable()
    return nil

proc gen_condition(test: Expr, lhs: Expr, rhs: Expr): Value =
  ## build a `cond ? lhs : rhs` expression
  var ty = wrap(lhs.ty)
  var iftrue = addBlock()
  var iffalse = addBlock()
  var ifend = addBlock()
  condBr(gen_cond(test), iftrue, iffalse)

  setInsertPoint(iftrue)
  var left = gen(lhs)
  br ifend
  
  setInsertPoint(iffalse)
  var right =gen(rhs)
  br ifend

  setInsertPoint(ifend)
  var phi = buildPhi(b.builder, ty, "")

  var blocks = [iftrue, iffalse]
  var values = [left, right]
  addIncoming(phi, addr values[0], addr blocks[0], 2)
  return phi

proc gen_logical(lhs: Expr, rhs: Expr, isand = true): Value =
  var cond2 = addBlock()
  var phib = addBlock()
  var a = buildAlloca(b.builder, b.types[DIi1].ty, "")
  var left = gen_cond(lhs)
  store(a, left)
  if isand:
    condBr(left, cond2, phib)
  else:
    condBr(left, phib, cond2)

  setInsertPoint(cond2)
  var right = gen_cond(rhs)
  store(a, right)
  br phib

  setInsertPoint(phib)

  return load(a, b.types[DIi1].ty)

proc gen_if(test: Expr, body: Stmt) =
  var iftrue = addBlock()
  var ifend = addBlock()
  condBr(gen_cond(test), iftrue, ifend)

  setInsertPoint(iftrue)
  gen(body)
  if getTerminator() == nil:
    br ifend

  setInsertPoint(ifend)

proc gen_if(test: Expr, body: Stmt, elsebody: Stmt) =
  var iftrue = addBlock()
  var iffalse = addBlock()
  var ifend = addBlock()
  condBr(gen_cond(test), iftrue, iffalse)

  setInsertPoint(iftrue)
  gen(body)
  if getTerminator() == nil:
      br ifend

  setInsertPoint(iffalse)
  gen(elsebody)
  if getTerminator() == nil:
    br ifend

  setInsertPoint(ifend)

proc gen_switch(test: Expr, body: Stmt) =
  var old_switch = b.topSwitch
  var old_break = b.topBreak
  var old_test = b.topTest
  var old_case = b.topCase
  var old_default = b.topdefaultCase

  b.topTest = gen(test)
  b.topBreak = addBlock()
  b.topSwitch = getInsertBlock(b.builder)
  b.topCase = nil
  b.topdefaultCase = addBlock()
  gen(body)
  if b.topCase != nil:
    br b.topBreak
  setInsertPoint(b.topSwitch)
  br b.topdefaultCase
  setInsertPoint(b.topBreak)

  b.topdefaultCase = old_default
  b.topCase = old_case
  b.topTest = old_test
  b.topBreak = old_break
  b.topSwitch = old_switch

proc gen_case(test: Expr, body: Stmt) =
  var thiscase = addBlock()
  if b.topCase != nil:
    br thiscase
  b.topCase = thiscase
  setInsertPoint(b.topSwitch)
  let lhs = gen(test)
  let rhs = b.topTest
  let cond = buildICmp(b.builder, IntEQ, lhs, rhs, "")
  b.topSwitch = addBlock()
  condBr(cond, thiscase, b.topSwitch)
  setInsertPoint(thiscase)
  gen(body)

proc gen_default(body: Stmt) =
  if b.topCase != nil:
    br b.topdefaultCase
  b.topCase = b.topdefaultCase
  setInsertPoint(b.topdefaultCase)
  gen(body)

proc gen_while(test: Expr, body: Stmt) =
  var old_break = b.topBreak
  var old_continue = b.topContinue

  var whilecmp = addBlock()
  var whilebody = addBlock()
  var whileleave = addBlock()

  b.topBreak = whileleave
  b.topContinue = whilecmp

  if getTerminator() == nil:
      br whilecmp

  setInsertPoint(whilecmp)
  var cond = gen_cond(test)
  condBr(cond, whilebody, whileleave)

  setInsertPoint(whilebody)
  gen(body)

  if getTerminator() == nil:
      br whilecmp

  setInsertPoint(whileleave)

  b.topBreak = old_break
  b.topContinue = old_continue

proc gen_for(test: Expr, body: Stmt, sforinit: Stmt, eforincl: Expr) =
    var old_break = b.topBreak
    var old_continue = b.topContinue
  
    if sforinit != nil:
        gen(sforinit)
    enterScope()
    var forcmp = addBlock()
    var forbody = addBlock()
    var forleave = addBlock()
    var forincl = addBlock()
  
    b.topBreak = forleave
    b.topContinue = forincl
  
    if getTerminator() == nil:
        br forcmp
  
    # for.cmp
    setInsertPoint(forcmp)
    if test != nil:
        var cond = gen_cond(test)
        condBr(cond, forbody, forleave)
    else:
        br forbody

    # for.body
    setInsertPoint(forbody)
    gen(body)
    br forincl

    # for.incl
    setInsertPoint(forincl)
    if eforincl != nil:
        b.expr_stmt = true
        discard gen(eforincl)
  
    if getTerminator() == nil:
        br forcmp
    setInsertPoint(forleave)
    leaveScope()
  
    b.topBreak = old_break
    b.topContinue = old_continue

proc gen_dowhile(test: Expr, body: Stmt) =
    var old_break = b.topBreak
    var old_continue = b.topContinue
  
    var dowhilecmp = addBlock()
    var dowhilebody = addBlock()
    var dowhileleave = addBlock()
  
    b.topBreak = dowhileleave
    b.topContinue = dowhilecmp
  
    setInsertPoint(dowhilebody)
    gen(body)
  
    setInsertPoint(dowhilecmp)
    var cond = gen_cond(test)
    if getTerminator() == nil:
        condBr(cond, dowhilebody, dowhileleave)
  
    setInsertPoint(dowhileleave)
    b.topBreak = old_break
    b.topContinue = old_continue

# the compiler may generate a table(array), so require `O(1)` time indexing
proc getOp(a: BinOP): llvm.Opcode =
  case a:
  of BinOp.UAdd: llvm.LLVMAdd
  of BinOp.FAdd: llvm.LLVMFAdd
  of BinOp.USub: llvm.LLVMSub
  of BinOp.FSub: llvm.LLVMFSub
  of BinOp.UMul: llvm.LLVMMul
  of BinOp.FMul: llvm.LLVMFMul
  of BinOp.UDiv: llvm.LLVMUDiv
  of BinOp.SDiv: llvm.LLVMSDiv
  of BinOp.FDiv: llvm.LLVMFDiv
  of BinOp.URem: llvm.LLVMURem
  of BinOp.SRem: llvm.LLVMSRem
  of BinOp.FRem: llvm.LLVMFRem
  of BinOp.Shr: llvm.LLVMLShr
  of BinOp.AShr: llvm.LLVMAShr
  of BinOp.Shl: llvm.LLVMShl
  of BinOp.And: llvm.LLVMAnd
  of BinOp.Xor: llvm.LLVMXor
  of BinOp.Or: llvm.LLVMOr
  else: assert(false);cast[llvm.Opcode](0)

proc getICmpOp(a: BinOP): llvm.IntPredicate =
  case a:
  of BinOP.EQ: llvm.IntEQ
  of BinOP.NE: llvm.IntNE
  of BinOP.UGE: llvm.IntUGE
  of BinOP.UGT: llvm.IntUGT
  of BinOP.ULE: llvm.IntULE
  of BinOP.ULT: llvm.IntULT
  of BinOP.SGE: llvm.IntSGE
  of BinOP.SGT: llvm.IntSGT
  of BinOP.SLT: llvm.IntSLT
  of BinOP.SLE: llvm.IntSLE 
  else: unreachable();cast[llvm.IntPredicate](0)

proc getFCmpOp(a: BinOP): RealPredicate =
  case a:
  of BinOP.FEQ: RealOEQ
  of BinOP.FNE: RealONE
  of BinOP.FGT: RealOGT
  of BinOP.FGE: RealOGE
  of BinOP.FLT: RealOLT
  of BinOP.FLE: RealOLE
  else: unreachable();cast[RealPredicate](0)

proc getCastOp(a: CastOp): llvm.Opcode =
  case a:
  of CastOp.Trunc:
    llvm.LLVMTrunc
  of CastOp.ZExt:
    llvm.LLVMZExt
  of CastOp.SExt:
    llvm.LLVMSExt
  of CastOp.FPToUI:
    llvm.LLVMFPToUI
  of CastOp.FPToSI:
    llvm.LLVMFPToSI
  of CastOp.UIToFP:
    llvm.LLVMUIToFP
  of CastOp.SIToFP:
    llvm.LLVMSIToFP
  of CastOp.FPTrunc:
    llvm.LLVMFPTrunc
  of CastOp.FPExt:
    llvm.LLVMFPExt
  of CastOp.PtrToInt:
    llvm.LLVMPtrToInt
  of CastOp.IntToPtr:
    llvm.LLVMIntToPtr
  of CastOp.BitCast:
    llvm.LLVMBitCast

proc addAttribute(f: Value, attrID: cuint) =
    var attr = createEnumAttribute(b.ctx, attrID, 0)
    addAttributeAtIndex(f, cast[AttributeIndex](AttributeFunctionIndex), attr)

proc newFunction(fty: Type, name: string, tags: uint32): Value =
    result = b.vars[0].getOrDefault(name, nil)
    if result != nil:
        return result
    result = addFunction(b.module, name.cstring, fty)
    nimLLVMSetDSOLocal(result)
    addAttribute(result, NoUnwind)
    addAttribute(result, OptimizeForSize)
    if bool(tags and TYSTATIC):
        setLinkage(result, InternalLinkage)
    if bool(tags and TYNORETURN):
        addAttribute(result, NoReturn)
    if bool(tags and TYINLINE):
        addAttribute(result, InlineHint)
    b.vars[0][name] = result

proc getLinkageName(tags: uint32): (cstring, uint) =
    if bool(tags and TYSTATIC):
        return (cstring("static"), 6.uint)
    if bool(tags and TYEXTERN):
        return (cstring("extern"), 6.uint)
    if bool(tags and TYTHREAD_LOCAL):
        return (cstring("_Thread_local"), 13.uint)
    return (nil, 0.uint)

proc gen(s: Stmt) =
  if app.g:
    emitDebugLocation(s)
  case s.k:
  of SCompound:
    if app.g:
          b.lexBlocks.add(
              dIBuilderCreateLexicalBlock(
                  b.di, getLexScope(), getFile(), s.loc.line, s.loc.col
              )
          )
    enterScope()
    for i in s.stmts:
      gen(i)
    leaveScope()
    if app.g:
        discard b.lexBlocks.pop()
  of SExpr:
      b.expr_stmt = true
      discard gen(s.exprbody)
  of SFunction:
      var debugMain = app.libtrace and s.funcname == "main"
      enterScope()
      if debugMain:
        var l = s.functy.params.len
        var v = false
        if l > 0 and s.functy.params[^1][1] == nil:
          v = true
          discard s.functy.params.pop()
          dec l
        if l == 0:
          s.functy.params.add(("argc", tycache.iintty))
        if l <= 1:
          s.functy.params.add(("argv", getPointerType(getPointerType(tycache.i8))))
        if v:
          s.functy.params.add(("", nil))
      var ty = wrap(s.functy)
      b.currentfunction = newFunction(ty, s.funcname, s.functy.tags)
      var sp: MetadataRef
      if app.g:
        var l = getLinkageName(s.functy.ret.tags)
        sp = dIBuilderCreateFunction(b.di, getLexScope(), s.funcname.cstring, s.funcname.len.csize_t, l[0], l[1], getFile(), 
            s.loc.line, createDebugFuctionType(s.functy), False, True, 0, DIFlagZero, False)
        setSubprogram(b.currentfunction, sp)
        b.lexBlocks.add(sp)
        emitDebugLocation()
      var entry = addBlock("entry")
      setInsertPoint(entry)
      b.labels.add(initTable[string, Label]())
      for L in s.labels:
        var j = addBlock(cstring(L))
        b.labels[^1][L] = j
      var paramLen = countParamTypes(ty)
      var loc: MetadataRef
      if app.g:
         loc = wrap(s.loc)
      if paramLen > 0:
        var fparamsTypes = create(Type, paramLen)
        var typesarr = cast[ptr UncheckedArray[Type]](fparamsTypes)
        getParamTypes(ty, fparamsTypes)
        var i = 0
        var iter = getFirstParam(b.currentfunction)
        while iter != nil:
          var name = cstring(s.functy.params[i][0])
          var p = buildAlloca(b.builder, typesarr[i], name)
          if app.g:
            var v = dIBuilderCreateParameterVariable(b.di, getLexScope(), name, s.functy.params[i][0].len.csize_t, cuint(i + 1), getFile(), s.loc.line, wrap3(s.functy.params[i][1]), False, DIFlagZero)
            discard dIBuilderInsertDeclareAtEnd(b.di, p, v, dIBuilderCreateExpression(b.di, nil, 0), loc, entry)
          store(p, iter)
          putVar(s.functy.params[i][0], p)
          iter = getNextParam(iter)
          inc i
        dealloc(fparamsTypes)
      if debugMain:
          var args = [getParam(b.currentfunction, 0), getParam(b.currentfunction, 1)]
          var argtypes = [b.types[DIi32].ty, b.types[DIptr].ty]
          var fty = functionType(b.types[DIvoidty].ty, addr argtypes[0], 2, False)
          var f = addFunction(b.module, "__libtrace_init", fty)
          discard buildCall2(b.builder, fty, f, addr args[0], 2, "")
      for i in s.funcbody.stmts:
        gen(i)
      leaveScope()
      if app.g:
        discard b.lexBlocks.pop()
      if app.g:
        dIBuilderFinalizeSubprogram(b.di, sp)
      discard b.labels.pop()
      b.currentfunction = nil
  of SReturn:
      if s.exprbody != nil:
        # gen(s.exprbody) may be nil if EVoid
        discard buildRet(b.builder, gen(s.exprbody))
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
    if app.g:
        discard wrap3Noqualified(s.decl)
    discard wrap(s.decl)
  of SLabled:
    var ib = b.labels[^1].getOrDefault(s.label, nil)
    assert ib != nil
    if getTerminator() == nil:
      br ib
    setInsertPoint(ib)
    if app.g:
        nimAddLabel(b.di, getLexScope(), ib, cstring(s.label), s.label.len.csize_t, getFile(), s.loc.line, loc=wrap(s.loc))
    gen(s.labledstmt)
  of SGoto:
    let loc = b.labels[^1].getOrDefault(s.location, nil)
    assert loc != nil
    if getTerminator() == nil:
        br loc
    setInsertPoint(loc)
  of SSemicolon:
    discard
  of SContinue:
    if getTerminator() == nil:
        br b.topContinue
  of SBreak:
    if getTerminator() == nil:
        br b.topBreak
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
        discard newFunction(wrap(varty), name, varty.tags)
      else:
        if b.currentfunction == nil or bool(varty.tags and (TYEXTERN or TYSTATIC)):
          block InitGV:
            var old = getVar(name)
            if old != nil:
              if bool(varty.tags and TYEXTERN):
                break InitGV
              deleteGlobal(old)
            var ty = wrap(varty)
            var ginit = if init == nil: constNull(ty) else: gen(init)
            var g = addGlobal(b.module, ty, cstring(name))
            if align != 0:
              setAlignment(g, align)
            if isConstant(ginit) == False:
              llvm_error("global initializer is not constant")
              return
            if (varty.tags and TYEXTERN) == 0:
                setInitializer(g, ginit)
                nimLLVMSetDSOLocal(g)
            if (varty.tags and TYTHREAD_LOCAL) != 0:
              # LLVMSetThreadLocalMode ?
              setThreadLocal(g, 1)
            if (varty.tags and TYSTATIC) != 0:
              setLinkage(g, InternalLinkage)
            elif (varty.tags and TYEXTERN) != 0:
              setLinkage(g, ExternalLinkage)
            else:
              discard
            putVar(name, g)
            if app.g:
              var linkage = getLinkageName(varty.tags)
              var gve = dIBuilderCreateGlobalVariableExpression(b.di, getLexScope(), cstring(name), name.len.csize_t, linkage[0], linkage[1], getFile(), s.loc.line, wrap3(varty), False, dIBuilderCreateExpression(b.di, nil, 0), nil, align)
              nimGlobalAddDebugInfo(g, gve)
        else:
          var ty: Type = nil
          var vla: Value = nil
          if varty.spec == TYARRAY and varty.vla != nil:
            vla = gen(varty.vla)
            ty = wrap(varty.arrtype)
          else:
            ty = wrap(varty)
          var val: Value
          if vla != nil:
            val = buildArrayAlloca(b.builder, ty, vla, cstring(name))
          else:
            val = buildAlloca(b.builder, ty, cstring(name))
          if app.g:
            var v = dIBuilderCreateAutoVariable(b.di, getLexScope(), cstring(name), name.len.csize_t, getFile(), s.loc.line, wrap3(varty), False, 0, 0)
            discard dIBuilderInsertDeclareAtEnd(b.di, val, v, dIBuilderCreateExpression(b.di, nil, 0), wrap(s.loc), getInsertBlock(b.builder))
          if align != 0:
            setAlignment(val, align)
            if init != nil:
              let initv = gen(init)
              store(val, initv, align)
          else:
            if init != nil:
              let initv = gen(init)
              store(val, initv)
          putVar(name, val)
  of SVarDecl1:
    unreachable()

proc gen_cast(e: Expr, to: CType, op: CastOp): Value

proc incl(p: Value, t: Type, align: uint32): Value =
  var l = load(p, t, align)
  var l2 = buildAdd(b.builder, l, constInt(t, 1), "")
  store(p, l2, align)
  return l

proc decl(p: Value, t: Type, align: uint32): Value =
  var l = load(p, t, align)
  var l2 = buildSub(b.builder, l, constInt(t, 1), "")
  store(p, l2, align)
  return l

proc getStruct(e: Expr): Value =
    var ty = wrap(e.ty)
    if len(e.arr) == 0:
      result = constNull(ty)
    else:
      var l = len(e.arr)
      var L = e.ty.selems.len
      var inits = create(Value, L)
      var arr = cast[ptr UncheckedArray[Value]](inits)
      for i in 0 ..< l:
        arr[i] = gen(e.arr[i])
      for i in l ..< L:
        arr[i] = constNull(wrap(e.ty.selems[i][1]))
      result = constNamedStruct(ty, inits, cuint(L))
      dealloc(inits)

proc getArray(e: Expr): Value =
    var l = len(e.arr).uintmax_t
    var elemTy = wrap(e.ty.arrtype)
    var inits = create(Value, e.ty.arrsize or 1)
    var arr = cast[ptr UncheckedArray[Value]](inits)
    for i in 0 ..< l:
      arr[i] = gen(e.arr[i])
    for i in l ..< e.ty.arrsize:
      arr[i] = constNull(elemTy)
    result = constArray(elemTy, inits, e.ty.arrsize.cuint)
    dealloc(inits)

proc getAddress(e: Expr): Value =
  if app.g:
      emitDebugLocation(e)
  case e.k:
  of EVar:
    getVar(e.sval)
  of EPointerMemberAccess, EMemberAccess:
    var basep = if e.k == EMemberAccess: getAddress(e.obj) else: gen(e.obj)
    var r = [b.i32_0, constInt(b.types[DIi32].ty, e.idx.culonglong)]
    var ty = wrap(e.obj.ty)
    gep(ty, basep, r)
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
        var ty = wrap(e.poperand.ty.p)
        var i = b.i32_1
        if e.pop == PostfixDecrement:
          i = constNeg(i)
        var l = load(basep, wrap(e.poperand.ty))
        var g = gep(ty, l, i)
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
    gep(ty, v, r)
  of ArrToAddress:
    var arr = getAddress(e.voidexpr)
    buildBitCast(b.builder, arr, wrap(e.ty), "")
  of EString:
    gen_str_ptr(e.str, e.is_constant)
  of EArray, EStruct:
    var v = if e.k == EArray: getArray(e) else: getStruct(e)
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

proc gen(e: Expr): Value =
  if app.g:
    emitDebugLocation(e)
  var expr_stmt = b.expr_stmt
  b.expr_stmt = false
  case e.k:
  of EUndef:
    getUndef(wrap(e.ty))
  of EVLAGetSize:
    var s = nimLLVMGetAllocaArraySize(gen(e.vla))
    var z = buildZExt(b.builder, s, b.types[DIi64].ty, "")
    buildMul(b.builder, z, constInt(b.types[DIi64].ty, getsizeof(e.vla.voidexpr.ty.arrtype)), "")
  of ArrToAddress:
    var arr = getAddress(e.voidexpr)
    buildBitCast(b.builder, arr, wrap(e.ty), "")
  of ESubscript:
    assert e.left.ty.spec == TYPOINTER # the left must be a pointer
    var ty = wrap(e.left.ty.p)
    var v = gen(e.left) # a pointer
    var r = gen(e.right) # get index
    var gaddr = gep(ty, v, r)
    load(gaddr, ty) # return lvalue
  of EMemberAccess, EPointerMemberAccess:
    var base = gen(e.obj)
    if e.k == EPointerMemberAccess:
      base = load(base, wrap(e.obj.ty), e.obj.ty.align)
    buildExtractValue(b.builder, base, e.idx.cuint, "")
  of EString:
    gen_str_ptr(e.str, e.is_constant)
  of EBackend:
    cast[Value](e.p)
  of EBin:
    case e.bop:
    of Assign:
      var v = gen(e.rhs)
      let basep = getAddress(e.lhs)
      store(basep, v, e.lhs.ty.align)
      if expr_stmt:
        nil
      else:
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
        var c = constInt(b.types[DIintPtr].ty, s.culonglong)
        buildExactSDiv(b.builder, sub, c, "")
      else:
        sub
    of SAddP:
       var l = gen(e.lhs)
       var r = gen(e.rhs)
       gep(if bool(e.lhs.ty.p.tags and TYVOID) or (e.lhs.k == EUnary and e.lhs.uop == AddressOf and e.lhs.ty.p.spec == TYFUNCTION): b.types[DIi8].ty else: wrap(e.ty.p), l, r)
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
      b.expr_stmt = true
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
    b.expr_stmt = true
    discard gen(e.voidexpr)
    nil
  of EUnary:
    case e.uop:
    of Pos: gen(e.uoperand)
    of SNeg: buildNSWNeg(b.builder, gen(e.uoperand), "")
    of UNeg: buildNeg(b.builder, gen(e.uoperand), "")
    of FNeg: buildFNeg(b.builder, gen(e.uoperand), "")
    of Not: buildNot(b.builder, gen(e.uoperand), "")
    of AddressOf: getAddress(e.uoperand)
    of PrefixIncrement, PrefixDecrement:
      var i = b.i32_1
      if e.uop == PrefixDecrement:
        i = constNeg(i)
      let basep = getAddress(e.uoperand)
      if e.ty.spec == TYPOINTER:
        var ty = wrap(e.ty.p)
        var l = load(basep, wrap(e.uoperand.ty))
        var g = gep(ty, l, i)
        store(basep, g)
        g
      else:
        var ty = wrap(e.ty)
        var l = load(basep, ty)
        i = constIntCast(i, ty, False)
        var l2 = buildAdd(b.builder, l, i, "")
        store(basep, l2)
        if expr_stmt:
          nil
        else:
          load(basep, ty)
    of Dereference: load(gen(e.uoperand), wrap(e.ty))
    of LogicalNot: buildisNull(b.builder, gen(e.uoperand), "")
  of EPostFix:
    case e.pop:
    of PostfixIncrement, PostfixDecrement:
      var basep = getAddress(e.poperand)
      if e.ty.spec == TYPOINTER:
        var ty = wrap(e.poperand.ty.p)
        var i = b.i32_1
        if e.pop == PostfixDecrement:
          i = constNot(i)
        var l = load(basep, wrap(e.poperand.ty))
        var g = gep(ty, l, i)
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
    var l = len(e.callargs)
    var args = create(Value, l or 1)
    var arr = cast[ptr UncheckedArray[Value]](args)
    for i in 0 ..< l:
      arr[i] = gen(e.callargs[i]) # eval argument from left to right
    var old: Value
    if app.libtrace:
      var s = $e
      var line = e.loc.line
      var file = "FooBar"
      old = buildlibtraceBeginCall(s, line, file)
    var res = buildCall2(b.builder, ty, f, args, l.cuint, "")
    if app.libtrace:
      buildlibtraceAfterCall(old)
    dealloc(args)
    res
  of EStruct:
    getStruct(e)
  of EArray:
    getArray(e)

proc gen_cast(e: Expr, to: CType, op: CastOp): Value =
  var c = gen(e)
  buildCast(b.builder, getCastOp(op), c, wrap(to), "")

proc jit_error(msg: string) =
  raw_message "LLVM JIT ERROR: " & msg & '\n'

proc jit_error(err: ErrorRef) =
  var msg = getErrorMessage(err)
  raw_message_line msg
  disposeErrorMessage(msg)

proc getThreadSafeModule(): OrcThreadSafeModuleRef =
    result = orcCreateNewThreadSafeModule(b.module, b.tsCtx)
    orcDisposeThreadSafeContext(b.tsCtx)

proc runjit() =
    if targetHasJIT(b.target) == False:
      llvm_error("this target has no JIT!")
      return
    var thread_safe_mod = getThreadSafeModule()
    var jit: OrcLLJITRef 
    
    var err = orcCreateLLJIT(addr jit, nil)
    if err != nil:
      jit_error(err)
      return

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

    var argslen = len(options)
    var mainargs = create(cstring, argslen or 1)
    var arr = cast[ptr UncheckedArray[cstring]](mainargs)

    for i in 0..<argslen:
      arr[i] = cstring(options[i])

    let ret = fmain(cint(argslen), mainargs)

    dealloc(mainargs)

    set_exit_code(ret)

    verbose("main() return: " & $ret)

    err = orcDisposeLLJIT(jit)
    if err != nil:
      jit_error(err)
      return

proc str(s: string): seq[TokenV] =
  @[TokenV(tok: TStringLit, tags: TVSVal, s: s)]
proc str2(s: string): TokenV =
  TokenV(tok: TStringLit, tags: TVSVal, s: s)
proc num(s: string): seq[TokenV] =
  @[TokenV(tok: TPPNumber, tags: TVSVal, s: s)]
proc space(): TokenV = 
  TokenV(tok: TSpace, tags: TVNormal)

var
  one = num("1")
  empty: seq[TokenV] 

iterator getDefines(): (string, seq[TokenV]) =
  discard F32Bit
  # (windows) gcc -dM -E - <NUL:
  # (bash) gcc -dM -E - </dev/null
  yield ("__builtin_LINE", str("__LINE__"))
  yield ("__builtin_FILE", str("__FILE__"))
  yield ("__builtin_FUNCTION", str("__func__"))
  yield ("__STDC__", one)
  yield ("__STDC_VERSION__", num("201710L"))
  yield ("__STDC_HOSTED__", one)
#  yield ("__STDC_NO_THREADS__", @[one])
#  yield ("__STDC_NO_ATOMICS__", @[one])
  yield ("__STDC_UTF_16__", one)
  yield ("__STDC_UTF_32__", one)
  yield ("__SIZE_TYPE__", str("size_t"))
  yield ("__INT8_TYPE__", str("__int8"))
  yield ("__INT16_TYPE__", str("__int16"))
  yield ("__INT32_TYPE__", str("__int32"))
  yield ("__INT64_TYPE__", str("__int64"))
  yield ("__INT_FAST8_TYPE__", str("__int8"))
  yield ("__INT_FAST16_TYPE__", str("__int16"))
  yield ("__INT_FAST32_TYPE__", str("__int32"))
  yield ("__INT_FAST64_TYPE__", str("__int64"))
  yield ("__UINT_FAST8_TYPE__", @[str2("unsigned"), space(), str2("__int8")])
  yield ("__UINT_FAST16_TYPE__", @[str2("unsigned"), space(), str2("__int16")])
  yield ("__UINT_FAST32_TYPE__", @[str2("unsigned"), space(), str2("__int32")])
  yield ("__UINT_FAST64_TYPE__", @[str2("unsigned"), space(), str2("__int64")])
  yield ("__INTPTR_TYPE__", @[str2("long"), space(), str2("long"), space(), str2("int")])
  yield ("__UINTPTR_TYPE__", @[str2("unsigned"), space(), str2("long"), space(), str2("long"), space(), str2("int")])
  yield ("__CHAR_BIT__", num("8"))
  # https://stackoverflow.com/questions/142508/how-do-i-check-os-with-a-preprocessor-directive

  if bool(b.f and FMinGW):
    if bool(b.f and F64Bit):
      yield ("__MINGW64__", one)
    yield ("__MINGW32__", one)

  case b.os:
  of IOS:
    yield ("__APPLE__", empty)
  of Darwin:
    yield ("__APPLE__", empty)
  of MacOSX:
    yield ("__APPLE__", empty)
    yield ("__MACH__", empty)
  of FreeBSD:
    yield ("__FreeBSD__", empty)
  of Solaris:
    yield ("__sun", empty)
  of Linux:
    yield ("__linux__", one)
    yield ("__linux", one)
    yield ("linux", empty)
    yield ("__unix__", one)
    yield ("__unix", one)
    yield ("unix", empty)
  of Win32:
    yield ("_WIN32", one)
    yield ("WIN32", one)
    if bool(b.f and F64Bit):
      yield ("_WIN64", one)
      yield ("WIN32", one)
  of NetBSD:
    yield ("__NetBSD__", empty)
    yield ("__unix__", one)
    yield ("__unix", one)
  of OpenBSD:
    yield ("__OpenBSD__", empty)
    yield ("__unix__", one)
    yield ("__unix", one)
  of DragonFly:
    discard
  of Fuchsia:
    discard
  of KFreeBSD:
    discard
  of Lv2:
    discard
  of ZOS:
    discard
  of Haiku:
    discard
  of Minix:
    discard
  of RTEMS:
    discard
  of NaCl:
    discard
  of AIX:
    discard
  of CUDA:
    discard
  of NVCL:
    discard
  of AMDHSA:
    discard
  of PS4:
    discard
  of PS5:
    discard
  of ELFIAMCU:
    discard
  of TvOS:
    discard
  of WatchOS:
    discard
  of DriverKit:
    discard
  of Mesa3D:
    discard
  of Contiki:
    discard
  of AMDPAL:
    discard
  of HermitCore:
    discard
  of Hurd:
    discard
  of WASI:
    discard
  of Emscripten:
    discard
  of ShaderModel:
    discard
  of UnknownOS:
    discard
  of Ananas:
    discard
  of CloudABI:
    discard

  case b.env:
  of UnknownEnvironment:
    discard
  of GNU:
    discard
  of GNUABIN32:
    discard
  of GNUABI64:
    discard
  of GNUEABI:
    discard
  of GNUEABIHF:
    discard
  of GNUX32:
    discard
  of GNUILP32:
    discard
  of CODE16:
    discard
  of EABI:
    discard
  of EABIHF:
    discard
  of Android:
    yield ("__ANDROID__", empty)
  of Musl:
    discard
  of MuslEABI:
    discard
  of MuslEABIHF:
    discard
  of MuslX32:
    discard
  of MSVC:
    discard
  of Itanium:
    discard
  of Cygnus:
    yield ("__CYGWIN__", empty)
  of CoreCLR:
    discard
  of Simulator:
    discard
  of MacABI:
    discard
  of Pixel:
    discard
  of Vertex:
    discard
  of Geometry:
    discard
  of Hull:
    discard
  of Domain:
    discard
  of Compute:
    discard
  of Library:
    discard
  of RayGeneration:
    discard
  of Intersection:
    discard
  of AnyHit:
    discard
  of ClosestHit:
    discard
  of Miss:
    discard
  of Callable:
    discard
  of Mesh:
    discard
  of Amplification:
    discard

  case b.arch:
  of UnknownArch:
    discard
  of arm:
    discard
  of armeb:
    discard         
  of aarch64:
    discard       
  of aarch64_be:
    discard    
  of aarch64_32:
    discard    
  of arc:
    discard           
  of avr:
    discard           
  of bpfel:
    discard         
  of bpfeb:
    discard         
  of csky:
    discard          
  of dxil:
    discard          
  of hexagon:
    discard       
  of loongarch32:
    discard   
  of loongarch64:
    discard   
  of m68k:
    discard          
  of mips:
    discard          
  of mipsel:
    discard        
  of mips64:
    discard        
  of mips64el:
    discard      
  of msp430:
    discard        
  of ppc:
    discard           
  of ppcle:
    discard         
  of ppc64:
    discard         
  of ppc64le:
    discard       
  of r600:
    discard          
  of amdgcn:
    discard        
  of riscv32:
    discard       
  of riscv64:
    discard       
  of sparc:
    discard         
  of sparcv9:
    discard       
  of sparcel:
    discard       
  of systemz:
    discard       
  of tce:
    discard           
  of tcele:
    discard         
  of thumb:
    discard         
  of thumbeb:
    discard       
  of x86:
    discard           
  of x86_64:
    discard        
  of xcore:
    discard         
  of nvptx:
    discard         
  of nvptx64:
    discard       
  of le32:
    discard          
  of le64:
    discard          
  of amdil:
    discard         
  of amdil64:
    discard       
  of hsail:
    discard         
  of hsail64:
    discard       
  of spir:
    discard          
  of spir64:
    discard        
  of spirv32:
    discard       
  of spirv64:
    discard       
  of kalimba:
    discard       
  of shave:
    discard         
  of lanai:
    discard         
  of wasm32:
    discard        
  of wasm64:
    discard        
  of renderscript32:
      discard
  of renderscript64:
      discard
  of ve:
    discard


const
  LBL_UNDEFINED = 0'u8
  LBL_FORWARD = 1'u8
  LBL_DECLARED = 2'u8
  LBL_OK = 4'u8

proc isTopLevel(): bool =
    t.sema.scopes.len == 1

const 
  INFO_USED = 2

proc binop(a: Expr, op: BinOP, b: Expr, ty: CType): Expr = 
    ## construct a binary operator
    Expr(k: EBin, lhs: a, rhs: b, bop: op, ty: ty, loc: a.loc)

proc unary(e: Expr, op: UnaryOP, ty: CType): Expr = 
    ## construct a unary operator
    Expr(k: EUnary, uop: op, uoperand: e, ty: ty, loc: e.loc)

proc enterBlock() =
  t.sema.scopes.add(BScope(
    typedefs: initTable[string, Info](),
    tags: initTable[string, Info](),
    enums: initTable[string, uintmax_t]()
  ))

proc leaveBlock() =
    if isTopLevel():
        for (name, i) in t.sema.scopes[0].typedefs.pairs():
            if bool(i.ty.tags and TYTYPEDEF):
                continue
            if (i.tag and INFO_USED) == 0:
                if i.ty.spec == TYFUNCTION:
                    if bool(i.ty.ret.tags and TYSTATIC):
                        warning("static function '" & name & "' defined/declared but not used")
                elif bool(i.ty.tags and TYSTATIC):
                    warning("static variable '" & name & "' defined/declared but not used")
    else:
      for (name, i) in t.sema.scopes[^1].typedefs.pairs():
        if (i.tag and INFO_USED) == 0:
            if bool(i.ty.tags and TYPARAM):
                warning("unused parameter '" & name & '\'')
            else:
                warning("unused variable '" & name & '\'')
    discard t.sema.scopes.pop()

proc isFloating(ty: CType): bool =
    bool(ty.tags and (TYFLOAT or TYDOUBLE))

proc isSigned(ty: CType): bool =
    ## `_Bool` is not signed
    if ty.spec == TYBITFIELD:
        isSigned(ty.bittype)
    else:
        bool(ty.tags and (
            TYINT8 or
            TYINT16 or
            TYINT16 or
            TYINT32 or
            TYINT64
        ))

proc checkInteger(a: CType): bool =
    return (a.spec == TYPRIM) and 
    bool(
        a.tags and (
            TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
            TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64 or 
            TYBOOL
        )
    )

proc checkScalar(a: CType): bool =
    return a.spec == TYPOINTER or 
    (
        a.spec == TYPRIM and
        (
            (a.tags and TYVOID) == 0
        )
    )

proc intRank(a: uint32): int =
    if bool(a and TYBOOL):
        return 1
    if bool(a and (TYINT8 or TYUINT8)):
        return 2
    if bool(a and (TYINT16 or TYUINT16)):
        return 3
    if bool(a and (TYINT32 or TYUINT32)):
        return 4
    return 5

proc err(): bool =
  t.sema.type_error or t.parse_error or t.pp.eval_error

proc inTheExpression(e: Expr) =
    ## emit in the expression message
    note("in the expression '" & $e & '\'')

proc getTag(name: string): Info =
  for i in countdown(len(t.sema.scopes)-1, 0):
    result = t.sema.scopes[i].tags.getOrDefault(name, nil)
    if result != nil:
      result.tag = result.tag or INFO_USED
      return result

proc gettypedef(name: string): Info =
  for i in countdown(len(t.sema.scopes)-1, 0):
    result = t.sema.scopes[i].typedefs.getOrDefault(name, nil)
    if result != nil:
      result.tag = result.tag or INFO_USED
      return result

proc getstructdef(name: string): CType =
    var r = getTag(name)
    if r == nil:
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYSTRUCT, name: name)
    elif r.ty.spec != TYSTRUCT:
        type_error(name & " is not a struct")
    else:
        result = r.ty

proc getLabel(name: string) =
    let p = t.sema.lables[^1].getOrDefault(name, LBL_UNDEFINED)
    case p:
    of LBL_UNDEFINED:
        t.sema.lables[^1][name] = LBL_FORWARD
    of LBL_FORWARD:
        discard
    of LBL_DECLARED:
        t.sema.lables[^1][name] = LBL_OK
    else:
        unreachable()

proc putLable(name: string) =
    let p = t.sema.lables[^1].getOrDefault(name, LBL_UNDEFINED)
    case p:
    of LBL_UNDEFINED:
        t.sema.lables[^1][name] = LBL_DECLARED
    of LBL_FORWARD:
        t.sema.lables[^1][name] = LBL_OK
    of LBL_DECLARED:
        type_error("duplicate label: " & name)
    else:
        unreachable()

proc putstructdef(ty: CType) =
    let o = t.sema.scopes[^1].tags.getOrDefault(ty.sname)
    if o != nil:
        error("struct " & ty.sname & " aleady defined")
        note(ty.sname & "was defined at " & $o.loc)
    else:
        t.sema.scopes[^1].tags[ty.sname] = Info(ty: ty, loc: Location(line: t.l.line, col: t.l.col))

proc getenumdef(name: string): CType =
    let r = getTag(name)
    if r == nil:
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYENUM, name: name)
    elif r.ty.spec != TYENUM:
        type_error(name & " is not a enum")
    else:
        result = r.ty

proc putenumdef(ty: CType) =
    let o = t.sema.scopes[^1].tags.getOrDefault(ty.ename)
    if o != nil:
        error("enum " & ty.ename & " aleady defined")
        note(ty.ename & "was defined at " & $o.loc)
    else:
        t.sema.scopes[^1].tags[ty.ename] = Info(ty: ty, loc: Location(line: t.l.line, col: t.l.col))

proc getuniondef(name: string): CType =
    let r = getTag(name)
    if r == nil:
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYUNION, name: name)
    elif r.ty.spec != TYUNION:
        type_error(name & " is not a union")
    else:
        result = r.ty

proc putuniondef(ty: CType) =
    let o = t.sema.scopes[^1].tags.getOrDefault(ty.sname)
    if o != nil:
        error("`union` " & ty.sname & " aleady defined")
        note(ty.sname & " was defined at " & $o.loc)
    else:
        t.sema.scopes[^1].tags[ty.sname] = Info(ty: ty, loc: Location(line: t.l.line, col: t.l.col))

proc putenum(name: string, val: uintmax_t) =
    if t.sema.scopes[^1].enums.contains(name) or t.sema.scopes[^1].typedefs.contains(name):
        type_error('\'' & name & "' redeclared")
    t.sema.scopes[^1].enums[name] = val

proc noEnum(name: string) =
    if t.sema.scopes[^1].enums.hasKey(name):
        type_error(name & " redeclared")

# function
proc putsymtype2(name: string, yt: CType) =
  noEnum(name)
  let ty = t.sema.scopes[^1].typedefs.getOrDefault(name, nil)
  if ty != nil:
    if not compatible(yt, ty.ty):
        warning("conflicting types for function declaration '" & name &  "'")
  t.sema.scopes[^1].typedefs[name] = Info(ty: yt, loc: Location(line: t.l.line, col: t.l.col))

# typedef, variable
proc putsymtype(name: string, yt: CType) =
  noEnum(name)
  if yt.spec == TYFUNCTION:
    putsymtype2(name, yt)
    return
  let ty = t.sema.scopes[^1].typedefs.getOrDefault(name, nil)
  if ty != nil:
    if isTopLevel() or bool(yt.tags and (TYEXTERN or TYSTATIC)):
        var old = ty.ty
        var err = true
        const q = TYATOMIC or TYCONST or TYRESTRICT
        if bool(yt.tags and TYSTATIC) and (old.tags and TYSTATIC) == 0:
            type_error("static declaration of '" & name & "' follows non-static declaration")
        elif bool(old.tags and TYSTATIC) and (yt.tags and TYSTATIC) == 0:
            type_error("non-static declaration of '" & name & "' follows static declaration")
        elif bool(yt.tags and TYTHREAD_LOCAL) and (old.tags and TYTHREAD_LOCAL) == 0:
            type_error("thread-local declaration of '" & name & "' follows non-thread-local declaration")
        elif bool(old.tags and TYTHREAD_LOCAL) and (yt.tags and TYTHREAD_LOCAL) == 0:
            type_error("non-thread-local declaration of '" & name & "' follows thread-local declaration")
        elif (yt.tags and q) != (old.tags and q):
            type_error("conflicting type qualifiers for '" & name & "'")
        elif not compatible(old, yt):
            type_error("conflicting types for '" & name & '\'')
        else:
            err = false
        if err:
            note("previous declaration of '" & name & "'' with type '" & $old & "'")
    else:
        type_error(name & " redeclared")
  t.sema.scopes[^1].typedefs[name] = Info(ty: yt, loc: Location(line: t.l.line, col: t.l.col))

proc istype(a: Token): bool =
    if a in (declaration_specifier_set + {Kstruct, Kenum, Kunion, K_Alignas}):
        return true
    if a == TIdentifier:
      let o = gettypedef(t.l.tok.s)
      return o != nil and (o.ty.tags and TYTYPEDEF) != 0
    return false

proc intcast(e: Expr, to: CType): Expr = 
    if bool(to.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64)) and 
       bool(e.ty.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64 or TYBOOL)):
        if to.tags == e.ty.tags:
            return Expr(k: ECast, castop: CastOp.BitCast, castval: e, ty: to, loc: e.loc)
        if intRank(to.tags) > intRank(e.ty.tags):
            if isSigned(to) and (e.ty.tags and TYBOOL) == 0:
                return Expr(k: ECast, castop: CastOp.SExt, castval: e, ty: to, loc: e.loc)
            else:
                return Expr(k: ECast, castop: CastOp.ZExt, castval: e, ty: to, loc: e.loc)
        else:
            return Expr(k: ECast, castop: CastOp.Trunc, castval: e, ty: to, loc: e.loc)
    type_error("cannot cast " & $e & " to " & $to)
    note("expression " & $e & " has type " & $e.ty)
    return nil

proc type_equal(a, b: CType): bool =
    if a.spec != b.spec:
        false
    else:
        case a.spec: 
        of TYPRIM:
            var ig = not (TYLVALUE or TYCONST or TYVOLATILE or TYRESTRICT)
            (a.tags and ig) == (b.tags and ig)
        of TYPOINTER:
            type_equal(a.p, b.p)
        of TYENUM, TYSTRUCT, TYUNION:
            cast[pointer](a) == cast[pointer](b)
        of TYINCOMPLETE:
            a.name == b.name
        else:
            false

proc castto(e: Expr, to: CType): Expr =
    if type_equal(e.ty, to):
        return e
    if bool(e.ty.tags and TYVOID):
        type_error("cannot cast 'void' expression to type " & $to)
        return nil
    if to.spec == TYINCOMPLETE:
        error_incomplete(to)
        return nil
    if (to.spec == TYSTRUCT and e.ty.spec == TYSTRUCT) or (to.spec == TYUNION and e.ty.spec == TYUNION):
        if to != e.ty:
            type_error("cannot cast between different struct/unions")
            return nil
        return e
    if e.ty.spec == TYENUM:
        # cast enum to int
        return castto(Expr(k: ECast, castop: BitCast, castval: e, ty: tycache.iintty, loc: e.loc), to)
    if to.spec == TYENUM:
        # cast xxx to int, then cast(bitcast) to enum
        return Expr(k: ECast, castop: BitCast, castval: castto(e, tycache.iintty), ty: to, loc: e.loc)
    if checkScalar(e.ty) == false or checkScalar(to) == false:
        type_error("cast uoperand shall have scalar type")
        return nil
    if bool(to.tags and TYBOOL):
        return binop(e, NE, Expr(k: EDefault, ty: e.ty, loc: e.loc), tycache.b)
    if bool(to.tags and (TYFLOAT or TYDOUBLE)) and e.ty.spec == TYPOINTER:
        type_error("A floating type shall not be converted to any pointer type")
        return nil
    if bool(e.ty.tags and (TYFLOAT or TYDOUBLE)) and to.spec == TYPOINTER:
        type_error("A floating type shall not be converted to any pointer type")
        return nil
    if e.ty.spec == TYPOINTER and to.spec == TYPOINTER:
        return Expr(k: ECast, castop: BitCast, castval: e, ty: to, loc: e.loc)
    if bool(e.ty.tags and TYDOUBLE) and bool(to.tags and TYFLOAT):
        return Expr(k: ECast, castop: FPTrunc, castval: e, ty: to, loc: e.loc)
    if bool(e.ty.tags and TYFLOAT) and bool(to.tags and TYDOUBLE):
        return Expr(k: ECast, castop: FPExt, castval: e, ty: to, loc: e.loc)
    if bool(e.ty.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64 or TYBOOL)):
        if to.spec == TYPOINTER:
            return Expr(k: ECast, castop: IntToPtr, castval: e, ty: to, loc: e.loc)
        if bool(to.tags and (TYFLOAT or TYDOUBLE)):
            if isSigned(e.ty):
                return Expr(k: ECast, castop: SIToFP, castval: e, ty: to, loc: e.loc)
            else:
                return Expr(k: ECast, castop: UIToFP, castval: e, ty: to, loc: e.loc)
    elif bool(to.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64)):
        if e.ty.spec == TYPOINTER:
            return Expr(k: ECast, castop: PtrToInt, castval: e, ty: to, loc: e.loc)
        if bool(e.ty.tags and (TYFLOAT or TYDOUBLE)):
            if isSigned(to):
                return Expr(k: ECast, castop: FPToSI, castval: e, ty: to, loc: e.loc)
            else:
                return Expr(k: ECast, castop: FPToUI, castval: e, ty: to, loc: e.loc)
    return intcast(e, to)

proc to(e: var Expr, tag: uint32) =
    if e.ty.tags != tag:
        e = castto(e, CType(tags: tag, spec: TYPRIM))

proc integer_promotions(e: Expr): Expr =
    if e.ty.spec == TYBITFIELD or getsizeof(e) < sizeofint:
        castto(e, tycache.iintty)
    else:
        e

proc integer_promotions(e: var Expr) =
    if e.ty.spec == TYBITFIELD or getsizeof(e) < sizeofint:
        to(e, TYINT)

proc conv(a, b: var Expr) =
    var at = a.ty.tags and prim
    var bt = b.ty.tags and prim
    if a.ty.spec != TYPRIM or b.ty.spec != TYPRIM:
        return
    if (at and TYLONGDOUBLE) != 0:
        to(b, TYLONGDOUBLE)
    elif (bt and TYLONGDOUBLE) != 0:
        to(a, TYLONGDOUBLE)
    elif (at and TYDOUBLE) != 0:
        to(b, TYDOUBLE)
    elif (bt and TYDOUBLE) != 0:
        to(a, TYDOUBLE)
    elif (at and TYFLOAT) != 0:
        to(b, TYFLOAT)
    elif (bt and TYFLOAT) != 0:
        to(a, TYFLOAT)
    else:
        integer_promotions(a)
        integer_promotions(b)
        if intRank(a.ty.tags) == intRank(b.ty.tags):
            return
        let
          sizeofa = getsizeof(a.ty)
          sizeofb = getsizeof(b.ty)
          isaunsigned = bool(at and unsigned)
          isbunsigned = bool(bt and unsigned)
        if (isaunsigned and isbunsigned) or (isaunsigned == false and isbunsigned == false):
            if sizeofa > sizeofb:
                to(b, at)
            else:
                to(a, bt)
        else:
            if isaunsigned and (sizeofa > sizeofb):
                to(b, at)
            elif isbunsigned and (sizeofb > sizeofa):
                to(a, bt)
            elif isaunsigned==false and sizeofa > sizeofb:
                to(b, at)
            elif isbunsigned==false and sizeofb > sizeofa:
                to(a, bt)
            else:
                if isaunsigned:
                    to(b, at)
                else:
                    to(a, bt)

proc default_argument_promotions(e: Expr): Expr =
    if e.ty.spec == TYENUM or e.ty.spec == TYUNION or e.ty.spec == TYSTRUCT or e.ty.spec == TYPOINTER:
        return e
    if bool(e.ty.tags and TYFLOAT):
        castto(e, tycache.fdoublety)
    else:
        integer_promotions(e)

proc checkInteger(a, b: Expr) =
    let ok = checkInteger(a.ty) and checkInteger(b.ty)
    if ok == false:
        type_error("integer expected")

proc checkScalar(a, b: Expr) =
    let ok = checkScalar(a.ty) and checkScalar(b.ty)
    if ok == false:
        type_error("scalar expected")

proc checkArithmetic(a: CType): bool =
    if a.spec != TYPRIM:
        return false
    return true

proc checkArithmetic(a, b: Expr) =
    let ok = checkArithmetic(a.ty) and checkArithmetic(b.ty)
    if ok == false:
        type_error("arithmetic type expected")

proc checkSpec(a, b: var Expr) =
    if a.ty.spec != b.ty.spec:
        type_error("operands type mismatch: " & $a & ", " & $b)
    else:
        checkScalar(a, b)
        conv(a, b)

proc make_add(result, r: var Expr) =
    if isFloating(result.ty):
        result = binop(result, FAdd, r, r.ty)
        conv(result, r)
    else:
        if result.ty.spec == TYPOINTER:
            if checkInteger(r.ty) == false:
                type_error("integer expected")
            result = binop(result, SAddP, r, result.ty)
        else:
            checkSpec(result, r)
            result = binop(result, if isSigned(r.ty): SAdd else: UAdd, r, r.ty)

proc make_sub(result, r: var Expr) =
    if isFloating(result.ty):
        result = binop(result, FSub, r, r.ty)
        conv(result, r)
    else:
        if result.ty.spec == TYPOINTER:
            if r.ty.spec == TYPOINTER:
                # two pointer substraction
                # https://stackoverflow.com/questions/65748155/what-is-the-motivation-for-ptrdiff-t
                if bool(result.ty.p.tags and TYVOID) or bool(r.ty.p.tags and TYVOID):
                    type_error("cannot substract on a pointer to void")
                    return
                if not compatible(result.ty.p, r.ty.p):
                    type_error("incompatible type when substract two pointers")
                    return
                result = binop(
                    Expr(k: ECast, castop: PtrToInt, castval: result, ty: tycache.iptrdiff_tty, loc: result.loc),
                    PtrDiff,
                    Expr(k: ECast, castop: PtrToInt, castval: r, ty: tycache.iptrdiff_tty, loc: result.loc),
                    tycache.iintptrty
                )
                return
            if checkInteger(r.ty) == false:
                type_error("integer expected")
            result = binop(result, SAddP, unary(r, if isSigned(r.ty): SNeg else: UNeg, r.ty), result.ty)
        else:
            checkSpec(result, r)
            result = binop(result, if isSigned(r.ty): SSub else: USub, r, r.ty) 

proc make_shl(result, r: var Expr) =
    checkInteger(result, r)
    integer_promotions(result)
    integer_promotions(r)
    result = binop(result, Shl, r, result.ty)

proc make_shr(result, r: var Expr) =
    checkInteger(result, r)
    integer_promotions(result)
    integer_promotions(r)
    if isSigned(result.ty):
        result = binop(result, AShr, r, result.ty)
    else:
        result = binop(result, Shr, r, result.ty)

proc make_bitop(result, r: var Expr, op: BinOP) =
    checkInteger(result, r)
    conv(result, r)
    result = binop(result, op, r, result.ty)

proc make_mul(result, r: var Expr) =
    checkArithmetic(result, r)
    conv(result, r)
    result = binop(result, if isFloating(r.ty): FMul else: (if isSigned(r.ty): SMul else: UMul), r, r.ty)

proc make_div(result, r: var Expr) =
    checkArithmetic(result, r)
    conv(result, r)
    result = binop(result, if isFloating(r.ty): FDiv else: (if isSigned(r.ty): SDiv else: UDiv), r, r.ty)

proc make_rem(result, r: var Expr) =
    checkInteger(result, r)
    conv(result, r)
    result = binop(result, if isFloating(r.ty): FRem else: (if isSigned(r.ty): SRem else: URem)  ,r, r.ty)

proc boolToInt(e: Expr): Expr =
    Expr(k: ECast, castop: CastOp.ZExt, castval: e, ty: tycache.iintty, loc: e.loc)

type 
    DeclaratorFlags = enum 
        ##  Direct: declarator with name
        ##  Abstract: declarator without name
        ##  Function: function parameter-type-list
        Direct, Abstract, Function

proc direct_declarator(base: CType; flags=Direct): Stmt

proc declarator(base: CType; flags=Direct): Stmt

template abstract_decorator(base, f): untyped = 
    ## abstract decorator has no name
    ## for example: `static int        ()(int, char)`
    ##               base-type      abstact-decorator
    declarator(base, flags=f)


proc struct_union(tok: Token): CType

proc enum_decl(): CType

proc static_assert(): Stmt

proc translation_unit(): Stmt

proc statament(): Stmt

proc compound_statement(): Stmt

proc compound_statement(params: seq[(string, CType)], lables: var HashSet[string]): Stmt

proc postfix(e: Expr, op: PostfixOP, ty: CType): Expr = 
    ## construct a postfix operator
    Expr(k: EPostFix, pop: op, poperand: e, ty: ty, loc: e.loc)

proc consume() =
    ## eat token from lexer and c preprocesser
    ##
    ## alias for `getToken`
    getToken()
    if t.l.tok.tok == TIdentifier:
        let k = isKeyword(t.l.tok.s)
        if k != TNul:
            make_tok(k)


proc addTag(ty: var CType, t: Token): bool =
    ## add a tag to type
    let t = (
        case t:
        of Kinline: TYINLINE
        of K_Noreturn: TYNORETURN
        of Kextern: TYEXTERN
        of Kstatic: TYSTATIC
        of K_Thread_local: TYTHREAD_LOCAL
        of Kregister: TYREGISTER
        of Krestrict: TYRESTRICT
        of Kvolatile: TYVOLATILE
        of Ktypedef: TYTYPEDEF
        of Kconst: TYCONST
        of K_Atomic: TYATOMIC
        else: TYINVALID
    )
    if t == TYINVALID:
        return false
    ty.tags = ty.tags or t
    return true

proc merge_types(ts: seq[Token]): CType =
    ## merge many token to a type
    ##
    ## for example:
    ##
    ##   `long long int const` => const long long
    if ts.len == 0:
        return nil
    result = CType(tags: TYINVALID, spec: TYPRIM)
    var b: seq[Token]
    for t in ts:
        if addTag(result, t) == false:
            b.add(t)
    if b.len == 0: # no type
        warning("deault type to `int`")
        result.tags = result.tags or TYINT
        return result
    if b.len == 1: # one type
        result.tags = result.tags or ( 
          case b[0]:
          of Kchar:
            TYCHAR
          of Kint:
            TYINT
          of Kvoid:
            TYVOID
          of Klong:
            TYLONG
          of Ksigned:
            TYINT
          of Kunsigned:
            TYUINT
          of Kshort:
            TYSHORT
          of Kdouble:
            TYDOUBLE
          of Kfloat:
            TYFLOAT
          of K_Bool:
            TYBOOL
          else:
            assert(false)
            TYINVALID
        )
        return result
    var signed = b.count(Ksigned)
    var unsigned = b.count(Kunsigned)
    let su = signed + unsigned
    if signed>1 or unsigned>1:
      type_error("too many `signed`/`unsigned`")
      return nil
    if su == 2:
     type_error("cannot both `signed` and `unsigned`")
     return nil
    let l = b.count(Klong)
    let s = b.count(Kshort)
    let f = b.count(Kfloat)
    let d = b.count(Kdouble)
    let i = b.count(Kint)
    let c = b.count(Kchar)
    let v = b.count(Kvoid)
    if c > 1:
        type_error("too many `char`s")
    if i > 1:
      type_error("too many `int`s")
      return nil
    if f > 0:
      type_error("`float` cannot combine with other types")
      return nil
    if v > 0:
      type_error("`void` cannot combine with other types")
      return nil
    if l >= 3:
      type_error("too many `long`s, max is `long long`")
      return nil
    if s >= 2:
      if s == 2:
        warning("duplicate 'short' declaration specifier")
      else:
        type_error("too many `short`s, max is `short`")
        return nil
    if d > 1:
       type_error("too many `double`s")
       return nil
    if bool(d):
      if d != 1:
        type_error("too many `long` for `double`")
        return nil
      if len(b) != (1 + l):
        type_error("extra `double` declaration specifier")
        return nil
      result.tags = result.tags or (if d==1: TYLONGDOUBLE else: TYDOUBLE)
      return result
    if bool(s): # short+int
      if (s + i + su) != len(b):
        type_error("extra `short` declaration specifier")
        return nil
      result.tags = result.tags or (if bool(unsigned): TYUSHORT else: TYSHORT)
      return result
    if bool(l): # long+int
      if (l + i + su) != len(b):
        type_error("extra `long` declaration specifier")
        return nil
      case l:
      of 1:
        result.tags = result.tags or (if bool(unsigned): TYULONG else: TYLONG)
      of 2:
        result.tags = result.tags or (if bool(unsigned): TYULONGLONG else: TYLONGLONG)
      else:
        assert(false)
      return result
    if bool(c):
      if (c + su) != len(b):
        type_error("extra `char` declaration specifier")
        return nil
      result.tags = result.tags or (if bool(unsigned): TYUCHAR else: TYCHAR)
      return result
    if bool(i):
      if (i + su) != len(b):
        type_error("extra `int` declaration specifier")
        return nil
      result.tags = result.tags or (if bool(unsigned): TYUINT else: TYINT)
      return result
    type_error("cannot combine types: " & $b)
    return nil

proc checkSemicolon() =
   if t.l.tok.tok != TSemicolon:
       warning("missing ';'")
   else:
       consume()

proc type_qualifier_list(ty: var CType) =
    # parse many type qualifiers, add to type
    while true:
        case t.l.tok.tok:
        of Kconst:
            ty.tags = ty.tags or TYCONST
            consume()
        of Krestrict:
            ty.tags = ty.tags or TYRESTRICT
            consume()
        of Kvolatile:
            ty.tags = ty.tags or TYVOLATILE
            consume()
        of K_Atomic:
            ty.tags = ty.tags or TYATOMIC
            consume()
        else:
            break

proc more(s: var seq[Token]) = 
    while t.l.tok.tok in declaration_specifier_set:
        s.add(t.l.tok.tok)
        consume()

proc read_enum_sepcs(c: var CType, sepc: Token) = 
    # TODO: ...
    discard

proc read_struct_union_sepcs(c: var CType, sepc: Token) = 
    # TODO: ...
    discard

proc handle_typedef(s: var seq[Token], ty: CType): CType =
    consume()
    while t.l.tok.tok in declaration_specifier_set:
        s.add(t.l.tok.tok)
        consume()
    result = deepCopy(ty)
    if len(s) > 0:
        for i in s:
            case i:
            of Ktypedef:
                return result
            else:
                if not addTag(result, i):
                    warning("typedef types cannot combine with type-specifier and type-qualifiers")
    result = deepCopy(ty)
    result.tags = ty.tags and (not TYTYPEDEF)
    return result

proc specifier_qualifier_list(): CType =
    # specfier-qualifier-list: parse many type specfiers and type qualifiers
    var s: seq[Token]
    while t.l.tok.tok in (type_specifier_set + type_qualifier_set + {Kstruct, Kenum, Kunion}):
        if t.l.tok.tok == Kenum:
            result = enum_decl()
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_enum_sepcs(result, i)
            return result
        elif t.l.tok.tok == Kunion or t.l.tok.tok == Kstruct:
            result = struct_union(t.l.tok.tok)
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_struct_union_sepcs(result, i)
            return result
        elif t.l.tok.tok == K_Atomic:
            consume()
            if t.l.tok.tok == TLbracket:
                consume()
                let ty = type_name()
                if ty[0] == nil:
                    expect("type-name")
                    return nil
                if t.l.tok.tok != TRbracket:
                    expectRB()
                    return nil
                consume()
                more(s)
                if len(s) > 0:
                    warning("atomic-type-specifier cannot combine with other types")
                return ty[0]
            s.add(K_Atomic)
        else:
            s.add(t.l.tok.tok)
            consume()
    if t.l.tok.tok == TIdentifier:
        let o = gettypedef(t.l.tok.s)
        if o != nil and (o.ty.tags and TYTYPEDEF) != 0:
            more(s)
            return handle_typedef(s, o.ty)
    if s.len > 0:
        return merge_types(s)
    return nil

proc declarator(base: CType; flags=Direct): Stmt =
    ## take a base type, return the final type and name
    ##
    ## for example: `static   int     foo`
    ##
    ##                base-type    decorator
    var ty = base
    while t.l.tok.tok == TMul:
        consume()
        ty = getPointerType(ty)
        type_qualifier_list(ty)
    return direct_declarator(ty, flags)

proc warn_if_bad_cast(e: Expr, ty: CType, msg: string): bool =
    if e.ty.spec == TYPOINTER and ty.spec != TYPOINTER:
        warning(msg & " makes pointer from integer without a cast")
        result = true
    elif e.ty.spec != TYPOINTER and ty.spec == TYPOINTER:
        warning(msg & " makes integer from pointer without a cast")
        result = true

proc initializer_list(): Expr =
    if t.l.tok.tok != TLcurlyBracket:
        if t.sema.currentInitTy == nil:
            # when 'excess elements in initializer-list', 't.sema.currentInitTy' is nil
            return assignment_expression()
        if t.sema.currentInitTy.spec notin {TYPRIM, TYPOINTER, TYENUM}:
            type_error("expect bracket initializer")
            return nil
        var e = assignment_expression()
        if e == nil:
            expectExpression()
            return nil
        discard warn_if_bad_cast(e, t.sema.currentInitTy, "initialization of '" & $t.sema.currentInitTy & "' from '" & $e.ty & '\'')
        return castto(e, t.sema.currentInitTy)
    if t.sema.currentInitTy.spec == TYSTRUCT:
        result = Expr(k: EStruct, ty: t.sema.currentInitTy)
    else:
        # array, scalar
        result = Expr(k: EArray, ty: t.sema.currentInitTy)
    result.loc = getLoc()
    consume()
    var m: int
    if t.sema.currentInitTy.spec == TYARRAY:
        if t.sema.currentInitTy.hassize:
            m = t.sema.currentInitTy.arrsize.int
        else:
            result.ty.hassize = true
            result.ty.arrsize = 0
            m = -1
    elif t.sema.currentInitTy.spec == TYSTRUCT:
        m = t.sema.currentInitTy.selems.len.int
    else:
        # warning("braces around scalar initializer")
        m = 1
    var i = 0
    while true:
        if t.l.tok.tok == TRcurlyBracket:
            consume()
            break
        var ty: CType
        if t.sema.currentInitTy.spec == TYSTRUCT:
            ty = if i < m: t.sema.currentInitTy.selems[i][1] else: nil
        elif t.sema.currentInitTy.spec == TYARRAY:
            ty = t.sema.currentInitTy.arrtype
        else:
            ty = t.sema.currentInitTy
        var o = t.sema.currentInitTy
        t.sema.currentInitTy = ty
        var e = initializer_list()
        t.sema.currentInitTy = o
        if e == nil:
            return nil
        if m == -1:
            result.arr.add(e)
            inc result.ty.arrsize
        elif i < m:
            result.arr.add(e)
        else:
            warning("excess elements in initializer-list")
        if t.l.tok.tok == TComma:
            consume()
        inc i
    if t.sema.currentInitTy.spec in {TYPRIM, TYPOINTER, TYENUM}:
        result = result.arr[0]

proc direct_declarator_end(base: CType, name: string): Stmt

proc direct_declarator(base: CType; flags=Direct): Stmt =
    case t.l.tok.tok:
    of TIdentifier:
        if flags == Abstract:
            return nil
        var name = t.l.tok.s
        consume()
        return direct_declarator_end(base, name)
    of TLbracket:
        consume()
        let st = declarator(base, flags)
        if st == nil:
            expect("declarator")
            return nil
        if t.l.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
        if st.k == SFunction:
            return direct_declarator_end(st.functy, st.funcname)
        else:
            return direct_declarator_end(st.var1type, st.var1name)
    else:
        if flags != Direct:
            return direct_declarator_end(base, "")
        return nil

proc direct_declarator_end(base: CType, name: string): Stmt =
    var loc = getLoc()
    case t.l.tok.tok:
    of TLSquareBrackets: # int arr[5], int arr[], int arr[static 5], int arr[static const 5], int arr[const static 5], int arr2[static const restrict 5]
        consume() # eat ]
        var ty = CType(tags: TYINVALID, spec: TYARRAY, arrsize: 0, arrtype: base, hassize: false)
        if t.l.tok.tok == TMul: # int arr[]
          consume()
          if t.l.tok.tok != TRSquareBrackets:
            parse_error("expect ']'")
            note("the syntax is:\n\tint arr[]")
            return nil
          # only allowed at extern variable and function prototype scope!
          return Stmt(k: SVarDecl1, var1name: name, var1type: ty)
        if t.l.tok.tok == Kstatic:
           consume()
           type_qualifier_list(ty)
        else:
            type_qualifier_list(ty)
            if t.l.tok.tok == Kstatic:
                consume()
        if t.l.tok.tok != TRSquareBrackets:
            let e = assignment_expression()
            if e == nil:
                expectExpression()
                return nil
            if not checkInteger(e.ty):
                type_error("size of array has non-integer type '" & $e.ty & '\'')
            ty.hassize = true
            var o = t.pp.eval_error
            ty.arrsize = evali(e)
            if t.pp.eval_error:
                t.pp.eval_error = o
                # VLA
                ty.vla = e
                ty.arrsize = 0
            if t.l.tok.tok != TRSquareBrackets:
               parse_error("expect ']'")
               return nil
        consume() # eat ]
        return direct_declarator_end(ty, name)
    of TLbracket:
        consume()
        if base.spec == TYARRAY:
            type_error("function cannot return array")
        if (base.tags and (TYREGISTER)) != 0:
            warning("'register' in function has no effect")
        if (base.tags and (TYTHREAD_LOCAL)) != 0:
            warning("'_Thread_local' in function has no effect")
        if bool(base.tags and (TYCONST or TYRESTRICT or TYVOLATILE)):
            warning("type qualifiers ignored in function")
        var ty = CType(tags: TYINVALID, spec: TYFUNCTION, ret: base)
        if t.l.tok.tok != TRbracket:
            let res = parameter_type_list()
            if res[0]:
                ty.params = res[1]
            else:
                return nil
            if t.l.tok.tok != TRbracket:
                expectRB()
                return nil
        else:
            ty.params.add(("", nil))
        consume()
        if t.l.tok.tok == TLcurlyBracket: # function definition
            putsymtype2(name, ty)
            t.sema.pfunc = name
            if not isTopLevel():
                parse_error("function definition is not allowed here")
                note("function can only declared in global scope")
            var body: Stmt
            var labels = initHashSet[string]()
            block:
                var oldRet = t.sema.currentfunctionRet
                t.sema.currentfunctionRet = ty.ret
                body = compound_statement(ty.params, labels)
                t.sema.currentfunctionRet = oldRet
            if body == nil:
                expect("function body")
                return nil
            if name == "main":
                if (ty.ret.tags and TYINT) == 0:
                    warning("main should return 'int'")
                if ty.params.len >= 1:
                    if ty.params[0][1] != nil and (ty.params[0][1].tags and TYINT) == 0:
                        warning("first parameter of 'main' should be of type 'int'")
                    if ty.params.len >= 2:
                        if ty.params[1][1] != nil and (not (ty.params[1][1].spec == TYPOINTER and ty.params[1][1].p.spec == TYPOINTER)):
                            warning("second parameter of 'main' is not 'char**'")
            if len(body.stmts) == 0 or (body.stmts[^1].k != SReturn and body.stmts[^1].k != SGoto):
                body.stmts &= Stmt(k: SReturn, exprbody: if bool(ty.ret.tags and TYVOID): nil else: 
                    (if name == "main": Expr(k: EDefault, ty: ty.ret, loc: getLoc()) else: Expr(k: EUndef, ty: ty.ret, loc: getLoc()))
                )
            return Stmt(k: SFunction, funcname: name, functy: ty, funcbody: body, labels: labels, loc: loc)
        return direct_declarator_end(ty, name)
    else:
        return Stmt(k: SVarDecl1, var1name: name, var1type: base, loc: loc)

proc struct_declarator(base: CType): (string, CType) =
    if t.l.tok.tok == TColon:
        consume()
        let e = constant_expression()
        if e == nil:
            expectExpression()
            note("in bit field declaration")
            return ("", nil)
        let bitsize = evali(e)
        return ("", CType(tags: TYINVALID, spec: TYBITFIELD, bittype: base, bitsize: bitsize))
    else:
        let d = declarator(base, Direct)
        if d == nil:
            return ("", nil)
        if t.l.tok.tok == TColon:
            consume()
            let e = constant_expression()
            if e == nil:
                expectExpression()
                note("in bit field declaration")
                return
            let bitsize = evali(e)
            return ("", CType(tags: TYINVALID, spec: TYBITFIELD, bittype: if d.k == SFunction: d.functy else: d.var1type, bitsize: bitsize))
        if d.k == SFunction:
            return (d.funcname, d.functy)
        return (d.var1name, d.var1type)

proc struct_union(tok: Token): CType =
    ## parse a struct or union, return it
    ## for example:  `struct Foo`
    ##
    ##               `struct { ... }`
    ##
    ##               `struct Foo { ... }`
    consume() # eat struct/union
    var name = ""
    if t.l.tok.tok == TIdentifier:
        name = t.l.tok.s
        consume()
        if t.l.tok.tok != TLcurlyBracket: # struct Foo
           return if tok==Kstruct: getstructdef(name) else: getuniondef(name)
    elif t.l.tok.tok != TLcurlyBracket:
        parse_error("expect '{' for start anonymous struct/union")
        return nil
    if tok == Kstruct:
        result = CType(tags: TYINVALID, spec: TYSTRUCT, sname: name)
    else:
        result = CType(tags: TYINVALID, spec: TYUNION, sname: name)
    consume()
    var names: HashSet[string] = initHashSet[string]()
    if t.l.tok.tok != TRcurlyBracket:
        while true:
            if t.l.tok.tok == K_Static_assert:
                let s = static_assert()
                if s == nil:
                    return nil
                if t.l.tok.tok == TRcurlyBracket:
                    break
                continue
            var base = specifier_qualifier_list()
            if base == nil:
                expect("specifier-qualifier-list")
                return nil

            if t.l.tok.tok == TSemicolon:
                warning("struct/union member declaration doest not declare anything")
                consume()
                continue
            else:
                while true:
                    let e = struct_declarator(base)
                    if e[1] == nil:
                        parse_error("expect struct-declarator")
                        return nil
                    if e[0] in names:
                        type_error("duplicate member " & e[0])
                        note("in the declaration of struct/union '" & result.sname & '\'')
                    names.incl(e[0])
                    result.selems.add(e)
                    if t.l.tok.tok == TComma:
                        consume()
                    else:
                        checkSemicolon()
                        break
                if t.l.tok.tok == TRcurlyBracket:
                    consume()
                    break
    else:
        consume()
    if len(result.sname) > 0:
        if tok == Kstruct:
            putstructdef(result)
        else:
            putuniondef(result)
    return result

proc enum_decl(): CType =
    ## parse a enum, return it
    ## for example: 
    ## 
    ##      `enum State`
    ##
    ##      `enum { ... }`
    ##
    ##      `enum State { ... }`
    consume() # eat enum
    var name = ""
    if t.l.tok.tok == TIdentifier:
        name = t.l.tok.s
        consume()
        if t.l.tok.tok != TLcurlyBracket: # struct Foo
          return getenumdef(name)
    elif t.l.tok.tok != TLcurlyBracket:
        parse_error("expect '{' for start anonymous enum")
        return nil
    result = CType(tags: TYINVALID, spec: TYENUM, ename: name)
    consume()
    var c: uintmax_t = 0
    while true:
        if t.l.tok.tok != TIdentifier:
            break # enum {A, } is ok !
        var s = t.l.tok.s # copy
        consume()
        if t.l.tok.tok == TAssign:
            consume()
            var e = constant_expression()
            c = evali(e)
            if err():
                return nil
        putenum(s, c)
        result.enumerators.add((s, c))
        inc c
        if t.l.tok.tok == TComma:
            consume()
        else:
            break
    if t.l.tok.tok != TRcurlyBracket:
        parse_error("expect '}'")
    consume()
    if len(result.ename) > 0:
        putenumdef(result)
    return result

proc parameter_type_list(): (bool, seq[(string, CType)]) =
    enterBlock() # function prototype scope
    result = (true, default(typeof(result[1])))
    var i = 0
    while true:
        inc i
        if not istype(t.l.tok.tok):
            type_error("expect a type")
            return (false, default(typeof(result[1])))
        var base = declaration_specifiers()
        if base == nil:
            return (false, default(typeof(result[1])))
        let res = abstract_decorator(base, Function)
        if res == nil:
            return (false, default(typeof(result[1])))
        if res.k == SFunction:
            result[1].add((res.funcname, res.functy))
        else:
            result[1].add((res.var1name, res.var1type))
        if result[1][^1][1].spec == TYINCOMPLETE:
            type_error("parameter " & $i & " has imcomplete type '" & $result[1][^1][1] & '\'')
        result[1][^1][1].tags = result[1][^1][1].tags or TYPARAM
        t.sema.scopes[^1].typedefs[result[1][^1][0]] = Info(ty: result[1][^1][1], tag: INFO_USED)
        if t.l.tok.tok == TRbracket:
            break
        if t.l.tok.tok == TComma:
            consume()
        if t.l.tok.tok == TEllipsis:
            result[1].add(("", nil))
            consume()
            break
        elif t.l.tok.tok == TRbracket:
            break
    var zero = false
    for i in 0..<len(result[1]):
        if i == 0 and bool(result[1][0][1].tags and TYVOID):
            zero = true
            discard
        if result[1][i][1] == nil:
            break
        if result[1][i][1].spec == TYINCOMPLETE:
            error_incomplete(result[1][i][1])
            return (false, default(typeof(result[1])))
        if result[1][i][1].spec == TYARRAY:
            result[1][i][1] = getPointerType(result[1][i][1].arrtype)
    if zero:
        if result[1].len > 1:
            warning("'void' must be the only parameter")
        result[1].setLen 0
    leaveBlock()

proc checkAlign(a: uint32) =
    if (a and (a - 1)) != 0:
        type_error("requested alignment is not a power of 2")

proc parse_alignas(): bool =
    consume()
    if t.l.tok.tok != TLbracket:
        expectLB()
        return false
    consume()
    if istype(t.l.tok.tok):
        let (ty, isf) = type_name()
        discard isf
        var a = uint32(getAlignof(ty))
        if a == 0:
            type_error("zero alignment is not valid")
        else:
            checkAlign(a)
            t.sema.currentAlign = a
    else:
        let e = expression()
        if e == nil:
            expectExpression()
            return false
        var a = evali(e)
        if a <= 0:
            type_error("alignment " & $a &  " too small")
        else:
            checkAlign(uint32(a))
            t.sema.currentAlign = uint32(a)
    if t.l.tok.tok != TRbracket:
        expectRB()
        return false
    consume()
    return true

proc declaration_specifiers(): CType =
    # declaration-specifiers: parse a declaration-specifiers(e.g: int a, *b, c[10];)
    var s: seq[Token]
    var should_return = false
    while t.l.tok.tok in (declaration_specifier_set + {Kstruct, Kenum, Kunion, K_Alignas}):
        if t.l.tok.tok == Kenum:
            result = enum_decl()
            should_return = true
        elif t.l.tok.tok == Kunion or t.l.tok.tok == Kstruct:
            result = struct_union(t.l.tok.tok)
            should_return = true
        elif t.l.tok.tok == K_Atomic:
            consume()
            if t.l.tok.tok == TLbracket:
                consume()
                let ty = type_name()
                if ty[0] == nil:
                    expect("type-name")
                    return nil
                if t.l.tok.tok != TRbracket:
                    expectRB()
                    return nil
                consume()
                more(s)
                if len(s) > 0:
                    warning("atomic-type-specifier cannot combine with other types")
                return ty[0]
            s.add(K_Atomic)
        elif t.l.tok.tok == K_Alignas:
            if not parse_alignas():
                return nil
        else:
            s.add(t.l.tok.tok)
            consume()
    if should_return:
        for tok in s:
            discard addTag(result, tok)
        return result
    if t.l.tok.tok == TIdentifier:
        let o = gettypedef(t.l.tok.s)
        if o != nil and (o.ty.tags and TYTYPEDEF) != 0:
            more(s)
            return handle_typedef(s, o.ty)
    if s.len > 0:
        return merge_types(s)
    warning("type defaults to 'int' in declaration")
    return tycache.iintty

proc parse_asm(): Stmt =
   consume() # eat asm
   if t.l.tok.tok != TLbracket:
       expectLB()
       return nil
   consume() # eat '('
   if t.l.tok.tok != TStringLit:
       expect("string literal")
       return nil
   result = Stmt(k: SAsm, asms: t.l.tok.str)
   consume() # eat string
   if t.l.tok.tok != TRbracket:
       expectRB()
       return nil
   consume() # eat ')'

proc static_assert(): Stmt =
    consume()
    if t.l.tok.tok != TLbracket:
        expectLB()
        return nil
    consume()
    var msg = ""
    let e = constant_expression()
    if e == nil:
        return nil
    let ok = evali(e) != 0
    if t.pp.eval_error:
        write_eval_msg()
    if t.l.tok.tok == TRbracket: # no message
        if ok == false:
            error("static assert failed!")
        consume()
    elif t.l.tok.tok == TComma:
        consume()
        if t.l.tok.tok != TStringLit:
            expect("string literal in static assert")
            return nil
        if ok == false:
            error(t.l.tok.str)
            msg = t.l.tok.str
        consume()
        if t.l.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
    else:
        expect("',' or ')'")
        return nil
    checkSemicolon()
    return Stmt(k: SSemicolon)

proc assignable(e: Expr): bool =
    if e.ty.spec == TYPOINTER:
        if bool(e.ty.tags and TYLVALUE) and e.ty.p.spec == TYARRAY:
            type_error("array is not assignable")
            return false
        return true
    if bool(e.ty.tags and TYLVALUE):
        return true
    type_error("expression is not assignable")
    inTheExpression(e)
    return false

proc declaration(): Stmt =
    ## parse declaration or function definition
    if t.l.tok.tok == K_Static_assert:
        return static_assert()
    elif t.l.tok.tok == TSemicolon:
        consume()
        return Stmt(k: SSemicolon)
    t.sema.currentAlign = 0
    var base = declaration_specifiers()
    if base == nil:
        expect("declaration-specifiers")
        return nil
    if t.sema.currentAlign > 0:
        var m = getAlignof(base)
        if t.sema.currentAlign < m:
            type_error("requested alignment is less than minimum alignment of " & $m & " for type '" & $base & "'")
        else:
            base.align = t.sema.currentAlign
    if t.l.tok.tok == TSemicolon:
        consume()
        if base.align != 0:
            type_error("'_Alignas' can only used in variables")
        return Stmt(k: SDeclOnly, decl: base)
    result = Stmt(k: SVarDecl)
    while true:
        var st = declarator(base)
        if st == nil:
            expect("declarator")
            note("maybe you missing ';' after declarator")
            return nil
        if st.k == SFunction:
            return st
        noralizeType(st.var1type)
        st.var1type.tags = st.var1type.tags or TYLVALUE
        putsymtype(st.var1name, st.var1type)
        result.vars.add((st.var1name, st.var1type, nil))
        if (st.var1type.tags and TYINLINE) != 0:
            warning("inline can only used in function declaration")
        if (st.var1type.tags and TYEXTERN) == 0:
            if bool(st.var1type.tags and TYVOID) and (st.var1type.tags and TYTYPEDEF) == 0:
                type_error("variable '" & st.var1name & "' declared void")
                return nil
            if st.var1type.spec == TYINCOMPLETE:
                echo st.var1type
                error_incomplete(st.var1type)
                return nil
        if t.l.tok.tok == TAssign:
            if st.var1type.spec == TYARRAY:
                if st.var1type.vla != nil:
                    type_error("variable length array may not be initialized")
            elif st.var1type.spec == TYFUNCTION:
                type_error("function may not be initialized")
                return nil
            if bool(st.var1type.tags and TYTYPEDEF):
                type_error("'typedef' may not be initialized")
                return nil
            if bool(st.var1type.tags and TYEXTERN):
                type_error("'extern' variables may not be initialized")
                if not isTopLevel():
                    note("place initializer after 'extern' declaration to fix this\nint foo(){\n\textern int a;\n\ta = 0;")
            consume()
            var old = t.sema.currentInitTy
            t.sema.currentInitTy = st.var1type
            let init = initializer_list()
            t.sema.currentInitTy = old
            if init == nil:
                expect("initializer-list")
                return nil
            result.vars[^1][2] = init
            if isTopLevel() and isConstant(result.vars[^1][2]) == false:
                type_error("initializer element is not constant")
                note("global variable requires constant initializer")
        else:
            if st.var1type.spec == TYARRAY:
                if st.var1type.vla != nil and isTopLevel():
                    type_error("variable length array declaration not allowed at file scope")
                    note("in the declaration of variable: '" & st.var1name & '\'')
                elif st.var1type.hassize == false and st.var1type.vla == nil and (st.var1type.tags and TYEXTERN) == 0:
                    if isTopLevel():
                        warning("array '" & st.var1name & "' assumed to have one element")
                        st.var1type.hassize = true
                        st.var1type.arrsize = 1
                    else:
                        type_error("array size missing in ‘" & st.var1name & "’")
        if t.l.tok.tok == TComma:
            consume()
        elif t.l.tok.tok == TSemicolon:
            consume()
            break

proc cast_expression(): Expr =
    case t.l.tok.tok:
    of TLbracket:
        consume()
        if istype(t.l.tok.tok):
            var (n, isf) = type_name()
            discard isf
            if n == nil:
                return nil
            n.tags = n.tags or TYLVALUE
            if t.l.tok.tok != TRbracket:
                expect("`)`")
                return nil
            consume()
            if t.l.tok.tok == TLcurlyBracket:
                block:
                    var old = t.sema.currentInitTy
                    t.sema.currentInitTy = n
                    result = initializer_list()
                    t.sema.currentInitTy = old
                return result
            let e = cast_expression()
            if e == nil:
                return nil
            if (n.tags and TYVOID) != 0:
                # TODO: add void tag!
                return Expr(k: EVoid, voidexpr: e, ty: tycache.v)
            return castto(e, n)
        putToken()
        t.l.tok = TokenV(tok: TLbracket, tags: TVNormal)
    else:
        discard
    return unary_expression()

proc type_name(): (CType, bool) =
    ## parse a type-name
    ##
    ## for example:
    ##
    ##    sizeof(type-name)
    let base = declaration_specifiers()
    if base == nil:
        (nil, false)
    elif t.l.tok.tok == TRbracket:
        (base, false)
    else:
        let st = abstract_decorator(base, Abstract)
        if st == nil:
            (nil, false)
        elif st.k == SFunction:
            (st.functy, true)
        else:
            (st.var1type, false)

proc getsizeof2(e: Expr, loc: Location): Expr =
    if e.k == ArrToAddress:
        if e.voidexpr.ty.vla != nil:
            return Expr(ty: tycache.isize_tty, k: EVLAGetSize, vla: e, loc: loc)
        else:
            return Expr(ty: tycache.isize_tty, k: EIntLit, ival: cast[uintmax_t](getsizeof(e.voidexpr.ty)), loc: loc)
    if e.k == EUnary and e.uop == AddressOf and e.ty.p.spec == TYFUNCTION:
        return Expr(ty: tycache.isize_tty, k: EIntLit, ival: 1, loc: loc)
    else:
        return Expr(ty: tycache.isize_tty, k: EIntLit, ival: cast[uintmax_t](getsizeof(e)), loc: loc)

proc unary_expression(): Expr =
    let tok = t.l.tok.tok
    case tok:
    of TNot:
        consume()
        let e = cast_expression()
        if e == nil:
            return nil
        if not checkScalar(e.ty):
            type_error("scalar expected")
            return nil
        return boolToInt(unary(e, LogicalNot, tycache.iintty))
    of TMul:
        consume()
        var e = cast_expression()
        if e.ty.spec != TYPOINTER:
            type_error("pointer expected")
            return nil
        var ty = deepCopy(e.ty.p)
        ty.tags = ty.tags or TYLVALUE
        return unary(e, Dereference, ty)
    of TBitNot:
        consume()
        var e = cast_expression()
        if e == nil:
            return nil
        if not checkInteger(e.ty):
            type_error("integer type expected")
            return nil
        result = unary(e, Not, e.ty)
        integer_promotions(result)
        return result
    of TBitAnd:
        consume()
        var e = expression()
        if e == nil:
            return nil
        if e.k == EString:
            return e
        if e.k == ArrToAddress:
            e.ty = getPointerType(e.voidexpr.ty)
            return e
        if (e.ty.tags and TYLVALUE) == 0:
            type_error("cannot take the address of an rvalue")
            inTheExpression(e)
            return nil
        if (e.k == EUnary and e.uop == AddressOf) and e.ty.p.spec == TYFUNCTION:
            e.ty.tags = TYINVALID
            return e
        return unary(e, AddressOf, getPointerType(e.ty))
    of TDash:
        consume()
        let e = unary_expression()
        if e == nil:
            return nil
        if not checkArithmetic(e.ty):
            type_error("arithmetic type expected")
            return nil
        result = unary(e, if isSigned(e.ty): SNeg else: UNeg, e.ty)
        integer_promotions(result)
        return result
    of TAdd:
        consume()
        let e = unary_expression()
        if e == nil:
            return nil
        if not checkArithmetic(e.ty):
            type_error("arithmetic type expected")
            return nil
        result = unary(e, Pos, e.ty)
        integer_promotions(result)
        return result
    of TAddAdd, TSubSub:
        consume()
        let e = unary_expression()
        if e == nil:
            return nil
        if not checkScalar(e.ty):
            type_error("scalar expected")
            return nil
        return unary(e, if tok == TAddAdd: PrefixIncrement else: PrefixDecrement, e.ty)
    of Ksizeof:
        var loc = getLoc()
        consume()
        if t.l.tok.tok == TLbracket:
            consume()
            if istype(t.l.tok.tok):
                let ty = type_name()
                if ty[0] == nil:
                    expect("type-name or expression")
                    return nil
                if ty[1] == true:
                    type_error("invalid application of 'sizeof' to a function type")
                if t.l.tok.tok != TRbracket:
                    expectRB()
                    return nil
                consume()
                return Expr(ty: tycache.isize_tty, k: EIntLit, ival: cast[uintmax_t](getsizeof(ty[0])), loc: getLoc())
            let e = unary_expression()
            if e == nil:
                expectExpression()
                return nil
            if t.l.tok.tok != TRbracket:
                expectRB()
                return nil
            consume()
            return getsizeof2(e, loc)
        else:
            let e = unary_expression()
            if e == nil:
                expectExpression()
                return nil
            return getsizeof2(e, loc)
    of K_Alignof:
        var loc = getLoc()
        consume()
        if t.l.tok.tok != TLbracket:
            expect("(")
            return nil
        consume()
        if istype(t.l.tok.tok):
            let ty = type_name()
            if ty[0] == nil:
                expect("type-name")
                return nil
            if ty[1] == true:
                type_error("invalid application of '_Alignof' to a function type")
            result = Expr(ty: tycache.isize_tty, k: EIntLit, ival: cast[uintmax_t](getAlignof(ty[0])), loc: loc)
        else:
            let e = constant_expression()
            if e == nil:
                expectExpression()
                return nil
            result = Expr(ty: tycache.isize_tty, k: EIntLit, ival: cast[uintmax_t](getAlignof(e)), loc: loc)
        if t.l.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
        return result
    else:
        return postfix_expression()

proc read_pp_float(s: string, o: var float, c: int, base: uintmax_t, init = 0.0): int =
    var f = init
    var i = c
    if len(s) == c:
        parse_error("expect more digits after '.'")
        return 0
    var e = 1.0
    while true:
        let c = s[i]
        if base == 16:
            e /= 16.0
            if c in {'0'..'9'}:
                f += float(uintmax_t(c) - '0'.uintmax_t) * e
            elif c in {'a'..'f'}:
                f += float(uintmax_t(c) - 'a'.uintmax_t + 10) * e
            elif c in {'A'..'F'}:
                f += float(uintmax_t(c) - 'A'.uintmax_t + 10) * e
            else:
                break
        else:
            if c notin {'0'..'9'}:
                break
            e /= 10.0
            f += float(uintmax_t(c) - '0'.uintmax_t) * e
        inc i
        if i == len(s):
            o = f
            return 2
    if s[i] in {'P', 'p', 'E', 'e'}:
        inc i
        if i == len(s):
            parse_error("expect exponents")
            return 0        
        var negate = false
        if s[i] == '-':
            negate = true
            inc i
            if i == len(s):
                parse_error("expect exponent digits")
                return 0
        elif s[i] == '+':
            inc i
            if i == len(s):
                parse_error("expect exponent digits")
                return 0
        elif s[i] notin {'0'..'9'}:
            parse_error("expect exponent digits")
            return 0
        var exps: string
        while true:
            exps.add(s[i])
            inc i
            if i == len(s):
                break
            if s[i] notin {'0'..'9'}:
                break
        let powerby = decimaltoInt(exps)
        if base == 10:
            let fr = pow(10.0, float(powerby))
            if negate:
                f /= fr
            else:
                f = fr
        else:
            f = pow(2, if negate: -float(powerby) else: float(powerby))
    result = 2
    if i != len(s):
        if s[i] notin {'f', 'F', 'L', 'l'}:
            parse_error("invalid float suffix: " & s[i])
            return 0
        if s[i] == 'F' or s[i] == 'f':
            result = 3
    o = f
    return result

# attempt to paser pp-number to int
# return value
#    0: <failed>
#    1: int
#    2: double
#    3: float
#    4: long
#    5: long long
#    6: unsigned long
#    7: unsigned long long
#    8: unsigned int

proc read_pp_number(s: string, f: var float, n: var uintmax_t): int =
    var base = uintmax_t(10)
    var i = 0
    if len(s) == 0:
        return 0
    if s[0] == '0':
        if len(s) == 1:
            n = 0
            return 1
        case s[1]:
        of 'b', 'B': 
            base = 2
            i = 2
        of 'x', 'X':
            base = 16
            i = 2
        of '0' .. '7':
            i = 2
            var num = uintmax_t(s[1]) - uintmax_t('0')
            while true:
                if i == len(s):
                    n = num
                    return 1
                if s[i] notin {'0'..'7'}:
                    parse_error("bad octal literal")
                    return 0
                num = (num shl 3) or (uintmax_t(s[i]) - uintmax_t('0'))
                inc i
        of '.':
            return read_pp_float(s, f, c=2, base=10)
        else:
            parse_error("invalid number prefix: " & s[1])
            return 0
    elif s[0] == '.':
        return read_pp_float(s, f, c=1, base=10)
    elif s[0] notin {'1'..'9'}:
        parse_error("invalid number")
        return 0
    var digits: string
    while true:
        if i == s.len:
          break
        if base == 10:
            if s[i] notin {'0'..'9'}:
                break
        elif base == 2:
            if s[i] notin {'0', '1'}:
                break
        else: # base == 16
            if s[i] notin {'0'..'9', 'A'..'F', 'a'..'f'}:
                break
        digits.add(s[i])
        inc i
    var num = if base == 10: decimaltoInt(digits) else: (if base==2: decimal2toInt(digits) else: decimal16toInt(digits))
    if i == len(s):
      n = num
      return 1
    elif base == 2:
        parse_error("invalid binary number suffix: " & s[i])
    if s[i] == '.':
        return read_pp_float(s, f, i+1, base=base, init=float(num))
    result = 1
    case s[i]:
    of 'E', 'e', 'P', 'p':
        result = 2
        inc i
        if i == len(s):
            parse_error("expect exponents")
            return 0
        var negate = false
        if s[i] == '-':
            negate = true
            inc i
            if i == len(s):
                parse_error("expect exponent digits")
                return 0
        elif s[i] == '+':
            inc i
            if i == len(s):
                parse_error("expect exponent digits")
                return 0
        elif s[i] notin {'0'..'9'}:
            parse_error("expect exponent digits")
            return 0
        var exps: string
        while true:
            exps.add(s[i])
            inc i
            if i == len(s):
                break
            if s[i] notin {'0'..'9'}:
                break
        let powerby = decimaltoInt(exps)
        if base == 10:
            var pw = pow10(powerby)
            if negate:
                f = float(num) / float(pw)
            else:
                f = float(num) * float(pw)
        else:
            f = float(num) * pow(2, if negate: -float(powerby) else: float(powerby))
    else:
        discard
    while true:
        if i == len(s):
            break
        case s[i]:
        of 'F', 'f':
            if not (result == 2 or result == 3):
                parse_error("invalid integer suffix " & s[i])
                return 0
            result = 3
            break
        of 'L', 'l':
            if result == 2 or result == 3:
                break
            case result:
            of 1: # int => long
                result = 4
            of 4: # long => long long
                result = 5
            of 8: # unsigned int => unsigned long
                result = 6
            of 6: # unsigned long => unsigned long long
                result = 7
            else:
                parse_error("double 'L' suffix in integer constant")
                return 0
        of 'U', 'u':
            if result == 2 or result == 3:
                parse_error("invalid float suffix " & show(s[i]))
                return 0
            case result:
            of 1:
                result = 8
            of 4:
                result = 6
            of 5:
                result = 7
            else:
                parse_error("double 'U' suffix in integer constant")
                return
        else:
            if result == 2 or result == 3:
                parse_error("invalid float suffix: " & show(s[i]))
            else:
                parse_error("invalid integer suffix: " & show(s[i]))
            return 0
        inc i
    if result == 3 or result == 2:
        discard
    else:
        n = num
    return result

proc fixName(v: string) =
    var fixList = initHeapQueue[FixName]()
    for i in countdown(len(t.sema.scopes)-1, 0):
        for name in t.sema.scopes[i].typedefs.keys():
            var d = editDistance(name, v)
            fixList.push(FixName(priority: d, name: name))
    var msg = "\n"
    while len(fixList) > 0:
        var f = fixList.pop()
        if f.priority < 3:
            msg.add("Perhaps you meant: " & f.name & "\n")
    if msg.len > 1:
        note(msg)

proc primary_expression(): Expr =
    ## primary expressions:
    ##      constant
    ##      `(` expression `)`
    ##      identfier
    var loc = getLoc()
    case t.l.tok.tok:
    of TCharLit:
        var ty: CType = nil
        case t.l.tok.itag:
        of Iint:
            # Iint: 'c'
            ty = tycache.iintty
        of Ilong:
            # Ilong: u'c'
            ty = tycache.u16
        of Iulong:
            # Iulong: U'c'
            ty = tycache.u32
        of Ilonglong:
            # Ilonglong: L'c'
            ty = tycache.iwcharty
        else:
            unreachable()
        result = Expr(k: EIntLit, ival: t.l.tok.i, ty: ty, loc: loc)
        consume()
    of TNumberLit:
        var ty: CType = nil
        case t.l.tok.itag:
        of Iint:
            ty = tycache.iintty
        of Ilong:
            ty = tycache.ilongty
        of Iulong:
            ty = tycache.ulongty
        of Ilonglong:
            ty = tycache.ilonglongty
        of Iulonglong:
            ty = tycache.ulonglongty
        of Iuint:
            ty = tycache.uintty
        result = Expr(k: EIntLit, ival: t.l.tok.i, ty: ty, loc: loc)
        consume()
    of TFloatLit:
        result = Expr(k: EFloatLit, fval: t.l.tok.f, ty: if t.l.tok.ftag == Ffloat: tycache.ffloatty else: tycache.fdoublety, loc: loc)
        consume()
    of TStringLit:
        var s: string
        var enc = t.l.tok.enc
        while true:
            s.add(t.l.tok.str)
            consume()
            if t.l.tok.tok != TStringLit:
                break
            if t.l.tok.enc != enc:
                type_error("unsupported non-standard concatenation of string literals")
                note("concatenation UTF-" & $enc & " and UTF-" & $t.l.tok.enc)
                return nil
        case enc:
        of 8:
            let ty = tycache.strty
            result = Expr(
                k: ArrToAddress, voidexpr: Expr(k: EString, ty: ty, str: s, is_constant: true, loc: loc),
                ty: ty
            )
        of 16:
            var loc = getLoc()
            var a: seq[Expr]
            let ty = tycache.ushortty
            for i in writeUTF8toUTF16(s):
                a.add(Expr(k: EIntLit, ival: cast[uintmax_t](i), ty: ty, loc: loc))
            result = Expr(
                k: ArrToAddress, voidexpr: Expr(k: EArray, ty: ty, arr: a, loc: loc), 
                ty: getPointerType(ty)
            )
        of 32:
            var a: seq[Expr]
            let ty = tycache.u32
            for i in writeUTF8toUTF32(s):
                a.add(Expr(k: EIntLit, ival: uintmax_t(i), ty: ty, loc: loc))
            result = Expr(
                k: ArrToAddress, voidexpr: Expr(k: EArray, ty: ty, arr: a), 
                ty: getPointerType(ty), loc: getLoc()
            )
        else:
            unreachable()
        result.loc = loc
    of TPPNumber:
        var f: float
        var n: uintmax_t
        let ok = read_pp_number(t.l.tok.s, f, n)
        case ok:
        of 0:
            return nil
        of 1:
            result = Expr(ty: tycache.iintty, k: EIntLit, ival: n)
        of 2, 3:
            result = Expr(ty: if ok == 2: tycache.fdoublety else: tycache.ffloatty, k: EFloatLit, fval: f)
        of 4:
            result = Expr(ty: tycache.ilongty, k: EIntLit, ival: n)
        of 5:
            result = Expr(ty: tycache.ulongty, k: EIntLit, ival: n)
        of 6:
            result = Expr(ty: tycache.ilonglongty, k: EIntLit, ival: n)
        of 7:
            result = Expr(ty: tycache.ulonglongty, k: EIntLit, ival: n)
        of 8:
            result = Expr(ty: tycache.uintty, k: EIntLit, ival: n)
        else:
            unreachable()
        result.loc = getLoc()
        consume()
    of CPPident:
        result = Expr(k: EIntLit, ival: 0, ty: tycache.iintty) # no location!
        consume()
    of TIdentifier:
        if t.pp.want_expr:
            result = Expr(k: EIntLit, ival: 0, ty: tycache.iintty) # no location!
        elif t.l.tok.s == "__func__":
            let ty = tycache.strty
            result = Expr(
                k: ArrToAddress, voidexpr: Expr(k: EString, str: t.sema.pfunc, ty: ty, is_constant: true), 
                ty: ty, loc: loc
            )
        else:
            var ty: CType = nil
            for i in countdown(len(t.sema.scopes)-1, 0):
                var info = t.sema.scopes[i].typedefs.getOrDefault(t.l.tok.s, nil)
                if info != nil:
                  info.tag = info.tag or INFO_USED
                  ty = info.ty
                  break
                if t.sema.scopes[i].enums.hasKey(t.l.tok.s):
                  var val = t.sema.scopes[i].enums[t.l.tok.s]
                  consume()
                  return Expr(k: EIntLit, ival: val, ty: tycache.iintty, loc: loc)
            if ty == nil:
                type_error("Variable not in scope: " & t.l.tok.s)
                if t.l.tok.s.len > 2:
                    fixName(t.l.tok.s)
                return nil
            case ty.spec:
            of TYFUNCTION:
                result = unary(
                    Expr(k: EVar, sval: t.l.tok.s, ty: ty, loc: loc), AddressOf, 
                    CType(tags: TYLVALUE, spec: TYPOINTER, p: ty)
                )
            of TYARRAY:
                var cp = deepCopy(ty)
                cp.arrtype.tags = cp.arrtype.tags and prim
                result = Expr(
                    k: ArrToAddress, voidexpr: Expr(k: EVar, sval: t.l.tok.s, ty: cp, loc: loc),
                    ty: CType(tags: TYLVALUE, spec: TYPOINTER, p: ty.arrtype), loc: loc
                )
            else:
                result = Expr(k: EVar, sval: t.l.tok.s, ty: ty, loc: loc)
        result.loc = getLoc()
        consume()
    of TLbracket:
        consume()
        result = expression()
        if result == nil:
            expectExpression()
            return nil
        if t.l.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
        result.loc = loc
    of K_Generic:
        consume()
        if t.l.tok.tok != TLbracket:
            expectLB()
            return nil
        consume()
        let test = assignment_expression()
        if test == nil:
            expectExpression()
            note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
            return nil
        if t.l.tok.tok != TComma:
            expect("','")
            return nil
        consume()
        let testty = test.ty
        var defaults: Expr
        while true:
            var tname: CType = nil
            if t.l.tok.tok == Kdefault:
                if defaults != nil:
                    type_error("more then one default case in Generic expression")
                    return nil
                consume()
            else:
                tname = type_name()[0]
                if tname == nil:
                    expect("type-name")
                    note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                    return nil
            if t.l.tok.tok != TColon:
                expect("':'")
                note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                return nil
            consume()
            let e = assignment_expression()
            if e == nil:
                expectExpression()
                note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                return nil
            if tname == nil:
                defaults = e
            elif compatible(tname, testty):
                if result != nil:
                    type_error("more then one compatible types in Generic expression")
                    return nil
                result = e
            case t.l.tok.tok:
            of TComma:
              consume()
              continue
            of TRbracket:
              consume()
              break
            else:
              expect("','' or ')'")
              return nil
        if result != nil:
            return result
        if defaults != nil:
            return defaults
        type_error("Generic expression not compatible with any generic association type")
        note("no match of type " & $testty)
    of TEOF, TNul:
        return nil
    else:
        parse_error("unexpected token in expression: " & showToken())
        return nil

proc postfix_expression(): Expr =
    let e = primary_expression()
    if e == nil:
        return nil
    case t.l.tok.tok:
    of TSubSub, TAddAdd:
        if not assignable(e):
            return nil
        let op = if t.l.tok.tok == TAddAdd: PostfixIncrement else: PostfixDecrement
        consume()
        return postfix(e, op, e.ty)
    of TArrow, TDot: # member access
        let isarrow = t.l.tok.tok == TArrow
        consume()
        if t.l.tok.tok != TIdentifier:
            expect("identifier")
            note("the syntax is:\n\tfoo.member\n\tfoo->member")
            return nil
        if not (e.ty.spec == TYSTRUCT or e.ty.spec == TYUNION):
            type_error("member access is not struct or union")
            inTheExpression(e)
            return nil
        if isarrow and e.ty.spec != TYPOINTER:
            type_error("pointer member access('->') must be used in a pointer")
            note("maybe you mean: '.'")
            return nil
        elif isarrow == false and e.ty.spec == TYPOINTER:
            type_error("member access('.') cannot used in a pointer")
            note("maybe you mean: '->'")
        for i in 0..<len(e.ty.selems):
            if t.l.tok.s == e.ty.selems[i][0]:
                consume()
                var ty = e.ty.selems[i][1]
                ty.tags = ty.tags or TYLVALUE
                if isarrow:
                    return Expr(k: EPointerMemberAccess, obj: e, idx: i, ty: ty, loc: e.loc)
                return Expr(k: EMemberAccess, obj: e, idx: i, ty: ty, loc: e.loc)
        type_error("struct/union " & $e.ty.sname & " has no member " & t.l.tok.s)
        return nil
    of TLbracket: # function call
        consume()
        var ty: CType
        var f = e
        if f.ty.spec == TYPOINTER:
            if e.ty.p.spec != TYFUNCTION:
                type_error("call function is not a function pointer")
                inTheExpression(e)
                return nil
            ty = f.ty.p
            f = f.uoperand
        elif f.ty.spec != TYFUNCTION:
            type_error("call function is not a function or function pointer")
            note("expression has type " & $f.ty)
            inTheExpression(f)
            return nil
        var args: seq[Expr]
        if t.l.tok.tok == TRbracket:
            consume()
        else:
            while true:
                let a = assignment_expression()
                if a == nil:
                    expectExpression()
                    note("the syntax is:\n\tfunction-name(argument)")
                    return nil
                args.add(a)
                if t.l.tok.tok == TComma:
                    consume()
                elif t.l.tok.tok == TRbracket:
                    consume()
                    break
        let params = ty.params
        if len(params) > 0 and params[^1][1] == nil: # varargs
            if len(args) < (len(params) - 1):
                type_error("too few arguments to variable argument function")
                note("at lease " & $(len(params) - 0) & " arguments needed")
                return nil
        elif len(params) != len(args):
            type_error("expect " & $(len(params)) & " parameters, " & $len(args) & " provided")
            return nil
        var i = 0
        while true:
            if i == len(params):
                break
            if params[i][1] == nil:
                for j in i ..< len(args):
                    args[j] = default_argument_promotions(args[j])
                break
            var msg = "passing argument " 
            msg.add($(i + 1))
            msg.add(" of '")
            msg.add($f)
            msg.add('\'')
            discard warn_if_bad_cast(args[i], params[i][1], msg)
            var a = castto(args[i], params[i][1])
            args[i] = a
            inc i
        return Expr(k: ECall, callfunc: f, callargs: args, ty: ty.ret, loc: e.loc)
    of TLSquareBrackets: # array subscript
        if e.ty.spec != TYPOINTER:
            type_error("array subscript is not a pointer")
            inTheExpression(e)
            return nil
        consume()
        var rhs = expression()
        if rhs == nil:
            expectExpression()
            note("the syntax is:\n\tarray[expression]")
            return nil
        if t.l.tok.tok != TRSquareBrackets:
            expect("']'")
            return
        consume()
        var ty = e.ty.p
        ty.tags = ty.tags or TYLVALUE
        return Expr(k: ESubscript, left: e, right: rhs, ty: ty, loc: e.loc)
    else:
        return e

proc multiplicative_expression(): Expr =
    result = cast_expression()
    while true:
        case t.l.tok.tok:
        of TMul:
            consume()
            var r = cast_expression()
            if r == nil:
                return nil
            make_mul(result, r)
        of TSlash:
            consume()
            var r = cast_expression()
            if r == nil:
                return nil
            make_div(result, r)
        of TPercent:
            consume()
            var r = cast_expression()
            if r == nil:
                return nil
            make_rem(result, r)
        else:
            return result

proc additive_expression(): Expr =
    result = multiplicative_expression()
    while true:
        case t.l.tok.tok:
        of TAdd:
            consume()
            var r = multiplicative_expression()
            if r == nil:
                return nil
            make_add(result, r)
        of TDash:
            consume()
            var r = multiplicative_expression()
            if r == nil:
                return nil
            make_sub(result, r)
        else:
            return result

proc shift_expression(): Expr =
    result = additive_expression()
    while true:
        case t.l.tok.tok:
        of Tshl:
            consume()
            var r = additive_expression()
            if r == nil:
                return nil
            make_shl(result, r)
        of Tshr:
            consume()
            var r = additive_expression()
            if r == nil:
                return nil
            make_shr(result, r)
        else:
            return result

proc relational_expression(): Expr =
    result = shift_expression()
    while true:
        case t.l.tok.tok:
        of TLt:
            consume()
            var r = shift_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result =  boolToInt(binop(result, if isFloating(r.ty): FLT else: (if isSigned(r.ty): SLT else: ULT), r, tycache.b))
        of TLe:
            consume()
            var r = shift_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result = boolToInt(binop(result, if isFloating(r.ty): FLE else: (if isSigned(r.ty): SLE else: ULE), r, tycache.b))
        of TGt:
            consume()
            var r = shift_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result = boolToInt(binop(result, if isFloating(r.ty): FGT else: (if isSigned(r.ty): SGT else: UGT), r, tycache.b))
        of TGe:
            consume()
            var r = shift_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result = boolToInt(binop(result, if isFloating(r.ty): FGE else: (if isSigned(r.ty): SGE else: UGE), r, tycache.b))
        else:
            return result

proc equality_expression(): Expr =
    result = relational_expression()
    while true:
        case t.l.tok.tok:
        of TEq:
            consume()
            var r = relational_expression()
            if r == nil:
                return nil
            if result.ty.spec == TYPOINTER and r.ty.spec == TYPOINTER:
                result = boolToInt(binop(result, EQ, r, tycache.b))
            else:
                checkSpec(result, r)
                result = boolToInt(binop(result, if isFloating(r.ty): FEQ else: EQ, r, tycache.b))
        of TNe:
            consume()
            var r = relational_expression()
            if r == nil:
                return nil
            if result.ty.spec == TYPOINTER and r.ty.spec == TYPOINTER:
                result = boolToInt(binop(result, NE, r, tycache.b))
            else:
                checkSpec(result, r)
                result = boolToInt(binop(result, if isFloating(r.ty): FNE else: NE, r, tycache.b))
        else:
            return result

proc AND_expression(): Expr =
    result = equality_expression()
    while true:
        case t.l.tok.tok:
        of TBitAnd:
            consume()
            var r = equality_expression()
            if r == nil:
                return nil
            make_bitop(result, r, And)
        else:
            return result

proc exclusive_OR_expression(): Expr =
    result = AND_expression()
    while true:
        case t.l.tok.tok:
        of TXOr:
            consume()
            var r = AND_expression()
            if r == nil:
                return nil
            make_bitop(result, r, Xor)
        else:
            return result

proc inclusive_OR_expression(): Expr =
    result = exclusive_OR_expression()
    while true:
        case t.l.tok.tok:
        of TBitOr:
            consume()
            var r = exclusive_OR_expression()
            if r == nil:
                return nil
            make_bitop(result, r, Or)
        else:
            return result

proc logical_AND_expression(): Expr =
    result = inclusive_OR_expression()
    while true:
        case t.l.tok.tok:
        of TLogicalAnd:
            consume()
            var r = inclusive_OR_expression()
            if r == nil:
                return nil
            checkScalar(result, r)
            result = boolToInt(binop(result, LogicalAnd, r, tycache.iintty))
        else:
            return result

proc logical_OR_expression(): Expr =
    result = logical_AND_expression()
    while true:
        case t.l.tok.tok:
        of TLogicalOr:
            consume()
            var r = logical_AND_expression()
            if r == nil:
                return nil
            checkScalar(result, r)
            result = boolToInt(binop(result, LogicalOr, r, tycache.iintty))
        else:
            return result

proc expression(): Expr =
    ## parse a expression
    result = assignment_expression()
    while true:
        if t.l.tok.tok == TComma:
            consume()
            var r = assignment_expression()
            if r == nil:
                return nil
            result = Expr(k: EBin, lhs: result, rhs: r, bop: Comma, ty: r.ty, loc: r.loc)
        else:
            return result

proc conditional_expression(start: Expr): Expr =
    var lhs = logical_OR_expression()
    if lhs == nil:
        expectExpression()
        return nil
    if t.l.tok.tok != TColon:
        return lhs
    consume()
    var rhs = conditional_expression()
    if rhs == nil:
        expectExpression()
        note("the syntax is:\n\texpr ? a ? b")
        return nil
    if not compatible(lhs.ty, rhs.ty):
        type_error("incompatible type for conditional-expression")
        note("the left is " & $lhs.ty)
        note("the right is " & $rhs.ty)
    conv(lhs, rhs)
    return Expr(k: ECondition, cond: start, cleft: lhs, cright: rhs, ty: lhs.ty, loc: start.loc)

proc conditional_expression(): Expr =
    let e = logical_OR_expression()
    if e == nil:
        return nil
    if t.l.tok.tok == TQuestionMark:
      return conditional_expression(e)
    return e

proc assignment_expression(): Expr =
    result = logical_OR_expression()
    if result == nil:
        return nil
    let tok = t.l.tok.tok
    if tok == TQuestionMark:
        consume()
        return conditional_expression(result)
    if t.l.tok.tok in {TAssign, TAsignAdd, TAsignSub, 
    TAsignMul, TAsignDiv, TAsignRem, TAsignShl, 
    TAsignShr, TAsignBitAnd, TAsignBitOr, TAsignBitXor}: 
        if not assignable(result):
            return nil
        consume()
        var e = assignment_expression()
        if e == nil:
            expectExpression()
            return nil
        var lhs = deepCopy(result)
        return binop(lhs, Assign, castto(
            case tok:
            of TAssign:
                e
            of TAsignAdd:
                make_add(result, e)
                result
            of TAsignSub:
                make_sub(result, e)
                result
            of TAsignMul:
                make_mul(result, e)
                result
            of TAsignDiv:
                make_div(result, e)
                result
            of TAsignRem:
                make_rem(result, e)
                result
            of TAsignShl:
                make_shl(result, e)
                result
            of TAsignShr:
                make_shr(result, e)
                result
            of TAsignBitAnd:
                make_bitop(result, e, And)
                result
            of TAsignBitOr:
                make_bitop(result, e, Or)
                result
            of TAsignBitXor:
                make_bitop(result, e, Xor)
                result
            else:
                unreachable()
                nil
            , lhs.ty), lhs.ty)
    else:
        return result

proc translation_unit(): Stmt =
    ## parse top-level declaration until EOF reaches, the entry point of program
    ##
    ## never return nil, return a compound statement
    result = Stmt(k: SCompound)
    var s: Stmt
    while t.l.tok.tok != TEOF:
        if t.l.tok.tok == KAsm:
            s = parse_asm()
            checkSemicolon()
        else:
            s = declaration()
        if s == nil:
            break
        result.stmts.add(s)

proc runParser(): Stmt =
    ## eat first token and parse a translation_unit
    ##
    ## never return nil, return a compound statement
    consume()
    enterBlock()
    result = translation_unit()
    leaveBlock()

proc compound_statement(): Stmt =
    ## parse mant statements
    result = Stmt(k: SCompound, loc: getLoc())
    consume()
    enterBlock()
    while t.l.tok.tok != TRcurlyBracket:
        var s: Stmt = nil
        if istype(t.l.tok.tok):
            s = declaration()
        else:
            s = statament()
        if s == nil:
            leaveBlock()
            return nil
        result.stmts.add(s)
    leaveBlock()
    consume()
    return result

proc compound_statement(params: seq[(string, CType)], lables: var HashSet[string]): Stmt =
    result = Stmt(k: SCompound, loc: getLoc())
    consume()
    enterBlock()
    t.sema.lables.add(initTable[string, uint8]())
    for (name, vty) in params:
        if vty == nil:
            break
        var ty = deepCopy(vty)
        ty.tags = ty.tags or TYLVALUE
        putsymtype(name, ty)
    while t.l.tok.tok != TRcurlyBracket:
        var s: Stmt = nil
        if istype(t.l.tok.tok):
            s = declaration()
        else:
            s = statament()
        if s == nil:
            leaveBlock()
            return nil
        result.stmts.add(s)
    for (name, t) in t.sema.lables[^1].pairs():
        if t == LBL_FORWARD:
            type_error("use of undeclared label '" & name & "'")
        elif t == LBL_DECLARED:
            warning("unused label: '" & name & '\'')
        else:
            assert t == LBL_OK
            lables.incl name
    discard t.sema.lables.pop()
    leaveBlock()
    consume()
    return result

proc statament(): Stmt =
    ## parse a statement
    var loc = getLoc()
    if t.l.tok.tok == KAsm:
        result = parse_asm()
        checkSemicolon()
        return result
    if t.l.tok.tok == TSemicolon:
        consume()
        return Stmt(k: SSemicolon, loc: loc)
    elif t.l.tok.tok == TLcurlyBracket:
        return compound_statement()
    elif t.l.tok.tok == Kcase:
        consume()
        var e = constant_expression()
        if e == nil:
            expect("constant-expression")
            return nil
        if t.l.tok.tok != TColon:
            expect(":")
            return nil
        consume()
        let s = statament()
        if s == nil:
            expectStatement()
            return nil
        return Stmt(k: Scase, case_expr: castto(e, t.sema.currentCase), case_stmt: s, loc: loc)
    elif t.l.tok.tok == Kdefault:
        consume()
        if t.l.tok.tok != TColon:
            expect(":")
            return nil
        consume()
        let s = statament()
        if s == nil:
            return nil
        return Stmt(k: SDefault, default_stmt: s, loc: loc)
    elif t.l.tok.tok == Kgoto:
        consume()
        if t.l.tok.tok != TIdentifier:
            expect("identifier")
            note("the syntax is:\n\tgoto label;")
            return nil
        var location = t.l.tok.s
        consume()
        checkSemicolon()
        getLabel(location)
        return Stmt(k: SGoto, location: location, loc: loc)
    elif t.l.tok.tok == Kcontinue:
        consume()
        checkSemicolon()
        return Stmt(k: SContinue, loc: loc)
    elif t.l.tok.tok == Kbreak:
        consume()
        checkSemicolon()
        return Stmt(k: SBreak, loc: loc)
    elif t.l.tok.tok == Kreturn:
        consume()
        if t.l.tok.tok == TSemicolon:
            consume()
            if bool(t.sema.currentfunctionRet.tags and TYVOID):
                return Stmt(k: SReturn, exprbody: nil, loc: loc)
            warning("missing return value in function returning non-void")
            note("A return statement without an expression shall only appear in a function whose return type is void")
            return Stmt(k: SReturn, exprbody: Expr(k: EDefault, ty: t.sema.currentfunctionRet), loc: loc)
        let e = expression()
        if e == nil:
            expectExpression()
            return nil
        checkSemicolon()
        if bool(t.sema.currentfunctionRet.tags and TYVOID):
            warning("ignored the return value in function returning void")
            note("A return statement with an expression shall not appear in a function whose return type is void")
            return Stmt(k: SReturn, exprbody: nil, loc: loc)
        discard warn_if_bad_cast(e, t.sema.currentfunctionRet, " returning '" & $t.sema.currentfunctionRet & "' from a function with return type '" & $e.ty & "'")
        return Stmt(k: SReturn, exprbody: castto(e, t.sema.currentfunctionRet), loc: loc)
    elif t.l.tok.tok == Kif:
        consume()
        if t.l.tok.tok != TLbracket:
            expectLB()
            return nil
        consume()
        let e = expression()
        if e == nil:
            expectExpression()
            return nil
        if not checkScalar(e.ty):
            type_error("expect scalar types")
        if t.l.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
        let s = statament()
        if s == nil:
            expectStatement()
            return nil
        var elsebody: Stmt = nil
        if t.l.tok.tok == Kelse:
            consume()
            elsebody = statament()
            if elsebody == nil:
                expectStatement()
                return nil
        return Stmt(k: SIf, iftest: e, ifbody: s, elsebody: elsebody, loc: loc)
    elif t.l.tok.tok == Kwhile or t.l.tok.tok == Kswitch:
        let tok = t.l.tok.tok
        consume()
        if t.l.tok.tok != TLbracket:
            expectLB()
            return nil
        consume()
        var e = expression()
        if e == nil:
            expectExpression()
            return nil
        if not checkScalar(e.ty):
            type_error("expect scalar")
        if t.l.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
        if tok == Kswitch:
            integer_promotions(e)
            var oldcase = t.sema.currentCase
            t.sema.currentCase = e.ty
            let s = statament()
            if s == nil:
                expectStatement()
                return nil
            t.sema.currentCase = oldcase
            return Stmt(k: SSwitch, test: e, body: s, loc: loc)
        let s = statament()
        if s == nil:
            expectStatement()
            return nil
        return Stmt(k: SWhile, test: e, body: s, loc: loc)
    elif t.l.tok.tok == Kfor:
        # this are valid in C
        # for(int a;;)                                                                                                                                          
        # {                                                                                                                                                  
        #    int a;                                                                                                                                        
        # }
        consume()
        enterBlock()
        if t.l.tok.tok != TLbracket:
            expectLB()
            return nil
        consume()
        # init-clause may be an expression or a declaration
        var init: Stmt = nil
        if istype(t.l.tok.tok):
            init = declaration()
            if init == nil:
                expect("declaration")
                return nil
        else:
            if t.l.tok.tok != TSemicolon:
                let ex = expression()
                if ex == nil:
                    expectExpression()
                    return nil
                init = Stmt(k: SExpr, exprbody: ex, loc: loc)
                if t.l.tok.tok != TSemicolon:
                    expect("';'")
                    return nil
            consume()
        #  cond-expression 
        var cond: Expr = nil
        if t.l.tok.tok != TSemicolon:
            cond = expression()
            if cond == nil:
                expectExpression()
                return nil
            if not checkScalar(cond.ty):
                type_error("expect scalar")
            if t.l.tok.tok != TSemicolon:
                expect("';'")
                return nil
        consume()
        # incl-expression
        var forincl: Expr = nil
        if t.l.tok.tok != TRbracket:
            forincl = expression()
            if forincl == nil:
                expectExpression()
                return nil
            if t.l.tok.tok != TRbracket:
                expectRB()
                return nil
        consume()
        var s = statament()
        if s == nil:
            expectStatement()
            return nil
        leaveBlock()
        return Stmt(k: SFor, forinit: init, forcond: cond, forbody: s, forincl: forincl, loc: loc)
    elif t.l.tok.tok == Kdo:
        consume()
        let s = statament()
        if s == nil:
            expectStatement()
            return nil
        if t.l.tok.tok != Kwhile:
            expect("'while'")
            return nil
        consume()
        if t.l.tok.tok != TLbracket:
            expectLB()
            return nil
        consume()
        let e = expression()
        if e == nil:
            expectExpression()
            return nil
        if not checkScalar(e.ty):
            type_error("expect scalar")
        if t.l.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
        checkSemicolon()
        return Stmt(k: SDoWhile, test: e, body: s, loc: loc)
    elif t.l.tok.tok == TIdentifier:
        var val = t.l.tok.s
        consume()
        if t.l.tok.tok == TColon: # # labeled-statement
            consume()
            let s = statament()
            if s == nil:
                expectStatement()
                note("to add a empty statement, use:\n\tlabel: ;")
                return nil
            putLable(val)
            return Stmt(k: SLabled, label: val, labledstmt: s, loc: loc)
        else: # expression
            putToken()
            t.l.tok = TokenV(tok: TIdentifier, tags: TVSVal, s: val)
    let e = expression()
    if e == nil:
        expectExpression()
        return nil
    checkSemicolon()
    return Stmt(k: SExpr, exprbody: e, loc: loc)

proc link(opath: string = app.output) =
    case app.linker:
    of GCCLD:
        var path = opath & ".o"
        writeObjectFile(path)
        runLD(path, opath)
    of LLD:
        var path = opath & ".o"
        writeObjectFile(path)
        runLLD(path, opath)

proc output() =
    case app.mode:
    of OutputLLVMAssembly:
        writeModuleToFile(app.output)
    of OutputBitcode:
        writeBitcodeToFile(app.output)
    of OutputObjectFile:
        writeObjectFile(app.output)
    of OutputAssembly:
        writeAssemblyFile(app.output)
    of OutputCheck:
        discard
    of OutputLink:
        link()

set_exit_code(1)
if parseCLI():
      if initTarget(): 
          initBackend()
          for (name, v) in getDefines():
              t.pp.macros[name] = PPMacro(tokens: v, flags: MOBJ)
          addLLVMModule(t.path)
          let translation_unit = runParser()
          if not err():
              set_exit_code(0)
              if app.mode != OutputCheck:
                  enterScope()
                  for i in translation_unit.stmts:
                    gen(i)
                  leaveScope()
                  optimize()
                  if app.runJit:
                      runJit()
                      if app.mode != OutputLink:
                          warning("jit cannot combine with other output flags\njit will not write output\n")
                  else:
                      if app.g:
                          dIBuilderFinalize(b.di)
                          disposeDIBuilder(b.di)
                      disposeBuilder(b.builder)
                      output()
              
          llvm.shutdown()
