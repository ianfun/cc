import std/[macrocache, strutils, tables]

type
  intmax_t* = int64
  uintmax_t* = uint64

type 
  VerboseLevel* = enum
    WError, WWarning, WNote, WVerbose
  Linker* = enum LLD, GCCLD

proc perror*(str: cstring) {.importc: "perror", header: "stdio.h".}

type
    CTypeSpec* = enum
      TYPRIM,
      TYPOINTER,
      TYSTRUCT,
      TYUNION,
      TYENUM,
      TYBITFIELD,
      TYARRAY,
      TYFUNCTION,
      TYINCOMPLETE
    CType* = ref object
      tags*: uint32
      align*: uint32 # zero if use default (not specified)
      case spec*: CTypeSpec
      of TYPRIM:
        discard
      of TYPOINTER:
        p*: CType
      of TYSTRUCT, TYUNION:
        sname*: string
        selems*: seq[(string, CType)]
        packed*: bool
      of TYENUM:
        ename*: string
        eelems*: seq[(string, intmax_t)]
      of TYBITFIELD:
        bittype*: CType
        bitsize*: intmax_t
      of TYFUNCTION:
        fname*: string
        ret*: CType
        params*: seq[(string, CType)]
      of TYARRAY:
        arrsize*: intmax_t
        arrtype*: CType
      of TYINCOMPLETE:
        tag*: CTypeSpec
        name*: string

proc unreachable*() =
  assert false

const 
  INFO_USED* = 2

type
  Location* = object
    line*: int
    col*: int
  Info* = ref object
    tag*: uint32
    loc*: Location
    ty*: CType

type Token* = enum
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

const tyCounter = CacheCounter"tyCounter"

proc make_ty(): uint32 =
  result = 1'u32 shl tyCounter.value
  tyCounter.inc()

const ## optional type tags
  TYINVALID* = 0'u32
  # TYAUTO* = make_ty()
  TYCONST* = make_ty()
  TYRESTRICT* = make_ty()
  TYVOLATILE* = make_ty()
  TYATOMIC* = make_ty()
  TYINLINE* = make_ty()
  TYSTATIC* = make_ty()
  TYNORETURN* = make_ty()
  TYALIGNAS* = make_ty()
  TYEXTERN* = make_ty()
  TYREGISTER* = make_ty()
  TYTHREAD_LOCAL* = make_ty()
  TYTYPEDEF* = make_ty()
  TYLVALUE* = make_ty()

const ## basic types
  ## note: they value ordered by bit size!
  ## do not re-order them
  TYVOID* = make_ty()
  TYBOOL* = make_ty()
  TYCOMPLEX* = make_ty()
  TYINT8* = make_ty()
  TYINT16* = make_ty()
  TYINT32* = make_ty()
  TYINT64* = make_ty()
  TYUINT8* = make_ty()
  TYUINT16* = make_ty()
  TYUINT32* = make_ty()
  TYUINT64* = make_ty()
  TYFLOAT* = make_ty()
  TYDOUBLE* = make_ty()

const ## type alias
  TYCHAR* = TYINT8
  TYSHORT* = TYINT16
  TYINT* = TYINT32
  TYLONG* = TYINT64 ## 32 bit in MSVC, 64 bit in GCC/JVM
  TYLONGLONG* = TYINT64
  TYUCHAR* = TYUINT8
  TYUSHORT* = TYUINT16
  TYUINT* = TYUINT32
  TYULONG* = TYUINT64 ## 32 bit in MSVC, 64 bit in GCC/JVM
  TYULONGLONG* = TYUINT64
  TYLONGDOUBLE* = TYDOUBLE
  TYSIZE_T* = when sizeof(pointer) == 8: TYUINT64 else: TYUINT32

const 
  prim* = TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
    TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64 or 
    TYFLOAT or TYDOUBLE or
    TYBOOL
  signed*   = TYINT8  or TYINT16  or TYINT32  or TYINT64
  unsigned* = TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64

type
    PostfixOP* = enum
      PostfixIncrement="++", PostfixDecrement="--"
    UnaryOP* = enum
      Pos, # it like nop, but it will do integer promotion
      UNeg, SNeg, FNeg,
      Not, AddressOf,
      PrefixIncrement, PrefixDecrement, Dereference, LogicalNot
    BinOP* = enum
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
      SAddP, # num add/sub pointer 
      Comma,

      # compare operators
      EQ, NE, 
      UGT, UGE, ULT, ULE, 
      SGT, SGE, SLT, SLE,
      # ordered float compare
      FEQ, FNE, 
      FGT, FGE, FLT, FLE
    CastOp* = enum
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
    Codepoint* = uint32
    PPMacroFlags* = enum
      MOBJ, MFUNC, MBuiltin
    PPMacro* = ref object
      tokens*: seq[TokenV]
      case flags*: PPMacroFlags # if a object like macro?
      of MFUNC:
        params*: seq[string]
        ivarargs*: bool
      of MBuiltin:
        fn*: proc ()
      of MOBJ:
        discard
    ParseFlags* = enum
      PFNormal=1, PFPP=2
    TokenVTags* = enum
      TVNormal, TVSVal, TVIVal, TVFval, TVStr
    # integer tags
    ITag* = enum
      Iint,   Ilong,   Iulong,  Ilonglong, Iulonglong, Iuint
    # also for chars
    # char,   char16,  char32,  wchar
    #  u8       u        U       L
    # float tags
    FTag* = enum
      Fdobule, Ffloat
    TokenV* = ref object
      tok*: Token
      case tags*: TokenVTags
      of TVNormal:
        discard
      of TVSVal:
        s*: string
      of TVFval:
        f*: float
        ftag*: FTag
      of TVIVal:
        i*: int
        itag*: ITag
      of TVStr:
        str*: string
        enc*: uint8
    StmtKind* = enum
      SSemicolon, SCompound, SGoto, SContinue, SBreak, SReturn, SExpr, SLabled, SIf, 
      SDoWhile, SWhile, SFor, SSwitch, SDeclOnly, SAsm
      SVarDecl, SDefault, SCase, SFunction, SVarDecl1
    StmtList* = seq[Stmt] # compound_stmt, { body }
    Stmt* = ref object
      case k*: StmtKind
      of SFunction:
        funcname*: string
        functy*: CType
        funcbody*: Stmt
      of SAsm:
        asms*: string
      of SCompound:
        stmts*: StmtList
      of SDefault:
        default_stmt*: Stmt
      of Scase:
        case_expr*: Expr
        case_stmt*: Stmt
      of SBreak, SContinue, SSemicolon:
        discard
      of SExpr, SReturn:
        exprbody*: Expr
      of SGoto:
        location*: string
      of SLabled:
        label*: string
        labledstmt*: Stmt
      of SIf:
        iftest*: Expr
        ifbody*: Stmt
        elsebody*: Stmt
      of SDoWhile, SWhile, SSwitch:
        test*: Expr
        body*: Stmt
      of SFor:
        forinit*: Stmt
        forcond*, forincl*: Expr
        forbody*: Stmt
      of SVarDecl1:
        var1name*: string
        var1type*: CType
      of SVarDecl:
        vars*: seq[(string, CType, Expr)]
        # (name, type, init<opt>, align<opt>)
      of SDeclOnly:
        decl*: CType
    ExprKind* = enum
      EBin, EUnary, EPostFix, EIntLit, EFloatLit, EVoid,
      EVar, ECondition, ECast, ECall, ESubscript, EDefault,
      EArray, EStruct, EBackend, EString, EUndef
      EPointerMemberAccess, EMemberAccess, ArrToAddress
    Expr* = ref object
      ty*: CType
      case k*: ExprKind
      of EVoid, ArrToAddress:
        voidexpr*: Expr 
      of EBin:
        lhs*, rhs*: Expr
        bop*: BinOP
      of EUnary:
        uop*: UnaryOP
        uoperand*: Expr
      of EPostFix:
        pop*: PostfixOP
        poperand*: Expr
      of EIntLit:
        ival*: int
      of EFloatLit:
        fval*: float
      of EString:
        str*: string
      of EVar:
        sval*: string
      of EArray, EStruct:
        arr*: seq[Expr]
      of ECondition:
        cond*: Expr
        cleft*, cright*: Expr
      of ECast:
        castop*: CastOp
        castval*: Expr
      of ECall:
        callfunc*: Expr
        callargs*: seq[Expr]
      of ESubscript:
        left*, right*: Expr
      of EPointerMemberAccess, EMemberAccess:
        obj*: Expr
        idx*: int
      of EBackend:
        p*: pointer
      of EDefault, EUndef:
        discard

const
  CSkip* = {' ', '\t', '\f', '\v'} # space, tab, new line, form feed
  KwStart* = Kauto
  KwEnd* = K_Thread_local

proc addTag*(a: uint32, dst: var string) =
# if bool(a and TYAUTO): dst.add("auto ")
  if bool(a and TYCONST): dst.add("const ")
  if bool(a and TYRESTRICT): dst.add("restrict ")
  if bool(a and TYVOLATILE): dst.add("volatile ")
  if bool(a and TYATOMIC): dst.add("_Atomic ")
  if bool(a and TYINLINE): dst.add("inline ")
  if bool(a and TYSTATIC): dst.add("static ")
  if bool(a and TYNORETURN): dst.add("_Noreturn ")
  if bool(a and TYALIGNAS): dst.add("_Alignas ")
  if bool(a and TYEXTERN): dst.add("extern ")
  if bool(a and TYREGISTER): dst.add("register ")
  if bool(a and TYTHREAD_LOCAL): dst.add("_Thread_local ")
  if bool(a and TYTYPEDEF): dst.add("typedef ")

proc `$`*(a: Stmt, level=0): string

proc `$`*(e: Expr): string

proc `$`*(a: CType, level=0): string

proc joinShow6*(a: seq[(string, CType, Expr)]): string =
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

proc joinShow5*(a: seq[Stmt], level=1): string =
    var padding = "  ".repeat(level)
    result = "{\n"
    for i in a:
        result.add(padding & '\t')
        result.add(`$`(i, level + 1))
        result.add('\n')
    if level > 2:
      result.add("  ".repeat(level - 2))
    result.add("}")

proc joinShow4*(a: seq[(string, intmax_t)], level=1): string =
    var padding = "  ".repeat(level)
    result = "{\n"
    for (s, val) in a:
        result.add(padding & '\t')
        result.add(s)
        result.add('=')
        result.add($val)
        result.add(',')
        result.add('\n')
    if level > 2:
      result.add("  ".repeat(level - 2))
    result.add("}")

proc joinShow3*(a: seq[(string, CType)], level=1): string =
    var padding = "  ".repeat(level)
    result = "{\n"
    for (s, ty) in a:
        result.add(padding & '\t')
        result.add(`$`(ty, level=level + 1))
        result.add(' ')
        result.add(s)
        result.add(';')
        result.add('\n')
    if level > 2:
      result.add("  ".repeat(level - 2))
    result.add("}")

proc joinShow2*[A, B](a: seq[(A, B)]): string =
    if len(a) == 0:
        return ""
    for i in 0..<(len(a)-1):
        result.add($a[i][1])
        result.add(' ')
        result.add($a[i][0])
        result.add(", ")
    result.add($a[^1][1] & ' ' & $a[^1][0])

proc joinShow*[T](a: seq[T], c: string = " "): string =
    if len(a) == 0:
        return ""
    for i in 0..<(len(a)-1):
        result.add($a[i])
        result.add(c)
    result.add($a[^1])

proc `$`*(a: Stmt, level=0): string =
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

proc `$`*(a: CType, level=0): string =
  if a == nil:
    return "<nil>"
  result = ""
  case a.spec:
  of TYINCOMPLETE:
    let s = if a.tag == TYSTRUCT:  "struct" else: (if a.tag == TYUNION: "union" else: "enum")
    return s & ' ' & a.name & " <incomplete-type>"
  of TYPRIM: # format style: const int
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
    result.add("enum " & a.ename & ' ' & joinShow4(a.eelems, level + 1))
  of TYFUNCTION:
    result.add($a.ret & a.fname & " (" & joinShow2(a.params) & ')')
  of TYSTRUCT:
    result.add("struct " & a.sname & ' ' & joinShow3(a.selems, level + 1))
  of TYUNION:
    result.add("union " & a.sname & ' ' & joinShow3(a.selems, level + 1))
  of TYBITFIELD:
    result.add($a.bittype & " : " &  $a.bitsize)
  of TYPOINTER: # format style: (int *const)
    result.add('(')
    result.add($a.p & '*')
    addTag(a.tags, result)
    result.add(')')
  of TYARRAY:
    result.add($a.arrtype & " [" & (if a.arrsize < 0: "*" else: $a.arrsize) & "]")

var
  gkeywordtable* = initTable[string, Token](int(KwEnd) - int(KwStart) + 1)

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

proc `$`*(e: Expr): string =
  if e == nil:
    return "<nil>"
  case e.k:
  of EUndef:
    "<undefined-value>"
  of EVoid:
    "(void)" & $e.voidexpr
  of ArrToAddress:
    $e.voidexpr
  of EDefault:
    "<zero>"
  of EStruct:
    "<struct-init>"
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
    '(' & $e.ty & ')' & $e.castval & "(" & $e.castop & ')'
  of ESubscript:
    $e.left & '[' & $e.right & ']'
  of ECall:
    $e.callfunc & '(' & $joinShow(e.callargs, ", ") & ')'
  of EArray:
    "{" & $e.arr & "}"
  of EBackend:
    "<backend>"

proc `$`*(loc: Location): string =
  $loc.line & ':' & $loc.col

proc initCore() =
  for k in int(KwStart)..int(KwEnd):
    gkeywordtable[$cast[Token](k)] = cast[Token](k)

template init*(lexer, cpp, parser, eval, backend): untyped =
  initCore()
  import lexer, cpp, parser, eval, backend
  setLexer()
  setCpp()
  setParser()
  setEval()

template shutdown*() =
  closeParser()
  shutdown_backend()

proc isKeyword*(a: string): Token =
  gkeywordtable.getOrDefault(a, TNul)

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

when false:
  type
      time_t {.importc: "time_t", nodecl, header: "time.h".} = clonglong 
      tm {.importc: "struct tm", nodecl, header: "time.h".} = object
        tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday, tm_isdst: cint
  
  proc time(t: ptr[time_t]): time_t {.importc: "time", nodecl, header: "time.h".}
  
  proc asctime(time_ptr: ptr[tm]): cstring {.importc: "asctime", nodecl, header: "time.h".}
  
  proc localtime(time_ptr: ptr[time_t]): ptr[tm] {.importc: "localtime", nodecl, header: "time.h".}

iterator getDefines*(): (string, seq[TokenV]) =
  proc str(s: string): TokenV =
    TokenV(tok: TStringLit, tags: TVSVal, s: s)
  proc num(s: string): TokenV =
    TokenV(tok: TPPNumber, tags: TVSVal, s: s)
  proc space(): TokenV = 
    TokenV(tok: TSpace, tags: TVNormal)
  proc empty(): seq[TokenV] =
    discard
  yield ("__STDC__", @[num("1")])
  yield ("__STDC_VERSION__", @[num("201710L")])
  yield ("__STDC_HOSTED__", @[num("1")])
#  yield ("__STDC_NO_THREADS__", @[num("1")])
#  yield ("__STDC_NO_ATOMICS__", @[num("1")])
  yield ("__STDC_UTF_16__", @[num("1")])
  yield ("__STDC_UTF_32__", @[num("1")])
  yield ("__SIZE_TYPE__", @[str("size_t")])
  yield ("__INT8_TYPE__", @[str("__int8")])
  yield ("__INT16_TYPE__", @[str("__int16")])
  yield ("__INT32_TYPE__", @[str("__int32")])
  yield ("__INT64_TYPE__", @[str("__int64")])
  yield ("__INT_FAST8_TYPE__", @[str("__int8")])
  yield ("__INT_FAST16_TYPE__", @[str("__int16")])
  yield ("__INT_FAST32_TYPE__", @[str("__int32")])
  yield ("__INT_FAST64_TYPE__", @[str("__int64")])
  yield ("__UINT_FAST8_TYPE__", @[str("unsigned"), space(), str("__int8")])
  yield ("__UINT_FAST16_TYPE__", @[str("unsigned"), space(), str("__int16")])
  yield ("__UINT_FAST32_TYPE__", @[str("unsigned"), space(), str("__int32")])
  yield ("__UINT_FAST64_TYPE__", @[str("unsigned"), space(), str("__int64")])
  yield ("__INTPTR_TYPE__", @[str("long"), space(), str("long"), space(), str("int")])
  yield ("__UINTPTR_TYPE__", @[str("unsigned"), space(),str("long"), space(), str("long"), space(), str("int")])
  yield ("__CHAR_BIT__", @[num("8")])
  case hostOS:
  of "windows":
    yield ("_WIN32", empty())
    yield ("WIN32", empty())
    if sizeof(cstring) == 8:
      yield ("WIN64", empty())
      yield ("_WIN64", empty())
  of "macosx":
    yield ("__APPLE__", empty())
  of "linux":
    yield ("__linux__", empty())
    yield ("__linux", empty())
  of "netbsd":
    yield ("__NetBSD__", empty())
  of "freebsd":
    yield ("__FreeBSD__", empty())

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
    stderr.writeLine("cc: \e[33m" & "warning: " & msg & "\e[0m")

proc error*() =
  stderr.write("cc: \e[31merror\e[0m: ")

proc verbose*(msg: string) =
  if ord(app.verboseLevel) >= ord(WVerbose):
    stderr.writeLine("cc: " & msg)

