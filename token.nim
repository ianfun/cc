import std/[streams, tables, times, sets, macrocache]

type
    InputError* = object of CatchableError
    LexerError* = object of InputError
    ParseError* = object of InputError
    EvalError* = object of InputError

type Token* = enum
  TNul=0,
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
  K_Bool="_Bool", K_Complex="Complex", 
  K_Decimal128="_Decimal128", K_Decimal32="_Decimal32", 
  K_Decimal64="_Decimal64", K_Generic="_Generic", 
  K_Imaginary="_Imaginary", K_Noreturn="_Noreturn", K_Static_assert="_Static_assert",
  K_Thread_local="_Thread_local"

  TAddAdd="++", TSubSub="--", TArrow="->",
  Tshl="<<", Tshr=">>",
  TGe=">=", TLe="<=",
  TEq="==", TNe="!=",
  TLogicalOr="||",
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

  TNumberLit="<number>",
  TFloatLit="<float>",
  TCharLit="<char>",
  TIdentifier="<identifier>",
  TStringLit="<string>",
  TPPNumber="<pp-number>", # pp-token => pp-number, used by preprocessor
  PPEllipsis="...",
  TEOF="<EOF>"

const tyCounter = CacheCounter"tyCounter"

proc make_ty(): uint32 =
  result = 1'u32 shl tyCounter.value
  tyCounter.inc()

const # optional type tags
  TYINVALID* = 0'u32
  TYAUTO* = make_ty()
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
  TYEXPR* = make_ty() # internal flag

const # basic types
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

const # type alias
  TYCHAR* = TYINT8
  TYSHORT* = TYINT16
  TYINT* = TYINT32
  TYLONG* = TYINT64 # 32 bit in MSVC, 64 bit in GCC/JVM
  TYLONGLONG* = TYINT64
  TYUCHAR* = TYUINT8
  TYUSHORT* = TYUINT16
  TYUINT* = TYUINT32
  TYULONG* = TYUINT64 # 32 bit in MSVC, 64 bit in GCC/JVM
  TYULONGLONG* = TYUINT64
  TYLONGDOUBLE* = TYDOUBLE

type
    CValueKind* = enum
      VI, VF
    CValue* = object
      case k*: CValueKind
      of VI:
        i*: int64
      of VF:
        f*: float64
    PostfixOP* = enum
      PNop="<nop>"
      OPostfixIncrement="++", OPostfixDecrement="--"
    UnaryOP* = enum
      UNop="<nop>"
      OUnaryPlus="+", OUnaryMinus="-", OLogicalNot="!", OAddressOf="&", OAlignOf="_Alignof",
      OPrefixIncrement="++", OPrefixDecrement="--", ODereference="*", OBitwiseNot="~"
    BinOP* = enum
      BNop="<nop>",
      OMemberAccess=".", OPointerMemberAccess="->",
      OMultiplication="*", ODivision="/", Oremainder="%",
      OAddition="+", OSubtraction="-",
      Oshl="<<", Oshr=">>",
      OGe=">=", OGt=">", OLe="<=", OLt="<",
      OEq="==", ONe="!=",
      OBitwiseAnd="&",
      OBitwiseXor="^",
      OBitwiseOr="|",
      OLogicalAnd="&&",
      OLogicalOr="||",
      OAsign="=",
      OAsignAdd="+=",
      OAsignSub="-=",
      OAsignMul="*=",
      OAsignDiv="/=",
      OAsignRem="%=",
      OAsignShl="<<=",
      OAsignShr=">>=",
      OAsignBitAnd="&=",
      OAsignBitOr="|=",
      OAsignBitXor="^=",
      Comma=","
    ValueKind* = enum
      VInt, VFloat, VChar, VOperator, VKeyword, VIdentifier
    Value  = object
      ival*: int # Operator, int
      sval*: string # TIdentifier, string
      fval*: float # float
      utf16*: seq[uint16]
      utf32*: seq[uint32]
    Codepoint* = uint32
    PPToken* = object
      tok*: Token
      s*: string
    PPMacro* = ref object
      tokens*: seq[PPToken]
      case funcmacro*: bool # if a object like macro?
      of true:
        params*: seq[string]
        ivarargs*: bool
      of false:
        discard
    ParseFlags* = enum
      PFNormal=1, PFPP=2
    Parser* = object
      err*: bool
      fstack*: seq[Stream]
      tok*: Token
      line*: int
      col*: int
      val*: Value
      c*: char
      lastc*: uint16
      filename*: string
      path*: string
      macros*: Table[string, PPMacro]
      flags*: ParseFlags
      ppstack*: seq[uint8]
      ok*: bool
      onces*: HashSet[string]
      fs_read*: proc (p: var Parser)
      # 6.2.3 Name spaces of identifiers
      lables*: seq[TableRef[string, int]]
      tags*: seq[TableRef[string, CType]]
      typedefs*: seq[TableRef[string, CType]]
    CTypeSpec* = enum
      TYPRIM,
      TYPOINTER,
      TYSTRUCT,
      TYUNION,
      TYENUM,
      TYBITFIELD,
      TYARRAY,
      TYFUNCTION,
    CType* = ref object
      tags*: uint32
      case spec*: CTypeSpec
      of TYPRIM:
        discard
      of TYPOINTER:
        p*: CType
      of TYSTRUCT, TYUNION:
        sname*: string
        selems*: seq[(string, CType)]
      of TYENUM:
        ename*: string
        eelems*: seq[(string, int)]
      of TYBITFIELD:
        bittype*: CType
        bitsize*: int
      of TYFUNCTION:
        fname*: string
        ret*: CType
        params*: seq[(string, CType)]
      of TYARRAY: # static parameter...
        arrsize*: int
        arrtype*: CType
    ConstantKind* = enum
      CsInt, CsULong, CsLong, CsULongLong, CsLongLong, 
      CsChar8, CsChar16, CsChar32,
      CsDouble, CsFloat,
      CsUTF8, CsUTF16, CsUTF32
    StmtKind* = enum
      SSemicolon, SCompound, SGoto, SContinue, SBreak, SReturn, SExpr, SLabled, SIf, 
      SDoWhile, SWhile, SFor, SSwitch, SStructDecl, SUnionDecl, SEnumDecl, 
      SVarDecl, SStaticAssertDecl, SDefault, SCase
    StmtList* = seq[Stmt] # compound_stmt, { body }
    Stmt* = ref object
      case k*: StmtKind
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
        location*: int
      of SLabled:
        label*: int
        labledstmt*: Stmt
      of SIf:
        iftest*: Expr
        ifbody*: Stmt
        elsebody*: Stmt
      of SDoWhile, SWhile, SSwitch:
        test*: Expr
        body*: Stmt
      of SFor:
        forinit*, forcond*, forincl*: Expr
        forbody*: Stmt
      of SStaticAssertDecl:
        assertexpr*: Expr
        msg*: string
      of SVarDecl:
        discard
      of SStructDecl, SUnionDecl, SEnumDecl:
        stype*: CType
    ExprKind* = enum
      EBin, EUnary, EPostFix, EIntLit, ECharLit, EFloatLit, EStringLit, 
      ESizeOf, EVar, ECondition, ECast, ECall, ESubscript, 
      EAlignof, EGeneric, EInitializer_list
    Expr* = ref object
      case k*: ExprKind
      of EBin:
        lhs*, rhs*: Expr
        bop*: BinOP
      of EUnary:
        uop*: UnaryOP
        uoperand*: Expr
      of EPostFix:
        pop*: PostfixOP
        poperand*: Expr
      of EIntLit, ECharLit:
        ival*: int
      of EFloatLit:
        fval*: float
      of EStringLit, EVar:
        sval*: string
      of ECondition:
        cond*: Expr
        cleft*, cright*: Expr
      of ESizeOf, EAlignof:
        sizeofx*: Expr
      of ECast:
        casttype*: Expr
        castval*: Expr
      of ECall, ESubscript:
        left*, right*: Expr
      of EInitializer_list:
        inits*: seq[Expr]
      of EGeneric:
        selectexpr*: Expr
        selectors*: seq[(Expr, Expr)] # CType is nil if default!

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

proc `$`*(a: Stmt): string

proc `$`*(e: Expr): string

proc `$`*(a: CType): string

proc joinShow5*(a: seq[Stmt]): string =
    result = ""
    for i in a:
        result.add($i)
        result.add(' ')

proc joinShow4*(a: seq[(string, int)]): string =
    if len(a) == 0:
        return ""
    result = "\n\t"
    for i in 0..<(len(a)-1):
        result.add(a[i][0])
        result.add('=')
        result.add($a[i][1])
        result.add(", ")
    result.add(a[^1][0] & '=' & $a[^1][1] & '\n')

proc joinShow3*[A, B](a: seq[(A, B)]): string =
    if len(a) == 0:
        return ""
    result = "\n\t"
    for i in 0..<(len(a)-1):
        result.add($a[i][1])
        result.add(' ')
        result.add($a[i][0])
        result.add(";\n\t")
    result.add($a[^1][1] & ' ' & $a[^1][0] & ";\n")

proc joinShow2*[A, B](a: seq[(A, B)]): string =
    if len(a) == 0:
        return ""
    for i in 0..<(len(a)-1):
        result.add($a[i][1])
        result.add(' ')
        result.add($a[i][0])
        result.add(", ")
    result.add($a[^1][1] & ' ' & $a[^1][0])

proc joinShow*[T](a: seq[T]): string =
    if len(a) == 0:
        return ""
    for i in 0..<(len(a)-1):
        result.add($i)
        result.add(' ')
    result.add($a[^1])

proc `$`*(a: Stmt): string =
  if a == nil:
    return "<nil>"
  case a.k:
  of SCompound:
    '{' & joinShow5(a.stmts) & '}'
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
    $a.exprbody
  of SReturn:
    if a.exprbody == nil:
      "return;"
    else:
      "return " & $a.exprbody & ';'
  of SGoto:
    "goto " & $a.location & ';'
  of SLabled:
    $a.label & ':' & $a.labledstmt
  of SIf:
    "if (" & $a.iftest & ") {" & $a.ifbody & (if a.elsebody==nil: "" else: '{' & $a.elsebody & '}')
  of SWhile:
    "while (" & $a.test & ") {" & $a.body & '}'
  of SSwitch:
    "switch (" & $a.test & ") {" & $a.body & '}'
  of SDoWhile:
    "do {" & $a.body & "} while (" & $a.test & ");"
  of SFor:
    "for (" & (if a.forinit==nil: "" else: $a.forinit) & ';' & 
    (if a.forcond==nil: "" else: $a.forcond) & ';' &
    (if a.forincl==nil: "" else: $a.forincl) & ')' &
    $a.forbody
  of SStaticAssertDecl:
    "_Static_assert(" & $a.assertexpr & a.msg & ");"
  of SVarDecl:
    "<declaration>"
  of SStructDecl, SUnionDecl, SEnumDecl:
    "<some declaration>"

proc `$`*(a: CType): string =
  if a == nil:
    return "<nil>"
  result = ""
  case a.spec:
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
    result.add("enum " & a.ename & '{' & joinShow4(a.eelems) & '}')
  of TYFUNCTION:
    result.add($a.ret & a.fname & " (" & joinShow2(a.params) & ')')
  of TYSTRUCT:
    result.add("struct " & a.sname & " {" & joinShow3(a.selems) & '}')
  of TYUNION:
    result.add("union " & a.sname & " {" & joinShow3(a.selems) & '}')
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

proc tokenToPPToken*(p: var Parser): PPToken =
  PPToken(s: p.val.sval, tok: p.tok)

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
  case e.k:
  of EBin:
    '(' & $e.lhs & ' ' & $e.bop & ' ' & $e.rhs & ')'
  of EPostFix:
    $e.poperand & $e.pop
  of EUnary:
    $e.uop & $e.uoperand
  of ECharLit:
    show(char(e.ival))
  of EIntLit:
    $e.ival
  of EFloatLit:
    $e.fval
  of EStringLit, EVar:
    e.sval
  of ECondition:
    $e.cond & '?' & $e.cleft & ':' & $e.cright
  of EAlignof:
    "_Alignof(" & $e.sizeofx & ')'
  of ESizeOf:
    "sizeof(" & $e.sizeofx & ')'
  of ECast:
    $e.casttype & '(' & $e.castval & ')'
  of ESubscript:
    $e.left & '[' & $e.right & ']'
  of ECall:
    $e.left & '(' & $e.right & ')'
  of EGeneric:
    "_Generic(" & $e.selectexpr & (var s: string;for (tp, e) in e.selectors: s.add($tp & ':' & $e & ',');s) & ')'
  of EInitializer_list:
    "{" & $e.inits & "}"

proc show*(s: seq[PPToken]): string =
  for tok in s:
    case tok.tok:
    of TIdentifier, TPPNumber:
      result &= $tok.s
    else:
      if int(tok.tok) < 255:
        result &= chr(int(tok.tok))
      else:
        result &= $tok.tok
    result &= ' '

proc showToken*(p: var Parser): string =
  case p.tok:
  of TNumberLit: "number => " & $p.val.ival
  of TCharLit: "char => " & show(char(p.val.ival))
  of TIdentifier: "identifier => " & p.val.sval
  of TFloatLit: "float => " & $p.val.fval
  of TStringLit: "UTF " & $p.val.ival & " string => " & p.val.sval
  else:
    if p.tok < T255:
      "token => " & show(chr(int(p.tok)))
    else:
      "keyword => " & $p.tok

proc warning*(p: var Parser, msg: string) =
  stderr.writeLine("\e[33m" & p.filename & ": " & $p.line & '.' & $p.col & ": warning: " & msg & "\e[0m")

proc error*(p: var Parser, msg: string) =
    p.tok = TNul
    if p.err == false:
      stderr.writeLine("\e[31m" & p.filename & ": " & $p.line & '.' & $p.col & ": error: " & msg & "\e[0m")
      p.err = true

proc type_error*(p: var Parser, msg: string) =
    p.tok = TNul
    if p.err == false:
      stderr.writeLine("\e[35m" & p.filename & ": " & $p.line & '.' & $p.col & ": type error: " & msg & "\e[0m")
      p.err = true

proc parse_error*(p: var Parser, msg: string) =
    p.tok = TNul
    if p.err == false:
      stderr.writeLine("\e[34m" & p.filename & ": " & $p.line & '.' & $p.col & ": parse error: " & msg & "\e[0m")
      p.err = true

proc note*(p: var Parser, msg: string) =
    stderr.writeLine("\e[32mnote: " & msg & "\e[0m")
    p.err = true

proc init() =
  for k in int(KwStart)..int(KwEnd):
    gkeywordtable[$cast[Token](k)] = cast[Token](k)

proc isKeyword*(a: string): Token =
  gkeywordtable.getOrDefault(a, TNul)

proc unsafe_utf8_codepoint*(s: cstring): (Codepoint, int) =
    # note: this is unchecked! it will not check whether input is a valid utf8 and overflow!
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

proc writeUTF8toUTF32*(p: var Parser) =
  # TODO: reserve 3 more bytes to prevent bad utf8 terminate access overflow
  p.val.utf32.setLen 0
  var i = 0
  while true:
    if i >= len(p.val.sval):
      break
    let (codepoint, length) = unsafe_utf8_codepoint(cast[cstring](cast[int](cstring(p.val.sval)) + i))
    p.val.utf32.add(codepoint)
    i += length

proc writeUTF8toUTF16*(p: var Parser) =
  writeUTF8toUTF32(p)
  p.val.utf16.setLen 0
  for codepoint in p.val.utf32:
    var c = codepoint
    if c <= 0xFFFF:
      p.val.utf16.add(uint16(c))
    else:
      c -= 0x10000
      p.val.utf16.add(uint16(0xD800 + (c shr 10)))
      p.val.utf16.add(uint16(0xDC00 + (c and 0x3FF)))

init()

when false:
  type
      time_t {.importc: "time_t", nodecl, header: "time.h".} = clonglong 
      tm {.importc: "struct tm", nodecl, header: "time.h".} = object
        tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday, tm_isdst: cint
  
  proc time(t: ptr[time_t]): time_t {.importc: "time", nodecl, header: "time.h".}
  
  proc asctime(time_ptr: ptr[tm]): cstring {.importc: "asctime", nodecl, header: "time.h".}
  
  proc localtime(time_ptr: ptr[time_t]): ptr[tm] {.importc: "localtime", nodecl, header: "time.h".}

iterator getDefines*(): (string, string) =
  let n = now()
  yield ("__DATE__", n.format(initTimeFormat("MMM dd yyyy")))
  yield ("__TIME__", n.format(initTimeFormat("hh:mm:ss")))
  yield ("__STDC__", "1")
  yield ("__STDC_VERSION__", "201710L")
  yield ("__STDC_HOSTED__", "1")
  yield ("__STDC_NO_THREADS__", "1")
  yield ("__STDC_NO_ATOMICS__", "1")
  yield ("__STDC_UTF_16__", "1")
  yield ("__STDC_UTF_32__", "1")
  yield ("__SIZE_TYPE__", "size_t")
  yield ("__INT8_TYPE__", "__int8")
  yield ("__INT16_TYPE__", "__int16")
  yield ("__INT32_TYPE__", "__int32")
  yield ("__INT64_TYPE__", "__int64")
  yield ("__INT_FAST8_TYPE__", "__int8")
  yield ("__INT_FAST16_TYPE__", "__int16")
  yield ("__INT_FAST32_TYPE__", "__int32")
  yield ("__INT_FAST64_TYPE__", "__int64")
  yield ("__UINT_FAST8_TYPE__", "unsigned __int8")
  yield ("__UINT_FAST16_TYPE__", "unsigned __int16")
  yield ("__UINT_FAST32_TYPE__", "unsigned __int32")
  yield ("__UINT_FAST64_TYPE__", "unsigned __int64")
  yield ("__INTPTR_TYPE__", "long long int")
  yield ("__UINTPTR_TYPE__", "unsigned long long int")
  yield ("__CHAR_BIT__", "8")
  case hostOS:
  of "windows":
    yield ("_WIN32", "")
    yield ("WIN32", "")
    if sizeof(cstring) == 8:
      yield ("WIN64", "")
      yield ("_WIN64", "")
  of "macosx":
    yield ("__APPLE__", "")
  of "linux":
    yield ("__linux__", "")
    yield ("__linux", "")
  of "netbsd":
    yield ("__NetBSD__", "")
  of "freebsd":
    yield ("__FreeBSD__", "12")

proc tokensEq(a, b: seq[PPToken]): bool =
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
  result = (a.funcmacro == b.funcmacro) and
  tokensEq(a.tokens, b.tokens) and
  (if a.funcmacro: (a.ivarargs == b.ivarargs) else: true)

proc macro_define*(p: var Parser, name: string, m: PPMacro) =
  if name in p.macros:
    if ppMacroEq(p.macros[name], m):
      p.warning("macro " & name & " redefined")
  p.macros[name] = m

proc macro_defined*(p: var Parser, name: string): bool =
  p.macros.contains(name)

proc macro_undef*(p: var Parser, name: string) =
  p.macros.del(name)

proc macro_find*(p: var Parser, name: string): PPMacro =
  p.macros.getOrDefault(name, nil)

proc resetLine*(p: var Parser) =
    p.col = 1
    inc p.line

proc fs_read*(p: var Parser) =
    if p.fstack.len == 0:
        p.c = '\0'
    else:
        p.c = p.fstack[^1].readChar()
        if p.c == '\0':
            let fd = p.fstack.pop()
            fd.close()
            fs_read(p) # tail call
        elif p.c == '\r':
          resetLine(p)
          p.c = p.fstack[^1].readChar()
          if p.c == '\n':
            p.c = p.fstack[^1].readChar()

proc fs_read_stdin*(p: var Parser) =
    if p.fstack.len == 0:
        stdout.write(">>> ")
        try:
          p.fstack.add(newStringStream(stdin.readLine() & '\n'))
        except IOError:
          p.c = '\0'
          return
    p.c = p.fstack[^1].readChar()
    if p.c == '\0':
        let fd = p.fstack.pop()
        fd.close()
        fs_read_stdin(p) # tail call
    elif p.c == '\r':
      resetLine(p)
      p.c = p.fstack[^1].readChar()
      if p.c == '\n':
        p.c = p.fstack[^1].readChar()

proc stdinParser*(p: var Parser) =
  p.filename = "<stdin>"
  p.path = "/dev/stdin"
  p.fs_read = fs_read_stdin

proc reset*(p: var Parser) =
  p.tok = TNul
  p.col = 1
  p.line = 1
  p.err = false
  p.c = ' '
  p.lastc = 256
  p.flags = PFNormal
  p.ok = true
  p.ppstack.setLen 0
  p.fstack.setLen 0
  p.filename.setLen 0
  p.path.setLen 0
  p.onces.clear()
  p.fs_read = fs_read
  p.tags.setLen 0
  p.typedefs.setLen 0
  p.lables.setLen 0
  p.tags.add(newTable[string, CType]())
  p.typedefs.add(newTable[string, CType]())
  p.lables.add(newTable[string, int]())

proc addString*(p: var Parser, s: string, filename: string) =
  p.fstack.add(newStringStream(s))
  p.filename = (filename) # copy
  p.path = (filename) # copy

proc addFile*(p: var Parser, fd: File, filename: string) =
  p.fstack.add(newFileStream(fd))
  p.filename = (filename) # copy
  p.path = (filename) # copy

proc closeParser*(p: var Parser) =
  for fd in p.fstack:
    fd.close()

proc getTag(p: var Parser, name: string): CType =
  for i in (len(p.tags)-1) .. 0:
    result = p.tags[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc gettypedef*(p: var Parser, name: string): CType =
  for i in (len(p.typedefs)-1) .. 0:
    result = p.typedefs[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc getLabel*(p: var Parser, name: string): int =
  result = -1
  for i in (len(p.lables)-1) .. 0:
    result = p.lables[i].getOrDefault(name, -1)
    if result != -1:
      return result

proc putLable*(p: var Parser, name: string, t: int) =
    p.lables[^1][name] = t

proc getstructdef*(p: var Parser, name: string): CType =
    result = getTag(p, name)
    if result == nil:
        p.type_error("variable has incomplete type `struct " & name & '`')
        p.note("in forward references of `struct " & name & '`')
        p.note("add struct definition before use")
    elif result.spec != TYSTRUCT:
        p.type_error(name & " is not a struct")

proc putstructdef*(p: var Parser, t: CType) =
    let o = getTag(p, t.sname)
    if o != nil:
        p.error("struct " & t.sname & " aleady defined")
    else:
        p.tags[^1][t.sname] = t

proc getenumdef*(p: var Parser, name: string): CType =
    result = getTag(p, name)
    if result == nil:
        p.type_error("variable has incomplete type `enum " & name & '`')
        p.note("in forward references of `enum " & name & '`')
        p.note("add struct definition before use")
    elif result.spec != TYENUM:
        p.type_error(name & " is not a enum")

proc putenumdef*(p: var Parser, t: CType) =
    let o = getTag(p, t.ename)
    if o != nil:
        p.error("enum " & t.ename & " aleady defined")
    else:
        p.tags[^1][t.ename] = t

proc getuniondef*(p: var Parser, name: string): CType =
    result = getTag(p, name)
    if result == nil:
        p.type_error("variable has incomplete type `union " & name & '`')
        p.note("in forward references of `union " & name & '`')
        p.note("add union definition before use")
    elif result.spec != TYUNION:
        p.type_error(name & " is not a union")

proc putuniondef*(p: var Parser, t: CType) =
    let o = getTag(p, t.sname)
    if o != nil:
        p.error("`union` " & t.sname & " aleady defined")
    else:
        p.tags[^1][t.sname] = t

proc getsymtype*(p: var Parser, name: string): CType =
  return gettypedef(p, name)

# typedef, symbol
proc putsymtype*(p: var Parser, name: string, t: CType) =
  let ty = getsymtype(p, name)
  if ty != nil:
    p.error(name & " redeclared")
    return
  p.typedefs[^1][name] = t

proc enterBlock*(p: var Parser) =
  p.typedefs.add(newTable[string, CType]())
  p.tags.add(newTable[string, CType]())
  p.lables.add(newTable[string, int]())

proc leaveBlock*(p: var Parser) =
  discard p.typedefs.pop()
  discard p.tags.pop()
  discard p.lables.pop()

proc checkOnce*(p: var Parser, filename: string): bool =
    return p.onces.contains(filename)

proc addOnce*(p: var Parser) =
    p.onces.incl p.path

proc addInclude*(p: var Parser, filename: string): bool =
    if checkOnce(p, filename) == true:
        return true
    let s = newFileStream(filename)
    if s == nil:
        return false
    p.fstack.add(s)
    p.filename = filename # copy
    p.path = filename # copy
    return true
