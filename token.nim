import std/[streams, tables, times, sets, macrocache]

type
  Location = object
    line: int
    col: int

type
  intmax_t* = int64

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
  
  TEllipsis="...", # varargs
  TNumberLit="<number>",
  TFloatLit="<float>",
  TCharLit="<char>",
  TIdentifier="<identifier1>",
  TIdentifier2="<identifier>",
  TStringLit="<string>",
  TPPNumber="<pp-number>", # pp-token => pp-number, used by preprocessor
  PPPlaceholder="<placeholder>",
  PPSharp="#",
  PPSharpSharp="##",
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
    Codepoint* = uint32
    PPMacroFlags* = enum
      MOBJ, MPragma, MFUNC
    PPMacro* = ref object
      tokens*: seq[TokenV]
      case flags*: PPMacroFlags # if a object like macro?
      of MFUNC:
        params*: seq[string]
        ivarargs*: bool
      of MOBJ, MPragma:
        discard
    ParseFlags* = enum
      PFNormal=1, PFPP=2
    TokenVTags* = enum
      TVNormal, TVSVal, TVIVal, TVFval
    TokenV* = ref object
      tok*: Token
      case tags*: TokenVTags
      of TVNormal:
        discard
      of TVSVal:
        s*: string
      of TVFval:
        f*: float
      of TVIVal:
        i*: int
    Parser* = ref object
      err*: bool
      fstack*: seq[Stream]
      pathstack*: seq[string]
      filenamestack*: seq[string]
      tok*: TokenV
      line*: int
      col*: int
      c*: char
      lastc*: uint16
      filename*: string
      path*: string # the real path
      macros*: Table[string, PPMacro]
      flags*: ParseFlags
      ppstack*: seq[uint8]
      ok*: bool
      onces*, expansion_list*: HashSet[string]
      fs_read*: proc () # 6.2.3 Name spaces of identifiers
      lables*: seq[TableRef[string, (int, Location)]]
      tags*: seq[TableRef[string, (CType, Location)]]
      typedefs*: seq[TableRef[string, (CType, Location)]]
      tokenq*: seq[TokenV]
      counter*: int
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
        eelems*: seq[(string, intmax_t)]
      of TYBITFIELD:
        bittype*: CType
        bitsize*: intmax_t
      of TYFUNCTION:
        fname*: string
        ret*: CType
        params*: seq[(string, CType)]
      of TYARRAY: # static parameter...
        arrsize*: intmax_t
        arrtype*: CType
    ConstantKind* = enum
      CsInt, CsULong, CsLong, CsULongLong, CsLongLong, 
      CsChar8, CsChar16, CsChar32,
      CsDouble, CsFloat,
      CsUTF8, CsUTF16, CsUTF32
    StmtKind* = enum
      SSemicolon, SCompound, SGoto, SContinue, SBreak, SReturn, SExpr, SLabled, SIf, 
      SDoWhile, SWhile, SFor, SSwitch, SStructDecl, SUnionDecl, SEnumDecl, 
      SVarDecl, SStaticAssertDecl, SDefault, SCase, SFunction, SVarDecl1
    StmtList* = seq[Stmt] # compound_stmt, { body }
    Stmt* = ref object
      case k*: StmtKind
      of SFunction:
        funcname*: string
        functy*: CType
        funcbody*: Stmt
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
      of SStaticAssertDecl:
        assertexpr*: Expr
        msg*: string
      of SVarDecl1:
        var1name*: string
        var1type*: CType
      of SVarDecl:
        vars*: seq[(string, CType, Expr)]
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
        sizeofty*: CType
      of ECast:
        casttype*: CType
        castval*: Expr
      of ECall:
        callfunc*: Expr
        callargs*: seq[Expr]
      of ESubscript:
        left*, right*: Expr
      of EInitializer_list:
        inits*: seq[Expr]
      of EGeneric:
        selectexpr*: Expr
        selectors*: seq[(Expr, Expr)] # CType is nil if default!

var p*: Parser = nil

proc setParser*(a: var Parser) =
  p = a

proc getParser*(): var Parser = 
  p

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

proc joinShow5*(a: seq[Stmt]): string =
    result = ""
    for i in a:
        result.add($i)
        result.add(' ')

proc joinShow4*(a: seq[(string, intmax_t)]): string =
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

proc joinShow*[T](a: seq[T], c: string = " "): string =
    if len(a) == 0:
        return ""
    for i in 0..<(len(a)-1):
        result.add($a[i])
        result.add(c)
    result.add($a[^1])

proc `$`*(a: Stmt): string =
  if a == nil:
    return "<nil>"
  case a.k:
  of SFunction:
    "Function " & a.funcname & " : " & $a.functy & $a.funcbody
  of SVarDecl1:
    "<SVarDecl1>"
  of SVarDecl:
    joinShow6(a.vars)
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
    "goto " & a.location & ';'
  of SLabled:
    a.label & ':' & $a.labledstmt
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
  of EVar:
    e.sval
  of EStringLit:
    '"' & (e.sval) & '"'
  of ECondition:
    $e.cond & '?' & $e.cleft & ':' & $e.cright
  of EAlignof:
    "_Alignof(" & (if e.sizeofx != nil: $e.sizeofx else: $e.sizeofty)  & ')'
  of ESizeOf:
    "sizeof(" & (if e.sizeofx != nil: $e.sizeofx else: $e.sizeofty) & ')'
  of ECast:
    $e.casttype & '(' & $e.castval & ')'
  of ESubscript:
    $e.left & '[' & $e.right & ']'
  of ECall:
    $e.callfunc & '(' & $joinShow(e.callargs, ", ") & ')'
  of EGeneric:
    "_Generic(" & $e.selectexpr & (var s: string;for (tp, e) in e.selectors: s.add($tp & ':' & $e & ',');s) & ')'
  of EInitializer_list:
    "{" & $e.inits & "}"

proc showToken*(): string =
  case p.tok.tok:
  of TNumberLit: "number => " & $p.tok.i
  of TCharLit: "char => " & show(char(p.tok.i))
  of TIdentifier: "identifier => " & p.tok.s
  of TFloatLit: "float => " & $p.tok.f
  of TStringLit: "UTF " & $p.tok.i & " string => " & p.tok.s
  else:
    if p.tok.tok < T255:
      "token => " & show(chr(int(p.tok.tok)))
    else:
      "keyword => " & $p.tok.tok

proc `$`*(loc: Location): string =
  $loc.line & ':' & $loc.col

proc warning*(msg: string) =
  stderr.writeLine("\e[33m" & p.filename & ": " & $p.line & '.' & $p.col & ": warning: " & msg & "\e[0m")

proc error*(msg: string) =
    p.tok = TokenV(tok: TNul, tags: TVNormal)
    if p.err == false:
      stderr.writeLine("\e[31m" & p.filename & ": " & $p.line & '.' & $p.col & ": error: " & msg & "\e[0m")
      p.err = true

proc type_error*(msg: string) =
    p.tok = TokenV(tok: TNul, tags: TVNormal)
    if p.err == false:
      stderr.writeLine("\e[35m" & p.filename & ": " & $p.line & '.' & $p.col & ": type error: " & msg & "\e[0m")
      p.err = true

proc parse_error*(msg: string) =
    p.tok = TokenV(tok: TNul, tags: TVNormal)
    if p.err == false:
      stderr.writeLine("\e[34m" & p.filename & ": " & $p.line & '.' & $p.col & ": parse error: " & msg & "\e[0m")
      p.err = true

proc note*(msg: string) =
    stderr.writeLine("\e[32mnote: " & msg & "\e[0m")

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
#[ 
proc writeUTF8toUTF32*() =
  # TODO: reserve 3 more bytes to prevent bad utf8 terminate access overflow
  p.val.utf32.setLen 0
  var i = 0
  while true:
    if i >= len(p.val.sval):
      break
    let (codepoint, length) = unsafe_utf8_codepoint(cast[cstring](cast[int](cstring(p.val.sval)) + i))
    p.val.utf32.add(codepoint)
    i += length

proc writeUTF8toUTF16*() =
  writeUTF8toUTF32()
  p.val.utf16.setLen 0
  for codepoint in p.val.utf32:
    var c = codepoint
    if c <= 0xFFFF:
      p.val.utf16.add(uint16(c))
    else:
      c -= 0x10000
      p.val.utf16.add(uint16(0xD800 + (c shr 10)))
      p.val.utf16.add(uint16(0xDC00 + (c and 0x3FF)))
]#

init()

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

proc macro_define*(name: string, m: PPMacro) =
  let pr = p.macros.getOrDefault(name, nil)
  if pr != nil:
    if not ppMacroEq(pr, m):
      warning("macro " & name & " redefined")
  p.macros[name] = m

proc macro_defined*(name: string): bool =
  p.macros.contains(name)

proc macro_undef*(name: string) =
  p.macros.del(name)

proc macro_find*(name: string): PPMacro =
  p.macros.getOrDefault(name, nil)

proc resetLine*() =
    p.col = 1
    inc p.line

proc fs_read*() =
    if p.fstack.len == 0:
        p.c = '\0'
    else:
        p.c = p.fstack[^1].readChar()
        if p.c == '\0':
            let fd = p.fstack.pop()
            p.path = p.pathstack.pop()
            p.filename = p.filenamestack.pop()
            fd.close()
            fs_read() # tail call
        elif p.c == '\r':
          resetLine()
          p.c = p.fstack[^1].readChar()
          if p.c == '\n':
            p.c = p.fstack[^1].readChar()

proc fs_read_stdin*() =
    if p.fstack.len == 0:
        stdout.write(">>> ")
        try:
          p.fstack.add(newStringStream(stdin.readLine() & '\n'))
        except IOError:
          p.c = '\0'
          return
        p.filenamestack.add(p.filename)
        p.pathstack.add(p.path)
    p.c = p.fstack[^1].readChar()
    if p.c == '\0':
        let fd = p.fstack.pop()
        fd.close()
        p.filename = p.filenamestack.pop()
        p.path = p.pathstack.pop()
        fs_read_stdin() # tail call
    elif p.c == '\r':
      resetLine()
      p.c = p.fstack[^1].readChar()
      if p.c == '\n':
        p.c = p.fstack[^1].readChar()

proc stdinParser*() =
  p.filename = "<stdin>"
  p.path = "/dev/stdin"
  p.fs_read = fs_read_stdin

proc reset*() =
  p.counter = 0
  p.tok = TokenV(tok: TNul, tags: TVNormal)
  p.col = 1
  p.line = 1
  p.err = false
  p.c = ' '
  p.lastc = 256
  p.flags = PFNormal
  p.ok = true
  p.ppstack.setLen 0
  p.fstack.setLen 0
  p.filenamestack.setLen 0
  p.pathstack.setLen 0
  p.macros.clear()
  p.filename.setLen 0
  p.path.setLen 0
  p.onces.clear()
  p.expansion_list.clear()
  p.fs_read = fs_read
  p.tags.setLen 0
  p.typedefs.setLen 0
  p.lables.setLen 0
  p.tags.add(newTable[string, typeof(p.tags[0][""])]())
  p.typedefs.add(newTable[string, typeof(p.typedefs[0][""])]())
  p.lables.add(newTable[string, typeof(p.lables[0][""])]())
  p.tokenq.setLen 0
  for (name, v) in getDefines():
    p.macros[name] = PPMacro(tokens: v, flags: MOBJ)

proc newParser*() =
  ## create a Parser, and call `setParser proc<#setParser,Parser>`_, then call `reset proc<#reset>`_
  var p = Parser()
  setParser(p)
  reset()

proc addString*(s: string, filename: string) =
  p.fstack.add(newStringStream(s))
  p.filenamestack.add(p.filename)
  p.pathstack.add(p.path)
  p.filename = (filename) # copy
  p.path = (filename) # copy

proc addFile*(fd: File, filename: string) =
  p.fstack.add(newFileStream(fd))
  p.filenamestack.add(p.filename)
  p.pathstack.add(p.path)
  p.filename = (filename) # copy
  p.path = (filename) # copy

proc closeParser*() =
  for fd in p.fstack:
    fd.close()

proc getTag(name: string): (CType, Location) =
  for i in countdown(len(p.tags)-1, 0):
    result = p.tags[i].getOrDefault(name, (nil, Location()))
    if result[0] != nil:
      return result

proc gettypedef*(name: string): (CType, Location) =
  for i in countdown(len(p.typedefs)-1, 0):
    result = p.typedefs[i].getOrDefault(name, (nil, Location()))
    if result[0] != nil:
      return result

proc getLabel*(name: string): (int, Location) =
  for i in countdown(len(p.lables)-1, 0):
    result = p.lables[i].getOrDefault(name, (-1, Location()))
    if result[0] != -1:
      return result
  return (-1, Location())

proc putLable*(name: string, t: int) =
    let (l, loc) = getLabel(name)
    if  l != -1:
      error("duplicate label: " & name)
      note(name & "was defined at " & $loc)
      return
    p.lables[^1][name] = (t, Location(line: p.line, col: p.col))

proc getstructdef*(name: string): CType =
    let res = getTag(name)
    result = res[0]
    if result == nil:
        type_error("variable has incomplete type `struct " & name & '`')
        note("in forward references of `struct " & name & '`')
        note("add struct definition before use")
    elif result.spec != TYSTRUCT:
        type_error(name & " is not a struct")

proc putstructdef*(t: CType) =
    let (o, loc) = getTag(t.sname)
    if o != nil:
        error("struct " & t.sname & " aleady defined")
        note(o.sname & "was defined at " & $loc)
    else:
        p.tags[^1][t.sname] = (t, Location(line: p.line, col: p.col))

proc getenumdef*(name: string): CType =
    let res = getTag(name)
    result = res[0]
    if result == nil:
        type_error("variable has incomplete type `enum " & name & '`')
        note("in forward references of `enum " & name & '`')
        note("add struct definition before use")
    elif result.spec != TYENUM:
        type_error(name & " is not a enum")

proc putenumdef*(t: CType) =
    let (o, loc) = getTag(t.ename)
    if o != nil:
        error("enum " & t.ename & " aleady defined")
        note(o.ename & "was defined at " & $loc)
    else:
        p.tags[^1][t.ename] = (t, Location(line: p.line, col: p.col))

proc getuniondef*(name: string): CType =
    let res = getTag(name)
    result = res[0]
    if result == nil:
        type_error("variable has incomplete type `union " & name & '`')
        note("in forward references of `union " & name & '`')
        note("add union definition before use")
    elif result.spec != TYUNION:
        type_error(name & " is not a union")

proc putuniondef*(t: CType) =
    let o = getTag(t.sname)
    if o[0] != nil:
        error("`union` " & t.sname & " aleady defined")
        note(o[0].sname & "was defined at " & $o[1])
    else:
        p.tags[^1][t.sname] = (t, Location(line: p.line, col: p.col))

proc getsymtype*(name: string): CType =
  result = gettypedef(name)[0]

# typedef, symbol
proc putsymtype*(name: string, t: CType) =
  let ty = getsymtype(name)
  if ty != nil:
    error(name & " redeclared")
    return
  p.typedefs[^1][name] = (t, Location(line: p.line, col: p.col))

proc enterBlock*() =
  p.typedefs.add(newTable[string, typeof(p.typedefs[0][""])]())
  p.tags.add(newTable[string, typeof(p.tags[0][""])]())
  p.lables.add(newTable[string, typeof(p.lables[0][""])]())

proc leaveBlock*() =
  discard p.typedefs.pop()
  discard p.tags.pop()
  discard p.lables.pop()

proc checkOnce*(filename: string): bool =
    return p.onces.contains(filename)

proc addOnce*() =
    p.onces.incl p.path

proc addInclude*(filename: string): bool =
    if checkOnce(filename) == true:
        return true
    let s = newFileStream(filename)
    if s == nil:
        return false
    p.fstack.add(s)
    p.filenamestack.add(p.filename)
    p.pathstack.add(p.path)
    p.filename = filename # copy
    p.path = filename # copy
    return true

proc putToken*() = 
    echo "putToken: ", p.tok[]
    p.tokenq.add(p.tok)

proc beginExpandMacro*(a: string) =
  p.expansion_list.incl a

proc endExpandMacro*(a: string) =
  p.expansion_list.excl a

proc isMacroInUse*(a: string): bool =
  p.expansion_list.contains(a)

proc getMacro*(name: string): PPMacro =
  case name:
  of "__COUNTER__":
    result = PPMacro(tokens: @[TokenV(tok: TPPNumber, tags: TVSVal, s: $p.counter)], flags: MOBJ)
    inc p.counter
  of "__LINE__":
    result = PPMacro(tokens: @[TokenV(tok: TPPNumber, tags: TVSVal, s: $p.line)], flags: MOBJ)
  of "__FILE__":
    result = PPMacro(tokens: @[TokenV(tok: TStringLit, tags: TVSVal, s: p.filename)], flags: MOBJ)
  of "__DATE__":
    let n = now()
    result = PPMacro(tokens: @[TokenV(tok: TStringLit, tags: TVSVal, s: n.format(initTimeFormat("MMM dd yyyy")))], flags: MOBJ)
  of "__TIME__":
    let n = now()
    result = PPMacro(tokens: @[TokenV(tok: TStringLit, tags: TVSVal, s: n.format(initTimeFormat("hh:mm:ss")))], flags: MOBJ)
  else:
    result = p.macros.getOrDefault(name, nil)
