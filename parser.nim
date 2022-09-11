## C's Lexer, preprocessor(CPP) and recursive descent parser
##
## based on ISO C grammar for details
##
## see 
##
##  <https://gcc.gnu.org/onlinedocs/cpp>
##
##  <https://gcc.gnu.org/onlinedocs/gcc-10.4.0/cpp/> 
##
## for details
## 
## the main export function is runParser
##
## translation-unit:
##
## Source stream => Lexer => CPP => Parser => code generator => Optmizer => Write output(LLVM IR, Assembly)
##
## `myop.h` is defined as
## ```C
## #define myopand(a, b) ((a) && (b))
## #define myopor(a, b) ((a) || (b))
## #define myopnot(a) (!(a))
## #define myopneg(a) (-(a))
## ```

import config, stream, core, ast, token, operators, location
import std/[math, tables, sets, editdistance]
from std/sequtils import count

type
    Parser* = ref object
      currentfunctionRet*, currentInitTy*, currentCase*: CType
      pfunc*: string
      currentAlign*: uint32
      fstack*: seq[Stream]
      filenamestack*, pathstack*: seq[string]
      locstack*: seq[Location]
      tok*: TokenV
      line*: int
      col*: int
      c*: char
      want_expr*: bool
      lastc*: uint16
      filename*, path*: string
      macros*: Table[string, PPMacro]
      flags*: ParseFlags
      ppstack*: seq[uint8]
      ok*: bool
      onces*, expansion_list*: HashSet[string]
      # 6.2.3 Name spaces of identifiers
      lables*: seq[HashSet[string]]
      tags*: seq[TableRef[string, Info]]
      typedefs*: seq[TableRef[string, Info]]
      tokenq*: seq[TokenV]
      counter*: int
      retTy*: CType # current return type
      type_error*: bool
      eval_error*: bool
      parse_error*: bool
      bad_error*: bool

var p*: Parser = nil

proc make_tok*(op: Token) {.inline.} =
    p.tok = TokenV(tok: op, tags: TVNormal)

proc make_ch_lit*(ch: char) {.inline.} =
    p.tok.i = ch.int

when TYINT == TYINT32:
    const sizeofint = 4.culonglong
else:
    const sizeofint = 8.culonglong

proc type_error*(msg: string) =
    cstderr << "\e[35m" & p.filename & ":" & $p.line & '.' & $p.col & ": type error: " & msg & "\e[0m\n"
    if p.fstack.len > 0:
        printSourceLine(p.fstack[^1], p.line)
    p.type_error = true

proc error_incomplete*(ty: CType) =
  let s = if ty.tag == TYSTRUCT:  "struct" else: (if ty.tag == TYUNION: "union" else: "enum")
  type_error("use of incomplete type '" & s & " " & ty.name & '\'')

proc compatible*(p, expected: CType): bool

proc parse_error*(msg: string) =
    # p.tok = TokenV(tok: TNul, tags: TVNormal)
    if p.parse_error == false and p.type_error == false:
      cstderr << "\e[34m" & p.filename & ":" & $p.line & '.' & $p.col & ": parse error: " & msg & "\e[0m\n"
      if p.fstack.len > 0:
          printSourceLine(p.fstack[^1], p.line)
      p.parse_error = true

proc setParser*(a: var Parser) =
  p = a

proc getParser*(): var Parser = 
  p

proc binop*(a: Expr, op: BinOP, b: Expr, ty: CType): Expr = 
    ## construct a binary operator
    Expr(k: EBin, lhs: a, rhs: b, bop: op, ty: ty)

proc unary*(e: Expr, op: UnaryOP, ty: CType): Expr = 
    ## construct a unary operator
    Expr(k: EUnary, uop: op, uoperand: e, ty: ty)

proc showToken*(): string =
  case p.tok.tok:
  of TNumberLit: "number " & $p.tok.i
  of TPPNumber: "number" & p.tok.s
  of TCharLit: "char " & show(char(p.tok.i))
  of TIdentifier, CPPIdent: "identifier " & p.tok.s
  of TSpace: "space"
  of PPSharp: "'#'"
  of PPPlaceholder: "<placeholder>"
  of PPSharpSharp: "'##'"
  of TFloatLit: "float " & $p.tok.f
  of TEllipsis2: "'..'"
  of TEllipsis: "'...'"
  of TStringLit: "string \"" & p.tok.s & '"'
  of TEOF: "<EOF>"
  of TNul: "<null>"
  else:
    if p.tok.tok < T255:
      "char " & show(chr(int(p.tok.tok)))
    else:
      "keyword " & $p.tok.tok

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

proc warning*(msg: string) =
  if ord(app.verboseLevel) >= ord(WWarning):
    cstderr << "\e[33m" & p.filename & ": " & $p.line & '.' & $p.col & ": warning: " & msg & "\e[0m\n"

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

proc expect*(msg: string) =
    ## emit `expect ...` error message
    parse_error("expect " & msg & ", got " & showToken())

proc expectExpression*() =
    expect("expression")

proc expectStatement*() =
    expect("statement")

proc expectLB*() =
    expect("'('")

proc expectRB*() =
    expect("')'")

proc isFloating*(ty: CType): bool =
    bool(ty.tags and (TYFLOAT or TYDOUBLE))

proc isSigned*(ty: CType): bool =
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

proc getsizeof*(ty: CType): culonglong =
    if ty.spec == TYINCOMPLETE:
        error_incomplete(ty)
        return 0
    if ty.spec == TYPRIM:
        if (ty.tags and TYVOID) != 0:
            type_error("cannot get sizeof void")
            return 0
        if (ty.tags and (TYINT8 or TYUINT8)) != 0:
          return 1
        if (ty.tags and (TYINT16 or TYUINT16)) != 0:
          return 2
        if (ty.tags and (TYINT32 or TYUINT32)) != 0:
          return 4
        if (ty.tags and (TYINT64 or TYUINT64)) != 0:
            return 8
    if ty.spec == TYBITFIELD:
        type_error("invalid application of 'sizeof' to bit-field")
        return 0
    if ty.spec == TYFUNCTION:
        type_error("invalid application of 'sizeof' to a function type")
        return 0
    if ty.spec == TYENUM:
        return sizeofint
    if ty.spec == TYPOINTER:
        return app.pointersize
    return app.getSizeof(ty)

proc getsizeof*(e: Expr): culonglong =
    getsizeof(e.ty)

proc getAlignof*(ty: CType): culonglong =
    if ty.spec == TYFUNCTION:
        return 1
    if ty.spec == TYENUM:
        return sizeofint
    if ty.spec == TYINCOMPLETE:
        error_incomplete(ty)
        return 0
    app.getAlignOf(ty)

proc getAlignof*(e: Expr): culonglong =
    getAlignof(e.ty)

proc checkInteger*(a: CType): bool =
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

proc intRank*(a: uint32): int =
    if bool(a and TYBOOL):
        return 1
    if bool(a and (TYINT8 or TYUINT8)):
        return 2
    if bool(a and (TYINT16 or TYUINT16)):
        return 3
    if bool(a and (TYINT32 or TYUINT32)):
        return 4
    return 5

proc err*(): bool =
  p.type_error or p.parse_error or p.eval_error

proc note*(msg: string) =
    if ord(app.verboseLevel) >= ord(WNote):
      cstderr << "\e[32mnote: " & msg & "\e[0m\n"

proc error*(msg: string) =
    if p.bad_error == false:
      cstderr << "\e[31m" & p.filename & ":" & $p.line & '.' & $p.col & ": error: " & msg & "\e[0m\n"
      if p.fstack.len > 0:
          printSourceLine(p.fstack[^1], p.line)
      p.bad_error = true

proc inTheExpression*(e: Expr) =
    ## emit in the expression message
    note("in the expression '" & $e & '\'')

proc enterBlock*() =
  p.typedefs.add(newTable[string, typeof(p.typedefs[0][""])]())
  p.tags.add(newTable[string, typeof(p.tags[0][""])]())
  p.lables.add(initHashSet[string]())

proc isTopLevel(): bool =
    p.typedefs.len == 1

proc leaveBlock*() =
  if isTopLevel() == false:
    for (name, i) in p.typedefs[^1].pairs():
      if not bool(i.tag and INFO_USED):
          warning("declared but not used: '" & name & '\'')
          note("declared at " & $i.loc)
  discard p.typedefs.pop()
  discard p.tags.pop()
  discard p.lables.pop()

proc reset*() =
  p.bad_error = false
  p.eval_error = false
  p.parse_error = false
  p.type_error = false
  p.want_expr = false
  p.counter = 0
  p.tok = TokenV(tok: TNul, tags: TVNormal)
  p.col = 1
  p.line = 1
  p.c = ' '
  p.lastc = 256
  p.flags = PFNormal
  p.ok = true
  #p.pathstack.setLen 0
  #p.ppstack.setLen 0
  #p.fstack.setLen 0
  #p.filenamestack.setLen 0
  #p.locstack.setLen 0
  #p.macros.clear()
  #p.filename.setLen 0
  #p.path.setLen 0
  #p.onces.clear()
  #p.lables.clear()
  #p.expansion_list.clear()
  #p.tags.setLen 0
  #p.typedefs.setLen 0
  #p.lables.setLen 0
  #p.tokenq.setLen 0
  enterBlock()
  for (name, v) in getDefines():
    p.macros[name] = PPMacro(tokens: v, flags: MOBJ)

proc finishParsing() =
    leaveBlock()

proc addString*(s: string, filename: string) =
  p.fstack.add(newStringStream(s))
  p.filenamestack.add(p.filename)
  p.pathstack.add(p.path)
  p.locstack.add(Location(line: p.line, col: p.col))
  p.filename = filename
  p.path = filename
  p.line = 1
  p.col = 1

proc addStdin*() =
  p.fstack.add(newStdinStream())
  p.filenamestack.add(p.filename)
  p.pathstack.add(p.path)
  p.locstack.add(Location(line: p.line, col: p.col))
  p.filename = "<stdin>"
  p.path = "/dev/stdin"
  p.line = 1
  p.col = 1

proc addFile*(fd: File, filename: string) =
  let f = newFileStream(fd)
  p.fstack.add(f)
  p.filenamestack.add(p.filename)
  p.pathstack.add(p.path)
  p.locstack.add(Location(line: p.line, col: p.col))
  p.filename = filename
  p.path = filename
  p.line = 1
  p.col = 1

proc addFile*(filename: string) =
  let f = newFileStream(filename)
  if f == nil:
    return
  p.fstack.add(f)
  p.filenamestack.add(p.filename)
  p.pathstack.add(p.path)
  p.locstack.add(Location(line: p.line, col: p.col))
  p.filename = filename
  p.path = filename
  p.line = 1
  p.col = 1

proc closeParser*() =
  for fd in p.fstack:
    fd.close()

proc getTag(name: string): Info =
  for i in countdown(len(p.tags)-1, 0):
    result = p.tags[i].getOrDefault(name, nil)
    if result != nil:
      result.tag = result.tag or INFO_USED
      return result

proc gettypedef*(name: string): Info =
  for i in countdown(len(p.typedefs)-1, 0):
    result = p.typedefs[i].getOrDefault(name, nil)
    if result != nil:
      result.tag = result.tag or INFO_USED
      return result

proc hasLabel*(name: string): bool =
  for i in countdown(len(p.lables)-1, 0):
    if name in p.lables[i]:
        return true
  return false

proc putLable*(name: string, t: int) =
    if hasLabel(name):
      error("duplicate label: " & name)
      return
    p.lables[^1].incl name

proc getstructdef*(name: string): CType =
    var r = getTag(name)
    if r == nil:
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYSTRUCT, name: name)
    elif r.ty.spec != TYSTRUCT:
        type_error(name & " is not a struct")
    else:
        result = r.ty

proc putstructdef*(t: CType) =
    let o = getTag(t.sname)
    if o != nil:
        error("struct " & t.sname & " aleady defined")
        note(t.sname & "was defined at " & $o.loc)
    else:
        p.tags[^1][t.sname] = Info(ty: t, loc: Location(line: p.line, col: p.col))

proc getenumdef*(name: string): CType =
    let r = getTag(name)
    if r == nil:
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYENUM, name: name)
    elif r.ty.spec != TYENUM:
        type_error(name & " is not a enum")
    else:
        result = r.ty

proc putenumdef*(t: CType) =
    let o = getTag(t.ename)
    if o != nil:
        error("enum " & t.ename & " aleady defined")
        note(t.ename & "was defined at " & $o.loc)
    else:
        p.tags[^1][t.ename] = Info(ty: t, loc: Location(line: p.line, col: p.col))

proc getuniondef*(name: string): CType =
    let r = getTag(name)
    if r == nil:
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYUNION, name: name)
    elif r.ty.spec != TYUNION:
        type_error(name & " is not a union")
    else:
        result = r.ty

proc putuniondef*(t: CType) =
    let o = getTag(t.sname)
    if o != nil:
        error("`union` " & t.sname & " aleady defined")
        note(t.sname & "was defined at " & $o.loc)
    else:
        p.tags[^1][t.sname] = Info(ty: t, loc: Location(line: p.line, col: p.col))

proc getsymtype*(name: string): CType =
  let o = gettypedef(name)
  if o == nil:
    nil
  else:
    o.ty

# function definition
proc putsymtype3*(name: string, t: CType) =
  let ty = p.typedefs[^1].getOrDefault(name, nil)
  if ty != nil:
    type_error("function " & name & " redefined")
  p.typedefs[^1][name] = Info(ty: t, loc: Location(line: p.line, col: p.col))

# function declaration
proc putsymtype2*(name: string, t: CType) =
  let ty = p.typedefs[^1].getOrDefault(name, nil)
  if ty != nil:
    if not compatible(t, ty.ty):
        type_error("conflicting types for function declaration '" & name &  "'")
  p.typedefs[^1][name] = Info(ty: t, loc: Location(line: p.line, col: p.col))

# typedef, variable
proc putsymtype*(name: string, t: CType) =
  if t.spec == TYFUNCTION:
    putsymtype2(name, t)
    return
  let ty = p.typedefs[^1].getOrDefault(name, nil)
  if ty != nil and not bool(ty.ty.tags and TYEXTERN):
    type_error(name & " redeclared")
    return
  p.typedefs[^1][name] = Info(ty: t, loc: Location(line: p.line, col: p.col))

proc checkOnce*(filename: string): bool =
    return p.onces.contains(filename)

proc istype(a: Token): bool =
    if a in (declaration_specifier_set + {Kstruct, Kenum, Kunion, K_Alignas}):
        return true
    if a == TIdentifier:
      let o = gettypedef(p.tok.s)
      return o != nil and (o.ty.tags and TYTYPEDEF) != 0
    return false

proc addOnce*() =
    p.onces.incl p.path

proc addInclude*(filename: string, isInclude: bool) =
    if checkOnce(filename) == true:
        return
    let s = newFileStream(filename)
    if s == nil:
        core.error()
        perror(filename)
        if isInclude:
            note("in the #include directive")
        return
    p.fstack.add(s)
    p.filenamestack.add(p.filename)
    p.pathstack.add(p.path)
    p.locstack.add(Location(line: p.line, col: p.col))
    p.filename = filename
    p.path = filename
    p.line = 1
    p.col = 1


proc putToken*() = 
    p.tokenq.add(p.tok)

proc isprint*(a: cint): cint {.importc: "isprint", nodecl, header: "ctype.h".}

const hexs*: cstring = "0123456789ABCDEF"

proc intcast*(e: Expr, to: CType): Expr = 
    if bool(to.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64)) and 
       bool(e.ty.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64 or TYBOOL)):
        if to.tags == e.ty.tags:
            return Expr(k: ECast, castop: BitCast, castval: e, ty: to)
        if intRank(to.tags) > intRank(e.ty.tags):
            if isSigned(to) and not bool(e.ty.tags and TYBOOL):
                return Expr(k: ECast, castop: SExt, castval: e, ty: to)
            else:
                return Expr(k: ECast, castop: ZExt, castval: e, ty: to)
        else:
            return Expr(k: ECast, castop: Trunc, castval: e, ty: to)
    type_error("cannot cast " & $e & " to " & $to)
    note("expression " & $e & " has type " & $e.ty)
    return nil

proc type_equal*(a, b: CType): bool =
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

proc castto*(e: Expr, to: CType): Expr =
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
        return castto(Expr(k: ECast, castop: BitCast, castval: e, ty: getIntType()), to)
    if to.spec == TYENUM:
        # cast xxx to int, then cast(bitcast) to enum
        return Expr(k: ECast, castop: BitCast, castval: castto(e, getIntType()), ty: to)
    if checkScalar(e.ty) == false or checkScalar(to) == false:
        type_error("cast uoperand shall have scalar type")
        return nil
    if bool(to.tags and TYBOOL):
        return binop(e, NE, Expr(k: EDefault, ty: e.ty), getBoolType())
    if bool(to.tags and (TYFLOAT or TYDOUBLE)) and e.ty.spec == TYPOINTER:
        type_error("A floating type shall not be converted to any pointer type")
        return nil
    if bool(e.ty.tags and (TYFLOAT or TYDOUBLE)) and to.spec == TYPOINTER:
        type_error("A floating type shall not be converted to any pointer type")
        return nil
    if e.ty.spec == TYPOINTER and to.spec == TYPOINTER:
        return Expr(k: ECast, castop: BitCast, castval: e, ty: to)
    if bool(e.ty.tags and TYDOUBLE) and bool(to.tags and TYFLOAT):
        return Expr(k: ECast, castop: FPTrunc, castval: e, ty: to)
    if bool(e.ty.tags and TYFLOAT) and bool(to.tags and TYDOUBLE):
        return Expr(k: ECast, castop: FPExt, castval: e, ty: to)
    if bool(e.ty.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64 or TYBOOL)):
        if to.spec == TYPOINTER:
            return Expr(k: ECast, castop: IntToPtr, castval: e, ty: to)
        if bool(to.tags and (TYFLOAT or TYDOUBLE)):
            if isSigned(e.ty):
                return Expr(k: ECast, castop: SIToFP, castval: e, ty: to)
            else:
                return Expr(k: ECast, castop: UIToFP, castval: e, ty: to)
    elif bool(to.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64)):
        if e.ty.spec == TYPOINTER:
            return Expr(k: ECast, castop: PtrToInt, castval: e, ty: to)
        if bool(e.ty.tags and (TYFLOAT or TYDOUBLE)):
            if isSigned(to):
                return Expr(k: ECast, castop: FPToSI, castval: e, ty: to)
            else:
                return Expr(k: ECast, castop: FPToUI, castval: e, ty: to)
    return intcast(e, to)

proc to*(e: var Expr, tag: uint32) =
    if e.ty.tags != tag:
        e = castto(e, CType(tags: tag, spec: TYPRIM))

proc integer_promotions*(e: Expr): Expr =
    if e.ty.spec == TYBITFIELD or getsizeof(e) < sizeofint:
        castto(e, getIntType())
    else:
        e

proc integer_promotions*(e: var Expr) =
    if e.ty.spec == TYBITFIELD or getsizeof(e) < sizeofint:
        to(e, TYINT)

proc conv*(a, b: var Expr) =
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


proc compatible*(p, expected: CType): bool =
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

proc default_argument_promotions*(e: Expr): Expr =
    if e.ty.spec == TYENUM or e.ty.spec == TYUNION or e.ty.spec == TYSTRUCT or e.ty.spec == TYPOINTER:
        return e
    if bool(e.ty.tags and TYFLOAT):
        castto(e, getDoubleType())
    else:
        integer_promotions(e)

proc checkInteger*(a, b: Expr) =
    let ok = checkInteger(a.ty) and checkInteger(b.ty)
    if ok == false:
        type_error("integer expected")

proc checkScalar*(a, b: Expr) =
    let ok = checkScalar(a.ty) and checkScalar(b.ty)
    if ok == false:
        type_error("scalar expected")

proc checkArithmetic*(a: CType): bool =
    if a.spec != TYPRIM:
        return false
    return true

proc checkArithmetic*(a, b: Expr) =
    let ok = checkArithmetic(a.ty) and checkArithmetic(b.ty)
    if ok == false:
        type_error("arithmetic type expected")

proc checkSpec*(a, b: var Expr) =
    if a.ty.spec != b.ty.spec:
        type_error("operands type mismatch: " & $a & ", " & $b)
    else:
        checkScalar(a, b)
        conv(a, b)

proc make_add*(result, r: var Expr) =
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

proc make_sub*(result, r: var Expr) =
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
                    Expr(k: ECast, castop: PtrToInt, castval: result, ty: getPtrDiff_t()),
                    PtrDiff,
                    Expr(k: ECast, castop: PtrToInt, castval: r, ty: getPtrDiff_t()),
                    getPtrDiff_t()
                )
                return
            if checkInteger(r.ty) == false:
                type_error("integer expected")
            result = binop(result, SAddP, unary(r, if isSigned(r.ty): SNeg else: UNeg, r.ty), result.ty)
        else:
            checkSpec(result, r)
            result = binop(result, if isSigned(r.ty): SSub else: USub, r, r.ty) 

proc make_shl*(result, r: var Expr) =
    checkInteger(result, r)
    integer_promotions(result)
    integer_promotions(r)
    result = binop(result, Shl, r, result.ty)

proc make_shr*(result, r: var Expr) =
    checkInteger(result, r)
    integer_promotions(result)
    integer_promotions(r)
    if isSigned(result.ty):
        result = binop(result, AShr, r, result.ty)
    else:
        result = binop(result, Shr, r, result.ty)

proc make_bitop*(result, r: var Expr, op: BinOP) =
    checkInteger(result, r)
    conv(result, r)
    result = binop(result, op, r, result.ty)

proc make_mul*(result, r: var Expr) =
    checkArithmetic(result, r)
    conv(result, r)
    result = binop(result, if isFloating(r.ty): FMul else: (if isSigned(r.ty): SMul else: UMul), r, r.ty)

proc make_div*(result, r: var Expr) =
    checkArithmetic(result, r)
    conv(result, r)
    result = binop(result, if isFloating(r.ty): FDiv else: (if isSigned(r.ty): SDiv else: UDiv), r, r.ty)

proc make_rem*(result, r: var Expr) =
    checkInteger(result, r)
    conv(result, r)
    result = binop(result, if isFloating(r.ty): FRem else: (if isSigned(r.ty): SRem else: URem)  ,r, r.ty)

proc reverse(a: var string) =
    var l = len(a) - 1
    for i in 0 ..< (len(a) div 2):
        let c = a[i]
        a[i] = a[l - i]
        a[l - i] = c

proc hex*(a: uint): string =
    var c = a
    while true:
        result &= hexs[a mod 16]
        c = c shr 4
        if c == 0:
            break
    reverse(result)


proc stringizing*(a: char): string =
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

proc stringizing*(a: string): string =
    result.add('"')
    for v in a:
        result.add(stringizing(v))
    result.add('"')

proc stringizing*(a: TokenV): string =
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

proc stringizing*(a: seq[TokenV]): string =
    for t in a:
        result.add(stringizing(t))

proc boolToInt*(e: Expr): Expr =
    Expr(k: ECast, castop: ZExt, castval: e, ty: getIntType())

proc conditional_expression*(): Expr

proc type_name*(): (CType, bool)

proc expression*(): Expr

proc primary_expression*(): Expr

proc postfix_expression*(): Expr

proc unary_expression*(): Expr

proc cast_expression*(): Expr

proc multiplicative_expression*(): Expr

proc shift_expression*(): Expr

proc relational_expression*(): Expr

proc equality_expression*(): Expr

proc AND_expression*(): Expr

proc exclusive_OR_expression*(): Expr

proc inclusive_OR_expression*(): Expr

proc logical_AND_expression*(): Expr

proc logical_OR_expression*(): Expr

proc conditional_expression*(start: Expr): Expr

proc assignment_expression*(): Expr

proc initializer_list*(): Expr

proc parameter_type_list*(): (bool, seq[(string, CType)])

proc declaration*(): Stmt

proc type_qualifier_list*(ty: var CType)

proc merge_types*(ts: seq[Token]): Ctype

proc declaration_specifiers*(): CType

proc specifier_qualifier_list*(): CType

proc constant_expression*(): Expr =
    conditional_expression()

type 
    DeclaratorFlags* = enum 
        ## * Direct: declarator with name
        ## * Abstract: declarator without name
        ## * Function: function parameter-type-list
        Direct, Abstract, Function

proc direct_declarator*(base: CType; flags=Direct): Stmt

proc declarator*(base: CType; flags=Direct): Stmt

template abstract_decorator*(base, f): untyped = 
    ## abstract decorator has no name
    ## for example: `static int        (*)(int, char)`
    ##               base-type      abstact-decorator
    declarator(base, flags=f)


proc struct_union*(t: Token): CType

proc penum*(): CType

proc static_assert*(): Stmt

proc translation_unit*(): Stmt

proc statament*(): Stmt

proc compound_statement*(): Stmt

proc compound_statement*(params: seq[(string, CType)]): Stmt

proc postfix*(e: Expr, op: PostfixOP, ty: CType): Expr = 
    ## construct a postfix operator
    Expr(k: EPostFix, pop: op, poperand: e, ty: ty)

proc consume*() =
    ## eat token from lexer and c preprocesser
    ##
    ## alias for `getToken`
    app.cpp()
    if p.tok.tok == TIdentifier:
        let k = isKeyword(p.tok.s)
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

proc merge_types*(ts: seq[Token]): CType =
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

proc type_qualifier_list(ty: var CType) =
    ## parse many type qualifiers, add to type
    while true:
        case p.tok.tok:
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
    while p.tok.tok in declaration_specifier_set:
        s.add(p.tok.tok)
        consume()

proc read_enum_sepcs*(c: var CType, sepc: Token) = 
    # TODO: ...
    discard

proc read_struct_union_sepcs*(c: var CType, sepc: Token) = 
    # TODO: ...
    discard

proc handle_typedef(s: var seq[Token], ty: CType): CType =
    consume()
    while p.tok.tok in declaration_specifier_set:
        s.add(p.tok.tok)
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

proc specifier_qualifier_list*(): CType =
    ## specfier_qualifier_list is used in struct declaration
    var s: seq[Token]
    while p.tok.tok in (type_specifier_set + type_qualifier_set + {Kstruct, Kenum, Kunion}):
        if p.tok.tok == Kenum:
            result = penum()
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_enum_sepcs(result, i)
            return result
        elif p.tok.tok == Kunion or p.tok.tok == Kstruct:
            result = struct_union(p.tok.tok)
            more(s)
            for i in s:
                if i == Ktypedef:
                    result.tags = result.tags or TYTYPEDEF
                    continue
                read_struct_union_sepcs(result, i)
            return result
        elif p.tok.tok == K_Atomic:
            consume()
            if p.tok.tok == TLbracket:
                consume()
                let ty = type_name()
                if ty[0] == nil:
                    expect("type-name")
                    return nil
                if p.tok.tok != TRbracket:
                    expectRB()
                    return nil
                consume()
                more(s)
                if len(s) > 0:
                    warning("atomic-type-specifier cannot combine with other types")
                return ty[0]
            s.add(K_Atomic)
        else:
            s.add(p.tok.tok)
            consume()
    if p.tok.tok == TIdentifier:
        let o = gettypedef(p.tok.s)
        if o != nil and (o.ty.tags and TYTYPEDEF) != 0:
            more(s)
            return handle_typedef(s, o.ty)
    if s.len > 0:
        return merge_types(s)
    return nil

proc declarator*(base: CType; flags=Direct): Stmt =
    ## take a base type, return the final type and name
    ##
    ## for example: `static   int     *foo`
    ##
    ##                base-type    decorator
    var ty = base
    while p.tok.tok == TMul:
        consume()
        ty = getPointerType(ty)
        type_qualifier_list(ty)
    return direct_declarator(ty, flags)

proc initializer_list*(): Expr =
    if p.tok.tok != TLcurlyBracket:
        if p.currentInitTy == nil:
            # when 'excess elements in initializer-list', 'p.currentInitTy' is nil
            return assignment_expression()
        if p.currentInitTy.spec notin {TYPRIM, TYPOINTER}:
            type_error("expect bracket initializer")
            return nil
        var e = assignment_expression()
        if e == nil:
            expectExpression()
            return nil
        return castto(e, p.currentInitTy)
    if p.currentInitTy.spec == TYSTRUCT:
        result = Expr(k: EStruct, ty: p.currentInitTy)
    else:
        # array, scalar
        result = Expr(k: EArray, ty: p.currentInitTy)
    consume()
    var m: int
    if p.currentInitTy.spec == TYARRAY:
        if p.currentInitTy.hassize:
            m = p.currentInitTy.arrsize.int
        else:
            result.ty.hassize = true
            result.ty.arrsize = 0
            m = -1
    elif p.currentInitTy.spec == TYSTRUCT:
        m = p.currentInitTy.selems.len.int
    else:
        # warning("braces around scalar initializer")
        m = 1
    var i = 0
    while true:
        if p.tok.tok == TRcurlyBracket:
            consume()
            break
        var ty: CType
        if p.currentInitTy.spec == TYSTRUCT:
            ty = if i < m: p.currentInitTy.selems[i][1] else: nil
        elif p.currentInitTy.spec == TYARRAY:
            ty = p.currentInitTy.arrtype
        else:
            ty = p.currentInitTy
        var o = p.currentInitTy
        p.currentInitTy = ty
        var e = initializer_list()
        p.currentInitTy = o
        if e == nil:
            return nil
        if m == -1:
            result.arr.add(e)
            inc result.ty.arrsize
        elif i < m:
            result.arr.add(e)
        else:
            warning("excess elements in initializer-list")
        if p.tok.tok == TComma:
            consume()
        inc i
    if p.currentInitTy.spec == TYPRIM or p.currentInitTy.spec == TYPOINTER:
        result = result.arr[0]

proc direct_declarator_end*(base: CType, name: string): Stmt

proc direct_declarator*(base: CType; flags=Direct): Stmt =
    case p.tok.tok:
    of TIdentifier:
        if flags == Abstract:
            return nil
        var name = p.tok.s
        consume()
        return direct_declarator_end(base, name)
    of TLbracket:
        consume()
        let st = declarator(base, flags)
        if st == nil:
            expect("declarator")
            return nil
        if p.tok.tok != TRbracket:
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

proc direct_declarator_end*(base: CType, name: string): Stmt =
    case p.tok.tok:
    of TLSquareBrackets: # int arr[5], int arr[*], int arr[static 5], int arr[static const 5], int arr[const static 5], int arr2[static const restrict 5]
        consume() # eat ]
        var ty = CType(tags: TYINVALID, spec: TYARRAY, arrsize: 0, arrtype: base, hassize: false)
        if p.tok.tok == TMul: # int arr[*]
          consume()
          if p.tok.tok != TRSquareBrackets:
            parse_error("expect ']'")
            note("the syntax is:\n\tint arr[*]")
            return nil
          # only allowed at extern variable and function prototype scope!
          return Stmt(k: SVarDecl1, var1name: name, var1type: ty)
        if p.tok.tok == Kstatic:
           consume()
           type_qualifier_list(ty)
        else:
            type_qualifier_list(ty)
            if p.tok.tok == Kstatic:
                consume()
        if p.tok.tok != TRSquareBrackets:
            let e = assignment_expression()
            if e == nil:
                expectExpression()
                return nil
            if not checkInteger(e.ty):
                type_error("size of array has non-integer type '" & $e.ty & '\'')
            ty.hassize = true
            var o = p.eval_error
            ty.arrsize = app.eval_const_expression(e)
            if p.eval_error:
                p.eval_error = o
                # VLA
                ty.vla = e
                ty.arrsize = 0
            if p.tok.tok != TRSquareBrackets:
               parse_error("expect ']'")
               return nil
        consume() # eat ]
        return direct_declarator_end(ty, name)
    of TLbracket:
        consume()
        var ty = CType(tags: TYINVALID, spec: TYFUNCTION, ret: base)
        if p.tok.tok != TRbracket:
            let res = parameter_type_list()
            if res[0]:
                ty.params = res[1]
            else:
                return nil
            if p.tok.tok != TRbracket:
                expectRB()
                return nil
        else:
            ty.params.add(("", nil))
        consume()
        if p.tok.tok == TLcurlyBracket: # function definition
            putsymtype3(name, ty)
            p.pfunc = name
            if not isTopLevel():
                parse_error("function definition is not allowed here")
                note("function can only declared in global scope")
            var body: Stmt
            block:
                var oldRet = p.currentfunctionRet
                p.currentfunctionRet = ty.ret
                body = compound_statement(ty.params)
                p.currentfunctionRet = oldRet
            if body == nil:
                expect("function body")
                return nil
            if name == "main":
                if not bool(ty.ret.tags and TYINT):
                    type_error("main should return 'int'")
                if ty.params.len >= 1:
                    if ty.params[0][1] != nil and (not bool(ty.params[0][1].tags and TYINT)):
                        type_error("first parameter of 'main' should be of type 'int'")
                    if ty.params.len >= 2:
                        if ty.params[1][1] != nil and (not (ty.params[1][1].spec == TYPOINTER and ty.params[1][1].p.spec == TYPOINTER)):
                            type_error("second parameter of 'main' should be has type 'char**'(pointer to char array)")
            if len(body.stmts) == 0 or (body.stmts[^1].k != SReturn and body.stmts[^1].k != SGoto):
                if not bool(ty.ret.tags and TYVOID):
                    if name != "main":
                        warning("no 'return' statement in a function should return a value")
                        note("in the function definition: '" & name & '\'')
                body.stmts &= Stmt(k: SReturn, exprbody: if bool(ty.ret.tags and TYVOID): nil else: 
                    (if name == "main": Expr(k: EDefault, ty: ty.ret) else: Expr(k: EUndef, ty: ty.ret))
                )
            return Stmt(funcname: name, k: SFunction, functy: ty, funcbody: body)
        return direct_declarator_end(ty, name)
    else:
        return Stmt(k: SVarDecl1, var1name: name, var1type: base)

proc struct_declarator(base: CType): (string, CType) =
    if p.tok.tok == TColon:
        consume()
        let e = constant_expression()
        if e == nil:
            expectExpression()
            note("in bit field declaration")
            return ("", nil)
        let bitsize = app.eval_const_expression(e)
        return ("", CType(tags: TYINVALID, spec: TYBITFIELD, bittype: base, bitsize: bitsize))
    else:
        let d = declarator(base, Direct)
        if d == nil:
            return ("", nil)
        if p.tok.tok == TColon:
            consume()
            let e = constant_expression()
            if e == nil:
                expectExpression()
                note("in bit field declaration")
                return
            let bitsize = app.eval_const_expression(e)
            return ("", CType(tags: TYINVALID, spec: TYBITFIELD, bittype: if d.k == SFunction: d.functy else: d.var1type, bitsize: bitsize))
        if d.k == SFunction:
            return (d.funcname, d.functy)
        return (d.var1name, d.var1type)

proc struct_union*(t: Token): CType =
    ## parse a struct or union, return it
    ## for example:  `struct Foo`
    ##
    ##               `struct { ... }`
    ##
    ##               `struct Foo { ... }`
    consume() # eat struct/union
    var name = ""
    if p.tok.tok == TIdentifier:
        name = p.tok.s
        consume()
        if p.tok.tok != TLcurlyBracket: # struct Foo
           return if t==Kstruct: getstructdef(name) else: getuniondef(name)
    elif p.tok.tok != TLcurlyBracket:
        parse_error("expect '{' for start anonymous struct/union")
        return nil
    if t == Kstruct:
        result = CType(tags: TYINVALID, spec: TYSTRUCT, sname: name)
    else:
        result = CType(tags: TYINVALID, spec: TYUNION, sname: name)
    consume()
    var names: HashSet[string] = initHashSet[string]()
    if p.tok.tok != TRcurlyBracket:
        while true:
            if p.tok.tok == K_Static_assert:
                let s = static_assert()
                if s == nil:
                    return nil
                if p.tok.tok == TRcurlyBracket:
                    break
                continue
            var base = specifier_qualifier_list()
            if base == nil:
                expect("specifier-qualifier-list")
                return nil

            if p.tok.tok == TSemicolon:
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
                    if p.tok.tok == TComma:
                        consume()
                    else:
                        if p.tok.tok != TSemicolon:
                            expect("';'")
                            return nil
                        consume()
                        break
                if p.tok.tok == TRcurlyBracket:
                    consume()
                    break
    else:
        consume()
    if len(result.sname) > 0:
        if t == Kstruct:
            putstructdef(result)
        else:
            putenumdef(result)
    return result

proc penum*(): CType =
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
    if p.tok.tok == TIdentifier:
        name = p.tok.s
        consume()
        if p.tok.tok != TLcurlyBracket: # struct Foo
          return getenumdef(name)
    elif p.tok.tok != TLcurlyBracket:
        parse_error("expect '{' for start anonymous enum")
        return nil
    result = CType(tags: TYINVALID, spec: TYENUM, ename: name)
    consume()
    var c: intmax_t = 0
    while true:
        if p.tok.tok != TIdentifier:
            break # enum {A, } is ok !
        var s = p.tok.s # copy
        consume()
        if p.tok.tok == TAssign:
            consume()
            var e = constant_expression()
            c = app.eval_const_expression(e)
            if err():
                return nil
        result.eelems.add((s, c))
        putsymtype(s, getIntType())
        inc c
        if p.tok.tok == TComma:
            consume()
        else:
            break
    if p.tok.tok != TRcurlyBracket:
        parse_error("expect '}'")
    consume()
    if len(result.ename) > 0:
        putenumdef(result)
    return result

proc parameter_type_list*(): (bool, seq[(string, CType)]) =
    result = (true, default(typeof(result[1])))
    while true:
        if not istype(p.tok.tok):
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
        if p.tok.tok == TRbracket:
            break
        if p.tok.tok == TComma:
            consume()
        if p.tok.tok == TEllipsis:
            result[1].add(("", nil))
            consume()
            break
        elif p.tok.tok == TRbracket:
            break

proc checkAlign(a: uint32) =
    if (a and (a - 1)) != 0:
        type_error("requested alignment is not a power of 2")

proc parse_alignas*(): bool =
    consume()
    if p.tok.tok != TLbracket:
        expectLB()
        return false
    consume()
    if istype(p.tok.tok):
        let (ty, isf) = type_name()
        discard isf
        var a = uint32(getAlignof(ty))
        if a == 0:
            type_error("zero alignment is not valid")
        else:
            checkAlign(a)
            p.currentAlign = a
    else:
        let e = expression()
        if e == nil:
            expectExpression()
            return false
        var a = app.eval_const_expression(e)
        if a <= 0:
            type_error("alignment " & $a &  " too small")
        else:
            checkAlign(uint32(a))
            p.currentAlign = uint32(a)
    if p.tok.tok != TRbracket:
        expectRB()
        return false
    consume()
    return true

proc declaration_specifiers*(): CType =
    ## declaration_specfier is used in function and variable declaration/definition
    var s: seq[Token]
    var should_return = false
    while p.tok.tok in (declaration_specifier_set + {Kstruct, Kenum, Kunion, K_Alignas}):
        if p.tok.tok == Kenum:
            result = penum()
            should_return = true
        elif p.tok.tok == Kunion or p.tok.tok == Kstruct:
            result = struct_union(p.tok.tok)
            should_return = true
        elif p.tok.tok == K_Atomic:
            consume()
            if p.tok.tok == TLbracket:
                consume()
                let ty = type_name()
                if ty[0] == nil:
                    expect("type-name")
                    return nil
                if p.tok.tok != TRbracket:
                    expectRB()
                    return nil
                consume()
                more(s)
                if len(s) > 0:
                    warning("atomic-type-specifier cannot combine with other types")
                return ty[0]
            s.add(K_Atomic)
        elif p.tok.tok == K_Alignas:
            if not parse_alignas():
                return nil
        else:
            s.add(p.tok.tok)
            consume()
    if should_return:
        for i in s:
            if i == Ktypedef:
                result.tags = result.tags or TYTYPEDEF
                continue
        return result
    if p.tok.tok == TIdentifier:
        let o = gettypedef(p.tok.s)
        if o != nil and (o.ty.tags and TYTYPEDEF) != 0:
            more(s)
            return handle_typedef(s, o.ty)
    if s.len > 0:
        return merge_types(s)
    warning("type defaults to 'int' in declaration")
    return getIntType()

proc parse_asm*(): Stmt =
   consume() # eat asm
   if p.tok.tok != TLbracket:
       expectLB()
       return nil
   consume() # eat '('
   if p.tok.tok != TStringLit:
       expect("string literal")
       return nil
   result = Stmt(k: SAsm, asms: p.tok.str)
   consume() # eat string
   if p.tok.tok != TRbracket:
       expectRB()
       return nil
   consume() # eat ')'

proc static_assert*(): Stmt =
    consume()
    if p.tok.tok != TLbracket:
        expectLB()
        return nil
    consume()
    var msg = ""
    let e = constant_expression()
    let ok = app.eval_const_expression(e) != 0
    if p.tok.tok == TRbracket: # no message
        if ok == false:
            error("static assert failed!")
        consume()
        note("static assert with no message")
    elif p.tok.tok == TComma:
        consume()
        if p.tok.tok != TStringLit:
            expect("string literal in static assert")
            return nil
        if ok == false:
            error(p.tok.str)
            msg = p.tok.str
        consume()
        if p.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
    else:
        expect("',' or ')'")
        return nil
    if p.tok.tok != TSemicolon:
        expect("';'")
        return nil
    consume()
    return Stmt(k: SSemicolon)

proc checkRetType(ty: CType) =
    if ty.spec == TYFUNCTION:
        if (ty.ret.tags and (TYREGISTER)) != 0:
            warning("'register' in function has no effect")
        if (ty.ret.tags and (TYTHREAD_LOCAL)) != 0:
            warning("'_Thread_local' in function has no effect")    

proc checkInComplete(base: CType) =
    case base.tag:
    of TYSTRUCT:
        type_error("variable has incomplete type `struct " & base.name & '`')
        note("in forward references of `struct " & base.name & '`')
        note("add struct definition before use")
    of TYENUM:
        type_error("variable has incomplete type `enum " & base.name & '`')
        note("in forward references of `enum " & base.name & '`')
        note("add struct definition before use")
    of TYUNION:
        type_error("variable has incomplete type `union " & base.name & '`')
        note("in forward references of `union " & base.name & '`')
        note("add union definition before use")
    else:
        unreachable()

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

proc declaration*(): Stmt =
    ## parse variable declaration or function definition
    ## 
    ## this is different from ISO C grammar
    if p.tok.tok == K_Static_assert:
        return static_assert()
    block:
        p.currentAlign = 0
        var base = declaration_specifiers()
        if base == nil:
            expect("declaration-specifiers")
            return nil
        if p.currentAlign > 0:
            var m = getAlignof(base)
            if p.currentAlign < m:
                type_error("requested alignment is less than minimum alignment of " & $m & " for type '" & $base & "'")
            else:
                base.align = p.currentAlign
        if p.tok.tok == TSemicolon:
            if base.spec notin {TYSTRUCT, TYUNION, TYENUM}:
                warning("declaration does not declare anything")
                note("add a variable name in the declaration")
            consume()
            if base.align != 0:
                type_error("'_Alignas' can only used in variables")
            return Stmt(k: SDeclOnly, decl: base)
        result = Stmt(k: SVarDecl)
        while true:
            let st = declarator(base)
            if st == nil:
                expect("declarator")
                return nil
            if st.k == SFunction:
                # function has not lvalue
                checkRetType(st.functy)
                return st
            assert st.k == SVarDecl1
            if st.var1type.spec == TYINCOMPLETE:
                checkInComplete(st.var1type)
                return nil
            if (st.var1type.tags and TYINLINE) != 0:
                warning("inline declaration is in block scope has no effect")
            checkRetType(st.var1type)
            # variable has lvalue
            st.var1type.tags = st.var1type.tags or TYLVALUE
            result.vars.add((st.var1name, st.var1type, nil))
            putsymtype(st.var1name, st.var1type)
            if p.tok.tok == TAssign:
                if st.var1type.spec == TYARRAY and st.var1type.vla != nil:
                    type_error("variable-sized object may not be initialized")
                    return nil
                consume()
                var old = p.currentInitTy
                p.currentInitTy = st.var1type
                let init = initializer_list()
                p.currentInitTy = old
                if init == nil:
                    expect("initializer-list")
                    return nil
                if st.var1type.spec == TYFUNCTION:
                    type_error("function declaration has no initializer")
                    note("only variables can be initialized")
                else:
                    result.vars[^1][2] = init
                    if isTopLevel() and isConstant(result.vars[^1][2]) == false:
                        type_error("initializer element is not constant")
                        note("global variable requires constant initializer")
            else:
                if st.var1type.spec == TYARRAY:
                    if st.var1type.vla != nil and isTopLevel():
                        type_error("variable length array declaration not allowed at file scope")
                        note("in the declaration of variable: '" & st.var1name & '\'')
                    elif st.var1type.hassize == false and st.var1type.vla == nil:
                        if isTopLevel():
                            warning("array '" & st.var1name & "' assumed to have one element")
                            st.var1type.hassize = true
                            st.var1type.arrsize = 1
                        else:
                            type_error("array size missing in ‘" & st.var1name & "’")
            if p.tok.tok == TComma:
                consume()
            elif p.tok.tok == TSemicolon:
                consume()
                break
        return result

proc cast_expression*(): Expr =
    case p.tok.tok:
    of TLbracket:
        consume()
        if istype(p.tok.tok):
            var (n, isf) = type_name()
            discard isf
            if n == nil:
                return nil
            n.tags = n.tags or TYLVALUE
            if p.tok.tok != TRbracket:
                expect("`)`")
                return nil
            consume()
            if p.tok.tok == TLcurlyBracket:
                block:
                    var old = p.currentInitTy
                    p.currentInitTy = n
                    result = initializer_list()
                    p.currentInitTy = old
                return result
            let e = cast_expression()
            if e == nil:
                return nil
            if (n.tags and TYVOID) != 0:
                return Expr(k: EVoid, voidexpr: e, ty: getVoidType())
            return castto(e, n)
        putToken()
        p.tok = TokenV(tok: TLbracket, tags: TVNormal)
    else:
        discard
    return unary_expression()

proc type_name*(): (CType, bool) =
    ## parse a type-name
    ##
    ## for example:
    ##
    ##    sizeof(type-name)
    let base = declaration_specifiers()
    if base == nil:
        (nil, false)
    elif p.tok.tok == TRbracket:
        (base, false)
    else:
        let st = abstract_decorator(base, Abstract)
        if st == nil:
            (nil, false)
        elif st.k == SFunction:
            (st.functy, true)
        else:
            (st.var1type, false)

proc getsizeof2(e: Expr): Expr =
    if e.k == ArrToAddress:
        if e.voidexpr.ty.vla != nil:
            Expr(ty: getSizetType(), k: EVLAGetSize, vla: e)
        else:
            Expr(ty: getSizetType(), k: EIntLit, ival: cast[int](getsizeof(e.voidexpr.ty)))
    else:
        Expr(ty: getSizetType(), k: EIntLit, ival: cast[int](getsizeof(e)))

proc unary_expression*(): Expr =
    let tok = p.tok.tok
    case tok:
    of TNot:
        consume()
        let e = cast_expression()
        if e == nil:
            return nil
        if not checkScalar(e.ty):
            type_error("scalar expected")
            return nil
        return boolToInt(unary(e, LogicalNot, getIntType()))
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
        if not bool(e.ty.tags and TYLVALUE):
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
        consume()
        if p.tok.tok == TLbracket:
            consume()
            if istype(p.tok.tok):
                let ty = type_name()
                if ty[0] == nil:
                    expect("type-name or expression")
                    return nil
                if ty[1] == true:
                    type_error("invalid application of 'sizeof' to a function type")
                if p.tok.tok != TRbracket:
                    expectRB()
                    return nil
                consume()
                return Expr(ty: getSizetType(), k: EIntLit, ival: cast[int](getsizeof(ty[0])))
            let e = unary_expression()
            if e == nil:
                expectExpression()
                return nil
            if p.tok.tok != TRbracket:
                expectRB()
                return nil
            consume()
            return getsizeof2(e)
        else:
            let e = unary_expression()
            if e == nil:
                expectExpression()
                return nil
            return getsizeof2(e)
    of K_Alignof:
        consume()
        if p.tok.tok != TLbracket:
            expect("(")
            return nil
        consume()
        if istype(p.tok.tok):
            let ty = type_name()
            if ty[0] == nil:
                expect("type-name")
                return nil
            if ty[1] == true:
                type_error("invalid application of '_Alignof' to a function type")
            result = Expr(ty: getSizetType(), k: EIntLit, ival: cast[int](getAlignof(ty[0])))
        else:
            let e = constant_expression()
            if e == nil:
                expectExpression()
                return nil
            result = Expr(ty: getSizetType(), k: EIntLit, ival: cast[int](getAlignof(e)))
        if p.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
        return result
    else:
        return postfix_expression()

proc decimaltoInt*(s: string): int =
    for c in s:
        result = (result * 10) + (int(c) - '0'.int)

proc decimal16toInt*(s: string): int =
    for c in s:
        if c in {'0'..'9'}:
            result = (result shl 4) or (int(c) - '0'.int)
        elif c in {'a'..'f'}:
            result = (result shl 4) or (int(c) - 'a'.int + 10)
        else:
            result = (result shl 4) or (int(c) - 'A'.int + 10)

proc decimal2toInt*(s: string): int =
    for c in s:
        if c == '1':
            result = (result shl 1) or 1
        else:
            result = result shl 1

proc pow10*(a: int): int =
    result = 1
    for i in 0..<a:
        result *= 10

proc read_pp_float(s: string, o: var float, c: int, base: int, init = 0.0): int =
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
                f += float(int(c) - '0'.int) * e
            elif c in {'a'..'f'}:
                f += float(int(c) - 'a'.int + 10) * e
            elif c in {'A'..'F'}:
                f += float(int(c) - 'A'.int + 10) * e
            else:
                break
        else:
            if c notin {'0'..'9'}:
                break
            e /= 10.0
            f += float(int(c) - '0'.int) * e
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
                f *= fr
        else:
            f *= pow(2, if negate: -float(powerby) else: float(powerby))
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
#   * 0: <failed>
#   * 1: int
#   * 2: double
#   * 3: float
#   * 4: long
#   * 5: long long
#   * 6: unsigned long
#   * 7: unsigned long long
#   * 8: unsigned int

proc read_pp_number*(s: string, f: var float, n: var int): int =
    var base = 10
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
            var num = int(s[1]) - int('0')
            while true:
                if i == len(s):
                    n = num
                    return 1
                if s[i] notin {'0'..'7'}:
                    parse_error("bad octal literal")
                    return 0
                num = (num shl 3) or (int(s[i]) - int('0'))
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

type FixName = object
    priority: int
    name: string

import std/[heapqueue]

proc `<`(a, b: FixName): bool = a.priority < b.priority

proc fixName(v: string) =
    var fixList = initHeapQueue[FixName]()
    for i in countdown(len(p.typedefs)-1, 0):
        for name in p.typedefs[i].keys():
            var d = editDistance(name, v)
            fixList.push(FixName(priority: d, name: name))
    var msg = "\n"
    while len(fixList) > 0:
        var f = fixList.pop()
        if f.priority < 3:
            msg.add("Perhaps you meant: " & f.name & "\n")
    if msg.len > 1:
        note(msg)

proc primary_expression*(): Expr =
    ## primary expressions:
    ##     * constant
    ##     * `(` expression `)`
    ##     * identfier
    case p.tok.tok:
    of TCharLit:
        var tags = 0'u32
        case p.tok.itag:
        of Iint:
            tags = TYCHAR
        of Ilong:
            tags = TYUINT16
        of Iulong:
            tags = TYUINT32
        of Ilonglong:
            when defined(windows):
                tags = TYUINT32
            else:
                tags = TYUINT32
        else:
            unreachable()
        result = Expr(k: EIntLit, ival: p.tok.i, ty: CType(tags: tags, spec: TYPRIM))
        consume()
    of TNumberLit:
        var tags = TYINT
        case p.tok.itag:
        of Iint:
            tags = TYINT
        of Ilong:
            tags = TYLONG
        of Iulong:
            tags = TYULONG
        of Ilonglong:
            tags = TYLONGLONG
        of Iulonglong:
            tags = TYULONGLONG
        of Iuint:
            tags = TYUINT
        result = Expr(k: EIntLit, ival: p.tok.i, ty: CType(tags: tags, spec: TYPRIM))
        consume()
    of TFloatLit:
        result = Expr(k: EFloatLit, fval: p.tok.f, ty: CType(tags: if p.tok.ftag == Ffloat: TYFLOAT else: TYDOUBLE, spec: TYPRIM))
        consume()
    of TStringLit:
        var s: string
        var enc = p.tok.enc
        while true:
            s.add(p.tok.str)
            consume()
            if p.tok.tok != TStringLit:
                break
            if p.tok.enc != enc:
                type_error("unsupported non-standard concatenation of string literals")
                note("concatenation UTF-" & $enc & " and UTF-" & $p.tok.enc)
                return
        case enc:
        of 8:
            let ty = getInt8Type()
            return Expr(
                k: ArrToAddress, voidexpr: Expr(k: EString, ty: ty, str: s), 
                ty: getPointerType(ty)
            )
        of 16:
            var a: seq[Expr]
            let ty = getUShortType()
            for i in writeUTF8toUTF16(s):
                a.add(Expr(k: EIntLit, ival: cast[int](i), ty: ty))
            return Expr(
                k: ArrToAddress, voidexpr: Expr(k: EArray, ty: ty, arr: a), 
                ty: getPointerType(ty)
            )
        of 32:
            var a: seq[Expr]
            let ty = getUInt32Type()
            for i in writeUTF8toUTF32(s):
                a.add(Expr(k: EIntLit, ival: cast[int](i), ty: ty))
            return Expr(
                k: ArrToAddress, voidexpr: Expr(k: EArray, ty: ty, arr: a), 
                ty: getPointerType(ty)
            )
        else:
            unreachable()
    of TPPNumber:
        var f: float
        var n: int
        let ok = read_pp_number(p.tok.s, f, n)
        case ok:
        of 0:
            result = nil
        of 1:
            result = Expr(ty: getIntType(), k: EIntLit, ival: n)
            consume()
        of 2, 3:
            result = Expr(ty: if ok == 2: getDoubleType() else: getFloatType(), k: EFloatLit, fval: f)
            consume()
        of 4:
            result = Expr(ty: getLongType(), k: EIntLit, ival: n)
            consume()
        of 5:
            result = Expr(ty: getULongType(), k: EIntLit, ival: n)
            consume() 
        of 6:
            result = Expr(ty: getLongLongType(), k: EIntLit, ival: n)
            consume()
        of 7:
            result = Expr(ty: getULongLongType(), k: EIntLit, ival: n)
            consume()
        of 8:
            result = Expr(ty: getUIntType(), k: EIntLit, ival: n)
            consume()
        else:
            unreachable()
    of CPPident:
        result = Expr(k: EIntLit, ival: 0, ty: getIntType())  
        consume()
    of TIdentifier:
        if p.want_expr:
            result = Expr(k: EIntLit, ival: 0, ty: getIntType())
        elif p.tok.s == "__func__":
            result = Expr(k: EString, str: p.pfunc, ty: CType(tags: TYCONST, spec: TYPOINTER, p: getCharType()))
        else:
            let ty = getsymtype(p.tok.s)
            if ty == nil:
                type_error("Variable not in scope: " & p.tok.s)
                if p.tok.s.len > 2:
                    fixName(p.tok.s)
                return nil
            case ty.spec:
            of TYFUNCTION:
                result = unary(
                    Expr(k: EVar, sval: p.tok.s, ty: ty), AddressOf, 
                    CType(tags: TYLVALUE, spec: TYPOINTER, p: ty)
                )
            of TYARRAY:
                var cp = deepCopy(ty)
                cp.arrtype.tags = cp.arrtype.tags and prim
                result = Expr(
                    k: ArrToAddress, voidexpr: Expr(k: EVar, sval: p.tok.s, ty: cp),
                    ty: CType(tags: TYLVALUE, spec: TYPOINTER, p: ty.arrtype)
                )
            else:
                result = Expr(k: EVar, sval: p.tok.s, ty: ty)
            consume()
    of TLbracket:
        consume()
        result = expression()
        if result == nil:
            expectExpression()
            return nil
        if p.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
    of K_Generic:
        consume()
        if p.tok.tok != TLbracket:
            expectLB()
            return nil
        consume()
        let test = assignment_expression()
        if test == nil:
            expectExpression()
            note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
            return nil
        if p.tok.tok != TComma:
            expect("','")
            return nil
        consume()
        let testty = test.ty
        var defaults: Expr
        while true:
            var tname: CType = nil
            if p.tok.tok == Kdefault:
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
            if p.tok.tok != TColon:
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
            case p.tok.tok:
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

proc postfix_expression*(): Expr =
    let e = primary_expression()
    case p.tok.tok:
    of TSubSub, TAddAdd:
        if not assignable(e):
            return nil
        let op = if p.tok.tok == TAddAdd: PostfixIncrement else: PostfixDecrement
        consume()
        return postfix(e, op, e.ty)
    of TArrow, TDot: # member access
        let isarrow = p.tok.tok == TArrow
        consume()
        if p.tok.tok != TIdentifier:
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
            if p.tok.s == e.ty.selems[i][0]:
                consume()
                var ty = e.ty.selems[i][1]
                ty.tags = ty.tags or TYLVALUE
                if isarrow:
                    return Expr(k: EPointerMemberAccess, obj: e, idx: i, ty: ty)
                return Expr(k: EMemberAccess, obj: e, idx: i, ty: ty)
        type_error("struct/union " & $e.ty.sname & " has no member " & p.tok.s)
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
        if p.tok.tok == TRbracket:
            consume()
        else:
            while true:
                let a = assignment_expression()
                if a == nil:
                    expectExpression()
                    note("the syntax is:\n\tfunction-name(argument)")
                    return nil
                args.add(a)
                if p.tok.tok == TComma:
                    consume()
                elif p.tok.tok == TRbracket:
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
            inTheExpression(f)
            return nil
        var i = 0
        while true:
            if i == len(params):
                break
            if params[i][1] == nil:
                for j in i ..< len(args):
                    args[j] = default_argument_promotions(args[j])
                break
            #if not compatible(args[i].ty, params[i][1]):
            #    warning("incompatible type in calling function '" & $f & '\'')
            #    note("expect '" & $params[i][1] & "' but argument " & $i & " is of type '" & $args[i].ty & '\'')
            var a = castto(args[i], params[i][1])
            args[i] = a
            inc i
        return Expr(k: ECall, callfunc: f, callargs: args, ty: ty.ret)
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
        if p.tok.tok != TRSquareBrackets:
            expect("']'")
            return
        consume()
        var ty = e.ty.p
        ty.tags = ty.tags or TYLVALUE
        return Expr(k: ESubscript, left: e, right: rhs, ty: ty)
    else:
        return e

proc multiplicative_expression*(): Expr =
    result = cast_expression()
    while true:
        case p.tok.tok:
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

proc additive_expression*(): Expr =
    result = multiplicative_expression()
    while true:
        case p.tok.tok:
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

proc shift_expression*(): Expr =
    result = additive_expression()
    while true:
        case p.tok.tok:
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

proc relational_expression*(): Expr =
    result = shift_expression()
    while true:
        case p.tok.tok:
        of TLt:
            consume()
            var r = shift_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result =  boolToInt(binop(result, if isFloating(r.ty): FLT else: (if isSigned(r.ty): SLT else: ULT), r, getBoolType()))
        of TLe:
            consume()
            var r = shift_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result = boolToInt(binop(result, if isFloating(r.ty): FLE else: (if isSigned(r.ty): SLE else: ULE), r, getBoolType()))
        of TGt:
            consume()
            var r = shift_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result = boolToInt(binop(result, if isFloating(r.ty): FGT else: (if isSigned(r.ty): SGT else: UGT), r, getBoolType()))
        of TGe:
            consume()
            var r = shift_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result = boolToInt(binop(result, if isFloating(r.ty): FGE else: (if isSigned(r.ty): SGE else: UGE), r, getBoolType()))
        else:
            return result

proc equality_expression*(): Expr =
    result = relational_expression()
    while true:
        case p.tok.tok:
        of TEq:
            consume()
            var r = relational_expression()
            if r == nil:
                return nil
            if result.ty.spec == TYPOINTER and r.ty.spec == TYPOINTER:
                result = boolToInt(binop(result, EQ, r, getBoolType()))
            else:
                checkSpec(result, r)
                result = boolToInt(binop(result, if isFloating(r.ty): FEQ else: EQ, r, getBoolType()))
        of TNe:
            consume()
            var r = relational_expression()
            if r == nil:
                return nil
            if result.ty.spec == TYPOINTER and r.ty.spec == TYPOINTER:
                result = boolToInt(binop(result, NE, r, getBoolType()))
            else:
                checkSpec(result, r)
                result = boolToInt(binop(result, if isFloating(r.ty): FNE else: NE, r, getBoolType()))
        else:
            return result

proc AND_expression*(): Expr =
    result = equality_expression()
    while true:
        case p.tok.tok:
        of TBitAnd:
            consume()
            var r = equality_expression()
            if r == nil:
                return nil
            make_bitop(result, r, And)
        else:
            return result

proc exclusive_OR_expression*(): Expr =
    result = AND_expression()
    while true:
        case p.tok.tok:
        of TXOr:
            consume()
            var r = AND_expression()
            if r == nil:
                return nil
            make_bitop(result, r, Xor)
        else:
            return result

proc inclusive_OR_expression*(): Expr =
    result = exclusive_OR_expression()
    while true:
        case p.tok.tok:
        of TBitOr:
            consume()
            var r = exclusive_OR_expression()
            if r == nil:
                return nil
            make_bitop(result, r, Or)
        else:
            return result

proc logical_AND_expression*(): Expr =
    result = inclusive_OR_expression()
    while true:
        case p.tok.tok:
        of TLogicalAnd:
            consume()
            var r = inclusive_OR_expression()
            if r == nil:
                return nil
            checkScalar(result, r)
            result = boolToInt(binop(result, LogicalAnd, r, getIntType()))
        else:
            return result

proc logical_OR_expression*(): Expr =
    result = logical_AND_expression()
    while true:
        case p.tok.tok:
        of TLogicalOr:
            consume()
            var r = logical_AND_expression()
            if r == nil:
                return nil
            checkScalar(result, r)
            result = boolToInt(binop(result, LogicalOr, r, getIntType()))
        else:
            return result

proc expression*(): Expr =
    ## parse a expression
    result = assignment_expression()
    while true:
        if p.tok.tok == TComma:
            consume()
            var r = assignment_expression()
            if r == nil:
                return nil
            result = binop(result, Comma, r, r.ty)
        else:
            return result

proc conditional_expression*(start: Expr): Expr =
    var lhs = logical_OR_expression()
    if lhs == nil:
        expectExpression()
        return nil
    if p.tok.tok != TColon:
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
    return Expr(k: ECondition, cond: start, cleft: lhs, cright: rhs, ty: lhs.ty)

proc conditional_expression*(): Expr =
    let e = logical_OR_expression()
    if e == nil:
        return nil
    if p.tok.tok == TQuestionMark:
      return conditional_expression(e)
    return e

proc assignment_expression*(): Expr =
    result = logical_OR_expression()
    if result == nil:
        return nil
    let tok = p.tok.tok
    if tok == TQuestionMark:
        consume()
        return conditional_expression(result)
    if p.tok.tok in {TAssign, TAsignAdd, TAsignSub, 
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

proc translation_unit*(): Stmt =
    ## parse top-level declaration until EOF reaches, the entry point of program
    ##
    ## never return nil, return a compound statement
    result = Stmt(k: SCompound)
    var s: Stmt
    while p.tok.tok != TEOF:
        if p.tok.tok == KAsm:
           s = parse_asm()
           if p.tok.tok != TSemicolon:
             expect("';'")
             s = nil
           consume()
        else:
           s = declaration()
        if s == nil:
          break
        result.stmts.add(s)

proc runParser*(): Stmt =
    ## eat first token and parse a translation_unit
    ##
    ## never return nil, return a compound statement
    consume()
    result = translation_unit()
    finishParsing()

proc compound_statement*(): Stmt =
    ## parse mant statements
    result = Stmt(k: SCompound)
    consume()
    enterBlock()
    while p.tok.tok != TRcurlyBracket:
        var s: Stmt = nil
        if istype(p.tok.tok):
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

proc compound_statement*(params: seq[(string, CType)]): Stmt =
    result = Stmt(k: SCompound)
    consume()
    enterBlock()
    for (name, vty) in params:
        if vty == nil:
            break
        var t = deepCopy(vty)
        t.tags = t.tags or TYLVALUE
        putsymtype(name, t)
    while p.tok.tok != TRcurlyBracket:
        var s: Stmt = nil
        if istype(p.tok.tok):
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

proc statament*(): Stmt =
    ## parse a statement
    if p.tok.tok == KAsm:
        result = parse_asm()
        if p.tok.tok != TSemicolon:
           expect("';'")
           return nil
        consume()
        return result
    if p.tok.tok == TSemicolon:
        consume()
        return Stmt(k: SSemicolon)
    elif p.tok.tok == TLcurlyBracket:
        return compound_statement()
    elif p.tok.tok == Kcase:
        consume()
        var e = constant_expression()
        if e == nil:
            expect("constant-expression")
            return nil
        if p.tok.tok != TColon:
            parse_error("':' expected")
            return nil
        consume()
        let s = statament()
        if s == nil:
            expectStatement()
            return nil
        return Stmt(k: Scase, case_expr: castto(e, p.currentCase), case_stmt: s)
    elif p.tok.tok == Kdefault:
        consume()
        if p.tok.tok != TColon:
            parse_error("':' expected")
            return nil
        consume()
        let s = statament()
        if s == nil:
            return nil
        return Stmt(k: SDefault, default_stmt: s)
    elif p.tok.tok == Kgoto:
        consume()
        if p.tok.tok != TIdentifier:
            expect("identifier")
            note("the syntax is:\n\tgoto label;")
            return nil
        var location = p.tok.s
        if not hasLabel(location):
            type_error("undeclared label " & location)
        consume()
        if p.tok.tok != TSemicolon:
            parse_error("expect ';'")
            return nil
        consume()
        return Stmt(k: SGoto, location: location)
    elif p.tok.tok == Kcontinue:
        consume()
        if p.tok.tok != TSemicolon:
            parse_error("expect ';'")
            return nil
        consume()
        return Stmt(k: SContinue)
    elif p.tok.tok == Kbreak:
        consume()
        if p.tok.tok != TSemicolon:
            expect("';'")
            return nil
        consume()
        return Stmt(k: SBreak)
    elif p.tok.tok == Kreturn:
        consume()
        if p.tok.tok == TSemicolon:
            consume()
            if bool(p.currentfunctionRet.tags and TYVOID):
                return Stmt(k: SReturn, exprbody: nil)
            warning("use default value in 'return' statement")
            note("function should return a value, but no value provided in 'return'")
            note("A return statement without an expression shall only appear in a function whose return type is void")
            return Stmt(k: SReturn, exprbody: Expr(k: EDefault, ty: p.currentfunctionRet))
        let e = expression()
        if e == nil:
            expectExpression()
            return nil
        if p.tok.tok != TSemicolon:
            expect("';'")
            return nil
        consume()
        if bool(p.currentfunctionRet.tags and TYVOID):
            warning("the value of 'return' statement is ignored")
            warning("'return' a value in function return void")
            note("A return statement with an expression shall not appear in a function whose return type is void")
            return Stmt(k: SReturn, exprbody: nil)
        if not compatible(e.ty, p.currentfunctionRet) and e.ty.spec != TYPRIM:
            # if it is cast from int to long, int to double, ...etc, we do not emit a warning
            warning("incompatible type in 'return' statement")
            note("expect " & $p.currentfunctionRet & ", but got " & $e.ty)
            inTheExpression(e)
        return Stmt(k: SReturn, exprbody: castto(e, p.currentfunctionRet))
    elif p.tok.tok == Kif:
        consume()
        if p.tok.tok != TLbracket:
            expectLB()
            return nil
        consume()
        let e = expression()
        if e == nil:
            expectExpression()
            return nil
        if not checkScalar(e.ty):
            type_error("expect scalar")
        if p.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
        let s = statament()
        if s == nil:
            expectStatement()
            return nil
        var elsebody: Stmt = nil
        if p.tok.tok == Kelse:
            consume()
            elsebody = statament()
            if elsebody == nil:
                expectStatement()
                return nil
        return Stmt(k: SIf, iftest: e, ifbody: s, elsebody: elsebody)
    elif p.tok.tok == Kwhile or p.tok.tok == Kswitch:
        let tok = p.tok.tok
        consume()
        if p.tok.tok != TLbracket:
            expectLB()
            return nil
        consume()
        var e = expression()
        if e == nil:
            expectExpression()
            return nil
        if not checkScalar(e.ty):
            type_error("expect scalar")
        if p.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
        if tok == Kswitch:
            integer_promotions(e)
            var oldcase = p.currentCase
            p.currentCase = e.ty
            let s = statament()
            if s == nil:
                expectStatement()
                return nil
            p.currentCase = oldcase
            return Stmt(k: SSwitch, test: e, body: s)
        let s = statament()
        if s == nil:
            expectStatement()
            return nil
        return Stmt(k: SWhile, test: e, body: s)
    elif p.tok.tok == Kfor:
        # this are valid in C
        # for(int a;;)                                                                                                                                          
        # {                                                                                                                                                  
        #    int a;                                                                                                                                        
        # }
        consume()
        enterBlock()
        if p.tok.tok != TLbracket:
            expectLB()
            return nil
        consume()
        # init-clause may be an expression or a declaration
        var init: Stmt = nil
        if istype(p.tok.tok):
            init = declaration()
            if init == nil:
                expect("declaration")
                return nil
        else:
            if p.tok.tok != TSemicolon:
                let ex = expression()
                if ex == nil:
                    expectExpression()
                    return nil
                init = Stmt(k: SExpr, exprbody: ex)
                if p.tok.tok != TSemicolon:
                    expect("';'")
                    return nil
            consume()
        #  cond-expression 
        var cond: Expr = nil
        if p.tok.tok != TSemicolon:
            cond = expression()
            if cond == nil:
                expectExpression()
                return nil
            if not checkScalar(cond.ty):
                type_error("expect scalar")
            if p.tok.tok != TSemicolon:
                expect("';'")
                return nil
        consume()
        # incl-expression
        var forincl: Expr = nil
        if p.tok.tok != TRbracket:
            forincl = expression()
            if forincl == nil:
                expectExpression()
                return nil
            if p.tok.tok != TRbracket:
                expectRB()
                return nil
        consume()
        var s = statament()
        if s == nil:
            expectStatement()
            return nil
        leaveBlock()
        return Stmt(k: SFor, forinit: init, forcond: cond, forbody: s, forincl: forincl)
    elif p.tok.tok == Kdo:
        consume()
        let s = statament()
        if s == nil:
            expectStatement()
            return nil
        if p.tok.tok != Kwhile:
            expect("'while'")
            return nil
        consume()
        if p.tok.tok != TLbracket:
            expectLB()
            return nil
        consume()
        let e = expression()
        if e == nil:
            expectExpression()
            return nil
        if not checkScalar(e.ty):
            type_error("expect scalar")
        if p.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
        if p.tok.tok != TSemicolon:
            expect("';'")
            return nil
        consume()
        return Stmt(k: SDoWhile, test: e, body: s)
    elif p.tok.tok == TIdentifier:
        var val = p.tok.s
        consume()
        if p.tok.tok == TColon: # # labeled-statement
            consume()
            let s = statament()
            if s == nil:
                expectStatement()
                note("to add a empty statement, use:\n\tlabel: ;")
                return nil
            putLable(val, 100)
            return Stmt(k: SLabled, label: val, labledstmt: s)
        else: # expression
            putToken()
            p.tok = TokenV(tok: TIdentifier, tags: TVSVal, s: val)
    let e = expression()
    if e == nil:
        expectExpression()
        return nil
    if p.tok.tok != TSemicolon:
        expect("';'")
        return nil
    consume()
    return Stmt(k: SExpr, exprbody: e)

proc setParser*() =
  ## create a Parser, and call `setParser proc<#setParser,Parser>`_, then call `reset proc<#reset>`_
  var p = Parser()
  setParser(p)
  reset()
