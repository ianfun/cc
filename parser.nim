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

import stream, core
import std/[math, tables, sets]
from std/sequtils import count

type
    Parser* = ref object
      currentfunctionRet*, currentInitTy*: CType
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
      lables*: seq[TableRef[string, (int, Location)]]
      tags*: seq[TableRef[string, (CType, Location)]]
      typedefs*: seq[TableRef[string, (CType, Location)]]
      tokenq*: seq[TokenV]
      counter*: int
      retTy*: CType # current return type
      type_error*: bool
      eval_error*: bool
      parse_error*: bool
      bad_error*: bool

var p*: Parser = nil

proc parse_error*(msg: string) =
    p.tok = TokenV(tok: TNul, tags: TVNormal)
    if p.parse_error == false:
      stderr.writeLine("\e[34m" & p.filename & ": " & $p.line & '.' & $p.col & ": parse error: " & msg & "\e[0m")
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
  of TNumberLit: $p.tok.i
  of TPPNumber: p.tok.s
  of TCharLit: show(char(p.tok.i))
  of TIdentifier: p.tok.s
  of TFloatLit: $p.tok.f
  of TStringLit: '"' & p.tok.s & '"'
  of TEOF: "<EOF>"
  of TNul: "<null>"
  else:
    if p.tok.tok < T255:
      show(chr(int(p.tok.tok)))
    else:
      $p.tok.tok

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
    stderr.writeLine("\e[33m" & p.filename & ": " & $p.line & '.' & $p.col & ": warning: " & msg & "\e[0m")

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

proc err*(): bool =
  p.type_error or p.parse_error or p.eval_error

proc error*(msg: string) =
    if p.bad_error == false:
      stderr.writeLine("\e[31m" & p.filename & ": " & $p.line & '.' & $p.col & ": error: " & msg & "\e[0m")
      p.bad_error = true

proc type_error*(msg: string) =
    if p.type_error == false:
      stderr.writeLine("\e[35m" & p.filename & ": " & $p.line & '.' & $p.col & ": type error: " & msg & "\e[0m")
      p.type_error = true

proc error_incomplete*(ty: CType) =
  let s = if ty.tag == TYSTRUCT:  "struct" else: (if ty.tag == TYUNION: "union" else: "enum")
  type_error("use of incomplete type '" & s & " " & ty.name & '\'')

proc inTheExpression*(e: Expr) =
    ## emit in the expression message
    parse_error("in the expression " & $e)

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
  p.pathstack.setLen 0
  p.ppstack.setLen 0
  p.fstack.setLen 0
  p.filenamestack.setLen 0
  p.locstack.setLen 0
  p.macros.clear()
  p.filename.setLen 0
  p.path.setLen 0
  p.onces.clear()
  p.expansion_list.clear()
  p.tags.setLen 0
  p.typedefs.setLen 0
  p.lables.setLen 0
  p.tags.add(newTable[string, typeof(p.tags[0][""])]())
  p.typedefs.add(newTable[string, typeof(p.typedefs[0][""])]())
  p.lables.add(newTable[string, typeof(p.lables[0][""])]())
  p.tokenq.setLen 0
  for (name, v) in getDefines():
    p.macros[name] = PPMacro(tokens: v, flags: MOBJ)

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
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYSTRUCT, name: name)
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
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYENUM, name: name)
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
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYUNION, name: name)
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
    p.locstack.add(Location(line: p.line, col: p.col))
    p.filename = filename
    p.path = filename
    p.line = 1
    p.col = 1
    return true

proc putToken*() = 
    p.tokenq.add(p.tok)

proc isprint*(a: cint): cint {.importc: "isprint", nodecl, header: "ctype.h".}

const hexs*: cstring = "0123456789ABCDEF"

const
   sizoefint = sizeof(cint).csize_t
   sizeofpointer = sizeof(pointer).csize_t

proc isFloating*(ty: CType): bool =
    bool(ty.tags and (TYFLOAT or TYDOUBLE))

proc isSigned*(ty: CType): bool =
    bool(ty.tags and (
        TYBOOL or
        TYINT8 or
        TYINT16 or
        TYINT16 or
        TYINT32 or
        TYINT64
    ))

proc getsizeof*(ty: CType): csize_t =
    case ty.spec:
    of TYPRIM:
        if (ty.tags and TYVOID) != 0:
            type_error("cannot get size of void")
            0.csize_t
        elif (ty.tags and TYBOOL) != 0:
            1.csize_t
        elif (ty.tags and TYINT8) != 0:
            1.csize_t
        elif (ty.tags and TYUINT8) != 0:
            1.csize_t
        elif (ty.tags and TYINT16) != 0:
            2.csize_t
        elif (ty.tags and TYUINT16) != 0:
            2.csize_t
        elif (ty.tags and TYINT32) != 0:
            4.csize_t
        elif (ty.tags and TYUINT32) != 0:
            4.csize_t
        elif (ty.tags and TYINT64) != 0:
            8.csize_t
        elif (ty.tags and TYUINT64) != 0:
            8.csize_t
        elif (ty.tags and TYFLOAT) != 0:
            4.csize_t
        elif (ty.tags and TYDOUBLE) != 0:
            8.csize_t
        else:
            type_error("cannot get size of " & $ty)
            0.csize_t
    of TYPOINTER:
        sizeofpointer
    of TYSTRUCT:
        var max = 0.csize_t
        for (_, t) in ty.selems:
            let tmp = getsizeof(t)
            if tmp > max:
                max = tmp
        max
    of TYUNION:
        var sum = 0.csize_t
        for (_, t) in ty.selems:
            sum += getsizeof(t)
        sum
    of TYENUM:
        sizoefint
    of TYBITFIELD:
        getsizeof(ty.bittype)
    of TYARRAY:
        csize_t(ty.arrsize) * getsizeof(ty.arrtype)
    of TYFUNCTION:
        sizeofpointer
    of TYINCOMPLETE:
        error_incomplete(ty)
        0.csize_t

proc getsizeof*(e: Expr): csize_t =
    getsizeof(e.ty)

proc getAlignof*(ty: CType): csize_t =
    getsizeof(ty)

proc getAlignof*(e: Expr): csize_t =
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

proc intcast*(e: Expr, to: CType): Expr = 
    if bool(to.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64)) and 
       bool(e.ty.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64 or TYBOOL)):
        if to.tags == e.ty.tags:
            return Expr(k: ECast, castop: BitCast, castval: e, ty: to)
        if to.tags > e.ty.tags:
            if isSigned(to):
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
            a.tags == b.tags
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
        return castto(Expr(k: ECast, castop: BitCast, castval: e, ty: CType(tags: TYINT, spec: TYPRIM)), to)
    if to.spec == TYENUM:
        # cast xxx to int, then cast(bitcast) to enum
        return Expr(k: ECast, castop: BitCast, castval: castto(e, CType(tags: TYINT, spec: TYPRIM)), ty: to)
    if checkScalar(e.ty) == false or checkScalar(to) == false:
        type_error("cast uoperand shall have scalar type")
        return nil
    if bool(to.tags and TYBOOL):
        return binop(e, NE, Expr(k: EDefault, ty: e.ty), CType(tags: TYBOOL, spec: TYPRIM))
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
            if isSigned(to):
                return Expr(k: ECast, castop: FPToSI, castval: e, ty: to)
            else:
                return Expr(k: ECast, castop: FPToUI, castval: e, ty: to)
    elif bool(to.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64)):
        if e.ty.spec == TYPOINTER:
            return Expr(k: ECast, castop: IntToPtr, castval: e, ty: to)
        if bool(e.ty.tags and (TYFLOAT or TYDOUBLE)):
            if isSigned(e.ty):
                return Expr(k: ECast, castop: SIToFP, castval: e, ty: to)
            else:
                return Expr(k: ECast, castop: UIToFP, castval: e, ty: to)
    return intcast(e, to)

proc to*(e: var Expr, tag: uint32) =
    if e.ty.tags != tag:
        e = castto(e, CType(tags: tag, spec: TYPRIM))

proc integer_promotions*(e: Expr): Expr =
    if e.ty.spec == TYBITFIELD or getsizeof(e) < sizoefint:
        castto(e, CType(tags: TYINT, spec: TYPRIM))
    else:
        e

proc integer_promotions*(e: var Expr) =
    if e.ty.spec == TYBITFIELD or getsizeof(e) < sizoefint:
        to(e, TYINT)

proc conv*(a, b: var Expr) =
    a.ty.tags = a.ty.tags and prim
    b.ty.tags = b.ty.tags and prim
    if a.ty.spec != TYPRIM or b.ty.spec != TYPRIM:
        return
    if (a.ty.tags and TYLONGDOUBLE) != 0:
        to(b, TYLONGDOUBLE)
    elif (b.ty.tags and TYLONGDOUBLE) != 0:
        to(a, TYLONGDOUBLE)
    elif (a.ty.tags and TYDOUBLE) != 0:
        to(b, TYDOUBLE)
    elif (b.ty.tags and TYDOUBLE) != 0:
        to(a, TYDOUBLE)
    elif (a.ty.tags and TYFLOAT) != 0:
        to(b, TYFLOAT)
    elif (b.ty.tags and TYFLOAT) != 0:
        to(a, TYFLOAT)
    else:
        integer_promotions(a)
        integer_promotions(b)
        if a.ty.tags == b.ty.tags:
            return
        let
          sizeofa = getsizeof(a.ty)
          sizeofb = getsizeof(b.ty)
          isaunsigned = bool(a.ty.tags and unsigned)
          isbunsigned = bool(b.ty.tags and unsigned)
        if (isaunsigned and isbunsigned) or (isaunsigned == false and isbunsigned == false):
            if sizeofa > sizeofb:
                to(b, a.ty.tags)
            else:
                to(a, b.ty.tags)
        else:
            if isaunsigned and (sizeofa > sizeofb):
                to(b, a.ty.tags)
            elif isbunsigned and (sizeofb > sizeofa):
                to(a, b.ty.tags)
            elif isaunsigned==false and sizeofa > sizeofb:
                to(b, a.ty.tags)
            elif isbunsigned==false and sizeofb > sizeofa:
                to(a, b.ty.tags)
            else:
                if isaunsigned:
                    to(b, a.ty.tags)
                else:
                    to(a, b.ty.tags)


proc compatible*(p, expected: CType): bool =
    true

proc default_argument_promotions*(e: Expr): Expr =
    if bool(e.ty.tags and TYFLOAT):
        castto(e, CType(tags: TYDOUBLE, spec: TYPRIM))
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
            checkScalar(result, r)
            result = binop(result, if isSigned(r.ty): SAdd else: UAdd, r, r.ty)

proc make_sub*(result, r: var Expr) =
    if isFloating(result.ty):
        result = binop(result, FSub, r, r.ty)
        conv(result, r)
    else:
        if result.ty.spec == TYPOINTER:
            if checkInteger(r.ty) == false:
                type_error("integer expected")
            result = binop(result, SAddP, unary(r, if isSigned(r.ty): SNeg else: UNeg, r.ty), result.ty)
        else:
            checkScalar(result, r)
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
    if isSigned(r.ty):
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
        of TSpace:
            result.add(" ")
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

proc postfix*(e: Expr, op: PostfixOP, ty: CType): Expr = 
    ## construct a postfix operator
    Expr(k: EPostFix, pop: op, poperand: e, ty: ty)

proc consume*() =
    ## eat token from lexer and c preprocesser
    ##
    ## alias for `getToken`
    app.cpp()


const type_specifier_set* = {
    Kchar, Kint, Kshort, Ksigned, 
    Kunsigned, 
    Klong, Kdouble, Kfloat,
    K_Atomic,
    K_Complex, Kvoid, K_Bool
} ## primitive types


const function_specifier_set* = {
    Kinline, K_Noreturn
} ## function specfiers


const storage_class_specifier_set* = {
    Ktypedef, Kextern, Kstatic, 
    K_Thread_local, Kauto, Kregister
} ## storage specfiers


const type_qualifier_set* = {
     Kvolatile, Krestrict, Kconst
} ## type qualifiers


const declaration_specifier_set* = 
    type_specifier_set +
    storage_class_specifier_set +
    type_qualifier_set +
    function_specifier_set ## declaration specfiers

proc addTag(ty: var CType, t: Token): bool =
    ## add a tag to type
    let t = (
        case t:
        of Kinline: TYINLINE
        of K_Noreturn: TYNORETURN
        of K_Alignas: TYALIGNAS
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
    if len(s) > 0:
        if Ktypedef in s:
            return ty
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
                    expect("')'")
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
        let ty = gettypedef(p.tok.s)[0]
        more(s)
        if ty != nil and (ty.tags and TYTYPEDEF) != 0:
            return handle_typedef(s, ty)
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
        ty = CType(tags: TYINVALID, spec: TYPOINTER, p: ty)
        type_qualifier_list(ty)
    return direct_declarator(ty, flags)

proc initializer_list*(): Expr =
    if p.tok.tok != TLcurlyBracket:
        return assignment_expression()
    result = Expr(k: EArray)
    consume()
    while true:
        if p.tok.tok == TRcurlyBracket:
            consume()
            break
        if p.tok.tok == TLcurlyBracket:
            let e = initializer_list()
            if e == nil:
                return nil
            result.arr.add(e)
        else:
            let e = assignment_expression()
            if e == nil:
                parse_error("expect expression")
                return nil
            result.arr.add(e)
        if p.tok.tok == TComma:
            consume()

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
            expect("')'")
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
        var ty = CType(tags: TYINVALID, spec: TYARRAY, arrsize: -1, arrtype: base)
        if p.tok.tok == TMul: # int arr[*]
          consume()
          if p.tok.tok != TRSquareBrackets:
            parse_error("expect ']'")
            note("the syntax is:\n\tint arr[*]")
            return nil
          return Stmt(k: SVarDecl1, var1name: name, var1type: ty)
        if p.tok.tok == Kstatic:
           consume()
           ty.tags = ty.tags or TYSTATIC
           type_qualifier_list(ty)
        else:
            type_qualifier_list(ty)
            if p.tok.tok == Kstatic:
                ty.tags = ty.tags or TYSTATIC
                consume()
        if p.tok.tok != TRSquareBrackets:
            let e = assignment_expression()
            if e == nil:
                expect("expression")
                return nil
            ty.arrsize = app.eval_const_expression(e)
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
            expect("')'")
            return nil
        consume()
        if p.tok.tok == TLcurlyBracket: # function definition
            for (name, vty) in ty.params:
                putsymtype(name, vty)
            var body: Stmt
            block:
                var oldRet = p.currentfunctionRet
                p.currentfunctionRet = ty.ret
                body = compound_statement()
                p.currentfunctionRet = oldRet
            if body == nil:
                expect("function body")
                return nil
            if len(body.stmts) == 0 or (body.stmts[^1].k != SReturn and body.stmts[^1].k != SGoto):
                if bool(ty.ret.tags and TYVOID):
                    warning("no 'return' statement in a function should return a value")
                body.stmts &= Stmt(k: SReturn, exprbody: if bool(ty.ret.tags and TYVOID): nil else: Expr(k: EDefault, ty: ty.ret))
            return Stmt(funcname: name, k: SFunction, functy: ty, funcbody: body)
        return direct_declarator_end(ty, name)
    else:
        return Stmt(k: SVarDecl1, var1name: name, var1type: base)

proc struct_declarator(base: CType): (string, CType) =
    if p.tok.tok == TColon:
        consume()
        let e = constant_expression()
        if e == nil:
            expect("expression")
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
                expect("expression")
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

let enum_type = CType(tags: TYEXPR or TYINT, spec: TYPRIM)

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
        putsymtype(s, enum_type)
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

proc istype(a: Token): bool =
    if a in (declaration_specifier_set + {Kstruct, Kenum, Kunion}):
        return true
    if a == TIdentifier:
      let ty = gettypedef(p.tok.s)[0]
      return ty != nil and (ty.tags and TYTYPEDEF) != 0
    return false

proc declaration_specifiers*(): CType =
    ## declaration_specfier is used in function and variable declaration/definition
    var s: seq[Token]
    while p.tok.tok in (declaration_specifier_set + {Kstruct, Kenum, Kunion}):
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
                    expect("')'")
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
        let ty = gettypedef(p.tok.s)[0]
        more(s)
        if ty != nil and (ty.tags and TYTYPEDEF) != 0:
            return handle_typedef(s, ty)
    if s.len > 0:
        return merge_types(s)
    warning("type defaults to 'int' in declaration")
    return CType(tags: TYINVALID or TYINT, spec: TYPRIM)

proc static_assert*(): Stmt =
    consume()
    if p.tok.tok != TLbracket:
        expect("'('")
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
            expect("')'")
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

proc declaration*(): Stmt =
    ## parse variable declaration or function definition
    ## 
    ## this is different from ISO C grammar
    if p.tok.tok == K_Static_assert:
        return static_assert()
    else:
        var base = declaration_specifiers()
        if base == nil:
            expect("declaration-specifiers")
            return nil
        if p.tok.tok == TSemicolon:
            if base.spec notin {TYSTRUCT, TYUNION, TYENUM}:
                warning("declaration does not declare anything")
                note("add a variable name in the declaration")
            consume()
            return Stmt(k: SDeclOnly, decl: base)
        result = Stmt(k: SVarDecl)
        while true:
            let st = declarator(base)
            if st == nil:
                expect("declarator")
                return nil
            if st.k == SFunction:
                checkRetType(st.functy)
                putsymtype(st.funcname, st.functy)
                return st
            assert st.k == SVarDecl1
            if st.var1type.spec == TYINCOMPLETE:
                checkInComplete(st.var1type)
                return nil
            if (st.var1type.tags and TYINLINE) != 0:
                warning("inline declaration is in block scope has no effect")
            checkRetType(st.var1type)
            result.vars.add((st.var1name, st.var1type, nil))
            putsymtype(st.var1name, st.var1type)
            if p.tok.tok == TAssign:
                var init: Expr
                consume()
                block:
                    var old = p.currentInitTy
                    p.currentInitTy = st.var1type
                    init = initializer_list()
                    p.currentInitTy = old
                if init == nil:
                    expect("initializer-list")
                    return nil
                if st.var1type.spec == TYFUNCTION:
                    type_error("function declaration has no initializer")
                    note("only variables can be initialized")
                else:
                    result.vars[^1][2] = init
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
            let (n, isf) = type_name()
            discard isf
            if n == nil:
                return nil
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
                return Expr(k: EVoid, voidexpr: e, ty: CType(tags: TYVOID, spec: TYPRIM))
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

proc unary_expression*(): Expr =
    let tok = p.tok.tok
    case tok:
    of TNot:
        consume()
        let e = cast_expression()
        if e == nil:
            return nil
        result = unary(e, LogicalNot, CType(tags: TYINT, spec: TYPRIM))
        return result
    of TMul:
        consume()
        let e = cast_expression()
        if e.ty.spec != TYPOINTER:
            type_error("pointer expected")
            return nil
        result = unary(e, Dereference, e.ty.p)
        return result
    of TBitAnd, TBitNot:
        consume()
        let e = cast_expression()
        if e == nil:
            return nil
        let op = if tok == TBitAnd: AddressOf else: Not
        result = unary(e, op, e.ty)
        integer_promotions(result)
        return result
    of TDash:
        consume()
        let e = unary_expression()
        if e == nil:
            return nil
        integer_promotions(result)
        result = unary(e, if isSigned(e.ty): SNeg else: UNeg, e.ty)
        return result
    of TAdd:
        consume()
        let e = unary_expression()
        if e == nil:
            return nil
        integer_promotions(result)
        result = unary(e, Pos, e.ty)
        return result
    of TAddAdd, TSubSub:
        consume()
        let e = unary_expression()
        if e == nil:
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
                    expect("')'")
                    return nil
                consume()
                return Expr(ty: CType(tags: TYSIZE_T, spec: TYPRIM), k: EIntLit, ival: cast[int](getsizeof(ty[0])))
            let e = unary_expression()
            if e == nil:
                expect("expression")
                return nil
            if p.tok.tok != TRbracket:
                expect("')'")
                return nil
            consume()
            return Expr(ty: CType(tags: TYSIZE_T, spec: TYPRIM), k: EIntLit, ival: cast[int](getsizeof(e)))
        else:
            let e = unary_expression()
            if e == nil:
                expect("expression")
                return nil
            return Expr(ty: CType(tags: TYSIZE_T, spec: TYPRIM), k: EIntLit, ival: cast[int](getsizeof(e)))
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
            result = Expr(ty: CType(tags: TYSIZE_T, spec: TYPRIM), k: EIntLit, ival: cast[int](getAlignof(ty[0])))
        else:
            let e = constant_expression()
            if e == nil:
                expect("expression")
                return nil
            result = Expr(ty: CType(tags: TYSIZE_T, spec: TYPRIM), k: EIntLit, ival: cast[int](getAlignof(e)))
        if p.tok.tok != TRbracket:
            expect("')'")
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
            return Expr(k: EString, str: s, ty: CType(tags: TYCONST, spec: TYPOINTER, p: CType(tags: TYCHAR, spec: TYPRIM)))
        of 16:
            var a: seq[Expr]
            let ty = CType(tags: TYUSHORT, spec: TYPRIM)
            for i in writeUTF8toUTF16(s):
                a.add(Expr(k: EIntLit, ival: cast[int](i), ty: ty))
            return Expr(k: EArray, ty: CType(tags: TYINVALID, spec: TYPOINTER, p: ty), arr: a)
        of 32:
            var a: seq[Expr]
            let ty = CType(tags: TYUINT, spec: TYPRIM)
            for i in writeUTF8toUTF32(s):
                a.add(Expr(k: EIntLit, ival: cast[int](i), ty: ty))
            return Expr(k: EArray, ty: CType(tags: TYINVALID, spec: TYPOINTER, p: ty), arr: a)
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
            result = Expr(ty: CType(tags: TYINT, spec: TYPRIM), k: EIntLit, ival: n)
            consume()
        of 2, 3:
            result = Expr(ty: CType(tags: if ok == 2: TYDOUBLE else: TYFLOAT, spec: TYPRIM), k: EFloatLit, fval: f)
            consume()
        of 4:
            result = Expr(ty: CType(tags: TYLONG, spec: TYPRIM), k: EIntLit, ival: n)
            consume()
        of 5:
            result = Expr(ty: CType(tags: TYULONG, spec: TYPRIM), k: EIntLit, ival: n)
            consume() 
        of 6:
            result = Expr(ty: CType(tags: TYLONGLONG, spec: TYPRIM), k: EIntLit, ival: n)
            consume()
        of 7:
            result = Expr(ty: CType(tags: TYULONGLONG, spec: TYPRIM), k: EIntLit, ival: n)
            consume()
        of 8:
            result = Expr(ty: CType(tags: TYUINT, spec: TYPRIM), k: EIntLit, ival: n)
            consume()
        else:
            unreachable()
    of CPPident:
        result = Expr(k: EIntLit, ival: 0, ty: CType(tags: TYINT, spec: TYPRIM))  
        consume()
    of TIdentifier:
        if p.want_expr:
            result = Expr(k: EIntLit, ival: 0, ty: CType(tags: TYINT, spec: TYPRIM))  
        else: 
            let ty = getsymtype(p.tok.s)
            if ty == nil:
                type_error("symbol not found: " & p.tok.s)
                return nil
            result = Expr(k: EVar, sval: p.tok.s, ty: ty)
        consume()
    of TLbracket:
        consume()
        result = expression()
        if result == nil:
            expect("expression")
            return nil
        if p.tok.tok != TRbracket:
            expect("')'")
            return nil
        consume()
    of K_Generic:
        consume()
        if p.tok.tok != TLbracket:
            expect("'('")
            return nil
        consume()
        let test = assignment_expression()
        if test == nil:
            expect("expression")
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
                expect("expression")
                note("the syntax is:\n\t_Generic(expr, type1: expr, type2: expr, ..., default: expr)")
                return nil
            if tname == nil:
                defaults = e
            elif tname.tags == testty.tags and tname.spec == testty.spec:
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
                if isarrow:
                    return Expr(k: EPointerMemberAccess, obj: e, idx: i, ty: e.ty.selems[i][1])
                return Expr(k: EMemberAccess, obj: e, idx: i, ty: e.ty.selems[i][1])
        type_error("struct/union " & $e.ty.sname & " has no member " & p.tok.s)
        return nil
    of TLbracket: # function call
        consume()
        if e.ty.spec != TYPOINTER or e.ty.p.spec != TYFUNCTION:
            type_error("expression " & $e & " is of type " & $e.ty & " and is not callable")
            return nil
        var args: seq[Expr]
        if p.tok.tok == TRbracket:
            consume()
        else:
            while true:
                let a = assignment_expression()
                if a == nil:
                    expect("expression")
                    note("the syntax is:\n\tfunction-name(argument)")
                    return nil
                args.add(a)
                if p.tok.tok == TComma:
                    consume()
                elif p.tok.tok == TRbracket:
                    consume()
                    break
        let params = e.ty.params
        if len(params) > 0 and params[^1][1] == nil: # varargs
            if len(args) < (len(params) - 1):
                type_error("too few arguments to variable argument function")
                note("at lease " & $(len(params) - 0) & " arguments needed")
                return nil
        elif len(params) != len(args):
            type_error("expect " & $(len(params)) & " parameters, " & $len(args) & " provided")
            inTheExpression(e)
            return nil
        var i = 0
        while true:
            if i == len(params):
                break
            if params[i][1] == nil:
                for j in i ..< len(args):
                    args[j] = default_argument_promotions(args[j])
                break
            if not compatible(args[i].ty, expected=params[i][1]):
                type_error("function call type incompatible: in argument " & $i & " of calling function " & $e)
                note("expected " & $params[i] & " but argument is of type " & $args[i].ty)
                return nil
            args[i] = castto(args[i], params[i][1])
            inc i
        return Expr(k: ECall, callfunc: e, callargs: args, ty: e.ty.ret)
    of TLSquareBrackets: # array subscript
        consume()
        let e = expression()
        if e == nil:
            expect("expression")
            note("the syntax is:\n\tarray[expression]")
            return nil
        if p.tok.tok != TRSquareBrackets:
            expect("']'")
            return
        if e.ty.spec == TYPOINTER:
            discard
        else:
            type_error("expression " & $e & " is of type " & $e.ty & ", and is not subscriptable")
            return nil
        consume()
        return Expr(k: ESubscript, left: e, right: e, ty: e.ty.p)
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
            result = binop(result, if isFloating(r.ty): FLT else: (if isSigned(r.ty): SLT else: ULT), r, result.ty)
        of TLe:
            consume()
            var r = shift_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result = binop(result, if isFloating(r.ty): FLE else: (if isSigned(r.ty): SLE else: ULE), r, result.ty)
        of TGt:
            consume()
            var r = shift_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result = binop(result, if isFloating(r.ty): FGT else: (if isSigned(r.ty): SGT else: UGT), r, result.ty)
        of TGe:
            consume()
            var r = shift_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result = binop(result, if isFloating(r.ty): FGE else: (if isSigned(r.ty): SGE else: UGE), r, result.ty)
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
            checkSpec(result, r)
            result = binop(result, if isFloating(r.ty): FEQ else: EQ, r, result.ty)
        of TNe:
            consume()
            var r = relational_expression()
            if r == nil:
                return nil
            checkSpec(result, r)
            result = binop(result, if isFloating(r.ty): FNE else: NE, r, result.ty)
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
            result = binop(result, LogicalAnd, r, CType(tags: TYINT, spec: TYPRIM))
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
            result = binop(result, LogicalOr, r, CType(tags: TYINT, spec: TYPRIM))
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
        expect("expression")
        return nil
    if p.tok.tok != TColon:
        return lhs
    consume()
    var rhs = conditional_expression()
    if rhs == nil:
        expect("expression")
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

proc assignable(e: Expr): bool =
    case e.k:
    of EVar:
        if e.ty.spec == TYARRAY:
            type_error("array object is not assignable")
            false
        else:
            true
    of EUnary:
        e.uop == Dereference
    of EPostFix:
        assignable(e.poperand)
    else:
        false

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
        var e = assignment_expression()
        if e == nil:
            return nil
        if not assignable(e):
            type_error("expression " & $e & " is not assignable")
            return nil
        var lhs = deepCopy(result)
        return binop(lhs, Assign,(
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
            )
        , lhs.ty)
    else:
        return result

proc translation_unit*(): Stmt =
    ## parse top-level declaration until EOF reaches, the entry point of program
    ##
    ## never return nil, return a compound statement
    result = Stmt(k: SCompound)
    while p.tok.tok != TEOF:
        let s = declaration()
        if s == nil:
            break
        echo s
        result.stmts.add(s)

proc runParser*(): Stmt =
    ## eat first token and parse a translation_unit
    ##
    ## never return nil, return a compound statement
    consume()
    return translation_unit()

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

proc statament*(): Stmt =
    ## parse a statement
    if p.tok.tok == TSemicolon:
        consume()
        return Stmt(k: SSemicolon)
    elif p.tok.tok == TLcurlyBracket:
        return compound_statement()
    elif p.tok.tok == Kcase:
        consume()
        let e = constant_expression()
        if e == nil:
            expect("constant-expression")
            return nil
        if p.tok.tok != TColon:
            parse_error("':' expected")
            return nil
        consume()
        let s = statament()
        if s == nil:
            expect("statament")
            return nil
        return Stmt(k: Scase, case_expr: e, case_stmt: s)
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
        let l = getLabel(location)[0]
        consume()
        if p.tok.tok != TSemicolon:
            parse_error("expect ';'")
            return nil
        consume()
        if l == -1:
            type_error("undeclared label " & location)
            return nil
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
            expect("expression")
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
        if not compatible(e.ty, p.currentfunctionRet):
            type_error("incompatible type in 'return' statement")
            note("expect " & $p.currentfunctionRet & ", but got " & $e.ty)
            inTheExpression(e)
        return Stmt(k: SReturn, exprbody: castto(e, p.currentfunctionRet))
    elif p.tok.tok == Kif:
        consume()
        if p.tok.tok != TLbracket:
            expect("'('")
            return nil
        consume()
        let e = expression()
        if e == nil:
            expect("expression")
            return nil
        if p.tok.tok != TRbracket:
            expect("')'")
            return nil
        consume()
        let s = statament()
        if s == nil:
            expect("statament")
            return nil
        var elsebody: Stmt = nil
        if p.tok.tok == Kelse:
            consume()
            elsebody = statament()
            if elsebody == nil:
                expect("statament")
                return nil
        return Stmt(k: SIf, iftest: e, ifbody: s, elsebody: elsebody)
    elif p.tok.tok == Kwhile or p.tok.tok == Kswitch:
        let tok = p.tok.tok
        consume()
        if p.tok.tok != TLbracket:
            expect("'('")
            return nil
        consume()
        let e = expression()
        if e == nil:
            expect("expression")
            return nil
        if p.tok.tok != TRbracket:
            expect("')'")
            return nil
        consume()
        let s = statament()
        if s == nil:
            expect("statament")
            return nil
        if tok == Kwhile:
            return Stmt(k: SWhile, test: e, body: s)
        else:
            return Stmt(k: SSwitch, test: e, body: s)  
    elif p.tok.tok == Kfor:
        # this are valid in C
        # for(int a;;)                                                                                                                                          
        # {                                                                                                                                                  
        #    int a;                                                                                                                                        
        # }
        consume()
        enterBlock()
        if p.tok.tok != TLbracket:
            expect("'('")
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
                    expect("expression")
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
                expect("expression")
                return nil
            if p.tok.tok != TSemicolon:
                expect("';'")
                return nil
        consume()
        # incl-expression
        var forincl: Expr = nil
        if p.tok.tok != TRbracket:
            forincl = expression()
            if forincl == nil:
                expect("expression")
                return nil
            if p.tok.tok != TRbracket:
                expect("')'")
                return nil
        consume()
        var s = statament()
        if s == nil:
            expect("statament")
            return nil
        leaveBlock()
        return Stmt(k: SFor, forinit: init, forcond: cond, forbody: s, forincl: forincl)
    elif p.tok.tok == Kdo:
        consume()
        let s = statament()
        if s == nil:
            expect("statament")
            return nil
        if p.tok.tok != Kwhile:
            expect("'while'")
            return nil
        consume()
        if p.tok.tok != TLbracket:
            expect("'('")
            return nil
        consume()
        let e = expression()
        if e == nil:
            expect("expression")
            return nil
        if p.tok.tok != TRbracket:
            expect("')'")
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
                expect("statament")
                note("to add a empty statement, use:\n\tlabel: ;")
                return nil
            putLable(val, 100)
            return Stmt(k: SLabled, label: val, labledstmt: s)
        else: # expression
            putToken()
            p.tok = TokenV(tok: TIdentifier, tags: TVSVal, s: val)
    let e = expression()
    if e == nil:
        expect("expression")
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
