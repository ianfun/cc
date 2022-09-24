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

when defined(windows):
    # download from https://github.com/llvm/llvm-project/releases
    # LLVM-15.0.0-rc3-win64.exe 
    # unpack and install
    {.passL: "C:\\Users\\林仁傑\\source\\llvm-mingw-20220906-ucrt-x86_64\\bin\\libLLVM-15.dll ./llvm/llvmAPI.o".}
else:
    # llvm-config --ldflags --system-libs --libs all
    {.passL: "-L/usr/lib/llvm-15/lib -lLLVM-15 ./llvm/llvmAPI -lreadline -ltinfo".}

import std/[math, sets, tables, editdistance, heapqueue, sequtils, os, times, unicode]
import llvm/llvm
import core, stream, ast, token, operators, location, messageWriter, constString

type
  Preprocessor* {.final.} = object ## The C Preprocessor
    counter*: uint ## `__COUNTER__`
    onces*: HashSet[string] ## `#pragma once`
    ok*: bool ## preprocessor condition is false: `#if 0`
    macros*: Table[string, PPMacro] ## defined macros
    ppstack*: seq[uint8] ## preprocessor condition stack
    want_expr*: bool ## true if parsing `#if`
    expansion_list*: HashSet[string] ## current expanding macros
    eval_error*: bool ## error during evalate constant expressions
    eval_error_msg*: string ## eval error message
    flags*: PPFlags ## preprocessor flags
  Sema* {.final.} = object ## Semantics processor
    currentfunctionRet*, currentInitTy*, currentCase*: CType
    pfunc*: string ## current function name: `__func__`
    retTy*: CType ## current function return type
    currentAlign*: uint32 ## current align(bytes)
    type_error*: bool ## type error
    lables*: seq[TableRef[string, uint8]] ## labels
    tags*: seq[TableRef[string, Info]] ## struct/union/enums
    typedefs*: seq[TableRef[string, Info]] ## typedef, variables
  Lexer* {.final.} = object
    tok*: TokenV
    line*: int ## current line
    col*: int ## current column
    c*: char ## current char
    lastc*: uint16
  Parser* {.final.} = object ## Parser can parse a translation unit.A *translation unit* is many input files, including `#include <xxx>`
    tokenq*: seq[TokenV]
    l*: Lexer ## current Lexer
    pp*: Preprocessor
    sema*: Sema
    w*: MessageWriter
    fstack*: seq[Stream] ## input files
    filenamestack*, pathstack*: seq[string]
    locstack*: seq[Location] 
    filename*, path*: string
    parse_error*: bool ## parse error
    bad_error*: bool ## other error
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
    triple*: tripleRef
    archName*: cstring
    arch*: ArchType
    os*: OSType
    env*: EnvironmentType
    f*: uint32 ## some flags
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

var t* = Parser(
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
        w: newConsoleColoredWritter(),
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

proc showToken*(): string =
  case t.l.tok.tok:
  of TNumberLit: "number " & $t.l.tok.i
  of TPPNumber: "number " & t.l.tok.s
  of TCharLit: "char " & show(char(t.l.tok.i))
  of TIdentifier, CPPIdent: "identifier " & t.l.tok.s
  of TSpace: "space"
  of PPSharp: "'#'"
  of PPPlaceholder: "<placeholder>"
  of PPSharpSharp: "'##'"
  of TFloatLit: "float " & $t.l.tok.f
  of TEllipsis2: "'..'"
  of TEllipsis: "'...'"
  of TStringLit: "string \"" & t.l.tok.s & '"'
  of TEOF: "<EOF>"
  of TNul: "<null>"
  else:
    if t.l.tok.tok < T255:
      "char " & show(chr(int(t.l.tok.tok)))
    else:
      "keyword " & $t.l.tok.tok

proc make_tok*(op: Token) {.inline.} =
    t.l.tok = TokenV(tok: op, tags: TVNormal)

proc make_ch_lit*(ch: char) {.inline.} =
    t.l.tok.i = ch.int

template error(msg: untyped) =
    t.w.write(msg, MError)

template raw_message(msg: untyped) =
    t.w.write(msg, MRaw)

template perror(msg: untyped) =
    t.w.write(msg, Mperror)

template type_error(msg: untyped) =
    t.w.write(msg, MTypeError)
    t.sema.type_error = true

template eval_error(msg: untyped) =
    t.w.write(msg, MEvalError)
    t.sema.type_error = true

proc error_incomplete(ty: CType) =
  let s = if ty.tag == TYSTRUCT:  "struct" else: (if ty.tag == TYUNION: "union" else: "enum")
  type_error("use of incomplete type '" & s & " " & ty.name & '\'')

template parse_error(msg: untyped) =
    if t.parse_error == false and t.sema.type_error == false:
      t.w.write(msg, MParseError)
      t.parse_error = true

template warning(msg: untyped) =
    if ord(app.verboseLevel) >= ord(VWarning):
        t.w.write(msg, MVerbose)

template expect*(msg: untyped) =
    ## emit `expect ...` error message
    parse_error("expect " & msg & ", got " & showToken())

template expectExpression*() =
    expect("expression")

template expectStatement*() =
    expect("statement")

template expectLB*() =
    expect("'('")

template expectRB*() =
    expect("')'")

template verbose(msg: untyped) =
    if ord(app.verboseLevel) >= ord(VVerbose):
        t.w.write(msg, MVerbose)

template note(msg: untyped) =
    if ord(app.verboseLevel) >= ord(VNote):
        t.w.write(msg, MNote)

proc addOnce*() =
    t.pp.onces.incl t.path

proc putToken*() = 
    t.tokenq.add(t.l.tok)

proc addFile*(filename: string) =
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

proc my_UNEG(a: uintmax_t): intmax_t {.importc: "myopneg", nodecl, header: "myop.h".}

proc my_SNEG(a: intmax_t): intmax_t {.importc: "myopneg", nodecl, header: "myop.h".}

proc `&&`(a, b: intmax_t): intmax_t {.importc: "myopand", nodecl, header: "myop.h".}

proc `||`(a, b: intmax_t): intmax_t {.importc: "myopor", nodecl, header: "myop.h".}

proc `!`(a: intmax_t): intmax_t {.importc: "myopnot", nodecl, header: "myop.h".}

proc evali*(e: Expr): intmax_t

proc eval_const_expression(e: Expr): intmax_t = evali(e)

proc eval_error2(msg: string): intmax_t =
  t.pp.eval_error = true
  t.pp.eval_error_msg = msg
  return intmax_t(0)

proc write_eval_msg*() = 
    eval_error(t.pp.eval_error_msg)

proc evali*(e: Expr): intmax_t =
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
        cast[intmax_t](cast[uintmax_t](evali(e.lhs)) div cast[uintmax_t](evali(e.rhs)))
      of SDiv:
        evali(e.lhs) div evali(e.rhs)
      of URem:
        cast[intmax_t](cast[uintmax_t](evali(e.lhs)) mod cast[uintmax_t](evali(e.rhs)))
      of Shr:
        cast[intmax_t](cast[uintmax_t](evali(e.lhs)) shr cast[uintmax_t](evali(e.rhs)))
      of AShr:
        # https://stackoverflow.com/questions/53746160/how-to-implement-arithmetic-right-shift-in-c
        # https://stackoverflow.com/questions/7622/are-the-shift-operators-arithmetic-or-logical-in-c
        evali(e.lhs) shr evali(e.rhs)
      of EQ:
        if evali(e.lhs) == evali(e.rhs): 1 else: 0
      of NE:
        if evali(e.lhs) != evali(e.rhs): 1 else: 0
      of SGE:
        if evali(e.lhs) >= evali(e.rhs): 1 else: 0
      of SGT:
        if evali(e.lhs) > evali(e.rhs): 1 else: 0
      of SLE:
        if evali(e.lhs) <= evali(e.rhs): 1 else: 0
      of SLT:
        if evali(e.lhs) < evali(e.rhs): 1 else: 0
      of UGE:
        if cast[uintmax_t](evali(e.lhs)) >= cast[uintmax_t](evali(e.rhs)): 1 else: 0
      of UGT:
        if cast[uintmax_t](evali(e.lhs)) > cast[uintmax_t](evali(e.rhs)): 1 else: 0
      of ULE:
        if cast[uintmax_t](evali(e.lhs)) <= cast[uintmax_t](evali(e.rhs)): 1 else: 0
      of ULT:
        if cast[uintmax_t](evali(e.lhs)) < cast[uintmax_t](evali(e.rhs)): 1 else: 0
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
        eval_error2("cannot eval constant-expression: " & $e)
  of EUnary:
    case e.uop:
    of Pos:
      evali(e.uoperand)
    of UNeg:
      cast[intmax_t](my_UNEG(cast[uintmax_t](evali(e.uoperand))))
    of SNeg:
      my_SNEG(evali(e.uoperand))
    of LogicalNot:
      ! evali(e.uoperand)
    of Not:
      not evali(e.uoperand)
    else:
      eval_error2("cannot eval constant-expression: bad unary operator: " & $e)
  of EIntLit:
    e.ival
  of EFloatLit:
    return eval_error2("floating constant in constant-expression")
  of ECondition:
    if evali(e.cond) != 0: evali(e.cleft) else: evali(e.cright)
  else:
    eval_error2("cannot eval constant-expression: " & $e)

proc getToken*()

proc nextTok*()

proc isalnum*(c: char): bool =
    return c in {'A'..'Z', 'a'..'z', '0'..'9'}

proc resetLine() =
    t.l.col = 1
    inc t.l.line

proc fs_read*() =
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
            fs_read() # tail call
        if t.l.c == '\n':
          resetLine()
        elif t.l.c == '\r':
          resetLine()
          t.l.c = s.readChar()
          if t.l.c != '\n':
            putc(s, cint(t.l.c))

proc lowleveleat() =
    fs_read()

proc do_eat*() =
    lowleveleat()
    if t.l.c == '/':
        lowleveleat()
        if t.l.c == '*':
            while true:
                lowleveleat()
                if t.l.c == '*':
                    lowleveleat()
                    if t.l.c == '/':
                        t.l.c = ' ' # make a space: GNU CPP do it
                        break
                elif t.l.c == '\0':
                    parse_error("expect '*/' before EOF")
                    return
        elif t.l.c == '/':
            while true:
                lowleveleat()
                if t.l.c == '\n' or t.l.c == '\0':
                    break
        else:
            t.l.lastc = uint16(t.l.c)
            t.l.c = '/'
    elif t.l.c == '\\':
        let c = t.l.c
        lowleveleat()
        if t.l.c == '\n':
            do_eat()
        else:
            t.l.lastc = uint16(t.l.c)
            t.l.c = c

proc eat*() =
    ## skip any C/C++ style comments
    ## https://gcc.gnu.org/onlinedocs/gcc-12.1.0/cpp/Initial-processing.html
    if t.l.lastc <= 255:
        t.l.c = char(t.l.lastc)
        t.l.lastc = 256
        return
    do_eat()

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
            t.l.tok.f *= pow(2, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
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
            t.l.tok.f *= pow(10, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
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
              t.l.tok.i = (t.l.tok.i shl 4) or (int(t.l.c) - '0'.int)
            elif t.l.c in {'a'..'f'}:
              t.l.tok.i = (t.l.tok.i shl 4) or (int(t.l.c) - 'a'.int + 10)
            elif t.l.c in {'A'..'F'}:
              t.l.tok.i = (t.l.tok.i shl 4) or (int(t.l.c) - 'A'.int + 10)
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
        t.l.tok.i = (10 * t.l.tok.i) + (int(t.l.c) - '0'.int)
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

proc macro_define*(name: string, m: PPMacro) =
  let pr = t.pp.macros.getOrDefault(name, nil)
  if pr != nil:
    if not ppMacroEq(pr, m):
      warning("macro " & name & " redefined")
  t.pp.macros[name] = m

proc macro_defined*(name: string): bool =
  t.pp.macros.contains(name)

proc macro_undef*(name: string) =
  t.pp.macros.del(name)

proc nextTok*() =
    ## Tokenize
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
                var name = t.l.tok.s # copy to string
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
                    macro_define(name, m)
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
                    ok = eval_const_expression(e) != 0
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
                let v = if ndef: not macro_defined(name) else: macro_defined(name)
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
                        ok = eval_const_expression(e) != 0
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
                t.l.line = decimaltoInt(s)
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
                macro_undef(t.l.tok.s)
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
          warning("invalid token: " & show(t.l.c))

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
          getToken()
          if t.l.tok.tok != TRbracket:
              expectRB()
          else:
              echo "pragma: ", pra
              getToken()

proc getMacro*(name: string): PPMacro =
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

proc macro_find*(name: string): PPMacro =
  t.pp.macros.getOrDefault(name, nil)

proc beginExpandMacro*(a: string) =
  t.pp.expansion_list.incl a

proc endExpandMacro*(a: string) =
  t.pp.expansion_list.excl a

proc isMacroInUse*(a: string): bool =
  t.pp.expansion_list.contains(a)

proc checkMacro()

proc getToken*() =
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


var b* = Backend()

var i = 0
var options* = commandLineParams()
var appFileName* = getAppFilename()

proc hasNext(): bool =
  return i < len(options)

proc get(): string =
  result = options[i]
  inc i

type P = proc () {.nimcall.}

proc system(command: cstring): cint {.importc: "system", header: "stdio.h".}
# we use gcc to invoke linker instead of ld commandm which require a lot of commands
# /usr/bin/ld -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o a.out /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crt1.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/crtbegin.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/usr/lib/x86_64-linux-gnu/../../lib64 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../.. -L/usr/lib/llvm-10/bin/../lib -L/lib -L/usr/lib /tmp/t-ef1090.o -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/10/crtend.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crtn.o
when defined(windows):
  # windows has `_execlp()`, but windows has no `fork()`
  # CreateProcess is ok, but `system()` is easy
  proc runLD*(input, path: string) =
    var cmd = "gcc \"" & $input & "\" -o \"" & $path & '"'
    let status = system(cmd.cstring)
    if status != 0:
      error("gcc returned " & $status & " exit status")
else:
  import posix
  proc runLD*(input, path: string) =
    var pid = fork()
    if pid < 0:
      perror("fork")
    else:
      if pid == 0:
        discard execlp("gcc", "gcc", input.cstring, "-o", path.cstring, nil)
        perror("execlp")
      else:
        var status: cint = 0
        discard waitpid(pid, status, 0)
        if status != 0:
            error("gcc returned " & $status & " exit status")

proc runLLD*(input, output: string) =
  var cmd = "ld.lld --hash-style=gnu --no-add-needed  --build-id --eh-frame-hdr -dynamic-linker /lib/x86_64-linux-gnu/libc.so.6 /usr/lib64/ld-linux-x86-64.so.2 "
  cmd &= input
  cmd &= " -o "
  cmd &= output
  let status = system(cmd.cstring)
  if status != 0:
    error("ld.lld returned " & $status & " exit status")

proc dumpVersionInfo*() =
  var arr = [cstring("llvm"), cstring("--version")]
  parseCommandLineOptions(2, cast[cstringArray](addr arr[0]), nil)

proc showVersion() =
  fstderr <<< "CC: C Compiler"
  fstderr <<< "Homepage: https://github.com/ianfun/cc.git"
  fstderr <<< "Bug report: https://github.com/ianfun/cc/issues"
  dumpVersionInfo()

proc setInput(s: string) =
  case s:
  of "c", "C":
    app.input = InputC
  of "S", "s", "assembler", "asm", "Asm", "ASM":
    app.input = InputAsm
  of "ir", "IR":
    app.input = InputIR
  of "bc", "BC":
    app.input = InputBC
  else:
    error("unrecognized input language: " & s)
    quit 0

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

proc h(s: string): (ConstString, uint64) = (constStr(s), hash1(s))

proc newBackend*() =
  initializeCore(getGlobalPassRegistry())
  b.tsCtx = orcCreateNewThreadSafeContext()
  b.ctx = orcThreadSafeContextGetContext(b.tsCtx)
  b.builder = createBuilderInContext(b.ctx)
  if app.input == InputC:
    contextSetDiscardValueNames(b.ctx, True)
  b.voidty = voidTypeInContext(b.ctx)
  b.ptrty = pointerTypeInContext(b.ctx, 0)
  b.i1 = int1TypeInContext(b.ctx)
  b.i8 = int8TypeInContext(b.ctx)
  b.i16 = int16TypeInContext(b.ctx)
  b.i32 = int32TypeInContext(b.ctx)
  b.i64 = int64TypeInContext(b.ctx)
  b.ffloat = floatTypeInContext(b.ctx)
  b.fdouble = doubleTypeInContext(b.ctx)
  b.i32_1 = constInt(b.i32, 1)
  b.i32_0 = constInt(b.i32, 0)
  b.i1_0 = constInt(b.i1, 0)
  b.i1_1 = constInt(b.i1, 1)

proc initTarget2() =
  discard nimLLVMConfigureTarget(nil, nil, nil, nil, nil, nil, True)

proc listTargets*() =
  var e = newStringOfCap(10)
  newBackend()
  initTarget2()
  fstderr <<< "  Registered Targets:"
  var t = getFirstTarget()
  while t != nil:
    fstderr << "    "
    var n = getTargetName(t)
    var l = 15 - len(n)
    e.setLen 0
    while l > 0:
      e.add(' ')
      dec l
    fstderr << n
    fstderr << e
    fstderr << " - "
    fstderr <<< getTargetDescription(t)
    t = getNextTarget(t)
  cc_exit(1)

var cliOptions = [
  ("v".h, 0, cast[P](showVersion), "print version info"),
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
  ("no-opaque-pointers".h, 0, proc () = app.opaquePointerEnabled = false, "disable opaque pointer"),
  ("O0".h, 0, proc () = app.optLevel = 0, "no optimization(default)"),
  ("O1".h, 0, proc () = app.optLevel = 1, "Somewhere between -O0 and -O2"),
  ("O2".h, 0, proc () = app.optLevel = 2, "enables most optimizations"),
  ("O3".h, 0, proc () = app.optLevel = 3, "enables optimizations that take longer to perform or that may generate larger code(for example, loop unrolling)"),
  ("O4".h, 0, proc () = app.optLevel = 3, " = O3"),
  ("Os".h, 0, proc () = app.sizeLevel = 1, "reduce code size"),
  ("Oz".h, 0, proc () = app.sizeLevel = 2, "reduce code size further"),
  ("x".h, 1, cast[P](setInput), "set input langauge")
]

proc help() =
  var e = newStringOfCap(20)
  fstderr <<< "command line options"
  fstderr <<< "Option                         Description"
  for i in cliOptions:
    fstderr << '-'
    fstderr << i[0][0].str
    var l = 30 - len(i[0][0].str)
    e.setLen 0
    while l > 0:
      e.add(' ')
      dec l
    fstderr << e
    fstderr <<< i[3]
  fstderr << '\n'
  showVersion()

include "builtins.def"

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

proc addString*(s: string, filename: string) =
  t.fstack.add(newStringStream(s))
  t.filenamestack.add(t.filename)
  t.pathstack.add(t.path)
  t.locstack.add(Location(line: t.l.line, col: t.l.col))
  t.filename = filename
  t.path = filename
  t.l.line = 1
  t.l.col = 1


proc parseCLI*(): bool =
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
        name = i
      addFile(one)
  if t.fstack.len == 0:
    error("no input files")
    return false
  if app.output.len == 0:
    i = name - 1
    app.output = get()
  case app.mode:
  of OutputLink:
    app.output &= (when defined(windows): ".exe" else: ".out")
  of OutputLLVMAssembly:
    app.output &= ".ll"
  of OutputBitcode:
    app.output &= ".bc"
  of OutputObjectFile:
    app.output &= ".o"
  of OutputAssembly:
    app.output &= ".s"
  of OutputCheck:
    discard
  addString(builtin_predef, "built-in")
  return true

const
 FNone* = 0'u32
 FMinGW* = 1'u32
 F32Bit* = 2'u32
 F64Bit* = 4'u32

proc llvm_error*(msg: string) =
  fstderr << "LLVM ERROR: "
  fstderr <<< cstring(msg)

proc llvm_error*(msg: cstring) =
  if msg != nil:
    fstderr << "LLVM ERROR: "
    fstderr <<< msg

proc wrap*(ty: CType): Type

proc wrap2*(ty: CType): Type

proc llvmGetAlignof*(ty: CType): culonglong =
  aBIAlignmentOfType(b.layout, wrap2(ty))

proc llvmGetsizeof*(ty: CType): culonglong =
  storeSizeOfType(b.layout, wrap2(ty))

proc llvmGetOffsetof*(ty: CType, idx: int): culonglong =
  offsetOfElement(b.layout, wrap(ty), cuint(idx))

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
    return llvmGetSizeof(ty)

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
    llvmGetAlignOf(ty)

proc getAlignof*(e: Expr): culonglong =
    getAlignof(e.ty)


proc addLLVMModule*(source_file: string) =
  b.module = moduleCreateWithNameInContext(source_file.cstring, b.ctx)
  setSourceFileName(b.module, cstring(source_file), source_file.len.csize_t)
  setModuleDataLayout(b.module, b.layout)
  setTarget(b.module, app.triple.cstring)

  const idents: cstring = "cc: A C Compiler(https://ianfun.github.io/cc.git)"
  var ident = mDStringInContext(b.ctx, idents, len(idents))
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
  var wcharsize_arr = [b.i32_1, mDStringInContext(b.ctx, wcharsizes, len(wcharsizes)), constInt(b.i32, 4)]
  var wcharsizenode = mDNodeInContext(b.ctx, addr wcharsize_arr[0], 3)
  addNamedMetadataOperand(b.module, "llvm.module.flags", wcharsizenode)

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

proc gep(ty: Type, p: Value, indices: var Value): Value {.inline.} =
  buildInBoundsGEP2(b.builder, ty, p, addr indices, 1, "")

proc gep(ty: Type, p: ValueRef, indices: ptr ValueRef, num: cuint): Value {.inline.} =
  buildInBoundsGEP2(b.builder, ty, p, indices, num, "")

proc gep(ty: Type, p: ValueRef, indices: var openarray[Value]): Value {.inline.} =
  gep(ty, p, addr indices[0], indices.len.cuint)

proc initTarget*(all = false): bool =
  var err = nimLLVMConfigureTarget(app.triple.cstring, addr b.target, addr b.machine, addr b.layout, addr b.triple, addr b.f, Bool(all))
  if err != nil or b.triple == nil:
    llvm_error(err)
    return false
  app.pointersize = pointerSize(b.layout)
  contextSetOpaquePointers(b.ctx, if app.opaquePointerEnabled: True else: False)
  b.intPtr = intPtrTypeInContext(b.ctx, b.layout)
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
    var asmtype = functionType(b.voidty, nil, 0, False)
    var f = getInlineAsm(asmtype, cstring(s), len(s).csize_t, nil, 0, True, False, InlineAsmDialectATT, False)
    discard buildCall2(b.builder, asmtype, f, nil, 0, "")

proc enterScope() =
  b.tags.add(newTable[string, Type]())
  b.vars.add(newTable[string, Value]())

proc leaveScope() =
  ## not token.leaveBlock
  discard b.tags.pop()
  discard b.vars.pop()

proc getVar*(name: string): Value =
  for i in countdown(len(b.vars)-1, 0):
    result = b.vars[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc putVar*(name: string, val: Value) =
  b.vars[^1][name] = val

proc getTags*(name: string): Type =
  for i in countdown(len(b.tags)-1, 0):
    result = b.tags[i].getOrDefault(name, nil)
    if result != nil:
      return result

proc putTags*(name: string, t: Type) =
  b.tags[^1][name] = t

proc shutdownBackend*() =
  disposeBuilder(b.builder)
  shutdown()

proc optimize*() =
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
    return nil
  if parseBitcodeInContext(b.ctx, mem, addr result, cast[cstringArray](addr err)) == True:
    llvm_error(err)
  disposeMemoryBuffer(mem)

proc readIRToModule*(path: cstring): ModuleRef =
  var mem: MemoryBufferRef
  var err: cstring
  if createMemoryBufferWithContentsOfFile(path, addr mem, cast[cstringArray](addr err)) == True:
    llvm_error(err)
    return nil
  if parseIRInContext(b.ctx, mem, addr result, cast[cstringArray](addr err)) == True:
    llvm_error(err)
  disposeMemoryBuffer(mem)

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
        constInt(b.i8, i)
    elif (tags and (TYINT16 or TYUINT16)) != 0:
        constInt(b.i16, i)
    elif (tags and (TYINT32 or TYUINT32)) != 0:
        constInt(b.i32, i)
    elif (tags and (TYINT64 or TYUINT64)) != 0:
        constInt(b.i64, i)
    else:
        unreachable()
        nil

proc gen_float*(f: float, tag: uint32): Value =
  constReal(if (tag and TYFLOAT) != 0: b.ffloat else: b.fdouble, f)

proc gen_str*(val: string, ty: var Type): Value =
  var gstr = constStringInContext(b.ctx, cstring(val), len(val).cuint, False)
  ty = typeOfX(gstr)
  result = addGlobal(b.module, ty, ".str")
  # setGlobalConstant(result, True)
  # we use array instead of constant string!
  setLinkage(result, PrivateLinkage)
  setInitializer(result, gstr)
  setAlignment(result, 1)
  setUnnamedAddr(result, 1)

proc gen_str_ptr*(val: string): Value =
  var ty: Type
  var s = gen_str(val, ty)
  if app.opaquePointerEnabled:
    s
  else:
    var indices = [b.i32_0, b.i32_0]
    gep(ty, s, indices)

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
        var init = constInt(result, v.culonglong)
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
  constInt(wrap(ty), 1)

proc gen_condition*(test: Expr, lhs: Expr, rhs: Expr): Value =
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
  var right =gen (rhs)
  br ifend

  setInsertPoint(ifend)
  var phi = buildPhi(b.builder, ty, "")

  var blocks = [iftrue, iffalse]
  var values = [left, right]
  addIncoming(phi, addr values[0], addr blocks[0], 2)
  return phi

proc gen_logical*(lhs: Expr, rhs: Expr, isand = true): Value =
  var cond2 = addBlock()
  var phib = addBlock()
  var a = buildAlloca(b.builder, b.i1, "")
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

  return load(a, b.i1)

proc gen_if*(test: Expr, body: Stmt) =
  var iftrue = addBlock()
  var ifend = addBlock()
  condBr(gen_cond(test), iftrue, ifend)

  setInsertPoint(iftrue)
  gen(body)
  br ifend

  setInsertPoint(ifend)

proc gen_if*(test: Expr, body: Stmt, elsebody: Stmt) =
  var iftrue = addBlock()
  var iffalse = addBlock()
  var ifend = addBlock()
  condBr(gen_cond(test), iftrue, iffalse)

  setInsertPoint(iftrue)
  gen(body)
  br ifend

  setInsertPoint(iffalse)
  gen(elsebody)
  br ifend

  setInsertPoint(ifend)

proc gen_switch*(test: Expr, body: Stmt) =
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

proc gen_case*(test: Expr, body: Stmt) =
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

proc gen_default*(body: Stmt) =
  if b.topCase != nil:
    br b.topdefaultCase
  b.topCase = b.topdefaultCase
  setInsertPoint(b.topdefaultCase)
  gen(body)

proc gen_while*(test: Expr, body: Stmt) =
  var old_break = b.topBreak
  var old_continue = b.topContinue

  var whilecmp = addBlock()
  var whilebody = addBlock()
  var whileleave = addBlock()

  b.topBreak = whileleave
  b.topContinue = whilecmp

  br whilecmp

  setInsertPoint(whilecmp)
  var cond = gen_cond(test)
  condBr(cond, whilebody, whileleave)

  setInsertPoint(whilebody)
  gen(body)

  br whilecmp

  setInsertPoint(whileleave)

  b.topBreak = old_break
  b.topContinue = old_continue

proc gen_for*(test: Expr, body: Stmt, sforinit: Stmt, eforincl: Expr) =
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

  br forcmp

  # for.cmp
  setInsertPoint(forcmp)
  var cond = gen_cond(test)
  condBr(cond, forbody, forleave)

  # for.body
  setInsertPoint(forbody)
  gen(body)
  br forincl

  # for.incl
  setInsertPoint(forincl)
  if eforincl != nil:
    discard gen(eforincl)
  br forcmp
  setInsertPoint(forleave)
  leaveScope()

  b.topBreak = old_break
  b.topContinue = old_continue

proc gen_dowhile*(test: Expr, body: Stmt) =
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
  condBr(cond, dowhilebody, dowhileleave)

  setInsertPoint(dowhileleave)
  b.topBreak = old_break
  b.topContinue = old_continue

# the compiler may generate a table(array), so require `O(1)` time indexing
proc getOp*(a: BinOP): llvm.Opcode =
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

proc getICmpOp*(a: BinOP): llvm.IntPredicate =
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

proc getFCmpOp*(a: BinOP): RealPredicate =
  case a:
  of BinOP.FEQ: RealOEQ
  of BinOP.FNE: RealONE
  of BinOP.FGT: RealOGT
  of BinOP.FGE: RealOGE
  of BinOP.FLT: RealOLT
  of BinOP.FLE: RealOLE
  else: unreachable();cast[RealPredicate](0)

proc getCastOp*(a: CastOp): llvm.Opcode =
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
      var entry = addBlock("entry")
      setInsertPoint(entry)
      b.labels.add(newTable[string, Label]())
      for L in s.labels:
        var j = addBlock(cstring(L))
        b.labels[^1][L] = j
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
    discard wrap(s.decl)
  of SLabled:
    var ib = b.labels[^1].getOrDefault(s.label, nil)
    assert ib != nil
    if getBasicBlockTerminator(getInsertBlock(b.builder)) == nil:
      br ib
    setInsertPoint(ib)
    gen(s.labledstmt)
  of SGoto:
    let loc = b.labels[^1].getOrDefault(s.location, nil)
    assert loc != nil
    br loc
    setInsertPoint(loc)
  of SSemicolon:
    discard
  of SContinue:
    br b.topContinue
  of SBreak:
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
        discard newFunction(varty, name)
      else:
        if b.currentfunction == nil:
          var ty = wrap(varty)
          var ginit = if init == nil: constNull(ty) else: gen(init)
          var g = addGlobal(b.module, ty, cstring(name))
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
            if init == nil:
              setLinkage(g, CommonLinkage)
            if (varty.tags and TYREGISTER) != 0:
              warning("register variables is ignored in LLVM backend")
          # setLinkage(g, CommonLinkage)
          putVar(name, g)
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
            val = buildArrayAlloca(b.builder, ty, vla, "")
          else:
            val = buildAlloca(b.builder, ty, "")
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

proc gen_cast*(e: Expr, to: CType, op: CastOp): Value

proc neZero*(a: Expr): Expr =
  Expr(k: EBin, lhs: a, rhs: Expr(k: EBackend, p: cast[pointer](getZero(a.ty))), bop: NE)

proc eqZero*(a: Expr): Expr =
  Expr(k: EBin, lhs: a, rhs: Expr(k: EBackend, p: cast[pointer](getZero(a.ty))), bop: EQ)

proc incl*(p: Value, t: Type): Value =
  var l = load(p, t)
  var l2 = buildAdd(b.builder, l, constInt(t, 1), "")
  store(p, l2)
  return l

proc incl*(p: Value, t: Type, align: uint32): Value =
  var l = load(p, t, align)
  var l2 = buildAdd(b.builder, l, constInt(t, 1), "")
  store(p, l2, align)
  return l

proc decl*(p: Value, t: Type, align: uint32): Value =
  var l = load(p, t, align)
  var l2 = buildSub(b.builder, l, constInt(t, 1), "")
  store(p, l2, align)
  return l

proc decl*(p: Value, t: Type): Value =
  var l = load(p, t)
  var l2 = buildSub(b.builder, l, constInt(t, 1.culonglong), "")
  store(p, l2)
  return l

proc getStruct*(e: Expr): Value =
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

proc getArray*(e: Expr): Value =
    var l = len(e.arr)
    var elemTy = wrap(e.ty.arrtype)
    var inits = create(Value, e.ty.arrsize or 1)
    var arr = cast[ptr UncheckedArray[Value]](inits)
    for i in 0 ..< l:
      arr[i] = gen(e.arr[i])
    for i in l ..< e.ty.arrsize:
      arr[i] = constNull(elemTy)
    result = constArray(elemTy, inits, e.ty.arrsize.cuint)
    dealloc(inits)

proc getAddress*(e: Expr): Value =
  # return address
  case e.k:
  of EVar:
    getVar(e.sval)
  of EPointerMemberAccess, EMemberAccess:
    var basep = if e.k == EMemberAccess: getAddress(e.obj) else: gen(e.obj)
    var r = [b.i32_0, constInt(b.i32, e.idx.culonglong)]
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
    gen_str_ptr(e.str)
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
  of EVLAGetSize:
    var s = nimLLVMGetAllocaArraySize(gen(e.vla))
    var z = buildZExt(b.builder, s, b.i64, "")
    buildMul(b.builder, z, constInt(b.i64, getsizeof(e.vla.voidexpr.ty.arrtype)), "")
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
        var c = constInt(b.intPtr, s.culonglong)
        buildExactSDiv(b.builder, sub, c, "")
      else:
        sub
    of SAddP:
      var l = gen(e.lhs)
      var r = gen(e.rhs)
      gep( wrap(e.ty.p), l, r)
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
    discard gen(e.voidexpr)
    return nil
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
      assert basep != nil
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
      res
  of EStruct:
    getStruct(e)
  of EArray:
    getArray(e)

proc gen_cast*(e: Expr, to: CType, op: CastOp): Value =
  var c = gen(e)
  buildCast(b.builder, getCastOp(op), c, wrap(to), "")

proc jit_error*(msg: string) =
  fstderr << "LLVM JIT ERROR: " 
  fstderr <<< msg

proc jit_error*(msg: cstring) =
  fstderr << "LLVM JIT ERROR: "
  fstderr <<< msg

proc jit_error*(err: ErrorRef) =
  var msg = getErrorMessage(err)
  fstderr <<< msg
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

    var o = @[appFileName]
    o &= options
    var argslen = len(o)
    var mainargs = create(cstring, argslen or 1)
    var arr = cast[ptr UncheckedArray[cstring]](mainargs)

    for i in 0..<argslen:
      arr[i] = cstring(o[i])

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

iterator getDefines*(): (string, seq[TokenV]) =
  # (windows) gcc -dM -E - <NUL:
  # (bash) gcc -dM -E - </dev/null
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
  LBL_UNDEFINED* = 0'u8
  LBL_FORWARD* = 1'u8
  LBL_DECLARED* = 2'u8
  LBL_OK* = 4'u8

proc isTopLevel*(): bool =
    t.sema.typedefs.len == 1

const 
  INFO_USED* = 2

proc getPtrDiff_t*(): CType = get(if app.pointersize == 4: TYINT32 else: TYINT64)

proc getIntPtr_t*(): CType = getPtrDiff_t()

proc binop*(a: Expr, op: BinOP, b: Expr, ty: CType): Expr = 
    ## construct a binary operator
    Expr(k: EBin, lhs: a, rhs: b, bop: op, ty: ty)

proc unary*(e: Expr, op: UnaryOP, ty: CType): Expr = 
    ## construct a unary operator
    Expr(k: EUnary, uop: op, uoperand: e, ty: ty)

proc leaveBlock*()

proc enterBlock*() =
  t.sema.typedefs.add(newTable[string, Info]())
  t.sema.tags.add(newTable[string, Info]())

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
  t.sema.type_error or t.parse_error or t.pp.eval_error


proc inTheExpression*(e: Expr) =
    ## emit in the expression message
    note("in the expression '" & $e & '\'')

proc closeParser*() =
  for fd in t.fstack:
    fd.close()

proc getTag(name: string): Info =
  for i in countdown(len(t.sema.tags)-1, 0):
    result = t.sema.tags[i].getOrDefault(name, nil)
    if result != nil:
      result.tag = result.tag or INFO_USED
      return result

proc gettypedef*(name: string): Info =
  for i in countdown(len(t.sema.typedefs)-1, 0):
    result = t.sema.typedefs[i].getOrDefault(name, nil)
    if result != nil:
      result.tag = result.tag or INFO_USED
      return result

proc getstructdef*(name: string): CType =
    var r = getTag(name)
    if r == nil:
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYSTRUCT, name: name)
    elif r.ty.spec != TYSTRUCT:
        type_error(name & " is not a struct")
    else:
        result = r.ty

proc getLabel*(name: string) =
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

proc putLable*(name: string) =
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

proc putstructdef*(ty: CType) =
    let o = t.sema.tags[^1].getOrDefault(ty.sname)
    if o != nil:
        error("struct " & ty.sname & " aleady defined")
        note(ty.sname & "was defined at " & $o.loc)
    else:
        t.sema.tags[^1][ty.sname] = Info(ty: ty, loc: Location(line: t.l.line, col: t.l.col))

proc getenumdef*(name: string): CType =
    let r = getTag(name)
    if r == nil:
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYENUM, name: name)
    elif r.ty.spec != TYENUM:
        type_error(name & " is not a enum")
    else:
        result = r.ty

proc putenumdef*(ty: CType) =
    let o = t.sema.tags[^1].getOrDefault(ty.ename)
    if o != nil:
        error("enum " & ty.ename & " aleady defined")
        note(ty.ename & "was defined at " & $o.loc)
    else:
        t.sema.tags[^1][ty.ename] = Info(ty: ty, loc: Location(line: t.l.line, col: t.l.col))

proc getuniondef*(name: string): CType =
    let r = getTag(name)
    if r == nil:
        result = CType(tags: TYINVALID, spec: TYINCOMPLETE, tag: TYUNION, name: name)
    elif r.ty.spec != TYUNION:
        type_error(name & " is not a union")
    else:
        result = r.ty

proc putuniondef*(ty: CType) =
    let o = t.sema.tags[^1].getOrDefault(ty.sname)
    if o != nil:
        error("`union` " & ty.sname & " aleady defined")
        note(ty.sname & "was defined at " & $o.loc)
    else:
        t.sema.tags[^1][ty.sname] = Info(ty: ty, loc: Location(line: t.l.line, col: t.l.col))

proc getsymtype*(name: string): CType =
  let o = gettypedef(name)
  if o == nil:
    nil
  else:
    o.ty

# function definition
proc putsymtype3*(name: string, yt: CType) =
  let ty = t.sema.typedefs[^1].getOrDefault(name, nil)
  if ty != nil:
    type_error("function " & name & " redefined")
  t.sema.typedefs[^1][name] = Info(ty: yt, loc: Location(line: t.l.line, col: t.l.col))

# function declaration
proc putsymtype2*(name: string, yt: CType) =
  let ty = t.sema.typedefs[^1].getOrDefault(name, nil)
  if ty != nil:
    if not compatible(yt, ty.ty):
        type_error("conflicting types for function declaration '" & name &  "'")
  t.sema.typedefs[^1][name] = Info(ty: yt, loc: Location(line: t.l.line, col: t.l.col))

# typedef, variable
proc putsymtype*(name: string, yt: CType) =
  if yt.spec == TYFUNCTION:
    putsymtype2(name, yt)
    return
  let ty = t.sema.typedefs[^1].getOrDefault(name, nil)
  if ty != nil and not bool(ty.ty.tags and TYEXTERN):
    type_error(name & " redeclared")
    return
  t.sema.typedefs[^1][name] = Info(ty: yt, loc: Location(line: t.l.line, col: t.l.col))

proc istype(a: Token): bool =
    if a in (declaration_specifier_set + {Kstruct, Kenum, Kunion, K_Alignas}):
        return true
    if a == TIdentifier:
      let o = gettypedef(t.l.tok.s)
      return o != nil and (o.ty.tags and TYTYPEDEF) != 0
    return false

proc intcast*(e: Expr, to: CType): Expr = 
    if bool(to.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64)) and 
       bool(e.ty.tags and (TYINT8 or TYINT16 or TYINT32 or TYINT64 or 
        TYUINT8 or TYUINT16 or TYUINT32 or TYUINT64 or TYBOOL)):
        if to.tags == e.ty.tags:
            return Expr(k: ECast, castop: CastOp.BitCast, castval: e, ty: to)
        if intRank(to.tags) > intRank(e.ty.tags):
            if isSigned(to) and not bool(e.ty.tags and TYBOOL):
                return Expr(k: ECast, castop: CastOp.SExt, castval: e, ty: to)
            else:
                return Expr(k: ECast, castop: CastOp.ZExt, castval: e, ty: to)
        else:
            return Expr(k: ECast, castop: CastOp.Trunc, castval: e, ty: to)
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

proc boolToInt*(e: Expr): Expr =
    Expr(k: ECast, castop: CastOp.ZExt, castval: e, ty: getIntType())

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


proc struct_union*(tok: Token): CType

proc penum*(): CType

proc static_assert*(): Stmt

proc translation_unit*(): Stmt

proc statament*(): Stmt

proc compound_statement*(): Stmt

proc compound_statement*(params: seq[(string, CType)], lables: var HashSet[string]): Stmt

proc postfix*(e: Expr, op: PostfixOP, ty: CType): Expr = 
    ## construct a postfix operator
    Expr(k: EPostFix, pop: op, poperand: e, ty: ty)

proc consume*() =
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

proc read_enum_sepcs*(c: var CType, sepc: Token) = 
    # TODO: ...
    discard

proc read_struct_union_sepcs*(c: var CType, sepc: Token) = 
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

proc specifier_qualifier_list*(): CType =
    ## specfier_qualifier_list is used in struct declaration
    var s: seq[Token]
    while t.l.tok.tok in (type_specifier_set + type_qualifier_set + {Kstruct, Kenum, Kunion}):
        if t.l.tok.tok == Kenum:
            result = penum()
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

proc declarator*(base: CType; flags=Direct): Stmt =
    ## take a base type, return the final type and name
    ##
    ## for example: `static   int     *foo`
    ##
    ##                base-type    decorator
    var ty = base
    while t.l.tok.tok == TMul:
        consume()
        ty = getPointerType(ty)
        type_qualifier_list(ty)
    return direct_declarator(ty, flags)

proc initializer_list*(): Expr =
    if t.l.tok.tok != TLcurlyBracket:
        if t.sema.currentInitTy == nil:
            # when 'excess elements in initializer-list', 't.sema.currentInitTy' is nil
            return assignment_expression()
        if t.sema.currentInitTy.spec notin {TYPRIM, TYPOINTER}:
            type_error("expect bracket initializer")
            return nil
        var e = assignment_expression()
        if e == nil:
            expectExpression()
            return nil
        return castto(e, t.sema.currentInitTy)
    if t.sema.currentInitTy.spec == TYSTRUCT:
        result = Expr(k: EStruct, ty: t.sema.currentInitTy)
    else:
        # array, scalar
        result = Expr(k: EArray, ty: t.sema.currentInitTy)
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
    if t.sema.currentInitTy.spec == TYPRIM or t.sema.currentInitTy.spec == TYPOINTER:
        result = result.arr[0]

proc direct_declarator_end*(base: CType, name: string): Stmt

proc direct_declarator*(base: CType; flags=Direct): Stmt =
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

proc direct_declarator_end*(base: CType, name: string): Stmt =
    case t.l.tok.tok:
    of TLSquareBrackets: # int arr[5], int arr[*], int arr[static 5], int arr[static const 5], int arr[const static 5], int arr2[static const restrict 5]
        consume() # eat ]
        var ty = CType(tags: TYINVALID, spec: TYARRAY, arrsize: 0, arrtype: base, hassize: false)
        if t.l.tok.tok == TMul: # int arr[*]
          consume()
          if t.l.tok.tok != TRSquareBrackets:
            parse_error("expect ']'")
            note("the syntax is:\n\tint arr[*]")
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
            ty.arrsize = eval_const_expression(e)
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
            putsymtype3(name, ty)
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
            return Stmt(k: SFunction, funcname: name, functy: ty, funcbody: body, labels: labels)
        return direct_declarator_end(ty, name)
    else:
        return Stmt(k: SVarDecl1, var1name: name, var1type: base)

proc struct_declarator(base: CType): (string, CType) =
    if t.l.tok.tok == TColon:
        consume()
        let e = constant_expression()
        if e == nil:
            expectExpression()
            note("in bit field declaration")
            return ("", nil)
        let bitsize = eval_const_expression(e)
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
            let bitsize = eval_const_expression(e)
            return ("", CType(tags: TYINVALID, spec: TYBITFIELD, bittype: if d.k == SFunction: d.functy else: d.var1type, bitsize: bitsize))
        if d.k == SFunction:
            return (d.funcname, d.functy)
        return (d.var1name, d.var1type)

proc struct_union*(tok: Token): CType =
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
                        if t.l.tok.tok != TSemicolon:
                            expect("';'")
                            return nil
                        consume()
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
    var c: intmax_t = 0
    while true:
        if t.l.tok.tok != TIdentifier:
            break # enum {A, } is ok !
        var s = t.l.tok.s # copy
        consume()
        if t.l.tok.tok == TAssign:
            consume()
            var e = constant_expression()
            c = eval_const_expression(e)
            if err():
                return nil
        result.eelems.add((s, c))
        putsymtype(s, getIntType())
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

proc parameter_type_list*(): (bool, seq[(string, CType)]) =
    result = (true, default(typeof(result[1])))
    while true:
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

proc checkAlign(a: uint32) =
    if (a and (a - 1)) != 0:
        type_error("requested alignment is not a power of 2")

proc parse_alignas*(): bool =
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
        var a = eval_const_expression(e)
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

proc declaration_specifiers*(): CType =
    ## declaration_specfier is used in function and variable declaration/definition
    var s: seq[Token]
    var should_return = false
    while t.l.tok.tok in (declaration_specifier_set + {Kstruct, Kenum, Kunion, K_Alignas}):
        if t.l.tok.tok == Kenum:
            result = penum()
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
        for i in s:
            if i == Ktypedef:
                result.tags = result.tags or TYTYPEDEF
                continue
        return result
    if t.l.tok.tok == TIdentifier:
        let o = gettypedef(t.l.tok.s)
        if o != nil and (o.ty.tags and TYTYPEDEF) != 0:
            more(s)
            return handle_typedef(s, o.ty)
    if s.len > 0:
        return merge_types(s)
    warning("type defaults to 'int' in declaration")
    return getIntType()

proc parse_asm*(): Stmt =
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

proc static_assert*(): Stmt =
    consume()
    if t.l.tok.tok != TLbracket:
        expectLB()
        return nil
    consume()
    var msg = ""
    let e = constant_expression()
    let ok = eval_const_expression(e) != 0
    if t.l.tok.tok == TRbracket: # no message
        if ok == false:
            error("static assert failed!")
        consume()
        note("static assert with no message")
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
    if t.l.tok.tok != TSemicolon:
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
    if t.l.tok.tok == K_Static_assert:
        return static_assert()
    block:
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
            if t.l.tok.tok == TAssign:
                if st.var1type.spec == TYARRAY and st.var1type.vla != nil:
                    type_error("variable-sized object may not be initialized")
                    return nil
                consume()
                var old = t.sema.currentInitTy
                t.sema.currentInitTy = st.var1type
                let init = initializer_list()
                t.sema.currentInitTy = old
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
            if t.l.tok.tok == TComma:
                consume()
            elif t.l.tok.tok == TSemicolon:
                consume()
                break
        return result

proc cast_expression*(): Expr =
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
                return Expr(k: EVoid, voidexpr: e, ty: getVoidType())
            return castto(e, n)
        putToken()
        t.l.tok = TokenV(tok: TLbracket, tags: TVNormal)
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

proc getsizeof2(e: Expr): Expr =
    if e.k == ArrToAddress:
        if e.voidexpr.ty.vla != nil:
            Expr(ty: getSizetType(), k: EVLAGetSize, vla: e)
        else:
            Expr(ty: getSizetType(), k: EIntLit, ival: cast[int](getsizeof(e.voidexpr.ty)))
    else:
        Expr(ty: getSizetType(), k: EIntLit, ival: cast[int](getsizeof(e)))

proc unary_expression*(): Expr =
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
                return Expr(ty: getSizetType(), k: EIntLit, ival: cast[int](getsizeof(ty[0])))
            let e = unary_expression()
            if e == nil:
                expectExpression()
                return nil
            if t.l.tok.tok != TRbracket:
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
            result = Expr(ty: getSizetType(), k: EIntLit, ival: cast[int](getAlignof(ty[0])))
        else:
            let e = constant_expression()
            if e == nil:
                expectExpression()
                return nil
            result = Expr(ty: getSizetType(), k: EIntLit, ival: cast[int](getAlignof(e)))
        if t.l.tok.tok != TRbracket:
            expectRB()
            return nil
        consume()
        return result
    else:
        return postfix_expression()

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

proc fixName(v: string) =
    var fixList = initHeapQueue[FixName]()
    for i in countdown(len(t.sema.typedefs)-1, 0):
        for name in t.sema.typedefs[i].keys():
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
    case t.l.tok.tok:
    of TCharLit:
        var tags = 0'u32
        case t.l.tok.itag:
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
        result = Expr(k: EIntLit, ival: t.l.tok.i, ty: CType(tags: tags, spec: TYPRIM))
        consume()
    of TNumberLit:
        var tags = TYINT
        case t.l.tok.itag:
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
        result = Expr(k: EIntLit, ival: t.l.tok.i, ty: CType(tags: tags, spec: TYPRIM))
        consume()
    of TFloatLit:
        result = Expr(k: EFloatLit, fval: t.l.tok.f, ty: CType(tags: if t.l.tok.ftag == Ffloat: TYFLOAT else: TYDOUBLE, spec: TYPRIM))
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
        let ok = read_pp_number(t.l.tok.s, f, n)
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
        if t.pp.want_expr:
            result = Expr(k: EIntLit, ival: 0, ty: getIntType())
        elif t.l.tok.s == "__func__":
            result = Expr(k: EString, str: t.sema.pfunc, ty: CType(tags: TYCONST, spec: TYPOINTER, p: getCharType()))
        else:
            let ty = getsymtype(t.l.tok.s)
            if ty == nil:
                type_error("Variable not in scope: " & t.l.tok.s)
                if t.l.tok.s.len > 2:
                    fixName(t.l.tok.s)
                return nil
            case ty.spec:
            of TYFUNCTION:
                result = unary(
                    Expr(k: EVar, sval: t.l.tok.s, ty: ty), AddressOf, 
                    CType(tags: TYLVALUE, spec: TYPOINTER, p: ty)
                )
            of TYARRAY:
                var cp = deepCopy(ty)
                cp.arrtype.tags = cp.arrtype.tags and prim
                result = Expr(
                    k: ArrToAddress, voidexpr: Expr(k: EVar, sval: t.l.tok.s, ty: cp),
                    ty: CType(tags: TYLVALUE, spec: TYPOINTER, p: ty.arrtype)
                )
            else:
                result = Expr(k: EVar, sval: t.l.tok.s, ty: ty)
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

proc postfix_expression*(): Expr =
    let e = primary_expression()
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
                    return Expr(k: EPointerMemberAccess, obj: e, idx: i, ty: ty)
                return Expr(k: EMemberAccess, obj: e, idx: i, ty: ty)
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
        if t.l.tok.tok != TRSquareBrackets:
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

proc additive_expression*(): Expr =
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

proc shift_expression*(): Expr =
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

proc relational_expression*(): Expr =
    result = shift_expression()
    while true:
        case t.l.tok.tok:
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
        case t.l.tok.tok:
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
        case t.l.tok.tok:
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
        case t.l.tok.tok:
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
        case t.l.tok.tok:
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
        case t.l.tok.tok:
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
        case t.l.tok.tok:
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
        if t.l.tok.tok == TComma:
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
    return Expr(k: ECondition, cond: start, cleft: lhs, cright: rhs, ty: lhs.ty)

proc conditional_expression*(): Expr =
    let e = logical_OR_expression()
    if e == nil:
        return nil
    if t.l.tok.tok == TQuestionMark:
      return conditional_expression(e)
    return e

proc assignment_expression*(): Expr =
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

proc translation_unit*(): Stmt =
    ## parse top-level declaration until EOF reaches, the entry point of program
    ##
    ## never return nil, return a compound statement
    result = Stmt(k: SCompound)
    var s: Stmt
    while t.l.tok.tok != TEOF:
        if t.l.tok.tok == KAsm:
            s = parse_asm()
            if t.l.tok.tok != TSemicolon:
               expect("';'")
               s = nil
            consume()
        else:
            s = declaration()
        if s == nil:
            break
        result.stmts.add(s)

proc leaveBlock*() =
    if isTopLevel() == false:
      for (name, i) in t.sema.typedefs[^1].pairs():
        if not bool(i.tag and INFO_USED):
            warning("declared but not used: '" & name & '\'')
            note("declared at " & $i.loc)
    discard t.sema.typedefs.pop()
    discard t.sema.tags.pop()

proc runParser*(): Stmt =
    ## eat first token and parse a translation_unit
    ##
    ## never return nil, return a compound statement
    consume()
    enterBlock()
    result = translation_unit()
    leaveBlock()

proc compound_statement*(): Stmt =
    ## parse mant statements
    result = Stmt(k: SCompound)
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

proc compound_statement*(params: seq[(string, CType)], lables: var HashSet[string]): Stmt =
    result = Stmt(k: SCompound)
    consume()
    enterBlock()
    t.sema.lables.add(newTable[string, uint8]())
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
            warning("un-used label: '" & name & '\'')
        else:
            assert t == LBL_OK
            lables.incl name
    discard t.sema.lables.pop()
    leaveBlock()
    consume()
    return result

proc statament*(): Stmt =
    ## parse a statement
    if t.l.tok.tok == KAsm:
        result = parse_asm()
        if t.l.tok.tok != TSemicolon:
           expect("';'")
           return nil
        consume()
        return result
    if t.l.tok.tok == TSemicolon:
        consume()
        return Stmt(k: SSemicolon)
    elif t.l.tok.tok == TLcurlyBracket:
        return compound_statement()
    elif t.l.tok.tok == Kcase:
        consume()
        var e = constant_expression()
        if e == nil:
            expect("constant-expression")
            return nil
        if t.l.tok.tok != TColon:
            parse_error("':' expected")
            return nil
        consume()
        let s = statament()
        if s == nil:
            expectStatement()
            return nil
        return Stmt(k: Scase, case_expr: castto(e, t.sema.currentCase), case_stmt: s)
    elif t.l.tok.tok == Kdefault:
        consume()
        if t.l.tok.tok != TColon:
            parse_error("':' expected")
            return nil
        consume()
        let s = statament()
        if s == nil:
            return nil
        return Stmt(k: SDefault, default_stmt: s)
    elif t.l.tok.tok == Kgoto:
        consume()
        if t.l.tok.tok != TIdentifier:
            expect("identifier")
            note("the syntax is:\n\tgoto label;")
            return nil
        var location = t.l.tok.s
        consume()
        if t.l.tok.tok != TSemicolon:
            parse_error("expect ';'")
            return nil
        consume()
        getLabel(location)
        return Stmt(k: SGoto, location: location)
    elif t.l.tok.tok == Kcontinue:
        consume()
        if t.l.tok.tok != TSemicolon:
            parse_error("expect ';'")
            return nil
        consume()
        return Stmt(k: SContinue)
    elif t.l.tok.tok == Kbreak:
        consume()
        if t.l.tok.tok != TSemicolon:
            expect("';'")
            return nil
        consume()
        return Stmt(k: SBreak)
    elif t.l.tok.tok == Kreturn:
        consume()
        if t.l.tok.tok == TSemicolon:
            consume()
            if bool(t.sema.currentfunctionRet.tags and TYVOID):
                return Stmt(k: SReturn, exprbody: nil)
            warning("use default value in 'return' statement")
            note("function should return a value, but no value provided in 'return'")
            note("A return statement without an expression shall only appear in a function whose return type is void")
            return Stmt(k: SReturn, exprbody: Expr(k: EDefault, ty: t.sema.currentfunctionRet))
        let e = expression()
        if e == nil:
            expectExpression()
            return nil
        if t.l.tok.tok != TSemicolon:
            expect("';'")
            return nil
        consume()
        if bool(t.sema.currentfunctionRet.tags and TYVOID):
            warning("the value of 'return' statement is ignored")
            warning("'return' a value in function return void")
            note("A return statement with an expression shall not appear in a function whose return type is void")
            return Stmt(k: SReturn, exprbody: nil)
        if not compatible(e.ty, t.sema.currentfunctionRet) and e.ty.spec != TYPRIM:
            # if it is cast from int to long, int to double, ...etc, we do not emit a warning
            warning("incompatible type in 'return' statement")
            note("expect " & $t.sema.currentfunctionRet & ", but got " & $e.ty)
            inTheExpression(e)
        return Stmt(k: SReturn, exprbody: castto(e, t.sema.currentfunctionRet))
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
            type_error("expect scalar")
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
        return Stmt(k: SIf, iftest: e, ifbody: s, elsebody: elsebody)
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
            return Stmt(k: SSwitch, test: e, body: s)
        let s = statament()
        if s == nil:
            expectStatement()
            return nil
        return Stmt(k: SWhile, test: e, body: s)
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
                init = Stmt(k: SExpr, exprbody: ex)
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
        return Stmt(k: SFor, forinit: init, forcond: cond, forbody: s, forincl: forincl)
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
        if t.l.tok.tok != TSemicolon:
            expect("';'")
            return nil
        consume()
        return Stmt(k: SDoWhile, test: e, body: s)
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
            return Stmt(k: SLabled, label: val, labledstmt: s)
        else: # expression
            putToken()
            t.l.tok = TokenV(tok: TIdentifier, tags: TVSVal, s: val)
    let e = expression()
    if e == nil:
        expectExpression()
        return nil
    if t.l.tok.tok != TSemicolon:
        expect("';'")
        return nil
    consume()
    return Stmt(k: SExpr, exprbody: e)


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

proc c() =
    let translation_unit = runParser()
    if err():
        stderr.writeLine("compilation terminated.")
        set_exit_code(1)
        return
    if app.mode != OutputCheck:
        gen(translation_unit)
        nimLLVMOptModule(b.module)
        optimize()
        if app.runJit:
            runJit()
            if app.mode != OutputLink:
                stderr.write("cc: warning: jit cannot combine with other output flags\njit will not write output\n")
        else:
            verify()
            output()

proc brainfuck(i: Stream) =
    var s = "int getchar(void);int putchar(int);int main(){char*ptr=malloc(1024);"
    while true:
        var x = readChar(i)
        if x == '\0':
            break
        s.add(
            case x:
            of '+': "++ptr;"
            of '-': "--ptr;"
            of '<': "++*ptr"
            of '>': "--*ptr"
            of '.': "putchar(*ptr);"
            of ',': "*ptr = getchar();"
            of '[': "while(*ptr){"
            of ']': "}"
            else: ""
        )
    s.add("free(ptr);}")
    addString(s, "<brainfuck>")

proc bf() =
    for s in t.fstack:
        brainfuck(s)
        close(s)
    c()

set_exit_code(1)
if parseCLI():
    newBackend()
    if initTarget():
        for (name, v) in getDefines():
            t.pp.macros[name] = PPMacro(tokens: v, flags: MOBJ)
        addLLVMModule(t.pathstack[1])
        case app.input:
        of InputC:
            c()
        of InputBF:
            bf()
        of InputIR, InputBC:
            var reader = if app.input == InputBC: readBitcodeToModule else: readIRToModule
            b.module = reader(t.pathstack[1].cstring)
            if b.module != nil:
                var i = 2
                while true:
                    if i == len(t.pathstack):
                        optimize()
                        output()
                        break
                    var n = reader(t.pathstack[i].cstring)
                    if n == nil:
                        break
                    if link(b.module, n):
                        break
                    inc i
        of InputObject, InputAsm:
            warning("use gcc instead for object and assembly file")

        closeParser()
        shutdown_backend()
        set_exit_code(0)

