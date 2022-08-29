import core, parser, stream
import std/[unicode, math]

proc nextTok*()

proc setLexer*() =
    app.lex = nextTok

proc isalnum*(c: char): bool =
    return c in {'A'..'Z', 'a'..'z', '0'..'9'}

proc resetLine() =
    p.col = 1
    inc p.line

proc fs_read*() =
    if p.fstack.len == 0:
        p.c = '\0'
    else:
        let s = p.fstack[^1]
        p.c = s.readChar()
        inc p.col
        if p.c == '\0':
            let fd = p.fstack.pop()
            p.filename = p.filenamestack.pop()
            p.path = p.pathstack.pop()
            let loc = p.locstack.pop()
            p.line = loc.line
            p.col = loc.col
            fd.close()
            fs_read() # tail call
        if p.c == '\n':
          resetLine()
        elif p.c == '\r':
          resetLine()
          p.c = s.readChar()
          if p.c != '\n':
            putc(s, cint(p.c))

proc stdin_hook*() =
  stdout.write(">>> ")

proc lowleveleat() =
    fs_read()

proc do_eat*() =
    lowleveleat()
    if p.c == '/':
        lowleveleat()
        if p.c == '*':
            while true:
                lowleveleat()
                if p.c == '*':
                    lowleveleat()
                    if p.c == '/':
                        p.c = ' ' # make a space: GNU CPP do it
                        break
                elif p.c == '\0':
                    parse_error("expect '*/' before EOF")
                    return
        elif p.c == '/':
            while true:
                lowleveleat()
                if p.c == '\n' or p.c == '\0':
                    break
        else:
            p.lastc = uint16(p.c)
            p.c = '/'
    elif p.c == '\\':
        let c = p.c
        lowleveleat()
        if p.c == '\n':
            do_eat()
        else:
            p.lastc = uint16(p.c)
            p.c = c

proc eat*() =
    ## skip any C/C++ style comments
    ## https://gcc.gnu.org/onlinedocs/gcc-12.1.0/cpp/Initial-processing.html
    if p.lastc <= 255:
        p.c = char(p.lastc)
        p.lastc = 256
        return
    do_eat()

proc make_tok(op: Token) {.inline.} =
    p.tok = TokenV(tok: op, tags: TVNormal)

proc make_ch_lit(ch: char) {.inline.} =
    p.tok.i = ch.int

proc readHexChar(): Codepoint =
    var n = Codepoint(0)
    while true:
        if p.c in {'0'..'9'}:
            n = (n shl 4.Codepoint) or (Codepoint(p.c) - '0'.Codepoint)
        elif p.c in {'a'..'f'}:
            n = (n shl 4.Codepoint) or (Codepoint(p.c) - 'a'.Codepoint + 10.Codepoint)
        elif p.c in {'A'..'F'}:
            n = (n shl 4.Codepoint) or (Codepoint(p.c) - 'A'.Codepoint + 10.Codepoint)
        else:
            return n
        eat()

proc readFloatSuffix() =
    if p.c == 'F' or p.c == 'f':
        p.tok.ftag = Ffloat

proc readSuffix() =
    if p.c == 'U' or p.c == 'u':
        case p.tok.itag:
        of Iint:
            p.tok.itag = Iuint
        of Ilong:
            p.tok.itag = Iulong
        of Ilonglong:
            p.tok.itag = Iulonglong
        else:
            parse_error("double 'u' suffix in integer constant")
            return
    else:
        case p.tok.itag:
        of Iint:
            p.tok.itag = Ilong
        of Iuint:
            p.tok.itag = Iulong
        of Ilong:
            p.tok.itag = Ilonglong
        of Iulong:
            p.tok.itag = Iulonglong
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
        if p.c in {'0'..'9'}:
            n = (n shl 4) or (Codepoint(p.c) - '0'.Codepoint)
        elif p.c in {'a'..'f'}:
            n = (n shl 4) or (Codepoint(p.c) - 'a'.Codepoint + 10'u32)
        elif p.c in {'A'..'F'}:
            n = (n shl 4) or (Codepoint(p.c) - 'A'.Codepoint + 10'u32)
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
    case p.c:
    of 'a':
        eat()
        return '\a'.Codepoint
    of 'b':
        eat()
        return '\b'.Codepoint
    of 'f':
        eat()
        return '\f'.Codepoint
    of 'n':
        eat()
        return '\n'.Codepoint
    of 'r':
        eat()
        return '\r'.Codepoint
    of 't':
        eat()
        return '\t'.Codepoint
    of 'v':
        eat()
        return '\v'.Codepoint
    of 'e':
        eat()
        return '\x1b'.Codepoint
    of 'x':
        eat()
        readHexChar()
    of 'u', 'U':
        let n = if p.c == 'U': 8 else: 4
        eat()
        return readUChar(n)
    of '0' .. '7':
        const octalChs = {'0'..'7'}
        var n = Codepoint(p.c) - '0'.Codepoint
        eat() # eat first
        if p.c in octalChs:
            n = (n shl 3.Codepoint) or (Codepoint(p.c) - '0'.Codepoint)
            eat() # eat second
            if p.c in octalChs:
                n = (n shl 3.Codepoint) or (Codepoint(p.c) - '0'.Codepoint)
                eat() # eat third
        return n
    else:
        let c = p.c.Codepoint
        eat()
        return c

proc readCharLit(tag: ITag = Iint) =
    p.tok = TokenV(tok: TCharLit, tags: TVIVal, itag: tag)
    eat() # eat '
    if p.c == '\\':
        let codepoint = readEscape()
        if codepoint > 0xFF:
            warning("character constant too large")
        make_ch_lit(char(codepoint and 0xFF))
    else:
        make_ch_lit(p.c)
        eat()
    if p.c != '\'':
        parse_error("expect ' , got " & show(p.c))
    eat()

proc readIdentLit() =
    while true:
      if p.c == '\\':
        eat()
        if p.c == 'U':
            eat()
            let codepoint = readUChar(8)
            validUCNName(codepoint)
            p.tok.s.add(Rune(codepoint).toUTF8)
        elif p.c == 'u':
            eat()
            let codepoint = readUChar(4)
            validUCNName(codepoint)
            p.tok.s.add(Rune(codepoint).toUTF8)
        else:
            parse_error("invalid escape in identifier")
            return
      else:
        if not (isalnum(p.c) or (uint8(p.c) and 0x80'u8) != 0 or p.c == '$' or p.c == '_'):
            break
        p.tok.s.add(p.c)
        eat()
    if p.flags == PFNormal:
      let k = isKeyword(p.tok.s)
      if k != TNul:
          make_tok(k)

proc readHexFloatLit(intPart: float = 0.0) =
    # read a float start from '.'
    p.tok = TokenV(tok: TFloatLit, tags: TVFVal, ftag: Fdobule)
    p.tok.f = intPart
    var e = 1.0
    while true:
        e /= 16.0
        eat()
        if p.c in {'0'..'9'}:
          p.tok.f += float(int(p.c) - '0'.int) * e
        elif p.c in {'a'..'f'}:
          p.tok.f += float(int(p.c) - 'a'.int + 10) * e
        elif p.c in {'A'..'F'}:
          p.tok.f += float(int(p.c) - 'A'.int + 10) * e
        else:
            if p.c in {'L', 'l', 'F', 'f'}:
                readFloatSuffix()
                eat()
                break
            if p.c != 'P' and p.c != 'p':
                parse_error("expect p or P in hex floating constant, got " & show(p.c))
                break
            eat()
            var negate = false
            if p.c == '-':
                negate = true
                eat()
            elif p.c == '+':
                eat()
            elif p.c notin {'0'..'9'}:
                parse_error("expect exponent digits")
            var exps: string
            while true:
                exps.add(p.c)
                eat()
                if p.c notin {'0'..'9'}:
                    break
            p.tok.f *= pow(2, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
            if p.c in {'L', 'l', 'F', 'f'}:
                readFloatSuffix()
                eat()
            break

# read a float start from '.'
proc readFloatLit(intPart: float = 0.0) =
    p.tok = TokenV(tok: TFloatLit, tags: TVFval, ftag: Fdobule)
    p.tok.f = intPart
    var e = 0
    while true:
        inc e
        if p.c notin {'0'..'9'}:
            if p.c in {'L', 'l', 'F', 'f'}:
                readFloatSuffix()
                eat()
                break
            if p.c != 'E' and p.c != 'e':
                break
            eat()
            var negate = false
            if p.c == '-':
                negate = true
                eat()
            elif p.c == '+':
                eat()
            elif p.c notin {'0'..'9'}:
                parse_error("expect exponent digits")
                return
            var exps: string
            while true:
                exps.add(p.c)
                eat()
                if p.c notin {'0'..'9'}:
                    break
            p.tok.f *= pow(10, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
            if p.c in {'L', 'l', 'F', 'f'}:
                readFloatSuffix()
                eat()
            break
        p.tok.f += float(int(p.c) - '0'.int) / pow(10.0, float(e))
        eat()

proc readNumberLit() =
    p.tok = TokenV(tok: TNumberLit, tags: TVIVal, itag: Iint)
    p.tok.i = 0
    if p.c == '0':
        eat()
        case p.c:
        of '0'..'7': # octal
          discard
        of 'x', 'X': # hex
          eat()
          if p.c notin {'0'..'9', 'A'..'F', 'a'..'f'}:
            parse_error("invalid hex literal: " & show(p.c))
            return
          while true:
            if p.c in {'0'..'9'}:
              p.tok.i = (p.tok.i shl 4) or (int(p.c) - '0'.int)
            elif p.c in {'a'..'f'}:
              p.tok.i = (p.tok.i shl 4) or (int(p.c) - 'a'.int + 10)
            elif p.c in {'A'..'F'}:
              p.tok.i = (p.tok.i shl 4) or (int(p.c) - 'A'.int + 10)
            elif p.c in {'L', 'l', 'U', 'u'}:
              while p.c in {'L', 'l', 'U', 'u'}:
                eat()
              return
            elif p.c == '.':
                readHexFloatLit(float(p.tok.i))
                return
            else:
              return
            eat()
        of 'b', 'B': # binary
          eat()
          if p.c != '0' and p.c != '1':
            parse_error("invalid binary literal: expect 0 or 1, got " & show(p.c))
            return
          while true:
            case p.c:
            of '0':
              p.tok.i = p.tok.i shl 1
            of '1':
              p.tok.i = (p.tok.i shl 1) or 1
            else:
              if p.c in {'0'..'9'}:
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
        p.tok.i = (10 * p.tok.i) + (int(p.c) - '0'.int)
        eat()
        if p.c == '.':
            eat()
            readFloatLit(float(p.tok.i))
            break
        if p.c in {'L', 'l', 'U', 'u'}:
            while p.c in {'L', 'l', 'U', 'u'}:
                readSuffix()
                eat()
            break
        elif p.c notin {'0'..'9'}:
            if p.c in {'a'..'z', 'A'..'Z'}: # User-defined literals?
              warning("invalid decimal suffix " & show(p.c))
              note("user-defined literals is a C++ feature")
            break

proc readStringLit(enc: uint8) =
    p.tok = TokenV(tok: TStringLit, tags: TVStr)
    while true:
        if p.c == '\\':
            p.tok.str.add(Rune(readEscape()).toUTF8)
        elif p.c == '"':
            eat()
            break
        else:
            if 0xF0 == (0xF8 and Codepoint(p.c)): # 4 byte
              p.tok.str.add(p.c)
              eat()
              p.tok.str.add(p.c)
              eat()
              p.tok.str.add(p.c)
              eat()
              p.tok.str.add(p.c)
            elif 0xE0 == (0xF0 and Codepoint(p.c)): # 3 byte
              p.tok.str.add(p.c)
              eat()
              p.tok.str.add(p.c)
              eat()
              p.tok.str.add(p.c)
            elif 0xC0 == (0xE0 and Codepoint(p.c)): # 2 byte
              p.tok.str.add(p.c)
              eat()
              p.tok.str.add(p.c)
            else: # 1 byte
              if p.c == '\n':
                warning("missing terminating '\"' character, read newline as \\n")
              elif p.c == '\0':
                parse_error("unexpected EOF")
                return
              p.tok.str.add(p.c)
            eat()
    p.tok.enc = enc

proc readPPNumberAfterDot() =
    p.tok.s.add('.')
    while p.c in {'0'..'9'}:
        p.tok.s.add(p.c)
        eat()

proc readPPNumber() =
    p.tok = TokenV(tok: TPPNumber, tags: TVSVal)
    if p.c == '0':
        eat()
        case p.c:
        of 'x', 'X':
            p.tok.s = "0x"
            while true:
                eat()
                if p.c notin {'0'..'9', 'a'..'f', 'A'..'F'}:
                    break
                p.tok.s.add(p.c)
        of 'B', 'b':
            p.tok.s = "0b"
            while true:
                eat()
                if p.c notin {'0', '1'}:
                    break
                p.tok.s.add(p.c)
        of '0' .. '7':
            p.tok.s = "0"
            p.tok.s.add(p.c)
            while true:
                eat()
                if p.c notin {'0'..'7'}:
                    break
                p.tok.s.add(p.c)
        of '.':
            eat()
            readPPNumberAfterDot()
        else:
            p.tok.s = "0"
            return
    else:
        while true:
            p.tok.s.add(p.c)
            eat()
            if p.c notin {'0'..'9'}:
                break
        if p.c == '.':
            eat()
            readPPNumberAfterDot()
    if p.c in {'e', 'p', 'E', 'P'}:
        p.tok.s.add(p.c)
        eat()
        if p.c == '+' or p.c == '-':
            p.tok.s.add(p.c)
            eat()
        while p.c in {'0'..'9'}:
            p.tok.s.add(p.c)
            eat()
    # nodigit
    elif p.c in {'a'..'z', 'A'..'Z', '_'}:
        while true:
            p.tok.s.add(p.c)
            eat()
            if p.c notin {'a'..'z', 'A'..'Z', '_'}:
                break
    # universal-character-name
    elif p.c == '\\':
        eat()
        let n = if p.c == 'U': 8 else: 4
        eat()
        let c = readUChar(n)
        p.tok.s.add(Rune(c).toUTF8)

proc skipLine() =
    while p.c != '\n' and p.c != '\0':
        eat()
    p.flags = PFNormal

proc nextTok*() =
    ## Tokenize
    while true:
        if p.c in CSkip:
            while true:
                eat()
                if p.c notin CSkip:
                    break
            if p.flags == PFPP and p.want_expr == false:
                make_tok(TSpace)
                return
            continue
        if p.c == '#':
            if p.flags == PFPP:
                eat()
                if p.c == '#':
                    make_tok(PPSharpSharp)
                    eat()
                else:
                    make_tok(PPSharp) 
                return
            eat()
            p.flags = PFPP
            nextTok() # directive
            if p.tok.tok != CPPIdent:
                p.flags = PFNormal
                parse_error("invalid preprocessing directive: expect an identifier")
                return
            case p.tok.s:
            of "define":
                nextTok() # name
                while p.tok.tok == TSpace:
                    nextTok()
                if p.tok.tok != CPPIdent:
                    p.flags = PFNormal
                    parse_error("macro name should be a identifier, got " & showToken())
                    note("the syntax is:\n\t#define identifier replacement-list\n\t#define identifier(identifier-list) replacement-list")
                    return
                var name = p.tok.s # copy to string
                nextTok()
                var m = if p.tok.tok == TLbracket: PPMacro(flags: MFUNC, ivarargs: false) else: PPMacro(flags: MOBJ)
                if m.flags == MFUNC:
                    while true:
                        nextTok()
                        if p.tok.tok == CPPIdent:
                            m.params.add(p.tok.s)
                            nextTok()
                            if p.tok.tok == TRbracket:
                                break
                            elif p.tok.tok == TComma:
                                continue
                            else:
                                parse_error("')' or ',' expected")
                                p.flags = PFNormal
                                return
                        elif p.tok.tok == TEllipsis:
                            nextTok()
                            if p.tok.tok != TEllipsis:
                                parse_error("'.' expected")
                                p.flags = PFNormal
                                return
                            nextTok()
                            if p.tok.tok != TEllipsis:
                                parse_error("'.' expected")
                                p.flags = PFNormal
                                return
                            m.ivarargs = true
                            nextTok()
                            if p.tok.tok != TRbracket:
                                parse_error("')' expected")
                                p.flags = PFNormal
                                return
                            break
                        elif p.tok.tok == CPPIdent and p.tok.s == "__VA_ARGS__":
                            m.ivarargs = true
                            nextTok()
                            if p.tok.tok != TRbracket:
                                parse_error("')' expected")
                                p.flags = PFNormal
                                return
                            break
                        elif p.tok.tok == TRbracket:
                            break
                    nextTok()
                while p.tok.tok == TSpace:
                    nextTok()
                while true:
                    if p.flags != PFPP:
                        break
                    m.tokens.add(p.tok)
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
                while p.tok.tok == TSpace:
                    nextTok()
                var ok: bool
                p.want_expr = true
                let e = constant_expression()
                p.want_expr = false
                if e == nil:
                    parse_error("expect constant_expression")
                    ok = false
                else:
                    ok = app.eval_const_expression(e) != 0
                p.ppstack.add(if ok: 1 else: 0)
                p.ok = ok
                skipLine()
            of "ifdef", "ifndef":
                let ndef = p.tok.s == "ifndef"
                nextTok()
                while p.tok.tok == TSpace:
                    nextTok()
                if p.tok.tok != CPPIdent:
                    p.flags = PFNormal
                    parse_error("expect identifier")
                    note("the syntax is:\n\t#ifdef identifier\n\t#ifndef identifier")
                    return
                let name = p.tok.s # no copy
                let v = if ndef: not macro_defined(name) else: macro_defined(name)
                p.ppstack.add(if v: 1 else: 0)
                p.ok = v
                skipLine()
            of "else":
                if p.ppstack.len == 0:
                    parse_error("no matching #if")
                    return
                if (p.ppstack[^1] and 2) != 0:
                    parse_error("#else after #else")
                    return
                p.ppstack[^1] = p.ppstack[^1] or 2
                p.ok = not p.ok
                skipLine()
            of "elif":
                nextTok() # elif
                while p.tok.tok == TSpace:
                    nextTok()
                if p.ppstack.len == 0:
                    parse_error("no matching #if")
                    return
                if (p.ppstack[^1] and 2) != 0:
                    parse_error("#elif after #else")
                    return
                if p.ok == false:
                    var ok: bool
                    p.want_expr = true
                    let e = constant_expression()
                    p.want_expr = false
                    if e == nil:
                        parse_error("expect constant_expression")
                        ok = false
                    else:
                        ok = app.eval_const_expression(e) != 0
                    p.ok = ok
                else:
                    p.ok = false
                skipLine()
            of "endif":
                if p.ppstack.len == 0:
                    parse_error("no matching #if")
                    skipLine()
                    return
                discard p.ppstack.pop() # p.ppstack.del(p.ppstack.len - 1)
                if p.ppstack.len == 0:
                    p.ok = true
                else:
                    p.ok = (p.ppstack[^1] or 1) != 0
                skipLine()
            of "include":
                while p.c in CSkip:
                    eat()
                var path: string
                case p.c:
                of '"':
                    while true:
                        eat()
                        if p.c == '"':
                            eat()
                            break
                        if p.c == '\0' or p.c == '\n':
                            p.flags = PFNormal
                            parse_error("unexpected EOF, expect path or '\"'")
                            return
                        path.add(p.c)
                    let r = addInclude(path)
                    if not r:
                        parse_error("include file not found: " & path)
                of '<':
                    while true:
                        eat()
                        if p.c == '>':
                            eat()
                            break
                        if p.c == '\0' or p.c == '\n':
                            p.flags = PFNormal
                            parse_error("unexpected EOF, expect path or '>'")
                            return
                        path.add(p.c)
                    let r = addInclude(path)
                    if not r:
                        parse_error("include file not found: " & path)
                else:
                    parse_error("expect \"FILENAME\" or <FILENAME>")
                    note("the syntax is:\n\t#include <path>\n\t#include \"path\"")
                    return
                skipLine()
            of "line":
                while p.c in CSkip:
                    eat()
                if p.c notin {'0'..'9'}:
                    parse_error("expect digits (positive line number)")
                var s: string
                while true:
                    s.add(p.c)
                    eat()
                    if p.c in {'0'..'9'}:
                        continue
                    break
                p.line = decimaltoInt(s)
                while p.c in CSkip:
                    eat()
                if p.c == '\0' or p.c == '\n':
                    discard
                else:
                    if p.c != '"':
                        parse_error("expect \"FILENAME\"")
                        return
                    var f: string
                    while true:
                        eat()
                        if p.c == '\n' or p.c == '"' or p.c == '\\' or p.c == '\0':
                            break
                        f.add(p.c)
                    if p.c != '"':
                        parse_error("'\"' expected")
                        return
                    eat()
                    p.filename = f
                    skipLine()
            of "undef":
                nextTok()
                while p.tok.tok == TSpace:
                    nextTok()
                if p.tok.tok != CPPIdent:
                    p.flags = PFNormal
                    parse_error("macro name should be a identifier")
                    note("the syntax is: #undef identifier")
                    return
                macro_undef(p.tok.s)
                skipLine()
            of "pragma":
                nextTok() # eat pragma
                var pragmas: seq[TokenV]
                while true:
                    if p.flags != PFPP:
                        break
                    if p.tok.tok != TSpace:
                        pragmas.add(p.tok)
                    nextTok()
                app.pragma(pragmas)
                skipLine()
            of "error", "warning":
                var s: string
                let iswarning = p.tok.s == "warning"
                while p.c == ' ':
                    eat()
                while true:
                    if p.c == '\n' or p.c == '\0':
                        break
                    s.add(p.c)
                    eat()
                if iswarning:
                    warning("#warning: " & s)
                else:
                    parse_error("#error: " & s)
                skipLine()
            else:
                parse_error("invalid directive: " & p.tok.s)
                skipLine()
            eat()
            continue
        if p.flags == PFNormal and p.ok == false:
            while p.c != '\n' and p.c != '\0':
                eat()
            if p.c == '\n':
                eat()
            elif p.c == '\0':
                p.flags = PFNormal
                p.tok = TokenV(tok: TEOF, tags: TVNormal)
                return
            continue
        if p.c == '\n':
            if p.flags == PFPP:
                p.flags = PFNormal
                p.tok = TokenV(tok: TNewLine, tags: TVNormal)
                return
            eat()
            continue
        if p.c in {'0'..'9'}:
            if p.flags == PFPP:
                readPPNumber()
            else:
                readNumberLit()
            return
        case p.c:
        of 'u':
            eat()
            if p.c == '"':
                eat()
                readStringLit(16)
            elif p.c == '\'':
                readCharLit(Ilong)
            elif p.c == '8':
                eat()
                if p.c != '"':
                    if p.c == '\'':
                        readCharLit()
                    else:
                        p.tok = TokenV(tok: CPPIdent, tags: TVSVal, s: "u8")
                        readIdentLit()
                else:
                    eat()
                    readStringLit(8)
            else:
                p.tok = TokenV(tok: CPPIdent, tags: TVSVal, s: "u")
                readIdentLit()
            return
        of 'U':
          eat()
          if p.c != '"':
            if p.c == '\'':
                readCharLit(Iulong)
            else:
                p.tok = TokenV(tok: CPPIdent, tags: TVSVal, s: "U")
                readIdentLit()
          else:
            eat()
            readStringLit(32)
          return
        of 'L':
            eat()
            if p.c != '"':
                if p.c == '\'':
                    readCharLit(Ilonglong)
                else:
                    p.tok = TokenV(tok: CPPIdent, tags: TVSVal, s: "L")
                    readIdentLit()
            else:
                eat()
                readStringLit(16)
            return
        else:
            discard

        if p.c in {'a'..'z', 'A'..'Z', '\x80' .. '\xFD', '_', '$', '\\'}:
            p.tok = TokenV(tok: CPPIdent, tags: TVSVal, s: "")
            readIdentLit()
            return
        case p.c:
        of '.':
            if p.flags == PFNormal:
                eat() # first
                if p.c == '.':
                    eat() # second
                    if p.c != '.':
                        make_tok(TEllipsis2)
                        return
                    eat() # third
                    make_tok(TEllipsis)
                elif p.c in {'0'..'9'}:
                    readFloatLit()
                else:
                    make_tok(TDot)
            else:
                eat() # first '.'
                if p.c == '.':
                    make_tok(TDot)
                else:
                    p.tok = TokenV(tok: TPPNumber, tags: TVSVal)
                    readPPNumberAfterDot()
            return
        of '\0':
            if p.flags == PFPP:
                p.flags = PFNormal
            p.tok = TokenV(tok: TEOF, tags: TVNormal)
            return
        of '(', ')', '~', '?', '{', '}', ',', '[', ']', ';', '@':
            make_tok(cast[Token](p.c))
            eat()
            return
        of '"':
            eat()
            readStringLit(8)
            return
        of ':':
            eat()
            if p.c == '>':
                make_tok(TRSquareBrackets)
                eat()
            else:
                make_tok(TColon)
            return
        of '-':
            eat()
            if p.c == '-':
                make_tok(TSubSub)
                eat()
            elif p.c == '=':
                make_tok(TAsignSub)
                eat()
            else:
                make_tok(TDash)
            return
        of '+':
            eat()
            if p.c == '+':
                make_tok(TAddAdd)
                eat()
            elif p.c == '=':
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
            if p.c == '=':
                make_tok(TGe)
                eat()
            elif p.c == '>':
                make_tok(Tshr)
                eat()
            else:
                make_tok(TGt)
            return
        of '<':
            eat()
            case p.c:
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
                make_tok(TLe)
            return
        of '%':
            eat()
            if p.c == '=':
                make_tok(TAsignRem)
                eat()
            elif p.c == '>':
                make_tok(TRcurlyBracket)
                eat()
            elif p.c == ':':
                make_tok(TBash)
                eat()
            else:
                make_tok(TPercent)
            return
        of '*':
            eat()
            if p.c == '=':
                make_tok(TAsignMul)
                eat()
            else:
                make_tok(TMul)
            return
        of '=':
            eat()
            if p.c == '=':
                make_tok(TEq)
                eat()
            else:
                make_tok(TAssign)
            return  
        of '&':
            eat()
            if p.c == '=':
                make_tok(TAsignBitAnd)
                eat()
            elif p.c == '&':
                make_tok(TLogicalAnd)
                eat()
            else:
                make_tok(TBitAnd)
            return
        of '|':
            eat()
            if p.c == '=':
                make_tok(TAsignBitOr)
                eat()
            elif p.c == '|':
                make_tok(TLogicalOr)
                eat()
            else:
                make_tok(TBitOr)
            return
        of '^':
            eat()
            if p.c == '=':
                make_tok(TAsignBitXor)
                eat()
            else:
                make_tok(TXor)
            return
        of '/':
            eat()
            if p.c == '=':
                make_tok(TAsignDiv)
                eat()
                return
            else:
                make_tok(TSlash)
                return
        of '!':
            eat()
            if p.c == '=':
                make_tok(TNe)
                eat()
            else:
                make_tok(TNot)
            return
        else:
          warning("invalid token: " & show(p.c))

        eat()
