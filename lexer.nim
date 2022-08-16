##[
C's lexer - export nextTok function
]##

import token
import std/[unicode, math, deques]

proc isalnum*(c: char): bool =
    return c in {'A'..'Z', 'a'..'z', '0'..'9'}

proc lowleveleat*() =
    # LF, CR LF, CR is ok
    p.fs_read()
    inc p.col
    if p.c == '\n':
        resetLine()


proc do_eat*() =
    lowleveleat()
    if p.c == '/':
        lowleveleat()
        if p.c == '*':
            note("C++ style-comment")
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
            note("C style-comment")
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
    p.tok = op

proc make_ch_lit(ch: char) {.inline.} =
    p.val.ival = ch.int

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
    of '0', '1', '2', '3', '4', '5', '6', '7':
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

proc readCharLit() =
    p.tok = TCharLit
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
    p.tok = TIdentifier
    while true:
      if p.c == '\\':
        eat()
        if p.c == 'U':
            eat()
            let codepoint = readUChar(8)
            validUCNName(codepoint)
            p.val.sval.add(Rune(codepoint).toUTF8)
        elif p.c == 'u':
            eat()
            let codepoint = readUChar(4)
            validUCNName(codepoint)
            p.val.sval.add(Rune(codepoint).toUTF8)
        else:
            parse_error("invalid escape in identifier")
      else:
        if not (isalnum(p.c) or (uint8(p.c) and 0x80'u8) != 0 or p.c == '$' or p.c == '_'):
            break
        p.val.sval.add(p.c)
        eat()
    if p.flags == PFNormal:
      let k = isKeyword(p.val.sval)
      if k != TNul:
          make_tok(k)

proc decimaltoInt(s: string): int =
    for c in s:
        result = (result * 10) + (int(c) - '0'.int)

proc decimal16toInt(s: string): int =
    for c in s:
        if c in {'0'..'9'}:
            result = (result shl 4) or (int(c) - '0'.int)
        elif c in {'a'..'f'}:
            result = (result shl 4) or (int(c) - 'a'.int + 10)
        else:
            result = (result shl 4) or (int(c) - 'A'.int + 10)

proc decimal2toInt(s: string): int =
    for c in s:
        if c == '1':
            result = (result shl 1) or 1
        else:
            result = result shl 1

proc pow10(a: int): int =
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
    while true:
        if i == len(s):
            break
        if s[i] notin {'f', 'F', 'L', 'l'}:
            parse_error("invalid float suffix: " & s[i])
            return 0
        inc i
    o = f
    return 2

# attempt to paser pp-number to int
# return 0 if failed, 1 if is a integer, 2 if is a float
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
        of '0', '1', '2', '3', '4', '5', '6', '7':
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
    var isfloat = 0
    case s[i]:
    of 'E', 'e', 'P', 'p':
        isfloat = 1
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
        if s[i] notin {'L', 'l', 'F', 'f'}:
            parse_error("invalid float suffix::::: " & show(s[i]))
            return 0
        if s[i] == 'F' or s[i] == 'f':
            if isfloat != 1:
                isfloat = 3
        inc i
    if isfloat == 3:
        f = float(num)
    elif isfloat == 0:
        n = num
    return if isfloat==0: 1 else: 2

proc readHexFloatLit(intPart: float = 0.0) =
    # read a float start from '.'
    p.tok = TFloatLit
    p.val.fval = intPart
    var e = 1.0
    while true:
        e /= 16.0
        eat()
        if p.c in {'0'..'9'}:
          p.val.fval += float(int(p.c) - '0'.int) * e
        elif p.c in {'a'..'f'}:
          p.val.fval += float(int(p.c) - 'a'.int + 10) * e
        elif p.c in {'A'..'F'}:
          p.val.fval += float(int(p.c) - 'A'.int + 10) * e
        else:
            if p.c in {'L', 'l', 'F', 'f'}:
                eat() # TODO: mark as float
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
            p.val.fval *= pow(2, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
            if p.c in {'L', 'l', 'F', 'f'}:
                 # TODO: mark as float
                eat()
            break


proc readFloatLit(intPart: float = 0.0) =
    # read a float start from '.'
    p.tok = TFloatLit
    p.val.fval = intPart
    var e = 0
    while true:
        inc e
        if p.c notin {'0'..'9'}:
            if p.c in {'L', 'l', 'F', 'f'}:
                eat() # TODO: mark as float
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
            var exps: string
            while true:
                exps.add(p.c)
                eat()
                if p.c notin {'0'..'9'}:
                    break
            p.val.fval *= pow(10, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
            if p.c in {'L', 'l', 'F', 'f'}:
                 # TODO: mark as float
                eat()
            break
        p.val.fval += float(int(p.c) - '0'.int) / pow(10.0, float(e))
        eat()

proc readNumberLit() =
    p.tok = TNumberLit
    p.val.ival = 0
    if p.c == '0':
        eat()
        case p.c:
        of '0', '1', '2', '3', '4', '5', '6', '7': # octal
          discard
        of 'x', 'X': # hex
          eat()
          if p.c notin {'0'..'9', 'A'..'F', 'a'..'f'}:
            parse_error("invalid hex literal: " & show(p.c))
            return
          while true:
            if p.c in {'0'..'9'}:
              p.val.ival = (p.val.ival shl 4) or (int(p.c) - '0'.int)
            elif p.c in {'a'..'f'}:
              p.val.ival = (p.val.ival shl 4) or (int(p.c) - 'a'.int + 10)
            elif p.c in {'A'..'F'}:
              p.val.ival = (p.val.ival shl 4) or (int(p.c) - 'A'.int + 10)
            elif p.c in {'L', 'l', 'U', 'u'}:
              while p.c in {'L', 'l', 'U', 'u'}:
                eat()
              return
            elif p.c == '.':
                readHexFloatLit(float(p.val.ival))
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
              p.val.ival = p.val.ival shl 1
            of '1':
              p.val.ival = (p.val.ival shl 1) or 1
            else:
              if p.c in {'0'..'9'}:
                warning("invalid decimal digit in binary literal")
              return
            eat()
        of '.': # float
          eat()
          readFloatLit()
        else:
          return # zero
    else: # decimal
      while true:
        p.val.ival = (10 * p.val.ival) + (int(p.c) - '0'.int)
        eat()
        if p.c == '.':
            readFloatLit(float(p.val.ival))
            return
        if p.c in {'L', 'l', 'U', 'u'}:
            while p.c in {'L', 'l', 'U', 'u'}:
                eat()
            return
        elif p.c notin {'0'..'9'}:
            if p.c in {'a'..'z', 'A'..'Z'}: # User-defined literals?
              warning("invalid decimal suffix " & show(p.c))
              note("user-defined literals is a C++ feature")
            return

proc readStringLit(enc: int) =
    p.tok = TStringLit
    while true:
        if p.c == '\\':
            p.val.sval.add(Rune(readEscape()).toUTF8)
        elif p.c == '"':
            eat()
            break
        else:
            if 0xF0 == (0xF8 and Codepoint(p.c)): # 4 byte
              p.val.sval.add(p.c)
              eat()
              p.val.sval.add(p.c)
              eat()
              p.val.sval.add(p.c)
              eat()
              p.val.sval.add(p.c)
            elif 0xE0 == (0xF0 and Codepoint(p.c)): # 3 byte
              p.val.sval.add(p.c)
              eat()
              p.val.sval.add(p.c)
              eat()
              p.val.sval.add(p.c)
            elif 0xC0 == (0xE0 and Codepoint(p.c)): # 2 byte
              p.val.sval.add(p.c)
              eat()
              p.val.sval.add(p.c)
            else: # 1 byte
              if p.c == '\n':
                warning("missing terminating '\"' character, read newline as \\n")
              elif p.c == '\0':
                parse_error("unexpected EOF")
                return
              p.val.sval.add(p.c)
            eat()
    p.val.ival = enc

proc readPPNumberAfterDot() =
    p.val.sval.add('.')
    while p.c in {'0'..'9'}:
        p.val.sval.add(p.c)
        eat()

proc readPPNumber() =
    make_tok(TPPNumber)
    while true:
        p.val.sval.add(p.c)
        eat()
        if p.c notin {'0'..'9'}:
            break
    if p.c == '.':
        eat()
        readPPNumberAfterDot()
    elif p.c in {'e', 'p', 'E', 'P'}:
        p.val.sval.add(p.c)
        eat()
        if p.c == '+' or p.c == '-':
            p.val.sval.add(p.c)
            eat()
    # nodigit
    elif p.c in {'a'..'z', 'A'..'Z', '_'}:
        while true:
            p.val.sval.add(p.c)
            if p.c notin {'a'..'z', 'A'..'Z', '_'}:
                break
    # universal-character-name
    elif p.c == '\\':
        eat()
        let n = if p.c == 'U': 8 else: 4
        eat()
        let c = readUChar(n)
        p.val.sval.add(Rune(c).toUTF8)

proc skipLine(warn: bool = true) =
    while p.c != '\n' and p.c != '\0':
        if warn:
          warning("extra tokens found")
        eat()
    p.flags = PFNormal

proc nextTok*() =
    while true:
        p.val.sval.setLen 0
        if p.c in CSkip:
          eat()
          continue
        if p.c == '#':
            if p.flags == PFPP:
                eat()
                if p.c == '#':
                    make_tok(PPSharpSharp)
                    eat()
                make_tok(PPSharp) 
                return
            eat()
            p.flags = PFPP
            nextTok() # directive
            if p.tok != TIdentifier:
                p.flags = PFNormal
                parse_error("invalid preprocessing directive: expect an identifier")
                return
            case p.val.sval:
            of "define":
                nextTok() # name
                if p.tok != TIdentifier:
                    p.flags = PFNormal
                    parse_error("macro name should be a identifier")
                    note("the syntax is:\n\t#define identifier replacement-list\n\t#define identifier(identifier-list) replacement-list")
                    return
                var name = p.val.sval # copy to string
                nextTok()
                var m = if p.tok == TLbracket: PPMacro(funcmacro: true, ivarargs: false) else: PPMacro(funcmacro: false)
                if m.funcmacro:
                    while true:
                        nextTok()
                        if p.tok == TIdentifier:
                            m.params.add(p.val.sval)
                            nextTok()
                            if p.tok == TRbracket:
                                break
                            elif p.tok == TComma:
                                continue
                            else:
                                parse_error("')' or ',' expected")
                                p.flags = PFNormal
                                return
                        elif p.tok == PPEllipsis or p.tok == PPVAARGS:
                            m.ivarargs = true
                            nextTok()
                            if p.tok != TRbracket:
                                parse_error("')' expected")
                                p.flags = PFNormal
                                return
                            break
                        elif p.tok == TRbracket:
                            break
                    nextTok()
                while true:
                    if p.flags != PFPP:
                        break
                    m.tokens.add(tokenToPPToken())
                    nextTok()
                echo m.tokens
                macro_define(name, m)
            of "if":
                let v = true # eval . parse
                p.ppstack.add(if v: 1 else: 0)
                p.ok = v
                skipLine()
            of "ifdef", "ifndef":
                let ndef = p.val.sval=="ifndef"
                nextTok()
                if p.tok != TIdentifier:
                    p.flags = PFNormal
                    parse_error("expect identifier")
                    note("the syntax is:\n\t#ifdef identifier\n\t#ifndef identifier")
                    return
                let name = p.val.sval # no copy
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
                if p.ppstack.len == 0:
                    parse_error("no matching #if")
                    return
                if (p.ppstack[^1] and 2) != 0:
                    parse_error("#elif after #else")
                    return
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
                    echo "including file: ", path
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
                if p.tok != TIdentifier:
                    p.flags = PFNormal
                    parse_error("macro name should be a identifier")
                    note("the syntax is: #undef identifier")
                    return
                macro_undef(p.val.sval)
                skipLine()
            of "pragma":
                # https://docs.microsoft.com/en-us/cpp/preprocessor/pragma-directives-and-the-pragma-keyword
                # https://gcc.gnu.org/onlinedocs/gcc/Pragmas.html
                # https://gcc.gnu.org/onlinedocs/cpp/Pragmas.html
                # pragma once
                nextTok()
                if p.tok != TIdentifier:
                    p.flags = PFNormal
                    note("pragma should start with a identifier")
                    return
                case p.val.sval:
                of "once":
                    addOnce()
                else:
                    discard
                skipLine(false)
            of "error", "warning":
                var s: string
                let iswarning = p.val.sval == "warning"
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
                return
            else:
                parse_error("invalid directive: " & p.val.sval)
                skipLine(false)
            eat()
            continue
        if p.flags == PFNormal and p.ok == false:
            while p.c != '\n' and p.c != '\0':
                eat()
            if p.c == '\n':
                eat()
            elif p.c == '\0':
                p.flags = PFNormal
                p.tok = TEOF
                return
            continue
        if p.c == '\n':
            if p.flags == PFPP:
                p.flags = PFNormal
                p.tok = TNul
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
            elif p.c == '8':
                eat()
                if p.c != '"':
                    p.val.sval = "u8"
                    readIdentLit()
                else:
                    eat()
                    readStringLit(8)
            else:
                p.val.sval = "u"
                readIdentLit()
            return
        of 'U':
          eat()
          if p.c != '"':
            p.val.sval = "U"
            readIdentLit()
          else:
            eat()
            readStringLit(32)
          return
        of 'L':
          eat()
          if p.c != '"':
            p.val.sval = "L"
            readIdentLit()
          else:
            eat()
            readStringLit(16)
          return
        else:
            discard

        if p.c in {'a'..'z', 'A'..'Z', '\x80' .. '\xFD', '_', '$', '\\'}:
         readIdentLit()
         return

        case p.c:
        of '.':
            if p.flags == PFNormal:
                eat() # first
                if p.c == '.':
                    eat() # second
                    if p.c != '.':
                        parse_error("'..' is invalid token, do you mean '...'?")
                        return
                    eat() # third
                    make_tok(PPEllipsis)    
                else:
                    readFloatLit()
            else:
                eat() # first '.'
                if p.c == '.':
                    eat() # second '.'
                    if p.c != '.':
                        parse_error("'..' is invalid token, do you mean '...'?")
                        return
                    eat() # third '.'
                    make_tok(PPEllipsis)
                else:
                    make_tok(TPPNumber)
                    readPPNumberAfterDot()
            return
        of '\0':
            if p.flags == PFPP:
                p.flags = PFNormal
            p.tok = TEOF
            return
        of '(', ')', '~', '?', '{', '}', ',', '[', ']', ';':
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
            else:
                make_tok(TBitAnd)
            return
        of '|':
            eat()
            if p.c == '=':
                make_tok(TAsignBitOr)
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

proc putToken*() = 
    p.tokenq.addLast(getTokenV())

proc expandObjMacro*(t: seq[PPToken]) =
    for i in t: 
        var v: TokenV
        case i.tok:
        of TPPNumber:
            var f: float
            var n: int
            let ok = read_pp_number(i.s, f, n)
            if ok == 0:
                return
            if ok == 1:
                v = TokenV(tags: TVIVal, i: n, t: i.tok)
            else:
                assert ok == 2
                v = TokenV(tags: TVFval, f: f, t: i.tok)
        of PPEllipsis:
            v = TokenV(tags: TVNormal, t: PPEllipsis)
        of PPSharp, PPSharpSharp:
            parse_error("stray # in program")
            return
        of TIdentifier:
            v = TokenV(tags: TVSVal, s: i.s)
        else:
            v = TokenV(tags: TVNormal, t: i.tok)
        p.tokenq.addLast(v)

proc getToken*() =
    if len(p.tokenq) == 0:
        nextTok()
    else:
        let v = p.tokenq.popFirst()
        p.tok = v.t
        case v.tags:
        of TVNormal:
            discard
        of TVIVal:
            p.val.ival = v.i
        of TVFval:
            p.val.fval = v.f
        of TVSVal:
            p.val.sval = v.s

