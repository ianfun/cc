## C's lexer and preprocessor(CPP)
##
## the main function is getToken and read_pp_number, used by parser
## 
## see 
##  <https://gcc.gnu.org/onlinedocs/cpp>
##  <https://gcc.gnu.org/onlinedocs/gcc-10.4.0/cpp/> 
## for details

import token
import std/[unicode, math, tables]

proc read_pp_number*(s: string, f: var float, n: var int): int

proc nextTok*()

proc checkMacro()

proc getToken*() =
    ## read a token from lexer, if detect a macro, expand to lex stream
    if len(p.tokenq) == 0:
        nextTok()
    else:
        p.tok = p.tokenq.pop()
    if p.tok.tok == TIdentifier:
        let o = p.flags
        p.flags = PFPP
        checkMacro()
        p.flags = o

proc expand(a: seq[TokenV]): seq[TokenV] = 
    ## expand object-like macro
    ##
    ## note: this function ignore all space tokens!
    for t in a:
        case t.tok:
        of TIdentifier:
            if isMacroInUse(t.s):
                note("self-reference macro " & t.s & " skipped")
                result &= TokenV(tok: TIdentifier2, tags: TVSVal, s: t.s)
            else:
                let m = p.macros.getOrDefault(t.s, nil)
                if m != nil:
                    beginExpandMacro(t.s)
                    result &= expand(m.tokens)
                    endExpandMacro(t.s)
                else:
                    result &= TokenV(tok: TIdentifier2, tags: TVSVal, s: t.s)
        of TSpace:
            discard
        of PPSharp:
            parse_error("'#' in object-like macro, ignore it")
        else:
            result &= t

proc isprint*(a: cint): cint {.importc: "isprint", nodecl, header: "ctype.h".}

const hexs*: cstring = "0123456789ABCDEF"

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

proc stringizing*(a: seq[TokenV]): string =
    for t in a:
        case t.tok:
        of TSpace:
            result.add(" ")
        of TIdentifier, TPPNumber:
            result.add(t.s)
        of TStringLit:
             result.add('"')
             for v in t.s:
                result.add(stringizing(v))
             result.add('"')
        of PPSharp:
            result.add('#')
        of PPSharpSharp:
            result.add("##")
        of TCharLit:
            result.add(stringizing(char(t.i)))
        else:
            if uint(t.tok) < 255:
                result.add(char(t.tok))
            else:
                result.add($t.tok)


proc expand2(a: seq[TokenV], args: seq[seq[TokenV]], params: seq[string]): seq[TokenV] = 
    ## expand function-like macro
    ##
    ## note: this function ignore all space tokens!
    var i = 0
    while true:
        if i == len(a):
            break
        var t = a[i]
        inc i
        case t.tok:
        of TIdentifier:
            let k = params.find(t.s)
            if k != -1:
                result &= args[k]
            elif isMacroInUse(t.s):
                note("self-reference macro " & t.s & " skipped")
                result &= TokenV(tok: TIdentifier2, tags: TVSVal, s: t.s)
            else:
                let m = p.macros.getOrDefault(t.s, nil)
                if m != nil:
                    beginExpandMacro(t.s)
                    result &= expand(m.tokens)
                    endExpandMacro(t.s)
                else:
                    result &= TokenV(tok: TIdentifier2, tags: TVSVal, s: t.s)
        of TSpace:
            discard
        of PPSharp:
            if i == len(a):
                parse_error("expect macro parameter after '#', got EOF")
            else:
                let n = a[i]
                if n.tok != TIdentifier:
                    parse_error("expect macro parameter after '#'")
                else:
                    let k = params.find(n.s)
                    if k == -1:
                        parse_error(n.s & " is not a found in macro parameters (in argument to '#')")
                    else:
                        let r = args[k]
                        result &= TokenV(tok: TStringLit, tags: TVSVal, s: stringizing(r))
                        inc i
        else:
            result &= t

proc checkMacro() =
    var name = p.tok.s
    let m = p.macros.getOrDefault(name, nil)
    if m != nil:
        if m.funcmacro == false:
            if len(m.tokens) == 0:
                getToken()
            else:
                beginExpandMacro(name)
                let l = expand(m.tokens)
                for i in l:
                    p.tokenq.insert(i, 0)
                endExpandMacro(name)
                getToken()
        else:
            var my = p.tok
            nextTok()
            if p.tok.tok == TLbracket:
                nextTok()
                var args: seq[seq[TokenV]]
                while true:
                    if p.tok.tok == TEOF:
                        parse_error("unexpected EOF while parsing function-like macro arguments")
                        return
                    if p.tok.tok == TRbracket:
                        break
                    elif p.tok.tok == TComma:
                        args.add(default(seq[TokenV]))
                    else:
                        if len(args) == 0:
                            args.add(@[p.tok])
                        else:
                            args[^1].add(p.tok)
                    nextTok()
                if m.ivarargs:
                    if len(args) < len(m.params):
                        parse_error("function-like macro " & name & " expect at least " & $len(m.params) & " arguments, but " & $len(args) & " provided")
                        return
                else:
                    if len(args) != len(m.params):
                        parse_error("function-like macro " & name & " expect " & $len(m.params) & " arguments, but got " & $len(args) & " provided")
                        return
                beginExpandMacro(name)
                let l = expand2(m.tokens, args=args, params=m.params)
                for i in l:
                    p.tokenq.insert(i, 0)
                endExpandMacro(name)
                getToken()
                echo p.tok[]
            else:
                putToken()
                p.tok = my
    else:
        p.tok = TokenV(tok: TIdentifier2, tags: TVSVal, s: p.tok.s)

import eval

proc isalnum*(c: char): bool =
    return c in {'A'..'Z', 'a'..'z', '0'..'9'}

proc lowleveleat*() =
    ## LF, CR LF, CR is ok
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
    p.tok = TokenV(tok: TCharLit, tags: TVIVal)
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
      else:
        if not (isalnum(p.c) or (uint8(p.c) and 0x80'u8) != 0 or p.c == '$' or p.c == '_'):
            break
        p.tok.s.add(p.c)
        eat()
    if p.tok.s == "__COUNTER__":
        p.tok = TokenV(tok: TPPNumber, tags: TVSVal, s: $p.counter)
        inc p.counter
    elif p.flags == PFNormal:
      let k = isKeyword(p.tok.s)
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
    p.tok = TokenV(tok: TFloatLit, tags: TVFVal)
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
            p.tok.f *= pow(2, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
            if p.c in {'L', 'l', 'F', 'f'}: # TODO: mark as float
                eat()
            break

# read a float start from '.'
proc readFloatLit(intPart: float = 0.0) =
    p.tok = TokenV(tok: TNul, tags: TVFval)
    p.tok.f = intPart
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
            p.tok.f *= pow(10, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
            if p.c in {'L', 'l', 'F', 'f'}:
                 # TODO: mark as float
                eat()
            break
        p.tok.f += float(int(p.c) - '0'.int) / pow(10.0, float(e))
        eat()

proc readNumberLit() =
    p.tok = TokenV(tok: TNumberLit, tags: TVIVal)
    p.tok.i = 0
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
        else:
          return # zero
    else: # decimal
      while true:
        p.tok.i = (10 * p.tok.i) + (int(p.c) - '0'.int)
        eat()
        if p.c == '.':
            readFloatLit(float(p.tok.i))
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
    p.tok = TokenV(tok: TStringLit, tags: TVSVal, s: "")
    while true:
        if p.c == '\\':
            p.tok.s.add(Rune(readEscape()).toUTF8)
        elif p.c == '"':
            eat()
            break
        else:
            if 0xF0 == (0xF8 and Codepoint(p.c)): # 4 byte
              p.tok.s.add(p.c)
              eat()
              p.tok.s.add(p.c)
              eat()
              p.tok.s.add(p.c)
              eat()
              p.tok.s.add(p.c)
            elif 0xE0 == (0xF0 and Codepoint(p.c)): # 3 byte
              p.tok.s.add(p.c)
              eat()
              p.tok.s.add(p.c)
              eat()
              p.tok.s.add(p.c)
            elif 0xC0 == (0xE0 and Codepoint(p.c)): # 2 byte
              p.tok.s.add(p.c)
              eat()
              p.tok.s.add(p.c)
            else: # 1 byte
              if p.c == '\n':
                warning("missing terminating '\"' character, read newline as \\n")
              elif p.c == '\0':
                parse_error("unexpected EOF")
                return
              p.tok.s.add(p.c)
            eat()
    # TODO: p.tok.i = enc

proc readPPNumberAfterDot() =
    p.tok.s.add('.')
    while p.c in {'0'..'9'}:
        p.tok.s.add(p.c)
        eat()

proc readPPNumber() =
    p.tok = TokenV(tok: TPPNumber, tags: TVSVal)
    while true:
        p.tok.s.add(p.c)
        eat()
        if p.c notin {'0'..'9'}:
            break
    if p.c == '.':
        eat()
        readPPNumberAfterDot()
    elif p.c in {'e', 'p', 'E', 'P'}:
        p.tok.s.add(p.c)
        eat()
        if p.c == '+' or p.c == '-':
            p.tok.s.add(p.c)
            eat()
    # nodigit
    elif p.c in {'a'..'z', 'A'..'Z', '_'}:
        while true:
            p.tok.s.add(p.c)
            if p.c notin {'a'..'z', 'A'..'Z', '_'}:
                break
    # universal-character-name
    elif p.c == '\\':
        eat()
        let n = if p.c == 'U': 8 else: 4
        eat()
        let c = readUChar(n)
        p.tok.s.add(Rune(c).toUTF8)

proc skipLine(warn: bool = true) =
    while p.c != '\n' and p.c != '\0':
        if warn:
          warning("extra tokens found")
        eat()
    p.flags = PFNormal

proc nextTok*() =
    while true:
        if p.c in CSkip:
            while true:
                eat()
                if p.c notin CSkip:
                    break
            if p.flags == PFPP:
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
            if p.tok.tok != TIdentifier:
                p.flags = PFNormal
                parse_error("invalid preprocessing directive: expect an identifier")
                return
            case p.tok.s:
            of "define":
                nextTok() # name
                while p.tok.tok == TSpace:
                    nextTok()
                if p.tok.tok != TIdentifier:
                    p.flags = PFNormal
                    parse_error("macro name should be a identifier, got " & showToken())
                    note("the syntax is:\n\t#define identifier replacement-list\n\t#define identifier(identifier-list) replacement-list")
                    return
                var name = p.tok.s # copy to string
                nextTok()
                var m = if p.tok.tok == TLbracket: PPMacro(funcmacro: true, ivarargs: false) else: PPMacro(funcmacro: false)
                if m.funcmacro:
                    while true:
                        nextTok()
                        if p.tok.tok == TIdentifier:
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
                        elif p.tok.tok == PPEllipsis or (p.tok.tok == TIdentifier and p.tok.s == "__VA_ARGS__"):
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
                if len(m.tokens) >= 1:
                    if m.tokens[0].tok == PPSharpSharp:
                        parse_error("'##' cannot appear at start of macro expansion")
                    if len(m.tokens) >= 2:
                        if m.tokens[^1].tok == PPSharpSharp:
                            parse_error("'##' cannot appear at end of macro expansion")
                macro_define(name, m)
            of "if":
                nextTok() # if
                while p.tok.tok == TSpace:
                    nextTok()
                var ok: bool
                let e = constant_expression()
                if e == nil:
                    parse_error("expect constant_expression")
                    ok = false
                else:
                    var f = p.flags
                    p.flags = PFPP
                    ok = eval_const_expression_bool(e)
                    p.flags = f
                p.ppstack.add(if ok: 1 else: 0)
                p.ok = ok
                skipLine()
            of "ifdef", "ifndef":
                let ndef = p.tok.s == "ifndef"
                nextTok()
                if p.tok.tok != TIdentifier:
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
                    let e = constant_expression()
                    if e == nil:
                        parse_error("expect constant_expression")
                        ok = false
                    else:
                        var f = p.flags
                        p.flags = PFPP
                        ok = eval_const_expression_bool(e)
                        p.flags = f
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
                if p.tok.tok != TIdentifier:
                    p.flags = PFNormal
                    parse_error("macro name should be a identifier")
                    note("the syntax is: #undef identifier")
                    return
                macro_undef(p.tok.s)
                skipLine()
            of "pragma":
                # https://docs.microsoft.com/en-us/cpp/preprocessor/pragma-directives-and-the-pragma-keyword
                # https://gcc.gnu.org/onlinedocs/gcc/Pragmas.html
                # https://gcc.gnu.org/onlinedocs/cpp/Pragmas.html
                # pragma once
                nextTok()
                if p.tok.tok != TIdentifier:
                    p.flags = PFNormal
                    note("pragma should start with a identifier")
                    return
                case p.tok.s:
                of "once":
                    addOnce()
                else:
                    discard
                skipLine(false)
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
                return
            else:
                parse_error("invalid directive: " & p.tok.s)
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
                p.tok = TokenV(tok: TEOF, tags: TVNormal)
                return
            continue
        if p.c == '\n':
            if p.flags == PFPP:
                p.flags = PFNormal
                p.tok = TokenV(tok: TNul, tags: TVNormal)
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
                    p.tok = TokenV(tok: TIdentifier, tags: TVSVal, s: "u8")
                    readIdentLit()
                else:
                    eat()
                    readStringLit(8)
            else:
                p.tok = TokenV(tok: TIdentifier, tags: TVSVal, s: "u")
                readIdentLit()
            return
        of 'U':
          eat()
          if p.c != '"':
            p.tok = TokenV(tok: TIdentifier, tags: TVSVal, s: "U")
            readIdentLit()
          else:
            eat()
            readStringLit(32)
          return
        of 'L':
            eat()
            if p.c != '"':
                p.tok = TokenV(tok: TIdentifier, tags: TVSVal, s: "L")
                readIdentLit()
            else:
                eat()
                readStringLit(16)
            return
        else:
            discard

        if p.c in {'a'..'z', 'A'..'Z', '\x80' .. '\xFD', '_', '$', '\\'}:
            p.tok = TokenV(tok: TIdentifier, tags: TVSVal, s: "")
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
                    p.tok = TokenV(tok: TPPNumber, tags: TVSVal)
                    readPPNumberAfterDot()
            return
        of '\0':
            if p.flags == PFPP:
                p.flags = PFNormal
            p.tok = TokenV(tok: TEOF, tags: TVNormal)
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


