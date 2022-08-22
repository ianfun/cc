## C's lexer and preprocessor(CPP)
##
## the main function is getToken and read_pp_number, used by parser
## 
## see 
##  <https://gcc.gnu.org/onlinedocs/cpp>
##  <https://gcc.gnu.org/onlinedocs/gcc-10.4.0/cpp/> 
## for details

import stream, token, pragmas
import std/[unicode, math, times, tables]

proc getToken*()

proc eat*()

proc isprint*(a: cint): cint {.importc: "isprint", nodecl, header: "ctype.h".}

const hexs*: cstring = "0123456789ABCDEF"


proc builtin_Pragma() =
  getToken()
  if p.tok.tok != TLbracket:
      parse_error("expect ')'")
      note("the syntax is:\n\t_Pragma(<string>)")
  else:
      getToken()
      if p.tok.tok != TStringLit:
          parse_error("expect string literal")
          note("the syntax is:\n\t_Pragma(<string>)")
      else:
          var pra = p.tok.s
          getToken()
          if p.tok.tok != TRbracket:
              parse_error("expect ')'")
          else:
              echo "pragma: ", pra
              getToken()


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
  of "_Pragma":
    result = PPMacro(flags: MBuiltin, fn: builtin_Pragma)
  else:
    result = p.macros.getOrDefault(name, nil)

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

proc read_pp_number*(s: string, f: var float, n: var int): int

proc nextTok*()

proc checkMacro()

proc getToken*() =
    ## CPP layer(Preprocessing)
    if len(p.tokenq) == 0:
        nextTok()
    else:
        p.tok = p.tokenq.pop()
    if p.tok.tok == CPPIdent:
        let o = p.flags
        p.flags = PFPP
        checkMacro()
        p.flags = o
        if p.tok.tok == TNewLine:
            getToken()
    elif p.tok.tok == PPMacroPop:
        endExpandMacro(p.tok.s)
        getToken()

proc paste_token(a, b: TokenV): TokenV =
    var oldp = p
    p = Parser(filenamestack: @["##"], pathstack: @["##"], flags: PFPP, line: 1, col: 1, ok: true, err: false, c: ' ', lastc: 256)
    p.tok = TokenV(tok: TNul, tags: TVNormal)
    result = TokenV(tok: TNul, tags: TVNormal)
    let s = stringizing(a) & stringizing(b)
    p.fstack.add(newStringStream(s))
    nextTok()
    if p.tok.tok == TSpace:
        nextTok() 
    if p.fstack.len != 0:
        discard # more then one tokens? invalid
    elif p.tok.tok == TNul:
        discard # parse failed!
    else:
        result = p.tok
    p = oldp
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
    var name = p.tok.s
    let m = getMacro(name)
    if m != nil:
        case m.flags:
        of MOBJ:
            if len(m.tokens) > 0:
                beginExpandMacro(name)
                p.tokenq.add(TokenV(tok: PPMacroPop, tags: TVSVal, s: name))
                var cp = m.tokens                
                while concatenation(cp):
                    discard
                for i in countdown(len(cp)-1, 0):
                    let t = cp[i]
                    if t.tok != TSpace:
                        if t.tok == CPPIdent:
                            if isMacroInUse(t.s):
                                note("self-referential macro '" & t.s & "' skipped")
                                t.tok = TIdentifier
                        p.tokenq.add(t)
            getToken()
        of MFUNC:
            var my = p.tok
            getToken()
            if p.tok.tok == TLbracket:
                getToken()
                var args: seq[seq[TokenV]]
                args.add(default(seq[TokenV]))
                while true:
                    if p.tok.tok == TEOF:
                        parse_error("unexpected EOF while parsing function-like macro arguments")
                        return
                    if p.tok.tok == TRbracket:
                        break
                    elif p.tok.tok == TComma:
                        args.add(default(seq[TokenV]))
                    elif p.tok.tok == TNewLine: # $6.10.3-10 new-line is considered a normal white-space character
                        args[^1].add(TokenV(tok: TSpace, tags: TVNormal))
                        p.flags = PFPP
                        eat()
                    else:
                        args[^1].add(p.tok)
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
                    
                    p.tokenq.add(TokenV(tok: PPMacroPop, tags: TVSVal, s: name))
                    for i in s:
                        p.tokenq.add(i)
                getToken()
            else:
                if p.tok.tok != TNewLine:
                    putToken()
                p.tok = my
                p.tok.tok = TIdentifier
        of MBuiltin:
            m.fn()
    else:
        p.tok.tok = TIdentifier

import eval

proc isalnum*(c: char): bool =
    return c in {'A'..'Z', 'a'..'z', '0'..'9'}

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
                let e = constant_expression()
                if e == nil:
                    parse_error("expect constant_expression")
                    ok = false
                else:
                    ok = eval_const_expression_bool(e)
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
                    let e = constant_expression()
                    if e == nil:
                        parse_error("expect constant_expression")
                        ok = false
                    else:
                        ok = eval_const_expression_bool(e)
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
                pragma(pragmas)
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
                        parse_error("'..' is invalid token, do you mean '...'?")
                        return
                    eat() # third
                    make_tok(TEllipsis)    
                else:
                    readFloatLit()
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


