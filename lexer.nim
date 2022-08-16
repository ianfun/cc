##[
C's lexer - export nextTok function
]##

import token
import std/[unicode, math]

proc putToken*(p: var Parser, k: PPToken) = 
    discard

proc isalnum*(c: char): bool =
    return c in {'A'..'Z', 'a'..'z', '0'..'9'}

proc lowleveleat*(p: var Parser) =
    # LF, CR LF, CR is ok
    p.fs_read(p)
    inc p.col
    if p.c == '\n':
        resetLine(p)


proc do_eat*(p: var Parser) =
    lowleveleat(p)
    if p.c == '/':
        lowleveleat(p)
        if p.c == '*':
            p.note("C++ style-comment")
            while true:
                lowleveleat(p)
                if p.c == '*':
                    lowleveleat(p)
                    if p.c == '/':
                        p.c = ' ' # make a space: GNU CPP do it
                        break
                elif p.c == '\0':
                    p.parse_error("expect '*/' before EOF")
                    return
        elif p.c == '/':
            p.note("C style-comment")
            while true:
                lowleveleat(p)
                if p.c == '\n' or p.c == '\0':
                    break
        else:
            p.lastc = uint16(p.c)
            p.c = '/'
    elif p.c == '\\':
        let c = p.c
        lowleveleat(p)
        if p.c == '\n':
            do_eat(p)
        else:
            p.lastc = uint16(p.c)
            p.c = c

proc eat*(p: var Parser) =
    ## skip any C/C++ style comments
    ## https://gcc.gnu.org/onlinedocs/gcc-12.1.0/cpp/Initial-processing.html
    if p.lastc <= 255:
        p.c = char(p.lastc)
        p.lastc = 256
        return
    do_eat(p)

proc make_tok(p: var Parser, op: Token) {.inline.} =
    p.tok = op

proc make_ch_lit(p: var Parser, ch: char) {.inline.} =
    p.val.ival = ch.int

proc readHexChar(p: var Parser): Codepoint =
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
        eat(p)

proc validUCN(p: var Parser, codepoint: Codepoint) =
    if codepoint > 0x10FFFF:
        p.warning("codepoint too large")
    if codepoint >= 0xD800 and codepoint <= 0xDFFF:
        p.warning("universal character in surrogate range")

proc validUCNName(p: var Parser, codepoint: Codepoint) =
    if codepoint <= 0x009F'u32 and codepoint != Codepoint('`') and codepoint != Codepoint('$') and codepoint != Codepoint('@'):
        p.warning("codepoint " & $codepoint &  " cannot be a universal character name")

proc readUChar(p: var Parser, count: int): Codepoint =
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
            p.warning("expect " & $count & " hex digits for universal character")
            break
        eat(p)
        if i == count:
            break
    validUCN(p, n)
    return n

proc readEscape(p: var Parser): Codepoint =
    eat(p)
    case p.c:
    of 'a':
        eat(p)
        return '\a'.Codepoint
    of 'b':
        eat(p)
        return '\b'.Codepoint
    of 'f':
        eat(p)
        return '\f'.Codepoint
    of 'n':
        eat(p)
        return '\n'.Codepoint
    of 'r':
        eat(p)
        return '\r'.Codepoint
    of 't':
        eat(p)
        return '\t'.Codepoint
    of 'v':
        eat(p)
        return '\v'.Codepoint
    of 'e':
        eat(p)
        return '\x1b'.Codepoint
    of 'x':
        eat(p)
        readHexChar(p)
    of 'u', 'U':
        let n = if p.c == 'U': 8 else: 4
        eat(p)
        return readUChar(p, n)
    of '0', '1', '2', '3', '4', '5', '6', '7':
        const octalChs = {'0'..'7'}
        var n = Codepoint(p.c) - '0'.Codepoint
        eat(p) # eat first
        if p.c in octalChs:
            n = (n shl 3.Codepoint) or (Codepoint(p.c) - '0'.Codepoint)
            eat(p) # eat second
            if p.c in octalChs:
                n = (n shl 3.Codepoint) or (Codepoint(p.c) - '0'.Codepoint)
                eat(p) # eat third
        return n
    else:
        let c = p.c.Codepoint
        eat(p)
        return c

proc readCharLit(p: var Parser) =
    p.tok = TCharLit
    eat(p) # eat '
    if p.c == '\\':
        let codepoint = readEscape(p)
        if codepoint > 0xFF:
            p.warning("character constant too large")
        make_ch_lit(p, char(codepoint and 0xFF))
    else:
        make_ch_lit(p, p.c)
        eat(p)
    if p.c != '\'':
        p.parse_error("expect ' , got " & show(p.c))
    eat(p)

proc readIdentLit(p: var Parser) =
    p.tok = TIdentifier
    while true:
      if p.c == '\\':
        eat(p)
        if p.c == 'U':
            eat(p)
            let codepoint = readUChar(p, 8)
            validUCNName(p, codepoint)
            p.val.sval.add(Rune(codepoint).toUTF8)
        elif p.c == 'u':
            eat(p)
            let codepoint = readUChar(p, 4)
            validUCNName(p, codepoint)
            p.val.sval.add(Rune(codepoint).toUTF8)
        else:
            p.parse_error("invalid escape in identifier")
      else:
        if not (isalnum(p.c) or (uint8(p.c) and 0x80'u8) != 0 or p.c == '$' or p.c == '_'):
            break
        p.val.sval.add(p.c)
        eat(p)
    if p.flags == PFNormal:
      let k = isKeyword(p.val.sval)
      if k != TNul:
          make_tok(p, k)

proc decimaltoInt(s: string): int =
    for i in 0..<len(s):
        result = (result * 10) + (int(s[i]) - '0'.int)

proc parse_pp_afterdot(p: var Parser, s: string, init: int = 0): (bool, int) =
  # attempt to paser pp-number to int
  discard

proc parse_pp*(p: var Parser, s: string): (bool, int)  =
  # attempt to paser pp-number to int
  if len(s) == 0:
    return (false, 0)
  if s[0] == '.':
    return parse_pp_afterdot(p, s)
  if s[0] notin {'0'..'9'}:
    p.parse_error("expect '0'..'9' or '.'")
    return (false, 0)
  var digits: string
  var i = 0
  while true:
    if i == s.len or digits[i] notin {'0'..'9'}:
      break
    inc i
  # var base = decimaltoInt(digits)
  if i == len(s):
    return (true, (i))
  case s[i]:
  of 'E', 'e', 'P', 'p':
    discard
  else:
    discard


proc readHexFloatLit(p: var Parser, intPart: float = 0.0) =
    # read a float start from '.'
    p.tok = TFloatLit
    p.val.fval = intPart
    var e = 1.0
    while true:
        e /= 16.0
        eat(p)
        if p.c in {'0'..'9'}:
          p.val.fval += float(int(p.c) - '0'.int) * e
        elif p.c in {'a'..'f'}:
          p.val.fval += float(int(p.c) - 'a'.int + 10) * e
        elif p.c in {'A'..'F'}:
          p.val.fval += float(int(p.c) - 'A'.int + 10) * e
        else:
            if p.c in {'L', 'l', 'F', 'f'}:
                eat(p) # TODO: mark as float
                break
            if p.c != 'P' and p.c != 'p':
                p.parse_error("expect p or P in hex floating constant, got " & show(p.c))
                break
            eat(p)
            var negate = false
            if p.c == '-':
                negate = true
                eat(p)
            elif p.c == '+':
                eat(p)
            elif p.c notin {'0'..'9'}:
                p.parse_error("expect exponent digits")
            var exps: string
            while true:
                exps.add(p.c)
                eat(p)
                if p.c notin {'0'..'9'}:
                    break
            p.val.fval *= pow(2, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
            if p.c in {'L', 'l', 'F', 'f'}:
                 # TODO: mark as float
                eat(p)
            break


proc readFloatLit(p: var Parser, intPart: float = 0.0) =
    # read a float start from '.'
    p.tok = TFloatLit
    p.val.fval = intPart
    var e = 0
    while true:
        inc e
        if p.c notin {'0'..'9'}:
            if p.c in {'L', 'l', 'F', 'f'}:
                eat(p) # TODO: mark as float
                break
            if p.c != 'E' and p.c != 'e':
                break
            eat(p)
            var negate = false
            if p.c == '-':
                negate = true
                eat(p)
            elif p.c == '+':
                eat(p)
            elif p.c notin {'0'..'9'}:
                p.parse_error("expect exponent digits")
            var exps: string
            while true:
                exps.add(p.c)
                eat(p)
                if p.c notin {'0'..'9'}:
                    break
            p.val.fval *= pow(10, if negate: -float(decimaltoInt(exps)) else: float(decimaltoInt(exps)))
            if p.c in {'L', 'l', 'F', 'f'}:
                 # TODO: mark as float
                eat(p)
            break
        p.val.fval += float(int(p.c) - '0'.int) / pow(10.0, float(e))
        eat(p)

proc readNumberLit(p: var Parser) =
    p.tok = TNumberLit
    p.val.ival = 0
    if p.c == '0':
        eat(p)
        case p.c:
        of '0', '1', '2', '3', '4', '5', '6', '7': # octal
          discard
        of 'x', 'X': # hex
          eat(p)
          if p.c notin {'0'..'9', 'A'..'F', 'a'..'f'}:
            p.parse_error("invalid hex literal: " & show(p.c))
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
                eat(p)
              return
            elif p.c == '.':
                readHexFloatLit(p, float(p.val.ival))
                return
            else:
              return
            eat(p)
        of 'b', 'B': # binary
          eat(p)
          if p.c != '0' and p.c != '1':
            p.parse_error("invalid binary literal: expect 0 or 1, got " & show(p.c))
            return
          while true:
            case p.c:
            of '0':
              p.val.ival = p.val.ival shl 1
            of '1':
              p.val.ival = (p.val.ival shl 1) or 1
            else:
              if p.c in {'0'..'9'}:
                p.warning("invalid decimal digit in binary literal")
              return
            eat(p)
        of '.': # float
          eat(p)
          readFloatLit(p)
        else:
          return # zero
    else: # decimal
      while true:
        p.val.ival = (10 * p.val.ival) + (int(p.c) - '0'.int)
        eat(p)
        if p.c == '.':
            readFloatLit(p, float(p.val.ival))
            return
        if p.c in {'L', 'l', 'U', 'u'}:
            while p.c in {'L', 'l', 'U', 'u'}:
                eat(p)
            return
        elif p.c notin {'0'..'9'}:
            if p.c in {'a'..'z', 'A'..'Z'}: # User-defined literals?
              p.warning("invalid decimal suffix " & show(p.c))
              p.note("user-defined literals is a C++ feature")
            return

proc readStringLit(p: var Parser, enc: int) =
    p.tok = TStringLit
    while true:
        if p.c == '\\':
            p.val.sval.add(Rune(readEscape(p)).toUTF8)
        elif p.c == '"':
            eat(p)
            break
        else:
            if 0xF0 == (0xF8 and Codepoint(p.c)): # 4 byte
              p.val.sval.add(p.c)
              eat(p)
              p.val.sval.add(p.c)
              eat(p)
              p.val.sval.add(p.c)
              eat(p)
              p.val.sval.add(p.c)
            elif 0xE0 == (0xF0 and Codepoint(p.c)): # 3 byte
              p.val.sval.add(p.c)
              eat(p)
              p.val.sval.add(p.c)
              eat(p)
              p.val.sval.add(p.c)
            elif 0xC0 == (0xE0 and Codepoint(p.c)): # 2 byte
              p.val.sval.add(p.c)
              eat(p)
              p.val.sval.add(p.c)
            else: # 1 byte
              if p.c == '\n':
                p.warning("missing terminating '\"' character, read newline as \\n")
              elif p.c == '\0':
                p.parse_error("unexpected EOF")
                return
              p.val.sval.add(p.c)
            eat(p)
    p.val.ival = enc

proc readPPNumberAfterDot(p: var Parser) =
    p.val.sval.add('.')
    while p.c in {'0'..'9'}:
        p.val.sval.add(p.c)
        eat(p)

proc readPPNumber(p: var Parser) =
    make_tok(p, TPPNumber)
    while true:
        p.val.sval.add(p.c)
        eat(p)
        if p.c notin {'0'..'9'}:
            break
    if p.c == '.':
        eat(p)
        readPPNumberAfterDot(p)
    elif p.c in {'e', 'p', 'E', 'P'}:
        p.val.sval.add(p.c)
        eat(p)
        if p.c == '+' or p.c == '-':
            p.val.sval.add(p.c)
            eat(p)
    # nodigit
    elif p.c in {'a'..'z', 'A'..'Z', '_'}:
        while true:
            p.val.sval.add(p.c)
            if p.c notin {'a'..'z', 'A'..'Z', '_'}:
                break
    # universal-character-name
    elif p.c == '\\':
        eat(p)
        let n = if p.c == 'U': 8 else: 4
        eat(p)
        let c = readUChar(p, n)
        p.val.sval.add(Rune(c).toUTF8)

proc skipLine(p: var Parser; warn=true) =
    while p.c != '\n' and p.c != '\0':
        if warn:
          p.warning("extra tokens found")
        eat(p)
    p.flags = PFNormal

proc nextTok*(p: var Parser) =
    while true:
        p.val.sval.setLen 0
        if p.c in CSkip:
          eat(p)
          continue
        if p.c == '#':
            eat(p)
            p.flags = PFPP
            nextTok(p) # directive
            if p.tok != TIdentifier:
                p.flags = PFNormal
                p.parse_error("invalid preprocessing directive: expect an identifier")
                return
            case p.val.sval:
            of "define":
                nextTok(p) # name
                if p.tok != TIdentifier:
                    p.flags = PFNormal
                    p.parse_error("macro name should be a identifier")
                    p.note("the syntax is:\n\t#define identifier replacement-list\n\t#define identifier(identifier-list) replacement-list")
                    return
                var name = p.val.sval # copy to string
                nextTok(p)
                var m = if p.tok == TLbracket: PPMacro(funcmacro: true, ivarargs: false) else: PPMacro(funcmacro: false)
                if m.funcmacro:
                    while true:
                        nextTok(p)
                        if p.tok == TIdentifier:
                            m.params.add(p.val.sval)
                            nextTok(p)
                            if p.tok == TRbracket:
                                break
                            elif p.tok == TComma:
                                continue
                            else:
                                p.parse_error("')' or ',' expected")
                                p.flags = PFNormal
                                return
                        elif p.tok == PPEllipsis:
                            m.ivarargs = true
                            nextTok(p)
                            if p.tok != TRbracket:
                                p.parse_error("')' expected")
                                p.flags = PFNormal
                                return
                            break
                        elif p.tok == TRbracket:
                            break
                    nextTok(p)
                while true:
                    if p.flags != PFPP:
                        break
                    m.tokens.add(tokenToPPToken(p))
                    nextTok(p)
                echo m.tokens
                macro_define(p, name, m)
            of "if":
                let v = true # eval . parse
                p.ppstack.add(if v: 1 else: 0)
                p.ok = v
                skipLine(p)
            of "ifdef", "ifndef":
                let ndef = p.val.sval=="ifndef"
                nextTok(p)
                if p.tok != TIdentifier:
                    p.flags = PFNormal
                    p.parse_error("expect identifier")
                    p.note("the syntax is:\n\t#ifdef identifier\n\t#ifndef identifier")
                    return
                let name = p.val.sval # no copy
                let v = if ndef: not macro_defined(p, name) else: macro_defined(p, name)
                p.ppstack.add(if v: 1 else: 0)
                p.ok = v
                skipLine(p)
            of "else":
                if p.ppstack.len == 0:
                    p.parse_error("no matching #if")
                    return
                if (p.ppstack[^1] and 2) != 0:
                    p.parse_error("#else after #else")
                    return
                p.ppstack[^1] = p.ppstack[^1] or 2
                p.ok = not p.ok
                skipLine(p)
            of "elif":
                if p.ppstack.len == 0:
                    p.parse_error("no matching #if")
                    return
                if (p.ppstack[^1] and 2) != 0:
                    p.parse_error("#elif after #else")
                    return
                skipLine(p)
            of "endif":
                if p.ppstack.len == 0:
                    p.parse_error("no matching #if")
                    skipLine(p)
                    return
                discard p.ppstack.pop() # p.ppstack.del(p.ppstack.len - 1)
                if p.ppstack.len == 0:
                    p.ok = true
                else:
                    p.ok = (p.ppstack[^1] or 1) != 0
                skipLine(p)
            of "include":
                while p.c in CSkip:
                    eat(p)
                var path: string
                case p.c:
                of '"':
                    while true:
                        eat(p)
                        if p.c == '"':
                            eat(p)
                            break
                        if p.c == '\0' or p.c == '\n':
                            p.flags = PFNormal
                            p.parse_error("unexpected EOF, expect path or '\"'")
                            return
                        path.add(p.c)
                    let r = addInclude(p, path)
                    if not r:
                        p.parse_error("include file not found: " & path)
                of '<':
                    while true:
                        eat(p)
                        if p.c == '>':
                            eat(p)
                            break
                        if p.c == '\0' or p.c == '\n':
                            p.flags = PFNormal
                            p.parse_error("unexpected EOF, expect path or '>'")
                            return
                        path.add(p.c)
                    echo "including file: ", path
                else:
                    p.parse_error("expect \"FILENAME\" or <FILENAME>")
                    p.note("the syntax is:\n\t#include <path>\n\t#include \"path\"")
                    return
                skipLine(p)
            of "line":
                while p.c in CSkip:
                    eat(p)
                if p.c notin {'0'..'9'}:
                    p.parse_error("expect digits (positive line number)")
                var s: string
                while true:
                    s.add(p.c)
                    eat(p)
                    if p.c in {'0'..'9'}:
                        continue
                    break
                p.line = decimaltoInt(s)
                while p.c in CSkip:
                    eat(p)
                if p.c == '\0' or p.c == '\n':
                    discard
                else:
                    if p.c != '"':
                        p.parse_error("expect \"FILENAME\"")
                        return
                    var f: string
                    while true:
                        eat(p)
                        if p.c == '\n' or p.c == '"' or p.c == '\\' or p.c == '\0':
                            break
                        f.add(p.c)
                    if p.c != '"':
                        p.parse_error("'\"' expected")
                        return
                    eat(p)
                    p.filename = f
                    skipLine(p)
            of "undef":
                nextTok(p)
                if p.tok != TIdentifier:
                    p.flags = PFNormal
                    p.parse_error("macro name should be a identifier")
                    p.note("the syntax is: #undef identifier")
                    return
                macro_undef(p, p.val.sval)
                skipLine(p)
            of "pragma":
                # https://docs.microsoft.com/en-us/cpp/preprocessor/pragma-directives-and-the-pragma-keyword
                # https://gcc.gnu.org/onlinedocs/gcc/Pragmas.html
                # https://gcc.gnu.org/onlinedocs/cpp/Pragmas.html
                # pragma once
                nextTok(p)
                if p.tok != TIdentifier:
                    p.flags = PFNormal
                    p.note("pragma should start with a identifier")
                    return
                case p.val.sval:
                of "once":
                    addOnce(p)
                else:
                    discard
                skipLine(p, false)
            of "error", "warning":
                var s: string
                let iswarning = p.val.sval == "warning"
                while p.c == ' ':
                    eat(p)
                while true:
                    if p.c == '\n' or p.c == '\0':
                        break
                    s.add(p.c)
                    eat(p)
                if iswarning:
                    p.warning("#warning: " & s)
                else:
                    p.parse_error("#error: " & s)
                return
            else:
                p.parse_error("invalid directive: " & p.val.sval)
                skipLine(p, false)
            eat(p)
            continue
        if p.flags == PFNormal and p.ok == false:
            while p.c != '\n' and p.c != '\0':
                eat(p)
            if p.c == '\n':
                eat(p)
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
            eat(p)
            continue
        if p.c in {'0'..'9'}:
            if p.flags == PFPP:
                readPPNumber(p)
            else:
                readNumberLit(p)
            return
        case p.c:
        of 'u':
            eat(p)
            if p.c == '"':
                eat(p)
                readStringLit(p, 16)
            elif p.c == '8':
                eat(p)
                if p.c != '"':
                    p.val.sval = "u8"
                    readIdentLit(p)
                else:
                    eat(p)
                    readStringLit(p, 8)
            else:
                p.val.sval = "u"
                readIdentLit(p)
            return
        of 'U':
          eat(p)
          if p.c != '"':
            p.val.sval = "U"
            readIdentLit(p)
          else:
            eat(p)
            readStringLit(p, 32)
          return
        of 'L':
          eat(p)
          if p.c != '"':
            p.val.sval = "L"
            readIdentLit(p)
          else:
            eat(p)
            readStringLit(p, 16)
          return
        else:
            discard

        if p.c in {'a'..'z', 'A'..'Z', '\x80' .. '\xFD', '_', '$', '\\'}:
         readIdentLit(p)
         return

        case p.c:
        of '.':
            if p.flags == PFNormal:
                eat(p) # first
                if p.c == '.':
                    eat(p) # second
                    if p.c != '.':
                        p.parse_error("'..' is invalid token, do you mean '...'?")
                        return
                    eat(p) # third
                    make_tok(p, PPEllipsis)    
                else:
                    readFloatLit(p)
            else:
                eat(p) # first '.'
                if p.c == '.':
                    eat(p) # second '.'
                    if p.c != '.':
                        p.parse_error("'..' is invalid token, do you mean '...'?")
                        return
                    eat(p) # third '.'
                    make_tok(p, PPEllipsis)
                else:
                    make_tok(p, TPPNumber)
                    readPPNumberAfterDot(p)
            return
        of '\0':
            if p.flags == PFPP:
                p.flags = PFNormal
            p.tok = TEOF
            return
        of '(', ')', '~', '?', '{', '}', ',', '[', ']', ';':
            make_tok(p, cast[Token](p.c))
            eat(p)
            return
        of '"':
            eat(p)
            readStringLit(p, 8)
            return
        of ':':
            eat(p)
            if p.c == '>':
                make_tok(p, TRSquareBrackets)
                eat(p)
            else:
                make_tok(p, TColon)
            return
        of '-':
            eat(p)
            if p.c == '-':
                make_tok(p, TSubSub)
                eat(p)
            elif p.c == '=':
                make_tok(p, TAsignSub)
                eat(p)
            else:
                make_tok(p, TDash)
            return
        of '+':
            eat(p)
            if p.c == '+':
                make_tok(p, TAddAdd)
                eat(p)
            elif p.c == '=':
                make_tok(p, TAsignAdd)
                eat(p)
            else:
                make_tok(p, TAdd)
            return
        of '\'':
            readCharLit(p)
            return
        of '>':
            eat(p)
            if p.c == '=':
                make_tok(p, TGe)
                eat(p)
            elif p.c == '>':
                make_tok(p, Tshr)
                eat(p)
            else:
                make_tok(p, TGt)
            return
        of '<':
            eat(p)
            case p.c:
            of '<':
                make_tok(p, Tshl)
                eat(p)
            of '=':
                make_tok(p, TLe)
                eat(p)
            of ':':
                make_tok(p, TLSquareBrackets)
                eat(p)
            of '%':
                make_tok(p, TLcurlyBracket)
                eat(p)
            else:
                make_tok(p, TLe)
            return
        of '%':
            eat(p)
            if p.c == '=':
                make_tok(p, TAsignRem)
                eat(p)
            elif p.c == '>':
                make_tok(p, TRcurlyBracket)
                eat(p)
            elif p.c == ':':
                make_tok(p, TBash)
                eat(p)
            else:
                make_tok(p, TPercent)
            return
        of '*':
            eat(p)
            if p.c == '=':
                make_tok(p, TAsignMul)
                eat(p)
            else:
                make_tok(p, TMul)
            return
        of '=':
            eat(p)
            if p.c == '=':
                make_tok(p, TEq)
                eat(p)
            else:
                make_tok(p, TAssign)
            return  
        of '&':
            eat(p)
            if p.c == '=':
                make_tok(p, TAsignBitAnd)
                eat(p)
            else:
                make_tok(p, TBitAnd)
            return
        of '|':
            eat(p)
            if p.c == '=':
                make_tok(p, TAsignBitOr)
                eat(p)
            else:
                make_tok(p, TBitOr)
            return
        of '^':
            eat(p)
            if p.c == '=':
                make_tok(p, TAsignBitXor)
                eat(p)
            else:
                make_tok(p, TXor)
            return
        of '/':
            eat(p)
            if p.c == '=':
                make_tok(p, TAsignDiv)
                eat(p)
                return
            else:
                make_tok(p, TSlash)
                return
        of '!':
            eat(p)
            if p.c == '=':
                make_tok(p, TNe)
                eat(p)
            else:
                make_tok(p, TNot)
            return
        else:
          p.warning("invalid token: " & show(p.c))

        eat(p)
