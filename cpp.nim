import core, token, parser, stream
from lexer import eat
import std/[times, tables, sets]

proc getToken*()

proc setCpp*() =
    app.cpp = getToken

proc checkMacro()

proc builtin_Pragma() =
  getToken()
  if p.tok.tok != TLbracket:
      expectLB()
      note("the syntax is:\n\t_Pragma(<string>)")
  else:
      getToken()
      if p.tok.tok != TStringLit:
          expect("expect string literal")
          note("the syntax is:\n\t_Pragma(<string>)")
      else:
          var pra = p.tok.s
          getToken()
          if p.tok.tok != TRbracket:
              expectRB()
          else:
              echo "pragma: ", pra
              getToken()

proc getMacro*(name: string): PPMacro =
  case name:
  of "__COUNTER__":
    result = PPMacro(tokens: @[TokenV(tok: TPPNumber, tags: TVSVal, s: $t.counter)], flags: MOBJ)
    inc t.counter
  of "__LINE__":
    result = PPMacro(tokens: @[TokenV(tok: TPPNumber, tags: TVSVal, s: $p.line)], flags: MOBJ)
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
    result = p.macros.getOrDefault(name, nil)

proc macro_find*(name: string): PPMacro =
  p.macros.getOrDefault(name, nil)

proc beginExpandMacro*(a: string) =
  t.expansion_list.incl a

proc endExpandMacro*(a: string) =
  t.expansion_list.excl a

proc isMacroInUse*(a: string): bool =
  t.expansion_list.contains(a)

proc getToken*() =
    ## CPP layer(Preprocessing)
    if len(p.tokenq) == 0:
        app.lex()
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
    p = Parser(flags: PFPP, line: 1, col: 1, c: ' ', lastc: 256, tok: TokenV(tok: TNul, tags: TVNormal))
    result = TokenV(tok: TNul, tags: TVNormal)
    let s = stringizing(a) & stringizing(b)
    var oldlen = t.fstack.len
    t.fstack.add(newStringStream(s))
    app.lex()
    if p.tok.tok == TSpace:
        app.lex()
    if t.fstack.len != oldlen:
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
