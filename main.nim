import "."/[token, lexer, parser]

proc test(testLex = false) =
    var p: Parser
    reset(p)
    stdinParser(p)
    nextTok(p)
    while p.tok != TEOF:
        if testLex:
            stdout.writeLine("\e[4;46m" & showToken(p) & "\e[0m")
            nextTok(p)
        else:
            declaration(p)
        break
        p.err = false
    closeParser(p)

test(false)
