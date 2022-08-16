import "."/[token, lexer, parser]

proc test(testLex = false) =
    var p: Parser
    reset(p)
    stdinParser(p)
    nextTok(p)
    translation_unit(p)
    closeParser(p)
test(false)
